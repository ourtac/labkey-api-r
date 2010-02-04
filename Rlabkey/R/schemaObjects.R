##
# Copyright (c) 2010 LabKey Corporation
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
##

## an environment to hold metadata descriptions
.lksite <- new.env(parent=emptyenv())
.lksession <- new.env(parent=emptyenv())

##########################################
##  public functions
##
## a session holds current selected values of the site schema root and the user's currnt folder / container
## These are put in two different environment spaces, the schema root is keyed by the URL because the schema is not going to be 
## different if the URL is the same.  
## The session holds pointers to the site and session variables and holds the baseUrl and current folder path
##
## Two option buckets are created at the session root so that they  can be read by network-level code without having to pass
## them down through all the calls.

lkCreateSession <-
    function(baseUrl, folderPath="/Home", curlOptions=NULL, lkOptions=NULL)
{																					
	lks<- .LabkeySession(baseUrl)

	lks[["lksite"]]<-.lksite
	
	lks[["skey"]] <-  gsub("[ :]*", "", as.character(date()))

	
	.lksession[[lks$skey]]<- list("baseUrl"=baseUrl, "folderPath"=folderPath, "validSchemas"=NA, "lkOptions" = NA)
	
	## Why doesn't this work?
	lks[["session"]] <- .lksession[[lks$skey]]

	if (missing(lkOptions)) {lkOptions <- list(NA)}	
	.lksession[[lks$skey]]$lkOptions <- lkOptions
		
	# put curlOptions on a package-wide environment so we don't need to pass them everywhere
	.lksession[["curlOptions"]] <- curlOptions 
	
	.setupSchemas(lks, folderPath)	

	lks
}


##  getter and setters for the folderPath 
getFolderPath <-
    function(lks)
{
   	return(as.character(lks$session$folderPath))
}


getSchema <-
    function(lks,schemaIndex)
{
	if (is.integer(schemaIndex) )
		{sname <- lsSchemas(lks)[schemaIndex, 1]}
	else	
		{sname <- schemaIndex}
		
	if (!.validateSchemaName (lks, sname)) {    
		stop("Can't find that schema. \n")
	}
	## if new queriesList comes back empty, don't use it-- the cache will remain as it was
	newQueriesList <- .getQueriesList(lks, sname)
	if (length(newQueriesList)>0) 
	{		
		lks$lksite[[sname]]<- .LabkeySchema(newQueriesList)
	}



	return(lks$lksite[[sname]])
   			
}


##   returns the set of fields accessible through a lookup col.  these are not cached
##
getLookups<-
	function(lks, query, lookupField)
{
	if(!inherits(lks, "LabkeySession") || !inherits(query, "LabkeyQuery"))
		{stop("getLookups() requires a session and a schema$query object")}	
	
	if (is.character(lookupField))
		{luName <- lookupField}
	else
		{luName <- lookupField$fieldName}
	out <- labkey.getLookupDetails(lks$baseUrl, getFolderPath(lks), query$.schemaName, query$.name, luName )	
	fieldList <- list()
	for (r in row.names(out))
	{				
		rowList <- as.list(out[r,])
		fieldList[[as.character(rowList$fieldName)]]<- .LabkeyField(rowList)
	}
	return (.LabkeyQuery(fieldList))
}

#########################
###  wrapper on labkey.selectRows that makes all these objects worthwhile
getRows<-
	function(lks, query, maxRows=NULL, colNameOpt='fieldname', ...)
{
	if(!inherits(lks, "LabkeySession") && inherits(query, "LabkeyQuery"))
		{stop("getRows() requires a session and a schema$query object")}
	if (length(query$.schemaName)==0 || length(query$.name)==0)
		{stop("Invalid query object")}
	
	## support session defaults
	dflt <- .lksession[[lks$skey]]$lkOptions
	
	if (length(dflt)>0 && !is.na(dflt))
	{
		for (nm in names(dflt) )
		{
			if ((nm=="colNameOpt") && missing(colNameOpt)) {colNameOpt <- dflt$colNameOpt}
			if ((nm=="maxRows") && missing(maxRows)) {maxRows <- dflt$maxRows}
		}
	}
	lkdata <- labkey.selectRows(baseUrl=lks$baseUrl, folderPath=getFolderPath(lks), schemaName=query$.schemaName
			, queryName=query$.name, maxRows=maxRows, colNameOpt=colNameOpt, ...)	
	return(lkdata)			
						
}


############################################
##  list available schemas (given base Url and current folder path) 
##
lsSchemas <-
	function(lks)
{
	schemas <- as.array(.lksession[[lks$skey]]$validSchemas[,1] )
	return(schemas)
}


############################################
##  list available folders (given base Url and current folder path) 
##
lsFolders <-
	function(lks)
{
	## find the project name part
	path <- gsub("[\\]", "/", getFolderPath(lks))
	pathParts <- strsplit(path,"/")[[1]]
	pathParts <- pathParts[pathParts!=""]
	if (length(pathParts)==0) {projectName<- "/"} else {projectName<-pathParts[1]}
		
	folders <- labkey.getFolders(lks$baseUrl, projectName, includeSubfolders=TRUE)
	return (sort(as.array(folders$folderPath)))	

}


###########################################################################################
##############################################
##  Private functions
##
## load schemas from labkey server.  don't load queries until a specific schema is requested

.setupSchemas <-
    function(lks, folderPath)
{
	
	out <- labkey.getSchemas(lks$baseUrl, folderPath)
	## build a list of valid scheas for this folder context
	validSchemasDF <- cbind (out, date())
	validSchemasDF <- data.frame(validSchemasDF[order(validSchemasDF[,1]), ], stringsAsFactors=FALSE)
	colnames(validSchemasDF) <- c("schemaName", "timestamp")
	
	.lksession[[lks$skey]][["validSchemas"]] <- validSchemasDF	
	
	for (sn in out$schemaName)
	{		
		if (length(lks$lksite[[as.character(sn)]])==0) {
			lks$lksite[[as.character(sn)]]<- structure(as.list(NA))
		}	
	}
##    invisible(TRUE)                     # quiet success
}


.getQueriesList <- function(lks, schemaName)
{		
	out <- labkey.getQueries(lks$baseUrl, getFolderPath(lks), schemaName)
	## if the existing cached schema passes the checks in .checkValid, then we return an empty queries list
	queriesList <- NULL	
	if (.checkValid(lks, schemaName, out)==FALSE) {
		queries <- unique(out$queryName)	

		for (q in queries)
		{	
			queryObjName <- as.character(q)
			queriesList[[queryObjName]]<-.LabkeyQuery(c(.getQueryDetails(lks, schemaName, q), ".name"=q, ".schemaName"=schemaName))
		}
	}		
	return(queriesList)
}

.getQueryDetails <- function(lks, schemaName, queryName)
{	
	## get the default view info as this is what selectRows returns
	out <- labkey.getDefaultViewDetails(lks$baseUrl, getFolderPath(lks), schemaName, queryName)	
	fieldList <- list()
	for (r in row.names(out))
	{				
		rowList <- as.list(out[r,-1])
		fieldList[[as.character(rowList$fieldName)]]<- .LabkeyField(rowList)
		
	}
	return(fieldList)
}


.validateSchemaName <-
	function(lks, sname)
{
	retval<-FALSE
	vsch <- lsSchemas(lks)
	if (length(sname) > 0 && length(vsch[vsch==sname]) == 1)  {
		retval <- TRUE
	}
	invisible(TRUE)
	return (retval)
}

.checkValid <- function(lks, schemaName, queriesDF)
{	
	## check to see if the schema cache slots we are about to populate are identical down to the fieldName level
	## a return of false on any query causes the whole schema to reload
	sch <- lks$lksite[[schemaName]]
	retval <- FALSE
	if(length(sch)==0) {}
	else if((length(sch)==1) && is.na(sch)) {}
	else {
		qnames <- names(sch)
		if (length(qnames)==0) {}
		else {
			for (qn in qnames) 
			{				
				currentflds <- queriesDF[queriesDF$queryName==qn, 2]
				cachedflds <- grep(".", names(sch[[qn]]), fixed=TRUE, invert=TRUE, value=TRUE)
				
				if (  (length(currentflds) == length(cachedflds)) && length(all.equal(currentflds, cachedflds))==0) 
				{
					retval <- TRUE
				} 
			}	
		}	
	}
	return (retval)
}



###########################################################################
## Class declarations and print methods 
##definition of a 'LabkeySession' 
.LabkeySession <-
    function(baseUrl, ...)
{
	 structure(list("baseUrl"=baseUrl, ...), class="LabkeySession")
}

print.LabkeySession <-
    function(x, ..., pad="")
{
	cat("Base Url:  "	, x$baseUrl, "\n")
	cat("Folder Path:  ", getFolderPath(x), "\n")
	cat("Available schemas: \n\t")			
	for (i in lsSchemas(x)) {
		cat(as.character(i),"\n\t",sep="")
	}	
	cat("\n\n")
	cat("Folders in project:  \n\t")
	for (i in lsFolders(x)) {
		cat(as.character(i),"\n\t",sep="")
	}	

}

.LabkeySchema <-
    function(...)
{
    structure(as.list(...), class="LabkeySchema")
}

print.LabkeySchema <-
    function(x,...)
{  
    	cat("Available queries: \n\t")
    	for (i in names(x)) {
	  	cat(i,"\n\t",sep="")
	}
	cat("\n")
}

## wrap a list of LabKeyFields as a LabkeyQuery
.LabkeyQuery <-
    function(...)
{
     structure(as.list(...), class="LabkeyQuery")
}
#			fnames <- grep(".", names(query), fixed=TRUE, invert=TRUE, value=TRUE)
#			colSelect <- fnames[1]
#			for (n in fnames[-1])
#			{
#				colSelect <- paste(colSelect, query[[n]]$fieldName, sep=", ")	
#			}

print.LabkeyQuery <-
    function(x, ..., pad="")
{		
    	alldf <- as.data.frame(NULL, nrow=1, ncol=3)
    	for (i in grep(".", names(x), fixed=TRUE, invert=TRUE, value=TRUE)) {    		
    		luinfo <- " "
		if (!is.na(x[[i]]$lookupQueryName) ) 
			{luinfo<- paste("  lookup to ",x[[i]]$lookupSchemaName,".", x[[i]]$lookupQueryName, sep="") }
    		
		outdf <- data.frame(cbind(x[[i]]$fieldName, x[[i]]$caption, x[[i]]$type, luinfo))
		alldf <- rbind(alldf, outdf)		
	}
	colnames(alldf) <- c("fieldName", "caption", "type", "related queries")
	print(alldf)
}

.LabkeyField <-
    function(x, ...)
{
    structure(as.list(x, ...),
              class="LabkeyField")
}

#print.LabkeyField <-
#    function(x, ..., pad="")
#{
#	print(x)
#}

