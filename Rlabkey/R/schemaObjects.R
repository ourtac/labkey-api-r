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
## a session holds current selected values of site, folderpath, schema and query
## 

lkCreateSession <-
    function(baseUrl, folderPath="/", schemaName=NULL, queryName=NULL, ...)
{																					
		
	lks<- .LabkeySession(baseUrl)
	lks[["lksite"]]<-.lksite
	lks[["skey"]] <-  gsub("[ :]*", "", as.character(date()))
	.lksession[[lks$skey]]<- list("baseUrl"=baseUrl, "folderPath"=folderPath, 
					"curSchemaName"=as.character(NULL), "curQueryName"=as.character(NULL), "validSchemas"=NA )
	lks[["session"]] <- .lksession[[lks$skey]]
	
	.setupSchemas(lks, folderPath)	

	if (!missing(schemaName)) {
		curSchema(lks) <- schemaName
	}
	if (!missing(queryName)) {
		curQuery(lks) <- queryName
	}	
	lks
}


##  getter and setters for the folderPath 
curFolderPath <-
    function(lks, ...)
{
   	return(as.character(lks$session$folderPath))
}

curFolder <-
    function(lks, ...)
{
	##nyi
	fld <- list("root"="/", "Home"="/Home")
   	return(fld)
}


`curFolder<-` <-
    function(lks, ..., value)
{
	## recheck schemas
	lks$session$folderPath <- as.character(value)
	.setupSchemas(lks, value)	
	
	sname <- curSchemaName(lks)
	if (length(sname)>0 && (.validateSchemaName(lks, sname) == FALSE))  {
	   	lks$session$curSchemaName <- as.character(NULL)
	   	lks$session$curQueryName <- as.character(NULL)
		cat("Current schema and query have been reset.   \n")		
	}
	lks
}

####  getters and setter for the current schema.  
##   Two getters, one for the name and one for the object.  
##  only onoe method to set (takes a name)

curSchemaName <-
	function(lks)
{
	sname <-lks$session$curSchemaName
	return(sname)
}

curSchema <-
    function(lks, ...)
{
    sname <- curSchemaName(lks)
    if (length(sname)==0) {
		stop("Current schema has not been set.  \n")    	    
    }
	if (!.validateSchemaName (lks, sname)) {    
		lks$session$curSchemaName <- as.character(NULL)
		lks$session$curQueryName <- as.character(NULL)
		stop("Current schema and query have been reset. \n")
	}
	sch	<- lks$lksite[[sname]]
   	return (sch)     	
}

`curSchema<-` <-
    function(lks, ..., value)
{
	if (!.validateSchemaName (lks, value)) {    
		stop("Not a valid schema in the current folder context.\n")	
	}
	else {
		## if new queriesList comes back empty, don't use it-- the cache will remain as it was
		newQueriesList <- .getQueriesList(lks, value)
		if (length(newQueriesList)>0) {		
			lks$lksite[[value]]<- .LabkeySchema(newQueriesList)
		}
		lks$session$curSchemaName <- as.character(value)
	}	
##	invisible(TRUE)	
	lks
}

####  getters and setter for the current query.  

curQueryName <-
	function(lks)
{
	qname<- as.character(lks$session$curQueryName)
	if (length(qname)==0) {
	  #  	cat("No current query assigned.  \n")
	}
##	invisible(TRUE)	
	return (qname)
}

curQuery <-
    function(lks, ...)
{
	qname <- curQueryName(lks)
	cs<- curSchema(lks)
	cq<- cs[[qname]]
	if (length(cq) == 0) {
		lks$session$curQueryName <- as.character(NULL)
		cat("Current query not valid.  Resetting.\n")
	}	   		
   invisible(TRUE)
   return(cq)
}

`curQuery<-` <-
    function(lks, ..., value)
{	
	csname <- curSchemaName(lks)
	
	cs <- curSchema(lks)
	if (length(cs[[value]])> 0) {
	## valid name, fill in column details if needed
		if((length(cs[[value]])==1) && is.na(cs[[value]])) {
			lks$lksite[[csname]][[value]]<- .LabkeyQuery(.getQueryDetails(lks, csname, value))
		}
		lks$session$curQueryName<- value
	}	
	else 
		{ stop(cat(csname, "." , value, " not found in folder ", curFolderPath(lks), " \n", sep=""))	}
		
##	invisible(TRUE)			
	lks
}

##   returns the set of fields accessible through a lookup col.  these are not cached
getLookups<-
	function(lks, lookupField, ...)
{
	.checkSession(lks)
	out <- labkey.getLookupDetails(lks$baseUrl, curFolderPath(lks), curSchemaName(lks), curQueryName(lks), lookupField)	
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
	function(lks, viewName=NULL, colSelect=NULL,
        maxRows=NULL, rowOffset=NULL, colSort=NULL, colFilter=NULL, showHidden=FALSE, colNameOpt='fieldname')
{
	.checkSession(lks)	
	lkdata <- labkey.selectRows(lks$baseUrl, curFolderPath(lks), curSchemaName(lks), curQueryName(lks), 
					viewName=viewName, colSelect=colSelect, maxRows=maxRows, rowOffset=rowOffset, 
					colSort=colSort, colFilter=colFilter, showHidden=showHidden, colNameOpt=colNameOpt)
		
	return (lkdata)
}


############################################
##  list available schemas (given base Url and current folder path) 
##
lsSchemas <-
	function(lks, ...)
{
	return(as.array(.lksession[[lks$skey]]$validSchemas[,1] ))
}


############################################
##  list available queries (given base Url, current folder path, and current schema)
##
lsQueries <-
	function(lks, ...)
{
	sname<- curSchemaName(lks)
	if (length(sname)==0) {
		stop("Must set a current schema via curSchema(lks)<- \"schema name\"  first. \n")
	}		
	else {
		return (names(curSchema(lks)))		
	}	
}


############################################
##  list available folders (given base Url and current folder path) 
##
lsFolders <-
	function(lks, subfolders=TRUE, ...)
{
	##NYI
	return(c("/", "/Home", "/MyProject"))
}


###########################################################################################
##############################################
##  Private functions
##
## load schemas from labkey server.  don't load queries until curSchema is set
.setupSchemas <-
    function(lks, folderPath)
{
	out <- labkey.getSchemas(lks$baseUrl, folderPath)
	## build a list of valid scheas for this folder context
	validSchemas <- cbind (out, date())
	colnames(validSchemas) <- c("schemaName", "timestamp")
	
	.lksession[[lks$skey]][["validSchemas"]] <- validSchemas		
	
	for (sn in out$schemaName)
	{		
		if (length(lks$lksite[[as.character(sn)]])==0) {
			lks$lksite[[as.character(sn)]]<- structure(as.list(NA))
		}	
	}
##    invisible(TRUE)                     # quiet success
}


.getQueriesList <- function(lks, schemaName,...)
{		
	out <- labkey.getQueries(lks$baseUrl, curFolderPath(lks), schemaName)
	## if the existing cached schema passes the checks in .checkValid, then we return an empty queries list
	queriesList <- NULL	
	if (.checkValid(lks, schemaName, out)==FALSE) {
		queries <- unique(out$queryName)	

		for (q in queries)
		{			
			queriesList[[as.character(q)]]<-.LabkeyQuery(list(NA))			
		}
	}		
	return(queriesList)
}

.getQueryDetails <- function(lks, schemaName, queryName, ...)
{	
	## get the default view info as this is what selectRows returns
	out <- labkey.getDefaultViewDetails(lks$baseUrl, curFolderPath(lks), schemaName, queryName)	
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
				cachedflds <- names(sch[[qn]])
				if (length(currentflds) == length(cachedflds) && all.equal(currentflds, cachedflds)) {
					retval <- TRUE
				} 
			}	
		}	
	}
	return (retval)
}

.checkSession <-
	function(lks)
{
	if (length(lks$baseUrl)==0 ||
		length(curFolderPath(lks))==0 ||
		length(curSchema(lks))==0 ||
		length(curQueryName(lks))==0 )
	{
		stop("You must set curSchema and curQuery first")
	}
			
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
	cat("Folder Path:  ", curFolderPath(x), "\n")
	cat("Current Schema:  ", curSchemaName(x), "\n")
	cat("Current Query:  ", curQueryName(x), "\n")
	
	csname <- curSchemaName(x)
	cqname <- curQueryName(x)
	
	if (length(csname)>0) {
		if (length(cqname)==0) {
			cat("Available schemas: \n\t")			
			for (i in lsSchemas(x)) {
				cat(as.character(i),"\n\t",sep="")
			}	
			cat("\n")
		}
		else {
			cat("Available queries: \n\t")			
			for (i in lsQueries(x)) {
				cat(as.character(i),"\n\t",sep="")
			}	
			cat("\n")			
		}
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

print.LabkeyQuery <-
    function(x, ..., pad="")
{
	if (length(names(x)) == 0) {
		cat("Get field information on a query by making it current using curQuery()<-\"query name\" \n")
	}
    	cat("Available fields: \n")
    	for (i in names(x)) {
	  	cat(i, "\t")
		print(x[[i]])
	}
}

.LabkeyField <-
    function(x, ...)
{
    structure(as.list(x, ...),
              class="LabkeyField")
}

print.LabkeyField <-
    function(x, ..., pad="")
{
    with(x, {
		cat(" caption: ", caption 
#		, " name: ", fieldName
		, " type: ", type
		, pad="\t")
	if (!is.na(lookupQueryName) ) {cat ("  lookup to ",lookupSchemaName,"$", lookupQueryName, sep="") }	
	cat("\n")
    })
}

