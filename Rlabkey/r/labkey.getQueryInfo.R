##labkey.getLookupDetails
labkey.getLookupDetails <- function(baseUrl, folderPath, schemaName, queryName, lookupKey)
{
if(exists("lookupKey")==FALSE )
{stop ("You must supply the key (name) value of a query field defined as a lookup type field.")}

lookupFields <- getQueryInfo(baseUrl=baseUrl, folderPath=folderPath, schemaName=schemaName, queryName=queryName,showDefaultView=FALSE, lookupKey=lookupKey)
return(lookupFields)
}

##Public getQueryDetails
labkey.getQueryDetails <- function(baseUrl, folderPath, schemaName, queryName)
{
queryDetails <- getQueryInfo(baseUrl=baseUrl, folderPath=folderPath, schemaName=schemaName, queryName=queryName,showDefaultView=FALSE)
return(queryDetails)
}

## Public getDefaultViewDetails
labkey.getDefaultViewDetails <- function(baseUrl, folderPath, schemaName, queryName)
{
viewDetails <- getQueryInfo(baseUrl=baseUrl, folderPath=folderPath, schemaName=schemaName, queryName=queryName,showDefaultView=TRUE)
return(viewDetails)
}

## internal reoutine that handles all of these
getQueryInfo <- function(baseUrl, folderPath, schemaName, queryName, showDefaultView=FALSE, lookupKey=NULL)
{
## Error if any of baseUrl, folderPath, schemName or queryName are missing
if(exists("baseUrl")==FALSE || exists("folderPath")==FALSE || exists("schemaName")==FALSE || exists("queryName")==FALSE )
{stop ("A value must be specified for each of baseUrl, folderPath, schemaName, and queryName.")}

if(is.null(lookupKey)==FALSE) {char <- nchar(lookupKey); if(char<1) {lookupKey<-NULL} }

## URL encoding 
if(length(grep("%",schemaName))<1) {schemaName <- URLencode(schemaName)}
if(length(grep("%",queryName))<1) {queryName <- URLencode(queryName)}
if(length(grep("%",folderPath))<1) {folderPath <- URLencode(folderPath)}
if(is.null(lookupKey)==FALSE) {if(length(grep("%",lookupKey))<1) lookupKey <- URLencode(lookupKey)}

## Formatting
baseUrl <- gsub("[\\]", "/", baseUrl)
folderPath <- gsub("[\\]", "/", folderPath)
if(substr(baseUrl, nchar(baseUrl), nchar(baseUrl))!="/"){baseUrl <- paste(baseUrl,"/",sep="" )}
if(substr(folderPath, nchar(folderPath), nchar(folderPath))!="/"){folderPath <- paste(folderPath,"/",sep="")}
if(substr(folderPath, 1, 1)!="/"){folderPath <- paste("/",folderPath,sep="")}

## Construct url
myurl <- paste(baseUrl,"query",folderPath,"getQueryDetails.api?schemaName=", schemaName, "&queryName=", queryName, "&apiVersion=8.3", sep="")
if(is.null(lookupKey)==FALSE) {myurl <- paste(myurl,"&fk=",lookupKey,sep="")}

## Set options
reader <- basicTextGatherer()
header <- basicTextGatherer()
myopts <- curlOptions(writefunction=reader$update, headerfunction=header$update, netrc=1, ssl.verifyhost=FALSE, ssl.verifypeer=FALSE, followlocation=TRUE)

## Http get
handle <- getCurlHandle()
clist <- ifcookie()
if(clist$Cvalue==1) {mydata <- getURI(myurl, .opts=myopts, cookie=paste(clist$Cname,"=",clist$Ccont,sep=""))} else {mydata <- getURI(myurl, .opts=myopts, curl=handle)}

## Error checking, decode data and return data frame
h <- parseHeader(header$value())
status <- getCurlInfo(handle)$response.code
message <- h$statusMessage

if(status==500)
{decode <- fromJSON(mydata); message <- decode$exception; stop(paste("HTTP request was unsuccessful. Status code = ",status,", Error message = ",message,sep=""))}
if(status>=400)
  {contTypes <- which(names(h)=='Content-Type')
  if(length(contTypes)>1 & h[contTypes[2]]=="application/json;charset=utf-8")
      {decode <- fromJSON(mydata); message<-decode$exception; stop (paste("HTTP request was unsuccessful. Status code = ",status,", Error message = ",message,sep=""))} else
  {stop(paste("HTTP request was unsuccessful. Status code = ",status,", Error message = ",message,sep=""))}}

    ## substitute null literals for NA so the data frame gets constructed properly
mydata <- gsub("null", "\"NA\"", mydata)
decode <- fromJSON(mydata)


## If querying the default view, the metadata is in a differnt object in the json stream 
qcs <- decode$columns
if (showDefaultView==TRUE) {qcs<-decode$defaultView$columns}

if (length(qcs)==0) {stop(paste("No results found.  Check the queryName parameter.", sep=""))}

## if looking for the potential fields to add via lookups, the name returned has a different meaning
fieldNameCol <- "fieldName"
if(is.null(lookupKey)==FALSE) {fieldNameCol <- "relativeKey"}

dmall <- matrix(nrow=0, ncol=19, byrow=TRUE)

for (j in 1:length(qcs))
{
	{
		dmqrow<- matrix(data=cbind(decode$name[[1]], qcs[[j]]$name, qcs[[j]]$caption, qcs[[j]]$fieldKey, qcs[[j]]$type, qcs[[j]]$isNullable, qcs[[j]]$isKeyField, 
			qcs[[j]]$isAutoIncrement, qcs[[j]]$isVersionField, qcs[[j]]$isHidden, qcs[[j]]$isSelectable, 
			qcs[[j]]$isUserEditable, qcs[[j]]$isReadOnly, qcs[[j]]$isMvEnabled
			), ncol=14, byrow=FALSE)
		if (is.null(qcs[[j]]$lookup))
		{
			lookupinfo <- matrix(data=cbind(NA,NA,NA,NA,NA), ncol=5, byrow=FALSE)
		}			
		else
		{
			lookupinfo <- matrix(data=cbind(qcs[[j]]$lookup$keyColumn,qcs[[j]]$lookup$schemaName,qcs[[j]]$lookup$displayColumn,
			qcs[[j]]$lookup$queryName,qcs[[j]]$lookup$isPublic), ncol=5, byrow=FALSE)
		}
		dmqrow<-cbind(dmqrow, lookupinfo)
	}
	dmall <- rbind(dmall,dmqrow)
}
dfall <- as.data.frame(dmall, stringsAsFactors=FALSE)
colnames(dfall)<-c("queryName", fieldNameCol, "caption", "fieldKey", "type", "isNullable","isKeyField",
			"isAutoIncrement", "isVersionField","isHidden","isSelectable",
			"isUserEditable", "isReadOnly", "isMvEnabled", 
			"lookupKeyField","lookupSchemaName","lookupDisplayField", "lookupQueryName", "lookupIsPublic")

return(dfall)
}
