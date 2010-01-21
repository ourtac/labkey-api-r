
## public function getQueries, returns all queries associated with a specified schema
labkey.getQueries <- function(baseUrl, folderPath, schemaName)
{
mydata <- getQueryLists(baseUrl=baseUrl, folderPath=folderPath, schemaName=schemaName)
return(mydata)
}

## public function  getQueryViews, returns all views associated with a specified query 
labkey.getQueryViews <- function(baseUrl, folderPath, schemaName, queryName)
{
if(is.null(queryName)==FALSE) {char <- nchar(queryName); if(char<1){queryName<-NULL}}

if(exists("queryName")==FALSE)  { stop ("You must provide the query on which the view is based.") }
if(length(grep("%",queryName)) <1 ) { queryName <- URLencode(queryName) }

mydata <- getQueryLists(baseUrl=baseUrl, folderPath=folderPath, schemaName=schemaName, queryViewParent=queryName)
return(mydata)
}

##private implementation for the two calls above
getQueryLists <- function(baseUrl, folderPath, schemaName, queryViewParent=NULL)
{
## Error if any of baseUrl, folderPath, or schemName are missing
if(exists("baseUrl")==FALSE || exists("folderPath")==FALSE || exists("schemaName")==FALSE )
{stop ("A value must be specified for each of baseUrl, folderPath, schemaName.")}

## URL encoding of schemaName and folderPath
if(length(grep("%",schemaName))<1) {schemaName <- URLencode(schemaName)}
if(length(grep("%",folderPath))<1) {folderPath <- URLencode(folderPath)}

## Formatting
baseUrl <- gsub("[\\]", "/", baseUrl)
folderPath <- gsub("[\\]", "/", folderPath)
if(substr(baseUrl, nchar(baseUrl), nchar(baseUrl))!="/"){baseUrl <- paste(baseUrl,"/",sep="" )}
if(substr(folderPath, nchar(folderPath), nchar(folderPath))!="/"){folderPath <- paste(folderPath,"/",sep="")}
if(substr(folderPath, 1, 1)!="/"){folderPath <- paste("/",folderPath,sep="")}

## now setup the differeent columns for views vs queries
serverAction <- "getQueries.view?schemaName="
queryObjType <- "queries"
qParam<-""
columnNames<- c("queryName", "fieldName", "caption")

if (length(queryViewParent) > 0) {
	serverAction <- "getQueryViews.api?schemaName="  
	qParam <- paste("&queryName=",queryViewParent, sep="")
	queryObjType <- "views"
	columnNames<- c("viewName", "fieldName", "key")
}

## Construct url
myurl <- paste(baseUrl,"query",folderPath, serverAction, schemaName, qParam, "&apiVersion=8.3", sep="")

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

decode <- fromJSON(mydata)
qs <- decode[[queryObjType]]
	
dmall <- matrix(nrow=0, ncol=3, byrow=TRUE)

if (length(qs)>0)
{
	for (j in 1:length(qs))
	{
		dmq <- matrix(nrow=0, ncol=3, byrow=TRUE)
		nc <- length(qs[[j]]$columns)
		for (k in 1:nc) 
		{
			dmqrow<- matrix( cbind(qs[[j]]$name, qs[[j]]$columns[[k]]$name, qs[[j]]$columns[[k]][[columnNames[3]]]), ncol=3, byrow=FALSE)
			dmq<-rbind(dmq, dmqrow)
		}
		dmall <- rbind(dmall,dmq)
	}
}	
dfall <- as.data.frame(dmall, stringsAsFactors=FALSE)
colnames(dfall)<- columnNames

return(dfall)
}
