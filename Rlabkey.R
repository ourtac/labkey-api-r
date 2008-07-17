#-----------------------------------------------
#-----------------------------------------------
# filename:	Rlabkey.R
# purpose:	Fetch data from labkey server 
# author:	vobenchain
# date:		July 2008
#-----------------------------------------------
#-----------------------------------------------


rm(list=ls())
library(rjson)
library(RCurl)
library(XML)


## define variables
username <- "vobencha@fhcrc.org"
password <- "testpwd"
schemaName <- "lists"
queryName <- "Valerie"
project <- "SCHARP/Atlas%20Development/Test%20Study"
host <- "atlas.scharp.org"
labkeyroot <- "cpas"
protocol <- "http"

## build url
url <- paste(protocol,"://",host,"/",labkeyroot,"/query/",project,"/selectRows.api?schemaName=",schemaName,"&query.queryName=",queryName,sep="")

## encode username and password
backend <- base64(paste(username,":",password,sep=""))
encodepwd <- paste("Basic",backend)


#--------------------------
# see it all
myopts <- curlOptions(header=TRUE, httpheader=c(Authorization=encodepwd), verbose=TRUE, ssl.verifyhost=FALSE, ssl.verifypeer=FALSE, followlocation=TRUE)
myfile <- getURL(url,.opts=myopts)
htmlTreeParse(myfile, asText=TRUE)
#--------------------------



#--------------------------
# everything but the header
rm(myopts myfile)
myopts <- curlOptions(httpheader=c(Authorization=encodepwd), ssl.verifyhost=FALSE, ssl.verifypeer=FALSE, followlocation=TRUE)
#opts <- curlOptions(userpwd="vobencha@fhcrc.org:testpwd", netrc=TRUE, netrc.file="/home/vobencha/.netrc", verbose=TRUE, ssl.verifyhost=FALSE, ssl.verifypeer=FALSE, followlocation=TRUE)
myfile <- getURL(url,.opts=myopts)
decode <- fromJSON(myfile)
names(decode)
decode$rows
decode$rows[[1]]
#--------------------------

#--------------------------
# try parsing the header and getting error code
g = basicTextGatherer()
d = basicHeaderGatherer()
handle <- getCurlHandle()
#body <- curlOptions(header=TRUE, headerfunction=h, httpheader=c(Authorization=encodepwd), ssl.verifyhost=FALSE, ssl.verifypeer=FALSE, followlocation=TRUE)
myopts <- curlOptions(writefunction=g, header=TRUE, headerfunction=d, httpheader=c(Authorization=encodepwd), ssl.verifyhost=FALSE, ssl.verifypeer=FALSE, followlocation=TRUE, curl=handle)
body <- getURI(url, .opts=myopts, curl=handle)



reader <- basicTextGatherer()
handle <- getCurlHandle()
curlPerform(url, httpheader <- headerFields, postfields <- body, writefunction <- reader$update, curl <- handle)
status <- getCurlInfo(handle)$response.code

#--------------------------
# just the header

rm(myopts myfile) 
h <- basicTextGatherer()
myopts <- curlOptions(header=TRUE, headerfunction=h[[1]], httpheader=c(Authorization=encodepwd), verbose=TRUE, ssl.verifyhost=FALSE, ssl.verifypeer=FALSE, followlocation=TRUE)
myfile <- getURL(url,.opts=myopts)
read.dcf(textConnection(paste(h$value(NULL)[-1], collapse="")))
#--------------------------




#
#//this command would create a new 'connection' object,
#//which would represent server connection information
#//the first param is the base URL, and the second would be the
#//password file, or a specific user name and password to use
#//(not sure if R supports params of variable types or not)
#
#connection <- labkey.newConnection("http://atlas.scharp.org", passwordFile)
#
#//this command would select some data from the lists
#//named 'test' in the 'project/folder' directory
#
#data <- connection.selectRows("project/folder", "lists", "test")
#
#//data should now be a data.frame suitable for use
#//with R functions
#//for example, if the list had a column named
#//'GooAmount' that contained numbers, I could
#//print a summary of those numbers like this
#
#print(summary(data$GooAmount))
#
#
#Sorting, filtering, and other optional parameters might be best handled by making the user construct a 'command' object, which exposes several methods/properties. This is where my R knowledge is too weak to suggest what it should look like, but in Java it would be something like this:
#
#Connection cn = new Connection(...);
#Command cmd = new Command("project/folder", "lists", "test");
#
#//add a filter, parameters are:
#// column name
#// filter value
#// operator (optional, defaults to equals)
#cmd.addFilter("Name", "Valerie", Filter.Operators.EQUALS);
#
#//add a sort, parameters are:
#// column name
#// direction (optional, defaults to ascending)
#cmd.addSort("GooAmount", Sort.Direction.DESCENDING);
#
#//perhaps you want to set a maximum number of rows?
#cmd.setMaxRows(1000);
#
#//execute the command and get the data
#Dataset ds = cmd.execute(cn);
#
