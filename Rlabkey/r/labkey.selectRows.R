

labkey.selectRows <- function(baseUrl, folderPath, schemaName, queryName, viewName=NULL, colSelect=NULL, 
						maxRows=NULL, rowOffset=NULL, colSort=NULL, colFilter=NULL, stripAllHidden=TRUE)
	{	
		## If maxRows and/or rowOffset are specified, set showAllRows=FALSE
		showAllRows=TRUE
		if(is.null(maxRows)==FALSE || is.null(rowOffset)==FALSE){showAllRows=FALSE}


	
		## Error if any of baseUrl, folderPath, schemName or queryName are missing
		if(exists("baseUrl")==FALSE || exists("folderPath")==FALSE || exists("schemaName")==FALSE || exists("queryName")==FALSE)
		stop (paste("There must be a value specified for each of baseUrl, folderPath, schemaName and queryName. These 
are required fields.")) 



 		## URL encoding of schema, query, view and folder path
    	if(length(grep("%",schemaName))<1) {schemaName <- URLencode(schemaName)}
    	if(length(grep("%",queryName))<1) {queryName <- URLencode(queryName)}
    	if(length(grep("%",folderPath))<1) {folderPath <- URLencode(folderPath)}
    	if(is.null(viewName)==FALSE) {if(length(grep("%",viewName))<1) viewName <- URLencode(viewName)}


    	## Format colSelect
    	if(is.null(colSelect)==FALSE)
    	    {   holder <- NULL
    	        for(i in 1:length(colSelect)) holder <-paste(holder,colSelect[i],",",sep="")
    	        colSelect <- substr(holder, 1, nchar(holder)-1)}


    	## Replace backslashes if present and add forward slashes where needed
    	baseUrl <- gsub("[\\]", "/", baseUrl)
    	folderPath <- gsub("[\\]", "/", folderPath)
    	if(substr(baseUrl, nchar(baseUrl), nchar(baseUrl))!="/"){baseUrl <- paste(baseUrl,"/",sep="")}
    	if(substr(folderPath, nchar(folderPath), nchar(folderPath))!="/"){folderPath <- paste(folderPath,"/",sep="")}
    	if(substr(folderPath, 1, 1)!="/"){folderPath <- paste("/",folderPath,sep="")}


    	## Construct url 
    	myurl <- paste(baseUrl,"query",folderPath,"selectRows.api?schemaName=",schemaName,"&query.queryName=",queryName,sep="")
    	if(is.null(viewName)==FALSE) {myurl <- paste(myurl,"&query.viewName=",viewName,sep="")}
    	if(is.null(colSelect)==FALSE) {myurl <- paste(myurl,"&query.columns=",colSelect,sep="")}
    	if(is.null(maxRows)==FALSE) {myurl <- paste(myurl,"&query.maxRows=",maxRows,sep="")}
    	if(is.null(rowOffset)==FALSE) {myurl <- paste(myurl,"&query.offset=",rowOffset,sep="")}
    	if(is.null(colSort)==FALSE) {myurl <- paste(myurl,"&query.sort=",colSort,sep="")}
    	if(is.null(colFilter)==FALSE) {for(j in 1:length(colFilter)) myurl <- paste(myurl,"&query.",colFilter[j],sep="")}
    	if(showAllRows==TRUE) {myurl <- paste(myurl,"&query.showAllRows=",showAllRows,sep="")}


		## Set options 
		reader <- basicTextGatherer()
		header <- basicTextGatherer()
		myopts <- curlOptions(writefunction=reader$update, headerfunction=header$update, netrc=1, ssl.verifyhost=FALSE, ssl.verifypeer=FALSE, followlocation=TRUE)
		## Http request
		# Check for labkeycookie
		handle <- getCurlHandle()
		if(exists("labkey.sessionCookieName")==FALSE) {mydata <- getURI(myurl, .opts=myopts, curl=handle)} else
		{mydata <- getURI(myurl, .opts=myopts, cookie=paste(labkey.sessionCookieName,"=",labkey.sessionCookieContents,sep=""))}

		## Error checking for incoming file
		h <- parseHeader(header$value())
		status <- getCurlInfo(handle)$response.code
		#status <- h$status
		message <- h$statusMessage
		
		if(status==500) {decode <- fromJSON(reader$value()); message <- decode$exception; stop(paste("HTTP request was unsuccessful. Status code = ",status,", Error message = ",message,". Please check the spelling of schemaName, queryName, folderPath and viewName is applicable.",sep=""))}

		if(status>=400)	stop (paste("HTTP request was unsuccessful. Status code = ",status,", Error message = ",message,". Please check the spelling of the schemaName, queryName, folderPath and viewName if applicable.",sep="")) else
		{	## Decode data and put in data frame
			newdata <- makeDF(mydata, stripAllHidden)}

			return(newdata)
	} 
