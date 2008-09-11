

labkey.executeSql <- function(baseUrl, folderPath, schemaName, sql, maxRows=NULL, rowOffset=NULL, stripAllHidden=TRUE)
    {  
        ## If maxRows and/or rowOffset are specified, set showAllRows=FALSE
        showAllRows=TRUE
        if(is.null(maxRows)==FALSE || is.null(rowOffset)==FALSE){showAllRows=FALSE}
        

        ## Error if any of baseUrl, folderPath, schemName or sql are missing
        if(exists("baseUrl")==FALSE || exists("folderPath")==FALSE || exists("schemaName")==FALSE || exists("sql")==FALSE)
        stop (paste("A value must be specified for each of baseUrl, folderPath, schemaName and sql."))
        
 		## URL encoding of schema and folder path
    	if(length(grep("%",schemaName))<1) {schemaName <- URLencode(schemaName)}
    	if(length(grep("%",folderPath))<1) {folderPath <- URLencode(folderPath)}
    #	if(length(grep("%",sql))<1) {sql <- URLencode(sql)}


    	## Replace backslashes if present and add forward slashes where needed
    	baseUrl <- gsub("[\\]", "/", baseUrl)
    	folderPath <- gsub("[\\]", "/", folderPath)
    	if(substr(baseUrl, nchar(baseUrl), nchar(baseUrl))!="/"){baseUrl <- paste(baseUrl,"/",sep="")}
    	if(substr(folderPath, nchar(folderPath), nchar(folderPath))!="/"){folderPath <- paste(folderPath,"/",sep="")}
    	if(substr(folderPath, 1, 1)!="/"){folderPath <- paste("/",folderPath,sep="")}


    	## Construct url 
    	myurl <- paste(baseUrl,"query",folderPath,"executeSql.api",sep="")


		## Set options
		reader <- basicTextGatherer()
        header <- basicTextGatherer()
        handle <- getCurlHandle()
		if(exists("labkey.sessionCookieName")==FALSE) 
		{myopts <- curlOptions(netrc=1, writefunction=reader$update, headerfunction=header$update, ssl.verifyhost=FALSE, 
								ssl.verifypeer=FALSE, followlocation=TRUE)} else
		{myopts <- curlOptions(cookie=paste(labkey.sessionCookieName,"=",labkey.sessionCookieContents,sep=""), 
								writefunction=reader$update, headerfunction=header$update, ssl.verifyhost=FALSE, 
								ssl.verifypeer=FALSE, followlocation=TRUE)}


		## Post form
		postForm(uri=myurl, "schemaName"=schemaName, "sql"=sql, .opts=myopts, curl=handle)

 
 
		## Error checking for incoming file
    	h <- parseHeader(header$value())
    	status <- getCurlInfo(handle)$response.code
    	message <- h$statusMessage


  		if(status==500) {decode <- fromJSON(reader$value()); message <- decode$exception; stop(paste("HTTP request was unsuccessful. Status code = ",status,", Error message = ",message,". Please check the spelling of the input variables.",sep=""))}

    	if(status>=400) stop (paste("HTTP request was unsuccessful. Status code = ",status,", Error message = ",message,". Please check the spelling of the input variables.",sep="")) else
    	{   ## Decode data and put in data frame
    	    newdata <- makeDF(reader$value(), stripAllHidden)}

    	    return(newdata)
    }
                                                              
