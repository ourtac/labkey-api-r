

labkey.selectRows <- function(mypwd, baseUrl, folderPath, schemaName, queryName, viewName=NULL, colSelect=NULL, 
						maxRows=NULL, rowOffset=NULL, colSort=NULL, colFilter=NULL, keepKey=FALSE)
	{	## Set up options
		reader <- basicTextGatherer()
		header <- basicTextGatherer()
		handle <- getCurlHandle()
		# If maxRows and/or rowOffset are specified, set showAllRows=FALSE
		showAllRows=TRUE
		if(is.null(maxRows)==FALSE || is.null(rowOffset)==FALSE){showAllRows=FALSE}
		# Check for labkeycookie
		if(exists("valuelabkeycookie")==FALSE)
			{	myopts <- curlOptions(	netrc=1, httpheader=c(Authorization=mypwd, Accept="test/xml", 
										Accept="multipart/*", 'Content-Type'="text/xml; charset=utf-8"), 
										postfields=body, writefunction=reader$update, headerfunction=header$update, 
										ssl.verifyhost=FALSE, ssl.verifypeer=FALSE, followlocation=TRUE)} else
			{	myopts <- curlOptions(	cookie=paste("namelabkeycookie=",valuelabkeycookie,sep=""), httpheader=c( 
										Accept="test/xml", Accept="multipart/*", 'Content-Type'="text/xml; 
										charset=utf-8"), postfields=body, writefunction=reader$update, 
										headerfunction=header$update, ssl.verifyhost=FALSE, ssl.verifypeer=FALSE, 
										followlocation=TRUE)}
	
		## Error if any of baseUrl, folderPath, schemName or queryName are missing
		if(exists("baseUrl")==FALSE || exists("folderPath")==FALSE || exists("schemaName")==FALSE || exists("queryName")==FALSE)
		stop (paste("There must be a value specified for each of baseUrl, folderPath, schemaName and queryName. These 
are required fields.")) 



		## Make url
		myUrl <- makeUrl(baseUrl, folderPath, schemaName, queryName, viewName, colSelect, maxRows, rowOffset, colSort, colFilter, showAllRows)

		## Http request
		curlPerform(url=myUrl, .opts=myopts, curl=handle)

		## Error checking for incoming file
		h <- parseHeader(header$value())
		#status <- getCurlInfo(handle)$response.code
		status <- h$status
		message <- h$statusMessage


		# put checking for 500 "exception" here when figure out how to reproduce a 500 error
		#if(status=500( { decode <- fromJSON(reader$value()); 
		
		if(status>=400)	stop (paste("HTTP request was unsuccessful. Status code = ",status,", Error message = ",message,". Please check the spelling of the schemaName, queryName, folderPath and viewName if applicable.",sep="")) else
		{	## Decode data and put in data frame
			newdata <- makeDF(reader$value(), keepKey)}

			return(newdata)
	} 
