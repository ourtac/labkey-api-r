

labkey.selectRows <- function(mypwd, baseUrl, folderPath, schemaName, queryName, viewName=NULL, columns=NULL, 
								maxRows=NULL, rowOffset=NULL, colSort=NULL, colFilter=NULL, showAllRows=TRUE)
	{	## Set up options
		reader <- basicTextGatherer()
		header <- basicTextGatherer()
		handle <- getCurlHandle()
		# If maxRows and/or rowOffset are specified, set showAllRows=FALSE
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


		## Make url and encode special characters
	#	schemaName <- URLencode(schemaName)
    #	queryName <- URLencode(queryName)
	#	viewName <- URLencode(viewName)
		myUrl <- makeUrl(baseUrl, folderPath, schemaName, queryName, viewName, columns, maxRows, rowOffset, colSort, colFilter, showAllRows)

		## Https request
		curlPerform(url=myUrl, .opts=myopts, curl=handle)

		## Error checking for incoming file
		h <- parseHTTPHeader(header$value())
		status <- getCurlInfo(handle)$response.code
		status2 <- h$status
		#message2 <- names(h[11])
		message <- h$statusMessage

		if(status>=400)	stop (paste("HTTP request was unsuccessful. Status code =",status, ", with error message '",message,"' Please check the spelling of the schemaName, queryName, folderPath and viewName if applicable.",sep=""))

		
		# put checking for 500 "exception" here when figure out how to reproduce a 500 error
		#if(status=500( { decode <- fromJSON(reader$value()); 


		{	decode <- fromJSON(reader$value())
			

			## Put data in data frame 
			hold.dat <- NULL
			for(j in 1:length(decode$rows)) {hold.dat <- rbind(hold.dat, decode$rows[[j]])}
			hold.dat <- as.data.frame(hold.dat)


			## Get column names in proper order, associated header index, hidden tag, and data type
			cnames <- NULL
			hindex <- NULL
			hide <- NULL
			for(j in 1:length(decode$columnModel))
				{	cnames <- c(cnames, decode$columnModel[[j]]$header)
					hindex <- c(hindex, decode$columnModel[[j]]$dataIndex)
					hide <- c(hide, decode$columnModel[[j]]$hidden)}
			refdf <- data.frame(cnames,hindex,hide)
			oindex <- NULL
			for(k in 1:length(names(hold.dat))){oindex <- rbind(oindex, which(names(hold.dat)==refdf$hindex[k]))}
			refdf$oindex <- oindex
			refdf$type <- NULL
			for(p in 1:dim(refdf)[1])
				{	ind <- which(refdf$hindex==decode$metaData$fields[[p]]$name)
					refdf$type[ind] <- decode$metaData$fields[[p]]$type}

			## Reorder the columns to match what the user saw on the web site
			newdat <- NULL
			for(i in 1:length(cnames)){	newdat <- cbind(newdat, hold.dat[,refdf$oindex[i]])}
			newdat <- as.data.frame(newdat)
			names(newdat) <- cnames  

			## Set the mode and convert the dates
			index <- NULL
			for(j in 1:ncol(newdat)) 
				{	mod <- refdf$type[j]
					if(mod=="date"){ newdat[j] <- as.Date(as.character(newdat[,j]), "%d %b %Y %H:%M:%S %Z")}else
					if(mod=="string"){mode(newdat[,j]) <- "character"} else
					if(mod=="int"){ findex <- which(newdat[,j]=="FALSE")
									if(length(findex>0)) {newdat[findex,j] <- NA}
									mode(newdat[,j]) <- "numeric"} else
					if(mod=="boolean"){mode(newdat[,j]) <- "logical"} else
					if(mod=="float"){mode(newdat[,j]) <- "numeric"} else
     				{print("MetaData field type not recognized.")}
                }

			# Hide key column??

			}
			return(newdat)
		} 
