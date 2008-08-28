makeDF <- function(rawdata, stripAllHidden)
{	decode <- fromJSON(rawdata)

  	## Put data in data frame 
  	hold.dat <- NULL
  	for(j in 1:length(decode$rows)) {hold.dat <- rbind(hold.dat, decode$rows[[j]])}
  	hold.dat <- as.data.frame(hold.dat)


  	## Get column names in proper order, associated header index, hidden tag, and data type
  	cnames <- NULL
  	hindex <- NULL
  	hide <- NULL
  	for(j in 1:length(decode$columnModel))
  	    {   cnames <- c(cnames, decode$columnModel[[j]]$header)
  	        hindex <- c(hindex, decode$columnModel[[j]]$dataIndex)
  	        hide <- c(hide, decode$columnModel[[j]]$hidden)}
  	refdf <- data.frame(cnames,hindex,hide)
  	oindex <- NULL
  	for(k in 1:length(names(hold.dat))){oindex <- rbind(oindex, which(names(hold.dat)==refdf$hindex[k]))}
  	refdf$oindex <- oindex
  	refdf$type <- NULL
  	for(p in 1:dim(refdf)[1])
  	    {   ind <- which(refdf$hindex==decode$metaData$fields[[p]]$name)
  	        refdf$type[ind] <- decode$metaData$fields[[p]]$type}

  	newdat <- NULL
  	for(i in 1:length(cnames)){ newdat <- cbind(newdat, hold.dat[,refdf$oindex[i]])}
  	newdat <- as.data.frame(newdat)
  	names(newdat) <- cnames


  	## Delete key column unless keepkey=TRUE
  	if(stripAllHidden==FALSE)   {} else
  	                    {   if(is.null(decode$metaData$id)) {} else
  	                            {   hide.ind <- which(refdf$hide==TRUE)
  	                                newdat <- newdat[,-hide.ind]
									refdf <- refdf[-hide.ind,]}
  	                    }

  	## Set the mode and convert the dates
  	index <- NULL
  	for(j in 1:ncol(newdat))
  	    {   mod <- refdf$type[j]
  	        if(mod=="date"){ newdat[j] <- as.Date(as.character(newdat[,j]), "%d %b %Y %H:%M:%S %Z")}else
  	        if(mod=="string"){mode(newdat[,j]) <- "character"} else
  	        if(mod=="int"){ mode(newdat[,j]) <- "numeric"} else
  	        if(mod=="boolean"){mode(newdat[,j]) <- "logical"} else
  	        if(mod=="float"){mode(newdat[,j]) <- "numeric"} else
  	        {print("MetaData field type not recognized.")}
  	    }
  	
  	return(newdat)
}

