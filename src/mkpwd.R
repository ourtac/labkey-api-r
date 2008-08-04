mkpwd <- function(loc.labkey)	
	{	## Get password and username in .labkey file and encode

		
		if(substr(loc.labkey, nchar(loc.labkey), nchar(loc.labkey))!="/"){loc.labkey <- paste(loc.labkey,"/",sep="")}
   		if(substr(loc.labkey, 1, 1)!="/"){loc.labkey <- paste("/",loc.labkey,sep="")}
 
		getinfo <- read.table(paste(loc.labkey,".labkey",sep=""))
		u.ind <- which(getinfo[[1]]=="username")
		p.ind <- which(getinfo[[1]]=="password")
		username <- getinfo[u.ind,2]
		password <- getinfo[p.ind,2]
		backend <- base64(paste(username,":",password,sep=""))
		encodepwd <- paste("Basic",backend)
		return(encodepwd) 
	}
