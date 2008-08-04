
makeUrl <- function(baseUrl, folderPath, schemaName, queryName, viewName, columns, maxRows, rowOffset, colSort, colFilter, showAllRows)
	{ ## Formatting	
	  # replace potential backslashes by forward

	  if(substr(baseUrl, nchar(baseUrl), nchar(baseUrl))!="/"){baseUrl <- paste(baseUrl,"/",sep="")}
	  if(substr(folderPath, nchar(folderPath), nchar(folderPath))!="/"){folderPath <- paste(folderPath,"/",sep="")}
      if(substr(folderPath, 1, 1)!="/"){folderPath <- paste("/",folderPath,sep="")}
     

	  ## Construct url 
	  res <- paste(baseUrl,"query",folderPath,"selectRows.api?schemaName=",schemaName,"&query.queryName=",queryName,sep="")
	  if(is.null(viewName)==FALSE) {res <- paste(res,"&query.viewName=",viewName,sep="")}
	  if(is.null(columns)==FALSE) {res <- paste(res,"&query.columns=",columns,sep="")}
	  if(is.null(maxRows)==FALSE) {res <- paste(res,"&query.maxRows=",maxRows,sep="")}
	  if(is.null(rowOffset)==FALSE) {res <- paste(res,"&query.offset=",rowOffset,sep="")}
	  if(is.null(colSort)==FALSE) {res <- paste(res,"&query.sort=",colSort,sep="")}
	  if(is.null(colFilter)==FALSE) {res <- paste(res,"&query.",colFilter,sep="")}
	  if(showAllRows==TRUE) {res <- paste(res,"&query.showAllRows=",showAllRows,sep="")}

	  return(res)
	} 
