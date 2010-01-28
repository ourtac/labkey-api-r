\name{getRows}
\alias{getRows}
\title{ Retrieve data from Labkey server based on session values }
\description{ Retrive rows from a Labkey server given the current values of folder, schema and query }

\usage{
getRows(lks,viewName=NULL, colSelect=NULL, maxRows=NULL, rowOffset=NULL, colSort=NULL, colFilter=NULL, showHidden=FALSE
	, colNameOpt='fieldname') 
}
\arguments{
  \item{lks}{the session key returned from lkCreateSession}
  \item{viewName}{(optional) a string specifying the \code{viewName}}
  \item{colSelect}{(optional) a vector of comma separated strings specifying which columns of a dataset or view to import}
  \item{maxRows}{(optional) an integer specifying how many rows of data to return. If no value is specified, all rows are returned.}
  \item{colSort}{(optional) a string including the name of the column to sort preceeded by a \dQuote{+} or
\dQuote{-} to indicate sort direction}
  \item{rowOffset}{(optional) an integer specifying which row of data should be the first row in the retrieval. If no
value is specified, the retrieval starts with the first row.}
  \item{colFilter}{(optional) a vector or array object created by the \code{makeFilter} function which contains the
column name, operator and value of the filter(s) to be applied to the retrieved data.}
  \item{showHidden}{(optional) a logical value indicating whether or not to return data columns that would normally be hidden from user view. If no value is specified, the hidden columns are not returned. In versions 0.0.5 and earlier, this argument was called \code{stripAllHidden}. The \code{stripAllHidden} argument performed the same function as \code{showHidden} but with different logic. The change was made for clarity.}
  \item{colNameOpt}{Determines the source of the column names in the returned data frame .}
}

\details{  This function works as a shortcut wrapper to labkey.selectRows.  All of the arguments are the same as documented in
selectRows.  Note that with getRows the colNameOpt default is 'fieldname' instead of 'caption'.  

}

\value{ A data frame containing the query results corresponding to the default view of the specified query.
}
\references{http://www.omegahat.org/RCurl/, \cr
http://dssm.unipa.it/CRAN/web/packages/rjson/rjson.pdf,\cr
https://www.labkey.org/project/home/begin.view}
\author{Peter Hussey}
\seealso{\code{\link{lkCreateSession}}, \code{\link{labkey.selectRows}}}
\examples{

## Not run:
##
#library(Rlabkey)

#s<- lkCreateSession(baseUrl="http://localhost:8080/labkey", folderPath="/apisamples", schemaName="lists",queryName="AllTypes")

#q<- curQuery(s)
#q$Category          # can walk down the populataed schema tree from schema node or query node
#q$Category$caption
#q$Category$type

#lucols <- getLookups(s, "Category")  # can add fields from related queries

#cols <- c(names(curQuery(s)[2:6]), names(lucols)[2:4])

#alltypesDF <- getRows(s, colSelect=paste(cols, sep=","))

}
\keyword{file}
