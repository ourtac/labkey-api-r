\name{getRows}
\alias{getRows}
\title{ Retrieve data from Labkey server based on session values }
\description{ Retrive rows from a Labkey server given the current values of folder, schema and query }

\usage{
getRows(lks, query, maxRows=NULL, colNameOpt='fieldname', ...) 
}
\arguments{
  \item{lks}{the session key returned from lkCreateSession}
  \item{query}{an object representing a query on LabKey Server, a child object of the object returned by \code{getSchema()}}
  \item{maxRows}{(optional) an integer specifying how many rows of data to return. If no value is specified, all rows are returned.}
 \item{colNameOpt}{(optional) controls the name source for the columns of the output dataframe, with valid values of 'caption', 'fieldname', and 'rname'}
 \item{...}{Any of the remaining options to labkey.selectRows}
}

\details{  This function works as a shortcut wrapper to labkey.selectRows.  All of the arguments are the same as documented in
selectRows. 

Note tha with getRows the colNameOpt default is 'fieldname' instead of 'caption'.  

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
#s<-lkCreateSession(baseUrl="http://localhost:8080/labkey", folderPath="/apisamples")
#s  # shows schemas

#scobj <- getSchema(s, "lists")
#scobj   # shows available queries

#scobj$AllTypes  ## this is the query object

#getRows(s, scobj$AllTypes)
	

}
\keyword{file}
