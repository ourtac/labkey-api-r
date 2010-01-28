\name{getLookups}
\alias{getLookups}
\title{ Get related data fields from Labkey server based on session values }
\description{ Retrieve a query object based on the values of folder, schema and query AND a name of a lookup column in that query }

\usage{
getLookups(lks, lookupField, ...) 
}
\arguments{
  \item{lks}{the session key returned from lkCreateSession}
  \item{lookupField}{the fieldKey value from a field object that has a non-NA value of the lookupQueryName }
  \item{...}{ }
}

\details{  Lookup fields in LabKey server are the equivalent of declared foreign keys
}

\value{ A query object of the same shape as returned by curQuery()
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

#cols <- c(names(curQuery(s)[2:6]), names(lucols)[2:4], names())

#alltypesDF <- getLookups(s, colSelect=cols)

}
\keyword{file}
