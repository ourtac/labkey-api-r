\name{getLookups}
\alias{getLookups}
\title{ Get related data fields from Labkey server based on session values }
\description{ Retrieve a query object based on the values of folder, schema and query AND a name of a lookup column in that query }

\usage{
getLookups(lks, query, lookupField) 
}
\arguments{
  \item{lks}{the session key returned from lkCreateSession}
  \item{query}{an object representing a query on LabKey Server, a child object of the object returned by \code{getSchema()}}
  \item{lookupField}{the fieldKey value from a field object that has a non-NA value of the lookupQueryName, see example }
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
#LIbrary(Rlabkey)
#s<-lkCreateSession(baseUrl="http://localhost:8080/labkey", folderPath="/apisamples")
#s  # shows schemas

#scobj <- getSchema(s, "lists")
#scobj   # shows available queries

#scobj$AllTypes  ## this is the query object

#lkdata<- getRows(s, scobj$AllTypes)  # shorthand for labkey.selectRows, all the same args apply
#lkdata

#lucols <- getLookups(s, scobj$AllTypes, "Category")  # can add fields from related queries
#lucols

#lucols2 <- getLookups(s, scobj$AllTypes, "Category/Group")  # keep going to other tables

#cols <- c(names(scobj$AllTypes)[2:6], names(lucols)[2:4])

#getRows(s, scobj$AllTypes, colSelect=paste(cols, sep=","))

}
\keyword{file}
