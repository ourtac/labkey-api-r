\name{getSchema}
\alias{getSchema}
\title{ Returns an object representing a Labkey schema }
\description{ Creates and returns an object representing a Labkey schema, containing child objects representing Labkey queries}

\usage{
getSchema(session, schemaIndex) 
}
\arguments{
  \item{session}{the session key returned from getSession}
  \item{schemaIndex}{the name of the schema that contains the table on which you want to base a query}
}
\details{  Creates and returns an object representing a Labkey schema, containing child objects representing Labkey queries
}

\value{an object representing the schema
}
\references{http://www.omegahat.org/RCurl/, \cr
http://dssm.unipa.it/CRAN/web/packages/rjson/rjson.pdf,\cr
https://www.labkey.org/project/home/begin.view}
\author{Peter Hussey}
\seealso{\code{\link{getSession}}}
\examples{

## the basics of using session, schema, and query objects

library(Rlabkey)

s<- getSession(baseUrl="http://localhost:8080/labkey", folderPath="/apisamples")

sch<- getSchema(s, "lists")

sch$AllTypes$Category          # can walk down the populataed schema tree from schema node or query node
sch$AllTypes$Category$caption
sch$AllTypes$Category$type

lucols <- getLookups(s, sch$AllTypes$Category)  # can add fields from related queries

cols <- c(names(sch$AllTypes[2:6]), names(lucols)[2:4])

getRows(s, sch$AllTypes, colSelect=cols)

}
\keyword{file}
