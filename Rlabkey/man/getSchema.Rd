\name{getSchema}
\alias{getSchema}
\title{ Returns an object representing a Labkey schema }
\description{ Creates and returns an object representing a Labkey schema, containing child objects representing Labkey queries}

\usage{
getSchema(lks, schemaIndex) 
}
\arguments{
  \item{lks}{the session key returned from lkCreateSession}
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
\seealso{\code{\link{lkCreateSession}}}
\examples{

## Not run:
##
#library(Rlabkey)

#s<- lkCreateSession(baseUrl="http://localhost:8080/labkey", folderPath="/apisamples")

#sch<- getSchema(s, "lists")
#sch$AllTypes
#sch$AllTypes$Category          # can walk down the populataed schema tree from schema node or query node
#sch$AllTypes$Category$caption
#sch$AllTypes$Category$type

#lucols <- getLookups(s, sch$AllTypes, "Category")  # can add fields from related queries

#cols <- c(names(curQuery(s)[2:6]), names(lucols)[2:4], names())

#getRows(s, sch$AllTypes, colSelect=cols)

## End(Not run)

}
\keyword{file}
