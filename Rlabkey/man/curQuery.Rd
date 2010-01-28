\name{curQuery}
\alias{curQuery}
\alias{curQueryName}
\alias{curQuery<-}
\title{Set or get the current query }
\description{ Sets or gets the current query name and object for a Labkey session }

\usage{
curQuery(lks, ...) 
}
\arguments{
  \item{lks}{the session key returned from lkCreateSession}
  \item{...}{}
}
\usage{
curQueryName(lks, ...) 
}
\arguments{
  \item{lks}{the session key returned from lkCreateSession}
  \item{...}{}
}
\usage{
curQuery(lks, ...) <- queryName
}
\arguments{
  \item{lks}{the session key returned from lkCreateSession}
  \item{...}{}
  \item{value}{a string specifying a query name}
}

\details{  Sets the current query, or returns the currenct query in object or string form.
}

\value{an object representing the query
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

#s<- lkCreateSession(baseUrl="http://localhost:8080/labkey", folderPath="/apisamples", schemaName="lists",queryName="AllTypes")

#q<- curQuery(s)
#q$Category          # can walk down the populataed schema tree from schema node or query node
#q$Category$caption
#q$Category$type

#lucols <- getLookups(s, "Category")  # can add fields from related queries
#cols <- c(names(curQuery(s)[2:6]), names(lucols)[2:4], names())

#getRows(s, colSelect=cols)

}
\keyword{file}
