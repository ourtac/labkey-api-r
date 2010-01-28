\name{lsQueries}
\alias{lsQueries}
\title{List the available queries }
\description{ Lists the available queries given the current folder path and schema for a Labkey session }

\usage{
lsQueries(lks, ...) 
}
\arguments{
  \item{lks}{the session key returned from lkCreateSession}
  \item{...}{}
}

\details{  Lists the available queries given the current folder path and schema for a Labkey session 
}

\value{ A character array containing the available queries
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

#curQuery(lks)  #returns "AllTypes"

#lsQueries(lks) #returns several query names

}
\keyword{file}
