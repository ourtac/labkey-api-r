\name{lsSchemas}
\alias{lsSchemas}
\title{List the available queries }
\description{ Lists the available schemas given the current folder path for a Labkey session }

\usage{
lsSchemas(lks, ...) 
}
\arguments{
  \item{lks}{the session key returned from lkCreateSession}
  \item{...}{}
}

\details{ Lists the available schemas given the current folder path for a Labkey session 
}

\value{ A character array containing the available schema names
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

#curSchema(lks)  #returns "lists"

#lsSchemas(lks) #returns several schema names, e.g. "lists", "core", "MS1", etc.

}
\keyword{file}
