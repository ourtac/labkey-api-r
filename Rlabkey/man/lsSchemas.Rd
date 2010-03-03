\name{lsSchemas}
\alias{lsSchemas}
\title{List the available queries }
\description{ Lists the available schemas given the current folder path for a Labkey session }

\usage{
lsSchemas(session) 
}
\arguments{
  \item{session}{the session key returned from getSession}
}

\details{ Lists the available schemas given the current folder path for a Labkey session 
}

\value{ A character array containing the available schema names
}
\references{http://www.omegahat.org/RCurl/, \cr
http://dssm.unipa.it/CRAN/web/packages/rjson/rjson.pdf,\cr
https://www.labkey.org/project/home/begin.view}
\author{Peter Hussey}
\seealso{\code{\link{getSession}}}
\examples{

##  get a list of schemas available in the current session context
library(Rlabkey)

lks<- getSession(baseUrl="http://localhost:8080/labkey", folderPath="/apisamples")

lsSchemas(lks) #returns several schema names, e.g. "lists", "core", "MS1", etc.

}
\keyword{file}
