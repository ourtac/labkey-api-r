\name{lsProjects}
\alias{lsProjects}
\title{List the projects available at a given Labkey Server address }
\description{ Lists the projects available.  Takes a string URL instead of a session, as it is intended for use before creating a session.}

\usage{
lsProjects(baseUrl) 
}
\arguments{
  \item{baseUrl}{a string specifying the \code{baseUrl}for the Labkey Server, of the form http://<server dns name>/<contextroot> }

}

\details{  Lists the available folder paths relative to the current folder path for a Labkey session.
}

\value{ A character array containing the available folder paths, relative to the root.  These values can be set on a session usiig curFolder<-
}
\references{http://www.omegahat.org/RCurl/, \cr
http://dssm.unipa.it/CRAN/web/packages/rjson/rjson.pdf,\cr
https://www.labkey.org/project/home/begin.view}
\author{Peter Hussey}
\seealso{\code{\link{lkCreateSession}}}
\examples{
\dontrun{

## Not run:
##
library(Rlabkey)
lsProjects("http://www.labkey.org")

lkorg <- lkCreateSession("http://www.labkey.org", "/home")
lsFolders(lkorg)

lkorg <- lkCreateSession("http://www.labkey.org", "/home/Study/ListDemo")
lsSchemas(lkorg)

}

}
\keyword{file}
