\name{lsFolders}
\alias{lsFolders}
\title{List the available folder paths }
\description{ Lists the available folder paths relative to the current folder path for a Labkey session }

\usage{
lsFolders(lks, subfolders=TRUE, ...) 
}
\arguments{
  \item{lks}{the session key returned from lkCreateSession}
  \item{subfolders}{whether the folder list returned should recurse down to all subfolders below the current folder }
  \item{...}{}
}

\details{  Lists the available folder paths relative to the current folder path for a Labkey session 
}

\value{ A character array containing the available folder paths, relative to the root.  These values can be set on a session usiig curFolder<-
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

#curFolder(lks)  #returns project folder "/apisamples"

#lsFolders(lks) #returns values "/apisamples" , "/apisamples/sub1" ,"/apisamples/sub1/child" ,"/apisamples/sub2"  

}
\keyword{file}
