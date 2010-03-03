\name{getFolderPath}
\alias{getFolderPath}
\title{Reutrns the folder path associated with a session}
\description{ Returns the current folder path for a Labkey session }

\usage{
getFolderPath(session) 
}
\arguments{
  \item{session}{the session key returned from getSession}
}

\details{  Returns a string containing the current folder path for the passed in Labkey session 
}

\value{ A character array containing the folder path, relative to the root. 
}
\references{http://www.omegahat.org/RCurl/, \cr
http://dssm.unipa.it/CRAN/web/packages/rjson/rjson.pdf,\cr
https://www.labkey.org/project/home/begin.view}
\author{Peter Hussey}
\seealso{\code{\link{getSession}}}
\examples{


## list available folders
library(Rlabkey)

lks<- getSession(baseUrl="http://localhost:8080/labkey", folderPath="/apisamples")
getFolderPath(lks)  #returns "/apisamples"

lsFolders(lks) #returns values "/apisamples" , "/apisamples/sub1" ,"/apisamples/sub1/child" ,"/apisamples/sub2"  

}
\keyword{file}
