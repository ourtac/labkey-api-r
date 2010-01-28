\name{curFolder}
\alias{curFolder}
\alias{curFolderPath}
\alias{curFolder<-}
\title{Set or get the current folder path }
\description{ Sets or gets the current folder path for a Labkey session }

\usage{
curFolder(lks, ...) 
}
\arguments{
  \item{lks}{the session key returned from lkCreateSession}
  \item{...}{}
}
\usage{
curFolderPath(lks, ...) 
}
\arguments{
  \item{lks}{the session key returned from lkCreateSession}
  \item{...}{}
}
\usage{
curFolder(lks, ...) <- folderName
}
\arguments{
  \item{lks}{the session key returned from lkCreateSession}
  \item{...}{}
  \item{value}{a string specifying a folder path}
}

\details{  Sets the current folder path, or returns the currenct folder path in string or object form.
}

\value{a string containing the folder path from the root
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

#curFolderPath(lks)  #returns project folder "/apisamples"

#curFolder(lks) <- "/Home"  #, warns that curSchema and curQuery have been reset 
#fld<- curFolder(lks)  #returns an object with name"/Home"

}
\keyword{file}
