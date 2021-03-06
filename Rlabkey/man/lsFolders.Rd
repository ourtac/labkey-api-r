\name{lsFolders}
\alias{lsFolders}
\title{List the available folder paths }
\description{ Lists the available folder paths relative to the current folder path for a LabKey session }

\usage{
lsFolders(session) 
}
\arguments{
  \item{session}{the session key returned from getSession}
}

\details{  Lists the available folder paths relative to the current folder path for a LabKey session
}

\value{ A character array containing the available folder paths, relative to the project root. These values can be set on a session using curFolder<-
}
\references{https://www.labkey.org/wiki/home/Documentation/page.view?name=projects}
\author{Peter Hussey}
\seealso{\code{\link{getSession}}, \code{\link{lsProjects}}, \code{\link{lsSchemas}}}
\examples{
\dontrun{

##get a list if projects and folders
# library(Rlabkey)

# returns "/apisamples", "/home", ...
lsProjects(baseUrl="http://localhost:8080/labkey")

lks<- getSession(baseUrl="http://localhost:8080/labkey",
    folderPath="/apisamples")

#returns values "/apisamples" , "/apisamples/sub1" ,
#     "/apisamples/sub1/child" ,"/apisamples/sub2"
lsFolders(lks)

}
}
\keyword{file}
