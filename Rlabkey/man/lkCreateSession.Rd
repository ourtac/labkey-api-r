\name{lkCreateSession}
\alias{lkCreateSession}
\title{Allows interaction with LabKey Server schemas}
\description{
Builds field lists to be used in \code{labkey.selectRows}.
}
\usage{
lkCreateSession(baseUrl, folderPath="/", schemaName=NULL, queryName=NULL, ...)
}
\arguments{
  \item{baseUrl}{a string specifying the address of the LabKey Server, including the context root}
  \item{folderPath}{a string specifying the hierarchy of folders to the current folder (container) for the operation, starting with the project folder }
  \item{schemaName}{a string specifying the schema name in which the query object is defined}
  \item{queryName}{a string specifying the name of the query}
  \item{...}{}
}
\details{
Creates a session key that is passed to all the other functions
}


\value{
}
\references{http://www.omegahat.org/RCurl/, \cr
http://dssm.unipa.it/CRAN/web/packages/rjson/rjson.pdf,\cr
https://www.labkey.org/project/home/begin.view}
\author{Peter Hussey}
\seealso{\code{\link{labkey.selectRows}}}
\examples{

## Not run:
##
#library(Rlabkey)

#s<- lkCreateSession(baseUrl="http://localhost:8080/labkey", folderPath="/apisamples")
#s    #shows schemas

#curSchema(s)<- "lists"
#s   # now shows available queries
#curQuery(s) <- "NIMHSamples"
#s  # now ready for querying

# can also provide the schema and query in the constructor
#s<- lkCreateSession(baseUrl="http://localhost:8080/labkey", folderPath="/apisamples", schemaName="lists",queryName="AllTypes")

#lkdata<- getRows(s)   #shorthand for labkey.selectRows, all the same args apply
#lkdata

#q<- curQuery(s)
#q$Category          # can walk down the populataed schema tree from schema node or query node
#q$Category$caption
#q$Category$type

#lucols <- getLookups(s, "Category")  # can add fields from related queries
#lucols

##lucols2 <- getLookups(s, "Category/Group")  # keep going to other tables  (NYI)

#cols <- c(names(curQuery(s)[2:6]), names(lucols)[2:4])

#alltypesDF <- getRows(s, colSelect=paste(cols, sep=","))

#curFolder(s)<-'/Home'



}
\keyword{file}
