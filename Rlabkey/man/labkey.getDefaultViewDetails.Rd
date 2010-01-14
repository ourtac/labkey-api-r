\name{labkey.getDefaultViewDetails}
\alias{labkey.getDefaultViewDetails}
\title{Retrieve the fields of a LabKey query view}
\description{
Fetch a list of output fields and their attributes that are avaialble from the default view of a given query}
\usage{
labkey.getDefaultViewDetails(baseUrl, folderPath, schemaName, queryName)
}
\arguments{
  \item{baseUrl}{a string specifying the \code{baseUrl}for the labkey server}
  \item{folderPath}{a string specifying the \code{folderPath} }
  \item{schemaName}{a string specifying the  \code{schemaName} for the query}
  \item{queryName}{a string specifying the \code{queryName}}
}
\details{
Queries have a default \dQuote{views} associeated with them.  A query view can describe a subset or superset of the fields defined by the query.  
A query view is defined by using the \dQuote{Customize View} button option on a LabKey data grid page.  \code{getDefaultViewDetails} has the same arguments 
and returns the same shape of result data frame as \code{getQueryDetails}.
}
\value{
The available schemas are returned as a data frame.  See \code{\link{labkey.getQueryDetails}} for a description.
}
\references{http://www.omegahat.org/RCurl/,\cr 
http://dssm.unipa.it/CRAN/web/packages/rjson/rjson.pdf,\cr
https://www.labkey.org/project/home/begin.view}
\author{Peter Hussey, peter@labkey.com}
\seealso{
{Retrieve data:  } \code{\link{labkey.selectRows}}, \code{\link{makeFilter}}, \code{\link{labkey.executeSql}} \cr
{Modify data:  } \code{\link{labkey.updateRows}}, \code{\link{labkey.insertRows}}, \code{\link{labkey.deleteRows}}\cr
{List available data: } \code{\link{labkey.getSchemas}}, \code{\link{labkey.getQueries}}, \code{\link{labkey.getQueryViews}}, 
\code{\link{labkey.getQueryDetails}}, \code{\link{labkey.getLookupDetails}}

}
\examples{
## These example datasets are located at 
## https://isalmon.fhcrc.org/labkey/home/SharedData/begin.view?


## Details  of fields of a default query view
## Not run
#queryDF <- labkey.getDefaultViewDetails(
#baseUrl="https://isalmon.fhcrc.org/labkey",
#folderPath="/home/SharedData",
#schemaName="lists",
#queryName="Ovarian_SomaticMutations_Level3")
}
\keyword{IO}
