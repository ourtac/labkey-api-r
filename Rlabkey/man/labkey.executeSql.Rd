\name{labkey.executeSql}
\alias{labkey.executeSql}
\title{Retrieve data from a labkey database using Sql commands}
\description{
Use Sql commands to specify data to be imported into R. Prior to import, data can
be manipulated through all standard Sql commands.
}
\usage{
labkey.executeSql(baseUrl, folderPath, schemaName, sql, maxRows = NULL, 
				  rowOffset = NULL, stripAllHidden = TRUE)
}
\arguments{
  \item{baseUrl}{a string specifying the \code{baseUrl}for the HTTP request}
  \item{folderPath}{a string specifying the \code{folderPath} for the HTTP request}
  \item{schemaName}{a string specifying the  \code{schemaName} for the HTTP request}
  \item{sql}{a string containing the \code{sql} commands to be executed}
  \item{maxRows}{(optional) an integer specifying how many rows of data to return. If no value is specified, all rows are returned.}
  \item{rowOffset}{(optional) an integer specifying which row of data should be the first row in the retrieval. 
If no value is specified, all rows are returned.}
  \item{stripAllHidden}{(optional) a logical value indicating whether or not to save data columns that would 
normally be hidden from user veiw. If no value is specified, no hidden columns are returned.}
}
\details{
A full dataset or user saved view can be imported into an R data frame using the \code{labkey.executeSql}
function. The function accepts as its arguments components of the url that identify the location of the
data and what Sql actions should be taken on the data prior to import. Data are returned in a data frame
with column names as they appear in on the labkey database website.

}
\value{
The requested data are returned in a data frame.
}
\references{http://www.omegahat.org/RCurl/, 
http://dssm.unipa.it/CRAN/web/packages/rjson/rjson.pdf,
https://www.labkey.org/project/home/begin.view}

\author{Valerie Obenchain}
\seealso{\code{\link{labkey.selectRows}}}
\examples{

library(Rlabkey)

# Retrieve Participant ID, age and height from Demographics table
# on www.labkey.org
### NOTE: This won't work until 8.3 is up on www.labkey.org ####

#getdata <- labkey.executeSql(baseUrl="https://www.labkey.org", folderPath="/home/Study/demo",schemaName="study", sql="select Demographics.ParticipantId, Demographics.Age, Demographics.Height from Demographics")

}
\keyword{IO}
