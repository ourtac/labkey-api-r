\name{labkey.executeSql}
\alias{labkey.executeSql}
\title{Retrieve data from a labkey database using SQL commands}
\description{
Use Sql commands to specify data to be imported into R. Prior to import, data can
be manipulated through standard SQL commands supported in labkey SQL.
}
\usage{
labkey.executeSql(baseUrl, folderPath, schemaName, sql, maxRows = NULL, 
rowOffset = NULL, stripAllHidden = TRUE)
}
\arguments{
  \item{baseUrl}{a string specifying the \code{baseUrl}for the labkey server}
  \item{folderPath}{a string specifying the \code{folderPath} }
  \item{schemaName}{a string specifying the  \code{schemaName} for the query}
  \item{sql}{a string containing the \code{sql} commands to be executed}
  \item{maxRows}{(optional) an integer specifying the maximum number of rows to return. If no value is specified, all rows are returned.}
  \item{rowOffset}{(optional) an integer specifying which row of data should be the first row in the retrieval. 
If no value is specified, rows will begin at the start of the result set.}
  \item{stripAllHidden}{(optional) a logical value indicating whether or not to save data columns that would 
normally be hidden from user veiw. If no value is specified, no hidden columns are returned.}
}
\details{
Rows returned from the SQL query can be imported into an R data frame using the \code{labkey.executeSql}
function. The function accepts as its arguments components of the url that identify the location of the
data and what SQL actions should be taken on the data prior to import. Data are returned in a data frame
with column names as they appear in on the labkey database website.

}
\value{
The requested data are returned in a data frame.
}
\references{http://www.omegahat.org/RCurl/,\cr 
http://dssm.unipa.it/CRAN/web/packages/rjson/rjson.pdf,\cr
https://www.labkey.org/project/home/begin.view}

\author{Valerie Obenchain}
\seealso{
\code{\link{labkey.selectRows}}, \code{\link{makeFilter}}, \code{\link{labkey.insertRows}}, 
\code{\link{labkey.updateRows}}, \code{\link{labkey.deleteRows}}
}

\examples{

library(Rlabkey)

# Retrieve participant id, visit date and hemoglobin from Lab Results table
# from www.labkey.org
### NOTE: This won't work until 8.3 is up on www.labkey.org ####


#mydata <- labkey.executeSql(baseUrl="https://www.labkey.org", folderPath="/home/Study/demo", 
# schemaName="study", sql= 'select "Lab Results".ParticipantId, "Lab Results".Labdt, 
# "Lab Results".Labhemo from "Lab Results"')


}
\keyword{IO}
