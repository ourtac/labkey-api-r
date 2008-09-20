\name{labkey.selectRows}
\alias{labkey.selectRows}
\title{Retrieve data from a labkey database using url specifications}
\description{
Use simple filters and sorts to specify data to be imported into R. Prior to import, 
the data columns can be sorted, specific columns or number of rows can be requested and
data filters can be applied.
}
\usage{
labkey.selectRows(baseUrl, folderPath, schemaName, queryName, viewName = NULL, 
colSelect = NULL, maxRows = NULL, rowOffset = NULL, colSort = NULL, 
colFilter = NULL, stripAllHidden = TRUE)
}
\arguments{
  \item{baseUrl}{a string specifying the \code{baseUrl}for the labkey server}
  \item{folderPath}{a string specifying the \code{folderPath} }
  \item{schemaName}{a string specifying the  \code{schemaName} for the query}
  \item{queryName}{a string specifying the \code{queryName}}
  \item{viewName}{(optional) a string specifying the \code{viewName}}
  \item{colSelect}{(optional) a vector of comma separated strings specifying which columns of a dataset or view to import}
  \item{maxRows}{(optional) an integer specifying how many rows of data to return. If no value is specified, all rows are returned.}
  \item{colSort}{(optional) a string including the name of the column to sort preceeded by a \dQuote{+} or
\dQuote{-} to indicate sort direction}
  \item{rowOffset}{(optional) an integer specifying which row of data should be the first row in the retrieval. If no
value is specified, the retrieval starts with the first row.}
  \item{colFilter}{(optional) a vector or array object created by the \code{makeFilter} function which contains the
column name, operator and value of the filter(s) to be applied to the retrieved data.}
  \item{stripAllHidden}{(optional) a logical value indicating whether or not to save data columns that would normally be hiddenfrom user view. If no value is specified, no hidden columns are returned.}
}
\details{
A full dataset or user saved view can be imported into an R data frame using the \code{labkey.selectRows} 
function. The function accepts as its arguments the components of the url that identify
the location of the data and what actions should be taken on the data prior to import
(ie, sorting, selecting particular columns or maximum number of rows, etc.). Data are returned in a data 
frame with column names as they appear on the labkey database website. 

Use care when specifying column names for the colSelect or colFilter arguments. Often the column name
that appears on the web site is not the same as the column caption. The column name is what is needed in
colselect and colFilter arguments. 

When importing data from ATLAS.scharp.org, a quick and simple way to identify the necessary components of the url 
(i.e., schemaName, queryName, viewName, the correct column names etc.) is to use the \dQuote{export to R script}
option avaiable as a drop down options under the \dQuote{views} tab for each dataset.
}
\value{
The requested data are returned in a data frame.
}
\references{http://www.omegahat.org/RCurl/,\cr 
http://dssm.unipa.it/CRAN/web/packages/rjson/rjson.pdf,\cr
https://www.labkey.org/project/home/begin.view}
\author{Valerie Obenchain}
\seealso{
\code{\link{labkey.executeSql}}, \code{\link{makeFilter}}, \code{\link{labkey.insertRows}}, 
\code{\link{labkey.updateRows}}, \code{\link{labkey.deleteRows}}

}
\examples{

## Retrieving data from the Labkey.org web site:

library(Rlabkey)

# Retrieve HIV Test Results and plot Western Blot data
getdata <- labkey.selectRows(baseUrl="https://www.labkey.org", folderPath="/home/Study/demo", 
				schemaName="study", queryName="HIV Test Results")
plot(factor(getdata$"HIV Western Blot"), main="HIV Western Blot")


# Select columns and apply filters
myfilters<- makeFilter(c("HIVLoadQuant","GREATER_THAN",500), c("HIVRapidTest","EQUALS","Positive"))

getdata <- labkey.selectRows(baseUrl="https://www.labkey.org", folderPath="/home/Study/demo", 
schemaName="study", queryName="HIV Test Results", 
colSelect=c("ParticipantId","HIVDate","HIVLoadQuant","HIVRapidTest"), colFilter=myfilters)



}
\keyword{IO}
