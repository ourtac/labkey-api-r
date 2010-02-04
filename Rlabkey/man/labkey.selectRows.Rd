\name{labkey.selectRows}
\alias{labkey.selectRows}
\title{Retrieve data from a labkey database}
\description{
Import full datasets or selected rows into R. The data can be sorted and filtered prior to import.
}
\usage{
labkey.selectRows(baseUrl, folderPath, schemaName, queryName, 
viewName = NULL, colSelect = NULL, maxRows = NULL, 
rowOffset = NULL, colSort = NULL, colFilter = NULL, showHidden = FALSE, colNameOpt="caption")
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
  \item{showHidden}{(optional) a logical value indicating whether or not to return data columns that would normally be hidden from user view. If no value is specified, the hidden columns are not returned. In versions 0.0.5 and earlier, this argument was called \code{stripAllHidden}. The \code{stripAllHidden} argument performed the same function as \code{showHidden} but with different logic. The change was made for clarity.}
  \item{colNameOpt}{(optional) controls the name source for the columns of the output dataframe, with valid values of 'caption', 'fieldname', and 'rname'}
}
\details{
A full dataset or any portion of a dataset can be downloaded  into an R data frame using the 
\code{labkey.selectRows} function. Function arguments are the components of the url that identify
the location of the data and what actions should be taken on the data prior to import
(ie, sorting, selecting particular columns or maximum number of rows, etc.).

NOTE: 
In the returned data frame, there are three different ways to have the columns named:  
\code{colNameOpt='caption'} uses the caption value, and is the default option for backward compatibility.  
It may be the best option for displaying to another user, but may make scripting more difficult.

\code{colNameOpt='fieldname'} uses the field name value, so that the data frame colnames are the same names
that are used as arguments to labkey function calls.  It is the default for the new \code{\link{getRows}} shortcut
function over labkey.selectRows.

\code{colNameOpt='rname'} transforms the field name value into valid R names by substituting an underscore for both
spaces and forward slach (/) characters, and the lower casing the entire name.  It is the way a data frame is passed to a
script running at the LabKey server in the R View feature of the data grid.  If you are writing scripts for running in an 
R view on the server, or if you prefer to work with legal r names in the returned grid, this option may be useful.

}

\value{
The requested data are returned in a data frame with column names as they appear on the website.
}
\references{http://www.omegahat.org/RCurl/,\cr 
http://dssm.unipa.it/CRAN/web/packages/rjson/rjson.pdf,\cr
https://www.labkey.org/project/home/begin.view}
\author{Valerie Obenchain}
\seealso{
{Retrieve data:  } \code{\link{makeFilter}}, \code{\link{labkey.executeSql}} \cr
{Modify data:  } \code{\link{labkey.updateRows}}, \code{\link{labkey.insertRows}}, \code{\link{labkey.deleteRows}}\cr
{List available data: } \code{\link{labkey.getSchemas}}, \code{\link{labkey.getQueries}}, \code{\link{labkey.getQueryViews}},
\code{\link{labkey.getQueryDetails}},\code{\link{labkey.getDefaultViewDetails}},\code{\link{labkey.getLookupDetails}},

}
\examples{
## These example datasets are located at 
## https://www.labkey.org/project/home/Study/demo/begin.view?


## Retrieve full HIV Test Results dataset
## Not run
#fulldata <- labkey.selectRows(
#baseUrl="https://www.labkey.org",
#folderPath="/home/Study/demo",
#schemaName="study",
#queryName="HIV Test Results")


## Specifying filters, max rows and selecting columns
## Not run
#myfilters<- makeFilter(c("HIVLoadQuant","GREATER_THAN",500),
#c("HIVRapidTest","EQUAL","Positive"))
#smalldata <- labkey.selectRows(
#baseUrl="https://www.labkey.org",
#folderPath="/home/Study/demo",
#schemaName="study",
#queryName="HIV Test Results",
#colSelect=c("ParticipantId","HIVDate","HIVLoadQuant","HIVRapidTest"),
#maxRows=20,
#colFilter=myfilters)



}
\keyword{IO}
