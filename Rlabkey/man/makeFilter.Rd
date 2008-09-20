\name{makeFilter}
\alias{makeFilter}
\title{Builds an array of filters}
\description{
This function takes inputs of column name, filter value and filter operator and 
returns an array of filters to be used in \code{labkey.selectRows}.
}
\usage{
makeFilter(c(colname, operator,value))
}
\arguments{
  \item{colname}{a string specifying the name of the column to be filtered} 
  \item{operator}{a string specifying what operator should be used in the filter (see options below)}
  \item{value}{an integer or string specifying the value the columns should be filtered on}
}
\details{
These filters are applied to the data prior to import into R. Currently this function
allows the user to specify up to five filters. Multiple filters can be entered named or
un-named vectors (see examples below).

Possible operator values are as follows:
"EQUALS", "EQUALS\_ONE\_OF", "NOT\_EQUALS", "GREATER\_THAN", "GREATER\_THAN\_OR\_EQUAL\_TO", "LESS\_THAN",
"LESS\_THAN\_OR\_EQUAL\_TO", "DATE\_EQUAL", "DATE\_NOT\_EQUAL", "NOT\_EQUAL\_OR\_NULL",
"IS\_NULL", "IS\_NOT\_NULL", "CONTAINS", and "DOES\_NOT\_CONTAIN".

}


\value{
The function returns either a single string or an array of strings to be use in the
\code{colFilter} argument of the \code{labkey.selectRows} function.
}
\references{http://www.omegahat.org/RCurl/, \cr
http://dssm.unipa.it/CRAN/web/packages/rjson/rjson.pdf,\cr
https://www.labkey.org/project/home/begin.view}
\author{Valerie Obenchain}
\seealso{\code{\link{labkey.selectRows}}}
\examples{

# Specify two filters:
myfilters<- makeFilter(c("HIVLoadQuant","GREATER_THAN",500), c("HIVRapidTest","EQUALS","Positive"))

# Filter using "equals one of" operator:
myfilter2 <- makeFilter(filter1=c("HIVLoadIneq","EQUALS_ONE_OF","Equals ; Less than"))

# Use in labkey.selectRows function
getdata <- labkey.selectRows(baseUrl="https://www.labkey.org", folderPath="/home/Study/demo", 
schemaName="study", queryName="HIV Test Results", 
colSelect=c("ParticipantId","HIVDate","HIVLoadQuant","HIVRapidTest"), colFilter=myfilters)



}
\keyword{file}
