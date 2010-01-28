\name{labkey.getLookupDetails}
\alias{labkey.getLookupDetails}
\title{Retrieve detailed information on a LabKey query}
\description{
Fetch a list of output columns and their attributes from the query referenced by a lookup field}
\usage{
labkey.getLookupDetails(baseUrl, folderPath, schemaName, queryName, lookupKey)
}
\arguments{
  \item{baseUrl}{a string specifying the address of the LabKey Server, including the context root}
  \item{folderPath}{a string specifying the hierarchy of folders to the current folder (container) for the operation, starting with the project folder }
  \item{schemaName}{a string specifying the schema name in which the query object is defined}
  \item{queryName}{a string specifying the name the query}
  \item{lookupKey}{a string specifying the qualified name of a lookup field (foreign key) relative to the query specified by queryName }
}
\details{
When \code{getQueryDetails} returns non-NA values for the lookupQueryName, the \code{getLookupDetails} function can be called
to enumerate the fields from the query referenced by the lookup.  These lookup fields can be added to the \code{colSelect} list of \code{selectRows}.
}
\value{
The available schemas are returned as a data frame, with the same columns as detailed in \code{\link{labkey.getQueryDetails}}

}
\references{http://www.omegahat.org/RCurl/,\cr 
http://dssm.unipa.it/CRAN/web/packages/rjson/rjson.pdf,\cr
https://www.labkey.org/project/home/begin.view}
\author{Peter Hussey, peter@labkey.com}
\seealso{
{Retrieve data:  } \code{\link{labkey.selectRows}},\code{\link{makeFilter}}, \code{\link{labkey.executeSql}} \cr
{Modify data:  } \code{\link{labkey.updateRows}}, \code{\link{labkey.insertRows}}, \code{\link{labkey.deleteRows}}\cr
{List available data: } \code{\link{labkey.getSchemas}}, \code{\link{labkey.getQueries}}, \code{\link{labkey.getQueryViews}}, 
\code{\link{labkey.getQueryDetails}}, \code{\link{labkey.getDefaultViewDetails}}

}
\examples{
## These example datasets are located at 
## https://isalmon.fhcrc.org/labkey/home/SharedData/begin.view?

## Details  of fields of a query referenced by a lookup field
## Not run
#queryDF  <- labkey.getLookupDetails(
#baseUrl="https://isalmon.fhcrc.org/labkey",
#folderPath="/home/SharedData",
#schemaName="lists",
#queryName="Ovarian_SomaticMutations_Level3",
#lookupKey="sample_Id"
#)

## When a lookup field points to a query object that itself has a lookup field, use the returned "relativeFieldKey" to keep going
## The first query shows field of list Ovarian_Samples, pointed to by lookup field BCRSAMPLEBARCODE in Ovarian_Aliquots list
# lu1<- labkey.getLookupDetails(
#	baseUrl="http://isalmon.fhcrc.org/labkey",
#	folderPath="/home/SharedData",
#	schemaName="lists",queryName="Ovarian_Aliquots", 
#	lookupKey="BCRSAMPLEBARCODE"
#)

## This query uses the relativeFieldKey returned for the BCRPATIENTBARCODE field in the previous query 
## to get a second set of fields. Relative to the base query (Ovarian_Aliquots) the key is BCRSAMPLEBARCODE/BCRPATIENTBARCODE
# lu2<- labkey.getLookupDetails(
#	baseUrl="http://isalmon.fhcrc.org/labkey",
#	folderPath="/home/SharedData",
#	schemaName="lists",queryName="Ovarian_Aliquots", 
#	lookupKey="BCRSAMPLEBARCODE/BCRPATIENTBARCODE"
#)

## Now select a result set containing a field from the base query, a field from the 1st level of lookup, and one from the 2nd
# row<- labkey.selectRows(
#	baseUrl="http://isalmon.fhcrc.org/labkey",
#	folderPath="/home/SharedData",
#	schemaName="lists",
#	queryName="Ovarian_Aliquots", 
#	colSelect=c("BCRALIQUOTBARCODE","BCRSAMPLEBARCODE/Type","BCRSAMPLEBARCODE/BCRPATIENTBARCODE/TUMORTISSUESITE"), 
#	colFilter = makeFilter(c("BCRSAMPLEBARCODE/BCRPATIENTBARCODE/TUMORTISSUESITE", "NOT_MISSING",""))
#	, maxRows=20
# )

}
\keyword{IO}
