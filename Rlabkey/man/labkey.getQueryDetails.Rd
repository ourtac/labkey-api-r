\name{labkey.getQueryDetails}
\alias{labkey.getQueryDetails}
\title{Retrieve detailed information on a LabKey query}
\description{
Fetch a list of output columns and their attributes that are avaialble from a given query}
\usage{
labkey.getQueryDetails(baseUrl, folderPath, schemaName, queryName)
}
\arguments{
  \item{baseUrl}{a string specifying the address of the LabKey Server, including the context root}
  \item{folderPath}{a string specifying the hierarchy of folders to the current folder (container) for the operation, starting with the project folder }
  \item{schemaName}{a string specifying the schema name in which the query object is defined}
  \item{queryName}{a string specifying the name of the query}
}
\details{
Queries have a defined output list of fields.  To retrieve that set of fields with their detailed properties such as type and nullability, use
\code{labkey.getQueryDetails} function. Function arguments are the components of the url that identify
the location of the server, the folder path, the schema, and the name of the query.
\cr \cr
Each column in a dataset corresponsds to a field in a query.  Each field has a \dQuote{label} (also called a \dQuote{caption}), 
a name, and a key. The field label is visible at the top
of each column on a LabKey grid page, and is longer and more descriptive. The field name is the name as defined in the query.
The field key is the same as the name except for fields from other queries that have a defined 1-M relationship with the 
table designated by the queryName.  Such fields in related tables are referred to as \dQuote{lookup} fields and have fieldKeys of the format 
\cr
\cr
lookupkey/displayedfield
\cr
\cr
These lookup fieldKey strings can be added to the output column list of \code{labkey.selectRows}.  Lookup fields have the semantics of a LEFT JOIN in SQL, such that 
every record from the  target queryName apprears in the output whether or not there is a matching lookup field value.

}
\value{
The available schemas are returned as a data frame,

\code{queryName} {the name of the query, repeated n times, where n is the number of output fields from the query} \cr
\code{fieldName} {the output name of a field as defined in the query} \cr 
\code{caption}  {a more readable label for the data field, appears as a column header in grids} \cr 
\code{fieldKey} {the fully qualified name of the field, same as the name except for fields defined in queries referenced by lookup fields}\cr 
\code{type} {a string specifying the field type, e.g. Text, Number, Date, Integer}\cr 
\code{isNullable} {TRUE if the field can be left empty (null)} \cr
\code{isKeyField} {TRUE if the field is part of the primary key} \cr
\code{isAutoIncrement} {TRUE if the system will automatically assign a sequential integer in this on inserting a record}\cr 
\code{isVersionField} {TRUE if the field isused to detect changes since last read} \cr
\code{isHidden} {TRUE if the field is not displayed by default} \cr
\code{isSelectable} { }\cr
\code{isUserEditable} { }\cr 
\code{isReadOnly} { }\cr 
\code{isMvEnabled} { }\cr 
\code{lookupKeyField}  {for a field defined as a lookup the primary key column of the query referenced by the lookup field;  NA for non-lookup fields} \cr
\code{lookupSchemaName} {the schema of the query referenced by the lookup field;  NA for non-lookup fields} \cr
\code{lookupDisplayField} {the field from the query referenced by the lookup field that is shown by default in place of the lookup field;  NA for non-lookup fields} \cr 
\code{lookupQueryName} {the query referenced by the lookup field;  NA for non-lookup fields} \cr 
\code{lookupIsPublic}) {  }\cr

}
\references{http://www.omegahat.org/RCurl/,\cr 
http://dssm.unipa.it/CRAN/web/packages/rjson/rjson.pdf,\cr
https://www.labkey.org/project/home/begin.view}
\author{Peter Hussey, peter@labkey.com}
\seealso{
{Retrieve data:  } \code{\link{labkey.selectRows}}, \code{\link{makeFilter}}, \code{\link{labkey.executeSql}} \cr
{Modify data:  } \code{\link{labkey.updateRows}}, \code{\link{labkey.insertRows}}, \code{\link{labkey.deleteRows}} \cr
{List available data: } \code{\link{labkey.getSchemas}}, \code{\link{labkey.getQueries}}, \code{\link{labkey.getQueryViews}}, 
\code{\link{labkey.getDefaultViewDetails}}, \code{\link{labkey.getLookupDetails}}

}
\examples{
## Details  of fields of a query
## These example datasets are located at 
## https://isalmon.fhcrc.org/labkey/home/SharedData/begin.view?
## Not run
#queryDF <- labkey.getQueryDetails(
#baseUrl="https://isalmon.fhcrc.org/labkey",
#folderPath="/home/SharedData",
#schemaName="lists",
#queryName="Ovarian_SomaticMutations_Level3")
}
\keyword{IO}
