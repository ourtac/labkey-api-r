\name{Rlabkey-package}
\alias{Rlabkey-package}
\alias{Rlabkey}
\docType{package}
\title{
Import/export data between a labkey database and an R session
}
\description{
This package allows the transfer of data between a labkey database and an R session as well as the
ability to modify data in the labkey database from an R seession. Data can be imported from 
a labkey database into R by specifying the query schema information (\code{labkey.selectRows}) 
or by using sql commands (\code{labkey.executeSql}). From an R session,
a user can also send data to update existing data (\code{labkey.updateRows}) or to be inserted as new data 
(\code{labkey.insertRows}) or specify rows of data to be deleted from the labkey database (\code{labkey.deleteRows}). 

The user must have the appropriate authorization on the labkey
server in order to modify data in the database through the use of
these functions.
}
\details{
\tabular{ll}{
Package: \tab Rlabkey\cr
Type: \tab Package\cr
Version: \tab 0.0.1\cr
Date: \tab 2008-08-18\cr
License: \tab Apache 2.0\cr
LazyLoad: \tab yes\cr
}
Using this package to access a password protected labkey data base requires that the user
has their login information in a.netrc file. The .netrc file
contains configuration and autologin information for the File Transfer Protocol client (ftp) and
other programs such as CURL.
The file should be located in the users home directory and the permissions on the file should be unreadable for 
everybody except the owner. Permissions can be set in UNIX from the command line with chmod 600 .netrc.  

On a windows machine, create and  environment variable called 'HOME' that is set to your home directory ('c:/Users/<User-Name>' on Vista) or any directory you want to use. In that directory, create a text file named \_netrc (note that it's underscore netrc, not dot netrc like it is on UNIX). 

The following three lines must be included in the .netrc or \_netrc file either separated by white space
(spaces, tabs, or newlines) or commas.

machine <remote-machine-name>\cr
login <user-email>\cr
password <user-password>


An example would be:\cr
machine atlas.scharp.org\cr
login vobencha@fhcrc.org\cr
password mypassword\cr
}

\author{



Valerie Obenchain
}
\references{http://www.omegahat.org/RCurl/,\cr
http://dssm.unipa.it/CRAN/web/packages/rjson/rjson.pdf,\cr
https://www.labkey.org/project/home/begin.view}
\keyword{ package }
\seealso{
\code{\link{labkey.selectRows}}, \code{\link{labkey.executeSql}}, \code{\link{makeFilter}}, 
\code{\link{labkey.insertRows}}, \code{\link{labkey.updateRows}}, \code{\link{labkey.deleteRows}}
}
\examples{
}

