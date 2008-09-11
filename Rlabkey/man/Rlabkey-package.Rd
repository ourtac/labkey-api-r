\name{Rlabkey-package}
\alias{Rlabkey-package}
\alias{Rlabkey}
\docType{package}
\title{
Import data from a labkey data base into an R data frame
}
\description{
This package allows the import of data from a labkey data base
into an R data frame through the use of Sql
commands or by specifying the query schema information.

Data in a labkey data base can be modified from an R session
by using the \code{insert}, \code{update}, and \code{delete} functions.
The user must have the appropriate authorization on the labkey
server in order to modify data in the data base through the use of
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
contains configuration and autologin information for the File Transfer Protocol client (ftp).
The file should be located in your home directory and the permissions on the file should be unreadable for 
everybody except the owner. Permissions can be set with the chmod command from the unix command line
as chmod 600 .netrc.  ***Insert how to do this for windows.
The following three lines must be in your .netrc file:
machine machinename
login mylogin
password mypassword

An example would be:
machine atlas.scharp.org
login vobencha@fhcrc.org
password mypassword

See http://linux.about.com/library/cmd/blcmdl5_netrc.htm for more information on how to configure
the .netrc file.




}
\author{
Valerie Obenchain
}
\references{http://www.omegahat.org/RCurl/,
http://dssm.unipa.it/CRAN/web/packages/rjson/rjson.pdf,
https://www.labkey.org/project/home/begin.view}
\keyword{ package }
\seealso{
\code{\link{labkey.selectRows}}, \code{\link{labkey.executeSql}}, \code{\link{makeFilter}}
}
\examples{
}

