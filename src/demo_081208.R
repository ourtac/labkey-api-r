## Demo labkey.selectRows.R function
## August 12, 2008



rm(list=ls())
library(RCurl)
library(rjson)
source('mkpwd.R')
source('makeUrl.R')
source('parseHeader.R')
source('labkey.makeFilter.R')
source('labkey.selectRows.R')
source('makeDF.R')

## Encode username/password 
mypwd <- mkpwd("/home/vobencha")


## Import specified columns of dataset
mydata <- labkey.selectRows(mypwd=mypwd, baseUrl="https://atlas.scharp.org/cpas", folderPath="/CHAVI/Studies/001/Study%20Data/", schemaName="study", queryName="HIV+Test+Results", viewName="demoView",keepKey=FALSE)




## Plot some output

par(mfrow=c(2,1))
# Histogram of HIV Rapid Test 1 
plot(factor(mydata[,4]), main="HIV Rapid Test 1")

# VL for ptid 700010019
oneptid <- mydata[(mydata[,1]==700010019)==TRUE,]
oneptid$logVL <- log10(oneptid[,3])
plot(oneptid$logVL, main="Viral Load for ptid 700010019", xlab="Time", ylab="log10(VL)")



# colSelect=c("ParticipantID","ParticipantVisit/VisitDate","HTRnatqn","HTRrap")
