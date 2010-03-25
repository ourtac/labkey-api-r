##
#  Copyright (c) 2008-2010 Fred Hutchinson Cancer Research Center
# 
#  Licensed under the Apache License, Version 2.0 (the "License");
#  you may not use this file except in compliance with the License.
#  You may obtain a copy of the License at
# 
#      http://www.apache.org/licenses/LICENSE-2.0
# 
#  Unless required by applicable law or agreed to in writing, software
#  distributed under the License is distributed on an "AS IS" BASIS,
#  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#  See the License for the specific language governing permissions and
#  limitations under the License.
##

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
