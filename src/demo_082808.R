## Demo labkey.selectRows.R function
## August 28, 2008




library(Rlabkey)



## Import specified columns of dataset

mydata <- labkey.selectRows(baseUrl="https://atlas-test.scharp.org/cpas", folderPath="/CHAVI/Studies/001/Study Data/", schemaName="study", queryName="HIV+Test+Results", viewName="demoView", colSelect="ParticipantId")



## Plot some output

par(mfrow=c(2,1))
# Histogram of HIV Rapid Test 1 
plot(factor(mydata[,4]), main="HIV Rapid Test 1")
# VL for ptid 700010019
oneptid <- mydata[(mydata[,1]==700010019)==TRUE,]
oneptid$logVL <- log10(oneptid[,3])
plot(oneptid$logVL, main="Viral Load for ptid 700010019", xlab="Time", ylab="log10(VL)")


