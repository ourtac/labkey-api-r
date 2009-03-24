### Name: makeFilter
### Title: Builds filters to be used in labkey.selectRows
### Aliases: makeFilter
### Keywords: file

### ** Examples

## These example datasets are located at 
## https://www.labkey.org/project/home/Study/demo/begin.view?

## Two filters:
filter1<- makeFilter(c("HIVLoadQuant","GREATER_THAN",500), 
c("HIVRapidTest","EQUAL","Positive"))

## Using "equals one of" operator:
filter2 <- makeFilter(c("HIVLoadIneq","EQUALS_ONE_OF","Equals ; Less than"))

## Using "not missing" operator:
filter3 <- makeFilter(c("HIVRapidTest","NOT_MISSING","")) 

## Apply a filter in labkey.selectRows function
getdata <- labkey.selectRows(
baseUrl="https://www.labkey.org", 
folderPath="/home/Study/demo", 
schemaName="study", 
queryName="HIV Test Results", 
colSelect=c("ParticipantId","HIVDate","HIVLoadQuant","HIVRapidTest"), 
colFilter=filter3)





