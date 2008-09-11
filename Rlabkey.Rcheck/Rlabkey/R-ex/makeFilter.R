### Name: makeFilter
### Title: Builds an array of filters
### Aliases: makeFilter
### Keywords: file

### ** Examples


# Specification of two filters:
myfilters<- makeFilter(c("HIVLoadQuant","GREATER_THAN",500), c("HIVRapidTest","EQUALS","Positive"))

# Filter using "equals one of" operator:
myfilter2 <- makeFilter(filter1=c("HIVLoadIneq","EQUALS_ONE_OF","Equals ; Less than"))

# Use in labkey.selectRows function
getdata <- labkey.selectRows(baseUrl="https://www.labkey.org", folderPath="/home/Study/demo", schemaName="study", queryName="HIV Test Results", colSelect=c("ParticipantId","HIVDate","HIVLoadQuant","HIVRapidTest"), colFilter=myfilters)





