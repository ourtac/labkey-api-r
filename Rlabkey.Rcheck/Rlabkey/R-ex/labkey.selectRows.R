### Name: labkey.selectRows
### Title: Retrieve data from a labkey database using url specifications
### Aliases: labkey.selectRows
### Keywords: IO

### ** Examples


## Retrieving data from the Labkey.org web site:

library(Rlabkey)

# Retrieve HIV Test Results and plot Western Blot data
getdata <- labkey.selectRows(baseUrl="https://www.labkey.org", folderPath="/home/Study/demo", 
                                schemaName="study", queryName="HIV Test Results")
plot(factor(getdata$"HIV Western Blot"), main="HIV Western Blot")

# Select columns and apply filters
myfilters<- makeFilter(c("HIVLoadQuant","GREATER_THAN",500), c("HIVRapidTest","EQUALS","Positive"))

getdata <- labkey.selectRows(baseUrl="https://www.labkey.org", folderPath="/home/Study/demo", 
schemaName="study", queryName="HIV Test Results", 
colSelect=c("ParticipantId","HIVDate","HIVLoadQuant","HIVRapidTest"), colFilter=myfilters)





