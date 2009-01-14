### Name: labkey.selectRows
### Title: Retrieve data from a labkey database
### Aliases: labkey.selectRows
### Keywords: IO

### ** Examples

## These example datasets are located at 
## https://www.labkey.org/project/home/Study/demo/begin.view?

## Retrieve full HIV Test Results dataset
fulldata <- labkey.selectRows(  
baseUrl="https://www.labkey.org", 
folderPath="/home/Study/demo", 
schemaName="study", 
queryName="HIV Test Results")

## Specifying filters, max rows and selecting columns
myfilters<- makeFilter(c("HIVLoadQuant","GREATER_THAN",500), 
                                           c("HIVRapidTest","EQUALS","Positive"))
smalldata <- labkey.selectRows( 
baseUrl="https://www.labkey.org", 
folderPath="/home/Study/demo", 
schemaName="study",     
queryName="HIV Test Results", 
colSelect=c("ParticipantId","HIVDate","HIVLoadQuant","HIVRapidTest"), 
maxRows=20,
colFilter=myfilters)





