### Name: labkey.executeSql
### Title: Retrieve data from a labkey database using Sql commands
### Aliases: labkey.executeSql
### Keywords: IO

### ** Examples


library(Rlabkey)

# Retrieve participant id, visit date and hemoglobin from Lab Results table
# from www.labkey.org
### NOTE: This won't work until 8.3 is up on www.labkey.org ####

#mydata <- labkey.executeSql(baseUrl="https://www.labkey.org", folderPath="/home/Study/demo", 
# schemaName="study", sql= 'select "Lab Results".ParticipantId, "Lab Results".Labdt, 
# "Lab Results".Labhemo from "Lab Results"')




