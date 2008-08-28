### Name: labkey.executeSql
### Title: Retrieve data from a labkey database using Sql commands
### Aliases: labkey.executeSql
### Keywords: IO

### ** Examples


library(Rlabkey)

# Retrieve Participant ID, age and height from Demographics table
# on www.labkey.org
### NOTE: This won't work until 8.3 is up on www.labkey.org ####

#getdata <- labkey.executeSql(baseUrl="https://www.labkey.org", folderPath="/home/Study/demo",schemaName="study", sql="select Demographics.ParticipantId, Demographics.Age, Demographics.Height from Demographics")




