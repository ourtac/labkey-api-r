### Name: labkey.executeSql
### Title: Retrieve data from a labkey database using SQL commands
### Aliases: labkey.executeSql
### Keywords: IO

### ** Examples

## These example datasets are located at 
## https://www.labkey.org/project/home/Study/demo/begin.view?

## Select participants who meet acute status requirements
getacute <- labkey.executeSql(
baseUrl="https://www.labkey.org",
folderPath="/home/Study/demo",
schemaName="study",
sql = 'select "Status Assessment".ParticipantId, "Status Assessment".StatusMeetCriteria 
from "Status Assessment" where "Status Assessment".StatusMeetCriteria=\'Yes\'')

## Compute average ages over different gender groups, 
## use column alias "Number" to rename the column
getage <- labkey.executeSql(
baseUrl="https://www.labkey.org",
folderPath="/home/Study/demo",
schemaName="study",
sql = "select Demographics.Gender, avg(Demographics.Age) as Number from Demographics 
group by Demographics.Gender")

## Get a list of participants with partner information 
getpartners <- labkey.executeSql(
baseUrl="https://www.labkey.org",
folderPath="/home/Study/demo",
schemaName="study",
sql = 'select "Status Assessment".ParticipantID, "Status Assessment".StatusPartner1 
from "Status Assessment" where "Status Assessment".StatusPartner1 is not null')




