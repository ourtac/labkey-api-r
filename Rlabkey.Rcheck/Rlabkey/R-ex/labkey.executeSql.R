### Name: labkey.executeSql
### Title: Retrieve data from a labkey database using SQL commands
### Aliases: labkey.executeSql
### Keywords: IO

### ** Examples


### Select participants who meet acute status requirements
#getacute <- labkey.executeSql(baseUrl="https://www.labkey.org",
#                            folderPath="/home/Study/demo",
#                            schemaName="study",
#                            sql = 'select "Status Assessment".ParticipantId from "Status Assessment" where "Status Asses#sment"."StatusMeetCriteria"=\'yes\'')
#
#
### Average ages over different gender groups
#getage <- labkey.executeSql(baseUrl="https://www.labkey.org",
#                            folderPath="/home/Study/demo",
#                            schemaName="study",
#                            sql = "select Demographics.Gender, avg(Demographics.Age) as Number from Demographics group b#y Demographics.Gender")
#

### Select data for participants with partner information 
#getpartners <- labkey.executeSql(baseUrl="https://www.labkey.org",
#                            folderPath="/home/Study/demo",
#                            schemaName="study",
#                            sql = 'select "Status Assessment".StatusPartner1 from "Status Assessment" where "Status Asse#ssment".StatusPartner1 is not null')
#






