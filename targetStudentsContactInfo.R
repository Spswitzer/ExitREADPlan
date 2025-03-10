#contact information for students in the target group for family interviews ----
# Load libraries ----
library(odbc)
library(DBI)
library(janitor)
library(tidyverse)

# Load dataframe of target students with #23-24 school association ----
targetStudents <- 
  readRDS('data/targetStudent.rds')

targetStudentsId <- targetStudents %>% 
  pull(personID)
toString(targetStudentsId)

targetStudents2025 <- 
  readRDS(file = 'data/targetStudents2025.rds')


targetStudentsId2025 <- targetStudents2025 %>% 
  pull(personID)
toString(targetStudentsId2025)


# connect to SQL database ----
con <- dbConnect(odbc(),
                 Driver = "SQL Server",
                 Server = "qdc-soars-test",
                 trusted_connection = "true",
                 Port = 1433)

sort(unique(odbcListDrivers()[[1]]))

# Query of READ Plan Flag in Campus ----
qryStudentRoster <- dbGetQuery(con, "
SELECT DISTINCT
Enrollment.PersonID
,StudentDemographic.StudentNumber
,StudentDemographic.SASID
,StudentDemographic.LegalFirstName
,StudentDemographic.LegalLastName
,Enrollment.EndYear
,Enrollment.Grade
,SectionStudent.schoolName
,student.homePrimaryLanguage
--,v_OneRosterAddress.number
--,v_OneRosterAddress.prefix
--,v_OneRosterAddress.street
--,v_OneRosterAddress.tag
--,v_OneRosterAddress.apt
--,v_OneRosterAddress.city
--,v_OneRosterAddress.state
--,v_OneRosterAddress.zip
--,v_OneRosterAddress.phone
,v_MailingAddress.number
,v_MailingAddress.prefix
,v_MailingAddress.street
,v_MailingAddress.tag
,v_MailingAddress.apt
,v_MailingAddress.city
,v_MailingAddress.state
,v_MailingAddress.zip
,v_MailingAddress.phone
,v_MailingAddress.guardianName
,v_MailingAddress.guardian
,v_MailingAddress.modifiedDate
,v_MailingAddress.communicationLanguage
FROM 
  jeffco_IC.dbo.Enrollment (nolock)
LEFT JOIN 
  AchievementDW.dim.StudentDemographic (nolock) ON 
   Enrollment.personID = StudentDemographic.PersonID 
LEFT JOIN
  jeffco_IC.dbo.v_MailingAddress (nolock) ON
    Enrollment.personID = v_MailingAddress.personID
--LEFT JOIN
--  jeffco_IC.dbo.v_OneRosterAddress (nolock) ON
 --   Enrollment.personID = v_OneRosterAddress.personID
--LEFT JOIN
  --jeffco_IC.dbo.v_MessengerEmail (nolock) ON
  --  Enrollment.personID = v_MessengerEmail.personID
LEFT JOIN
  jeffco_IC.dbo.student (nolock) ON
    Enrollment.personID = student.personID
LEFT JOIN 
  AchievementDW.dim.SectionStudent (NOLOCK) ON 
  SectionStudent.EnrollmentID = Enrollment.EnrollmentID AND 
  SectionStudent.PersonID = Enrollment.PersonID
WHERE
 -- Enrollment.PersonID = 1626044 -- student will not show end year teacher
-- Enrollment.PersonID IN (1492288, 1517773, 1522073, 1522625, 1527399, 1548566, 1554950, 1561011, 1561744, 1572832, 1580806, 1581982, 1583060, 1589221, 1590608, 1610465, --1610644, 1615543, 1616123, 1619775, 1622387, 1623016, 1623318, 1623736, 1626000, 1626044, 1626642, 1628873, 1629838, 1630521, 1630794, 1631604, 1632517, 1632653, 1643060, --1645397, 1647125, 1651303, 1652729, 1654739, 1658731, 1661663, 1664117, 1666235, 1666305, 1667736, 1675167, 1676294, 1682050, 1685526, 1686209, 1690772, 1692953, 1694249, --1694301, 1695545, 1698980, 1699292, 1699763, 1705635, 1707524, 1715155, 1716487, 1721741, 1734217, 1740349, 1741670, 1744718, 1754366, 1761227, 1761739, 1761999, 1762816, --1762867, 1762868, 1762902, 1762908, 1762916, 1763104, 1763271, 1763572, 1763643, 1763752, 1764114, 1764539, 1764580, 1764958, 1765183, 1765236, 1765612, 1765780, 2224429, --2225351, 2225394, 2225776, 2225975, 2226510, 2227615, 2227782, 2227997, 2230040, 2230174, 2231180, 2231229, 2231595, 2235634, 2235635, 2235781, 2236101, 2236166, 2236409, --2238038, 2239152, 2239435, 2239604, 2239929, 2240062, 2240177, 2240583, 2241682, 2241859, 2242364, 2242736, 2244119, 2244122, 2244148, 2244858, 2244862, 2248247, 2250874, --2267177, 2270072, 2277056, 2277450, 2277632, 2278934, 2279487, 2282314, 2298439, 2303033, 2311833, 2324065, 2326786, 2332320)
Enrollment.PersonID In (2274107, 2263006, 1626011, 2244830, 1763904, 2266259, 2273616, 2272552, 2265132, 2265784, 2378085, 2272161, 2388389, 2271592)
AND 
  StudentDemographic.IsInvalid = 0
AND
  StudentDemographic.LatestRecord = 1
AND 
  StudentDemographic.IsInvalid = 0
AND
  StudentDemographic.LatestRecord = 1
AND
  Enrollment.EndYear = 2024
"
)

write.csv(qryStudentRoster, 'data/studentContactInfo2025.csv')


languageSummary <- qryStudentRoster %>% 
  distinct(PersonID, .keep_all = T) %>% 
  group_by(communicationLanguage) %>% 
  summarise(count = n())
