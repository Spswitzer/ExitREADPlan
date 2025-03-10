# # Script header ----
# Title: READ Plan Flags
# Author: Susan Switzer
# Created: 10/08/24
# Revised: 10/28/24
# The purpose of this script is to determine which 3rd students (2023-2024) from the kindergarten class of 2020-2021 had a READ plan at any time and when/if they exited the plan

#load libraries ----
library(gt)
library(odbc)
library(DBI)
library(corrplot)
library(Hmisc)
library(janitor)
library(tidyverse)
# connect to SQL database ----
con <- dbConnect(odbc(),
                 Driver = "SQL Server",
                 Server = "qdc-soars-test",
                 trusted_connection = "true",
                 Port = 1433)

sort(unique(odbcListDrivers()[[1]]))

# Query of READ Plan Flag in Campus ----
qryFlags <- odbc::dbGetQuery(con,
                                "
SELECT
 V_ProgramParticipation.name
 ,V_ProgramParticipation.personID
 ,V_ProgramParticipation.startDate
 ,V_ProgramParticipation.endDate
 ,Enrollment.Grade
 ,Enrollment.CampusSchoolName
 ,Enrollment.EnrollmentStartDate
 ,Enrollment.EnrollmentEndDate
 ,Enrollment.EndYear
 ,Enrollment.CalendarName
 ,Enrollment.EnrollmentType
 ,studentdemographic.frlstatus
 ,studentdemographic.calculatedlanguageproficiency
 ,studentdemographic.gt
 ,studentdemographic.ethnicity
 ,studentdemographic.WholeName
 ,studentdemographic.genderdescription as Gender
 ,studentdemographic.iep
 ,studentdemographic.primarydisability
 ,studentdemographic.programtype AS ELLProgram
FROM
  Jeffco_IC.dbo.V_ProgramParticipation (NOLOCK)
JOIN AchievementDW.dim.Enrollment (NOLOCK) ON
  Enrollment.PersonID = V_ProgramParticipation.PersonID
LEFT JOIN AchievementDW.dim.StudentDemographic WITH (NOLOCK) ON
  V_ProgramParticipation.PersonID = StudentDemographic.PersonID
WHERE
 name = 'READ'
--AND
--V_ProgramParticipation.personID in (1523935,1522073, 1632653)
--AND
 -- StudentDemographic.PersonID = 2240583
AND
  StudentDemoGraphic.LatestRecord = 1
AND
  V_ProgramParticipation.active = 1
AND
  Enrollment.DeletedInCampus = 0
AND
  Enrollment.LatestRecord = 1
--AND
 -- Enrollment.EnrollmentType = 'Primary'
--AND
 -- endDate > '2025-01-01 00:00:00'
"
)

# Load Demographics Look up Tables ----
frlLookup <- data.frame(
  stringsAsFactors = FALSE,
  frlstatus = c("No FRL", "Reduced Lunch", "Free Lunch"),
  frlBin = c(0L, 1L, 1L),
  frlLabels = c("Not Free or Reduced Lunch",
                "Free or Reduced Lunch Eligible",
                "Free or Reduced Lunch Eligible")
)

gtLookup <- data.frame(
  stringsAsFactors = FALSE,
  gt = c("GT", "Not GT"),
  gtBin = c(1L, 0L),
  gtLabels = c("GT", "Not GT")
)

iepLookup <- data.frame(
  stringsAsFactors = FALSE,
  iep = c("No IEP", "Exited IEP", "IEP"),
  iepBin = c(0L, 0L, 1L),
  iepLabels = c("No IEP", "No IEP", "IEP")
)

mlLookup <- data.frame(
  stringsAsFactors = FALSE,
  calculatedlanguageproficiency = c("Not ELL",
                                    "NEP",
                                    "LEP",
                                    "FEP M1",
                                    "FEP M2",
                                    "FEP T3+",
                                    "FELL",
                                    # "PHLOTE",
                                    "Prior to Feb 2013","FEP E1",
                                    "FEP E2"),
  mlBin = c(0L,1L,1L,
            1L,1L,1L,0L,
            # NA,
            0L,1L,
            1L),
  mlLabels = c("Not ML",
               "Multilingual Learner",
               "Multilingual Learner",
               "Multilingual Learner",
               "Multilingual Learner",
               "Multilingual Learner",
               "Not ML",
               "Not ML",
               # "Not ELL",
               "Multilingual Learner",
               "Multilingual Learner")
)

raceLookup <-data.frame(
  stringsAsFactors = FALSE,
  ethnicity = c("White","Hispanic","Multi",
                "Asian","Black","Am. Indian","Pacific Islander"),
  raceBin = c(0L, 1L, 1L, 1L, 1L, 1L, 1L),
  raceLabels = c("White Students",
                 "Students of Color or Hispanic","Students of Color or Hispanic",
                 "Students of Color or Hispanic","Students of Color or Hispanic",
                 "Students of Color or Hispanic","Students of Color or Hispanic")
)



flagStart <- qryFlags %>% 
  mutate(startDate = as_date(startDate), # format as date
         endDate = as_date(endDate),
         PlanStartDate = ymd(startDate), # format as Year, Month, Day
         planEndDate = ymd(endDate), 
         EnrollmentStartDate = ymd(EnrollmentStartDate), 
         EnrollmentEndDate = ymd(EnrollmentEndDate),
         enrollmentInterval = interval(EnrollmentStartDate, EnrollmentEndDate), #create time interval
         planStartInterval = PlanStartDate %within% enrollmentInterval, #determine if plan dates is within time interval
         planEndInterval = planEndDate %within% enrollmentInterval,
         planStart = ifelse(planStartInterval == TRUE, paste0(Grade, "- Start plan"), NA), #report if plan was started
         planEnd = ifelse(planEndInterval == TRUE, paste0(Grade, "- Exit plan"), NA)) %>% #report if plan was ended
  select(personID, Grade, CampusSchoolName, CalendarName, WholeName, EnrollmentStartDate, PlanStartDate, planStart, EnrollmentEndDate, 
         planEndDate, planEnd, EndYear, EnrollmentType, frlstatus, calculatedlanguageproficiency, gt, 
         ethnicity, iep, primarydisability, ELLProgram, ) %>%
  filter(planEndDate < "2025-03-07" & planEndDate > "2024-05-31") %>% 
  filter(Grade == '3') %>% 
  filter(EndYear == 2025) %>% 
  separate(CalendarName, into = c('cdeSchoolCode', 'calendarName'), sep = ' ') %>% 
  filter(cdeSchoolCode %in% c('8090', '5350', '3216', '3536', '4190', 
                              '5350', '9328', '5354', '7708', '8300', '9342'))

  