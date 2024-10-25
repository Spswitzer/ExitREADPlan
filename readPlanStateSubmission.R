# # Script header ----
# Title: READ Plan State Submission
# Author: Susan Switzer
# Created: 10/14/24
# Revised: 10/25/24
# The purpose of this script is to access READ Plan data subimission in 2024

#load libraries ----
library(odbc)
library(DBI)
library(gt)
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
qryREADPlanSubmission <- odbc::dbGetQuery(con, 
                                   "
  SELECT
     *
  FROM
    dbSOARS.readplan.tREADSubmission (NOLOCK) 
  WHERE
    tREADSubmission.EndYear = 2024
"
)

# Remove records noted as excluded ----
readCleaned <- qryREADPlanSubmission %>% 
  filter(is.na(Excluded)) %>% 
  filter(GradeID < 4)

## Summarise services students recieved for SRD ----
readServices <- readCleaned %>% 
  pivot_longer(cols = c(SummerSchoolRead, TutorRead, InterventionServicesREAD),
               names_to = 'service',
               values_to = 'serviceValue') %>%
  group_by(GradeID) %>% 
  mutate(n = n_distinct(personID)) %>% 
  group_by(ReadPlan, GradeID) %>% 
  mutate(readPlanN = n_distinct(personID)) %>% 
  group_by(GradeID, service, serviceValue) %>% 
  summarise(servicesN = n_distinct(personID), 
            n = first(n), 
            servicesPct = round(servicesN/n, 4),
            readPlanN = first(readPlanN),
            readPct = round(readPlanN/n, 4)) %>% 
  filter(serviceValue == 1)

## Summarize N and Percent of students at each school and grade level with READ Plan ----
readSchools <- readCleaned %>% 
  select(personID, CDESchoolCode, GradeID, ReadPlan) %>% 
  group_by(CDESchoolCode) %>% 
  mutate(schoolN = n()) %>% 
  group_by(CDESchoolCode, GradeID) %>% 
  mutate(gradeN = n()) %>% 
  group_by(CDESchoolCode, GradeID, ReadPlan) %>% 
  reframe(readPlanN = n(),
            gradeN = first(gradeN), 
            gradePct = round(readPlanN/gradeN, 4)
          ) %>% 
  filter(ReadPlan == 1)

## Summarize READ Act interim assessment administered used for determine SRD status ----
### Create look up table of assessments based on https://www.cde.state.co.us/coloradoliteracy/22-23springreadassessmentfilelayoutanddefinitions ---
readTestName <- data.frame(ReadTest = c('04', '19', '13', 
                                        '20', '06', '10'), 
                           TestName = c('Exemption NEP', 'DIBELS8', 'Alternative Assessment', 
                                        'Lectura', 'Exemption Attendance', 'iReady'))

readTest <- readCleaned %>% 
  select(personID, GradeID, ReadTest) %>% 
  group_by(GradeID) %>% 
  mutate(gradeN = n()) %>% 
  group_by(ReadTest, GradeID) %>% 
  reframe(n = n(), 
         gradeN = first(gradeN)) %>% 
  full_join(readTestName)

readTestGender <- readCleaned %>% 
  select(personID, GradeID, Gender, ReadPlan) %>% 
  group_by(GradeID, ReadPlan) %>% 
  mutate(gradeN = n()) %>% 
  group_by(Gender, GradeID, ReadPlan) %>% 
  reframe(n = n(), 
          gradeN = first(gradeN)) %>% 
  filter(ReadPlan == 1) %>% 
  mutate(genderPct = n/gradeN) %>% 
  mutate(genderName = case_when(
    Gender == '01' ~ 'female', 
    Gender == '02' ~ 'male', 
    TRUE ~ 'gender expansive'
  ))

nOfPlans <- readCleaned %>% 
  group_by(GradeID) %>% 
  mutate(gradeTotalN = n()) %>% 
  group_by(ReadPlan, GradeID) %>% 
  mutate(planStatusN = n()) %>% 
  filter(ReadPlan == 1) %>% 
  summarise(gradeTotalN = first(gradeTotalN), 
            planStatusN = first(planStatusN), 
            planStatusPct = planStatusN/gradeTotalN) %>% 
  mutate(GradeID = factor(GradeID, 
                          levels = c(0, 1, 2, 3), 
                          labels = c('Kinder', '1st', '2nd', '3rd'))) %>% 
  ungroup()


gt(nOfPlans) %>% 
  cols_label(GradeID = 'Grade Level', 
             gradeTotalN = 'Total Students', 
             planStatusN = 'Students with READ Plan', 
             planStatusPct = 'Percent') %>% 
  fmt_percent(planStatusPct, decimals = 0) %>% 
  fmt_number(gradeTotalN:planStatusN, decimals = 0) %>% 
  cols_hide(ReadPlan) %>% 
  cols_align(columns = GradeID, align = 'left') %>% 
  opt_table_outline() %>% 
  tab_header(
    title = 'Students who were on READ Plan at the end of the 2023-2024 School year', 
    subtitle = 'Jeffco Submission to CDE'
  ) %>% 
  tab_options(
    table.font.size = 12
  )

  
  
