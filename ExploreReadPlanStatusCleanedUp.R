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
 -- endDate > '2024-01-01 00:00:00'
"
)

# saveRDS(qryFlags, 'data/qryFlags.rds')

# qryFlags <- 
#   readRDS('data/qryFlags.rds')

#Load Demographics Look up Tables ----
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


### Transform query date demos fields ----
# filter to students of interest
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
  select(personID, Grade, CampusSchoolName, CalendarName, EnrollmentStartDate, PlanStartDate, planStart, EnrollmentEndDate, 
         planEndDate, planEnd, EndYear, EnrollmentType, frlstatus, calculatedlanguageproficiency, gt, 
         ethnicity, iep, primarydisability, ELLProgram) %>%
  filter(EnrollmentEndDate < '2024-06-30' | is.na(EnrollmentEndDate)) %>% #exclude enrollments in the 2025 school year
  mutate(gradeInt = case_when(
    Grade == 'K' ~ 0, 
    Grade == 'PK' ~ -1, 
    Grade == 'It' ~ -2, 
    TRUE ~ as.numeric(Grade)
  )) %>% # convert grade grade to numeric value to arrange/sort by
  filter(gradeInt < 4) %>% 
  arrange(personID, gradeInt, desc(planStart), desc(planEnd)) %>% 
  mutate(grade3In24 = case_when(
    gradeInt == 3 & EndYear == 2024 ~ 'Y', 
    TRUE ~ NA
  )) %>% 
  group_by(personID) %>% 
  fill(grade3In24, .direction = 'up') %>% 
  filter(grade3In24 == 'Y') %>% 
  #calculate summer programming and PK
  mutate(jeffcoPk = case_when(
  str_detect(CalendarName, 'PK') & !str_detect(CalendarName, 'Child Find') ~ 1,
  TRUE ~ NA_real_)) %>% 
  arrange(personID, EnrollmentStartDate) %>% 
  fill(jeffcoPk, .direction = 'down') %>% 
  mutate(enrollmentLength = EnrollmentEndDate - EnrollmentStartDate) %>% 
  filter(enrollmentLength > 10) %>% 
  mutate(jsel = str_detect(CalendarName, 'SEL')) %>% 
  mutate(remoteKinder = case_when(
    str_detect(CalendarName, 'RL') & EnrollmentType == 'Primary' & EndYear == 2021 ~ 1, 
    TRUE ~ NA_real_)) %>% 
  arrange(personID, desc(jeffcoPk)) %>% 
  fill(jeffcoPk, .direction = 'down') %>% 
  mutate(jsel = case_when(
    jsel == FALSE ~ NA_real_, 
    TRUE ~ 1
  )) %>% 
  mutate(childFind = case_when(
    str_detect(CalendarName, 'Child Find') ~ 1, 
    TRUE ~ NA_real_
  )) %>% 
  arrange(personID, desc(jsel)) %>% 
  fill(jsel, .direction = 'down') %>% 
  arrange(personID, desc(remoteKinder)) %>% 
  fill(remoteKinder, .direction = 'down') %>% 
  arrange(personID, desc(childFind)) %>% 
  fill(childFind, .direction = 'down') %>% 
  filter(EnrollmentType == 'Primary') %>% 
  mutate(planStart = case_when(
    is.na(planStart) & personID == 1625455 & Grade == 'K' ~ 'K- Start plan', 
    TRUE ~ planStart
  )) %>% #person 1625455 started plan after enrollment end
#Create binary values for demographic variables
  full_join(raceLookup) %>% 
  full_join(mlLookup) %>% 
  full_join(iepLookup) %>% 
  full_join(frlLookup) %>% 
  full_join(gtLookup) %>% 
  filter(!is.na(personID)) %>% 
  select(-c(frlstatus, calculatedlanguageproficiency, gt, ethnicity, iep, primarydisability, ELLProgram)) 
  # arrange(personID, desc(EnrollmentEndDate), EnrollmentEndDatePlus) %>% 
  # filter(personID == 2269294) 
  # distinct(personID, Grade, planStart, .keep_all = T)

## Transform data into long format to add in summarizing ----
flagLonger <- flagStart %>% 
  select(personID, Grade, planStart, planEnd) %>% 
  pivot_longer(c(planStart, planEnd), 
               names_to = 'status') %>% 
  filter(!is.na(value)) %>% 
  select(-status)   

## Summary table ----
flagSummary <- flagStart %>% 
  select(personID, planStart, planEnd) %>% 
  mutate(planStart = str_remove(planStart, '- Start plan'), 
         planEnd = str_remove(planEnd, '- Exit plan')) %>% 
  pivot_longer(c(planStart, planEnd), 
               names_to = 'status') %>% 
  filter(!is.na(value)) %>% 
  pivot_wider(names_from = status, 
              values_from = value) %>% 
  ungroup() %>% 
  mutate(totalN = n()) %>% 
  group_by(planStart) %>% 
  mutate(planStartN = n_distinct(personID)) %>% 
  group_by(planEnd) %>% 
  mutate(planEndN = n_distinct(personID)) %>% 
  group_by(planStart, planEnd) %>% 
  summarise(startEndN = n(), 
            planStartN = first(planStartN), 
            planEndN = first(planEndN), 
            planEndPct = startEndN/planStartN, 
            totalN = first(totalN)) %>% 
  mutate(planEnd = replace_na(planEnd, 'No End')) %>% 
  # filter(!is.na(planStart)) %>% 
  mutate(planStart = factor(planStart, 
                        levels = c('K', '1', '2', '3'), 
                        labels = c('Kindergarten', '1st', '2nd', '3rd'), 
                        ordered = T)) %>% 
  mutate(planStartN = paste0(planStartN, ' Students started plan')) %>% 
  group_by(planStart, planStartN) %>% 
  mutate(planEnd = factor(planEnd, 
                          levels = c('K', '1', '2', '3', 'No End'),
                          labels = c('Kinder', '1st', '2nd', '3rd', 'No End'),
                          ordered = T)) %>% 
  arrange(planEnd)

  ## Create summary GT table ----
gt(flagSummary) %>% 
  cols_label(planEnd = 'Grade when Plan Ended', 
             startEndN = 'Number of students', 
             planEndPct = 'Percent') %>% 
  row_group_order(groups = c('Kindergarten - 619 Students started plan', 
                             '1st - 704 Students started plan', 
                             '2nd - 321 Students started plan', 
                             '3rd - 290 Students started plan')) %>% 
  cols_width(
    planEnd  ~ px(200), 
    startEndN ~ px(200)) %>% 
  cols_align(
    align = c("left"),
    columns = planEnd
  ) %>% 
  cols_align(
    align = c("center"),
    columns = c(startEndN, planEndPct)
  ) %>% 
  opt_table_outline() %>% 
  tab_style(
    cell_fill('grey'), 
    cells_row_groups()
    ) %>% 
  cols_hide(
    c(planEndN, totalN)
    ) %>% 
  fmt_percent(
    planEndPct, 
    decimals = 0
    ) %>% 
  fmt_number(
    startEndN, 
    decimals = 0
  ) %>% 
  tab_header(
    title = 'Students who started and exited a READ plan by end of grade 3', 
    subtitle = '2020-2021 Kindergarten Student Cohort (Grade 3 in 2023-2024)'
    ) %>% 
  tab_options(
    table.font.size = 12
    )

##Summary By End Grade 3 plan exit results ----
cohortSummary <- flagSummary %>% 
  group_by(planEnd) %>% 
  summarise(
    totalN = first(totalN),
    planEndN = first(planEndN),
    planEndPct = planEndN/totalN)

gt(cohortSummary) %>% 
  cols_label(planEnd = 'Grade of Plan End', 
             planEndN = 'Number of students', 
             planEndPct = "Percent") %>% 
  cols_align(
    align = c("left"),
    columns = planEnd
  ) %>% 
  cols_align(
    align = c("center"),
    columns = c(planEndN, planEndPct)
  ) %>% 
  opt_table_outline() %>% 
  cols_hide(totalN) %>% 
  fmt_percent(planEndPct, decimals = 0) %>% 
  fmt_number(planEndN, decimals = 0) %>% 
  tab_header(
    title = 'Grade level when students exited READ plan', 
    subtitle = '2020-2021 Kindergarten Student Cohort (Grade 3 in 2023-2024)'
  ) 

## Transform data into Wide for personID## Transform data into Wide format to add in summarizing ----
flagWider <- flagLonger %>% 
  pivot_wider(names_from = Grade, 
              names_prefix = "Grade",
              values_from = value, 
              values_fn = list) %>% 
  select(personID, GradeK, Grade1, Grade2, Grade3) %>%
  mutate(across(GradeK:Grade3, ~replace(., lengths(.) == 0, NA))) %>% 
  mutate(across(GradeK:Grade3, ~str_replace(., '^c(.*)$', 
                                            'Start and Exit plan'))) #clean up output

# Query DIBELS8 ----
# holds data from 2023-2024 +
# some students repeated due to multiple tests for single window due to re-testing and testing in Spanish
qryDibels8 <- odbc::dbGetQuery(con, "

SELECT  studentdemographic.personid AS 'PersonID'
      ,studentdemographic.frlstatus
      ,studentdemographic.[readstatus]
      ,studentdemographic.calculatedlanguageproficiency
      ,studentdemographic.gt
      ,studentdemographic.ethnicity
      ,studentdemographic.Gender
      ,studentdemographic.iep
      ,studentdemographic.primarydisability
      ,studentdemographic.programtype AS ELLProgram
      ,ttest.gradeid AS 'GradeID'
      ,trange.proficiencylongdescription  AS 'ProficiencyLongDescription'
       ,ttesttype.testtypename AS testTypeName
       ,CONVERT(DATE, CONVERT(VARCHAR(10), DIBELS8Benchmark.assessmentdatekey)) AS StudentTestDate
       ,SchoolYear.endyear  AS EndYear
       ,tcontent.contentname
       ,ttestingperiod.testingperiodid AS testingPeriodNumeric
       ,ttestingperiod.testingperiodname AS 'TestingPeriodName'
       ,tcontentgroup.contentgroupname AS 'contentGroupName'
       ,school.cdeschoolnumber AS cdeSchoolNumber
       ,school.school AS schoolName
       ,calendar.calendarname

FROM   achievementdw.fact.DIBELS8Benchmark WITH (nolock)
       JOIN achievementdw.dim.studentdemographic WITH (nolock)
         ON studentdemographic.studentdemographickey =
            DIBELS8Benchmark.studentdemographickey
            AND studentdemographic.isinvalid = 0
       JOIN achievementdw.dim.assessmentingredient WITH (nolock)
         ON assessmentingredient.assessmentingredientkey =
           DIBELS8Benchmark.assessmentingredientkey
        JOIN achievementdw.dim.school WITH (nolock)
         ON school.schoolkey = DIBELS8Benchmark.schoolkey
       JOIN achievementdw.dim.calendar WITH (nolock)
         ON calendar.calendarkey = DIBELS8Benchmark.calendarkey
       JOIN dbsoars.uckie.ttesttestpart WITH (nolock)
         ON ttesttestpart.testtestpartid = assessmentingredient.testtestpartid
       JOIN dbsoars.uckie.ttestpart WITH (nolock)
         ON ttestpart.testpartid = ttesttestpart.testpartid
       JOIN dbsoars.uckie.tstandardlevel WITH (nolock)
         ON tstandardlevel.standardlevelid = ttestpart.standardlevelid
       JOIN dbsoars.uckie.ttest WITH (nolock)
         ON ttest.testid = ttesttestpart.testid
       JOIN dbsoars.uckie.ttestingperiod WITH (nolock)
         ON ttestingperiod.testingperiodid = ttest.testingperiodid
       JOIN dbsoars.uckie.ttesttype WITH (nolock)
         ON ttesttype.testtypename = 'DIBELS 8'
       JOIN dbsoars.uckie.ttestuse WITH (nolock)
         ON ttestuse.testuseid = ttest.testuseid
       JOIN dbsoars.dbo.tschoolyear SchoolYear WITH (nolock)
         ON SchoolYear.schoolyearid = ttest.schoolyearid
       JOIN dbsoars.uckie.tcontent WITH (nolock)
         ON tcontent.contentid = ttestpart.contentid
       JOIN dbsoars.uckie.tcontentgroup WITH (nolock)
         ON tcontentgroup.contentgroupid = tcontent.contentgroupid
       LEFT OUTER JOIN achievementdw.dim.assessmentscoringrange WITH (nolock)
                    ON assessmentscoringrange.assessmentscoringrangekey =
                       DIBELS8Benchmark.assessmentscoringrangekey
      LEFT OUTER JOIN dbsoars.uckie.trange WITH (nolock)
                    ON trange.rangeid = assessmentscoringrange.rangeid
                       AND trange.rangetypeid = 1
      LEFT OUTER JOIN dbsoars.uckie.tproficiency WITH (nolock)
                    ON tproficiency.proficiencyid = trange.proficiencyid
       LEFT OUTER JOIN dbsoars.uckie.tproficiencycolor WITH (nolock)
                    ON tproficiencycolor.proficiencyid =
                       tproficiency.proficiencyid
                       AND tproficiencycolor.colortypenameid = 'DIBELS 8'
       LEFT OUTER JOIN dbsoars.uckie.tcolor WITH (nolock)
                    ON tcolor.colorid = tproficiencycolor.colorid
       LEFT OUTER JOIN dbsoars.isr.tscoretypetesttype WITH (nolock)
                    ON tscoretypetesttype.testtypeid = ttesttype.testtypeid
                       AND tscoretypetesttype.usedonstudentprofile = 1
       --  BOEScoreTypeColumnName IS NOT NULL
       LEFT OUTER JOIN dbsoars.isr.tscoretype WITH (nolock)
                    ON tscoretype.scoretypeid = tscoretypetesttype.scoretypeid
WHERE  standardlevelname = 'Overall'
")

dibels8Grade3 <- qryDibels8 %>% 
  filter(EndYear == 2024) %>% 
  clean_names('lower_camel')

# Query Acadience----
# holds data from 2021-2022  & 2022-2023 
qryAcadience <- odbc::dbGetQuery(con, "
SELECT  
      vAcadienceStudentList.PersonID
      ,vAcadienceStudentList.FRLStatus AS 'frlstatus'
      ,vAcadienceStudentList.READStatus AS 'readstatus'
      ,vAcadienceStudentList.CalculatedLanguageProficiency AS 'calculatedlanguageproficiency'
      ,vAcadienceStudentList.GT AS 'gt'
      ,vAcadienceStudentList.Ethnicity AS 'ethnicity'
      ,vAcadienceStudentList.Gender
      ,vAcadienceStudentList.IEP AS 'iep'
      ,vAcadienceStudentList.PrimaryDisability AS 'primarydisability'
      ,vAcadienceStudentList.ProgramType AS 'ELLProgram'
      ,vAcadienceStudentList.GradeID
--  ,vAcadienceStudentList.ScaleScore
      ,vAcadienceStudentList.ProficiencyLongDescription
      ,vAcadienceStudentList.TestTypeName AS 'testTypeName'
      ,vAcadienceStudentList.StudentTestDate 
      ,vAcadienceStudentList.EndYear
      ,vAcadienceStudentList.ContentName AS 'contentname'
      ,vAcadienceStudentList.TestingPeriodID AS 'testingPeriodNumeric'
      ,vAcadienceStudentList.TestingPeriodName
      ,vAcadienceStudentList.ContentGroupName AS 'contentGroupName'
      ,vAcadienceStudentList.TestedAtSchool AS schoolName
      ,vAcadienceStudentList.TestedAtSchoolNumber AS cdeSchoolNumber
      ,vAcadienceStudentList.CalendarName AS 'calendarname'
  FROM dbSoars.acadience.vAcadienceStudentList WITH (NOLOCK)
 LEFT JOIN AchievementDW.dim.StudentDemographic WITH (NOLOCK) ON vAcadienceStudentList.PersonID = StudentDemographic.PersonID
	AND vAcadienceStudentList.StudentTestDate BETWEEN StudentDemographic.RecordStartDate AND StudentDemographic.RecordEndDate
  WHERE vAcadienceStudentList.StandardLevelName = 'Overall'
")

dibelsNext <- qryAcadience%>% 
  filter(EndYear %in% c(2022, 2023))%>% 
  clean_names('lower_camel') 

# Query DIBELS6----
# holds data from 2020-2021 and prior
qryOldDibels <- odbc::dbGetQuery(con, "
SELECT  
  studentdemographic.personid AS 'PersonID'
      ,studentdemographic.frlstatus
      ,studentdemographic.[readstatus]
      ,studentdemographic.calculatedlanguageproficiency
      ,studentdemographic.gt
      ,studentdemographic.ethnicity
      ,studentdemographic.genderdescription as Gender
      ,studentdemographic.iep
      ,studentdemographic.primarydisability
      ,studentdemographic.programtype AS ELLProgram
      ,vAcadienceStudentListPre2022.GradeID
	  ,vAcadienceStudentListPre2022.ProficiencyLongDescription
	  ,vAcadienceStudentListPre2022.TestTypeName
	  ,vAcadienceStudentListPre2022.TestingPeriodID AS 'testingPeriodNumeric'
	  ,vAcadienceStudentListPre2022.TestingPeriodName
	        ,vAcadienceStudentListPre2022.EndYear
	  ,vAcadienceStudentListPre2022.schoolName AS schoolName
  FROM dbSoars.acadience.vAcadienceStudentListPre2022
   LEFT JOIN AchievementDW.dim.StudentDemographic WITH (NOLOCK) ON vAcadienceStudentListPre2022.PersonID = StudentDemographic.PersonID
  WHERE  vAcadienceStudentListPre2022.StandardLevelName = 'Overall'
  AND vAcadienceStudentListPre2022.EndYear > 2019
  AND vAcadienceStudentListPre2022.GradeID = 0
                              ")

dibels6 <- qryOldDibels %>% 
  clean_names('lower_camel') %>% 
  mutate(gender = case_when(
    gender == 'Female' ~ 'F', 
    gender == 'Male' ~ 'M', 
    TRUE ~ 'U'
  ))

# Query CMAS results ----
qryCmas <- dbGetQuery(con, 
                       "
                  SELECT vPARCCStudentList.PersonID
                        ,vPARCCStudentList.StudentNumber
                       ,vPARCCStudentList.[SASID]
                       ,vPARCCStudentList.GradeDescription
                       ,vPARCCStudentList.Grade
                       ,vPARCCStudentList.FRLStatus
                       ,vPARCCStudentList.READStatus
                       ,vPARCCStudentList.CalculatedLanguageProficiency
                       ,vPARCCStudentList.[504Plan] AS X504Plan
                       ,vPARCCStudentList.GT
                       ,vPARCCStudentList.Ethnicity
                       ,vPARCCStudentList.Gender
                       ,vPARCCStudentList.GenderDescription
                       ,vPARCCStudentList.IEP
                       ,vPARCCStudentList.IEPStatus
                       ,vPARCCStudentList.PrimaryDisability
                       ,vPARCCStudentList.ScaleScore
                       ,vPARCCStudentList.ProficiencyLongDescription
                       ,vPARCCStudentList.ProficiencyDescription
                       ,vPARCCStudentList.TestID

                       ,vPARCCStudentList.TestName
                       ,vPARCCStudentList.TestTypeName
                       ,vPARCCStudentList.WindowStartDate
                       ,vPARCCStudentList.WindowEndDate
                       ,vPARCCStudentList.EndYear
                       ,vPARCCStudentList.ContentName
                       ,vPARCCStudentList.ContentGroupName
                       ,vPARCCStudentList.TestingPeriodName
                       ,vPARCCStudentList.TestTestPartID
                       ,vPARCCStudentList.TestTestPartLongDescription
                       ,vPARCCStudentList.TestTestPartShortDescription
                       ,vPARCCStudentList.TestTestPartDescription
                       ,vPARCCStudentList.RangeBottom
                       ,vPARCCStudentList.RangeTop
                       ,vPARCCStudentList.StandardLevelID
                       ,vPARCCStudentList.StandardLevelName
                       ,vPARCCStudentList.OverallFlag
                       ,vPARCCStudentList.CDESchoolNumber
                       ,vPARCCStudentList.School
                       
                       ,vGetPARCCGrowthStudentList.GrowthPercentile
                       ,vGetPARCCGrowthStudentList.ProficiencyLongDescription as ProficiencyLongDescriptionGrowth
                       ,vGetPARCCGrowthStudentList.RangeBottom as RangeBottomGrowth
                       ,vGetPARCCGrowthStudentList.RangeTop as RangeTopGrowth
                                        ,vGetPARCCGrowthStudentList.IsIncludedSchoolAccountability
                                        ,vGetPARCCGrowthStudentList.IsIncludedDistrictAccountability
                       
                       FROM dbSoars.parcc.vPARCCStudentList  WITH (NOLOCK)
                       LEFT JOIN dbSoars.parcc.vGetPARCCGrowthStudentList WITH (NOLOCK) ON dbSoars.parcc.vGetPARCCGrowthStudentList.PersonID = dbSoars.parcc.vPARCCStudentList.PersonID AND
                       dbSoars.parcc.vGetPARCCGrowthStudentList.TestID = dbSoars.parcc.vPARCCStudentList.TestID
                       
                       WHERE (dbSoars.parcc.vPARCCStudentList.OverallFlag = 1)
                        
                        
                        ")

## Clean up CMAS query ----
cmasPerformance <- qryCmas %>% 
  clean_names('lower_camel') %>% 
  filter(contentGroupName == 'READING', 
         grade == '3', 
         endYear == 2024, 
         !is.na(proficiencyLongDescription)) %>% 
  select(personId, grade, cmasProfLevel = proficiencyLongDescription, testName)

cmasWithRead <- cmasPerformance %>% 
  right_join(flagWider, join_by(personId == personID)) %>% 
  mutate(n = n_distinct(personId)) %>% 
  group_by(cmasProfLevel) %>% 
  mutate(profN = n(), 
         profPct = profN/n) 
  
### Explore the CMAS performance levels of students from cohort alongside their READ Plan info ----
#### Plot summary of CMAS performance for exited students ----
flagGrade3 <- flagStart %>% 
  filter(gradeInt == 3, 
         EndYear == 2024
         ) %>% 
  distinct(personID, planEnd)

grade3ExitedCMAS <- flagGrade3 %>% 
  ungroup() %>% 
  left_join(cmasPerformance, join_by(personID == personId)) %>% 
  filter(!is.na(cmasProfLevel)) %>% 
  distinct(personID, cmasProfLevel, .keep_all = T) %>% 
  mutate(totalN = n()) %>% 
  group_by(planEnd) %>% 
  mutate(planN = n()) %>% 
  group_by(cmasProfLevel, planEnd) %>% 
  reframe(groupN = n(), 
          planN = first(planN),
          totalN = first(totalN),
          groupPct = round(groupN/planN,4)) %>% 
  mutate(cmasProfLevel = str_remove(cmasProfLevel, ' Expectations')) %>% 
  mutate(cmasProfLevel = factor(cmasProfLevel, 
                                levels= c('Exceeded', 
                                          'Met', 
                                          'Approached', 
                                          'Partially Met', 
                                          'Did Not Yet Meet'))) %>% 
  mutate(planEnd = case_when(
    is.na(planEnd) ~ 'on READ Plan', 
    TRUE ~ 'Exited READ plan in Grade 3'
  ))

ggplot(data = grade3ExitedCMAS, 
       mapping = aes(x = totalN, 
                     y = groupPct, 
                     fill = cmasProfLevel))+
  geom_bar(stat= 'identity', 
           position = 'stack', 
           alpha = 0.8) +
  geom_label(aes(label = paste0(groupN, '-', scales::percent(groupPct, 1))), 
             position = position_stack(vjust = 0.5), 
             size = 4,
             show.legend = F
  ) +
  scale_fill_manual(values = c(
    'Exceeded' = '#317bb4', 
    'Met'= '#1b8367', 
    'Approached' = '#e2a331', 
    'Partially Met' = '#e57a3c', 
    'Did Not Yet Meet'= '#d8274a'
                               )) +
  facet_wrap(~planEnd) +
  labs(title = 'CMAS Performace', 
       subtitle = 'Grade 3 in 2024') +
  theme_minimal() +
  theme(axis.title = element_blank(), 
        axis.text = element_blank(), 
        legend.position = 'top', 
        legend.title = element_blank(), 
        panel.grid =  element_blank())

# Correlation between plan Exit in grade 3 and CMAS proficiency ----
flagGrade3Cor <- flagStart %>% 
  filter(gradeInt == 3, 
         EndYear == 2024
  ) %>% 
  distinct(personID, planEnd)

grade3ExitedCMASCor <- flagGrade3Cor %>% 
  ungroup() %>% 
  left_join(cmasPerformance, join_by(personID == personId)) %>% 
  filter(!is.na(cmasProfLevel)) %>% 
  distinct(personID, cmasProfLevel, .keep_all = T) %>% 
  mutate(cmasProfLevel = str_remove(cmasProfLevel, ' Expectations')) %>% 
  mutate(cmasProfLevel = case_when(
    cmasProfLevel == 'Exceeded' ~ 5, 
    cmasProfLevel == 'Met' ~ 4, 
    cmasProfLevel == 'Approached' ~ 3, 
    cmasProfLevel == 'Partially Met' ~ 2, 
    cmasProfLevel == 'Did Not Yet Meet' ~ 1)) %>% 
  mutate(planEnd = case_when(
    is.na(planEnd) ~ 0, 
    TRUE ~ 1
  )) %>% 
  select(planEnd, cmasProfLevel)

## Create CMAS Plan Exit Correlogram ----
cor(grade3ExitedCMASCor)
pearsonR <- rcorr(as.matrix(grade3ExitedCMASCor), type=c("pearson"))[["r"]]

#Check students with no CMAS score
cmasNoScore <-  qryCmas %>% 
  clean_names('lower_camel') %>% 
  filter(contentGroupName == 'READING', 
         grade == '3', 
         endYear == 2024) %>% 
  filter(is.na(proficiencyLongDescription)) %>% 
  inner_join(flagWider, join_by(personId == personID)) 

## Calculate length of time on plan with CMAS PL ----

readPlanGrade3 <- flagStart %>% 
  filter(gradeInt == 3, 
         EndYear == 2024
  )

grade3ExitedCMASAll <- readPlanGrade3 %>% 
  ungroup() %>% 
  left_join(cmasPerformance, join_by(personID == personId)) %>% 
  filter(!is.na(cmasProfLevel)) %>% 
  distinct(personID, cmasProfLevel, .keep_all = T) %>% 
  mutate(totalN = n()) %>% 
  group_by(planEnd) %>% 
  mutate(planN = n()) %>% 
  group_by(cmasProfLevel, planEnd) %>% 
  mutate(groupN = n(), 
          planN = first(planN),
          totalN = first(totalN),
          groupPct = round(groupN/planN,4)) %>% 
  mutate(cmasProfLevel = str_remove(cmasProfLevel, ' Expectations')) %>% 
  mutate(cmasProfLevel = factor(cmasProfLevel, 
                                levels= c('Exceeded', 
                                          'Met', 
                                          'Approached', 
                                          'Partially Met', 
                                          'Did Not Yet Meet'))) %>% 
  mutate(planEnd = case_when(
    is.na(planEnd) ~ 'on READ Plan', 
    TRUE ~ 'Exited READ plan in Grade 3'
  )) %>% 
  select(personID, cmasProfLevel, planEnd, totalN, planN, groupN, groupPct, PlanStartDate, planEndDate) %>% 
  # filter(!is.na(planStart)) %>%
  group_by(personID) %>% 
  mutate(planEndDate = replace_na(planEndDate, as.Date("2024-05-24"))) %>% 
  mutate(studentPlanLength = interval(PlanStartDate, planEndDate) %/% months(1)) %>% 
  ungroup() %>% 
  mutate(meanPlanLength = round(mean(studentPlanLength, na.rm = T), 0)) %>% 
  group_by(planEnd) %>% 
  mutate(meanPlanLengthEnd = round(mean(studentPlanLength, na.rm = T), 0)) %>% 
  group_by(cmasProfLevel, planEnd) %>% 
  reframe(cmasPlanLength = round(mean(studentPlanLength, na.rm = T), 0)) %>% 
  # filter(planEnd == 'Exited READ plan in Grade 3') %>% 
  group_by(cmasProfLevel) %>% 
  mutate(diff = lag(cmasPlanLength) - (cmasPlanLength)) 


shortPlan <- grade3ExitedCMASAll %>% 
  filter(studentPlanLength <7)

## Explore the DIBELS performance levels of students from cohort alongside their READ Plan info ----
### must recent DIBELS results ----
#### Filter to English testing ----
dibelsCombined <- dibels8Grade3 %>% 
  full_join(dibels6) %>% 
  full_join(dibelsNext)  %>% 
  arrange(personId, studentTestDate) %>% 
  filter(gradeId %in% 0:3) %>% 
  mutate(studentTestDate = as.Date(studentTestDate), 
         studentTestDate = ymd(studentTestDate)) %>% 
  group_by(personId) %>% 
  arrange(personId, desc(studentTestDate), endYear) %>%  # most recent testing for students with multiple
  distinct(personId, testingPeriodNumeric, endYear, .keep_all = T) %>% 
  filter(studentTestDate == max(studentTestDate, na.rm = T)) %>%
  filter(contentname == 'READING') 

flagStartFilledFlag <- flagStart %>% 
  fill(planEnd, .direction = 'updown') %>% 
  fill(planStart, .direction = 'updown') %>% 
  filter(EnrollmentEndDate == max(EnrollmentEndDate))

#Students with READ plan & DIBELS results & CMAS results ----
#READ Exceptions - Exempt-NEP (9), exempt due to attendance (213), Lectura (58), iReady (10), Alt Assessment (33) 
dibelsCmasWithFlag <-  dibelsCombined %>%
  right_join(flagStartFilledFlag,
             join_by(personId == personID,
                     endYear == EndYear,
                     gradeId == gradeInt),
             relationship = "many-to-many") %>% 
  filter(!is.na(planStart)) %>% 
  mutate(planEnd = str_remove(planEnd, "^.{0,3}")) %>% 
  inner_join(cmasPerformance, by = join_by(personId)) %>%
  # filter(!is.na(planEnd)) %>%
  mutate(planEnd = replace_na(planEnd, "No exit by end of grade 3")) %>% 
  group_by(personId) %>% 
  mutate(planEndDate = replace_na(planEndDate, as.Date("2024-05-24"))) %>% 
  mutate(studentPlanLength = interval( PlanStartDate, planEndDate) / years(1)) %>% 
  ungroup() %>% 
  mutate(meanPlanLength = round(mean(studentPlanLength, na.rm = T), 2)) %>% 
  filter(testName == 'CMAS ELA Grade 03 2023-24') %>% 
  group_by(cmasProfLevel, planEnd) %>% 
  mutate(cmasPlanLength = round(mean(studentPlanLength, na.rm = T), 2))

#Pull students with Exit Status and Did Not Meet CMAS Expectations ----
flagGrade3Students <- flagStart %>% 
  filter(gradeInt == 3, 
         EndYear == 2024, 
         !is.na(planEnd)) %>% 
  distinct(personID)

studentsExitDidNotMeetCMAS <- flagGrade3Students %>% 
  ungroup() %>% 
  left_join(cmasPerformance, join_by(personID == personId)) %>% 
  filter(!is.na(cmasProfLevel)) %>% 
  distinct(personID, cmasProfLevel) %>% 
  mutate(totalN = n()) %>% 
  filter(cmasProfLevel == 'Did Not Yet Meet Expectations') %>% 
  ungroup() %>% 
  distinct(personID)

dibelsAll <- dibels8Grade3 %>% 
  full_join(dibels6) %>% 
  full_join(dibelsNext)  %>% 
  arrange(personId, studentTestDate) %>% 
  filter(gradeId %in% 0:3) %>% 
  mutate(studentTestDate = as.Date(studentTestDate), 
         studentTestDate = ymd(studentTestDate)) %>% 
  group_by(personId) %>% 
  filter(contentname == 'READING') %>% 
  filter(studentTestDate == max(studentTestDate, na.rm = T)) 

allTestsStudentsExitDidNotMeetCMAS <- dibelsAll %>% 
  right_join(studentsExitDidNotMeetCMAS, 
             join_by(personId == personID))


# Summarize plan length by student group ----
flagStartFilledFlag <- flagStart %>% 
  fill(planEnd, .direction = 'updown') %>% 
  fill(planStart, .direction = 'updown') %>% 
  filter(EnrollmentEndDate == max(EnrollmentEndDate)) %>% 
  mutate(planEnd = str_remove(planEnd, "^.{0,3}")) %>% 
  mutate(planEnd = replace_na(planEnd, "No exit by end of grade 3"))

planLength <- flagStartFilledFlag %>% 
    mutate(across(c(jsel, jeffcoPk, childFind, remoteKinder), as.character)) %>% 
    pivot_longer(cols = c(raceLabels, mlLabels, iepLabels, frlLabels, gtLabels, jsel, jeffcoPk, childFind, remoteKinder), 
                 names_to = 'category', 
                 values_to = 'categoryValue') %>% 
    mutate(categoryValue = replace_na(categoryValue, '0')) %>% 
    group_by(category, categoryValue, planEnd) %>%
    filter(!is.na(planStart)) %>%
    # mutate(planEnd = str_remove(planEnd, "^.{0,3}")) %>% 
    # mutate(planEnd = replace_na(planEnd, "No exit by end of grade 3")) %>% 
    group_by(personID) %>% 
    mutate(planEndDate = replace_na(planEndDate, as.Date("2024-05-24"))) %>% 
    mutate(studentPlanLength = interval(PlanStartDate, planEndDate) %/% months(1)) %>% 
    ungroup() %>% 
    mutate(meanPlanLength = round(mean(studentPlanLength, na.rm = T), 0)) %>% 
    group_by(planEnd) %>% 
    mutate(meanPlanLengthEnd = round(mean(studentPlanLength, na.rm = T), 0)) %>% 
    group_by(category, categoryValue, planEnd) %>% 
    reframe(categoryPlanLength = round(mean(studentPlanLength, na.rm = T), 0)) %>% 
    filter(planEnd == 'Exit plan') %>% 
    group_by(category) %>% 
  mutate(category = factor(category, 
                           levels = c( 
                             "frlLabels", 
                             "iepLabels", 
                             "mlLabels", 
                             "raceLabels",  
                             "gtLabels",
                             "jeffcoPk",
                             "childFind", 
                             "jsel", 
                             "remoteKinder"),
                           labels = c(
                             'Free or Reduced Lunch Eligible', 
                             "Individualized Education Program",
                             "Multilingual Learned Program", 
                             "Students of Color or Hispanic", 
                             'Gifted and Talented Program', 
                             "Jeffco PK",
                             'Child Find Program', 
                             "Jeffco Summer Literacy Program", 
                             "Remote Kindergarten Program")
  )) %>% 
  mutate(categoryValue = factor(categoryValue, 
                                levels = c("Free or Reduced Lunch Eligible", "Not Free or Reduced Lunch" , 
                                           "IEP", "No IEP",
                                           "Multilingual Learner", "Not ML", 
                                           "Students of Color or Hispanic", "White Students", 
                                           "GT", "Not GT" , 
                                           '1', '0'), 
                                labels = c("Yes", "No" , 
                                           "Yes", "No" ,
                                           "Yes", "No" ,
                                           "Yes", "No" ,
                                           "Yes", "No" ,
                                           "Yes", "No")
  )) %>% 
  arrange(category, categoryValue) %>% 
    mutate(diff = lag(categoryPlanLength) - (categoryPlanLength)) %>% 
  mutate(diff = as.character(diff)) %>% 
  mutate(diff = replace_na(diff, ''))

gt(planLength) %>% 
  cols_label(categoryValue = 'Group Status', 
             categoryPlanLength = 'Plan Length (months)', 
             diff = 'Difference') %>% 
  cols_hide(c(planEnd)) %>%
  tab_header(title = 'Mean Plan Length For Students Who Exited READ Plan By The End of Grade 3', 
             subtitle = '2020-2021 Kindergarten Student Cohort (Grade 3 in 2023-2024)') %>% 
  cols_align(
    align = c("left"),
    columns = categoryValue
  ) %>% 
  cols_align(
    align = c("center"),
    columns = c(categoryPlanLength, diff)
  ) %>% 
  opt_table_outline() %>% 
  tab_style(
    cell_fill('grey'), 
    cells_row_groups()
  ) %>% 
  tab_options(
    table.font.size = 12
  ) 
  
# Summarize student groups by exit status ----
  allEoyGroupSummary <- flagStartFilledFlag %>% 
    pivot_longer(cols = c(raceBin, mlBin, iepBin, frlBin, gtBin,jsel, jeffcoPk, childFind, remoteKinder), 
                 names_to = 'category', 
                 values_to = 'categoryValue') %>% 
    ungroup() %>% 
    mutate(n = n_distinct(personID)) %>% 
    group_by(planEnd) %>% 
    mutate(planN = n_distinct(personID)) %>% 
    group_by(category, categoryValue) %>% 
    mutate(categoryN = n_distinct(personID)) %>% 
    group_by(category, categoryValue, planEnd) %>% 
    summarise(n = first(n), 
              planN = first(planN),
              groupN = n_distinct(personID),
              categoryN = first(categoryN),
              groupPct = groupN/categoryN
              )  %>% 
    filter(planEnd == 'Exit plan') %>% 
    mutate(categoryValue = replace_na(categoryValue, 0), 
           categoryValue = factor(categoryValue, 
                                 levels = 1:0, 
                                 labels = c('Yes', 'No'))) %>% 
    mutate(category = factor(category, 
                             levels = c( 
                                        'frlBin', 
                                        'iepBin', 
                                        'mlBin', 
                                        'raceBin', 
                                        'remoteKinder', 
                                        'childFind',
                                        'jeffcoPk', 
                                        'jsel', 
                                        'gtBin'), 
                             labels = c(
                               'Free or Reduced Lunch Eligible', 
                               'Individual Education Program', 
                               'Multilingual Learner Program', 
                               'Students of Color or Hispanic', 
                               'Remote Learning Program for Kindergarten', 
                               'ChildFind Program',
                               'Jeffco PK Program', 
                               'Jeffco Summer Learning Program >10 days', 
                               'Gifted and Talented Program'
                                      )
                             )) %>% 
    group_by(category) %>% 
    arrange(category, categoryValue)
  
  gt(allEoyGroupSummary) %>% 
    cols_label(categoryValue = 'Group Status', 
               groupN = 'Students in Exit Status', 
               categoryN = 'Total', 
               groupPct = 'Percent') %>% 
    cols_hide(c(planEnd, n, planN)) %>%
    fmt_percent(groupPct, decimals = 0) %>% 
    fmt_number(categoryN, decimals = 0) %>% 
    tab_header(title = 'Percent of Students Who Exited READ Plan By The End of Grade 3', 
               subtitle = '2020-2021 Kindergarten Student Cohort (Grade 3 in 2023-2024)') %>% 
    cols_align(
    align = c("left"),
    columns = categoryValue
  ) %>% 
    cols_align(
      align = c("center"),
      columns = c(groupN, categoryN, groupPct)
    ) %>% 
    opt_table_outline() %>% 
    tab_style(
      cell_fill('grey'), 
      cells_row_groups()
    ) %>% 
    tab_options(
      table.font.size = 12
    )
  
  ### Run T Test ----
  tTestGroups <- function(.cat) {
    

  allEoyGroups <- flagStartFilledFlag %>% 
    pivot_longer(cols = c(raceBin, mlBin, iepBin, frlBin, gtBin,jsel, jeffcoPk, childFind, remoteKinder), 
                 names_to = 'category', 
                 values_to = 'categoryValue') %>% 
    ungroup() %>% 
    mutate(planEnd = ifelse(planEnd== 'Exit plan', 1, 0)) %>% 
    mutate(categoryValue = replace_na(categoryValue, 0)) %>% 
    select(category, categoryValue, planEnd) %>% 
    filter(category == .cat)
  
  
  t.test(categoryValue ~ planEnd, data = allEoyGroups)
  }
  
  tTestGroups(.cat= 'childFind') 
  
  categoryValues <- c('raceBin', 'mlBin', 'iepBin', 'frlBin', 'gtBin','jsel', 'jeffcoPk', 'childFind', 'remoteKinder')
  
  tTtestResults <-   map(categoryValues, tTestGroups)
  
  pValues <- data.frame(category = categoryValues, 
                        p = c(tTtestResults[[1]][["p.value"]], 
                              tTtestResults[[2]][["p.value"]], 
                              tTtestResults[[3]][["p.value"]],
                              tTtestResults[[4]][["p.value"]],
                              tTtestResults[[5]][["p.value"]],
                              tTtestResults[[6]][["p.value"]],
                              tTtestResults[[7]][["p.value"]], 
                              tTtestResults[[8]][["p.value"]], 
                              tTtestResults[[9]][["p.value"]])) %>% 
    mutate(p = round(p, 4)) %>% 
    mutate(pFlag = case_when(
      p < 0.05 ~ 1, 
      TRUE ~ 0
    )) %>% 
    mutate(category = factor(category, 
                             levels = c( 'frlBin', 'iepBin', 'raceBin', 
                                         'mlBin','jsel', 'jeffcoPk', 
                                         'childFind', 'remoteKinder', 'gtBin'), 
                             labels = c( 'FRL', 'IEP', 'Students of Color', 
                                         'ML Program','JSEL', 'Jeffco PK', 
                                         'ChildFind', 'Remote Kinder', 'GT'))) %>% 
    arrange(category) %>% 
    mutate(
      pFlag = ifelse(pFlag == 1, "check", "x")
    )
  
  
  gt(pValues) %>% 
    cols_label(category = 'Student Group', 
               p = 'p-value',
               pFlag  = ' ') %>% 
    tab_header(title = 'Percent of students who exited READ Plan by the end of grade 3', 
               subtitle = 'with CMAS results in 2024') %>% 
    cols_align(align = 'left', columns = category) %>% 
    fmt_icon(columns = pFlag, 
             fill_color = list('check' = 'grey', 'x' = 'white')) %>% 
    tab_footnote(footnote =  md(glue::glue("{fontawesome::fa('check')} = Significant difference")))
  
## Proportion Testing ----
  propTestGroups <- function(.cat) {
    allEoyGroups <- flagStartFilledFlag %>% 
      pivot_longer(cols = c(raceBin, mlBin, iepBin, frlBin, gtBin,jsel, jeffcoPk, childFind, remoteKinder), 
                   names_to = 'category', 
                   values_to = 'categoryValue') %>% 
      ungroup() %>% 
      mutate(planEnd = ifelse(planEnd== 'Exit plan', 1, 0)) %>% 
      mutate(categoryValue = replace_na(categoryValue, 0)) %>% 
      select(category, categoryValue, planEnd) %>% 
      filter(category == .cat) %>% 
      mutate(n = n()) %>% 
      group_by(planEnd, categoryValue) %>% 
      reframe(planEndN = n())
    
    xValues <- allEoyGroups %>% 
      filter(planEnd == 1) %>% 
      pull(planEndN)
    
    nValues <- allEoyGroups %>% 
      filter(planEnd == 0) %>% 
      pull(planEndN)

    prop.test(x = xValues, 
              n = nValues,
              conf.level = 0.95)
  }
  
  propTestGroups(.cat= 'childFind') 
  
  categoryValues <- c('raceBin', 'mlBin', 'iepBin', 
                      'frlBin', 'gtBin','jsel', 
                      'jeffcoPk', 'childFind', 'remoteKinder')
  
  propTtestResults <-   map(categoryValues, propTestGroups)
  
  pValues <- data.frame(category = categoryValues, 
                        p = c(propTtestResults[[1]][["p.value"]], 
                              propTtestResults[[2]][["p.value"]], 
                              propTtestResults[[3]][["p.value"]],
                              propTtestResults[[4]][["p.value"]],
                              propTtestResults[[5]][["p.value"]],
                              propTtestResults[[6]][["p.value"]],
                              propTtestResults[[7]][["p.value"]], 
                              propTtestResults[[8]][["p.value"]], 
                              propTtestResults[[9]][["p.value"]])) %>% 
    mutate(p = round(p, 4)) %>% 
    mutate(pFlag = case_when(
      p < 0.05 ~ 1, 
      TRUE ~ 0
    )) %>% 
    mutate(category = factor(category, 
                             levels = c( 'frlBin', 'iepBin', 'raceBin', 
                                         'mlBin','jsel', 'jeffcoPk', 
                                         'childFind', 'remoteKinder', 'gtBin'), 
                             labels = c( 'FRL', 'IEP', 'Students of Color', 
                                         'ML Program','JSEL', 'Jeffco PK', 
                                         'ChildFind', 'Remote Kinder', 'GT'))) %>% 
    arrange(category) %>% 
    mutate(
      pFlag = ifelse(pFlag == 1, "check", "x")
    )
  
  gt(pValues) %>% 
    cols_label(category = 'Student Group', 
               p = 'p-value',
               pFlag  = ' ') %>% 
    tab_header(title = 'Pvalues of students who exited READ Plan by the end of grade 3') %>% 
    cols_align(align = 'left', columns = category) %>% 
    fmt_icon(columns = pFlag, 
             fill_color = list('check' = 'grey', 'x' = 'white')) %>% 
    tab_footnote(footnote =  md(glue::glue("{fontawesome::fa('check')} = Significant difference")))
  

# What time of the year of plans tend to start?----  
  planStartTime <- flagStart %>% 
    filter(!is.na(planStart)) %>% 
    mutate(planStartMonth = month(PlanStartDate), 
           planEndMonth = month((planEndDate))) %>% 
    distinct(personID, .keep_all = T)
  
## by month?----
  planStartTimeMonth <- planStartTime %>% 
    group_by(planStart) %>% 
    mutate(startN = n()) %>% 
    group_by(planStart, planStartMonth) %>% 
    mutate(startMonthN = n()) %>% 
    group_by(planStart, 
             planStartMonth, 
             startN, 
             startMonthN
    ) %>% 
    summarise() %>% 
    mutate(startPct = startMonthN/startN) %>% 
  group_by(planStart) %>% 
    mutate(planStart = str_remove(planStart, '- Start plan')) %>% 
    mutate(planStart = case_when(
      planStart == "K" ~ 0, 
      TRUE ~ as.numeric(planStart)
    )) %>% 
    mutate(planStart = factor(planStart, 
                              levels = 0:3, 
                              labels = c('Kindergarten', 'Grade 1', 
                                         'Grade 2', 
                                         'Grade 3'))) %>% 
    mutate(planStartMonth = factor(planStartMonth, 
                                   levels = c(8, 9, 10, 11, 12, 1, 2, 3, 4, 5, 6, 7),
                                   labels = c('Aug', 'Sep', 'Oct', 'Nov', 'Dec', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul'))) %>% 
    arrange(planStart, desc(startPct))

  gt(planStartTimeMonth)    %>% 
    cols_label(planStartMonth = '', 
               startMonthN = 'Number of students', 
               startPct = 'Percent') %>% 
    cols_align(
      align = c("left"),
      columns = planStartMonth
    ) %>% 
    cols_align(
      align = c("center"),
      columns = c(startMonthN, startPct)
    ) %>% 
    opt_table_outline() %>% 
    tab_style(
      cell_fill('grey'), 
      cells_row_groups()
    ) %>% 
    cols_hide(
      c(startN)
    ) %>% 
    fmt_percent(
      startPct, 
      decimals = 0
    ) %>% 
    tab_header(
      title = 'Student Plan starting Month', 
      subtitle = '2020-2021 Kindergarten Student Cohort (Grade 3 in 2023-2024)'
    ) %>% 
    tab_options(
      table.font.size = 12
    )
    
  ## by season? ----
   planStartTimeSeason<- planStartTime %>% 
    mutate(planStart = str_remove(planStart, '- Start plan')) %>% 
    mutate(planStart = case_when(
      planStart == "K" ~ 0, 
      TRUE ~ as.numeric(planStart)
    )) %>% 
    mutate(planStart = factor(planStart, 
                              levels = 0:3, 
                              labels = c('Kindergarten', 'Grade 1', 
                                         'Grade 2', 
                                         'Grade 3'))) %>% 
    group_by(planStart) %>% 
    mutate(startN = n()) %>% 
    mutate(planStartTimeSeason = case_when(
      planStartMonth %in% 8:11 ~ 'BOY', 
      planStartMonth %in% c(12, 1, 2, 3) ~ 'MOY', 
      planStartMonth %in% 4:5 ~ 'EOY', 
      TRUE ~ 'Summer'
    )) %>% 
    group_by(planStart, planStartTimeSeason) %>% 
    mutate(startSeasonN = n()) %>% 
    group_by(planStart, 
             planStartTimeSeason, 
             startN, 
             startSeasonN
    ) %>% 
    summarise() %>% 
    mutate(startPct = startSeasonN/startN) %>% 
    group_by(planStart) %>% 
    mutate(planStartTimeSeason = factor(planStartTimeSeason, 
                                   levels = c('BOY', 'MOY', "EOY"))) %>% 
    arrange(planStart, desc(startPct))
  
  gt(planStartTimeSeason)    %>% 
    cols_label(planStartTimeSeason = '', 
               startSeasonN = 'Number of students', 
               startPct = 'Percent') %>% 
    cols_align(
      align = c("left"),
      columns = planStartTimeSeason
    ) %>% 
    cols_align(
      align = c("center"),
      columns = c(startSeasonN, startPct)
    ) %>% 
    opt_table_outline() %>% 
    tab_style(
      cell_fill('grey'), 
      cells_row_groups()
    ) %>% 
    cols_hide(
      c(startN)
    ) %>% 
    fmt_percent(
      startPct, 
      decimals = 0
    ) %>% 
    tab_header(
      title = 'Student Plan starting season', 
      subtitle = '2020-2021 Kindergarten Student Cohort (Grade 3 in 2023-2024)'
    ) %>% 
    tab_options(
      table.font.size = 12
    )
  
  
  # What time of the year of plans tend to end?----  
  planEndTime <- flagStart %>% 
    filter(!is.na(planEnd)) %>% 
    mutate(planStartMonth = month(PlanStartDate), 
           planEndMonth = month((planEndDate))) %>% 
    distinct(personID, .keep_all = T)
  
  
  ## by season? ----
  planEndTimeSeason<- planEndTime %>% 
    mutate(planEnd = str_remove(planEnd, '- Exit plan')) %>% 
    mutate(planEnd = case_when(
      planEnd == "K" ~ 0, 
      TRUE ~ as.numeric(planEnd)
    )) %>% 
    mutate(planEnd = factor(planEnd, 
                              levels = 0:3, 
                              labels = c('Kindergarten', 'Grade 1', 
                                         'Grade 2', 
                                         'Grade 3'))) %>% 
    group_by(planEnd) %>% 
    mutate(endN = n()) %>% 
    mutate(planEndTimeSeason = case_when(
      planEndMonth %in% 8:11 ~ 'BOY', 
      planEndMonth %in% c(12, 1, 2, 3) ~ 'MOY', 
      planEndMonth %in% 4:5 ~ 'EOY', 
      TRUE ~ 'Summer'
    )) %>% 
    group_by(planEnd, planEndTimeSeason) %>% 
    mutate(endSeasonN = n()) %>% 
    group_by(planEnd, 
             planEndTimeSeason, 
             endN, 
             endSeasonN
    ) %>% 
    summarise() %>% 
    mutate(endPct = endSeasonN/endN) %>% 
    group_by(planEnd) %>% 
    mutate(planEndTimeSeason = factor(planEndTimeSeason, 
                                        levels = c('BOY', 'MOY', "EOY"))) %>% 
    arrange(planEnd, desc(endPct))
  
  gt(planEndTimeSeason)    %>% 
    cols_label(planEndTimeSeason = '', 
               endSeasonN = 'Number of students', 
               endPct = 'Percent') %>% 
    cols_align(
      align = c("left"),
      columns = planEndTimeSeason
    ) %>% 
    cols_align(
      align = c("center"),
      columns = c(endSeasonN, endPct)
    ) %>% 
    opt_table_outline() %>% 
    tab_style(
      cell_fill('grey'), 
      cells_row_groups()
    ) %>% 
    cols_hide(
      c(endN)
    ) %>% 
    fmt_percent(
      endPct, 
      decimals = 0
    ) %>% 
    tab_header(
      title = 'Student Plan Sseason when Students Exited Plan', 
      subtitle = '2020-2021 Kindergarten Student Cohort (Grade 3 in 2023-2024)'
    ) %>% 
    tab_options(
      table.font.size = 12
    )
  
  
# Sampling Plans ----
  flagsFilled <- flagStart %>% 
    fill(planEnd, .direction = 'updown') %>% 
    fill(planStart, .direction = 'updown') %>% 
    filter(EnrollmentEndDate == max(EnrollmentEndDate))
  
  groupSummary <- flagsFilled %>% 
    pivot_longer(cols = c(raceBin, mlBin, iepBin, frlBin, gtBin,jsel, jeffcoPk, childFind, remoteKinder), 
                 names_to = 'category', 
                 values_to = 'categoryValue') %>% 
    select(personID, planEnd, category, categoryValue) %>% 
    ungroup() %>% 
    mutate(n = n_distinct(personID)) %>% 
    group_by(category, categoryValue) %>% 
    mutate(categoryN = n_distinct(personID)) %>% 
    group_by(category, categoryValue, planEnd) %>% 
    # filter(planEnd %in% c('3- Exit plan', NA)) %>% #only grade 3 to inform potential interviews
    mutate(planEnd = replace_na(planEnd, '0')) %>% 
    mutate(categoryValue = replace_na(categoryValue, 0)) %>% 
    select(personID, category, categoryValue, planEnd) %>% 
    ungroup() %>% 
    mutate(n = n_distinct(personID)) %>% 
    select(planEnd, personID, category, categoryValue, n) %>% 
    group_by(planEnd) %>% 
    mutate(planN = n_distinct(personID)) %>% 
    mutate(categoryValue = replace_na(categoryValue, '0')) %>% 
    group_by(planEnd, category, categoryValue) %>% 
    mutate(categoryN = n_distinct(personID), 
             categoryPct = categoryN/planN)
    
    planEndSample <- groupSummary %>% 
      filter(planEnd %in% c( '3- Exit plan', '2- Exit plan', '1- Exit plan')) %>% 
      mutate(personID = as.character(personID)) %>% 
      group_by(category) %>% 
      slice_sample(n = 50, 
                   replace= T)
    
sample <- sample(x = planEndSample$personID, 
           prob = planEndSample$categoryPct , #a vector of probability weights for obtaining the elements of the vector being sampled.
           replace = F, #If replace is false, these probabilities are applied sequentially, that is the probability of choosing the next item is proportional to the weights among the remaining items
           size = (nrow(planEndSample)/9) * .33) # nine categories

planEndSampled <- planEndSample %>% 
  filter(personID %in% sample, 
         category == 'raceBin')

# Attach Student to Teacher ----
    library(DBI)
    library(odbc)
    library(janitor)
    library(tidyverse)
    # Create connection to databases ----
    con <- odbc::dbConnect(odbc(),  
                           Driver = "SQL Server",  
                           Server = "qdc-soars-test",  
                           trusted_connection = "true",                    
                           Port = 1433)  
    
    sort(unique(odbcListDrivers()[[1]])) 
    
qryStudentRoster <- dbGetQuery(con, "
SELECT DISTINCT	
Course.homeroom
,Course.courseID
,Course.name
,Section.sectionID
,Section.number
,Section.teacherDisplay
,SectionStudent.SchoolName
,Roster.PersonID
,StudentDemographic.LegalFirstName
,StudentDemographic.LegalLastName
--,Enrollment.EnrollmentEndDate
,Enrollment.EndYear
,Enrollment.Grade
,Roster.ModifiedDate
,Roster.StartDate
,Roster.EndDate
FROM Jeffco_IC.dbo.Course WITH (NOLOCK)
LEFT JOIN jeffco_IC.dbo.Section (nolock) ON 
  Course.courseID = Section.courseID
LEFT JOIN jeffco_IC.dbo.Roster (nolock) ON 
  Section.sectionID = Roster.SectionID
LEFT JOIN AchievementDW.dim.StudentDemographic (nolock) ON 
  Roster.personID = StudentDemographic.PersonID 
LEFT JOIN AchievementDW.dim.SectionStudent (NOLOCK) ON 
  Roster.personID = SectionStudent.personID 
RIGHT JOIN AchievementDW.dim.Enrollment WITH (NOLOCK) ON 
  SectionStudent.EnrollmentID = Enrollment.EnrollmentID AND 
  SectionStudent.PersonID = Enrollment.PersonID
LEFT JOIN AchievementDW.Dim.TeacherSection WITH (NOLOCK) ON 
  Roster.sectionID = TeacherSection.SectionID
WHERE
-- Roster.PersonID = 2240583 -- student will not show end year teacher
--AND 
  StudentDemographic.IsInvalid = 0
AND
  StudentDemographic.LatestRecord = 1
AND 
  course.active = 'TRUE'
AND	
  SectionStudent.IsInvalid = 0
AND 
  StudentDemographic.IsInvalid = 0
AND
  StudentDemographic.LatestRecord = 1
AND
  Course.name = 'Homeroom 3'
AND
  Enrollment.EndYear = 2024
AND 
  Roster.EndDate IS NULL
AND 
  Enrollment.Grade = '3'
"
)

studentExitGrade3 <- flagStart %>% 
    select(personID, Grade, planStart, planEnd) %>% 
    pivot_longer(c(planStart, planEnd), 
                  names_to = 'status') %>% 
    filter(!is.na(value)) %>% 
    select(-status) %>% 
    filter(value == '3- Exit plan') 
    
teacherStudentLink <- qryStudentRoster %>% 
    right_join(studentExitGrade3, join_by(PersonID == personID)) %>% 
    filter(homeroom == TRUE) %>% 
    distinct(PersonID, .keep_all = T) %>%
    # group_by(personID) %>% 
    # mutate(n = n()) %>% 
    # filter(n >1)
    filter(!is.na(courseID)) %>% 
    select(teacher = teacherDisplay, school = SchoolName, PersonID, firstName = LegalFirstName, lastName = LegalLastName)
  
teacherSummary <- teacherStudentLink %>% 
    group_by(teacher) %>% 
    reframe(n =n(), 
            school = first(school)) %>% 
    arrange(desc(n)) %>% 
    head(15)
  
schoolSummary <- teacherStudentLink %>% 
    group_by(school) %>% 
    reframe(n =n()) %>% 
    arrange(desc(n)) %>% 
    head(15)

  # Total Students in Cohort ----
  # Query of READ Plan Flag in Campus ----
  qryNoFlags <- odbc::dbGetQuery(con, 
                               "
SELECT
-- V_ProgramParticipation.name
-- ,V_ProgramParticipation.personID
-- ,V_ProgramParticipation.startDate
-- ,V_ProgramParticipation.endDate
  Enrollment.PersonID
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
 ,studentdemographic.genderdescription as Gender
 ,studentdemographic.iep
 ,studentdemographic.primarydisability
 ,studentdemographic.programtype AS ELLProgram
FROM 
AchievementDW.dim.Enrollment (NOLOCK) 
LEFT JOIN AchievementDW.dim.StudentDemographic WITH (NOLOCK) ON 
  Enrollment.PersonID = StudentDemographic.PersonID
WHERE
-- name = 'READ'
--AND
--V_ProgramParticipation.personID in (1523935,1522073, 1632653)
--AND
 -- StudentDemographic.PersonID = 2240583
--AND
  StudentDemoGraphic.LatestRecord = 1
--AND
--  V_ProgramParticipation.active = 1
AND 
  Enrollment.DeletedInCampus = 0
AND 
  Enrollment.LatestRecord = 1
--AND 
 -- Enrollment.EnrollmentType = 'Primary'
--AND 
 -- endDate > '2024-01-01 00:00:00'
"
  )
## Transform Data to attach demos ----
  flagStartNoFlag <- qryNoFlags %>% 
    mutate(EnrollmentStartDate = ymd(EnrollmentStartDate), 
           EnrollmentEndDate = ymd(EnrollmentEndDate),
           enrollmentInterval = interval(EnrollmentStartDate, EnrollmentEndDate), #create time interval
           ) %>% #report if plan was ended
    select(personID = PersonID, Grade, CampusSchoolName, CalendarName, EnrollmentStartDate, EnrollmentEndDate, 
           EndYear, EnrollmentType, frlstatus, calculatedlanguageproficiency, gt, ethnicity, iep, primarydisability, ELLProgram) %>%
    filter(EnrollmentEndDate < '2024-06-30' | is.na(EnrollmentEndDate)) %>% #exclude enrollments in the 2025 school year
    mutate(gradeInt = case_when(
      Grade == 'K' ~ 0, 
      Grade == 'PK' ~ -1, 
      Grade == 'It' ~ -2, 
      TRUE ~ as.numeric(Grade)
    )) %>% # convert grade grade to numeric value to arrange/sort by
    filter(gradeInt < 4) %>% 
    arrange(personID, gradeInt, desc(EnrollmentStartDate), desc(EnrollmentEndDate)) %>% 
    mutate(grade3In24 = case_when(
      gradeInt == 3 & EndYear == 2024 ~ 'Y', 
      TRUE ~ NA
    )) %>% 
    group_by(personID) %>% 
    fill(grade3In24, .direction = 'up') %>% 
    filter(grade3In24 == 'Y') %>% 
    #calculate summer programming and PK
    mutate(jeffcoPk = case_when(
      str_detect(CalendarName, 'PK') & !str_detect(CalendarName, 'Child Find') ~ 1,
      TRUE ~ NA_real_)) %>% 
    arrange(personID, EnrollmentStartDate) %>% 
    fill(jeffcoPk, .direction = 'down') %>% 
    mutate(enrollmentLength = EnrollmentEndDate - EnrollmentStartDate) %>% 
    filter(enrollmentLength > 10) %>% 
    mutate(jsel = str_detect(CalendarName, 'SEL')) %>% 
    mutate(remoteKinder = case_when(
      str_detect(CalendarName, 'RL') & EnrollmentType == 'Primary' & EndYear == 2021 ~ 1, 
      TRUE ~ NA_real_)) %>% 
    arrange(personID, desc(jeffcoPk)) %>% 
    fill(jeffcoPk, .direction = 'down') %>% 
    mutate(jsel = case_when(
      jsel == FALSE ~ NA_real_, 
      TRUE ~ 1
    )) %>% 
    mutate(childFind = case_when(
      str_detect(CalendarName, 'Child Find') ~ 1, 
      TRUE ~ NA_real_
    )) %>% 
    arrange(personID, desc(jsel)) %>% 
    fill(jsel, .direction = 'down') %>% 
    arrange(personID, desc(remoteKinder)) %>% 
    fill(remoteKinder, .direction = 'down') %>% 
    arrange(personID, desc(childFind)) %>% 
    fill(childFind, .direction = 'down') %>% 
    filter(EnrollmentType == 'Primary') %>% 
    #Create binary values for demographic variables
    full_join(raceLookup) %>% 
    full_join(mlLookup) %>% 
    full_join(iepLookup) %>% 
    full_join(frlLookup) %>% 
    full_join(gtLookup) %>% 
    filter(!is.na(personID)) %>% 
    select(-c(frlstatus, calculatedlanguageproficiency, gt, ethnicity, iep, primarydisability, ELLProgram) )
  
  ## Summarize cohort -----
  noFlagSummary <- flagStartNoFlag %>% 
    arrange(personID, desc(EnrollmentEndDate)) %>% 
    distinct(personID, .keep_all = T) %>% 
    ungroup() %>% 
    reframe(totalN = n()) 
  
  ### pull students in Cohort to join for CMAS analysis ----
  noFlagSummaryIds <- flagStartNoFlag %>% 
    arrange(personID, desc(EnrollmentEndDate)) %>% 
    distinct(personID) 
  
  ### pull students with plan to filter join for CMAS analysis ----
  studentsWithPlan <- flagLonger %>% 
    distinct(personID, .keep_all = T) 
  
  ### filter join to access students Never on plan ----
  neverOnPlan <- noFlagSummaryIds %>% 
    anti_join(studentsWithPlan)
  
  
  neverOnPlan <- cmasPerformance %>% 
    right_join(neverOnPlan, join_by(personId == personID)) %>% 
    mutate(n = n_distinct(personId)) %>% 
    group_by(cmasProfLevel) %>% 
    mutate(profN = n(), 
           profPct = profN/n) 
  
  ### Explore the CMAS performance levels of students Never on READ Plan ----
  #### Plot summary of CMAS performance for exited students ----
  grade3NoPlanCMAS <- neverOnPlan %>% 
    ungroup() %>% 
    # left_join(cmasPerformance, join_by(personId)) %>% 
    filter(!is.na(cmasProfLevel)) %>% 
    distinct(personId, cmasProfLevel, .keep_all = T) %>% 
    mutate(totalN = n()) %>% 
    group_by(cmasProfLevel) %>% 
    reframe(groupN = n(), 
            totalN = first(totalN),
            groupPct = round(groupN/totalN,4)) %>% 
    mutate(cmasProfLevel = str_remove(cmasProfLevel, ' Expectations')) %>% 
    mutate(cmasProfLevel = factor(cmasProfLevel, 
                                  levels= c('Exceeded', 
                                            'Met', 
                                            'Approached', 
                                            'Partially Met', 
                                            'Did Not Yet Meet'))) 
  
  ggplot(data = grade3NoPlanCMAS, 
         mapping = aes(x = totalN, 
                       y = groupPct, 
                       fill = cmasProfLevel))+
    geom_bar(stat= 'identity', 
             position = 'stack', 
             alpha = 0.8) +
    geom_label(aes(label = paste0(groupN, '-', scales::percent(groupPct, 1))), 
               position = position_stack(vjust = 0.5), 
               size = 4,
               show.legend = F
    ) +
    scale_fill_manual(values = c(
      'Exceeded' = '#317bb4', 
      'Met'= '#1b8367', 
      'Approached' = '#e2a331', 
      'Partially Met' = '#e57a3c', 
      'Did Not Yet Meet'= '#d8274a'
    )) +
    labs(title = 'CMAS Performace', 
         subtitle = 'Grade 3 in 2024') +
    theme_minimal() +
    theme(axis.title = element_blank(), 
          axis.text = element_blank(), 
          legend.position = 'top', 
          legend.title = element_blank(), 
          panel.grid =  element_blank())
  
  #### Explore students never on READ Plan with Low CMAS Performance -----
  grade3NoPlanCMASLo <- neverOnPlan %>% 
    ungroup() %>% 
    # left_join(cmasPerformance, join_by(personID == personId)) %>% 
    filter(cmasProfLevel == 'Did Not Yet Meet Expectations') %>%
    pull(personId)
  
  
  loCmasDibels <- dibelsCombined %>% 
    filter(personId %in% grade3NoPlanCMASLo, 
           readstatus == 'READ Plan') %>% 
    filter(proficiencyLongDescription == 'Well Below Benchmark')
  
  