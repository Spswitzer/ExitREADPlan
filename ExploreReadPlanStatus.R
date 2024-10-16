# # Script header ----
# Title: READ Plan Flags
# Author: Susan Switzer
# Created: 10/08/24
# Revised: 
# The purpose of this script is to determine which 3rd students (2023-2024) from the kindergarten class of 2020-2021 had a READ plan at any time and when/if they exited the plan

#load libraries ----
library(odbc)
library(DBI)
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
FROM 
  Jeffco_IC.dbo.V_ProgramParticipation (NOLOCK)
JOIN AchievementDW.dim.Enrollment (NOLOCK) ON 
  Enrollment.PersonID = V_ProgramParticipation.PersonID
WHERE
 name = 'READ'
--AND
--V_ProgramParticipation.personID in (1523935,1522073, 1632653)
AND
  V_ProgramParticipation.active = 1
AND 
  Enrollment.DeletedInCampus = 0
AND 
  Enrollment.LatestRecord = 1
AND 
  Enrollment.EnrollmentType = 'Primary'
--AND 
 -- endDate > '2024-01-01 00:00:00'
"
)

### Transform query date fields ----
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
         planStart = ifelse(planStartInterval == TRUE, "Start plan", NA), #report if plan was started
         planEnd = ifelse(planEndInterval == TRUE,"Exit plan", NA)) %>% #report if plan was ended
  select(personID, Grade, CampusSchoolName, EnrollmentStartDate, PlanStartDate, planStart, EnrollmentEndDate, planEndDate, planEnd, EndYear) %>%
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
  filter(EndYear > 2020) %>% 
  group_by(personID) %>% 
  fill(grade3In24, .direction = 'up') %>% 
  filter(grade3In24 == 'Y')

## Transform data into long format to add in summarizing ----
flagLonger <- flagStart %>% 
  select(personID, Grade, planStart, planEnd) %>% 
  pivot_longer(c(planStart, planEnd), 
               names_to = 'status') %>% 
  filter(!is.na(value)) %>% 
  select(-status)   

### Generate summary table ----
GradeSummary <- flagLonger %>% 
  group_by(Grade) %>% 
  mutate(gradeN = n()) %>% 
  group_by(value) %>% 
  mutate(totalN = n()) %>% 
  group_by(Grade, value) %>% 
  summarise(totalN = first(totalN), 
            gradeN = first(gradeN),
            statusN = n(), 
            endPct = round(statusN/gradeN*100, 2))

## Transform data into Wide format to add in summarizing ----
flagWider <- flagLonger %>% 
  pivot_wider(names_from = Grade, 
              names_prefix = "Grade",
              values_from = value, 
              values_fn = list) %>% 
  select(personID, GradeK, Grade1, Grade2, Grade3) %>%
  mutate(across(GradeK:Grade3, ~replace(., lengths(.) == 0, NA))) %>% 
  mutate(across(GradeK:Grade3, ~str_replace(., '^c(.*)$', 
                                            'Start and Exit plan')))#clean up output

# Query DIBELS8 ----
# which only holds data from 2023-2024 +
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
# which only holds data from 2021-2022  & 2022-2023 
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
# which only holds data from 2020-2021 and prior
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
  AND vAcadienceStudentListPre2022.EndYear = 2021
  AND vAcadienceStudentListPre2022.GradeID = 0
                              ")

dibels6 <- qryOldDibels %>% 
  clean_names('lower_camel') %>% 
  mutate(gender = case_when(
    gender == 'Female' ~ 'F', 
    gender == 'Male' ~ 'M', 
    TRUE ~ 'U'
  ))

# Query Student Demograpics ----
# It is challenging to pull READ Plan status from Enrollment. Need to careful set filters for enrollment and record dates, depending on inquiry
# qryStuDemos <- odbc::dbGetQuery(con, 
#                                 "
# SELECT
#   StudentDemographic.StudentNumber
#   ,StudentDemographic.PersonID
#   ,studentdemographic.[READ]
#   ,StudentDemographic.READStatus
#   ,Enrollment.Grade
#   ,Enrollment.CDESchoolNumber
#   ,Enrollment.CampusSchoolName
#   ,Enrollment.EnrollmentStartDate
#   ,Enrollment.EnrollmentEndDate
#   ,StudentDemographic.RecordStartDate
#   ,StudentDemographic.RecordEndDate
#   ,SchoolYear.endyear     
# FROM 
#   AchievementDW.dim.Enrollment (NOLOCK)
# JOIN 
#   AchievementDW.dim.StudentDemographic (NOLOCK) ON Enrollment.PersonID = StudentDemographic.PersonID
#   	AND (StudentDemographic.RecordEndDate BETWEEN Enrollment.EnrollmentstartDate AND Enrollment.EnrollmentEndDate
# 		OR StudentDemographic.RecordEndDate = '9999-12-31 23:59:59.9999999')
# --WHERE '05-26-2023' BETWEEN Enrollment.EnrollmentStartDate AND Enrollment.EnrollmentEndDate
# --AND '05-01-2023' BETWEEN StudentDemographic.RecordStartDate AND StudentDemographic.RecordEndDate
#   AND Enrollment.IsInvalid = 0
#   AND StudentDemographic.IsInvalid = 0
#   AND Enrollment.DeletedInCampus = 0
#   AND Enrollment.LatestRecord = 1
#   AND Enrollment.EnrollmentType = 'Primary'
#   --AND StudentDemographic.StudentNumber = '2057918'
# "
# )
# 
# studentDemos <- qryStuDemos %>% 
#   mutate(EnrollmentStartDate = as.Date(EnrollmentStartDate), 
#          EnrollmentEndDate = as.Date(EnrollmentEndDate), 
#          startDate = day(EnrollmentStartDate), 
#          startMonth = month(EnrollmentStartDate), 
#          startYear = year(EnrollmentStartDate), 
#          endDate = day(EnrollmentEndDate), 
#          endMonth = month(EnrollmentEndDate), 
#          endYear = year(EnrollmentEndDate)) %>% 
#   filter(startYear == 2023, 
#          str_detect(READStatus, 'Exit')) %>% 
#   distinct(PersonID, .keep_all = T)

# Query to CMAS results ----
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

cmasPerformance <- qryCmas %>% 
  clean_names('lower_camel') %>% 
  filter(contentGroupName == 'READING', 
         grade == '3', 
         endYear == 2024) %>% 
  select(personId, grade, cmasProfLevel = proficiencyLongDescription, testName)

cmasWithRead <- cmasPerformance %>% 
  right_join(flagWider, join_by(personId == personID)) %>% 
  mutate(n = n_distinct(personId)) %>% 
  group_by(cmasProfLevel) %>% 
  mutate(profN = n(), 
         profPct = profN/n) 
  

cmasNoScore <- cmasPerformance %>% 
  full_join(flagWider, join_by(personId == personID)) %>% 
  mutate(n = n_distinct(personId)) %>% 
  filter(is.na(cmasProfLevel)) %>% 
  mutate(readPlanN = n()) 

dibelsCombined <- dibels8Grade3 %>% 
  full_join(dibels6) %>% 
  full_join(dibelsNext) 

dibelsWithFlag <-  dibelsCombined %>% 
  right_join(flagStart, 
             join_by(personId == personID, 
                     endYear == EndYear, 
                     gradeId == gradeInt), 
             relationship = "many-to-many") %>% 
  left_join(cmasPerformance, by = join_by(personId)) %>% 
  filter(planEnd == 'Exit plan') %>% 
  mutate(planLength =  planEndDate- PlanStartDate) %>% 
  mutate(meanPlanLength = mean(planLength))

  flagGrade3 <- flagStart %>% 
    filter(gradeInt == 3, 
           EndYear == 2024, 
           !is.na(planEnd)) %>% 
    distinct(personID)
  
    grade3ExitedCMAS <- flagGrade3 %>% 
    ungroup() %>% 
    left_join(cmasPerformance, join_by(personID == personId)) %>% 
    filter(!is.na(cmasProfLevel)) %>% 
    distinct(personID, cmasProfLevel) %>% 
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

    
    ggplot(data = grade3ExitedCMAS, 
           mapping = aes(x = totalN, 
                         y = groupPct, 
                         fill = cmasProfLevel))+
      geom_bar(stat= 'identity', 
               position = 'stack') +
      geom_label(aes(label = paste0(groupN, '-', scales::percent(groupPct, 2))), 
                position = position_stack(vjust = 0.5), 
                size = 5,
                show.legend = F
                ) +
      labs(title = 'CMAS Performace', 
           subtitle = 'Grade 3 in 2024') +
      theme_minimal() +
      theme(axis.title = element_blank(), 
            axis.text = element_blank(), 
            legend.position = 'top', 
            legend.title = element_blank())
    