
library(odbc)
library(DBI)
library(janitor)
library(tidyverse)
con <- dbConnect(odbc(),
                 Driver = "SQL Server",
                 Server = "qdc-soars-test",
                 trusted_connection = "true",
                 Port = 1433)



sort(unique(odbcListDrivers()[[1]]))

##standard DIBELS pull
qryDibels8 <- odbc::dbGetQuery(con, "

SELECT  studentdemographic.personid AS 'PersonID'
      ,studentdemographic.studentnumber
      ,studentdemographic.firstname
      ,studentdemographic.lastname
      ,studentdemographic.frlstatus
      ,studentdemographic.[readstatus]
      ,studentdemographic.calculatedlanguageproficiency
      ,studentdemographic.[504plan]
      ,studentdemographic.gt
      ,studentdemographic.ethnicity
      ,studentdemographic.genderdescription as Gender
      ,studentdemographic.iep
      ,studentdemographic.iepstatus
      ,studentdemographic.primarydisability
      ,studentdemographic.programtype
      ,ttest.gradeid AS 'GradeID'
      ,trange.proficiencylongdescription                                AS
        'ProficiencyLongDescription'
		  ,tTestTestPart.TestTestPartLongDescription AS 'ProficiencyOrder'
       ,ttest.testid
       ,ttest.testname
       ,ttesttype.testtypename
       ,CONVERT(DATE, CONVERT(VARCHAR(10), DIBELS8Benchmark.assessmentdatekey)) AS
        StudentTestDate
       ,SchoolYear.endyear                                               AS
        EndYear
       ,tcontent.contentid
       ,tcontent.contentname
       ,ttestingperiod.testingperiodid
       ,ttestingperiod.testingperiodname AS 'TestingPeriodName'
       ,ttesttestpart.testtestpartid AS 'TestingPartID'
       ,ttesttestpart.testtestpartlongdescription
       ,ttesttestpart.testtestpartshortdescription
       ,ttesttestpart.testtestpartdescription
       ,trange.rangebottom
       ,trange.rangetop
       ,ttestpart.standardlevelid
       ,tstandardlevel.standardlevelname
       ,CASE tstandardlevel.standardlevelname
          WHEN 'Overall' THEN 1
          ELSE 0
        END                                                              AS
        OverallFlag
       ,tcontentgroup.contentgroupname
       ,school.school                                                    AS
        schoolName
       ,school.cdeschoolnumber                                           AS
        cdeSchoolNumber
       ,calendar.calendarname
  		 ,SchoolYear.ReportSchoolYear AS 'SchoolYear' 
       ,ttest.windowstartdate
       ,ttest.windowenddate
  
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

dibels8ReadStatus <- DIBELS %>% 
  clean_names('lower_camel') %>% 
  filter(gradeId < 4) %>% 
  arrange(personId, schoolYear, desc(studentTestDate)) %>%  # most recent testing for students with multiple
  distinct(personId, testingPeriodName, .keep_all = T) %>% 
  select(endYear = schoolYear, testingPeriodName, gradeId, calculatedlanguageproficiency, 
         programtype, iep, frlstatus, readstatus, rangebottom, rangetop, cdeSchoolNumber) %>% 
  group_by(endYear, testingPeriodName, gradeId) %>% 
  mutate(totalN = n()) %>% 
  group_by(endYear, readstatus, testingPeriodName, gradeId) %>% 
  mutate(statusN = n(), 
         statusPct = statusN/totalN) %>% 
  group_by(endYear, readstatus, totalN, statusN, statusPct, testingPeriodName, gradeId) %>% 
  summarise() %>% 
  filter(testingPeriodName == 'End')



##the query below pulls from a view which only holds >=2022 
qryAcadience <- odbc::dbGetQuery(con, "
SELECT  vAcadienceStudentList.PersonID
      ,vAcadienceStudentList.GradeDescription AS 'gradedescription'
      ,vAcadienceStudentList.Grade
      ,vAcadienceStudentList.FRLStatus AS 'frlstatus'
      ,vAcadienceStudentList.READStatus AS 'readstatus'
      ,vAcadienceStudentList.CalculatedLanguageProficiency AS 'calculatedlanguageproficiency'
      ,vAcadienceStudentList.[504Plan] 
      ,vAcadienceStudentList.GT AS 'gt'
      ,vAcadienceStudentList.Ethnicity AS 'ethnicity'
      ,vAcadienceStudentList.Gender AS 'gender'
      ,vAcadienceStudentList.GenderDescription AS 'genderdescription'
      ,vAcadienceStudentList.IEP AS 'iep'
      ,vAcadienceStudentList.IEPStatus AS 'iepstatus'
      ,vAcadienceStudentList.PrimaryDisability AS 'primarydisability'
      ,vAcadienceStudentList.ProgramType AS 'programtype'
      ,vAcadienceStudentList.PercentOfPoints
      ,vAcadienceStudentList.ScaleScore
      ,vAcadienceStudentList.ProficiencyLongDescription
      ,vAcadienceStudentList.TestID AS 'testid'
      ,vAcadienceStudentList.TestName AS 'testname'
      ,vAcadienceStudentList.TestTypeName AS 'testtypename'
      ,vAcadienceStudentList.StudentTestDate 
      ,vAcadienceStudentList.EndYear
      ,vAcadienceStudentList.ContentID AS 'contentid'
      ,vAcadienceStudentList.ContentName AS 'contentname'
      ,vAcadienceStudentList.TestingPeriodID AS 'testingperiodid'
      ,vAcadienceStudentList.TestingPeriodName
      ,vAcadienceStudentList.TestTestPartID
      ,vAcadienceStudentList.TestTestPartLongDescription AS 'testtestpartlongdescription'
      ,vAcadienceStudentList.TestTestPartShortDescription AS 'testtestpartshortdescription'
      ,vAcadienceStudentList.TestTestPartDescription AS 'testtestpartdescription'
      ,vAcadienceStudentList.RangeBottom AS 'rangebottom'
      ,vAcadienceStudentList.RangeTop AS 'rangetop'
      ,vAcadienceStudentList.StandardLevelID AS 'standardlevelid'
      ,vAcadienceStudentList.StandardLevelName AS 'standardlevelname'
      ,vAcadienceStudentList.OverallFlag
      ,vAcadienceStudentList.ContentGroupName AS 'contentgroupname'
      ,vAcadienceStudentList.TestedAtSchool
      ,vAcadienceStudentList.TestedAtSchoolNumber
      ,vAcadienceStudentList.CalendarName AS 'calendarname'
      ,vAcadienceStudentList.Score
	  ,vAcadienceStudentList.studenttestdate --there are only 68 distinct dates- does this sound correct?
	  ,StudentDemographic.FirstName AS 'firstname'
	  ,StudentDemographic.LastName AS 'lastname'
	  ,StudentDemographic.StudentNumber AS 'studentnumber'
,vAcadienceStudentList.TestedAtSchool
,vAcadienceStudentList.TestedAtSchoolNumber AS 'CDESchoolNumber'
,vAcadienceStudentList.ProficiencyReportOrder AS 'ProficiencyOrder'
,vAcadienceStudentList.GradeID 
,vAcadienceStudentList.SchoolYear
,vAcadienceStudentList.WindowStartDate AS 'windowstartdate'
,vAcadienceStudentList.WindowEndDate AS 'windowenddate'
  FROM dbSoars.acadience.vAcadienceStudentList WITH (NOLOCK)
 LEFT JOIN AchievementDW.dim.StudentDemographic WITH (NOLOCK) ON vAcadienceStudentList.PersonID = StudentDemographic.PersonID
	AND vAcadienceStudentList.StudentTestDate BETWEEN StudentDemographic.RecordStartDate AND StudentDemographic.RecordEndDate
  WHERE vAcadienceStudentList.StandardLevelName = 'Overall'
")


dibelsNextReadStatus <- qryAcadience %>% 
  clean_names('lower_camel') %>% 
  mutate(gradeId = case_when(
    grade == 'K' ~ 0, 
    TRUE ~ as.numeric(gradeId)
  )) %>% 
  # filter(gradeId < 4) %>% 
  filter(testingPeriodName == 'End', 
         endYear == 2023) %>% 
  arrange(personId, endYear, desc(studentTestDate)) %>%  # most recent testing for students with multiple
  distinct(personId, testingPeriodName, .keep_all = T) %>% 
  select(endYear, testingPeriodName, gradeId, calculatedlanguageproficiency, 
         programtype, iep, frlstatus, readstatus, rangebottom, rangetop, cdeSchoolNumber) %>% 
  group_by(endYear, testingPeriodName, gradeId) %>% 
  mutate(totalN = n()) %>% 
  group_by(endYear, readstatus, testingPeriodName, gradeId) %>% 
  mutate(statusN = n(), 
         statusPct = statusN/totalN) %>% 
  group_by(endYear, readstatus, totalN, statusN, statusPct, testingPeriodName, gradeId) %>% 
  summarise() 


dibelsOld <- odbc::dbGetQuery(con, "
SELECT  
      PersonID
      ,gradeID AS Grade
      ,LanguageProficiency AS CalculatedLanguageProficiency
      ,IEP
      ,FRL
      ,GT
      ,Ethnicity
      ,Gender
      ,EndYear
	  ,ScaleScore
	  ,ProficiencyLongDescription
	  ,TestID
	  ,TestName
	  ,TestTypeName
	  ,TestingPeriodID
	  ,TestingPeriodName
	  ,TestTestPartDescription AS TestPartDescription
	  ,StandardLevelName
	  ,schoolName AS School
	  ,CampusSchoolID
	  ,Score
  FROM dbSoars.acadience.vAcadienceStudentListPre2022
  WHERE  StandardLevelName = 'Overall'
                              ")


qryStuDemos <- odbc::dbGetQuery(con, 
"
SELECT
  StudentDemographic.StudentNumber
  ,StudentDemographic.PersonID
  ,studentdemographic.[READ]
  ,StudentDemographic.READStatus
  ,Enrollment.Grade
  ,Enrollment.CDESchoolNumber
  ,Enrollment.CampusSchoolName
  ,Enrollment.EnrollmentStartDate
  ,Enrollment.EnrollmentEndDate
  ,StudentDemographic.RecordStartDate
  ,StudentDemographic.RecordEndDate
  ,SchoolYear.endyear     
FROM 
  AchievementDW.dim.Enrollment (NOLOCK)
JOIN 
  AchievementDW.dim.StudentDemographic (NOLOCK) ON Enrollment.PersonID = StudentDemographic.PersonID
  	AND (StudentDemographic.RecordEndDate BETWEEN Enrollment.EnrollmentstartDate AND Enrollment.EnrollmentEndDate
		OR StudentDemographic.RecordEndDate = '9999-12-31 23:59:59.9999999')
--WHERE '05-26-2023' BETWEEN Enrollment.EnrollmentStartDate AND Enrollment.EnrollmentEndDate
--AND '05-01-2023' BETWEEN StudentDemographic.RecordStartDate AND StudentDemographic.RecordEndDate
  AND Enrollment.IsInvalid = 0
  AND StudentDemographic.IsInvalid = 0
  AND Enrollment.DeletedInCampus = 0
  AND Enrollment.LatestRecord = 1
  AND Enrollment.EnrollmentType = 'Primary'
  --AND StudentDemographic.StudentNumber = '2057918'
"
)

studentDemos <- qryStuDemos %>% 
  mutate(EnrollmentStartDate = as.Date(EnrollmentStartDate), 
         EnrollmentEndDate = as.Date(EnrollmentEndDate), 
         startDate = day(EnrollmentStartDate), 
         startMonth = month(EnrollmentStartDate), 
         startYear = year(EnrollmentStartDate), 
         endDate = day(EnrollmentEndDate), 
         endMonth = month(EnrollmentEndDate), 
         endYear = year(EnrollmentEndDate)) %>% 
  filter(startYear == 2023, 
         str_detect(READStatus, 'Exit')) %>% 
  distinct(PersonID, .keep_all = T)


michaelMackie <- qryDibels8 %>% 
  filter(PersonID == 2292860) %>% 
  select(readstatus, ProficiencyLongDescription, StudentTestDate, TestingPeriodName)

julianPerez <- qryAcadience %>% 
  filter(PersonID == 1610465) %>% 
  select(readstatus, ProficiencyLongDescription, StudentTestDate, TestingPeriodName)



# SQL Query to access CMAS results ----
cmasData <- dbGetQuery(con, 
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

library(janitor)

dibels24Grade3 <- qryDibels8 %>% 
  clean_names('lower_camel') %>% 
  filter(endYear == 2024, 
         gradeId == 3, 
         testingPeriodName == 'End',
         str_detect(readstatus, 'Exit')
         ) %>% 
  group_by(personId) 

exitedWithSpanish <- dibels24Grade3 %>% 
  mutate(n = n()) %>% 
  filter(n > 1)

dibels24Grade3distinct <- dibels24Grade3 %>% 
  ungroup() %>% 
  filter(contentname == 'READING') %>% 
  arrange(personId, schoolYear, desc(studentTestDate)) %>%  # most recent testing for students with multiple
  distinct(personId, testingPeriodName, .keep_all = T)
  
         



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

flagLonger <- flagStart %>% 
  select(personID, Grade, planStart, planEnd) %>% 
  pivot_longer(c(planStart, planEnd), 
               names_to = 'status') %>% 
  filter(!is.na(value)) %>% 
  select(-status)   
  # filter(personID == 1548566)

flagWider <- flagLonger %>% 
  pivot_wider(names_from = Grade, 
              names_prefix = "Grade",
              values_from = value, 
              values_fn = list) %>% 
  select(personID, GradeK, Grade1, Grade2, Grade3) %>%
  mutate(across(GradeK:Grade3, ~replace(., lengths(.) == 0, NA))) %>% 
  mutate(across(GradeK:Grade3, ~str_replace(., '^c(.*)$', 
                                            'Start and Exit plan')))#clean up output


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
  
