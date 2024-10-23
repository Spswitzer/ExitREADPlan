# # Script header ----
# Title: READ Plan documents
# Author: Susan Switzer
# Created: 10/14/24
# Revised: 
# The purpose of this script is to review READ Plan elements for 3rd students (2023-2024) from the kindergarten class of 2020-2021 

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
# Need Progress Monitoring Result ----
qrystudentPlan <- odbc::dbGetQuery(con, 
"
  SELECT
   tStudentNeed.studentNeedID
   ,tStudentNeed.FirstName
   ,tStudentNeed.LastName
   ,tStudentNeed.PersonID
   ,tStudentNeed.ConcernStartDate
   ,tStudentNeed.LastUpdatedDate
   --,tStudentNeed.NeedDescription --empty
   --,tStudentNeed.isDisplayedStaffDashboard --FALSE for sample student
   --,tStudentNeed.isDisplayedStudentParentDashboard  --FALSE for sample student
   ,tStudentNeed.ActiveFlag AS needFlag-- Not sure what this indicates
   ,tInstruction.ActiveFlag AS instFlag -- Not sure what this indicates
   ,tGoal.ActiveFlag AS goalFlag -- Not sure what this indicates
   ,tFocusAreaDesignator.ActiveFlag AS focusFlag -- Not sure what this indicates
   ,tGoalInstruction.ActiveFlag AS goalInstFlag -- Not sure what this indicates
   ,tNeedType.ActiveFlag AS needTypeFlag -- Not sure what this indicates
   ,tStudentNeedFocusArea.ActiveFlag AS focusAreaFlag -- Not sure what this indicates
   ,tProgress.ActiveFlag AS progressActiveFlag -- Not sure what this indicates
   ,tProgressResult.ActiveFlag AS progressResultActiveFlag -- Not sure what this indicates
   ,tStudentNeedFocusArea.StudentNeedFocusAreaID --Not using this
   ,tFocusArea.FocusAreaName
   --,tFocusArea.StartYear --Not working 1900
   --,tFocusArea.EndYear --Not working 2099
   --,tGoal.ParentAcknowledgeDate -- NA for sample student
   -- ,tGoal.ProgressMonitoring --error: ODBC SQL Server Driver]Invalid Descriptor Index 
   ,tStudentNeedFocusArea.FocusAreaID
   --,tStudentNeedFocusArea.FocusAreaRanking
   ,tStudentNeedFocusArea.StartDate AS studentNeedStart
   ,tStudentNeedFocusArea.EndDate AS studentNeedEnd
   ,tInstruction.InstructionDescription
  ,tInstruction.ImplementedDate
  ,tInstruction.CompletedDate
  ,tInstruction.InstructionMinutes
  ,tInstruction.TimesPerWeek
   --,tFocusAreaDesignator.DesignatorID
   ,tGoal.GoalStartDate
   ,tGoal.GoalEndDate
   ,tGoal.GoalID
   ,tProgress.ProgressResultNameID
   ,tProgress.ProgressDate
   ,tProgress.LastUpdatedDate AS prgressLastUpdate
   ,tProgressResult.ProgressValue
   ,tGoal.SmartGoal --must be at end of query
FROM
    dbSOARS.rti.tStudentNeed (NOLOCK)
  JOIN dbSOARS.rti.tStudentNeedFocusArea (NOLOCK) ON
    tStudentNeedFocusArea.StudentNeedID = tStudentNeed.StudentNeedID
  JOIN dbSOARS.rti.tGoal (NOLOCK) ON
    tGoal.StudentNeedID = tStudentNeed.StudentNeedID
  JOIN dbSOARS.rti.tNeedType (NOLOCK) ON
   tNeedType.NeedTypeID = tStudentNeed.NeedTypeID
  JOIN dbSOARS.rti.tFocusArea (NOLOCK) ON
   tFocusArea.FocusAreaID = tStudentNeedFocusArea.FocusAreaID
  JOIN dbSOARS.rti.tGoalInstruction (NOLOCK) ON
    tGoalInstruction.GoalID = tGoal.GoalID
  JOIN dbSOARS.rti.tGoalFocusArea (NOLOCK) ON
    tGoalFocusArea.GoalID = tGoal.GoalID
  JOIN dbSOARS.rti.tProgress(NOLOCK) ON
    tProgress.GoalID = tGoal.GoalID
  JOIN dbSOARS.rti.tProgressResult (NOLOCK) ON
    tProgressResult.ProgressResultNameID = tProgress.ProgressResultNameID
  JOIN dbSoARS.rti.tInstruction (NOLOCK) ON
    tInstruction.InstructionID = tGoalInstruction.InstructionID AND
     tInstruction.studentNeedID = tStudentNeed.studentNeedID
  JOIN dbSOARS.rti.tFocusAreaDesignator (NOLOCK) ON
    tFocusAreaDesignator.FocusAreaID =  tStudentNeedFocusArea.FocusAreaID
WHERE
    tNeedType.NeedTypeName = 'READ'
  AND
    tStudentNeed.lastUpdatedDate > '2023-07-01'
  AND 
    tstudentNeed.ConcernStartDate < '2024-07-01'
  AND 
    tStudentNeed.ActiveFlag = 1
  AND 
  tStudentNeed.studentNeedID = 225291
 --AND 
 --  tStudentNeed.PersonID = 2241269
 -- AND
  -- tProgress.GoalID = 756463
  --AND
  --  StudentNeedFocusAreaID = 459840
  ORDER BY
    tStudentNeed.ConcernStartDate
"
)

# Query of READ Plan Flag in Campus ----
qrystudentGoal <- odbc::dbGetQuery(con, 
                                   "
  SELECT 
    ProgressMonitoring
    ,studentNeedID
    ,tGoal.GoalStartDate
   ,tGoal.GoalEndDate
  FROM
    dbSOARS.rti.tGoal (NOLOCK)
  WHERE
    studentNeedID = 225291
  "
)

completeRecord <- qrystudentPlan %>% 
  left_join(qrystudentGoal, 
            join_by(studentNeedID, 
                    GoalStartDate, 
                    GoalEndDate),
            relationship = "many-to-many")
