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
qrystudentPlan <- odbc::dbGetQuery(con, 
"
  SELECT DISTINCT
   tStudentNeed.studentNeedID
   ,tStudentNeed.FirstName
   ,tStudentNeed.LastName
   ,tStudentNeed.PersonID
   ,tStudentNeed.ConcernStartDate
   ,tStudentNeed.LastUpdatedDate
   --,tStudentNeed.NeedDescription --empty
   --,tStudentNeed.isDisplayedStaffDashboard --FALSE for sample student
   --,tStudentNeed.isDisplayedStudentParentDashboard  --FALSE for sample student
   ,tStudentNeed.ActiveFlag -- Not sure what this indicates
   ,tInstruction.ActiveFlag AS InstFlag -- Not sure what this indicates
   ,tGoal.ActiveFlag AS GoalFlag -- Not sure what this indicates
   ,tFocusAreaDesignator.ActiveFlag AS FocusFlag -- Not sure what this indicates
   ,tGoalInstruction.ActiveFlag AS goalInstFlag -- Not sure what this indicates
   ,tNeedType.ActiveFlag AS needFlag -- Not sure what this indicates
   ,tStudentNeedFocusArea.ActiveFlag AS focusAreaFlag -- Not sure what this indicates
   ,tStudentNeedFocusArea.StudentNeedFocusAreaID
   ,tFocusArea.FocusAreaName
   --,tFocusArea.StartYear --Not working 1900
   --,tFocusArea.EndYear --Not working 2099
   --,tGoal.ParentAcknowledgeDate -- NA for sample student
   -- ,tGoal.ProgressMonitoring --error: ODBC SQL Server Driver]Invalid Descriptor Index 
   ,tStudentNeedFocusArea.FocusAreaID
   --,tStudentNeedFocusArea.FocusAreaRanking
   ,tStudentNeedFocusArea.StartDate AS SNStart
   ,tStudentNeedFocusArea.EndDate AS SNEnd
   ,tInstruction.InstructionDescription
  ,tInstruction.ImplementedDate
  ,tInstruction.CompletedDate
  ,tInstruction.InstructionMinutes
  ,tInstruction.TimesPerWeek
   --,tFocusAreaDesignator.DesignatorID
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
 -- AND 
    --tStudentNeed.PersonID = 1647360
  ORDER BY
    tStudentNeed.ConcernStartDate
"
)

