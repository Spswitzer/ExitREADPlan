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
  SELECT
   tStudentNeed.studentNeedID
   ,tStudentNeed.FirstName
   ,tStudentNeed.LastName
   ,tStudentNeed.PersonID
   ,tStudentNeed.ConcernStartDate
   ,tStudentNeed.LastUpdatedDate
   ,tStudentNeed.ActiveFlag -- Not sure what this indicates
   ,tInstruction.ActiveFlag AS InstFlag
   ,tStudentNeedFocusArea.ActiveFlag AS NFAFlag
   ,tGoal.ActiveFlag AS GoalFlag
   ,tFocusAreaDesignator.ActiveFlag AS FocusFlag
   ,tGoalInstruction.ActiveFlag AS goalInstFlag
   ,tNeedType.ActiveFlag AS needFlag
   ,tStudentNeedFocusArea.ActiveFlag AS focusAreaFlag --The multi-part identifier tStudentFocusArea.ActiveFlag could not be bound.
   ,tStudentNeedFocusArea.StudentNeedFocusAreaID 
   ,tFocusArea.FocusAreaName
  --,tGoal.SmartGoal
   ,tStudentNeedFocusArea.FocusAreaID
  --,tStudentNeedFocusArea.FocusAreaRanking
  ,tStudentNeedFocusArea.StartDate AS SNStart
  ,tStudentNeedFocusArea.EndDate AS SNEnd
  -- ,tGoal.ProgressMonitoring --error: ODBC SQL Server Driver]Invalid Descriptor Index 
  ,tInstruction.InstructionDescription
  ,tInstruction.ImplementedDate
  ,tInstruction.CompletedDate
  ,tInstruction.InstructionMinutes
  ,tInstruction.TimesPerWeek
  ,tFocusAreaDesignator.DesignatorID
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
    tInstruction.InstructionID = tGoalInstruction.InstructionID
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
    tInstruction.ActiveFlag = 1
  AND
   tStudentNeedFocusArea.ActiveFlag = 1
  AND 
    tGoal.ActiveFlag = 1
--  AND 
--    tInstruction.ImplementedDate > '2023-07-01'
  AND
 tGoalFocusArea.StartDate > '2023-07-01'
  AND 
    tFocusAreaDesignator.ActiveFlag = 1
  AND 
    tGoalInstruction.ActiveFlag = 1
  ORDER BY
    tStudentNeed.ConcernStartDate
"
)


focusAreas <- qrystudentPlan %>% 
  filter(PersonID == 2229924)
  distinct(studentNeedID, PersonID, LastUpdatedDate, StudentNeedFocusAreaID, FocusAreaID, .keep_all = T)

