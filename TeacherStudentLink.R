# Link Students to Teachers as READ Plans authors----
# Script header ----
# Title: student performance by teacher for Lawrence
# Author: Susan Switzer
# Created: 10/30/24
# Revised: 
# The purpose of this script is to link teachers and students for READ Plan analysis


# Load Libraries ----
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
,TeacherSection.TeacherName
,Section.homeroomSection
,SectionStudent.SchoolName
,Roster.personID
,StudentDemographic.LegalFirstName
,StudentDemographic.LegalLastName
,Enrollment.EnrollmentEndDate
,Enrollment.EndYear
,Enrollment.Grade
FROM Jeffco_IC.dbo.Course WITH (NOLOCK)
LEFT JOIN jeffco_IC.dbo.Section (nolock) ON 
  Course.courseID = Section.courseID
LEFT JOIN jeffco_IC.dbo.Roster (nolock) ON 
  Section.sectionID = Roster.SectionID
LEFT JOIN AchievementDW.dim.StudentDemographic (nolock) ON 
Roster.personID = StudentDemographic.PersonID 
LEFT JOIN AchievementDW.dim.SectionStudent (NOLOCK) ON 
  Roster.personID = SectionStudent.personID 
LEFT JOIN AchievementDW.dim.Enrollment WITH (NOLOCK) ON 
  SectionStudent.EnrollmentID = Enrollment.EnrollmentID AND 
  SectionStudent.PersonID = Enrollment.PersonID
LEFT JOIN AchievementDW.Dim.TeacherSection WITH (NOLOCK) ON 
  Roster.sectionID = TeacherSection.SectionID
WHERE COURSE.homeroom = 'TRUE'
AND course.active = 'TRUE'
AND Section.homeroomSection = 'TRUE'
AND	SectionStudent.IsInvalid = 0
AND StudentDemographic.IsInvalid = 0
AND
  StudentDemographic.LatestRecord = 1
AND
  Course.name = 'Homeroom 3'
AND
  Enrollment.EndYear = 2024
AND 
  Enrollment.Grade = '3'
--AND
 -- Roster.PersonID = 2240583 -- student will not show end year teacher
AND
  TeacherSection.PrimaryTeacherFlag = 1
AND	Enrollment.LatestRecord = 1
AND	StudentDemographic.LatestRecord = 1
AND Enrollment.EnrollmentType = 'Primary'
--AND
--Enrollment.EndStatus = ''
"
)
