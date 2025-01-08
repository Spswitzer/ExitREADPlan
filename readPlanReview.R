# # Script header ----
# Title: READ Plan documents
# Author: Susan Switzer
# Created: 10/14/24
# The purpose of this script is to review READ Plan elements for 3rd students (2023-2024) from the kindergarten class of 2020-2021 

#load libraries ----
library(odbc)
library(DBI)
library(gt)
library(janitor)
library(tidyverse)

# extract Exited students from Flag query ----
#file in ExploreReadPlanStatusCleanedUp.R must be ran first to populate flagStart to environment

# Pull IDs from each student group ----
## Students Exited Plan ----
flagStart <- readRDS('data/flagStart.rds')

planEnd <- flagStart %>% 
  filter(planEnd %in% c( 'K- Exit plan','3- Exit plan', '2- Exit plan', '1- Exit plan')) %>% 
  pull(personID)

# toString(planEnd)


planEndGrade3<- flagStart %>% 
  filter(planEnd %in% c('3- Exit plan')) %>% 
  pull(personID)

## PlanEnd Exited with Cmas MeetsExceeds ----
cmasPerformance <- 
  readRDS('data/cmasPerformance.rds')  

flagGrade3 <- flagStart %>% 
  filter(gradeInt == 3, 
         EndYear == 2024
  ) %>% 
  distinct(personID, planEnd)

grade3Cmas <- flagGrade3 %>% 
  ungroup() %>% 
  left_join(cmasPerformance, join_by(personID == personId)) %>% 
  filter(!is.na(cmasProfLevel)) %>%
  # filter(testName == 	'CMAS ELA Grade 03 2023-24') %>%
  # filter(cmasProfLevel %in% c('Exceeded Expectations', 'Met Expectations')) %>%
  distinct(personID, cmasProfLevel, .keep_all = T) %>%
  mutate(cmasProfLevel = str_remove(cmasProfLevel, ' Expectations')) %>% 
  mutate(cmasProfLevel = factor(cmasProfLevel, 
                                levels= c('Exceeded', 
                                          'Met', 
                                          'Approached', 
                                          'Partially Met', 
                                          'Did Not Yet Meet'))) %>% 
  mutate(planEnd = case_when(
    is.na(planEnd) ~ 0, 
    TRUE ~ 1
  )) %>% 
  mutate(pl = case_when(
    cmasProfLevel %in% c('Exceeded', 'Met') ~ 1, 
    TRUE ~ 0
  )) %>% 
  mutate(totalN = n()) %>% 
  group_by(planEnd) %>% 
  mutate(planN = n()) %>% 
  group_by(planEnd, pl) %>% 
  mutate(plN = n()) %>% 
  mutate(studentGroup = case_when(
    planEnd == 0 & pl == 0 ~ 'Plan DNM', 
    planEnd == 0 & pl == 1 ~ 'Plan ME', 
    planEnd == 1 & pl == 0 ~ 'Exit DNM', 
    planEnd == 1 & pl == 1 ~ 'Exit ME', 
    TRUE ~ 'Error'
  )) %>% 
  ungroup() %>% 
  select(personID, studentGroup)

exitCmasMeetExceed <- grade3Cmas %>% 
  filter(studentGroup %in% c('Exit DNM', 'Exit ME')) %>% 
  pull(personID)

# toString(exitCmasMeetExceed)
## Students Still on Plan ----
NoPlanEnd <- flagStart %>% 
  select(personID, planStart, planEnd) %>% 
  mutate(planStart = str_remove(planStart, '- Start plan'), 
         planEnd = str_remove(planEnd, '- Exit plan')) %>% 
  pivot_longer(c(planStart, planEnd), 
               names_to = 'status') %>% 
  filter(!is.na(value)) %>% 
  pivot_wider(names_from = status, 
              values_from = value) %>% 
  ungroup() %>% 
  filter(is.na(planEnd)) %>% 
  pull(personID)

# toString(NoPlanEnd)


## Students Still on Plan with CMAS M/E ----
noExitCmas <- grade3Cmas %>% 
  filter(studentGroup %in% c('Plan DNM', 'Plan ME')) %>% 
  pull(personID)

# toString(noExitCmas)

# connect to SQL database ----
con <- dbConnect(odbc(),
                 Driver = "SQL Server",
                 Server = "qdc-soars-test",
                 trusted_connection = "true",
                 Port = 1433)

sort(unique(odbcListDrivers()[[1]]))

# Need Focus Area Result Exited students only----
qrystudentFocusAreas <- odbc::dbGetQuery(con, 
                                         "
    SELECT
   tStudentNeed.studentNeedID
   ,tStudentNeed.FirstName
   ,tStudentNeed.LastName
   ,tStudentNeed.PersonID
   ,tFocusArea.FocusAreaName
   ,tFocusArea.ListSortOrder
   ,tGoalFocusArea.StartDate
   ,tGoalFocusArea.EndDate
   ,tGoal.GoalID
   ,tGoal.CreatedDate
   ,tStudentNeedFocusArea.FocusAreaID
FROM
    dbSOARS.rti.tStudentNeed (NOLOCK)
  JOIN dbSOARS.rti.tStudentNeedFocusArea (NOLOCK) ON
    tStudentNeedFocusArea.StudentNeedID = tStudentNeed.StudentNeedID
  JOIN dbSOARS.rti.tGoal (NOLOCK) ON
    tGoal.StudentNeedID = tStudentNeed.StudentNeedID
  JOIN dbSoars.rti.tGoalFocusArea (NOLOCK) ON
    tGoalFocusArea.GoalID = tGoal.GoalID 
  JOIN dbSOARS.rti.tNeedType (NOLOCK) ON
   tNeedType.NeedTypeID = tStudentNeed.NeedTypeID
  JOIN dbSOARS.rti.tFocusArea (NOLOCK) ON
   tFocusArea.FocusAreaID = tStudentNeedFocusArea.FocusAreaID AND
    tFocusArea.FocusAreaID = tGoalFocusArea.FocusAreaID 
WHERE
    tNeedType.NeedTypeName = 'READ'
  AND 
    tStudentNeed.ActiveFlag = 1
  AND
    tStudentNeed.PersonID IN (1453134, 1492205, 1492288, 1495409, 1495426, 1517773, 1518327, 1521347, 1522073, 1522625, 1523935, 1526117, 1527399, 1548566, 1554950, 1557323, 1557545, 1558787, 1560641, 1561011, 1561744, 1563560, 1565192, 1566037, 1572832, 1580806, 1581982, 1583060, 1584033, 1587167, 1588190, 1589221, 1589621, 1590608, 1596932, 1599992, 1610465, 1610644, 1611546, 1615543, 1616123, 1619775, 1622387, 1623016, 1623318, 1623643, 1623736, 1625455, 1625660, 1626000, 1626044, 1626642, 1626897, 1628873, 1629266, 1629838, 1630073, 1630077, 1630521, 1630794, 1631475, 1631604, 1632307, 1632517, 1632653, 1634300, 1634322, 1635250, 1635412, 1635503, 1637459, 1637830, 1641268, 1641914, 1642917, 1643060, 1644105, 1644629, 1644647, 1644670, 1645397, 1645712, 1646453, 1646863, 1647125, 1651303, 1652729, 1653977, 1654739, 1655740, 1656982, 1657085, 1657406, 1658165, 1658731, 1661663, 1662029, 1664117, 1665255, 1665361, 1666235, 1666305, 1667736, 1672131, 1672419, 1672680, 1672956, 1673312, 1675167, 1676192, 1676294, 1676418, 1676546, 1678435, 1678964, 1682050, 1682715, 1682928, 1685526, 1686209, 1687566, 1687715, 1687910, 1690600, 1690772, 1690890, 1691704, 1691920, 1692053, 1692474, 1692480, 1692561, 1692762, 1692953, 1693215, 1693230, 1693743, 1694249, 1694301, 1694566, 1694921, 1695545, 1695750, 1696586, 1696780, 1697135, 1698074, 1698623, 1698689, 1698764, 1698909, 1698980, 1699014, 1699292, 1699763, 1700714, 1701627, 1703648, 1705635, 1706041, 1707524, 1708279, 1715098, 1715155, 1715175, 1716487, 1716495, 1721741, 1723678, 1727420, 1728564, 1728570, 1733518, 1734217, 1735370, 1735937, 1736424, 1736682, 1736819, 1736898, 1737029, 1737032, 1737454, 1740349, 1740750, 1741670, 1741997, 1742076, 1743646, 1744669, 1744718, 1745204, 1745838, 1746453, 1747217, 1748408, 1753356, 1753395, 1754366, 1759355, 1759768, 1759985, 1761226, 1761227, 1761739, 1761800, 1761924, 1761968, 1761999, 1762278, 1762642, 1762816, 1762834, 1762867, 1762868, 1762902, 1762908, 1762916, 1763104, 1763107, 1763271, 1763572, 1763643, 1763752, 1763961, 1764114, 1764539, 1764580, 1764610, 1764958, 1765131, 1765183, 1765236, 1765612, 1765624, 1765780, 1765827, 1765933, 2224429, 2225243, 2225351, 2225394, 2225481, 2225610, 2225681, 2225745, 2225776, 2225790, 2225975, 2226138, 2226510, 2226533, 2226723, 2226746, 2226901, 2226916, 2226979, 2227098, 2227108, 2227375, 2227407, 2227476, 2227615, 2227764, 2227782, 2227804, 2227885, 2227997, 2228195, 2228347, 2228444, 2228571, 2229940, 2230040, 2230174, 2230204, 2230216, 2230222, 2230283, 2230475, 2231058, 2231180, 2231223, 2231226, 2231229, 2231530, 2231595, 2232722, 2232769, 2232796, 2234137, 2234872, 2235446, 2235574, 2235614, 2235634, 2235635, 2235674, 2235781, 2235976, 2236084, 2236101, 2236166, 2236409, 2236442, 2236471, 2236480, 2236586, 2237214, 2237720, 2238027, 2238038, 2238168, 2238284, 2238308, 2238684, 2239002, 2239152, 2239435, 2239449, 2239514, 2239604, 2239624, 2239634, 2239929, 2240050, 2240059, 2240062, 2240124, 2240177, 2240583, 2241219, 2241291, 2241682, 2241817, 2241859, 2241904, 2241978, 2242101, 2242208, 2242364, 2242736, 2243219, 2243223, 2243324, 2243611, 2243683, 2243752, 2243788, 2244119, 2244122, 2244148, 2244858, 2244860, 2244862, 2245234, 2245496, 2246137, 2248247, 2248475, 2248841, 2250874, 2256232, 2256241, 2257869, 2261264, 2267177, 2267331, 2269294, 2270072, 2271165, 2274088, 2276275, 2276556, 2276959, 2277056, 2277330, 2277450, 2277464, 2277632, 2278431, 2278934, 2279185, 2279487, 2280657, 2280747, 2280950, 2281070, 2282314, 2285074, 2286327, 2286457, 2286638, 2287001, 2287463, 2287500, 2298439, 2298739, 2299772, 2303033, 2311833, 2319955, 2324065, 2326786, 2329402, 2332320) --Exited students
  ORDER BY
    tStudentNeed.ConcernStartDate
"
)

## Query for all students ----
qrystudentFocusAreas <- odbc::dbGetQuery(con, 
                                         "
    SELECT
   tStudentNeed.studentNeedID
   ,tStudentNeed.FirstName
   ,tStudentNeed.LastName
   ,tStudentNeed.PersonID
   ,tFocusArea.FocusAreaName
   ,tFocusArea.ListSortOrder
   ,tGoalFocusArea.StartDate
   ,tGoalFocusArea.EndDate
   ,tGoal.GoalID
   ,tGoal.CreatedDate
   ,tStudentNeedFocusArea.FocusAreaID
FROM
    dbSOARS.rti.tStudentNeed (NOLOCK)
  JOIN dbSOARS.rti.tStudentNeedFocusArea (NOLOCK) ON
    tStudentNeedFocusArea.StudentNeedID = tStudentNeed.StudentNeedID
  JOIN dbSOARS.rti.tGoal (NOLOCK) ON
    tGoal.StudentNeedID = tStudentNeed.StudentNeedID
  JOIN dbSoars.rti.tGoalFocusArea (NOLOCK) ON
    tGoalFocusArea.GoalID = tGoal.GoalID 
  JOIN dbSOARS.rti.tNeedType (NOLOCK) ON
   tNeedType.NeedTypeID = tStudentNeed.NeedTypeID
  JOIN dbSOARS.rti.tFocusArea (NOLOCK) ON
   tFocusArea.FocusAreaID = tStudentNeedFocusArea.FocusAreaID AND
    tFocusArea.FocusAreaID = tGoalFocusArea.FocusAreaID 
WHERE
    tNeedType.NeedTypeName = 'READ'
  AND 
    tStudentNeed.ActiveFlag = 1
  ORDER BY
    tStudentNeed.ConcernStartDate
"
)

unique(qrystudentFocusAreas$PersonID)

## Summarize goal focus areas ----
planFocusAreas <- qrystudentFocusAreas %>% 
  clean_names('lower_camel') %>% 
  filter(personId %in% NoPlanEnd) %>%
  select(studentNeedId, firstName, lastName, personId, 
         focusAreaName, focusAreaId, focusStartDate = startDate) %>% 
  mutate(focusStartDate = as_date(focusStartDate), 
         focusStartDate = ymd(focusStartDate), 
         N = n_distinct(personId)) %>% 
  arrange(personId, focusAreaId, focusStartDate) %>% 
  group_by(personId, focusAreaId, focusStartDate, focusAreaName, N) %>% 
  summarise(focusAreaName = first(focusAreaName)) %>% 
  group_by(personId,  focusAreaName, N) %>% 
  summarise(focusStartDate = max(focusStartDate)) %>% 
  group_by(focusAreaName, N) %>% 
  reframe(focusAreaCount = n()) %>% 
  mutate(focusAreaCountPct = round(focusAreaCount/N, 2)) %>% 
  arrange(desc(focusAreaCountPct))

gt(planFocusAreas) %>% 
  cols_label(focusAreaName = 'Focus Area', 
             focusAreaCountPct = 'Percent of Exited Plans') %>% 
  tab_header(title = 'Plan Focus Areas for Students Who Exited READ Plan By The End of Grade 3', 
             subtitle = '2020-2021 Kindergarten Student Cohort (Grade 3 in 2023-2024)') %>% 
  cols_align(
    align = c("left"),
    columns = focusAreaName
  ) %>% 
  cols_align(
    align = c("center"),
    columns = c(focusAreaCountPct)
  ) %>% 
  cols_hide(c(N, focusAreaCount))%>% 
  fmt_percent(focusAreaCountPct, decimals = 0) %>% 
  opt_table_outline() %>% 
  tab_style(
    cell_fill('grey'), 
    cells_row_groups()
  ) %>% 
  tab_options(
    table.font.size = 12
  ) 

### Explore students with no goal focus area ----
planAndFlag <- data.frame(personID = unique(qrystudentFocusAreas$PersonID)) %>% 
  mutate(plan = 'Y')

planFlag <- flagStart %>% 
  filter(planEnd %in% c('K- Exit plan','3- Exit plan', '2- Exit plan', '1- Exit plan')) %>% 
  select(personID, planEnd)

noPlanYesFlag <- planAndFlag %>% 
 full_join(planFlag, join_by(personID == personID)) %>% 
  filter(is.na(plan))

#### Grouped by exit year ----
planFocusAreasPlanEndGrade <- qrystudentFocusAreas %>% 
  clean_names('lower_camel') %>% 
  filter(personId %in% NoPlanEnd) %>%
  select(studentNeedId, firstName, lastName, personId, 
         focusAreaName, focusAreaId, focusStartDate = startDate) %>% 
  mutate(focusStartDate = as_date(focusStartDate), 
         focusStartDate = ymd(focusStartDate)) %>% 
  full_join(planFlag, join_by(personId == personID)) %>% 
  filter(!is.na(studentNeedId)) %>% 
  mutate(N = n_distinct(personId)) %>% 
  group_by(planEnd) %>% 
  mutate(planEndN = n_distinct(personId)) %>% 
  arrange(personId, focusAreaId, focusStartDate, planEnd) %>% 
  group_by(personId, focusAreaId, focusStartDate, focusAreaName, N, planEnd) %>% 
  summarise(focusAreaName = first(focusAreaName), 
            planEndN = first(planEndN)) %>% 
  group_by(personId,  focusAreaName, N, planEnd, planEndN) %>% 
  summarise(focusStartDate = max(focusStartDate)) %>% 
  group_by(focusAreaName, N, planEnd, planEndN) %>% 
  reframe(focusAreaCount = n()) %>% 
  mutate(focusAreaCountPct = round(focusAreaCount/planEndN, 2)) %>% 
  arrange(planEnd, desc(focusAreaCountPct)) %>% 
  group_by(planEnd)

gt(planFocusAreasPlanEndGrade) %>% 
  cols_label(focusAreaName = 'Focus Area', 
             focusAreaCountPct = 'Percent of Exited Plans') %>% 
  tab_header(title = 'Plan Focus Areas for Students Who Exited READ Plan By The End of Grade 3', 
             subtitle = '2020-2021 Kindergarten Student Cohort (Grade 3 in 2023-2024)') %>% 
  cols_align(
    align = c("left"),
    columns = focusAreaName
  ) %>% 
  cols_align(
    align = c("center"),
    columns = c(focusAreaCountPct)
  ) %>% 
  cols_hide(c(N, focusAreaCount, planEndN))%>% 
  fmt_percent(focusAreaCountPct, decimals = 0) %>% 
  opt_table_outline() %>% 
  tab_style(
    cell_fill('grey'), 
    cells_row_groups()
  ) %>% 
  tab_options(
    table.font.size = 12
  ) 

#### Grouped by plan year Revised Approach----
planFocusAreasPlanYear <- qrystudentFocusAreas %>% 
  clean_names('lower_camel') %>% 
  filter(personId %in% NoPlanEnd) %>%
  select(studentNeedId, firstName, lastName, personId, 
         focusAreaName, focusAreaId, focusStartDate = startDate) %>% 
  mutate(focusStartDate = as_date(focusStartDate), 
         focusStartDate = ymd(focusStartDate)) %>% 
  full_join(planFlag, join_by(personId == personID)) %>% 
  filter(!is.na(studentNeedId)) %>% 
  mutate(N = n_distinct(personId)) %>%
  mutate(gradeLevel = case_when(
    focusStartDate >= '2020-08-01' & focusStartDate <='2021-05-30' ~ 0, 
    focusStartDate >= '2021-08-01' & focusStartDate <='2022-05-30' ~ 1, 
    focusStartDate >= '2022-08-01' & focusStartDate <='2023-05-30' ~ 2, 
    focusStartDate >= '2023-08-01' & focusStartDate <='2024-05-30' ~ 3, 
    TRUE ~ 9
  )) %>% 
  filter(gradeLevel != 9) %>% # 18 students with goal focus areas before K, repeated K
  group_by(gradeLevel) %>% 
  mutate(gradeN = n_distinct(personId)) %>% 
  arrange(personId, focusAreaId, focusStartDate, gradeLevel) %>% 
  group_by(personId, focusAreaId, focusStartDate, focusAreaName, N, gradeLevel) %>% 
  summarise(focusAreaName = first(focusAreaName), 
            gradeN = first(gradeN)) %>% 
  group_by(personId,  focusAreaName, N, gradeLevel, gradeN) %>% 
  summarise(focusStartDate = max(focusStartDate)) %>% 
  group_by(focusAreaName, N, gradeLevel, gradeN) %>% 
  reframe(focusAreaCount = n()) %>% 
  mutate(focusAreaCountPct = round(focusAreaCount/gradeN, 2)) %>% 
  arrange(gradeLevel, desc(focusAreaCountPct)) %>% 
  mutate(gradeLevel = factor(gradeLevel, 
                             levels = 0:3, 
                             labels = c('Kindergarten', '1st Grade', 
                                        '2nd Grade', '3rd Grade'))) %>% 
  group_by(gradeLevel)

gt(planFocusAreasPlanYear) %>% 
  cols_label(focusAreaName = 'Focus Area', 
             focusAreaCountPct = 'Percent of Exited Plans') %>% 
  tab_header(title = 'Plan Focus Areas for Students Who Exited READ Plan By The End of Grade 3', 
             subtitle = '2020-2021 Kindergarten Student Cohort (Grade 3 in 2023-2024)') %>% 
  cols_align(
    align = c("left"),
    columns = focusAreaName
  ) %>% 
  cols_align(
    align = c("center"),
    columns = c(focusAreaCountPct)
  ) %>% 
  cols_hide(c(N, focusAreaCount, gradeN))%>% 
  fmt_percent(focusAreaCountPct, decimals = 0) %>% 
  opt_table_outline() %>% 
  tab_style(
    cell_fill('grey'), 
    cells_row_groups()
  ) %>% 
  tab_options(
    table.font.size = 12
  ) 

##### Summarize focus areas ----
instructionPm <- qrystudentFocusAreas %>% 
  clean_names('lower_camel') %>% 
  filter(personId %in% NoPlanEnd) %>%
  mutate(studentCount = n_distinct(personId)) %>% 
  group_by(personId) %>% 
  mutate(focusCount = n_distinct(focusAreaId)) %>% 
  ungroup() %>% 
  summarise(meanFocusAreas = mean(focusCount), 
            rangeFocusAreaMin = range(instructionPm$focusCount)[[1]], 
            rangeFocusAreaMax = range(instructionPm$focusCount)[[2]])


# Need Instructional Plan ----
qrystudentPlan <- odbc::dbGetQuery(con, 
                                   "
 SELECT
   tStudentNeed.studentNeedID
   ,tStudentNeed.FirstName
   ,tStudentNeed.LastName
   ,tStudentNeed.PersonID
   ,tInstruction.ImplementedDate
   ,tInstruction.CompletedDate
   ,tInstruction.InstructionID
   ,tInstruction.InstructionDescription
   ,tInstruction.InstructionMinutes
   ,tInstruction.TimesPerWeek
   ,tInstruction.NumberOfTimes
   ,tInstructionFrequencyType.InstructionFrequencyTypeName
   ,tInstruction.InstructionFrequencyTypeID
   ,tInstructionType.InstructionTypeName
   ,tImplementerRole.ImplementerRoleName
  -- ,tGoal.ParentAcknowledgeDate
  --  ,tStudentFilterType.StartDateParentAcknowledgeDate
  --  ,tStudentFilterType.EndDateParentAcknowledgeDate
  --  ,tFilterType.InternalCD
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
  JOIN dbSoArs.rti.tInstruction (NOLOCK) ON
    tInstruction.InstructionID = tGoalInstruction.InstructionID AND
     tInstruction.studentNeedID = tStudentNeed.studentNeedID
  JOIN dbSoars.rti.tFocusAreaDesignator (NOLOCK) ON
    tFocusAreaDesignator.FocusAreaID =  tStudentNeedFocusArea.FocusAreaID
  JOIN dbSoars.rti.tImplementerRole (NOLOCK) ON
    tImplementerRole.ImplementerRoleID = tInstruction.ImplementerRoleID
  JOIN dbSoars.rti.tInstructionFrequencyType (NOLOCK) ON
   tInstructionFrequencyType.InstructionFrequencyTypeID = tInstruction.InstructionFrequencyTypeID
  JOIN dbSoars.rti.tInstructionType (NOLOCK) ON
    tInstructionType.InstructionTypeID = tInstruction.InstructionTypeID
--  JOIN dbSoars.dbo.tStudentFilterType (NOLOCK) ON
--    tStudentFilterType.personID = tStudentNeed.PersonID
--  JOIN dbSoars.dbo.tFilterType (NOLOCK) ON
--    tFilterType.FilterID = tStudentFilterType.FilterID
WHERE
    tNeedType.NeedTypeName = 'READ'
  AND 
    tStudentNeed.ActiveFlag = 1
  --AND 
  --  tFilterType.InternalCD = 'READ'
  ORDER BY
    tStudentNeed.ConcernStartDate
"
)
# Access Frequency Duration and Intensity ----
## Clean Plan Function ----
cleanStudentPlans <- function(stuGroup) {

  personIds <- grade3Cmas %>% 
    filter(studentGroup == stuGroup) %>% 
    pull(personID)
  
  #Goal Volume, Frequency, Intensity, Duration ----
  cleanPlans <- qrystudentPlan %>% 
    clean_names('lower_camel') %>% 
    filter(personId %in% personIds) %>% 
    mutate(implementedDate = ymd(as_date(implementedDate)),
           completedDate = ymd(as_date(completedDate))
    ) %>% 
    arrange(personId, desc(implementedDate)) %>% 
    distinct(personId, instructionId, 
             .keep_all = T) %>% 
    group_by(personId) %>% 
    filter(implementedDate == max(implementedDate)) 
  
  instructionInteractions <-  cleanPlans %>% 
    mutate(instructionN = n_distinct(personId)) %>% 
    group_by(instructionTypeName, instructionFrequencyTypeId) %>% 
    mutate(minutesMean = round(mean(instructionMinutes, na.rm = T), 1), 
           freqMean = round(mean(numberOfTimes, na.rm = T), 1)) %>% 
    group_by(instructionTypeName) %>% #implementerRoleName
    mutate(roleN = n()) %>% 
    group_by(instructionTypeName, instructionFrequencyTypeName) %>% #implementerRoleName
    summarise(minutesMean = first(minutesMean), 
              freqMean = first(freqMean), 
              roleN = first(roleN)) %>% 
    filter(instructionTypeName != 'Core', 
           instructionFrequencyTypeName == 'Week') %>% 
    mutate(studentGroup = stuGroup) %>% 
    arrange(instructionTypeName, desc(roleN))
  
  feqIntDur <- cleanPlans %>% 
    mutate(instructionN = n_distinct(personId)) %>% 
    group_by(instructionTypeName) %>% 
    mutate(instTypeN = n()) %>% 
    group_by(instructionTypeName, instructionFrequencyTypeName) %>% 
    summarise(instTypeN = first(instTypeN),
              instIntensityN = n(),
              minutesMean = round(mean(instructionMinutes, na.rm = T), 1), 
              freqMean = round(mean(numberOfTimes, na.rm = T), 1)) %>% 
    filter(instructionTypeName != 'Core', 
           instructionFrequencyTypeName == 'Week') %>% 
    mutate(studentGroup = stuGroup) %>% 
    arrange(instructionTypeName, desc(instIntensityN ))
  
  
  ## Explore students with no instructional plan ----
  instructionAndFlag <- data.frame(personId = unique(cleanPlans$personId)) %>% 
    mutate(plan = 'Y') %>% 
    mutate(n = n_distinct(personId)) %>% 
    distinct(n) %>% 
    pull()

  return(list(cleanPlans = cleanPlans, 
              instructionInteractions = instructionInteractions, 
              feqIntDur = feqIntDur, 
              instructionAndFlag = instructionAndFlag))
}

## create groups ----
studentGroups <- unique(grade3Cmas$studentGroup)

# cleanStudentPlans(stuGroup = 'Exit ME')

## Map groups through function ----
allResults <- map(.x = studentGroups, .f = cleanStudentPlans)

### Access N of service providers for each type of instructional service ----
instructionalInteractions <- allResults[[1]]$instructionInteractions %>% 
  bind_rows(allResults[[2]]$instructionInteractions) %>% 
  bind_rows(allResults[[3]]$instructionInteractions) %>% 
  bind_rows(allResults[[4]]$instructionInteractions) %>% 
  select(instructionTypeName, studentGroup, roleN) %>% #implementerRoleName
  group_by(studentGroup, instructionTypeName) %>% 
  # top_n(3) %>% 
  mutate(studentGroup = factor(studentGroup, 
                               levels = c('Exit ME', 
                                          'Exit DNM', 
                                          'Plan ME', 
                                          'Plan DNM'), 
                               labels = c(c('Exit Read Plan In Grade 3 and CMAS Meet/Exceed', 
                                            'Exit Read Plan In Grade 3 and CMAS Did Not Meet/Exceed', 
                                            'Did not Exit Plan and CMAS Meet/Exceed', 
                                            'Did not Exit Plan and CMAS Did Not Meet/Exceed'))))

### Access frequency and duration of each type of instructional service ----
freqDuration <- allResults[[1]][["feqIntDur"]] %>% 
  bind_rows(allResults[[2]][["feqIntDur"]]) %>% 
  bind_rows(allResults[[3]][["feqIntDur"]]) %>% 
  bind_rows(allResults[[4]][["feqIntDur"]]) %>% 
  mutate(studentGroup = factor(studentGroup, 
                               levels = c('Exit ME', 
                                          'Exit DNM', 
                                          'Plan ME', 
                                          'Plan DNM'), 
                               labels = c(c('Exit Read Plan In Grade 3 and CMAS Meet/Exceed', 
                                            'Exit Read Plan In Grade 3 and CMAS Did Not Meet/Exceed', 
                                            'Did not Exit Plan and CMAS Meet/Exceed', 
                                            'Did not Exit Plan and CMAS Did Not Meet/Exceed')))) %>% 
  group_by(studentGroup)
  
### Access N of students with CMAS score and documented instructional services ----
allResults[[1]]$instructionAndFlag
allResults[[2]]$instructionAndFlag
allResults[[3]]$instructionAndFlag 
allResults[[4]]$instructionAndFlag


#### Table for N of service providers for each type of instructional service ----
gt(instructionalInteractions) %>% 
  cols_label(implementerRoleName = 'Implementer Role', 
             roleN = 'Count') %>% 
  fmt_number(roleN, decimals = 0) %>% 
  tab_header(title = 'N of service providers for weekly services') %>% 
  cols_align(
    align = c("left"),
    columns = implementerRoleName
  ) %>% 
  cols_align(
    align = c("center"),
    columns = roleN
  ) %>% 
  opt_table_outline() %>% 
  tab_style(
    cell_fill('grey'), 
    cells_row_groups()
  ) %>% 
  tab_options(
    table.font.size = 12
  )

#### Table for frequency and duration of each type of instructional service ----
gt(freqDuration) %>% 
  cols_label(instructionTypeName = 'Type of Instruction', 
             minutesMean = 'Mean of Minutes', 
             freqMean = 'Mean of service frequency') %>% 
  cols_hide(c(instructionFrequencyTypeName, instTypeN, instIntensityN)) %>% 
  tab_header(title = 'Means for weekly services') %>% 
  cols_align(
    align = c("left"),
    columns = instructionTypeName
  ) %>% 
  cols_align(
    align = c("center"),
    columns = c(minutesMean, freqMean)
  ) %>% 
  opt_table_outline() %>% 
  tab_style(
    cell_fill('grey'), 
    cells_row_groups()
  ) %>% 
  tab_options(
    table.font.size = 12
  )

# Access the names of the resources for intervention ----
## Access Lookup table with the names of the resources and the names collapsed ----
interventionWordsCsv <- read_csv('G:/Shared drives/Research & Assessment Design (RAD)/L1 Projects/3rd Grade Reading/B  Scan/Document Review/READActApprovedResources.csv', 
                                col_select = c('regex', 'collapsed') )%>% 
  filter(!is.na(regex)) %>% 
  clean_names('lower_camel') 

regexString <- interventionWordsCsv$regex

### Compare the CDE approved resources to those found the Jeffco data set
instructionalInterventions <- qrystudentPlan %>% 
  clean_names('lower_camel') %>% 
  filter(instructionTypeName != 'Core') %>% 
  filter(personId %in% c(exitCmasMeetExceed, noExitCmas, NoPlanEnd, planEnd)) %>% 
  select(personId, instructionDescription, instructionId) %>%
  mutate(
    intervention = instructionDescription %>%
      str_to_lower() %>%
      str_squish() %>% #remove whitespace
      str_extract_all(
        str_c(
          regexString
          , collapse = "|")
        )
    ) %>% 
  unnest_longer(intervention, keep_empty = TRUE) %>% 
  left_join(interventionWordsCsv, 
            join_by(intervention == regex), 
            relationship = "many-to-many")

#Distinct instances of interventions used ----
instructionInfoDistinct <- function(stuGroup) {
  
  personIds <- grade3Cmas %>% 
    filter(studentGroup == stuGroup) %>% 
    pull(personID)
  
instInter <- instructionalInterventions %>% 
  mutate(collapsed = case_when(
    intervention == 'lli' ~ 'LLI', 
    intervention == 'o.g.' ~ 'Orton Gillingham',
    intervention == 'og ' ~ 'Orton Gillingham',
    is.na(intervention) ~ 'No resource Identified',
    TRUE ~ collapsed
  )) %>% 
  filter(personId %in% personIds) %>% 
  distinct(personId, collapsed) %>% 
  group_by(collapsed) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  head(5) %>% 
  mutate(studentGroup = stuGroup)
}

studentGroups <- unique(grade3Cmas$studentGroup)

## Map groups through function ----
interventionResults <- map(.x = studentGroups, .f = instructionInfoDistinct) %>% 
  bind_rows() %>%
  mutate(studentGroup = factor(studentGroup, 
                               levels = c('Exit ME', 
                                          'Exit DNM', 
                                          'Plan ME', 
                                          'Plan DNM'), 
                               labels = c(c('Exit Read Plan In Grade 3 and CMAS Meet/Exceed', 
                                            'Exit Read Plan In Grade 3 and CMAS Did Not Meet/Exceed', 
                                            'Did not Exit Plan and CMAS Meet/Exceed', 
                                            'Did not Exit Plan and CMAS Did Not Meet/Exceed')))) %>% 
  group_by(studentGroup)

gt(interventionResults) %>% 
  cols_label(collapsed = 'Resource Cited', 
             count = 'Count of Instances') %>% 
  fmt_number(columns = count, 
             decimals = 0) %>% 
  tab_header(title = 'Counts of Instances resources were used') %>% 
  cols_align(
    align = c("left"),
    columns = collapsed
  ) %>% 
  cols_align(
    align = c("center"),
    columns = count
  ) %>% 
  opt_table_outline() %>% 
  tab_style(
    cell_fill('grey'), 
    cells_row_groups()
  ) %>% 
  tab_options(
    table.font.size = 12
  )


instructionInfo <- function(stuGroup) {
 
   personIds <- grade3Cmas %>% 
    filter(studentGroup == stuGroup) %>% 
    pull(personID)
  
  instructInter <- instructionalInterventions %>% 
    mutate(collapsed = case_when(
      intervention == 'lli' ~ 'LLI', 
      intervention == 'o.g.' ~ 'Orton Gillingham',
      intervention == 'og ' ~ 'Orton Gillingham',
      is.na(intervention) ~ 'No resource Identified',
      TRUE ~ collapsed
    )) %>% 
    filter(personId %in% personIds) %>% 
    distinct(personId, 
             instructionId,
             intervention, 
             collapsed,
             instructionDescription,
             .keep_all = T) %>% 
    ungroup() %>% 
    group_by(collapsed) %>% #firstName, lastName, personId, instructionId, intervention, instructionTypeName
    summarise(n = n()) %>% 
    arrange(desc(n)) %>% 
    head(5) %>% 
    mutate(studentGroup = stuGroup)
}

studentGroups <- unique(grade3Cmas$studentGroup)

## Map groups through function ----
interventionResults <- map(.x = studentGroups, .f = instructionInfo) %>% 
  bind_rows() %>%
  mutate(studentGroup = factor(studentGroup, 
                               levels = c('Exit ME', 
                                          'Exit DNM', 
                                          'Plan ME', 
                                          'Plan DNM'), 
                               labels = c(c('Exit Read Plan In Grade 3 and CMAS Meet/Exceed', 
                                            'Exit Read Plan In Grade 3 and CMAS Did Not Meet/Exceed', 
                                            'Did not Exit Plan and CMAS Meet/Exceed', 
                                            'Did not Exit Plan and CMAS Did Not Meet/Exceed')))) %>% 
  group_by(studentGroup)

gt(interventionResults) %>% 
  cols_label(collapsed = 'Resource Cited', 
             n = 'Count of Instances') %>% 
  fmt_number(columns = n, 
             decimals = 0) %>% 
  tab_header(title = 'Counts of Instances resources were used') %>% 
  cols_align(
    align = c("left"),
    columns = collapsed
  ) %>% 
  cols_align(
    align = c("center"),
    columns = n
  ) %>% 
  opt_table_outline() %>% 
  tab_style(
    cell_fill('grey'), 
    cells_row_groups()
  ) %>% 
  tab_options(
    table.font.size = 12
  )


  
  # Parent Acknowledgement ----
  ## Query of students with parent acknowledgement of READ Plan ----
  qryPlanAcknowledge <- odbc::dbGetQuery(con,
 "
SELECT
  StudentNeedID
  ,PersonID
  ,CurrentStatusCode
  ,StatusDate
  ,AcknowledgeDate
  ,CreatedDate
  ,IsOpen
FROM
  dbSoars.rti.vCurrentNeedStatus (NOLOCK)
WHERE
  NeedTypeID = 7
")

  planAcknowledge <- qryPlanAcknowledge %>% 
    clean_names('lower_camel') %>% 
    filter(personId %in% NoPlanEnd | personId %in% planEnd) %>% 
    mutate(acknowledgedPlan = case_when(
      is.na(acknowledgeDate) ~ 0, 
      TRUE ~ 1
    )) %>% 
    group_by(isOpen) %>% 
    mutate(n = n()) %>% 
    group_by(isOpen, acknowledgedPlan) %>% 
    mutate(acknowN = n(), 
           acknowPct = acknowN/n) %>% 
    group_by(currentStatusCode, acknowledgedPlan) %>% 
    reframe(n = first(n), 
            acknowN = first(acknowN), 
            acknowPct = first(acknowPct))

  
  
  # Need Progress Monitoring Result ----
  qryStudentProgress <- odbc::dbGetQuery(con,
                                     "
   SELECT
    DISTINCT
     tStudentNeed.studentNeedID
     ,tStudentNeed.FirstName
     ,tStudentNeed.LastName
     ,tStudentNeed.PersonID
   --  ,tInstruction.ImplementedDate
   --  ,tInstruction.InstructionID
   --  ,tInstruction.InstructionDescription
   --  ,tInstruction.InstructionMinutes
  --   ,tInstruction.TimesPerWeek
  --   ,tInstruction.NumberOfTimes
  --   ,tInstructionFrequencyType.InstructionFrequencyTypeName
  --   ,tInstruction.InstructionFrequencyTypeID
  --   ,tInstructionType.InstructionTypeName
  --   ,tImplementerRole.ImplementerRoleName
     --,tGoal.ParentAcknowledgeDate
     ,tGoal.ProgressMonitoring
    -- ,tProgress.ProgressResultNameID
     ,tProgress.ProgressDate
     ,tProgress.LastUpdatedDate AS prgressLastUpdate
     ,tProgressResult.ProgressValue
     ,tProgressResult.ProgressResultShortDescription
     ,tMonitoringFrequencyType.MonitoringFrequencyTypeName
    ,tGoal.GoalStartDate
    -- ,tGoal.CreatedDate
     ,tGoal.GoalEndDate
    -- ,tGoal.LastUpdatedDate
     ,tGoal.GoalID
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
      tGoalFocusArea.GoalID = tGoal.GoalID AND
      tGoalFocusArea.GoalID = tGoalInstruction.GoalID
    JOIN dbSOARS.rti.tProgress(NOLOCK) ON
     tProgress.GoalID = tGoal.GoalID AND
    -- tProgress.LastUpdatedByStaffID = tGoal.LastUpdatedByStaffID AND
     tProgress.GoalID = tGoalInstruction.GoalID
    JOIN dbSOARS.rti.tProgressResult (NOLOCK) ON
     tProgressResult.ProgressResultNameID = tProgress.ProgressResultNameID
    JOIN dbSoArs.rti.tInstruction (NOLOCK) ON
      tInstruction.InstructionID = tGoalInstruction.InstructionID AND
       tInstruction.studentNeedID = tStudentNeed.studentNeedID
    JOIN dbSoars.rti.tFocusAreaDesignator (NOLOCK) ON
      tFocusAreaDesignator.FocusAreaID =  tStudentNeedFocusArea.FocusAreaID
    JOIN dbSoars.rti.tDesignator (NOLOCK) ON
      tDesignator.DesignatorID =  tFocusAreaDesignator.DesignatorID
    JOIN dbSoars.rti.tImplementerRole (NOLOCK) ON
      tImplementerRole.ImplementerRoleID = tInstruction.ImplementerRoleID
    JOIN dbSoars.rti.tInstructionFrequencyType (NOLOCK) ON
     tInstructionFrequencyType.InstructionFrequencyTypeID = tInstruction.InstructionFrequencyTypeID
    JOIN dbSoars.rti.tInstructionType (NOLOCK) ON
      tInstructionType.InstructionTypeID = tInstruction.InstructionTypeID
    JOIN dbSoars.rti.tMonitoringFrequencyType (NOLOCK) ON
      tMonitoringFrequencyType.MonitoringFrequencyTypeID = tGoal.MonitoringFrequencyTypeID
  WHERE
      tNeedType.NeedTypeName = 'READ'
    AND
      tStudentNeed.ActiveFlag = 1
    AND
      tStudentNeedFocusArea.ActiveFlag = 1
    AND
      tGoal.ActiveFlag = 1
    AND
      tNeedType.ActiveFlag = 1
    AND
      tFocusArea.ActiveFlag = 1
    AND
      tGoalInstruction.ActiveFlag = 1
    AND
      tProgress.ActiveFlag = 1
    AND
      tProgressResult.ActiveFlag = 1
    AND
      tInstruction.ActiveFlag = 1
    AND
      tFocusAreaDesignator.ActiveFlag = 1
    AND
      tImplementerRole.ActiveFlag = 1
    AND
      tInstructionFrequencyType.ActiveFlag = 1
    AND
      tInstructionType.ActiveFlag = 1
    AND
      tMonitoringFrequencyType.ActiveFlag = 1
    AND
      tDesignator.ActiveFlag = 1

    AND
      tInstruction.ImplementedDate > '2020-08-01'
    --AND 
   --   tStudentNeed.PersonID = 1453134
    ORDER BY
 --     tInstruction.InstructionID,
  --    tGoal.goalID,
      tProgress.ProgressDate
  "
  )
  
  
  progressMonitoring <- qryStudentProgress %>% 
    clean_names('lower_camel') %>% 
    right_join(grade3Cmas, 
               join_by(personId == personID), 
               relationship = 'many-to-one') %>% 
    mutate(n = n_distinct(personId)) %>% 
    group_by(personId, goalId) %>% 
    mutate(goalN = n()) %>% 
    mutate(progressValue = case_when(
      progressResultShortDescription == 'Goal Achieved' ~ 2, 
      TRUE ~ progressValue)) %>% 
    # mutate(progressDate = ymd(as_date(progressDate))) %>% 
    arrange(personId, goalId, progressDate) %>% 
    # mutate(interval = interval(lag(progressDate), progressDate),
    #        time = time_length(interval, 'days')) %>% 
    # mutate(progressResult = paste0(progressValue, " ", progressResultShortDescription)) %>% 
    group_by(personId, goalId, progressValue, 
             progressResultShortDescription) %>% 
    mutate(progressN = n()) %>% 
    mutate(smartGoal = factor(smartGoal)) %>% 
    mutate(goalStartDate = ymd(as_date(goalStartDate)), 
           goalEndDate = ymd(as_date(goalEndDate))) %>% 
    group_by(personId, goalId) %>% 
    mutate(goalInterval = interval(goalStartDate, goalEndDate), 
           goalTime = time_length(goalInterval, 'days')) %>% 
    group_by(personId) %>% 
    mutate(nOfGoals = n_distinct(goalId))
  
  pmSummary <- progressMonitoring %>% 
    group_by(studentGroup) %>% 
    reframe(goalTime = round(mean(goalTime, na.rm = T), 0), 
             nOfPmGoals = round(mean(nOfGoals, na.rm = T), 1)) %>% 
    mutate(studentGroup = factor(studentGroup, 
                                 levels = c('Exit ME', 
                                            'Exit DNM', 
                                            'Plan ME', 
                                            'Plan DNM'), 
                                 labels = c(c('Exit Read Plan In Grade
                                              3 and CMAS Meet/Exceed', 
                                              'Exit Read Plan In Grade 3 and CMAS Did Not Meet/Exceed', 
                                              'Did not Exit Plan and CMAS Meet/Exceed', 
                                              'Did not Exit Plan and CMAS Did Not Meet/Exceed')))) 

  # Distribution of time
  progressMonitoringPlot <- progressMonitoring %>% 
    distinct(personId, goalId, .keep_all = T)
  
  ggplot(data = progressMonitoringPlot, 
         mapping = aes(x= nOfGoals))+
    geom_histogram(bins = 50)
  
  # Summary PM for P.G.
ggplot(data = progressMonitoring, 
       mapping = aes(x = progressDate, 
                     y = progressValue, 
                     group = smartGoal, 
                     color = factor(progressValue)
                     )) +
  geom_line()+
  geom_point() +
  geom_text(aes(label = progressResultShortDescription), 
            size = 2, 
            vjust = 1)+
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
  scale_color_manual(values = c('#c1571a', '#fbb040', '#6c7070', '#1b8367'))+
  ylim(-2, 2)+
  facet_wrap(~fct_reorder(smartGoal, goalStartDate),
             ncol = 1
             ) +
  theme_minimal()+
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        legend.position = 'none', 
        legend.title = element_blank(), 
        strip.text = element_text(hjust = 0), 
        strip.background = element_rect(fill = 'pink'))



# # isntructionDescription ----
# qryInstructionDescription <- odbc::dbGetQuery(con, 
#                                        "
#  SELECT 
#   DISTINCT
#    tStudentNeed.studentNeedID
#    ,tStudentNeed.FirstName
#    ,tStudentNeed.LastName
#    ,tStudentNeed.PersonID
#    ,tInstruction.ImplementedDate
#    ,tInstruction.InstructionID
#    ,tInstruction.InstructionDescription
# --   ,tInstruction.InstructionMinutes
# --   ,tInstruction.TimesPerWeek
# --   ,tInstruction.NumberOfTimes
# --   ,tInstructionFrequencyType.InstructionFrequencyTypeName
# --   ,tInstruction.InstructionFrequencyTypeID
# --   ,tInstructionType.InstructionTypeName
#  --  ,tImplementerRole.ImplementerRoleName
#    --,tGoal.ParentAcknowledgeDate
#  --  ,tGoal.ProgressMonitoring
#  --  ,tProgress.ProgressResultNameID
#  --  ,tProgress.ProgressDate
#   -- ,tProgress.LastUpdatedDate AS prgressLastUpdate
#   -- ,tProgressResult.ProgressValue
#   -- ,tProgressResult.ProgressResultShortDescription
#   -- ,tMonitoringFrequencyType.MonitoringFrequencyTypeName
# --  ,tGoal.GoalStartDate
# --   ,tGoal.GoalEndDate
# --  ,tGoal.CreatedDate
#   -- ,tGoal.LastUpdatedDate
#   -- ,tGoal.GoalID
#  --  ,tGoal.SmartGoal --must be at end of query
# FROM
#   dbSOARS.rti.tStudentNeed (NOLOCK)
#   JOIN dbSOARS.rti.tStudentNeedFocusArea (NOLOCK) ON
#     tStudentNeedFocusArea.StudentNeedID = tStudentNeed.StudentNeedID
#   JOIN dbSOARS.rti.tGoal (NOLOCK) ON
#     tGoal.StudentNeedID = tStudentNeed.StudentNeedID
#   JOIN dbSOARS.rti.tNeedType (NOLOCK) ON
#    tNeedType.NeedTypeID = tStudentNeed.NeedTypeID
#   JOIN dbSOARS.rti.tFocusArea (NOLOCK) ON
#    tFocusArea.FocusAreaID = tStudentNeedFocusArea.FocusAreaID
#   JOIN dbSOARS.rti.tGoalInstruction (NOLOCK) ON
#     tGoalInstruction.GoalID = tGoal.GoalID
#   JOIN dbSOARS.rti.tGoalFocusArea (NOLOCK) ON
#     tGoalFocusArea.GoalID = tGoal.GoalID AND
#     tGoalFocusArea.GoalID = tGoalInstruction.GoalID
#   JOIN dbSOARS.rti.tProgress(NOLOCK) ON
#    tProgress.GoalID = tGoal.GoalID AND
#   -- tProgress.LastUpdatedByStaffID = tGoal.LastUpdatedByStaffID AND
#    tProgress.GoalID = tGoalInstruction.GoalID 
#   JOIN dbSOARS.rti.tProgressResult (NOLOCK) ON
#    tProgressResult.ProgressResultNameID = tProgress.ProgressResultNameID
#   JOIN dbSoArs.rti.tInstruction (NOLOCK) ON
#     tInstruction.InstructionID = tGoalInstruction.InstructionID AND
#      tInstruction.studentNeedID = tStudentNeed.studentNeedID
#   JOIN dbSoars.rti.tFocusAreaDesignator (NOLOCK) ON
#     tFocusAreaDesignator.FocusAreaID =  tStudentNeedFocusArea.FocusAreaID
#   JOIN dbSoars.rti.tDesignator (NOLOCK) ON
#     tDesignator.DesignatorID =  tFocusAreaDesignator.DesignatorID
#   JOIN dbSoars.rti.tImplementerRole (NOLOCK) ON
#     tImplementerRole.ImplementerRoleID = tInstruction.ImplementerRoleID
#   JOIN dbSoars.rti.tInstructionFrequencyType (NOLOCK) ON
#    tInstructionFrequencyType.InstructionFrequencyTypeID = tInstruction.InstructionFrequencyTypeID
#   JOIN dbSoars.rti.tInstructionType (NOLOCK) ON
#     tInstructionType.InstructionTypeID = tInstruction.InstructionTypeID
#   JOIN dbSoars.rti.tMonitoringFrequencyType (NOLOCK) ON
#     tMonitoringFrequencyType.MonitoringFrequencyTypeID = tGoal.MonitoringFrequencyTypeID
# WHERE
#     tNeedType.NeedTypeName = 'READ'
#  -- AND 
#  --   tStudentNeed.FirstName = 'Philip'
#  -- AND
#  --   tStudentNeed.LastName = 'Grippi'
#   AND 
#     tInstruction.ImplementedDate > '2020-08-01'
#  -- ORDER BY
# --    tInstruction.InstructionID,
# --    tGoal.goalID,
# --    tProgress.ProgressDate
# "
# )
# 