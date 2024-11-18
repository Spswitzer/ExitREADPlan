# # Script header ----
# Title: READ Plan documents
# Author: Susan Switzer
# Created: 10/14/24
# Revised: 11/11/24
# The purpose of this script is to review READ Plan elements for 3rd students (2023-2024) from the kindergarten class of 2020-2021 

#load libraries ----
library(odbc)
library(DBI)
library(janitor)
library(tidyverse)

#extract Exited students from Flag query ----
#file in ExploreReadPlanStatusCleanedUp.R must be ran first to populate flagStart to environment
planEnd <- flagStart %>% 
  filter(planEnd %in% c( 'K- Exit plan','3- Exit plan', '2- Exit plan', '1- Exit plan')) %>% 
  pull(personID)

toString(planEnd)

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

toString(NoPlanEnd)


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

## Explore students with no goal focus area ----
planAndFlag <- data.frame(personID = unique(qrystudentFocusAreas$PersonID)) %>% 
  mutate(plan = 'Y')

planFlag <- flagStart %>% 
  filter(planEnd %in% c('K- Exit plan','3- Exit plan', '2- Exit plan', '1- Exit plan')) %>% 
  select(personID, planEnd)

noPlanYesFlag <- planAndFlag %>% 
 full_join(planFlag, join_by(personID == personID)) %>% 
  filter(is.na(plan))

## Grouped by exit year ----
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

## Grouped by plan year Revised Approach----
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

# Summarize focus areas ----
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

# Need Progress Monitoring Result ----
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
   ,tGoal.ParentAcknowledgeDate
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


## Explore students with no instructional plan ----
instructionAndFlag <- data.frame(personID = unique(qrystudentPlan$PersonID)) %>% 
  mutate(plan = 'Y')

planFlag <- flagStart %>% 
  filter(planEnd %in% c( 'K- Exit plan','3- Exit plan', '2- Exit plan', '1- Exit plan')) %>% 
  select(personID, planEnd)

planFlagAll <- flagStart %>% 
  filter(planEnd %in% c( 'K- Exit plan','3- Exit plan', '2- Exit plan', '1- Exit plan')) %>% 
  select(personID, planStart, planEnd)

noInstYesFlag <- instructionAndFlag %>% 
  full_join(planFlag, join_by(personID == personID)) %>% 
  filter(is.na(plan))
# 52 students with no documented instructional plan

#Goal Volume, Frequency, Intensity, Duration ----
cleanPlans <- qrystudentPlan %>% 
  clean_names('lower_camel') %>% 
  mutate(implementedDate = ymd(as_date(implementedDate)),
         completedDate = ymd(as_date(completedDate))
  )

instructionInteractions <-  cleanPlans %>% 
  distinct(personId, instructionId, 
           .keep_all = T) %>% 
  mutate(instructionN = n_distinct(personId)) %>% 
  group_by(instructionTypeName, instructionFrequencyTypeId) %>% 
  mutate(minutesMean = round(mean(instructionMinutes), 1), 
         freqMean = round(mean(numberOfTimes), 1)) %>% 
  group_by(implementerRoleName, instructionTypeName) %>% 
  mutate(roleN = n()) %>% 
  group_by(instructionTypeName, implementerRoleName, instructionFrequencyTypeName) %>% 
  summarise(minutesMean = first(minutesMean), 
            freqMean = first(freqMean), 
            roleN = first(roleN))

feqIntDur <- cleanPlans %>% 
  distinct(personId, instructionId, 
           .keep_all = T) %>% 
  mutate(instructionN = n_distinct(personId)) %>% 
  group_by(instructionTypeName) %>% 
  mutate(instTypeN = n()) %>% 
  group_by(instructionTypeName, instructionFrequencyTypeName) %>% 
  summarise(instTypeN = first(instTypeN),
            instIntensityN = n(),
          minutesMean = round(mean(instructionMinutes), 1), 
         freqMean = round(mean(numberOfTimes), 1))


# Need Progress Monitoring Result ----
qrystudentProgress <- odbc::dbGetQuery(con, 
                                   "
 SELECT DISTINCT
   tStudentNeed.studentNeedID
   ,tStudentNeed.FirstName
   ,tStudentNeed.LastName
   ,tStudentNeed.PersonID
   ,tInstruction.ImplementedDate
   ,tInstruction.InstructionID
   ,tInstruction.InstructionDescription
   ,tInstruction.InstructionMinutes
   ,tInstruction.TimesPerWeek
   ,tInstruction.NumberOfTimes
   ,tInstructionFrequencyType.InstructionFrequencyTypeName
   ,tInstruction.InstructionFrequencyTypeID
   ,tInstructionType.InstructionTypeName
   ,tImplementerRole.ImplementerRoleName
   ,tGoal.ParentAcknowledgeDate
   ,tProgress.ProgressResultNameID
   ,tProgress.ProgressDate
   ,tProgress.LastUpdatedDate AS prgressLastUpdate
   ,tProgressResult.ProgressValue
   ,tProgressResult.ProgressResultShortDescription
   ,tMonitoringFrequencyType.MonitoringFrequencyTypeName
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
  JOIN dbSoars.rti.tMonitoringFrequencyType (NOLOCK) ON
    tMonitoringFrequencyType.MonitoringFrequencyTypeID = tGoal.MonitoringFrequencyTypeID
WHERE
    tNeedType.NeedTypeName = 'READ'
  AND 
    tStudentNeed.ActiveFlag = 1
  AND
tStudentNeed.PersonID IN (1453134, 1492205, 1492288, 1495409, 1495426, 1517773, 1518327, 1521347, 1522073, 1522625, 1523935, 1526117, 1527399, 1548566, 1554950, 1557323, 1557545, 1558787, 1560641, 1561011, 1561744, 1563560, 1565192, 1566037, 1572832, 1580806, 1581982, 1583060, 1584033, 1587167, 1588190, 1589221, 1589621, 1590608, 1596932, 1599992, 1610465, 1610644, 1611546, 1615543, 1616123, 1619775, 1622387, 1623016, 1623318, 1623643, 1623736, 1625455, 1625660, 1626000, 1626044, 1626642, 1626897, 1628873, 1629266, 1629838, 1630073, 1630077, 1630521, 1630794, 1631475, 1631604, 1632307, 1632517, 1632653, 1634300, 1634322, 1635250, 1635412, 1635503, 1637459, 1637830, 1641268, 1641914, 1642917, 1643060, 1644105, 1644629, 1644647, 1644670, 1645397, 1645712, 1646453, 1646863, 1647125, 1651303, 1652729, 1653977, 1654739, 1655740, 1656982, 1657085, 1657406, 1658165, 1658731, 1661663, 1662029, 1664117, 1665255, 1665361, 1666235, 1666305, 1667736, 1672131, 1672419, 1672680, 1672956, 1673312, 1675167, 1676192, 1676294, 1676418, 1676546, 1678435, 1678964, 1682050, 1682715, 1682928, 1685526, 1686209, 1687566, 1687715, 1687910, 1690600, 1690772, 1690890, 1691704, 1691920, 1692053, 1692474, 1692480, 1692561, 1692762, 1692953, 1693215, 1693230, 1693743, 1694249, 1694301, 1694566, 1694921, 1695545, 1695750, 1696586, 1696780, 1697135, 1698074, 1698623, 1698689, 1698764, 1698909, 1698980, 1699014, 1699292, 1699763, 1700714, 1701627, 1703648, 1705635, 1706041, 1707524, 1708279, 1715098, 1715155, 1715175, 1716487, 1716495, 1721741, 1723678, 1727420, 1728564, 1728570, 1733518, 1734217, 1735370, 1735937, 1736424, 1736682, 1736819, 1736898, 1737029, 1737032, 1737454, 1740349, 1740750, 1741670, 1741997, 1742076, 1743646, 1744669, 1744718, 1745204, 1745838, 1746453, 1747217, 1748408, 1753356, 1753395, 1754366, 1759355, 1759768, 1759985, 1761226, 1761227, 1761739, 1761800, 1761924, 1761968, 1761999, 1762278, 1762642, 1762816, 1762834, 1762867, 1762868, 1762902, 1762908, 1762916, 1763104, 1763107, 1763271, 1763572, 1763643, 1763752, 1763961, 1764114, 1764539, 1764580, 1764610, 1764958, 1765131, 1765183, 1765236, 1765612, 1765624, 1765780, 1765827, 1765933, 2224429, 2225243, 2225351, 2225394, 2225481, 2225610, 2225681, 2225745, 2225776, 2225790, 2225975, 2226138, 2226510, 2226533, 2226723, 2226746, 2226901, 2226916, 2226979, 2227098, 2227108, 2227375, 2227407, 2227476, 2227615, 2227764, 2227782, 2227804, 2227885, 2227997, 2228195, 2228347, 2228444, 2228571, 2229940, 2230040, 2230174, 2230204, 2230216, 2230222, 2230283, 2230475, 2231058, 2231180, 2231223, 2231226, 2231229, 2231530, 2231595, 2232722, 2232769, 2232796, 2234137, 2234872, 2235446, 2235574, 2235614, 2235634, 2235635, 2235674, 2235781, 2235976, 2236084, 2236101, 2236166, 2236409, 2236442, 2236471, 2236480, 2236586, 2237214, 2237720, 2238027, 2238038, 2238168, 2238284, 2238308, 2238684, 2239002, 2239152, 2239435, 2239449, 2239514, 2239604, 2239624, 2239634, 2239929, 2240050, 2240059, 2240062, 2240124, 2240177, 2240583, 2241219, 2241291, 2241682, 2241817, 2241859, 2241904, 2241978, 2242101, 2242208, 2242364, 2242736, 2243219, 2243223, 2243324, 2243611, 2243683, 2243752, 2243788, 2244119, 2244122, 2244148, 2244858, 2244860, 2244862, 2245234, 2245496, 2246137, 2248247, 2248475, 2248841, 2250874, 2256232, 2256241, 2257869, 2261264, 2267177, 2267331, 2269294, 2270072, 2271165, 2274088, 2276275, 2276556, 2276959, 2277056, 2277330, 2277450, 2277464, 2277632, 2278431, 2278934, 2279185, 2279487, 2280657, 2280747, 2280950, 2281070, 2282314, 2285074, 2286327, 2286457, 2286638, 2287001, 2287463, 2287500, 2298439, 2298739, 2299772, 2303033, 2311833, 2319955, 2324065, 2326786, 2329402, 2332320) --Exited students
  ORDER BY
    tProgress.ProgressDate
"
)


library(tidytext)

#Create table from https://www.cde.state.co.us/coloradoliteracy/advisorylistofinstructionalprogramming2020#intervention

interventionWords <- c('orton', 'og', #imse and Yoshimoto are paired with OG
                       'lexia', # core 5 is paired with lexia in all exited instances
                       'burst', 
                       '95', 
                       'sipps', 
                       'i-ready', 'iready', 
                       'istation', 'station','i-station', 
                       'hmh', 'into reading', 
                       'heggerty', 
                       'lli:', 'leveled literacy',
                       'ckla',
                       '^(.*)open court',
                       '^(.*)bridge the gap', 
                       'mindplay',
                       'wonderworks',
                       'blast', '^(.*)really great', #blast is paired with really great
                       'countdown', # countdown is paired with really great
                       'hd', '^(.*)hd word', #hd is paired with high dosage tutoring and really great and hd word
                       'naturally', '^(.*)read natually', '^(.*)read live',
                       'kilptrick',
                       '^(.*)reading corps', 'americorps', 
                       'wilson', 'fundations',
                       'voyager',
                       'ufli',
                       '^(.*)valley speech', 
                       'icali', 'cali', 
                       '^(.*)el education',
                       'wonders',
                       'boost',
                       '^(.*)six minute', '^(.*)six-minute',
                       'spire',
                       'spot on', 
                       'ixl', 
                       'raz',
                       'amplify')

instructionInventions <- qrystudentPlan %>% 
  clean_names('lower_camel') %>% 
  # select(personId, instructionDescription, instructionId) %>%
  # filter(personId == 1453134) %>%
  mutate(
    intervention = instructionDescription %>%
      str_to_lower() %>%
      str_squish() %>% #remove whitespace
      str_extract_all(
        str_c(
          interventionWords
          , collapse = "|")
        )
    ) %>% 
  unnest_longer(intervention, keep_empty = TRUE) %>% 
  # filter(!is.na(intervention)) %>%
  mutate(interventionSimple = case_when(
    str_detect(intervention, 'really great') ~ 'really great', 
    str_detect(intervention, 'i-ready') ~ 'iready',
    str_detect(intervention, 'i-station') | 
      str_detect(intervention, 'station') ~ 'istation',
    str_detect(intervention, 'open court') ~ 'open court',
    str_detect(intervention, 'into reading') ~ 'into reading',
    str_detect(intervention, 'bridge the gap') ~ 'bridge the gap', 
    str_detect(intervention, 'hd word') ~ 'hd', 
    str_detect(intervention, 'naturally') | 
      str_detect(intervention, 'read live') | 
      str_detect(intervention, 'read natually') ~ 'read natually', 
    str_detect(intervention, 'reading corps') ~'reading corps', 
    str_detect(intervention, 'valley speech') ~ 'valley speech', 
    str_detect(intervention, 'cali') | 
      str_detect(intervention, 'icali') ~ 'icali', 
    str_detect(intervention, 'spot on') ~ 'spot on', 
    str_detect(intervention, 'six minute') | 
      str_detect(intervention, 'six-minute') ~ 'six minute',
    str_detect(intervention, 'orton') | 
      str_detect(intervention, 'og') ~ 'og',
    str_detect(intervention, 'lli:') | 
      str_detect(intervention, 'leveled literacy') ~ 'leveled literacy Intervention',
    is.na(intervention) ~ 'no cde approved instruction',
    TRUE ~ intervention
)) %>% 
  distinct(personId, instructionId, instructionTypeName, smartGoal, .keep_all = T) %>% 
  ungroup() %>% 
  select(firstName, lastName, personId, instructionId, intervention, 
            interventionSimple, instructionTypeName)
