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

grade3PlanEndStatus <- flagGrade3 %>% 
  ungroup() %>% 
  left_join(cmasPerformance, join_by(personID == personId)) %>% 
  filter(!is.na(cmasProfLevel)) %>% 
  distinct(personID, cmasProfLevel, .keep_all = T) %>% 
  mutate(totalN = n()) %>% 
  group_by(planEnd) %>% 
  mutate(planN = n()) %>% 
  group_by(cmasProfLevel, planEnd) %>% 
  filter(cmasProfLevel %in% c('Met Expectations', 'Exceeded Expectations')) %>% 
  ungroup() %>% 
  distinct(personId = personID, planEnd) %>% 
  mutate(planEnd = ifelse(is.na(planEnd), 'on plan', planEnd))

planFocusAreas <-  qrystudentFocusAreas %>% 
  clean_names('lower_camel') %>% 
  right_join(grade3PlanEndStatus) %>% 
  select(studentNeedId, firstName, lastName, personId, 
         focusAreaName, focusAreaId, focusStartDate = startDate, planEnd) %>% 
  mutate(focusStartDate =  as.Date(focusStartDate, "%d/%m/%Y"),
         N = n_distinct(personId)) %>% 
  mutate(goalYear = year(focusStartDate)) %>% 
  arrange(personId, desc(focusStartDate)) %>% 
  group_by(personId, planEnd) %>% 
  reframe(focusStartDate = max(focusStartDate), 
          focusAreaName = first(focusAreaName), 
          focusAreaId = first(focusAreaId), 
          N = first(N)) %>%
  arrange(personId, focusAreaId, focusStartDate) %>% 
  group_by(personId, focusAreaId, focusAreaName, N, planEnd) %>% 
  summarise(focusAreaName = first(focusAreaName)) %>% 
  group_by(personId,  focusAreaName, N) %>% 
  group_by(planEnd) %>% 
  mutate(planN = n()) %>% 
  group_by(focusAreaName, N, planEnd, planN) %>% 
  reframe(focusAreaCount = n()) %>% 
  mutate(focusAreaCountPct = round(focusAreaCount/planN, 2)) %>% 
  arrange(focusAreaName, planEnd)

