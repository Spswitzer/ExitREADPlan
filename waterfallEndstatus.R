library(waterfalls)
library(ggplot2)

## Summary table ----
flagSummaryWaterfall <- flagStart %>% 
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
  mutate(startEndN = -startEndN) %>% 
  mutate(planEnd = replace_na(planEnd, 'No End')) %>% 
  group_by(planStart, planStartN) %>% 
  arrange(planEnd)

startValuesJoiner <- flagSummaryWaterfall %>% 
  mutate(planStart = paste0('Plan Start: ', planStart), 
         planEnd = paste0('Plan End: ', planEnd)) %>% 
  mutate(planEnd = case_when(
    planEnd == 'Plan End: K' ~ paste0(planStart, ' ', planEnd), 
    planEnd == 'Plan End: 1' ~ paste0(planStart, ' ', planEnd), 
    planEnd == 'Plan End: 2' ~ paste0(planStart, ' ', planEnd), 
    planEnd == 'Plan End: 3' ~ paste0(planStart, ' ', planEnd), 
    TRUE ~ paste0(planStart,  ' ', planEnd)
  )) %>% 
  ungroup() %>% 
  distinct(planEnd, planStartN) %>% 
  select(planEnd, value = planStartN) %>% 
  filter(planEnd %in% c('Plan Start: K Plan End: No End', 
                        'Plan Start: 1 Plan End: No End', 
                        'Plan Start: 2 Plan End: No End', 
                        'Plan Start: 3 Plan End: No End')) %>% 
  mutate(planEnd = str_remove(planEnd, ' Plan End: No End'))
  

flagSummaryWaterfallStart <- flagSummaryWaterfall %>% 
  mutate(planStart = paste0('Plan Start: ', planStart), 
         planEnd = paste0('Plan End: ', planEnd)) %>% 
  mutate(planEnd = case_when(
    planEnd == 'Plan End: K' ~ paste0(planStart, ' ', planEnd), 
    planEnd == 'Plan End: 1' ~ paste0(planStart, ' ', planEnd), 
    planEnd == 'Plan End: 2' ~ paste0(planStart, ' ', planEnd), 
    planEnd == 'Plan End: 3' ~ paste0(planStart, ' ', planEnd), 
    TRUE ~ paste0(planStart,  ' ', planEnd)
  )) %>% 
  pivot_longer(cols = c(planStart, planEnd), 
               names_to = 'planStatus', 
               values_to = 'planStatusValue') %>% 
  filter(planStatus == 'planEnd') %>% 
  distinct(planStatusValue, .keep_all = T) %>% 
  select(planStatusValue, startEndN, planStartN) %>% 
  pivot_longer(cols = c(startEndN)) %>% 
  ungroup() %>% 
  select(planStatusValue, value) %>% 
  distinct(planStatusValue, value) %>% 
  mutate(value = case_when(
    planStatusValue %in% c("Plan Start: K Plan End: No End", 
                           "Plan Start: 1 Plan End: No End", 
                           "Plan Start: 2 Plan End: No End", 
                           "Plan Start: 3 Plan End: No End"
    ) ~ abs(value), 
    TRUE ~ value
  )) %>% 
  full_join(startValuesJoiner, 
            join_by(planStatusValue == planEnd, value)) %>% 
  mutate(planStatusValue = factor(planStatusValue,
                          levels = c(
                            "Plan Start: K",
                            "Plan Start: K Plan End: K", 
                            "Plan Start: K Plan End: 1", 
                            "Plan Start: K Plan End: 2", 
                            "Plan Start: K Plan End: 3",  
                            "Plan Start: K Plan End: No End",
                            "Plan Start: 1",
                            "Plan Start: 1 Plan End: 1", 
                            "Plan Start: 1 Plan End: 2", 
                            "Plan Start: 1 Plan End: 3" , 
                            "Plan Start: 1 Plan End: No End", 
                            "Plan Start: 2",
                            "Plan Start: 2 Plan End: 2", 
                            "Plan Start: 2 Plan End: 3", 
                            "Plan Start: 2 Plan End: No End", 
                            "Plan Start: 3",
                            "Plan Start: 3 Plan End: 3",
                            "Plan Start: 3 Plan End: No End"),
                          labels = c(
                            "Plan Start: K",
                            "K\nEnd: K", 
                            "K\nEnd: 1", 
                            "K\nEnd: 2", 
                            "K\nEnd: 3",  
                            "K\nNo End",
                            "Plan Start: 1",
                            "1\nEnd: 1", 
                            "1\nEnd: 2", 
                            "1\nEnd: 3" , 
                            "1\nNo End", 
                            "Plan Start: 2",
                            "2\nEnd: 2", 
                            "2\nEnd: 3", 
                            "2\nNo End", 
                            "Plan Start: 3",
                            "3\nEnd: 3",
                            "3\nNo End"),
                          ordered = T)) %>%
  arrange(planStatusValue) 

waterfall(flagSummaryWaterfallStart, 
          calc_total = F) +
  labs(title = 'Summary of Read Plan Status for Cohort', 
       x = '', 
       y = '') +
  theme_classic()+
  theme(axis.text.y = element_blank(), 
        axis.ticks = element_blank())
