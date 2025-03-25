

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
                       AND 
                        vPARCCStudentList.EndYear = 2024
                       AND
                        vPARCCStudentList.Grade = '3'
                        ")

## Clean up CMAS query ----
cmasPerformance <- qryCmas %>% 
  clean_names('lower_camel') %>% 
  filter(contentGroupName == 'READING', 
         testName != 'CMAS CSLA Grade 03 2023-24', <- 
         grade == '3', 
         endYear == 2024, 
         !is.na(proficiencyLongDescription)) %>% 
  select(personId, grade, cmasProfLevel = proficiencyLongDescription, testName, cdeSchoolNumber, school)

flagStart <- readRDS('data/flagStart.rds')

flagGrade3 <- flagStart %>% 
  group_by(personID) %>% 
  fill(planStart, .direction = 'downup') %>% 
  fill(planEnd, .direction = 'downup') %>% 
  filter(gradeInt == 3,
         EndYear == 2024
  ) %>%
  distinct(personID, planEnd) 



cmasSchools <- flagGrade3 %>% 
  ungroup() %>% 
  left_join(cmasPerformance, join_by(personID == personId)) %>% 
  filter(!is.na(cmasProfLevel)) %>%
  distinct(personID, cmasProfLevel, .keep_all = T) %>%
  mutate(cmasProfLevel = str_remove(cmasProfLevel, ' Expectations')) %>% 
  mutate(cmasProfLevel = factor(cmasProfLevel, 
                                levels= c('Exceeded', 
                                          'Met', 
                                          'Approached', 
                                          'Partially Met', 
                                          'Did Not Yet Meet'))) %>% 
  mutate(pl = case_when(
    cmasProfLevel %in% c('Exceeded', 'Met') ~ 1, 
    TRUE ~ 0
  )) %>% 
  mutate(planEnd = case_when(
    is.na(planEnd) ~ 0,
    planEnd == '3- Exit plan' ~ 1, 
    TRUE ~ 3
  )) %>%
  filter(planEnd != 3) %>% 
  group_by(cdeSchoolNumber) %>% 
  mutate(total = n()) %>% 
  group_by(cdeSchoolNumber, pl) %>% 
  filter(pl > 0 & planEnd >0) %>% 
  mutate(plN = n()) %>% 
  # filter(cdeSchoolNumber =='3088') %>%
  group_by(cdeSchoolNumber, planEnd) %>%
  mutate(totalPlan = n(), 
         totalPct = plN/total) %>% 

  mutate(studentGroup = case_when(
    planEnd == 0 & pl == 0 ~ 'Plan DNM', 
    planEnd == 0 & pl == 1 ~ 'Plan ME', 
    planEnd == 1 & pl == 0 ~ 'Exit DNM', 
    planEnd == 1 & pl == 1 ~ 'Exit ME', 
    TRUE ~ 'Error'
  )) %>% 
  ungroup() %>% 
  filter(cmasProfLevel %in% c('Exceeded', 'Met'))

JeffcoLatLng <- read.csv(file = 'G:/Shared drives/Research & Assessment Design (RAD)/L1 Projects/School Insights & CDE Local Accountability Grant/School Insights 2.0.01/InsightsDataProcessing/noUploadToShinyIO/basicsTab/mapping/JeffcoSchoolAdresses_longlat_link.csv') %>% 
  mutate(cdeSchoolNumber = str_pad(SchoolID, side = 'left', pad = '0', width = 4)) %>% 
  select(cdeSchoolNumber, longitude, latitude)


library(leaflet)
library(leaflet.extras)
library(maps)
library(leaflegend)


colorado_counties <- maps::map(database = "county", 
                               regions = 'colorado,jefferson', 
                               fill = TRUE, 
                               plot = F) 

schoolLocations <- JeffcoLatLng %>% 
  full_join(cmasSchools, join_by(cdeSchoolNumber)) %>% 
  group_by(cdeSchoolNumber) %>% 
  filter(str_detect(studentGroup, 'Exit')) %>%
  mutate(n = n()) %>% 
  group_by(cdeSchoolNumber, school, n, longitude, latitude, studentGroup) %>% 
  summarise( total = first(total), 
             totalPlan = first(totalPlan), 
             totalPct = first(round(totalPct, 2)*100)) %>% 
  mutate(label = paste0(school, ' (', paste0(totalPlan), ')')) %>% 
  arrange(school) %>% 
  ungroup() %>% 
  mutate(school = str_remove(school, 'Elementary'))


library(gt)
gt(schoolLocations) %>% 
  cols_label(school = "School", 
             totalPlan = 'Total Students') %>% 
  cols_hide(columns = c(cdeSchoolNumber, n, longitude, latitude, studentGroup, label, total, totalPct)) %>% 
  tab_header(title = 'Students Who Exited a READ Plan in Grade 3 and Met/Exceeded CMAS') 
# %>% 
#   grand_summary_rows(columns = totalPlan, 
#                      fns = Total ~ sum(.))

symbols <- makeSymbolsSize(
  values = schoolLocations$totalPlan,
  shape = 'circle',
  color = '#1b8367',
  fillColor = '#1b8367',
  opacity = .75,
  baseSize = 10
)

library(htmlwidgets)
library(htmltools)
tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 14px;
  }
"))

title <- tags$div(
  tag.map.title, HTML("Students Who Exited a READ Plan in Grade 3 and Met/Exceeded CMAS")
)  

leaflet(data = colorado_counties) %>%
  setView(lng = -105.3, lat = 39.7, 
          zoom = 9.75) %>% 
  addProviderTiles(providers$CartoDB.VoyagerOnlyLabels) %>% 
  leaflet::addPolygons(
    color = "#444444",
    weight = 2,
    smoothFactor = 2,
    fillColor = '#fbb040',
    stroke = T,
    fillOpacity = .20,      
    highlightOptions = highlightOptions(
      stroke = T,
      color = "white",
      weight = 5,
      bringToFront = F)
  ) %>% 
  addMarkers(data = schoolLocations,
             icon = symbols,
             lat = ~schoolLocations$latitude, 
             lng = ~schoolLocations$longitude,  
             label = ~schoolLocations$label) %>%
  addLegendSize(
    values = schoolLocations$totalPlan,
    color = '#1b8367',
    fillColor = '#1b8367',
    opacity = 1,
    fillOpacity = 1,
    position = 'topright',
    baseSize = 7, 
    title = 'Students Who Exited a READ Plan in Grade 3 and Met/Exceeded CMAS',
    shape = 'circle',
    orientation = 'horizontal',
    stacked = F,
    breaks = unique(schoolLocations$totalPlan)) 
# %>% 
#   addControl(title,
#              position = "topleft",
#              className = "map-title")
#   %>% 
  # addLegendBin(
  #   pal = binPal,
  #   position = 'topright',
  #   values = ~schoolLocations$totalPct,
  #   title = 'Proportion of Students',
  #   labelStyle = 'font-size: 12px; 
  #   font-weight: bold;',
  #   orientation = 'vertical'
  # ) 
# %>% 
# 
# addCircleMarkers(lat = ~schoolLocations$latitude,
#                  lng = ~schoolLocations$longitude,
#                  radius = ~schoolLocations$n,
#                  label = ~schoolLocations$label)
  