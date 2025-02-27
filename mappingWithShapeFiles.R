# Mapping with updated shape files ----
library(leaflet)
library(leaflet.extras)
library(maps)
library(leaflegend)
library(sf)
library(tidyverse)
library(sp)
library(raster)

#Load shapefile
shp <- shapefile("data/gis/shapeFiles/Facilities.shp")

shapes <- read_sf("data/gis/shapeFiles/Facilities.shp") %>% 
  filter(Use_ == "Elementary School")

geoJason <- read_sf('data/gis/shapeFiles/Jeffco_Public_Schools_Locator_Features.geojson') %>% 
  filter(!is.na(Enroll_2021_22))

elem <- shp[shp@data$Use_ == "Elementary School", ]

colorado_counties <- maps::map(database = "county", 
                               regions = 'colorado,jefferson', 
                               fill = TRUE, 
                               plot = F) 


bins <- c(100, 200, 
          300, 400, 
          500, 600, 
          700, 800, 
           1000)
pal <- colorBin("YlOrRd", 
                domain = geoJason$Enroll_2021_22, 
                bins = bins)

leaflet(geoJason)  %>% 
  addTiles() %>% 
  setView(lng = -105.1, 
          lat = 39.6,
          zoom = 9) %>% 
  addPolygons(data = geoJason, 
              label = paste0(geoJason$ES_Name, ' ', geoJason$Enroll_2021_22),
              labelOptions = labelOptions(noHide = F),
              fillColor = ~pal(geoJason$Enroll_2021_22), 
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7)

