# All libs
library(maptools)
library(RColorBrewer)
library(classInt)
library(sp)
library(rgeos)
library(tmap)
library(tmaptools)
library(sf)
library(rgdal)
library(geojsonio)
library(RSQLite)
library(tidyverse)

# Func that allows to auto draw a map
self.autoDraw <- function(World_, FillCol_, Title_ = "NaN"){
  tm_shape(World_) +
    tm_borders() +
    tm_fill(col = FillCol_, title = Title_) +
    tm_layout(legend.position = c("right", "bottom"))
}

# read in Washington geo map
WashingtonMap <- st_read("E:/UCL/Term_01/GIS/Workshop/R_Proj/data/Washington_Area/Washington_Counties_with_Natural_Shoreline___washsh_area.shp")

# read in related csv dataset
AssessData <- read_csv("E:/UCL/Term_01/GIS/Workshop/R_Proj/data/Assessment1819.csv",
                       na = c("n/a", "na", "NA", "NULL"))

# filter by area prefix
# qtm(WashingtonMap)

# quick browse data type of each col
Datatypelist <- AssessData %>% 
  summarise_all(class) %>%
  pivot_longer(everything(), 
               names_to="All_variables", 
               values_to="Variable_class")

# Data process
# Get Science Students
AssessDataOfScience <- AssessData %>%
  select(OrganizationLevel, County, TestSubject,
         `Count of Students Expected to Test`,
         CountMetStandard,
         GradeLevel) %>%
  filter(County != "Multiple") %>%
  filter(TestSubject == "Science") %>%
  filter(OrganizationLevel == "School") %>%
  filter(GradeLevel == "All Grades")

# Add students' grades
GradePer <- AssessDataOfScience %>%
  group_by(County) %>%
  summarise(
    Total = sum(`Count of Students Expected to Test`, na.rm = TRUE),
    TotalPassed = sum(CountMetStandard, na.rm=TRUE)
  )

# Calculate whole state pass average
sumOut <- GradePer %>%
  select(Total, TotalPassed) %>%
  colSums(.)
TotalPassRate = sumOut[2] / sumOut[1] * 100 %>%
  as.numeric(.)

# Calculate percentage
GradePer <- GradePer %>% 
  mutate(PassRate = TotalPassed/Total * 100) %>% 
  mutate(IsAbove = PassRate > TotalPassRate) %>%
  mutate(IsAbove_ = ifelse(IsAbove, 10000, 1))

# rename and to upper
colnames(GradePer)[1] <- "COUNTY"
GradePer$COUNTY <- toupper(GradePer$COUNTY)

# left join
output <- WashingtonMap  %>%
  left_join(., GradePer,
          by="COUNTY")

# Draw
self.autoDraw(output, "IsAbove_", "IsAbove")
