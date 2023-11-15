library(sf)
library(here)
library(dplyr)
# plot
library(extrafont)
library(ggplot2)
library(ggspatial)
library(patchwork)
library(scico)
# Updated -> 4.x
library(tmap)
library(tidyverse)
# load data
library(jsonlite)
library(readxl)

# Read in world city shape file and gender inequality data
CountriesShape <- read_sf(here("data/week04", "World_Countries_Generalized/World_Countries_Generalized.shp"))
GenderInequality <- read_excel(here("data/week04", "HDR21-22_Statistical_Annex_HDI_Trends_Table.xlsx"),
                            sheet= 1,
                            na = c("..", " ", "na", "NA", "NULL", "null"))

# Table beautify
col_name = c("HDI_rank", "Country", "gen_1990", "N1","gen_2000", "N2", "gen_2010", "N3", "gen_2015"
             , "N4", "gen_2018", "N5", "gen_2019", "N6", "gen_2020", "N7", "gen_2021")
# Delete top 2 rows !Run once only
GenderInequality <- GenderInequality[-c(1:5),-c(17:ncol(GenderInequality))]
# Change the col name of the table
names(GenderInequality) <- col_name
# Remove Na columns
GenderInequality <- GenderInequality %>%
  select_if(~!all(is.na(.)))


self.colSub <- function(table_, col_1, col_2, one=1, auto_crop=FALSE){
  # table_: The table will be processed
  # col_1: The first column
  # col_2: The second column
  # one: Used contain info column start index
  # auto_crop: whether remove other not used columns
  
  out <- table_
  
  out <- out %>%
    mutate(sub_dif = round(table_[[col_1]] - table_[[col_2]], digits = 3))
  
  
  if(!auto_crop){
    print("No crop")
    return(out)
  }
  
  out <- out %>%
    select(., -all_of(c(one:ncol(out)-1)))
    
  return(out)
}

attr(self.colSub, "comment") <- "Apply subtraction to two specific columns and remove not used columns"

# Make a difference between two target columns
new <- self.colSub(GenderInequality, 9, 6, one = 4, auto_crop = TRUE) %>%
  as.data.frame(new)

# Join the difference value
# Do not always join by the str column, error may happen
combined_out <- left_join(CountriesShape, new, by = c("COUNTRY" = "Country"))
# filter(str_detect(A, B)

tm_shape(combined_out) +
  tm_polygons("sub_dif", palette = "-Blues", border.col = "gray30", title = "Value") +
  tm_layout(frame = FALSE, legend.title.size = 1)
