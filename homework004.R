library(sf)
library(here)
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
GenderInequality <- read_excel(here("data/week04", "HDR21-22_Statistical_Annex_GII_Table.xlsx"),
                            sheet= 1,
                            na = c("..", " ", "na", "NA", "NULL", "null"))

# Table beautify
# Delete top 2 rows !Run once only
GenderInequality <- GenderInequality[-c(1:7),-c(6:ncol(GenderInequality))]
# Change the col name of the table
names(GenderInequality) <- c("id", "country", "value_2021", " ", "rank_2021")
GenderInequality <- select(GenderInequality, -4)
GenderInequality <- GenderInequality %>%
  drop_na(value_2021)

as.numeric(GenderInequality$value_2021)


