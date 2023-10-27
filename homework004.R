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


                            