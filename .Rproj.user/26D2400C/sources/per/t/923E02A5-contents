library(sf)
# here dir should only be defined once
library(here)

library(raster)
library(terra)

library(fs)
library(tidyverse)
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

# Func that takes a table, do subtraction to generate new columns
self.colDecrease <- function(Table_, newColNames, Gap_ = 12, InitIndex_ = 1){
  # Total number of columns
  col_num = ncol(Table_)
  # output table
  outTable = Table_
  # col index
  index = 1
  
  # Calculate subtraction
  for(i in InitIndex_:Gap_+1){
    outTable <- outTable %>%
      mutate(newColNames[index] <- round(Table_[,i] - Table_[,i+Gap_], digits = 3))

    index <- index + 1
  } 
  index <- index+1 
  # Remove uselss cols
  useless_cols <- c(index:col_num)
  outTable <- select(outTable, -all_of(useless_cols))
  
  return(outTable)
}

################################################# Code #########################

# Check geo_pkg layers and read in files
st_layers(here("data/week03", "gadm41_CAN.gpkg"))
# Get boundary
CanadaBoundary <- st_read(here("data/week03", "gadm41_CAN.gpkg"),
                       layer='ADM_ADM_0')
# Get geo json boundary
CanadaGeo_shp <- read_sf(here("data/week03", "gadm41_CAN_shp/gadm41_CAN_1.shp")) %>%
  st_cast(., "MULTILINESTRING")
# read in Washington geo map
WorldCities <- read_sf(here("data/week03", "World_Cities/World_Cities.shp"))


# Get Canada key cities
CandaCities <- WorldCities %>%
  select(FID, CITY_NAME, CNTRY_NAME, geometry) %>%
  filter(CNTRY_NAME == "Canada")

# !TODO Convert CRS
st_crs(CanadaBoundary)$proj4string

# Set spatial reference system. Only useful if there is no CRS
CanadaBoundary <- CanadaBoundary %>%
  st_set_crs(., 4326)

# print(CanadaBoundary)

# Load tif files ssp1 and ssp5
tm_126 <- terra::rast(here("data/week03", "Max_Temp_2081_to_2100/wc2.1_2.5m_tmax_ACCESS-CM2_ssp126_2081-2100.tif")) %>%
  # project rast max temperature data to WGS84 !__Time consuming__
  terra::project(., "EPSG:4326")
tm_585 <- terra::rast(here("data/week03", "Max_Temp_2081_to_2100/wc2.1_2.5m_tmax_ACCESS-CM2_ssp585_2081-2100.tif")) %>%
  terra::project(., "EPSG:4326")

# Rename index name in each layer
StrMonth <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
names(tm_126) <- StrMonth
names(tm_585) <- StrMonth

# Extract data grouped by points
# Can also be done by terra::crop()
ext_126 <- terra::extract(tm_126, CandaCities)
ext_585 <- terra::extract(tm_585, CandaCities)

# view of ssp data
ext_126
ext_585
# Change .tif file to view between ssp1 and ssp5
CanadaView <- CanadaBoundary %>%
  terra::crop(tm_126, .)
plot(CanadaView)


# Add city name as new col: as_tibble
#pivot_longer(
#  cols = 2:13,
#  names_to = "Month",
#  values_to = "Temperture"
#  )
#
table_126 <- ext_126 %>% 
  as_tibble()%>% 
  add_column(Site = CandaCities[[2]], .before = "Jan")
table_585 <- ext_585 %>% 
  as_tibble()%>% 
  add_column(Site = CandaCities[[2]], .before = "Jan")

# Combine two tables
CanadaTempTable <- inner_join(table_126,table_585, by="Site") %>%
  select(., -c(ID.x, ID.y))

# Get sub and remove useless data
CanadaTempDif <- self.colDecrease(CanadaTempTable, StrMonth)
names(CanadaTempDif) <- c("Site", "Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                          "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# Make each city has multi-observations of temperature
CandaCitiesTemp_long <- CanadaTempDif %>%
  pivot_longer(cols = -Site, names_to = "Month", values_to = "Temperature")


############################################# Plot #############################

# set up the basic histogram
# Create the plot
gghist <- ggplot(CandaCitiesTemp_long, aes(x = Month, y = Temperature, color = Site, group = Site)) +
  geom_line(linewidth = 1.5) +
  labs(title = "Monthly Temperature Trends by City in Canada",
       x = "Month",
       y = "Temperature (°C)") +
  theme_minimal()
gghist
# hist plot
gghist_2 <- ggplot(CanadaTempDif, aes(x = Jan)) +
  geom_histogram()
gghist_2

# TODO: Map view
# Extract cities' cordinate (lon, lat) 
CandaCities_Copy <- CandaCities %>%
  janitor::clean_names() %>%
  # extract(., geometry, into = c('x_', 'y_'), '\\((.*),(.*)\\)', conv = T) %>%
  left_join(., CanadaTempDif, by=c("city_name"="Site"))
# Plot map
ggMap <- ggplot(data = CanadaGeo_shp) +
  geom_sf(color = "#FF6B58", alpha = 1,  size = 0.5)
ggMap
