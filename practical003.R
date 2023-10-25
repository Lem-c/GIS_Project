library(sf)
# here dir should only be defined once
library(here)

library(raster)
library(terra)

library(fs)
library(tidyverse)

# U should check how many layers there are by using: 
#   st_layers(here("data", "gadm36_AUS_gpkg/gadm36_AUS.gpkg"))
AusBoundary <- st_read(here("data", "gadm36_AUS_gpkg/gadm36_AUS.gpkg"),
                       layer='gadm36_AUS_0')

# When you do analysis on multiple data sets make sure 
# they are all use the same Coordiante Reference System.
st_crs(AusBoundary)$proj4string

## Set spatial reference system
## Only useful if there is no CRS
AusBoundary <- AusBoundary %>%
  st_set_crs(., 4326)
print(AusBoundary)

# Re_projecting
AusOutReprojected <- AusBoundary %>%
  st_transform(.,3112)
print(AusOutReprojected)

# view raster data
ra_rainFall <- terra::rast(here("data", "wc_5m_tmin/wc2.1_5m_tmin_01.tif"))
plot(ra_rainFall)


# project rain fall data to proj 4 
pr_1 <-  terra::project(ra_rainFall,
                        "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
plot(pr_1)


# project rain fall data to WGS84
pr_2 <- ra_rainFall %>%
  terra::project(., "EPSG:4326")
plot(pr_2)

# 3.5.3 Data loading
dir_info("data/wc_5m_tmin")

# Select the file with ".tif" extension/type
listfiles <- dir_info("data/wc_5m_tmin") %>%
  filter(str_detect(path, ".tif")) %>%
  dplyr::select(path) %>%
  pull()

# Load all of the data straight into a SpatRaster
# Generate a collection of raster layers
worldclimtemp <- listfiles %>%
  terra::rast()

# Access singe layer: worldclimtemp[[1]]

# Rename each layers
# lib(dplyr):rename(), this isn’t yet available for raster data 
names(worldclimtemp) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                          "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# Get one layer
worldclimtemp$Jan

site <- c("Brisbane", "Melbourne", "Perth", "Sydney", "Broome", "Darwin", "Orange", 
              "Bunbury", "Cairns", "Adelaide", "Gold Coast", "Canberra", "Newcastle", 
              "Wollongong", "Logan City" )
lon <- c(153.03, 144.96, 115.86, 151.21, 122.23, 130.84, 149.10, 115.64, 145.77, 
         138.6, 153.43, 149.13, 151.78, 150.89, 153.12)
lat <- c(-27.47, -37.91, -31.95, -33.87, 17.96, -12.46, -33.28, -33.33, -16.92, 
         -34.93, -28, -35.28, -32.93, -34.42, -27.64)
#Put all of this inforamtion into one list
samples <- data.frame(site, lon, lat, row.names="site")
# Extract the data from the Rasterstack for all points
AUcitytemp<- terra::extract(worldclimtemp, samples)

# 3.6: descriptive statistics
Aucitytemp2 <- AUcitytemp %>% 
  as_tibble()%>% 
  add_column(Site = site, .before = "Jan")


# Take perth and plot bar chart
Perthtemp <- Aucitytemp2[3,]
hist(as.numeric(Perthtemp))

#define where you want the breaks in the historgram
userbreak<-c(6, 8, 10, 12, 14, 16, 18, 20)

# remove the ID and site columns
Perthtemp <- Aucitytemp2 %>%
  filter(site=="Perth")
# Remove 'id' and site'
t<-Perthtemp %>%
  dplyr::select(Jan:Dec)
# Detailed plot
hist((as.numeric(t)), 
     breaks=userbreak, 
     col="black", 
     main="Histogram of Perth Temperature", 
     xlab="Temperature", 
     ylab="Frequency")

plot(AusBoundary$geom)

# Simplify
AusoutSIMPLE <- AusBoundary %>%
  st_simplify(., dTolerance = 1000, preserveTopology=TRUE) %>%
  st_geometry()%>%
  plot()

print(AusBoundary)
crs(worldclimtemp)

Austemp <- AusBoundary %>%
  # now crop our temp data to the extent
  terra::crop(worldclimtemp,.)

# plot the output
plot(Austemp)
plot(worldclimtemp)

# Apply maks and plot
exactAus<-terra::mask(Austemp, AusBoundary)
hist(exactAus[[3]], col="red", main ="March temperature")

# ggplot plot
exactAusdf <- exactAus %>%
  as.data.frame()

library(ggplot2)
# set up the basic histogram
gghist <- ggplot(exactAusdf, 
                 aes(x=Mar)) + 
  geom_histogram(color="black", 
                 fill="white")+
  labs(title="Ggplot2 histogram of Australian March temperatures", 
       x="Temperature", 
       y="Frequency")
# add a vertical line to the hisogram showing mean tempearture
gghist + geom_vline(aes(xintercept=mean(Mar, 
                                        na.rm=TRUE)),
                    color="blue", 
                    linetype="dashed", 
                    size=1)+
  theme(plot.title = element_text(hjust = 0.5))