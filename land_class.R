library(exactextractr)
library(dplyr); library(magrittr); library(tibble)
library(sf); library(ggplot2); library(tidyr); library(rstudioapi)
library(mgcv); library(gratia); library(lme4)
library(raster); library(terra) 

# ------------------------------------------Mean dissimilarity--------------------------------------------------
# read in dissimilarity raster
#ras = raster::raster(paste(predictors_location, "~/Desktop/DATABASES/data_raw/Copernicus/diss_1k.tif", sep=""))
#names(ras) = "evi_dissimilarity"

ras = raster::raster("~/Desktop/DATABASES/data_raw/Copernicus/diss_1k.tif")
names(ras) = "evi_dissimilarity"

# processing
ras = ras * 0.0001 # apply scaling factor to get actual values
#values(ras)[ values(ras) > 200000 ] = NA # set missing vals to NA

locs <- unique_sites
locs = sf::st_as_sf(x = locs, 
                    coords = c("Longitude", "Latitude"),
                    crs = 4326)
locs_buf = sf::st_buffer(locs, dist=5000)

# extract mean value for each buffer
# n.b. "sf_data" is an sf object of circular buffers around points (you will need to create this for your sites)
ext = exactextractr::exact_extract(ras, locs_buf, fun='mean')
ext = data.frame(x = ext); names(ext)[1] = names(ras)


#------------------------------------COP categorical classification---------------------------------------------
# first reading in the chart I exported called "cop_6"
cop_6 <- read_csv("~/Desktop/DATABASES/data_modified/cop_6.csv")
# next adding a column that states the classification of each point
cop_6 <- cop_6 %>%
  mutate(classification = case_when(
    median >= 19 & median <= 21 ~ "shrubs",
    median >= 29 & median <= 31 ~ "herbaceous vegetation",
    median >= 39 & median <= 41 ~ "agriculture",
    median >= 49 & median <= 51 ~ "urban",
    median >= 59 & median <= 61 ~ "bare/sparse vegetation",
    median >= 69 & median <= 71 ~ "snow and ice",
    median >= 79 & median <= 81 ~ "permanent water bodies",
    median >= 89 & median <= 91 ~ "herbaceous wetland",
    median >= 99 & median <= 101 ~ "moss and lichen",
    median >= 109 & median <= 117 ~ "closed forest",
    median >= 119 & median <= 127 ~ "open forest",
    median >= 199 & median <= 201 ~ "ocean",
    TRUE ~ "other"  # Default classification if no condition is met
  ))

# Yay it worked!

# Copernicus satellit information
ras = raster::raster("~/Desktop/DATABASES/data_raw/Copernicus/PROBAV_LC100_global_v3.0.1_2015-base_Tree-CoverFraction-layer_EPSG-4326.tif")
names(ras) = "forest_cover"

# sitessssss
locs <- unique_sites
locs = sf::st_as_sf(x = locs, 
                    coords = c("Longitude", "Latitude"),
                    crs = 4326)
locs_buf = sf::st_buffer(locs, dist=5000)

# extract then apply function to calculate proportion cover in buffer
# n.b. "sf_data" is an sf object of circular buffers created around points; you'll need to create this
ext = exactextractr::exact_extract(ras, locs_buf)
cfunc = function(x, ...){
  x = x %>% dplyr::filter(value != 255) # remove missing values
  return( sum((x$value/100) * x$coverage_fraction, na.rm=TRUE) / sum(x$coverage_fraction, na.rm=TRUE))
}
ext = sapply(ext, cfunc)
ext = data.frame(x = ext); names(ext)[1] = names(ras)