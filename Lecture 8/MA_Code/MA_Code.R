# Code for Calculating Market Access for Spatial Class, Fall 2021
# Date Created: 5-5-19
# Latest Edit: 10-26-21
# Author: Noel Johnson
# Based on code used in "Negative Shocks and Mass Persecutions: 
# Evidence from the Black Death", published in the Journal of 
# Economic Growth

library(raster)
library(tidyverse)
library(sf)
library(haven)

setwd("/Users/noeljohnson_laptop/Dropbox/Teaching/Spatial Fall2021/Lecture 8/MA_Code")

# Bring in the modern country borders (you will use it to crop the other data
# sets for class)
modern_countries <- st_read("/Users/noeljohnson_laptop/Dropbox/Teaching/Spatial Fall2021/Lecture 8/MA_Code/Modern Europe/Modern Europe Projected.shp") %>%
  st_transform(EEC)
glimpse(modern_countries)
modern_countries <- modern_countries %>% dplyr::select(CntryName)
france <- modern_countries %>% filter(CntryName == "France")
plot(france[2], col="NA", reset=F)

cities_raw <- read_dta("All_Cities.dta")
# Spatialize the data
EEC <- "+proj=eqdc +lat_0=0 +lon_0=0 +lat_1=43 +lat_2=62 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs"
cities <- st_as_sf(cities_raw, coords=c("longitude", "latitude"), crs=4326) %>%
  st_transform(EEC)
cities <- st_crop(cities, france)

# Bring in roman roads shape file
rom_roads <- st_read("Europe_Major_Roman_Roads/MajorRomRdsProj.shp") %>%
  st_transform(EEC)
glimpse(rom_roads)
rom_roads <- rom_roads %>% dplyr::select()
rom_roads <- st_crop(rom_roads, france)
#plot(rom_roads[1], col="red", reset=F)

# Bring in medieval roads shape file
med_roads <- st_read("Europe_Medieval_Trade_Routes/MedRdsProj.shp") %>%
  st_transform(EEC)
glimpse(med_roads)
med_roads <- med_roads %>% dplyr::select()
med_roads <- st_crop(med_roads, france)
#plot(med_roads[1], col="red", reset=F)

# Bring in rivers shape file
rivers <- st_read("Rivers/Rivers_clippedF.shp") %>%
  st_transform(EEC)
glimpse(rivers)
rivers <- rivers %>% dplyr::select()
rivers <- st_crop(rivers, france)
#plot(rivers[1], col="blue", reset=F)

# Bring in seas shape file
seas <- st_read("Seas/seas_conic.shp") %>%
  st_transform(EEC)
glimpse(seas)
seas <- seas %>% dplyr::select()
seas <- st_crop(seas, france)
# plot(seas[1], col="blue", reset=F)

# Visualize the data
plot(rom_roads[1], col="red", reset=F)
plot(med_roads[1], col="red", add=T)
plot(rivers[1], col="blue", add=T)
plot(seas[1], col="blue", add=T)
plot(cities[4], add=T)
# 

# Make a bounding box of the cities so you can create one a little bigger
bbox_cities <- st_bbox(cities, crs=EEC)
bbox_cities

#bbox <- st_bbox(c(xmin = -808309.6, xmax = 5825335.1, ymax = 7073868.6, ymin = 3042103.4), crs = EEC)

xrange <- bbox_cities$xmax - bbox_cities$xmin # range of x values
yrange <- bbox_cities$ymax - bbox_cities$ymin # range of y values

bbox_cities[1] <- bbox_cities[1] - (0.025 * xrange) # xmin - left
bbox_cities[3] <- bbox_cities[3] + (0.025 * xrange) # xmax - right
bbox_cities[2] <- bbox_cities[2] - (0.025 * yrange) # ymin - bottom
bbox_cities[4] <- bbox_cities[4] + (0.025 * yrange) # ymax - top

bbox_cities_sf <- bbox_cities %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

bbox_cities_sf

# make grid of 25km cells
grid_sf <- bbox_cities_sf %>% st_make_grid(cellsize = 25000,
                                    what="polygons")
# add IDs to grid, make sf
grid_sf <- st_sf(id = 1:length(grid_sf),
                 geometry = grid_sf)

st_geometry(grid_sf)

plot(grid_sf[1], col=NA, border=3, lwd=1, reset=F)
plot(cities[4], add=T)

# Create travel costs for the travel technologies (roman roads, medieval routes, rivers, seas, portage)

# find the travel cost data. The sources are:
# 1. Bairoch (early 20th c.): porters = 1; roads = 0.81; rivers = 0.21; seas = 0.08; nature of estimate = cost
# for each travel tech shape file add a value column...
# fill that column with the travel cost...

# rom_roads
rom_roads$rom_rd_cost_1 = 0.81

# med_roads
med_roads$med_rd_cost_1 = 0.81

# rivers
rivers$river_cost_1 = 0.21

# seas
seas$seas_cost_1 = 0.08

# add a portage value to the grid file...
grid_sf$portage_cost_1 = 1.00

# Sequentially st_join the data sets into the cities file...
grid_sf <- grid_sf %>%
  st_join(rom_roads) %>%
  st_join(med_roads) %>%
  st_join(rivers) %>%
  st_join(seas)

# grid_cost_1
grid_cost_1 <- grid_sf %>%
  dplyr::select(id, portage_cost_1, rom_rd_cost_1, med_rd_cost_1, river_cost_1, seas_cost_1) %>%
  group_by(id) %>%
  summarize(least_cost_1 = min(portage_cost_1, rom_rd_cost_1, med_rd_cost_1, river_cost_1, seas_cost_1, na.rm = T))

# Make 25k raster for cost_1
## Set up a raster "template" for a 10 km grid
bbox_cities
# xmin      ymin      xmax      ymax 
# -358527.9 4606347.5  817048.9 5690653.5 
# order for extent command is: xmin, xmax, ymin, ymax
ext <- extent(-358527.9, 817048.9, 4606347.5, 5690653.5)
gridsize <- 25000
r <- raster(ext, res=gridsize)
## Rasterize the shapefile
rr_cost_1 <- rasterize(grid_cost_1, r, field="least_cost_1")
crs(rr_cost_1) <- EEC
rr_cost_1
plot(rr_cost_1)

# load in the library to calculate least cost travel paths...
library(gdistance) # documentation here (https://cran.microsoft.com/snapshot/2014-12-09/web/packages/gdistance/vignettes/gdistance.pdf)
# library(gdata)
# library(dismo)
# library(rgdal)
# library(sp)
# library(maptools)

# Make the least cost travel cost...

# # # simplify the cities file to be just coordinates of cities you want...
# cities_raw <- read_dta("All_Cities.dta")
# # Spatialize the data
# EEC <- "+proj=eqdc +lat_0=0 +lon_0=0 +lat_1=43 +lat_2=62 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs"
# cities <- st_as_sf(cities_raw, coords=c("longitude", "latitude"), crs=4326) %>%
#   st_transform(EEC)
# # keep only the variables you want
# glimpse(cities)
# #cities <- cities %>% dplyr::select(ANY VARIABLES YOU WANT TO KEEP)
# cities <- cities %>% filter(countryname != "Oman")
# cities <- cities %>% filter(countryname != "Iraq")
# cities <- cities %>% filter(countryname != "Saudi Arabia")
# cities <- cities %>% filter(countryname != "Yemen")
# # cities <- distinct(cities)
# # cities %>% is.na()


# Cost_1
# convert rr to Formal class RasterLayer and calculate travel costs...
writeRaster(rr_cost_1, "test.tif", overwrite=T)
samplecost <- raster("test.tif")
cities_sp <- as(cities, Class = "Spatial")
proj4string(cities_sp)
projection(samplecost)
samplecitiescoords <-as.data.frame(coordinates(cities_sp))
plot(cities_sp,pch=16,col="red")
plot(samplecost,add=TRUE,alpha=.5)
transition_raster <- transition(1/samplecost, mean, 8)
x <- as.numeric(samplecitiescoords [,1])
y <- as.numeric(samplecitiescoords [,2])
coord <-matrix(c(x,y),ncol=2)
costmatrix <-costDistance(transition_raster,coord,coord)
Rouen <-c(78834.2302421596, 5479628.37436075)
Marseille <-c(435041.606629771, 4811852.37775111)
RouenToMarseille <-shortestPath(transition_raster,Rouen,Marseille,output="SpatialLines")
lines(RouenToMarseille,col="black")

plot(samplecost,reset=TRUE,alpha=.5)
lines(RouenToMarseille,col="black")

### BEGIN HERE ###

# Convert the matrix of travel costs (costmatrix) into a dataframe you can use...
glimpse(cities)
city_id <- cities[,2]
id_vector <- pull(city_id, city_id)
costmatrix_labeled <- cbind(id_vector, costmatrix)
colnames(costmatrix_labeled) <- c("id", id_vector)
df <- as_tibble(costmatrix_labeled)

# convert from wide to long...
library(reshape2)
ma <- melt(df, id.vars=c("id"))
ma <- ma %>% rename(city_id = id)
ma <- ma %>% rename(destination = variable)
ma <- ma %>% rename(cost1 = value)

# merge in city names...
library(hablar)
ma <- left_join(ma, cities_raw, by = "city_id")
ma <- ma %>% rename(origin = city_id)
ma <- ma %>% rename(city_id = destination)
cities <- cities %>% dplyr::select(city_jjk, city_id)
cities <- cities %>% dplyr::rename(dest_city = city_jjk)
ma <- ma %>% convert(num(city_id))
ma <- left_join(ma, cities, by = "city_id")
ma <- ma %>% rename(destination = city_id)
ma <- ma %>% rename(city_id = origin)
ma <- ma %>% rename(origin_city = city_jjk)
ma_cost1 <- ma

# combine the 4 measures of market access together...

ma_cost1 <- ma_cost1 %>%
  st_sf() %>%
  st_drop_geometry

ma_cost2 <- ma_cost2 %>%
  dplyr::select(-origin_city, -dest_city, -latitude, -longitude, -countryname) %>%
  st_sf() %>%
  st_drop_geometry

ma_cost3 <- ma_cost3 %>%
  dplyr::select(-origin_city, -dest_city, -latitude, -longitude, -countryname) %>%
  st_sf() %>%
  st_drop_geometry

ma_cost4 <- ma_cost4 %>%
  dplyr::select(-origin_city, -dest_city, -latitude, -longitude, -countryname) %>%
  st_sf() %>%
  st_drop_geometry

ma_cost_full <- left_join(ma_cost1, ma_cost2, by = c("city_id", "destination"))

ma_cost_full <- left_join(ma_cost_full, ma_cost3, by = c("city_id", "destination"))

ma_cost_full <- left_join(ma_cost_full, ma_cost4, by = c("city_id", "destination"))

head(ma_cost_full)
ma_cost_full <- ma_cost_full %>% dplyr::select(city_id, destination, origin_city, dest_city, cost1, cost2, cost3, cost4)

# save as tibble and as dta file...
write_rds(ma_cost_full, "ma_cost_full.rds")
write_dta(ma_cost_full, "ma_cost_full.dta", version = 14)








# # create the measures of market access for each century
# 
# # clear the environment
# rm(list = ls())
# 
# library(tidyverse)
# library(sf)
# library(stargazer)
# library(haven)
# library(raster)
# 
# setwd("/Users/noeljohnson/Dropbox/Research/Assimiliation and Cohesiveness/Empirical/")
# 
# # bring in the dyadic travel cost data
# ma_dyads <- read_rds("ma_cost_full.rds")
# head(ma_dyads)
# # bring in the century-level population data
# base_century <- read_dta("Base_Century_Data_12-3-19.dta")
# head(base_century)
# 
# # create a population variable = 500 people if pop==0
# base_century <- base_century %>% mutate(altpop = ifelse(is.na(pop), 0.5, pop))
# 
# # want to create a variable in dyad data set for each century that will contain the population of the destination city.
# altpop_mergedata <- base_century %>%
#   dplyr::select(Bairoch_id, year, altpop) %>%
#   dplyr::filter(year>=1100) %>%
#   spread(year, altpop) %>%
#   rename(destination = Bairoch_id) %>%
#   rename(pop_1100 = "1100") %>%
#   rename(pop_1200 = "1200") %>%
#   rename(pop_1300 = "1300") %>%
#   rename(pop_1400 = "1400") %>%
#   rename(pop_1500 = "1500") %>%
#   rename(pop_1600 = "1600") %>%
#   rename(pop_1700 = "1700") %>%
#   rename(pop_1750 = "1750") %>%
#   rename(pop_1800 = "1800") %>%
#   rename(pop_1850 = "1850")
# 
# jewspresent_mergedata <- base_century %>%
#   dplyr::select(Bairoch_id, year, jewspresent) %>%
#   dplyr::filter(year>=1100) %>%
#   spread(year, jewspresent) %>%
#   rename(destination = Bairoch_id) %>%
#   rename(jewspresent_1100 = "1100") %>%
#   rename(jewspresent_1200 = "1200") %>%
#   rename(jewspresent_1300 = "1300") %>%
#   rename(jewspresent_1400 = "1400") %>%
#   rename(jewspresent_1500 = "1500") %>%
#   rename(jewspresent_1600 = "1600") %>%
#   rename(jewspresent_1700 = "1700") %>%
#   rename(jewspresent_1750 = "1750") %>%
#   rename(jewspresent_1800 = "1800") %>%
#   rename(jewspresent_1850 = "1850")
# 
# merge <- left_join(altpop_mergedata, jewspresent_mergedata, by = "destination")
# 
# # merge in the variables to the dyad data set
# ma_dyads <- left_join(ma_dyads, merge, by = "destination")
# #write_dta(ma_dyads, "ma_dyads_2-13-20.dta", version = 14)
# 
# 
# # create market access and Jewish network access for each year. Assume sigma = 1...
# # do it iteratively for cost1, cost2, cost3, cost4, jewish_network_access...
# 
# # cost1 (exclude own city, pop = pop/1000)
# 
# ma_cost1 <- ma_dyads %>%
#   filter(cost1 != 0) %>%
#   group_by(Bairoch_id) %>%
#   mutate("1100" = log(sum(pop_1100/cost1))) %>%
#   mutate("1200" = log(sum(pop_1200/cost1))) %>%
#   mutate("1300" = log(sum(pop_1300/cost1))) %>%
#   mutate("1400" = log(sum(pop_1400/cost1))) %>%
#   mutate("1500" = log(sum(pop_1500/cost1))) %>%
#   mutate("1600" = log(sum(pop_1600/cost1))) %>%
#   mutate("1700" = log(sum(pop_1700/cost1))) %>%
#   mutate("1750" = log(sum(pop_1750/cost1))) %>%
#   mutate("1800" = log(sum(pop_1800/cost1))) %>%
#   mutate("1850" = log(sum(pop_1850/cost1))) %>%
#   ungroup() %>%
#   dplyr::select(Bairoch_id, "1100", "1200", "1300", "1400", "1500", "1600", "1700", "1750", "1800", "1850") %>%
#   group_by(Bairoch_id) %>%
#   summarize_all(mean) %>%
#   ungroup() %>%
#   gather(year, ma_cost1, 2:11) %>%
#   mutate_at(2, as.numeric)
# 
# 
# ###########################################
# ##########################################
# 
# # # cost1 (include own city (cost = 0.05, pop = 1,000's))
# # # make cost of travelling in own city 0.05
# # ma_dyads <- ma_dyads %>%
# #   mutate("cost1_own" = ifelse(cost1==0, 0.05, cost1)) %>%
# #   mutate("pop_1100" = 1000*pop_1100) %>%
# #   mutate("pop_1200" = 1000*pop_1200) %>%
# #   mutate("pop_1300" = 1000*pop_1300) %>%
# #   mutate("pop_1400" = 1000*pop_1400) %>%
# #   mutate("pop_1500" = 1000*pop_1500) %>%
# #   mutate("pop_1600" = 1000*pop_1600) %>%
# #   mutate("pop_1700" = 1000*pop_1700) %>%
# #   mutate("pop_1750" = 1000*pop_1750) %>%
# #   mutate("pop_1800" = 1000*pop_1800) %>%
# #   mutate("pop_1850" = 1000*pop_1850)
# 
# # cost1 (include own city (cost = 0.1))
# # make cost of travelling in own city 0.05
# # ma_dyads <- ma_dyads %>%
# #   mutate("cost1_own" = ifelse(cost1==0, 0.1, cost1))
# # 
# # ma_cost1_alt <- ma_dyads %>%
# #   group_by(Bairoch_id) %>%
# #   mutate("1100" = log(sum(pop_1100/cost1_own))) %>%
# #   mutate("1200" = log(sum(pop_1200/cost1_own))) %>%
# #   mutate("1300" = log(sum(pop_1300/cost1_own))) %>%
# #   mutate("1400" = log(sum(pop_1400/cost1_own))) %>%
# #   mutate("1500" = log(sum(pop_1500/cost1_own))) %>%
# #   mutate("1600" = log(sum(pop_1600/cost1_own))) %>%
# #   mutate("1700" = log(sum(pop_1700/cost1_own))) %>%
# #   mutate("1750" = log(sum(pop_1750/cost1_own))) %>%
# #   mutate("1800" = log(sum(pop_1800/cost1_own))) %>%
# #   mutate("1850" = log(sum(pop_1850/cost1_own))) %>%
# #   ungroup() %>%
# #   dplyr::select(Bairoch_id, "1100", "1200", "1300", "1400", "1500", "1600", "1700", "1750", "1800", "1850") %>%
# #   group_by(Bairoch_id) %>%
# #   summarize_all(mean) %>%
# #   ungroup() %>%
# #   gather(year, ma_cost1_alt, 2:11) %>%
# #   mutate_at(2, as.numeric)
# # 
# # # write_dta(ma_cost1_alt, "ma_cost1_alt.dta", version = 14)
# # 
# ######################################
# ######################################
# 
# 
# # cost2
# ma_cost2 <- ma_dyads %>%
#   filter(cost1 != 0) %>%
#   group_by(Bairoch_id) %>%
#   mutate("1100" = log(sum(pop_1100/cost2))) %>%
#   mutate("1200" = log(sum(pop_1200/cost2))) %>%
#   mutate("1300" = log(sum(pop_1300/cost2))) %>%
#   mutate("1400" = log(sum(pop_1400/cost2))) %>%
#   mutate("1500" = log(sum(pop_1500/cost2))) %>%
#   mutate("1600" = log(sum(pop_1600/cost2))) %>%
#   mutate("1700" = log(sum(pop_1700/cost2))) %>%
#   mutate("1750" = log(sum(pop_1750/cost2))) %>%
#   mutate("1800" = log(sum(pop_1800/cost2))) %>%
#   mutate("1850" = log(sum(pop_1850/cost2))) %>%
#   ungroup() %>%
#   dplyr::select(Bairoch_id, "1100", "1200", "1300", "1400", "1500", "1600", "1700", "1750", "1800", "1850") %>%
#   group_by(Bairoch_id) %>%
#   summarize_all(mean) %>%
#   ungroup() %>%
#   gather(year, ma_cost2, 2:11) %>%
#   mutate_at(2, as.numeric)
# 
# # cost3
# ma_cost3 <- ma_dyads %>%
#   filter(cost1 != 0) %>%
#   group_by(Bairoch_id) %>%
#   mutate("1100" = log(sum(pop_1100/cost3))) %>%
#   mutate("1200" = log(sum(pop_1200/cost3))) %>%
#   mutate("1300" = log(sum(pop_1300/cost3))) %>%
#   mutate("1400" = log(sum(pop_1400/cost3))) %>%
#   mutate("1500" = log(sum(pop_1500/cost3))) %>%
#   mutate("1600" = log(sum(pop_1600/cost3))) %>%
#   mutate("1700" = log(sum(pop_1700/cost3))) %>%
#   mutate("1750" = log(sum(pop_1750/cost3))) %>%
#   mutate("1800" = log(sum(pop_1800/cost3))) %>%
#   mutate("1850" = log(sum(pop_1850/cost3))) %>%
#   ungroup() %>%
#   dplyr::select(Bairoch_id, "1100", "1200", "1300", "1400", "1500", "1600", "1700", "1750", "1800", "1850") %>%
#   group_by(Bairoch_id) %>%
#   summarize_all(mean) %>%
#   ungroup() %>%
#   gather(year, ma_cost3, 2:11) %>%
#   mutate_at(2, as.numeric)
# 
# # cost4
# ma_cost4 <- ma_dyads %>%
#   filter(cost1 != 0) %>%
#   group_by(Bairoch_id) %>%
#   mutate("1100" = log(sum(pop_1100/cost4))) %>%
#   mutate("1200" = log(sum(pop_1200/cost4))) %>%
#   mutate("1300" = log(sum(pop_1300/cost4))) %>%
#   mutate("1400" = log(sum(pop_1400/cost4))) %>%
#   mutate("1500" = log(sum(pop_1500/cost4))) %>%
#   mutate("1600" = log(sum(pop_1600/cost4))) %>%
#   mutate("1700" = log(sum(pop_1700/cost4))) %>%
#   mutate("1750" = log(sum(pop_1750/cost4))) %>%
#   mutate("1800" = log(sum(pop_1800/cost4))) %>%
#   mutate("1850" = log(sum(pop_1850/cost4))) %>%
#   ungroup() %>%
#   dplyr::select(Bairoch_id, "1100", "1200", "1300", "1400", "1500", "1600", "1700", "1750", "1800", "1850") %>%
#   group_by(Bairoch_id) %>%
#   summarize_all(mean) %>%
#   ungroup() %>%
#   gather(year, ma_cost4, 2:11) %>%
#   mutate_at(2, as.numeric)
# 
# # jn_cost1
# jn_cost1 <- ma_dyads %>%
#   filter(cost1 != 0) %>%
#   group_by(Bairoch_id) %>%
#   mutate("1100" = log(sum(jewspresent_1100/cost1))) %>%
#   mutate("1200" = log(sum(jewspresent_1200/cost1))) %>%
#   mutate("1300" = log(sum(jewspresent_1300/cost1))) %>%
#   mutate("1400" = log(sum(jewspresent_1400/cost1))) %>%
#   mutate("1500" = log(sum(jewspresent_1500/cost1))) %>%
#   mutate("1600" = log(sum(jewspresent_1600/cost1))) %>%
#   mutate("1700" = log(sum(jewspresent_1700/cost1))) %>%
#   mutate("1750" = log(sum(jewspresent_1750/cost1))) %>%
#   mutate("1800" = log(sum(jewspresent_1800/cost1))) %>%
#   mutate("1850" = log(sum(jewspresent_1850/cost1))) %>%
#   ungroup() %>%
#   dplyr::select(Bairoch_id, "1100", "1200", "1300", "1400", "1500", "1600", "1700", "1750", "1800", "1850") %>%
#   group_by(Bairoch_id) %>%
#   summarize_all(mean) %>%
#   ungroup() %>%
#   gather(year, jn_cost1, 2:11) %>%
#   mutate_at(2, as.numeric)
# 
# # jn_cost2
# jn_cost2 <- ma_dyads %>%
#   filter(cost1 != 0) %>%
#   group_by(Bairoch_id) %>%
#   mutate("1100" = log(sum(jewspresent_1100/cost2))) %>%
#   mutate("1200" = log(sum(jewspresent_1200/cost2))) %>%
#   mutate("1300" = log(sum(jewspresent_1300/cost2))) %>%
#   mutate("1400" = log(sum(jewspresent_1400/cost2))) %>%
#   mutate("1500" = log(sum(jewspresent_1500/cost2))) %>%
#   mutate("1600" = log(sum(jewspresent_1600/cost2))) %>%
#   mutate("1700" = log(sum(jewspresent_1700/cost2))) %>%
#   mutate("1750" = log(sum(jewspresent_1750/cost2))) %>%
#   mutate("1800" = log(sum(jewspresent_1800/cost2))) %>%
#   mutate("1850" = log(sum(jewspresent_1850/cost2))) %>%
#   ungroup() %>%
#   dplyr::select(Bairoch_id, "1100", "1200", "1300", "1400", "1500", "1600", "1700", "1750", "1800", "1850") %>%
#   group_by(Bairoch_id) %>%
#   summarize_all(mean) %>%
#   ungroup() %>%
#   gather(year, jn_cost2, 2:11) %>%
#   mutate_at(2, as.numeric)
# 
# # jn_cost3
# jn_cost3 <- ma_dyads %>%
#   filter(cost1 != 0) %>%
#   group_by(Bairoch_id) %>%
#   mutate("1100" = log(sum(jewspresent_1100/cost3))) %>%
#   mutate("1200" = log(sum(jewspresent_1200/cost3))) %>%
#   mutate("1300" = log(sum(jewspresent_1300/cost3))) %>%
#   mutate("1400" = log(sum(jewspresent_1400/cost3))) %>%
#   mutate("1500" = log(sum(jewspresent_1500/cost3))) %>%
#   mutate("1600" = log(sum(jewspresent_1600/cost3))) %>%
#   mutate("1700" = log(sum(jewspresent_1700/cost3))) %>%
#   mutate("1750" = log(sum(jewspresent_1750/cost3))) %>%
#   mutate("1800" = log(sum(jewspresent_1800/cost3))) %>%
#   mutate("1850" = log(sum(jewspresent_1850/cost3))) %>%
#   ungroup() %>%
#   dplyr::select(Bairoch_id, "1100", "1200", "1300", "1400", "1500", "1600", "1700", "1750", "1800", "1850") %>%
#   group_by(Bairoch_id) %>%
#   summarize_all(mean) %>%
#   ungroup() %>%
#   gather(year, jn_cost3, 2:11) %>%
#   mutate_at(2, as.numeric)
# 
# # jn_cost4
# jn_cost4 <- ma_dyads %>%
#   filter(cost1 != 0) %>%
#   group_by(Bairoch_id) %>%
#   mutate("1100" = log(sum(jewspresent_1100/cost4))) %>%
#   mutate("1200" = log(sum(jewspresent_1200/cost4))) %>%
#   mutate("1300" = log(sum(jewspresent_1300/cost4))) %>%
#   mutate("1400" = log(sum(jewspresent_1400/cost4))) %>%
#   mutate("1500" = log(sum(jewspresent_1500/cost4))) %>%
#   mutate("1600" = log(sum(jewspresent_1600/cost4))) %>%
#   mutate("1700" = log(sum(jewspresent_1700/cost4))) %>%
#   mutate("1750" = log(sum(jewspresent_1750/cost4))) %>%
#   mutate("1800" = log(sum(jewspresent_1800/cost4))) %>%
#   mutate("1850" = log(sum(jewspresent_1850/cost4))) %>%
#   ungroup() %>%
#   dplyr::select(Bairoch_id, "1100", "1200", "1300", "1400", "1500", "1600", "1700", "1750", "1800", "1850") %>%
#   group_by(Bairoch_id) %>%
#   summarize_all(mean) %>%
#   ungroup() %>%
#   gather(year, jn_cost4, 2:11) %>%
#   mutate_at(2, as.numeric)
# 
# # merge all the access measures with the original base data set
# base_century <- base_century %>%
#   filter(year>=1100)
# base_century_merged <- left_join(base_century, ma_cost1, by = c("Bairoch_id", "year"))
# base_century_merged <- left_join(base_century_merged, ma_cost2, by = c("Bairoch_id", "year"))
# base_century_merged <- left_join(base_century_merged, ma_cost3, by = c("Bairoch_id", "year"))
# base_century_merged <- left_join(base_century_merged, ma_cost4, by = c("Bairoch_id", "year"))
# base_century_merged <- left_join(base_century_merged, jn_cost1, by = c("Bairoch_id", "year"))
# base_century_merged <- left_join(base_century_merged, jn_cost2, by = c("Bairoch_id", "year"))
# base_century_merged <- left_join(base_century_merged, jn_cost3, by = c("Bairoch_id", "year"))
# base_century_merged <- left_join(base_century_merged, jn_cost4, by = c("Bairoch_id", "year"))
# 
# # save as tibble and as dta file...
# write_rds(base_century_merged, "Base_Century_Data_12-3-19.rds")
# write_dta(base_century_merged, "Base_Century_Data_12-3-19.dta", version = 14)

############################

# Scratch Code

############################


# afr_rds <- afr_rds %>%
#   mutate(value=replace(value, type=="Unknown", 1))
# afr_rds <- afr_rds %>%
#   mutate(value=replace(value, type=="Road", 2))










# End Code