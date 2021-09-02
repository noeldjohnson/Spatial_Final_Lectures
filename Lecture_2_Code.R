# Code for Spatial Econometrics: Lecture 2
# 9/3/19
# Author: Noel D. Johnson

#test

#install.packages("tidyverse")

library(tidyverse)

i_df <- iris # built-in data
class(i_df)
head(i_df)

i_tbl <- as_tibble(iris)
class(i_tbl)

i_tbl # or print

i_tbl <- i_tbl[1:5, ] # Subset the first 5 rows
names(i_tbl)
i_tbl$Sepal.Length # Extract by name

i_tbl[["Sepal.Length"]] # Extract by name

i_tbl["Sepal.Length"] # or i_tbl[1]

i_tbl <- as_tibble(iris)
i_df <- as.data.frame(i_tbl)
class(i_df)

# Here's how to set a working directory
#setwd("/Users/noeljohnson/Dropbox/Research/Tambora/Data/Analysis/Base_Data_Sets/Base_Temps_Data/Europe_temps/")

temp_sp <- read_csv("/Users/noeljohnson/Dropbox/Research/Tambora/Data/Analysis/Base_Data_Sets/Base_Temps_Data/Europe_temps/temp_spring_eu.csv")
temp_sp
print(temp_sp, n=2)

temp <- select(temp_sp, year)
temp <- filter(temp_sp, year==1815)
filter(temp_sp, year==1815)

data_1815 <- filter(temp_sp, year==1815)

arrange(temp_sp, temp_sp)
arrange(temp_sp, -temp_sp)

rename(temp_sp, temp_spring=temp_sp, long=longitude, lat=latitude)
temp_sp

mutate(temp_sp, mean_deviation = temp_sp - mean(temp_sp, na.rm=TRUE) )

temp_sp_grpd <- group_by(temp_sp, year)
summarize(temp_sp_grpd, mean(temp_sp, na.rm=TRUE))

summarise_all(temp_sp, list(mean), na.rm=TRUE)

# or, you can use stargazer

library(stargazer)
temp_sp_no_na <- na.omit(temp_sp)
stargazer(as.data.frame(temp_sp_no_na), type="text")
stargazer(as.data.frame(temp_sp), type="text")

temp <- temp_sp %>%
  group_by(year) %>%
  summarize(mean(temp_sp, na.rm=TRUE)) %>%
  ungroup()

temp_sp <- temp_sp %>%
  group_by(year) %>%
  mutate(mean_yearly_temp = mean(temp_sp, na.rm=TRUE)) %>%
  ungroup()
temp_sp


# Here's where the spatial stuff starts

#install.packages(sf)

library(sf)

rm(list=ls()) # clear space

# get pkgs
library(tidyverse)
library(sf)

setwd("/Users/noeljohnson/Dropbox/Teaching/Spatial_Fall_2019/Lectures/")

africa_sf <- st_read("./data/africa_scale.shp")
africa_sf
plot(africa_sf["admin"])

class(africa_sf) # What is it?
st_crs(africa_sf) # What's the CRS?
st_bbox(africa_sf) # And the extent/bounding box?
dim(africa_sf)

# Subset, there are way too many variables
africa_sf <- africa_sf %>%
  select(admin, type, iso_a3, region_wb, pop_est)

# There's a geometry variable
names(africa_sf)[6]

# Make a fast and easy plot
plot(africa_sf["pop_est"], reset=F)

# read the csv of african cities
cities_csv <- read_csv("./data/africa_cities.csv")

# filter out missing coords
cities_csv <- cities_csv %>%
  filter( !( is.na(lon) & is.na(lat) ) )

# turn into sf points, specify coords and crs
cities_sf <- st_as_sf(cities_csv,
                      coords = c("lon", "lat"),
                      crs = 4326)

plot(cities_sf[1], axes = T, pch = 20,
     col = "black", main = "Cities")

# Plot the regions and cities together
plot(africa_sf["pop_est"], axes = T,
     main = "WB Regions", key.pos = 1,
     key.width = lcm(1.3), key.length = 1.0, reset=F)
plot(cities_sf[1], axes = T, pch = 20,
     col = "black", main = "Cities", add = TRUE)

# delete the existing proj.4 and epsg code
africa_sf <- africa_sf %>% st_set_crs(NA)
st_crs(africa_sf)

# assign a CRS
africa_sf <- africa_sf %>% st_set_crs("+proj=longlat")
st_crs(africa_sf)

# Nairobi in longlat
cty1 <- tibble(name= "Nairobi",
               lon = 36.82241, lat = -1.287822) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)
st_is_longlat(cty1)
st_coordinates(cty1)

# Kinshasa in mollweide
cty2 <- tibble(name = "Kinshasa",
               x = 1531775, y = -531896.9) %>%
  st_as_sf(coords = c("x", "y"), crs = "+proj=moll")
st_is_longlat(cty2)
st_coordinates(cty2)

ctys <- rbind(cty1, cty2)

# transform to longlat
cty2 <- cty2 %>% st_transform(4326)
st_is_longlat(cty2)
st_coordinates(cty2)
# row bind and take a look
ctys <- rbind(cty1, cty2) %>% glimpse()

st_distance(cty1,cty2) # st_distance(ctys) for matrix

st_area(ctys) # points have no area

require(units)
a_to_b <- st_distance(cty1,cty2)
set_units(a_to_b, km)
set_units(a_to_b, km^2)
#strip it of its units:
as.numeric(a_to_b)

# get the distance matrix and then gather
small_cities <- cities_sf %>%
  slice(1:10)
nameraw <- small_cities[[5,]]
nameraw
dist_matrix <- st_distance(small_cities)
dist_matrix
colnames(dist_matrix) <- nameraw
dist_matrix
dist_matrix <- as_tibble(dist_matrix)
dist_matrix
dist_matrix <- dist_matrix %>%
  mutate(nameraw = nameraw)
tt <- c(1:10)
distances <- gather(dist_matrix, key = "distname", names(dist_matrix)[tt], value = "distance")
distances
distance_Domoni <- distances %>% filter(nameraw=="Domoni")
distance_Domoni

# If I re-did this without redundant code...
small_cities <- cities_sf %>%
  slice(1:10)
nameraw <- small_cities[[5,]]
dist_matrix <- st_distance(small_cities)
colnames(dist_matrix) <- nameraw
dist_matrix <- as_tibble(dist_matrix)
dist_matrix <- dist_matrix %>%
  mutate(nameraw = nameraw)
tt <- c(1:10)
distances <- gather(dist_matrix, key = "distname", names(dist_matrix)[tt], value = "distance")
distance_Domoni <- distances %>% filter(nameraw=="Domoni")

# End Code




