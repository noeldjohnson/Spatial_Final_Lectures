# Code for Spatial Econometrics: Lecture 2
# 9/3/19
# Author: Noel D. Johnson


#install.packages("tidyverse")

library(tidyverse)

i_df <- iris # built-in data
i_df
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
#setwd("/Users/noeljohnson/Dropbox/Research/")

cps08 <- read_csv("data/cps08.csv")
cps08
print(cps08, n=2)

cps08 <- select(cps08, c(age,bachelor,female))
cps08 <- filter(cps08, age==33)

arrange(cps08, bachelor)

rename(cps08, married=bachelor)
cps08


mutate(cps08, agesq = age^2)

# you can use stargazer to summarize variables

cps08 <- read_csv("data/cps08.csv")

library(stargazer)
cps08_no_na <- na.omit(cps08)
stargazer(as.data.frame(cps08_no_na), type="text")

cps08_bachmean <- cps08 %>%
  group_by(bachelor) %>%
  summarize(mean(ahe, na.rm=TRUE)) %>%
  ungroup()
cps08_bachmean


# Here's where the spatial stuff starts

#install.packages(sf)

library(sf)

rm(list=ls()) # clear space

# get pkgs
library(tidyverse)
library(sf)

setwd("/Users/noeljohnson/Dropbox/Teaching/Spatial_Fall_2019/Lectures/")

africa_sf <- st_read("data/africa_scale.shp")
africa_sf
dim(africa_sf)

plot(africa_sf[64])

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




