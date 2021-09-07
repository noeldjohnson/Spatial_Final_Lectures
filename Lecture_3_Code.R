# Code for Exercises in Lecture 3: Spatial Econometrics
# 9/10/19
# Author: Noel D. Johnson

library(tidyverse)
library(sf)
library(stargazer)
library(units)

# Exercise 1: Buffers
# Open the African boundaries (africa_scale.shp) layer
setwd("/Users/noeljohnson/Dropbox/Teaching/Spatial_Fall_2019/Lectures/") 
africa_sf <- st_read("./data/africa_scale.shp", quiet = T)
africa_sf
plot(africa_sf[1], col=NA, axes=T)

# Compute the centroid of each country, compute a point on the surface of each polygon. Plot both including the boundaries and compare their locations.
africa_cen_sf <- st_centroid(africa_sf)
africa_pos_sf <- st_point_on_surface(africa_sf)

# plot
plot(africa_sf[1], col=NA, main="Centroids vs. Surface Points", reset=F)
plot(africa_cen_sf, col=2, pch=20, add=T)
plot(africa_pos_sf, col=3, pch=20, add=T)

# Do all of these steps in four different CRSs: geographic (4326), Mollweide equal area ("+proj=moll"), Lambert Azimuthal equal area (9820), and Azimuthal equidistant projection ("+proj=aeqd")

# 2.1 Buffer the geographic centroid by either 1 degree or 111 km.

# 2.2 Compute the area of each buffered centroid in km 2 based on geographic coordinates.

# geographic
africa_buf1_sf <- africa_cen_sf %>% select(admin, adm0_a3) %>%
  st_buffer(dist=1) %>% mutate(b1_area = st_area(.)) %>%
  mutate(b1_area = set_units(b1_area, km^2))
# mollweide equalarea
africa_buf2_sf <- africa_cen_sf %>% select(admin, adm0_a3) %>%
  st_transform("+proj=moll") %>% st_buffer(dist=111e3) %>%
  st_transform(4326) %>% mutate(b2_area = st_area(.)) %>%
  mutate(b2_area = set_units(b2_area, km^2))
# lambert azimuthal equal area
africa_buf3_sf <- africa_cen_sf %>% select(admin, adm0_a3) %>%
  st_transform("+proj=laea") %>% st_buffer(dist=111e3) %>%
  st_transform(4326) %>% mutate(b3_area = st_area(.)) %>%
  mutate(b3_area = set_units(b3_area, km^2))
# azimuthal equidistant
africa_buf4_sf <- africa_cen_sf %>% select(admin, adm0_a3) %>%
  st_transform("+proj=aeqd") %>% st_buffer(dist=111e3) %>%
  st_transform(4326) %>% mutate(b4_area = st_area(.)) %>%
  mutate(b4_area = set_units(b4_area, km^2))
plot(africa_buf4_sf[1])

# Compare the summary statistics of the resulting areas. Is this what you expected?
summary(africa_buf1_sf$b1_area)
summary(africa_buf2_sf$b2_area)
summary(africa_buf3_sf$b3_area)
summary(africa_buf4_sf$b4_area)

# look up
# desktop.arcgis.com/en/arcmap/10.3/guide-books/map-projections/mollweide.htm
# desktop.arcgis.com/en/arcmap/10.3/guide-books/map-projections/lambert-azimuthal-equal-area.htm

# Exercise 2: Geometry Operations
# Work through a few geometry operations. In an empty file, clear the work space, load sf and then . . .

rm(list=ls())
library(sf)

#1. Create two points: P1 = (0, 1) and P2 = (1, 1). Combine them into a feature collection and buffer each point by one unit. Plot the result with plot(..., col = 1:2).

# points as sfc
a <- st_point(c(0, 1))
b <- st_point(c(1, 1))
pts <- st_sfc(a,b)

# buffer
pts_buff <- st_buffer(pts, dist = 1) # convert points to circles

# plot both
plot(pts_buff, col=1:2)
plot(pts, add=T)

# 2. Use st_intersection() to calculate the intersection. Add this to the previous plot.

# intersection
x_and_y <- st_intersection(pts_buff[1], pts_buff[2])
plot(pts_buff, col=1:2)

# color intersecting area
plot(x_and_y, col = 3, add = T)

# 3. Subtract the intersection from the buffered points. You can do this in at least two ways. Do two of them. Plot the result with plot(..., col = 1:2).

# direct interpretation:
# erase intersection from union
diff1 <- st_difference(pts_buff, x_and_y)
plot(diff1, col=1:2)

# indirect interpretation:
# symmetric difference of buffers (returns portion of a and b that do not intersect)
diff2 <- st_sym_difference(pts_buff[1], pts_buff[2])
plot(diff2, col=1:2)

# 4. Calculate the union of the buffered points. What’s the difference to the earlier geometry?

both <- st_union(pts_buff)
plot(both)

# 5. Does the point P3 = (0.5, 0.5) intersect with the union of buffers? How about the buffers less their own intersection?

c <- st_point(c(0.5,0.5))
st_intersects(c, both, sparse = F)

st_intersects(c, diff1, sparse = F)

# Exercise 3: Railways
#We’ll examine railways in former British East Africa. Open the African boundaries (africa_scale.shp) and African railways (africa_rail.shp) layers.

rm(list=ls())

library(sf)
library(tidyverse)
library(units)

setwd("/Users/noeljohnson/Dropbox/Teaching/Spatial_Fall_2019/Lectures/")

africa_sf <- st_read("./data/africa_scale.shp", quiet = T)
rail_sf <- st_read("./data/africa_rail.shp", quiet = T)

# 1. British East Africa consisted of Kenya, Tanzania and Uganda. Union the boundaries of these polygons without creating slivers.

# union BEA without slivers
bea_sf <- africa_sf %>%
  filter(name %in% c("Kenya", "Tanzania", "Uganda")) %>%
  st_transform("+proj=moll")
bea_sf <- bea_sf %>% st_snap(bea_sf, 1) %>% group_by(continent) %>%
  summarize() %>% st_transform(4326)
bea_sf

# plot
plot(st_geometry(bea_sf), reset=F)
plot(rail_sf[1], add=T)

# Or...

plot(bea_sf[1], col=NA, reset=F)
plot(rail_sf[1], add=T)


# 2. Compute the total length of railway lines in km which fall inside the former British East Africa boundaries. Now compute the total length of railway lines in each of the three countries.

# compute intersection and length
bea_rail_sf <- rail_sf %>% st_intersection(bea_sf) %>%
  mutate(length = st_length(.)) %>%
  mutate(length = set_units(length, km)) %>%
  group_by(continent) %>% summarize(bea_railkm = sum(length))

# subset
africa_sf <- africa_sf %>%
  filter(name %in% c("Kenya", "Tanzania", "Uganda"))

# compute intersection and length
ctry_rail_sf <- rail_sf %>% st_intersection(africa_sf) %>%
  mutate(length = st_length(.)) %>% mutate(length = set_units(length, km)) %>%
  group_by(adm0_a3) %>% summarize(ctry_railkm = sum(length))


# 3. Join all of these data to the African boundaries file of the three countries. Plot the share of railways falling inside each of the three countries. Where were most of the railways built?

africa_sf <- africa_sf %>% st_join(bea_rail_sf) %>%
  st_join(ctry_rail_sf) %>% mutate(share = ctry_railkm/ bea_railkm)
br <- c(seq(0, 1, by=.01))
plot(africa_sf["share"], breaks = br)


# Exercise 4: Road Densities, Grids, and Projections

# Let’s revisit our road density results from earlier. Open the African boundaries (africa_scale.shp) and African roads layers (africa_roads.shp).

rm(list=ls())

library(sf)
library(tidyverse)
library(units)

setwd("/Users/noeljohnson/Dropbox/Teaching/Spatial_Fall_2019/Lectures/")

# read shp
africa_sf <- st_read("./data/africa_scale.shp", quiet = T)

# 1. Create a grid of 1° cells, starting at (-18, -35) in the lower left.

# make grid of 1° cells
grid_sf <- africa_sf %>% st_make_grid(cellsize = 1,
                                      offset = c(-18,-35),
                                      what="polygons")
st_crs(grid_sf)

# add IDs to grid, make sf
grid_sf <- st_sf(id = 1:length(grid_sf),
                 geometry = grid_sf)


# 2. Union the African boundaries without slivers and clip the grid to the boundary of the polygon using st_intersection().

# create union without slivers
africa_sf <- africa_sf %>% st_transform("+proj=moll")
outline_sf <- africa_sf %>% st_snap(africa_sf, tolerance = 100) %>%
  st_transform(4326) %>% st_union()

# create intersection
inter_sf <- st_intersection(grid_sf, outline_sf)
plot(inter_sf, col=NA)

# 3. Renumber the IDs, so that they run from 1 to N in the new clipped grid. Calculate the area of each cell in km2.

# renumber ids
inter_sf <- inter_sf %>% mutate(id = 1:nrow(inter_sf))

# calculate area, set units
inter_sf <- inter_sf %>% mutate(area = st_area(.)) %>%
  mutate(area = set_units(area, km^2))

#4. Intersect the roads layer with your grid. Compute the length in km of each road in the intersected cells. Calculate the total road length in each cell.

# open roads
roads_sf <- st_read("./data/africa_roads.shp", quiet = T)
roads_in_grid <- roads_sf %>% st_intersection(inter_sf) %>%
  mutate(road_length = st_length(.)) %>%
  mutate(road_length = set_units(road_length, km)) %>%
  st_set_geometry(NULL) %>% group_by(id) %>%
  summarize(total_length = sum(road_length))
glimpse(roads_in_grid)

# 5. Join this information back to the grid, calculate the road density in each cell, and the plot the result using plot(. . . , breaks=“kmeans”).

# join back to grid
inter_sf <- left_join(inter_sf, roads_in_grid, by="id")
inter_sf <- inter_sf %>% mutate(road_density = total_length/area)
inter_sf <- inter_sf %>%
  mutate(road_density = replace(road_density, is.na(road_density), 0))
plot(inter_sf["road_density"], breaks="kmeans", key.pos = 1,
     key.width = lcm(1.3), key.length = 1.0, axes=F, main="Road density")

#






















# End Code