# Code for Spatial Econometrics: Lecture 3 "Vector manipulations"
# Created: 9/10/19
# Last Edited: 9/8/21
# Author: Noel Johnson

library(tidyverse)
library(sf)
library(stargazer)


#Understanding sfg and sfc objects
# Create sfg point objects of some cities
h_sfg <- st_point(c(9.7320, 52.3759))
g_sfg <- st_point(c(9.9158, 51.5413))
w_sfg <- st_point(c(10.7865, 52.4227))
class(w_sfg)

# Create sfc object with multiple sfg objects
points_sfc <- st_sfc(h_sfg, g_sfg, w_sfg, crs = 4326)
class(points_sfc)

# Understanding sf objects
# Create a simple feature object
tbl <- tibble(name = c("Hannover", "Göttingen",
                       "Wolfsburg"))
points_sf <- st_sf(tbl, geometry = points_sfc)
class(points_sf)

points_sf %>% glimpse(width=50)

# Making lines and polygons
# Create line from points
line_sfc <- st_linestring(rbind(h_sfg, g_sfg, w_sfg))
print(line_sfc, width = 50)

# Create poly from points
poly_sfc <- st_polygon(list(rbind(h_sfg, g_sfg,
                                  w_sfg, h_sfg)))
print(poly_sfc, width = 50)


# Plotting basic geometries
par(mfrow = c(1, 3))
plot(points_sfc, main = "MULTIPOINT")
plot(line_sfc, main = "LINESTRING")
plot(poly_sfc, main = "POLYGON")

# Topological relations example
heim_sfg <- st_point(c(9.9580, 52.1548))
st_contains(poly_sfc, heim_sfg, sparse = F)

# Self-intersections illustrated
# Create a self-intersecting polygon:
p1_wkt <- "POLYGON((0 0, 0 10, 10 0, 10 10, 0 0))"
p1 <- st_as_sfc(p1_wkt, crs=4326)
st_is_valid(p1, reason = T)

par(mfrow = c(1, 1))
plot(p1)

# Repair using lwgeom, cast to single polygons
library(lwgeom)
p1_fixed <- st_make_valid(p1)
p1_fixed <- st_make_valid(p1) %>% st_cast("POLYGON")
st_is_valid(p1_fixed, reason = T)
plot(p1_fixed)

# Plot both polygons
par(mfrow=c(1,2))
plot(st_sf(a = 1, p1), reset=F)
plot(st_sf(b = 1:2, p1_fixed), key.pos=NULL, reset=F)

### BEGIN HERE IN CLASS ######

# Simplifiying polygons and lines using Douglas-Peucker
# algorithm (not spatially aware)
library(spData)
usa_ea <- st_transform(us_states, 2163) #from spData
plot(usa_ea[7], col=NA, main="")
usa_sf <- st_simplify(usa_ea, dTolerance = 1e+05)
plot(usa_sf[7], col=NA, main="")

# Simplifiying polygons and lines using rmapshaper package and 
# Visvalingam algorithm, which is spatially aware
library(rmapshaper)
usa_ea$AREA = as.numeric(usa_ea$AREA) # kill units
usa_ms <- ms_simplify(usa_ea, keep = 0.01)
plot(usa_ms[2], col=NA, main="")

# Centroids
dev.off()
par(mfrow=c(1,1))
plot(usa_ea[1], col=NA, main="", reset=F)
plot(st_centroid(usa_ea), col=2, add=T)
# plot(usa_ea[1], col=NA, main="", reset=F)
plot(st_point_on_surface(usa_ea), col=3, add=T)
dev.off()

# Buffers (be careful about CRS---sometimes must project using buffers)
buff_sf <- st_centroid(usa_ea) %>% st_buffer(dist = 1e5)
plot(buff_sf[7], col=NA, reset=F, main="")
plot(st_centroid(usa_ea)[7], col=2, add=T)

# Clipping
# Unions (dissolve)---pay attention to slivers
africa_sf <- st_read("./data/africa_scale.shp", quiet = T)
par(mfrow=c(1,2))
plot(africa_sf[64], col=NA, main="Original", reset=F)
plot(st_union(africa_sf), col=NA, main="Union")

# Use group and summarize for unions
# Transform to Mollweide
africa_sf <- st_transform(africa_sf, "+proj=moll")
# Group_by and summarize does unions
first_sf <- africa_sf %>%
  group_by(continent) %>% summarize()
# Snapping the object to itself by 1m kills slivers
second_sf <- africa_sf %>%
  st_snap(africa_sf, tolerance = 1) %>%
  group_by(continent) %>% summarize()
# Transform back to longlat
first_sf <- first_sf %>% st_transform(4326)
second_sf <- second_sf %>% st_transform(4326)

# Unions without slivers
par(mfrow=c(1,2))
plot(first_sf[1], col=NA, main="Union 1", reset=F)
plot(second_sf[1], col=NA, main="Union 2", reset=F)

# Cropping and bounding boxes
# get some data and subset boundaries to Kenya
afr_rds <- st_read("./data/africa_roads.shp", quiet=T)
kenya <- africa_sf %>% st_transform(4326) %>%
  filter(adm0_a3 == "KEN")
st_bbox(kenya)

## xmin ymin xmax ymax
## 33.89357 -4.67677 41.85508 5.50600
# calls st_bbox automatically
kenya_rds <- st_crop(afr_rds, kenya)

# Cropped roads and Kenyan boundaries
par(mfrow=c(1,1))
plot(kenya_rds[1], main="", reset=F,
     key.pos = 3, axes=T)
plot(kenya[1], col=NA, lwd=2, add=T)


# Spatial subsetting
dev.off()
par(mfrow=c(1,2))
plot(kenya_rds[kenya, "type", op = st_within],
     main="", key.pos=NULL, axes=T, reset=F)
plot(kenya[1], col=NA, lwd=2, add=T)
plot(kenya_rds["type"],
     main="", key.pos=NULL, axes=T, reset=F)
plot(kenya[1], col=NA, lwd=2, add=T)

# Spatial subsetting into a new object
names(kenya_rds)
kenya_rds_within <- kenya_rds[kenya, c("type", "adm0_a3"),
                              op = st_within]
class(kenya_rds_within)

# Spatial joins with topological operations
# Load in data and clean up variables
afr_ctys <- st_read("./data/africa_ctys.shp", quiet=T,
                    stringsAsFactors = F) %>%
  select(name, iso3v10)

afr_sf <- st_read("./data/africa_bnds.shp", quiet = T,
                  stringsAsFactors = F)
# join
afr_ctys_1 <- afr_ctys %>% st_join(afr_sf)

# filter out disagreements (that is, just keep disagreements)
afr_ctys_1 <- afr_ctys_1 %>% filter(iso3v10!=adm0_a3)

# all mismatches other than in MAR
glimpse(afr_ctys_1 %>% filter(admin != "Morocco"))

# let's focus on Bangui
bangui <- afr_ctys_1 %>% filter(name == "Bangui")

# What’s going on?
plot(bangui[1], reset=F, main="Geocoding mismatch")
plot(afr_sf[1], add=T)
plot(bangui[1], col=2, pch=20, add=T)
plot(st_point(c(18.558,4.395)), col=3, pch=20, add=T)
# I googled the coordinates and got these...
plot(st_point(c(18.563,4.373)), col=4, pch=20, add=T)

# Intersections on tibbles and data frames
# Let's try the previous operation using st_intersection()
# st_intersection (same as inner join)
afr_ctys_2 <- afr_ctys %>% st_intersection(afr_sf)
# kill disagreements
afr_ctys_2 <- afr_ctys_2 %>% filter(iso3v10!=adm0_a3)
# compare to st_join after filtering
nrow(afr_ctys_1) == nrow(afr_ctys_2)

# Creating grids
# back to degrees
africa_sf <- africa_sf %>% st_transform(4326)
# make grid of 4° cells
grid_sf <- africa_sf %>% st_make_grid(cellsize = 4,
                                      offset = c(-18,-35),
                                      what="polygons")
# add IDs to grid, make sf
grid_sf <- st_sf(id = 1:length(grid_sf),
                 geometry = grid_sf)

plot(africa_sf[1], col=NA, main = "Africa Gridded", axes=T, reset=F)
plot(grid_sf[1], col=NA, border=3, lwd=1, add=T)


# Now create in meters instead of degrees
africa_sf <- africa_sf %>% st_transform("+proj=moll")
# make grid of 100km cells
grid_sf <- africa_sf %>% st_make_grid(cellsize = 100000,
                                      what="polygons")
# add IDs to grid, make sf
grid_sf <- st_sf(id = 1:length(grid_sf),
                 geometry = grid_sf)

plot(africa_sf[1], col=NA, main = "Africa Gridded", axes=T, reset=F)
plot(grid_sf[1], col=NA, border=3, lwd=1, add=T)


#






















# End Code