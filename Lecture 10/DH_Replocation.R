# Code to partially replicate Donaldson and Hornbeck QJE Railroads...
# Author: Noel Johnson based on code from Richard Bluhm
# Date Created: 11-3-21
# Last Modified: 11-8-21

library(tidyverse)
library(sf)
library(units)
library(dodgr)

setwd("/Users/noeljohnson/Dropbox/Teaching/Spatial Fall2021/Spatial_Final_Lectures/Lecture 10/Donaldson_2016_rep") 

# Open the county data (./dh_shapes/US_county_1890.shp).
# Subset the data to counties in Pennsylvania (via STATENAM).
# What projection are the data in? Get them into EPSG:4326.
# Do the same with the centroids (./dh_shapes/Centroids_1890.shp).
# Plot the counties with their centroids

ctys.pol.sf <- st_read("./dh_shapes/US_county_1890.shp", quiet=T) %>% 
  filter(STATENAM == "Pennsylvania") %>% st_transform(4326)
ctys.pts.sf <- st_read("./dh_shapes/Centroids_1890.shp", quiet=T) %>% 
  filter(STATENAM == "Pennsylvania") %>% st_transform(4326)
plot(ctys.pol.sf[1], col=NA, main="Counties in Pennsylvania", reset = F, axes=T)
plot(ctys.pts.sf[2], col=2, pch=20, add=T)

# DH (2016) create direct wagon routes between the county centroids.
# Use st_nearest_points() to find these routes, st_sf() to create a data frame,
# st_length() to find the line length, and then subset to all lines shorter 
# than 300 km. Be careful with unit comparisons. Plot the routes to verify 
# your computations. Unselect all variables and create a new one called 
# waytype which is set to "wagon" for all observations.

ctys.distlines <- st_nearest_points(ctys.pts.sf, ctys.pts.sf)
ctys.distlines <- ctys.distlines %>% st_sf() %>% mutate(length = st_length(.))
ctys.distlines <- ctys.distlines %>% mutate(length = set_units(length, km)) %>%
  filter(length < set_units(300, km) & length > set_units(0, km))
ctys.distlines <- ctys.distlines %>% select(-everything()) %>% mutate(waytype = "wagon")

plot(st_geometry(ctys.distlines), axes=T, main="Wagon routes", col="darkgrey")
plot(st_geometry(ctys.pts.sf), add=T, col=2, pch=20)

# Load the railway lines in 1870 (./dh_shapes/rail_1870.shp), unselect all
# variables and then add a new variable called waytype which is always set
# to "railway".

railways.sf <- st_read("./dh_shapes/rail_1870.shp", quiet=T) %>%
  st_transform(4326) %>% st_crop(ctys.pol.sf) %>% 
  select(-everything()) %>% mutate(waytype = "railway")

# DH (2016) connect every county’s centroid to the nearest railway line 
# via a wagon route (i.e. a straight line). Use st_nearest_feature() to 
# identify the nearest line and then st_nearest_points() to connect the 
# county centroids only to the nearest line (you’ll need to subset to the 
# nearest index and use pairwise=T). Turn it into a simple features data 
# frame and add a waytype variable which is always set to "railaccess". 
# Plot the railways in 1870 and your access routes

railaccess.sf <- st_nearest_points(ctys.pts.sf, st_combine(railways.sf)) %>%
  st_sf()
railaccess.sf <- railaccess.sf %>% mutate(waytype = "railaccess")

plot(st_geometry(railways.sf), col=1, axes=T,
     main="Railways in 1870 w/ access routes")
plot(st_geometry(ctys.pts.sf), add=T, col=2, pch=20)
plot(railaccess.sf, add=T, col=4)

# Use rbind() to tie all three components of the network together. 
# Now you need to make sure everything is really precisely connected. 
# Take the entire network and project it into a meaningful CRS with meters 
# as native units. Snap the vertices of the network to itself using a 
# tolerance of 50 m. You need to make sure that your centroids also exactly 
# fit to this slightly modified network, so project them as well and snap 
# them to the network.1 Transform both back to the geograpic projection 
# and give each element in the networka unique ID.

# bind
network.sf <- ctys.distlines %>% rbind(railways.sf) %>% rbind(railaccess.sf)

# snap the net and the centroids
network.sf <- network.sf %>% st_transform("+proj=laea")
network.sf <- network.sf %>% st_snap(network.sf, tolerance = 50)

# the next 3 lines are now optional, see footnote
ctys.pts.sf <- ctys.pts.sf  %>% st_transform("+proj=laea")
ctys.pts.sf <- ctys.pts.sf %>% st_snap(network.sf, tolerance = 50)
ctys.pts.sf <- ctys.pts.sf %>% st_transform(4326)

network.sf <- network.sf %>% st_transform(4326)

# each linestring should have an ID
network.sf <- network.sf %>% mutate(ID = 1:n())

# Make a table of the different way types in the network.
# Then use dodgr’s weight_streetnet() to convert the vectors into a network. 
# Provide your ID to id_col=, use type_col = "waytype" and set the weighting 
# profile to wt_profile=rep(1,3). Careful the last step does not create the 
# appropriate weights, we will modify them shortly. Examine your graph 
# version of the network, esp. the variable d_weighted.

# get the structure
table(network.sf %>% pull(waytype))

## Things fail from here. Boo

write_dodgr_wt_profile(file = "profile")

# create an equally weighted net

my.net <- weight_streetnet(network.sf, id_col = "ID",
                           wt_profile="horse",
                           wt_profile_file = "/Users/noeljohnson_laptop/Dropbox/Teaching/Spatial Fall2021/Lecture 9/Donaldson_2016_rep/profile.json",
                           type_col = "waytype",
                           )


head(my.net)

# Convert your county centroids to a data frame (use st_coordinates() first).
# Then use dodgr_dists() to compute the cost matrix τ. 
# You may set parallel = T to speed things up.
ctys.pts.xy <- as.data.frame(st_coordinates(ctys.pts.sf))
tau <- dodgr_dists(my.net, from = ctys.pts.xy, to = ctys.pts.xy,
                   quiet = T, parallel = TRUE)
tau[1:5, 1:5]



f <- dodgr_flows_aggregate(my.net, from = ctys.pts.xy[1,], to = ctys.pts.xy[21,],
                           flows=1, contract= T)
# the call "merge_directed_flows()" seems to no longer work. (12/14/2020)
# dodgr_flowmap(merge_directed_flows(f), linescale=5, bbox=box)
# plot(haj[3], add=T)
# these two lines do seem to work...
dodgr_flowmap(f, linescale=5)
plot(network.sf[2], add=T)

#
















# End Code