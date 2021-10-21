# Code for Spatial Econometrics: "Alesina et al. (2016) Afghanistan Replication"
# Created: 10-28-19
# Last Updated: 11-20-21
# Author: Noel Johnson
# This code is based on that of Richard Bluhm.

library(raster)
library(sf)
library(tidyverse)
library(units)
library(reldist)
library(stargazer)
library(ncdf4) # package for netcdf manipulation

setwd("/Users/noeljohnson/Dropbox/Teaching/Spatial Fall2021/Lecture 7/")

# Voronoi Diagrams---These can be useful. (1) You want "random" variation in a border you already have. (2) You want a "natural" unit to use in your analysis.

# Get file
africa_sf <- st_read("./africa_scale.shp", quiet = T)
# Transform to an equal area projection
africa_sf <- st_transform(africa_sf, "+proj=aeqd")
# Snapping the object to itself by 1m kills slivers # Obtain union for outlines
second_sf <- africa_sf %>%
  st_snap(africa_sf, tolerance = 1) %>% group_by(continent) %>% summarize()

#We will create the diagram based on the centroid of each country. Careful, st_voronoi() requires one single combined input feature and also returns a single feature.

# Here's a full workflow...
africa_vo_sf <- africa_sf %>%
  st_centroid() %>% st_combine() %>%
  st_voronoi() %>% st_cast() %>%
  st_sf() %>% st_intersection(second_sf) %>%
  st_transform(4326)
africa_sf <- africa_sf %>% st_transform(4326)

# Here's a plot of the end result...
par(mfrow = c(1, 2))
plot(africa_sf[1], col=NA, key.pos = NULL, reset=F)
plot(africa_vo_sf[1], col=NA, key.pos = NULL, reset=F)
par(1, 1)

# This looks similar to the more complicated code above)...
africa_vo_sf <- africa_sf %>%
  st_centroid() %>%
  st_voronoi() %>%
  st_sf() %>% st_intersection(second_sf) %>%
  st_transform(4326)
plot(africa_vo_sf[1], key.pos = NULL, reset=T)

# Open the GREG map of ethnic groups (./greg_afg/GREG_Afghanistan_Box.shp) and the Digital Chart of the World (/vmap0_boundaries/world-bounds-a.shp). Make sure to set the CRS to EPSG 4326. Subset the latter to Afghanistan.

greg.sf <- st_read("Alesina_2016_rep/greg_afg/GREG_Afghanistan_Box.shp",
                   stringsAsFactors=F, quiet=T, crs = 4326)
dcw.sf <- st_read("Alesina_2016_rep/vmap0_boundaries/world-bounds-a.shp",
                  stringsAsFactors=F, quiet=T, crs = 4326)

# Intersect the GREG polygons with the DCW map of Afghanistan. Compute the area of each polygon and delete those smaller than 1 km2. Keep only the variables GREGID, GREGNAME and a new variable for the country name, called COUNTRY.

glimpse(dcw.sf)

dcw.sf <- dcw.sf %>% filter(NA2_DESCRI=="Afghanistan")

dcw.sf <- dcw.sf %>% filter(NA2 == "AF")
greg.dcw.sf <- greg.sf %>% st_intersection(dcw.sf)
plot(greg.dcw.sf[2])

table(greg.dcw.sf$GREGNAME)

sum(table(greg.dcw.sf$GREGNAME))

# identify small pieces: none here
greg.dcw.sf <- greg.dcw.sf %>% mutate(poly_area = st_area(.)) %>%
  mutate(poly_area = set_units(poly_area, km^2)) %>%
  filter(poly_area > set_units(1, km^2)) %>%
  select(GREGID, GREGNAME, COUNTRY = NA2_DESCRI, poly_area)

# How many ethnic groups are there in Afghanistan? Does this match Alesina et al. (2016)? Plot the result to achieve replication goal 1. To get a unique color for each group, you might want to specify plot(..., pal= sf.colors(x)) where x is the number of groups.

plot(greg.dcw.sf["GREGNAME"],
     main="Ethnic Homelands in Afghanistan",
     pal= sf.colors(31, categorical = F),
     key.pos=2, key.length = 1, key.width = lcm(6))

# Load the F142000 and F152000 light rasters from the folder ntl_2000. Crop both rasters to the spatial extent of the GREG-DCW intersection you computed above. Create a new raster which contains the average of the two rasters. Divide this new raster by the area of each cell and check what the new maximum is using cellStats().

# Areas with zero cloud-free observations are represented by the value 255,
# but there are none after clipping
r.light1 <- raster("Alesina_2016_rep/ntl_2000/F142000.v4b_web.stable_lights.avg_vis.tif")
r.light2 <- raster("Alesina_2016_rep/ntl_2000/F152000.v4b_web.stable_lights.avg_vis.tif")
greg.dcw.sp <- greg.dcw.sf %>% as("Spatial")
r.light1 <- crop(r.light1, greg.dcw.sp)
r.light2 <- crop(r.light2, greg.dcw.sp)
r.light <- mean(r.light1, r.light2)
r.light <- r.light/area(r.light)
cellStats(r.light, "max")

# Use raster::extract() to compute the average light density in each ethnic homeland. Call the new variable light and merge it back to the GREG-DCW feature.

light.df <- raster::extract(r.light, greg.dcw.sp, fun=mean, na.rm=T, df=T)
light.df <- light.df %>% rename(light = 2)
greg.dcw.sf <- greg.dcw.sf %>% mutate(ID = 1:nrow(.)) %>% left_join(light.df, by = "ID")

# Open the Gridded Population of the World (GPW) v3 population density raster for the year 2000 (./gpw3_pdens_2000/glds00g.bil) and set the CRS to match the GREG-DCW features. Then extract the population density in each ethnic homeland, call this new variable pop and merge back to the GREG-DCW feature.

r.pop <- raster("/Users/noeljohnson/Dropbox/Teaching/Spatial Fall2021/Spatial_Final_Lectures/Lecture 7/Alesina_2016_rep/gpw3_pdens_2000/glds00g.bil")
# ds00g population densities in 2000, unadjusted, persons per square km
crs(r.pop)=crs(greg.dcw.sf)
crs(r.pop)
crs(greg.dcw.sf)

pop.df <- raster::extract(r.pop, greg.dcw.sp, fun=mean, na.rm=T, df=T)
pop.df <- pop.df %>% rename(pop = 2)
greg.dcw.sf <- greg.dcw.sf %>% left_join(pop.df, by = "ID")

# Create a new variable called light_pc by dividing the light density by the population density. Plot this variable on a map using plot(..., breaks="kmeans"). Use the gini()-function to compute the unweighted and population-weighted Gini coefficient of light per capita. Which Gini coefficient is more meaningful in this context? How do your results compare to Alesina et al. (2016)? You have reached replication goal 2.

greg.dcw.sf <- greg.dcw.sf %>% mutate(light_pc = light/pop)
greg.dcw.sf %>% select(light, pop)

summary(greg.dcw.sf["pop"])

summary(greg.dcw.sf["light_pc"])

plot(greg.dcw.sf["light_pc"] , breaks="kmeans")

gini(greg.dcw.sf[["light"]])

gini(greg.dcw.sf[["light_pc"]], w = greg.dcw.sf[["pop"]])

# Create a grid of 2.5° cells starting from Pmin = (−180,−65) in the lower left corner. Intersect this grid with the DCW for Afghanistan and plot the result to verify it them.

# grid
grid_sf <- dcw.sf %>% st_make_grid(cellsize = 2.5, offset= c(-180,-65),
                                   what="polygons")
# add IDs to grid, make sf
grid_sf <- st_sf(cid = 1:length(grid_sf),
                 geometry = grid_sf, crs = 4326)
# intersection
grid.afg.sf <- grid_sf %>% st_intersection(dcw.sf)
# plot
plot(grid.afg.sf[1])

# Extract the light and population densities in each grid cell and add them back to the original feature. Calculate light per capita, plot the result and compute the unweighted and weighted Gini coefficients. Compare your results to Alesina et al. (2016). You have reached replication goal 3.

grid.afg.sp <- grid.afg.sf %>% as("Spatial")
# extract light
light.df <- raster::extract(r.light, grid.afg.sp, fun=mean, na.rm=T, df=T)
light.df <- light.df %>% rename(light = 2)
grid.afg.sf <- grid.afg.sf %>% mutate(ID = 1:nrow(.)) %>% left_join(light.df, by = "ID")
# extract pop
pop.df <- raster::extract(r.pop, grid.afg.sp, fun=mean, na.rm=T, df=T)
pop.df <- pop.df %>% rename(pop = 2)
grid.afg.sf <- grid.afg.sf %>% left_join(pop.df, by = "ID")
grid.afg.sf <- grid.afg.sf %>% mutate(light_pc = light/pop)
summary(grid.afg.sf["light_pc"])
plot(grid.afg.sf["light_pc"] , breaks="kmeans")
gini(grid.afg.sf[["light_pc"]])
gini(grid.afg.sf[["light_pc"]], w = grid.afg.sf[["pop"]])

# Project the GREG-DCW Afghanistan feature into the Azimuthal Equidistant Projection and then union the polygons to obtain the full polygon of Afghanistan again. Create a Voronoi diagram on the basis of the projected GREG-DCW feature, that is, compute the centroids, combine the features, call the st_voronoi-function, cast the result into single polygons, use st_sf() to merge back the original data and IDs, intersect this with the full polygon of Afghanistan, and finally transform the result back to EPSG 4326. How many Thiessen homelands do you have? Plot the result to verify.

# data should be projected, distances matter. delete stuff we have computed before
greg.vor.sf <- greg.dcw.sf %>%
  select(-light, -pop, -light_pc, -poly_area) %>%
  st_transform("+proj=aeqd")
dcw.bounds.sf <- greg.vor.sf %>% st_union()
# a full workflow
greg.vor.sf <- greg.vor.sf %>%
  st_centroid() %>% st_combine() %>%
  st_voronoi() %>% st_cast() %>% st_sf(greg.vor.sf, geometry=.) %>%
  st_intersection(dcw.bounds.sf) %>% st_transform(4326)
table(greg.vor.sf$GREGNAME)

plot(greg.vor.sf["GREGNAME"], main="Ethnic Homelands in Afghanistan",
     pal= sf.colors(31, categorical = F), key.pos=2, key.length = 1, key.width = lcm(6))

# Extract the light and population densities in each Thiessen homeland and add them back to the original feature. Calculate light per capita, plot the result and compute the unweighted and weighted Gini coefficients. Compare your results to Alesina et al. (2016). You have reached replication goal 4.

# extract light and delete the data we have previously computed and are still tagging along
greg.vor.sp <- greg.vor.sf %>% as("Spatial")
light.df <- raster::extract(r.light, greg.vor.sp, fun=mean, na.rm=T, df=T)
light.df <- light.df %>% rename(light = 2)
greg.vor.sf <- greg.vor.sf %>% mutate(ID = 1:nrow(.)) %>% left_join(light.df, by = "ID")
# extract pop
pop.df <- raster::extract(r.pop, greg.vor.sp, fun=mean, na.rm=T, df=T)
pop.df <- pop.df %>% rename(pop = 2)
greg.vor.sf <- greg.vor.sf %>% left_join(pop.df, by = "ID")
# light pc, summarize and plot
greg.vor.sf <- greg.vor.sf %>% mutate(light_pc = light/pop)
summary(greg.vor.sf["light_pc"])

plot(greg.vor.sf["light_pc"] , breaks="kmeans")

gini(greg.vor.sf[["light_pc"]])

gini(greg.vor.sf[["light_pc"]], w = greg.dcw.sf[["pop"]])

# Load the Database of Global Administrative Areas (GADM) v3.6 and try to independently reach replication goal 5. Compare your results to Alesina et al. (2016).

# get data and intersect
gadm.sf <- st_read("Alesina_2016_rep/gamd_36_afg/AFG_adm1.shp", stringsAsFactors=F) %>% st_set_crs(4326)

gadm.dcw.sf <- gadm.sf %>% st_intersection(dcw.sf)
gadm.sp <- gadm.sf %>% as("Spatial")
# extract light
light.df <- raster::extract(r.light, gadm.sp, fun=mean, na.rm=T, df=T)
light.df <- light.df %>% rename(light = 2)
gadm.sf <- gadm.sf %>% mutate(ID = 1:nrow(.)) %>% left_join(light.df, by = "ID")
# extract pop
pop.df <- raster::extract(r.pop, gadm.sp, fun=mean, na.rm=T, df=T)
pop.df <- pop.df %>% rename(pop = 2)
gadm.sf <- gadm.sf %>% left_join(pop.df, by = "ID")
# light pc, summarize and plot
gadm.sf <- gadm.sf %>% mutate(light_pc = light/pop)
summary(gadm.sf["light_pc"])

plot(gadm.sf["light_pc"] , breaks="kmeans")

# benchmark 0.76
gini(gadm.sf[["light_pc"]])

gini(gadm.sf[["light_pc"]], w = gadm.sf[["pop"]])

#



# Scratch Code

r.pop <- raster("/Users/noeljohnson/Dropbox/Teaching/Spatial Fall2021/Lecture 7/Alesina_2016_rep/gpw-v4-population-density-adjusted-to-2015-unwpp-country-totals-rev11_totpop_2pt5_min_nc/gpw_v4_population_density_adjusted_rev11_2pt5_min.nc")
# ds00g population densities in 2000, unadjusted, persons per square km
crs(r.pop)=crs(greg.dcw.sf)
crs(r.pop)
crs(greg.dcw.sf)















# End Code