# Code for Spatial Econometrics: Lecture 4 "Rasters"
# Created: 9/17/19
# Last Updated: 9/13/21
# Author: Noel Johnson

library(raster)
library(rgdal)
library(tidyverse)
library(sf)
library(stargazer)


# Reading and writing rasters
# setwd("/Users/noeljohnson/Dropbox/Teaching/Spatial_Fall_2019/Lectures/")
r.potato_world <- raster("./data/GAEZ_potato_suit.tif")
# Small rasters go into RAM, the rest stay on disk
inMemory(r.potato_world)
r.potato_world
plot(r.potato_world, col = gray.colors(10, start = .1, end = .9))
quantile(r.potato_world)

# You can write a GeoTiff using..
#writeRaster(r.potato_world, "potato_new.tif")
# but you should consider setting the data type to save storage space
# (e.g. INT1S for integers from -127 to 127) --- YOU MUST BE CAREFUL ABOUT THIS 
# OR WILL LOSE DATA!!!

# Raster properties]
res(r.potato_world) # resolution
# ...is about 9km resolution at the equator
# More generally, Length of 1 degree of Longitude = cosine (latitude in decimal 
# degrees) * length of degree (111 km or 60 naut miles) at equator
extent(r.potato_world) %>% as.vector() # bounding box
dim(r.potato_world) # rows and columns
ncell(r.potato_world) == nrow(r.potato_world)*ncol(r.potato_world) # rectangle!

# Let's crop this bastard...
e <- extent(-12, 30, 33, 65)
r.potato <- crop(r.potato_world, e)
plot(r.potato, col = gray.colors(10, start = .1, end = .9))

e_zoom <- extent(0, 6, 45, 51)
zoom.potato <- crop(r.potato_world, e_zoom)
plot(zoom.potato, col = gray.colors(10, start = .1, end = .9))

# Raster options
# Processing rasters takes time, memory and hard disk. By default you do not see
# the progress of computations or know where intermediate files are saved. 
# rasterOptions() allows you to change this.
rasterOptions(tmpdir = "./mytemp/",
              progress = "text",
              timer = TRUE)
tmpDir()


# Raster subsetting
# Selecting by coordinates or extent
# Selecting by coordinates
id <- cellFromXY(r.potato, xy = c(3, 47))
r.potato[id]

# Selecting by extent (could also create a raster object)
r.box <- extent(2, 5, 45, 50) #xmin, xmax, ymin, ymax
r.potato[r.box] %>% glimpse()

# Summarizing raster objects; These functions run “fast” even on disk...

cellStats(r.potato, 'mean')
cellStats(r.potato, 'sd')
cellStats(r.potato, 'min')
cellStats(r.potato, 'max')

quantile(r.potato) # all data

hist(r.potato, freq=F)

# Aggregating and disaggregating
# Changing the raster resolution can save disk space and computational time, or 
# allow for the calculation of smaller areas.
fact_2 <- aggregate(r.potato, fact=2, fun=mean) # or sum etc
fact_10 <- aggregate(r.potato, fact=10, fun=mean)
fact_25 <- aggregate(r.potato, fact=25, fun=mean)

# Plot 3 resolutions
plot(fact_2, col = gray.colors(10, start = .1, end = .9))
plot(fact_10, col = gray.colors(10, start = .1, end = .9))
plot(fact_25, col = gray.colors(25, start = .1, end = .9))

# Projections and transformations
#Contrary to vector data, projecting raster data backwards and forwards is not 
# lossless

projection(r.potato)

# projecting involves resampling to a new resolution
r.potato.moll <- projectRaster(r.potato,
                            crs = "+proj=moll")
projection(r.potato.moll)
## [1] "+proj=moll +ellps=WGS84"
plot(r.potato.moll, col = gray.colors(25, start = .1, end = .9))

# Resampling (goto slides---important information)

#Map algebra divides raster operations into four subclasses:
# 1. Local or per-cell operations (e.g. dividing two rasters cell-by-cell)
# 2. Focal or neighborhood operations. Most often the output cell value is the 
# result of a 3 x 3 input cell block.
# 3. Zonal operations are similar to focal operations but any irregular size or 
# shape is the basis of the calculations. 
# 4. Global or per-raster operations are essentially summary stats.

# Local operations
# Raster algebra operations (+, -, /, etc) are local operations

r.potato.normal <- log(r.potato/area(r.potato) + 0.01)
plot(r.potato, main="DN", reset=T)
plot(r.potato.normal, main="Normalized", reset=T)

# Focal operations
# Focal operations are neighborhood statistics. You must supply a function 
# (the default is sum) and a weighting matrix.

r.foc <- focal(r.potato, fun = min,
               w = matrix(1, nrow = 5, ncol = 5))

plot(r.foc, col = gray.colors(25, start = .1, end = .9))

# Zonal operations
#Zonal statistics summarize the values of one raster layer by values (“zones”) 
# of another raster of the same extent. For large rasters, the function only 
# takes predefined functions in characters: “mean”, “sd”, “min”, “max”, “sum” 
# or “count”.

# Vector-raster interactions
# Three Main Techniques
# 1. Raster cropping and masking using vector data
# 2. Extracting raster values using vector data
# 3. Raster-vector conversion

# The raster package only deals with sp-objects and does not understand simple 
# features. You can easily coerce one to the other:
modern_borders <- st_read("./data/Modern Europe Projected.shp")
modern_borders <- st_transform(modern_borders, 4326)
modern_borders
plot(modern_borders[1], col=NA)
# convert back and forth sf <--> sp
modern_borders.sp <- as(modern_borders, "Spatial")
modern_borders.sf <- st_as_sf(modern_borders.sp)

# 1. Raster cropping and masking using vector data
# Filter France borders
names(modern_borders.sf)
france.sf <- modern_borders.sf %>% filter(CntryName == "France")
# Convert to sp
france.sp <- as(france.sf, "Spatial")
# Crop
potato.france.crop <- crop(r.potato, france.sp)
# Mask
potato.france.mask <- mask(r.potato, france.sp)

# Plot them both
par(mfrow = c(1, 2))
plot(potato.france.crop, col = gray.colors(25, start = .1, end = .9))
plot(potato.france.mask, col = gray.colors(25, start = .1, end = .9))

# 2. Extracting raster values using vector data
# Load in modern country boders
modern_borders <- st_read("./data/Modern Europe Projected.shp")
modern_borders <- st_transform(modern_borders, 4326)
modern_borders
plot(modern_borders[1], col=NA)

# now run extract by polygon
modern_borders$mean_pot_suit <- raster::extract(r.potato, modern_borders,
                                                fun=mean, na.rm=TRUE)

# 3. Raster-vector conversion
# Vectorization is the conversion of raster objects into their representation 
# in vector form. Two common conversions are raster to points and raster to 
# polygons. Here are two examples:

# kill all zeros
r.potato <- reclassify(r.potato, c(-Inf,0,NA))
# to points
v.potato <- rasterToPoints(r.potato, spatial = T) %>% st_as_sf()
plot(v.potato)

# Rasterization
# Rasterization is the conversion of vector objects into their representation 
# in raster objects.

#






# End Code