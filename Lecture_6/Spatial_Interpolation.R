# Code for Lecture on Spatial Interpolation
# Created: 10/08/19
# Last Modified: 10/13/21
# Author: Noel Johnson
# This code is based on that used to reproduce the Appendix figure of spatially imputed mortality rates for Jedwab, Johnson, and Koyama (2019) "Negative Shocks and Mass Persecutions".

library(raster)
library(sf)
library(tidyverse)
library(tmap)
library(skimr)
library(viridis)

# Set your working directory
setwd("/Users/noeljohnson/Dropbox/Teaching/Spatial Fall2021/Spatial_Final_Lectures/Lecture_6")

# Load data
## Load in City Data and Project
### Three data sets: (1) Mort274.csv, (2) cities_for_map1801.csv, (3) ModernCountries/Modern Europe Projected.shp

# Assign projection you'll use to a vairable 
EEC <- "+proj=eqdc +lat_0=0 +lon_0=0 +lat_1=43 +lat_2=62 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs"

# Load in the .csv for the 274 mortality cities
Mort_274 <- read.csv("Data/Mort274.csv")

# Make the tibble spatial
Mort_274_spatial <- st_as_sf(Mort_274, coords = c("longitude", "latitude"), crs = 4326)

# Project it
Mort_274_spatial_proj <- st_transform(Mort_274_spatial, EEC)

# Make a simple plot
plot(Mort_274_spatial_proj["mortality"], reset = FALSE)


# Load in the 1801 cites data
Cities_1801 <- read.csv("Data/cities_for_map1801.csv")

# Make the tibble spatial
Cities_1801_spatial <- st_as_sf(Cities_1801, coords = c("longitude", "latitude"), crs = 4326)

# Project it
Cities_1801_spatial_proj <- st_transform(Cities_1801_spatial, EEC)

# Take a look
glimpse(Cities_1801_spatial_proj)

# Make a simple plot
plot(Cities_1801_spatial_proj[3], reset = T)


# Load in the modern country borders
modern_borders <- st_read("Data/ModernCountries/Modern Europe Projected.shp")

# Project the .shp file
modern_borders_proj <- st_transform(modern_borders, EEC)

# Check the dimensions of the .shp file so you can clip it
st_geometry(modern_borders_proj)
# Original Extent of modern countries c(xmin=-2639987, xmax=1655719, ymin=3243609, ymax=8097861))

# Clip it
modern_borders_clip <- st_crop(modern_borders_proj, c(xmin=-1100000, xmax=1655719, ymin=3243609, ymax=6900000))
plot(modern_borders_clip["name"], col=NA)

# Make a nicer plot of mortality cities with modern country borders using tmap (based on ggplot but for maps)
tm_shape(modern_borders_clip) + tm_polygons() +
  tm_shape(Mort_274_spatial_proj) +
  tm_dots(col="mortality", palette = "viridis", midpoint = FALSE,
          title="Mortality Rates", size=0.3) +
  #tm_text("mortality", just="left", xmod=.5, size = 0.7) +
  tm_legend(legend.outside=FALSE, legend.position = c("left", "center"))

# Make a plot of 1801 cities with modern country borders using tmap
tm_shape(modern_borders_clip) + tm_polygons() +
  tm_shape(Cities_1801_spatial_proj) +
  tm_dots(col=NA, palette = "viridis", midpoint = FALSE,
          title="All Cities", size=0.1) +
  #tm_text("mortality", just="left", xmod=.5, size = 0.7) +
  tm_legend(legend.outside=FALSE, legend.position = c("left", "center"))


# Calculate optimal IDW power
library(gstat) # Use gstat's idw routine
library(sp)    # Used for the spsample function

# Make a bounding box around modern_borders_clip
bbox <- st_make_grid(modern_borders_clip, n=1)
bbox
plot(modern_borders_clip["name"], col=NA, reset = FALSE)
plot(bbox, add = TRUE)

# Transform the 3 shape files from sf to sp objects
modern_borders_clip_sp = as(modern_borders_clip, Class = "Spatial") # Convert from sf to sp
bbox_sp = as(bbox, Class = "Spatial") # Convert from sf to sp
Mort_274_spatial_proj_sp = as(Mort_274_spatial_proj, Class = "Spatial") # Convert from sf to sp

# Create an empty grid where n is the total number of cells
grd              <- as.data.frame(spsample(bbox_sp, "regular", n=50000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object

# Add modern_borders_clip_sp projection information to the empty grid
proj4string(grd) <- proj4string(modern_borders_clip_sp)

plot(modern_borders_clip_sp, reset = TRUE)
plot(grd, add = TRUE)


# Interpolate the grid cells using the optimal power of 1.76 (idp=1.76)
mortality_274_idw <- gstat::idw(mortality ~ 1, Mort_274_spatial_proj_sp, newdata=grd, idp=1.76)


# Convert to raster object then clip
mort_274_raster       <- raster(mortality_274_idw)
mort_274_raster_clip     <- mask(mort_274_raster, modern_borders_clip_sp)

# Plot
tm_shape(mort_274_raster_clip) + 
  tm_raster(n=10,palette = "viridis", 
            title="Pred. Mortality") + 
  tm_shape(modern_borders_clip_sp) + tm_dots(NA) +
  tm_legend(legend.outside=FALSE, legend.position = c("left", "center"))

## Fine-tuning the interpolation

# Leave-one-out validation routine
IDW.out <- vector(length = length(Mort_274_spatial_proj_sp))
for (i in 1:length(Mort_274_spatial_proj_sp)) {
  IDW.out[i] <- idw(mortality ~ 1, Mort_274_spatial_proj_sp[-i,], Mort_274_spatial_proj_sp[i,], idp=1.76)$var1.pred
}

# Plot the differences
OP <- par(pty="s", mar=c(4,3,1,1))
plot(IDW.out ~ Mort_274_spatial_proj_sp$mortality, asp=1, xlab="Observed", ylab="Predicted", pch=16,
     col=rgb(0,0,0,0.5))
abline(lm(IDW.out ~ Mort_274_spatial_proj_sp$mortality), col="red", lw=2,lty=2)
abline(0,1)
par(OP)



model1 <- lm(IDW.out ~ Mort_274_spatial_proj_sp$mortality)
summary(model1)

# Compute RMSE
sqrt( sum((IDW.out - Mort_274_spatial_proj_sp$mortality)^2) / length(Mort_274_spatial_proj_sp))

## Cross-validation 1

# Estimate the optimal power ("idp") using a cross-validation technique

# Leave-one-out validation routine
step <- 10
res <- 3
IDW.out <- vector(length = length(Mort_274_spatial_proj_sp))
RMSE.out <- vector(length = step)
idw.power <- vector(length = step)

for (j in 1:step) {
  for (i in 1:length(Mort_274_spatial_proj_sp)) {
    IDW.out[i] <- idw(mortality ~ 1, Mort_274_spatial_proj_sp[-i,], Mort_274_spatial_proj_sp[i,], idp=j/res)$var1.pred
  }
  RMSE.out[j] <-  sqrt( sum((IDW.out - Mort_274_spatial_proj_sp$mortality)^2) / length(Mort_274_spatial_proj_sp))
  idw.power[j] <- j/res
}

df <- data.frame(idw.power, RMSE.out)
idw.power.name <- "power"
RMSE.name <- "RMSE"
names(df) <- c(idw.power.name, RMSE.name)  

optimal <- df[which.min(df$RMSE),]
optimal

gg <- ggplot(df, aes(x=power, y=RMSE)) + 
  geom_point() + 
  #geom_smooth(method="loess", se=F) + 
  labs(subtitle="Power vs RMSE", 
       y="RMSE", 
       x="Power")
gg


# End Code




















