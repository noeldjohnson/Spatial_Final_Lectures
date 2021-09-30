# Code for Spatial Econometrics: Lecture 5 "Geocoding, georeferencing and digitizing"
# Created: 9-24-19
# Last Edited: 9-29-21
# Author: Noel Johnson

library(tidyverse)
library(sf)
library(stargazer)

# Three methods...

# (1) Geocoding: Turning a name of some geographical feature into coordinates (or the reverse). ==> This can be done in R

# (2) Georeferencing: Taking an image of some place and giving it a known reference system. ==> QGIS or ARC

# (3) Digitizing: Taking a georeferenced image and extracting the vector features in some way. ==> QGIS or ARC

##################################################

# (1) Geocoding: See slides...

# An example using tmap’s geocode_OSM

library(tmaptools)

moshi <- geocode_OSM("Moshi, Tanzania", as.sf = T,
            details = F) %>% glimpse()

us_cities <- geocode_OSM(c("Bethesda, Maryland, USA", "Fairfax, VA, USA"), as.sf = T, details = F)

# (2) Georeferencing: See slides...

# (3) Digitizing: See slides...

#







# End Code