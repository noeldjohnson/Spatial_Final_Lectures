# Code for Spatial Econometrics: Lecture 7 "Market Access"
# 11/6/19
# Author: Noel D. Johnson

library(tidyverse)
library(sf)
library(dodgr)
library(stargazer)
library(units)

setwd("/Users/noeljohnson/Dropbox/Teaching/Spatial_Fall_2019/Lectures/") 

xy <- rbind (c( -77.301188, 38.831429), # carow
             c( -77.101535, 38.885626)) # mercatus
xy <- data.frame(lon = xy[, 1], lat = xy[, 2])
# retrieves the OSM network
haj <- dodgr_streetnet(pts = xy,
                       expand = 0.1, quiet = F) %>%
  select(highway, osm_id)
# turns it into a graph
haj.graph <- weight_streetnet(haj,
                              wt_profile = "motorcar")
# computes the weighted distance on the graph
test <- dodgr_dists(haj.graph, from = xy[1,], to = xy[2,])

plot(haj[3], reset=T)
box <- st_bbox(haj)

f <- dodgr_flows_aggregate(haj.graph, from = xy[1,], to = xy[2,], flows=1, contract= T)
dodgr_flowmap(merge_directed_flows(f), linescale=5, bbox=box)
plot(haj[3], add=T)


# A different problem...

city <- dodgr_streetnet("paris france") %>%
  select(highway, osm_id)
net <- weight_streetnet(city, wt_profile = "foot")
nodes <- dodgr_vertices(net) # get nodes of vertices of network
pts <- sample(nodes$id, size = 100) # sample 100 random nodes for plotting
d <- dodgr_dists(net, from = pts, to = pts)

fmat <- matrix(runif(100*100), nrow = 100)
f <- dodgr_flows_aggregate(net, from = pts, to = pts, flows=fmat, contract= T)

dodgr_flowmap(merge_directed_flows(f), linescale=5)
plot(city[3], add=T)


#












# End Code








# dodgr_route_sf <- function(graph, from, to, snapdist = 1, ...) {
#   
#   # sanity checks  
#   if (!inherits(from, "sf")) stop('from object is not of type sf')
#   if (!inherits(to, "sf")) stop('to object is not of type sf')
#   if (!inherits(st_geometry(from), "sfc_POINT")) stop("from object is not of type sf points")
#   if (!inherits(st_geometry(to), "sfc_POINT")) stop("from object is not of type sf points")
#   if (nrow(from) > 1) stop('from object has more than one row')
#   
#   ## snap points to net within some snap distance
#   
#   ## snap to distance
#   graph.small <- unique(graph[, c("from_id", "from_lon", "from_lat")])
#   graph.small.sf <- st_as_sf(graph.small, coords = c("from_lon", "from_lat"), crs=4326)
#   distvec <- st_distance(graph.small.sf, from)
#   fvertice <- graph.small[which(distvec == min(distvec)), "from_id"]
#   
#   ## now for to coordinates
#   graph.small <- unique(graph[, c("to_id", "to_lon", "to_lat")])
#   graph.small.sf <- st_as_sf(graph.small, coords = c("to_lon", "to_lat"), crs=4326)
#   distvec <- t(set_units(st_distance(graph.small.sf, to), NULL))
#   mins <- do.call(pmin, as.data.frame(distvec))
#   ind <- (mins < snapdist)
#   if (sum(ind) == 0) stop("no routing was found")  
#   mins <- mins[ind]
#   svertices <- graph.small[which(distvec ==mins, arr.ind = T), "to_id"]
#   to <- to[ind, ]
#   
#   ## read the direct paths of the network
#   n <- nrow(to)
#   
#   # from point to xy data
#   from <- as.data.frame(st_coordinates(from))
#   colnames(from) <- c("from_lon", "from_lat")
#   to <- as.data.frame(st_coordinates(to))
#   colnames(to) <- c("to_lon", "to_lat")
#   
#   # extract the relevant lines
#   out <- do.call(rbind,
#                  lapply(seq(n), function(i) {
#                    
#                    dodgr_subnet <- graph[graph$from_id==fvertice & graph$to_id==svertices[i],]
#                    dodgr_subnet <- subset(dodgr_subnet, !duplicated(subset(dodgr_subnet, select=c(from_id, to_id))))
#                    
#                    #print(nrow(dodgr_subnet))
#                    if (nrow(dodgr_subnet)==0) return(NULL)
#                    a.sf <- as.numeric(dodgr_subnet[c("from_lon","from_lat")])
#                    b.sf <- as.numeric(dodgr_subnet[c("to_lon","to_lat")])
#                    out_sfc <- st_sfc(st_linestring(rbind(a.sf, b.sf)), crs = 4326)
#                    
#                    out_line_sf <- st_sf(from_id = fvertice, from, to_id = svertices[i], to[i,], type="a", geometry=out_sfc, stringsAsFactors=F)
#                    return(out_line_sf)
#                  })
#   )
#   
#   # get all paths
#   fullpaths <- dodgr_paths(graph = graph, from = from, to = to,  vertices=F, ...)[[1]]
#   
#   # get only completed paths for sanity checking
#   suppressWarnings(paths <- lapply(fullpaths, function(x) x[!is.na(x)]))
#   paths <- paths[lapply(paths,length)>0]
#   n <- length(paths)
#   
#   if (n < 1 & is.null(out) == T) stop("no routing was found")  
#   if (n < 1) return(out)
#   
#   # now cycle trough the whole thing to keep correct order of ids
#   n <- length(fullpaths)
#   
#   # extract the relevant lines
#   out_dodgr <- do.call(rbind,
#                        lapply(seq(n), function(i) {
#                          
#                          # skip out if nothing is in the path
#                          if (length(fullpaths[[i]]) == 0) return(NULL)
#                          if (sum(is.na(fullpaths[[i]])) > 0)  return(NULL)
#                          
#                          dodgr_subnet <- graph[as.vector(unlist(fullpaths[[i]])), ]
#                          #print(nrow(dodgr_subnet))
#                          if (nrow(dodgr_subnet) == 1) {
#                            a.sf <- as.numeric(dodgr_subnet[c("from_lon","from_lat")])
#                            b.sf <- as.numeric(dodgr_subnet[c("to_lon","to_lat")])
#                            out_sfc <- st_sfc(st_linestring(rbind(as.numeric(from), a.sf, b.sf)), crs = 4326)
#                          } else {
#                            out_sfc <- dodgr_to_sfc(dodgr_subnet)$geoms
#                          }
#                          out_line_sf <- st_sf(from_id = fvertice, from, to_id = svertices[i], to[i,], type="b", geometry=out_sfc, stringsAsFactors=F)
#                          return(out_line_sf)
#                        })
#   )
#   s.index <- paste0(out$from_id, out$to_id) %in% paste0(out_dodgr$from_id, out_dodgr$to_id)
#   out <- out[!s.index,]
#   out <- rbind(out,out_dodgr)
#   out <- out[order(out$from_id, out$to_id), ]
#   names(out$geometry) <- 1:nrow(out)
#   return(out)
# }
# 
# xy_sf <- st_as_sf(xy, coords = c("lon", "lat"), crs = 4326)
# 
# plot(haj[3], reset=F)
# plot(xy_sf, col="Red", add=T)
# 
# route <- dodgr_route_sf(haj.graph, from = xy_sf[1,], to = xy_sf[2,], snapdist=1)







# End Code