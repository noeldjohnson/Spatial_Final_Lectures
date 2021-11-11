weight_streetnet <- 
  function (sf_lines, wt_profile = "bicycle", type_col = "highway", 
            id_col = "osm_id", keep_cols = NULL) 
  {
    if (!is(sf_lines, "sf")) 
      stop("sf_lines must be class \"sf\"")
    if (!"geometry" %in% names(sf_lines)) 
      stop(paste0("sf_lines must be class \"sf\" and have a geometry column"))
    if (type_col != "highway") 
      names(sf_lines)[which(names(sf_lines) == type_col)] <- "highway"
    if (id_col != "osm_id") 
      names(sf_lines)[which(names(sf_lines) == id_col)] <- "osm_id"
    if (!"highway" %in% names(sf_lines)) 
      stop("Please specify type_col to be used for weighting streetnet")
    if (!"osm_id" %in% names(sf_lines)) 
      stop("Please specifiy id_col to be used to identify streetnet rows")
    if (is.null(names(sf_lines$geometry))) 
      names(sf_lines$geometry) <- sf_lines$osm_id
    if (is.character(wt_profile)) {
      prf_names <- c("foot", "horse", "wheelchair", "bicycle", 
                     "moped", "motorcycle", "motorcar", "goods", "hgv", 
                     "psv")
      wt_profile <- match.arg(tolower(wt_profile), prf_names)
      profiles <- dodgr::weighting_profiles
      wt_profile <- profiles[profiles$name == wt_profile, ]
    }
    else if (is.numeric(wt_profile)) {
      nms <- names(wt_profile)
      if (is.null(nms)) 
        nms <- NA
      wt_profile <- data.frame(name = "custom", way = nms, 
                               value = wt_profile, stringsAsFactors = FALSE)
    }
    else if (is.data.frame(wt_profile)) {
      if (ncol(wt_profile) != 3 | !identical(names(wt_profile), 
                                             c("name", "way", "value"))) 
        stop("Weighting profiles must have three columsn of ", 
             "(name, way, value); see 'weighting_profiles' for examples")
    }
    else stop("Custom named profiles must be vectors with named values")
    if (nrow(wt_profile) > 1 & all(wt_profile$name != "custom")) 
      sf_lines <- dodgr:::remap_way_types(sf_lines, wt_profile)
    dat <- dodgr:::rcpp_sf_as_network(sf_lines, pr = wt_profile)
    graph <- data.frame(geom_num = dat$numeric_values[, 1] + 
                          1, edge_id = seq(nrow(dat$character_values)), from_id = as.character(dat$character_values[, 
                                                                                                                    1]), from_lon = dat$numeric_values[, 2], from_lat = dat$numeric_values[, 
                                                                                                                                                                                           3], to_id = as.character(dat$character_values[, 2]), 
                        to_lon = dat$numeric_values[, 4], to_lat = dat$numeric_values[, 
                                                                                      5], d = dat$numeric_values[, 6], d_weighted = dat$numeric_values[, 
                                                                                                                                                       7], highway = as.character(dat$character_values[, 
                                                                                                                                                                                                       3]), way_id = as.character(dat$character_values[, 
                                                                                                                                                                                                                                                       4]), stringsAsFactors = FALSE)
    graph$d_weighted[graph$d_weighted < 0] <- max(graph$d_weighted) * 
      1000000
    if (all(graph$highway == "")) 
      graph$highway <- NULL
    if (all(graph$way_id == "")) 
      graph$way_id <- NULL
    if (is.null(rownames(as.matrix(sf_lines$geometry[[1]])))) {
      xy <- data.frame(x = c(graph$from_lon, graph$to_lon), 
                       y = c(graph$from_lat, graph$to_lat))

      indx <- which(!duplicated(apply(xy, 2,  as.character)))
      xy_indx <- xy[indx, ]
      xy_indx$indx <- seq(nrow(xy_indx))
      xy_from <- data.frame(x = graph$from_lon, y = graph$from_lat, 
                            ord = seq(nrow(graph)))
      xy_from <- merge(xy_from, xy_indx)
      graph$from_id <- as.character(xy_from$indx[order(xy_from$ord)])
      xy_to <- data.frame(x = graph$to_lon, y = graph$to_lat, 
                          ord = seq(nrow(graph)))
      xy_to <- merge(xy_to, xy_indx)
      graph$to_id <- as.character(xy_to$indx[order(xy_to$ord)])
    }
    class(graph) <- c(class(graph), "dodgr_streetnet")
    graph <- dodgr::dodgr_components(graph)
    if (length(keep_cols) > 0) {
      keep_names <- NULL
      if (is.character(keep_cols)) {
        keep_names <- keep_cols
        keep_cols <- match(keep_cols, names(sf_lines))
      }
      else if (is.numeric(keep_cols)) {
        keep_names <- names(sf_lines)[keep_cols]
      }
      else {
        stop("keep_cols must be either character or numeric")
      }
      indx <- match(graph$geom_num, seq(sf_lines$geometry))
      for (k in seq(keep_names)) graph[[keep_names[k]]] <- sf_lines[indx, 
                                                                    keep_cols[k], drop = TRUE]
    }
    return(graph)
  }
