




# https://bookdown.org/nicohahn/making_maps_with_r5/docs/leaflet.html

# apply dbscan to the coordinates of the pharmacies
pharmacies_db <- dbscan(st_coordinates(pharmacies), eps = 0.008, minPts = 10)
# add a cluster variable
pharmacies$cluster <- pharmacies_db$cluster
# count the size of each cluster
pharmacies_count <- pharmacies %>%
  dplyr::group_by(cluster) %>%
  dplyr::mutate(count = n())
# split the pharmacies by cluster
pharmacies_split <- split(pharmacies_count, pharmacies_count$cluster)
# get the coordinates for all of the points that are in a cluster
pharmacies_split_coord <- lapply(pharmacies_split[2:length(pharmacies_split)], st_coordinates)
# compute which points lie on the convex hull of each cluster
pharmacies_split_chull <- lapply(pharmacies_split_coord, chull)
# keep only those points
pharmacies_outer <- lapply(seq_len(length(pharmacies_split_chull)), function(x, ...) {
  pharmacies_split_coord[[x]][pharmacies_split_chull[[x]], ]
})
# turn these points into a slightly buffered polygon
pharmacies_outer_sf <- lapply(pharmacies_outer, function(x) {
  # append the last point so that a polygon can be drawn
  x <- rbind(x, x[1, ])
  # turn the points into a polygon
  poly <- st_sfc(st_polygon(list(x))) %>%
    as.data.frame() %>%
    # set the crs system of the points
    st_as_sf(crs = 4326) %>%
    # transform the polygons
    st_transform(3035) %>%
    # buffer the polygons by 200 meters
    st_buffer(200) %>%
    # re-transorm the polygons
    st_transform(4326)
})

# bind the polygons together
clusters <- Reduce(rbind, pharmacies_outer_sf)
# set the count of points in no cluster to NA
pharmacies_count$count[pharmacies_count$cluster == 0] <- NA