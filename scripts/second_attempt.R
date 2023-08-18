# Second attempt (with sf)

# Setup ------------------------------------------------------------------

library(conflicted)
library(here)
library(tidyverse)
library(tidytransit)
library(igraph)
library(sf)
library(sftime)
library(tidygraph)
library(sfnetworks)
library(osmextract)

conflicts_prefer(
  dplyr::filter
)

# Data -------------------------------------------------------------------

tarc_file <- here("data", "tarc_gtfs.zip")

# tarc_feed <- "http://googletransit.ridetarc.org/feed/google_transit.zip"
# download.file(tarc_feed, destfile = tarc_file)

tarc_gtfs <- read_gtfs(tarc_file) %>%
  filter_feed_by_date("2023-08-30") %>%
  gtfs_as_sf()

graph_data <- tarc_gtfs$stop_times %>%
  inner_join(tarc_gtfs$stops) %>%
  inner_join(tarc_gtfs$trips) %>%
  inner_join(tarc_gtfs$routes) %>%
  filter(service_id == 3,
         !grepl("UPS", route_long_name)) %>%
  replace_na(list(shape_dist_traveled = 0)) %>%
  mutate(node_id = paste(trip_id, stop_sequence, sep = "-"),
         time = arrival_time,
         time_sec = period_to_seconds(hms(time))) %>%
  select(node_id, pt_geo = geometry, time, time_sec, stop_id, stop_name,
         route_id, route_long_name, trip_id, direction_id, trip_headsign,
         stop_sequence, shape_dist_traveled, shape_id) %>%
  arrange(time) %>%
  st_sf()

# Shapes Network ------------------------------------------------------------

shapes <- tarc_gtfs$shapes
plot(shapes, bg = "#111111")

shape_net <- as_sfnetwork(shapes)
autoplot(shape_net)

# shape_stops <- shape_net %>%
#   st_network_blend(graph_data)
# autoplot(shape_stops)

# Library Network ---------------------------------------------------------

library_coords <- read_csv(here("data", "library_coords.csv")) %>%
  st_as_sf(coords = c("library_lon", "library_lat"))

# Walking Graph ----------------------------------------------------------

# OSMExtract / OSMData for walking data

osm_walking <- oe_get_network(place = "louisville", mode = "walking")
plot(st_geometry(osm_walking))
