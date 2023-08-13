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

conflicts_prefer(
  dplyr::filter
)

# Data -------------------------------------------------------------------

# dir.create(here("data"))
tarc_file <- here("data", "tarc_gtfs.zip")

# tarc_feed <- "http://googletransit.ridetarc.org/feed/google_transit.zip"
# download.file(tarc_feed, destfile = tarc_file)

tarc_gtfs <- read_gtfs(tarc_file) %>%
  filter_feed_by_date("2023-08-30") %>%
  gtfs_as_sf()

plot(tarc_gtfs$shapes, bg = "#111111")

# stop_groups <- stop_group_distances(tarc_gtfs$stops) %>%
#   filter(n_stop_ids > 1) %>%
#   arrange(desc(n_stop_ids), desc(dist_mean))
#
# stop_clusters <- cluster_stops(tarc_gtfs$stops)

graph_data <- tarc_gtfs$stop_times %>%
  inner_join(tarc_gtfs$stops) %>%
  inner_join(tarc_gtfs$trips) %>%
  inner_join(tarc_gtfs$routes) %>%
  filter(service_id == 3,
         !grepl("UPS", route_long_name)) %>%
  replace_na(list(shape_dist_traveled = 0)) %>%
  mutate(node_id = paste(trip_id, stop_sequence, sep = "-"),
         stop_time = arrival_time,
         stop_time_sec = period_to_seconds(hms(stop_time))) %>%
  select(node_id, geometry, stop_time, stop_time_sec, stop_id, stop_name,
         route_id, route_long_name, trip_id, direction_id, trip_headsign,
         stop_sequence, shape_dist_traveled) %>%
  arrange(stop_time) %>%
  st_sftime(time_column_name = "stop_time", time_column_last = F)








# Walking Graph ----------------------------------------------------------

# OSMExtract / OSMData for walking data
