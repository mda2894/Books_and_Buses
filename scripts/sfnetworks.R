# Second attempt
#
# Using sfnetworks and osmextract
#

# Setup ------------------------------------------------------------------

library(conflicted)
library(here)
library(tidyverse)
library(tidytransit)
library(igraph)
library(tidygraph)
library(sf)
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

bus_data <- tarc_gtfs$stop_times %>%
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

# Bus Network -------------------------------------------------------------

trip_graph_edges <- bus_data %>%
  group_by(trip_id) %>%
  arrange(stop_sequence) %>%
  mutate(from = node_id,
         to = lead(node_id),
         distance = lead(shape_dist_traveled) - shape_dist_traveled,
         weight = lead(time_sec) - time_sec,
         ln_geo = st_cast(st_union(pt_geo, lead(pt_geo)), "LINESTRING")) %>%
  filter(row_number() <= n() - 1) %>%
  ungroup() %>%
  mutate(edge_type = "bus_trip") %>%
  st_set_geometry("ln_geo") %>%
  select(from, to, edge_type, weight, distance, route_id, route_long_name,
         trip_id, direction_id, trip_headsign, trip_leg = stop_sequence, ln_geo)

bus_net <- sfnetwork(bus_data, trip_graph_edges, force = T)

bus_net %>%
  activate("edges") %>%
  as_tibble() %>%
  View()


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

tarc_bbox <- shape_net %>%
  st_network_bbox() %>%
  st_as_sfc()

walking_net <- oe_get_network(place = "kentucky", mode = "walking") %>%
  st_crop(tarc_bbox) %>%
  filter(st_geometry_type(geometry) == "LINESTRING") %>%
  as_sfnetwork()

