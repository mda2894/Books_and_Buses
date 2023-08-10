# Setup -------------------------------------------------------------------

library(tidyverse)
library(tidytransit)
library(igraph)
library(tidygraph)
library(ggraph)

setwd("C:/Users/manderson/R/Projects/Books by Bus")

# Data --------------------------------------------------------------------

tarc_feed <- "http://googletransit.ridetarc.org/feed/google_transit.zip"
tarc_file <- "tarc_gtfs.zip"

download.file(tarc_feed, destfile = tarc_file)

tarc_gtfs <- read_gtfs(tarc_file)
summary(tarc_gtfs)

# create single table with all regular weekday stops and related information
weekdays <- tarc_gtfs$stop_times %>%
  inner_join(tarc_gtfs$stops) %>%
  inner_join(tarc_gtfs$trips) %>%
  inner_join(tarc_gtfs$routes) %>%
  filter(service_id == 3) %>%
  select(route_id, route_long_name, trip_id, direction_id, trip_headsign,
         stop_sequence, shape_dist_traveled, stop_id, stop_name, arrival_time,
         stop_lat, stop_lon) %>%
  replace_na(list(shape_dist_traveled = 0))

# # First Test Network ------------------------------------------------------
#
# # just one trip
# test_data_1 <- weekdays %>%
#   filter(trip_id == 853090)
#
# # create vertex table
# test_vertices_1 <- test_data_1 %>%
#   select(stop_sequence, trip_id, stop_id, stop_name, arrival_time,
#          stop_lat, stop_lon)
#
# # create edges table
# test_edges_1 <- test_data_1 %>%
#   mutate(from = stop_sequence,
#          to = lead(stop_sequence),
#          distance = lead(shape_dist_traveled) - shape_dist_traveled,
#          time = lead(arrival_time) - arrival_time) %>%
#   select(from, to, route_id, route_long_name, trip_id, direction_id,
#          trip_headsign, distance, time) %>%
#   filter(row_number() <= n() - 1)
#
# # create graph from vertices and edges
# test_graph_1 <- graph_from_data_frame(test_edges_1, directed = T,
#                                       vertices = test_vertices_1)
#
# # playing with visualizations
# print(test_graph_1, e = T, v = T)
# plot(test_graph_1, layout = layout_in_circle)
#
#
# # Second Test Network -----------------------------------------------------
#
# # two trips (same route, opposite directions)
# test_data_2 <- weekdays %>%
#   filter(trip_id %in% c(853090, 853092)) %>%
#   mutate(node_id = paste(trip_id, stop_sequence, sep = "|"))
#
# test_vertices_2 <- test_data_2 %>%
#   select(node_id, trip_id, stop_sequence, stop_id, stop_name, arrival_time,
#          stop_lat, stop_lon)
#
# test_edges_2 <- test_data_2 %>%
#   group_by(trip_id) %>%
#   mutate(from = node_id,
#          to = lead(node_id),
#          distance = lead(shape_dist_traveled) - shape_dist_traveled,
#          time = lead(arrival_time) - arrival_time) %>%
#   select(from, to, route_id, route_long_name, trip_id, direction_id,
#          trip_headsign, distance, time) %>%
#   filter(row_number() <= n() - 1)
#
# test_graph_2 <- graph_from_data_frame(test_edges_2, directed = T,
#                                       vertices = test_vertices_2)
#
# print(test_graph_2, full = T)
# plot(test_graph_2, vertex.label = NA)

# Full Graph --------------------------------------------------------------

graph_data <- weekdays %>%
  mutate(node_id = paste(trip_id, stop_sequence, sep = "|"))

graph_nodes <- graph_data %>%
  select(node_id, trip_id, stop_sequence, stop_id, stop_name, arrival_time,
         stop_lat, stop_lon) %>%
  arrange(trip_id, stop_sequence)

graph_edges <- graph_data %>%
  group_by(trip_id) %>%
  arrange(stop_sequence) %>%
  mutate(from = node_id,
         to = lead(node_id),
         distance = lead(shape_dist_traveled) - shape_dist_traveled,
         time = lead(arrival_time) - arrival_time) %>%
  filter(row_number() <= n() - 1) %>%
  ungroup() %>%
  select(from, to, route_id, route_long_name, trip_id, trip_leg = stop_sequence,
         direction_id, trip_headsign, distance, time) %>%
  arrange(trip_id, trip_leg)

g1 <- graph_from_data_frame(graph_edges, vertices = graph_nodes, directed = T)

print(g1, full = T)
