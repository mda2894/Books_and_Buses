# Setup -------------------------------------------------------------------

library(conflicted)
library(here)
library(tidyverse)
library(tidytransit)
library(tidygraph)
library(ggraph)

conflicts_prefer(
  dplyr::filter
)

# Data --------------------------------------------------------------------

tarc_feed <- "http://googletransit.ridetarc.org/feed/google_transit.zip"
tarc_file <- here("data", "tarc_gtfs.zip")

download.file(tarc_feed, destfile = tarc_file)

tarc_gtfs <- read_gtfs(tarc_file)
# summary(tarc_gtfs)

# create single table with all regular weekday stops and related information
graph_data <- tarc_gtfs$stop_times %>%
  inner_join(tarc_gtfs$stops) %>%
  inner_join(tarc_gtfs$trips) %>%
  inner_join(tarc_gtfs$routes) %>%
  filter(service_id == 3) %>%
  select(route_id, route_long_name, trip_id, direction_id, trip_headsign,
         stop_sequence, shape_dist_traveled, stop_id, stop_name, arrival_time,
         stop_lat, stop_lon) %>%
  replace_na(list(shape_dist_traveled = 0)) %>%
  mutate(node_id = paste(trip_id, stop_sequence, sep = "|"))

# # Testing igraph --------------------------------------------------------
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

# Trip Graph --------------------------------------------------------------

trip_graph_nodes <- graph_data %>%
  mutate(node_type = "bus_stop") %>%
  select(node_id, node_type, route_id, route_long_name, trip_id, stop_sequence,
         stop_id, stop_name, arrival_time, stop_lat, stop_lon) %>%
  arrange(trip_id, stop_sequence)

# route/trip edges (no transfers)
trip_graph_edges <- graph_data %>%
  group_by(trip_id) %>%
  arrange(stop_sequence) %>%
  mutate(from = node_id,
         to = lead(node_id),
         distance = lead(shape_dist_traveled) - shape_dist_traveled,
         time = lead(arrival_time) - arrival_time) %>%
  filter(row_number() <= n() - 1) %>%
  ungroup() %>%
  mutate(edge_type = "bus_trip") %>%
  select(from, to, edge_type, route_id, route_long_name, trip_id,
         trip_leg = stop_sequence, direction_id, trip_headsign, distance, time) %>%
  arrange(trip_id, trip_leg)


# "rooted forest" with each trip a separate tree
trip_graph <- tbl_graph(trip_graph_nodes, trip_graph_edges, directed = T)
# print(trip_graph)

# Transfer Graph ----------------------------------------------------------

transfer_graph_nodes <- trip_graph_nodes %>%
  arrange(stop_id, arrival_time)

transfer_graph_edges <- graph_data %>%
  group_by(stop_id) %>%
  arrange(arrival_time) %>%
  mutate(from = node_id,
         to = lead(node_id),
         from_route = route_id,
         to_route = lead(route_id),
         distance = 0,
         time = lead(arrival_time) - arrival_time) %>%
  filter(row_number() <= n() - 1) %>%
  ungroup() %>%
  mutate(edge_type = "transfer") %>%
  arrange(stop_id, arrival_time) %>%
  select(from, to, edge_type, stop_id, stop_name, from_route, to_route,
         distance, time)

transfer_graph <- tbl_graph(transfer_graph_nodes, transfer_graph_edges,
                            directed = T)
# print(transfer_graph)

# Bus-Only Graph ----------------------------------------------------------

bus_only_graph <- graph_join(trip_graph, transfer_graph)
# print(bus_only_graph)

bus_only_graph %>%
  as_tibble("edges") %>%
  View()
