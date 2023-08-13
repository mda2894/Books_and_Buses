# First Attempt

# Setup ------------------------------------------------------------------

library(conflicted)
library(here)
library(tidyverse)
library(tidytransit)
library(igraph)
library(tidygraph)
library(geosphere)
library(lubridate)

conflicts_prefer(
  dplyr::filter
)

# Data -------------------------------------------------------------------

tarc_file <- here("data", "tarc_gtfs.zip")

# # uncomment this block to download new TARC GTFS file
# dir.create(here("data"))
# tarc_feed <- "http://googletransit.ridetarc.org/feed/google_transit.zip"
# download.file(tarc_feed, destfile = tarc_file)

tarc_gtfs <- read_gtfs(tarc_file)
# summary(tarc_gtfs)

# create single table with all regular weekday stops and related information
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
  select(node_id, route_id, route_long_name, trip_id, direction_id,
         trip_headsign, stop_sequence, shape_dist_traveled, stop_id, stop_name,
         stop_time, stop_time_sec, stop_lat, stop_lon) %>%
  arrange(stop_time_sec)

# Trip Graph -------------------------------------------------------------

# node table for all bus stops
bus_stops <- graph_data %>%
  mutate(node_type = "bus_stop") %>%
  select(name = node_id, node_type, route_id, trip_id, direction_id,
         stop_sequence, stop_id, stop_time, stop_time_sec, stop_lat, stop_lon)

# route/trip edges (no transfers)
trip_graph_edges <- graph_data %>%
  group_by(trip_id) %>%
  arrange(stop_sequence) %>%
  mutate(from = node_id,
         to = lead(node_id),
         distance = lead(shape_dist_traveled) - shape_dist_traveled,
         weight = lead(stop_time_sec) - stop_time_sec) %>%
  filter(row_number() <= n() - 1) %>%
  ungroup() %>%
  mutate(edge_type = "bus_trip") %>%
  select(from, to, edge_type, weight, distance, route_id, trip_id, direction_id,
         trip_leg = stop_sequence)

trip_graph <- tbl_graph(bus_stops, trip_graph_edges, directed = T)
# print(trip_graph)

# Transfers --------------------------------------------------------------

# Add "same-stop" transfer edges (i.e. waiting for the next bus)
transfer_graph_edges <- graph_data %>%
  group_by(stop_id) %>%
  arrange(stop_time_sec) %>%
  mutate(from = node_id,
         to = lead(node_id),
         distance = 0,
         weight = lead(stop_time_sec) - stop_time_sec) %>%
  filter(row_number() <= n() - 1) %>%
  ungroup() %>%
  mutate(edge_type = "waiting") %>%
  select(from, to, edge_type, weight, distance)

transfer_graph <- tbl_graph(bus_stops, transfer_graph_edges,
                            directed = T)
# print(transfer_graph)

# combine trips w/ transfers
bus_only_graph <- graph_join(trip_graph, transfer_graph)
# print(bus_only_graph)

rm(trip_graph, trip_graph_edges, transfer_graph, transfer_graph_edges)

# # Bus-Only (Forrest Gump) Routing ----------------------------------------
#
# # graph_data %>%
# #   filter(grepl("Mall", stop_name)) %>%
# #   View()
#
# # playing with routing between two random bus stops
# start_nodes <- bus_only_graph %>%
#   activate("nodes") %>%
#   as_tibble() %>%
#   filter(stop_id == 21285) %>% # Jefferson Mall
#   arrange(stop_time_sec) %>%
#   pull(name)
#
# target_nodes <- bus_only_graph %>%
#   activate("nodes") %>%
#   as_tibble() %>%
#   filter(stop_id == 8290) %>% # St. Matthews Mall
#   arrange(stop_time_sec) %>%
#   pull(name)
#
# # find shortest paths between every possible connection between the two stops
# dist_matrix <- distances(bus_only_graph, v = start_nodes, to = target_nodes,
#                          mode = "out")
# min(dist_matrix)
# ind <- arrayInd(which.min(dist_matrix), dim(dist_matrix))
#
# # grab the two nodes that represent the starting and finishing stops
# best_start <- rownames(dist_matrix)[ind[,1]]
# best_finish <- colnames(dist_matrix)[ind[,2]]
#
# # get the full path between the two
# res <- bus_only_graph %>%
#   shortest_paths(from = best_start, to = best_finish,
#                  mode = "out", output = "both")
#
# node_list <- names(Filter(function(x) length(x) > 0, res$vpath)[[1]])
#
# node_path <- graph_data %>%
#   filter(node_id %in% node_list) %>%
#   arrange(stop_time_sec)
#
# # playing with the distances function for potential graph pruning
# start_node <- graph_data %>%
#   arrange(stop_time_sec) %>%
#   filter(row_number() == 1) %>%
#   pull(node_id)
#
# all_dist <- distances(bus_only_graph, v = start_node, mode = "out")
# sum(all_dist != Inf)

# Walking Graph ----------------------------------------------------------

get_walkable_nodes <- function(current, speed = 1, dist = 2000, time = 60*60){

  nearby_stops <- graph_data %>%
    # calculate "taxi-cab" distance (assuming a grid-like street network)
    mutate(distance = distHaversine(cbind(current$stop_lon, current$stop_lat),
                                cbind(current$stop_lon, stop_lat)) +
                      distHaversine(cbind(current$stop_lon, current$stop_lat),
                                cbind(stop_lon, current$stop_lat)),
           time_diff = stop_time_sec - current$stop_time_sec) %>%
    filter(distance <= dist,           # <= 2 km away
           stop_id != current$stop_id, # not same stop
           time_diff > dist / speed,   # can make it there in time
           time_diff < time) %>%    # but less than an hour in the future
    group_by(trip_id) %>%
    arrange(dist) %>%
    filter(row_number() == 1) %>%      # only closest stop for each trip
    group_by(stop_id) %>%
    arrange(stop_time_sec) %>%
    filter(row_number() == 1) %>%      # only the earliest node per stop
    ungroup() %>%
    mutate(from = current$node_id,
           to = node_id,
           weight = stop_time_sec - current$stop_time_sec,
           edge_type = "walking") %>%
    select(from, to, edge_type, weight, distance)

  return(nearby_stops)
}

walking_edges <- tibble()

for (i in 1:nrow(graph_data)) {
  walking_edges <- rbind(walking_edges, get_walkable_nodes(graph_data[i,]))
}



# Add Library Nodes ------------------------------------------------------

