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
library(hms)

conflicts_prefer(
  dplyr::filter,
  lubridate::hms
)

# Data -------------------------------------------------------------------

tarc_file <- here("data", "tarc_gtfs.zip")

# # uncomment this block to download new TARC GTFS file
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
         arrival_time = period_to_seconds(hms(arrival_time))) %>%
  select(node_id, route_id, route_long_name, trip_id, direction_id,
         trip_headsign, stop_sequence, shape_dist_traveled, stop_id, stop_name,
         arrival_time, node_lat = stop_lat, node_lon = stop_lon) %>%
  arrange(arrival_time)

# Trip Graph -------------------------------------------------------------

# node table for all bus stops
bus_stops <- graph_data %>%
  mutate(node_type = "bus_stop") %>%
  select(name = node_id, node_type, route_id, trip_id, direction_id,
         stop_sequence, stop_id, arrival_time, node_lat, node_lon)

# route/trip edges (no transfers)
trip_graph_edges <- graph_data %>%
  group_by(trip_id) %>%
  arrange(stop_sequence) %>%
  mutate(from = node_id,
         to = lead(node_id),
         distance = lead(shape_dist_traveled) - shape_dist_traveled,
         weight = lead(arrival_time) - arrival_time) %>%
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
  arrange(arrival_time) %>%
  mutate(from = node_id,
         to = lead(node_id),
         distance = 0,
         weight = lead(arrival_time) - arrival_time) %>%
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
#   arrange(arrival_time) %>%
#   pull(name)
#
# target_nodes <- bus_only_graph %>%
#   activate("nodes") %>%
#   as_tibble() %>%
#   filter(stop_id == 8290) %>% # St. Matthews Mall
#   arrange(arrival_time) %>%
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
#   arrange(arrival_time)
#
# # playing with the distances function for potential graph pruning
# start_node <- graph_data %>%
#   arrange(arrival_time) %>%
#   filter(row_number() == 1) %>%
#   pull(node_id)
#
# all_dist <- distances(bus_only_graph, v = start_node, mode = "out")
# sum(all_dist != Inf)
#
# rm(all_dist, dist_matrix, ind, node_list, node_path, res, start_node,
#    start_nodes, target_nodes)

# # Walking Graph ----------------------------------------------------------

taxicab_dist <- function(lon1, lon2, lat1, lat2) {
  lat_c <- 111.32 * 1000 # approximate # of meters in one degree latitude
  lon_c <- 40075 * 1000 * cos(lat1) / 360 # same for longitude
  return(abs(lat1 - lat2) * lat_c + abs(lon1 - lon2) * lon_c)
}

get_walkable_nodes <- function(current, speed = 1, max_dist = 2000,
                               max_time = 60*60, min_time = 60*10) {
  lat_c <- 111.32 * 1000 # approximate # of meters in one degree latitude
  lon_c <- 40075 * 1000 * cos(current$node_lat) / 360 # same for longitude

  nearby_stops <- graph_data %>%
    # calculate approximate "taxi-cab" distance
    mutate(distance = taxicab_dist(current$node_lon, node_lon,
                                   current$node_lat, node_lat),
           time_diff = arrival_time - current$arrival_time) %>%
    filter(stop_id != current$stop_id,   # not same stop
           distance <= max_dist,         # <= 2 km away
           time_diff > distance / speed, # can make it there in time
           time_diff < max_time,         # but less than an hour in the future
           time_diff > min_time) %>%     # at least 10 minutes to make transfer
    group_by(trip_id) %>%
    filter(distance == min(distance)) %>% # closest stop for each trip
    group_by(stop_id) %>%
    filter(arrival_time == min(arrival_time)) %>% # earliest node per stop
    ungroup() %>%
    mutate(from = current$node_id,
           to = node_id,
           weight = time_diff,
           edge_type = "walking") %>%
    select(from, to, edge_type, weight, distance)

  return(nearby_stops)
}

system.time(
walking_edges <- graph_data %>%
  select(node_id, stop_id, arrival_time, node_lon, node_lat) %>%
  rowwise() %>%
  mutate(new_edges = pick(everything()) %>% get_walkable_nodes() %>% list()) %>%
  select(new_edges) %>%
  unnest(cols = c(new_edges))
)

walking_graph <- tbl_graph(nodes = bus_stops, edges = walking_edges)

bus_and_walk <- graph_join(bus_only_graph, walking_graph)

write_csv(walking_edges, here("data", "walking_edges.csv"))

# # Testing the Full Graph --------------------------------------------------
#
# # playing with routing between two random bus stops
# start_nodes <- bus_and_walk %>%
#   activate("nodes") %>%
#   as_tibble() %>%
#   filter(stop_id == 21285) %>% # Jefferson Mall
#   arrange(arrival_time) %>%
#   pull(name)
#
# target_nodes <- bus_and_walk %>%
#   activate("nodes") %>%
#   as_tibble() %>%
#   filter(stop_id == 8290) %>% # St. Matthews Mall
#   arrange(arrival_time) %>%
#   pull(name)
#
# # find shortest paths between every possible connection between the two stops
# dist_matrix <- distances(bus_and_walk, v = start_nodes, to = target_nodes,
#                          mode = "out")
# min(dist_matrix)
# ind <- arrayInd(which.min(dist_matrix), dim(dist_matrix))
#
# # grab the two nodes that represent the starting and finishing stops
# best_start <- rownames(dist_matrix)[ind[,1]]
# best_finish <- colnames(dist_matrix)[ind[,2]]
#
# # get the full path between the two
# res <- bus_and_walk %>%
#   shortest_paths(from = best_start, to = best_finish,
#                  mode = "out", output = "both")
#
# node_list <- names(Filter(function(x) length(x) > 0, res$vpath)[[1]])
#
# node_path <- graph_data %>%
#   filter(node_id %in% node_list) %>%
#   arrange(arrival_time)
#
# # Success! My directions essentially match those given by GoogleMaps
#
# rm(all_dist, dist_matrix, ind, node_path, res)

# Add Library Nodes ------------------------------------------------------

# (DONE) Compile list of library coords
# (DONE) Select the closest (25?) stops to each library, based on walk distance
# (DONE) Limit to just the closest stop for every trip
# (DONE) Filter out stops too far away to walk to (>3K)
# (DONE) Create library and stop nodes
# Connect each stop to its library node, using walk time
# Connect each library to each of its stops, walk time + (10?) minutes inside
# Add waiting edges connecting all of the nodes for each library

library_data <- here("data", "library_coords.csv") %>%
  read_csv()

library_stops <- tibble()

for (i in 1:nrow(library_data)) {
  closest_stops <- graph_data %>%
    select(stop_id, stop_name, node_lat, node_lon) %>%
    unique() %>%
    mutate(library = library_data$library_name[i],
           library_lat = library_data$library_lat[i],
           library_lon = library_data$library_lon[i],
           dist = taxicab_dist(library_lon, node_lon,
                               library_lat, node_lat)) %>%
    arrange(dist) %>%
    filter(row_number() <= 25) %>%
    left_join(bus_stops) %>%
    group_by(trip_id) %>%
    filter(dist == min(dist)) %>%
    ungroup() %>%
    filter(dist < 3000)

  library_stops <- rbind(library_stops, closest_stops)
}

library_stops <- library_stops %>%
  arrange(library, arrival_time) %>%
  group_by(library) %>%
  mutate(library_id = paste(library, row_number(), sep = "-")) %>%
  ungroup()

# library_stats <- library_stops %>%
#   count(library, route_id, direction_id, stop_id, stop_name, dist)

library_nodes <- library_stops %>%
  mutate(name = library_id,
         node_type = "library",
         arrival_time = arrival_time + round(dist)) %>% # 1 m/s
  select(name, node_type, library, arrival_time, node_lat = library_lat,
         node_lon = library_lon)

stop_nodes <- library_stops %>%
  select(name, node_type, route_id, trip_id, direction_id, stop_sequence,
         stop_id, arrival_time, node_lat, node_lon)

all_nodes <- full_join(library_nodes, stop_nodes)

library_incoming_edges <- library_stops %>%
  mutate(to = library_nodes$name[row_number()],
         edge_type = "to_library",
         weight = round(dist)) %>%
  select(from = name, to, edge_type, weight, distance = dist)

library_outgoing_edges <- library_stops %>%
  mutate(distance = lead(dist),
         weight = lead(dist) + 600, # assuming 10 minutes in each library
         to = lead(name), # THIS IS BAD!! TIME TRAVEL IS POSSIBLE!!
         edge_type = "from_library") %>%
  select(from = library_id, to, edge_type, weight, distance)
