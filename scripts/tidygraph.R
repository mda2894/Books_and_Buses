# First Attempt
#
# Using tidygraph
#

# Setup ------------------------------------------------------------------

library(conflicted)
library(here)
library(tidyverse)
library(tidytransit)
library(igraph)
library(tidygraph)
library(geosphere)
library(lubridate)
library(TSP)
library(doParallel)

conflicts_prefer(
  dplyr::filter,
  dplyr::lag
)

registerDoParallel()

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
         arrival_time_sec = period_to_seconds(hms(arrival_time))) %>%
  select(node_id, route_id, route_long_name, trip_id, direction_id,
         trip_headsign, stop_sequence, shape_dist_traveled, stop_id, stop_name,
         arrival_time, arrival_time_sec, node_lat = stop_lat,
         node_lon = stop_lon) %>%
  arrange(arrival_time_sec)

rm(tarc_file)

# Trip Graph -------------------------------------------------------------

# node table for all bus stops
bus_stops <- graph_data %>%
  mutate(node_type = "bus_stop") %>%
  select(name = node_id, node_type, route_id, route_long_name, trip_id,
         direction_id, trip_headsign, stop_sequence, stop_id, stop_name,
         arrival_time, arrival_time_sec, node_lat, node_lon)

# route/trip edges (no transfers)
trip_graph_edges <- graph_data %>%
  group_by(trip_id) %>%
  arrange(stop_sequence) %>%
  mutate(from = node_id,
         to = lead(node_id),
         distance = lead(shape_dist_traveled) - shape_dist_traveled,
         weight = lead(arrival_time_sec) - arrival_time_sec) %>%
  filter(row_number() <= n() - 1) %>%
  ungroup() %>%
  mutate(edge_type = "bus_trip") %>%
  select(from, to, edge_type, weight, distance, route_id, route_long_name,
         trip_id, direction_id, trip_headsign, trip_leg = stop_sequence)

trip_graph <- tbl_graph(bus_stops, trip_graph_edges, directed = T)

# Transfers --------------------------------------------------------------

# Add "same-stop" transfer edges (i.e. waiting for the next bus)
transfer_graph_edges <- graph_data %>%
  group_by(stop_id) %>%
  arrange(arrival_time_sec) %>%
  mutate(from = node_id,
         to = lead(node_id),
         distance = 0,
         weight = lead(arrival_time_sec) - arrival_time_sec) %>%
  filter(row_number() <= n() - 1) %>%
  ungroup() %>%
  mutate(edge_type = "waiting") %>%
  select(from, to, edge_type, weight, distance)

transfer_graph <- tbl_graph(bus_stops, transfer_graph_edges,
                            directed = T)

# combine trips w/ transfers
bus_only_graph <- graph_join(trip_graph, transfer_graph)

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
#   arrange(arrival_time_sec) %>%
#   pull(name)
#
# target_nodes <- bus_only_graph %>%
#   activate("nodes") %>%
#   as_tibble() %>%
#   filter(stop_id == 8290) %>% # St. Matthews Mall
#   arrange(arrival_time_sec) %>%
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
#   arrange(arrival_time_sec)
#
# # playing with the distances function for potential graph pruning
# start_node <- graph_data %>%
#   arrange(arrival_time_sec) %>%
#   filter(row_number() == 1) %>%
#   pull(node_id)
#
# all_dist <- distances(bus_only_graph, v = start_node, mode = "out")
# sum(all_dist != Inf)
#
# rm(all_dist, dist_matrix, ind, node_list, node_path, res, start_node,
#    start_nodes, target_nodes, best_finish, best_start)

# # Walking Graph -----------------------------------------------------------

taxicab_dist <- function(lon1, lon2, lat1, lat2) {
  lat_c <- 111.32 * 1000 # approximate # of meters in one degree latitude
  lon_c <- 40075 * 1000 * cos(lat1) / 360 # same for longitude
  return(abs(lat1 - lat2) * lat_c + abs(lon1 - lon2) * lon_c)
}

# get_walkable_nodes <- function(current, speed = 1.5, max_dist = 2000,
#                                max_time = 60*60, min_time = 60*10) {
#   lat_c <- 111.32 * 1000 # approximate # of meters in one degree latitude
#   lon_c <- 40075 * 1000 * cos(current$node_lat) / 360 # same for longitude
#
#   nearby_stops <- graph_data %>%
#     # calculate approximate "taxi-cab" distance
#     mutate(distance = taxicab_dist(current$node_lon, node_lon,
#                                    current$node_lat, node_lat),
#            time_diff = arrival_time_sec - current$arrival_time_sec) %>%
#     filter(stop_id != current$stop_id,   # not same stop
#            distance <= max_dist,         # <= 2 km away
#            time_diff > distance / speed, # can make it there in time
#            time_diff < max_time,         # but less than an hour in the future
#            time_diff > min_time) %>%     # at least 10 minutes to make transfer
#     group_by(trip_id) %>%
#     filter(distance == min(distance)) %>% # closest stop for each trip
#     group_by(stop_id) %>%
#     filter(arrival_time_sec == min(arrival_time_sec)) %>% # earliest node per stop
#     ungroup() %>%
#     mutate(from = current$node_id,
#            to = node_id,
#            weight = time_diff,
#            edge_type = "walking") %>%
#     select(from, to, edge_type, weight, distance)
#
#   return(nearby_stops)
# }
#
# system.time({
# walking_edges <- graph_data %>%
#   select(node_id, stop_id, arrival_time_sec, node_lon, node_lat) %>%
#   rowwise() %>%
#   mutate(new_edges = pick(everything()) %>% get_walkable_nodes() %>% list()) %>%
#   select(new_edges) %>%
#   unnest(cols = c(new_edges))
# })
#
# write_csv(walking_edges, here("data", "walking_edges.csv"))

walking_edges <- read_csv(here("data", "walking_edges.csv"))

walking_graph <- tbl_graph(nodes = bus_stops, edges = walking_edges)

bus_and_walk <- graph_join(bus_only_graph, walking_graph)

rm(walking_edges, walking_graph, bus_only_graph)

# # Testing the Bus and Walk Graph ------------------------------------------
#
# # playing with routing between two random bus stops
# start_nodes <- bus_and_walk %>%
#   activate("nodes") %>%
#   as_tibble() %>%
#   filter(stop_id == 21285) %>% # Jefferson Mall
#   arrange(arrival_time_sec) %>%
#   pull(name)
#
# target_nodes <- bus_and_walk %>%
#   activate("nodes") %>%
#   as_tibble() %>%
#   filter(stop_id == 8290) %>% # St. Matthews Mall
#   arrange(arrival_time_sec) %>%
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
#   arrange(arrival_time_sec)
#
# # Success! My directions essentially match those given by GoogleMaps
#
# rm(dist_matrix, ind, node_path, res, best_finish, best_start,
#    node_list, start_nodes, target_nodes)

# Add Library Nodes ------------------------------------------------------

# (DONE) Compile list of library coords
# (DONE) Select the closest (25?) stops to each library, based on walk distance
# (DONE) Limit to just the closest stop for every trip
# (DONE) Filter out stops too far away to walk to (>3K)
# (DONE) Create library and stop nodes
# (DONE) Connect each stop to its library node, using walk time
# (DONE) Connect each library to each of its stops, walk time + (10?) minutes inside
# (DONE) Add waiting edges connecting all of the nodes for each library

library_info <- here("data", "library_info.csv") %>%
  read_csv()

library_data <- tibble()

for (i in 1:nrow(library_info)) {
  closest_stops <- graph_data %>%
    select(stop_id, stop_name, node_lat, node_lon) %>%
    unique() %>%
    mutate(library = library_info$library_name[i],
           library_lat = library_info$library_lat[i],
           library_lon = library_info$library_lon[i],
           dist = taxicab_dist(library_lon, node_lon,
                               library_lat, node_lat)) %>%
    arrange(dist) %>%
    filter(row_number() <= 25) %>%
    left_join(bus_stops) %>%
    group_by(trip_id) %>%
    filter(dist == min(dist)) %>%
    ungroup() %>%
    filter(dist < 3000)

  library_data <- rbind(library_data, closest_stops)
}

rm(closest_stops, i)

library_data <- library_data %>%
  arrange(library, arrival_time_sec) %>%
  group_by(library) %>%
  mutate(library_id = paste(library, row_number(), sep = "-")) %>%
  ungroup()

# library_stats <- library_data %>%
#   count(library, route_id, direction_id, stop_id, stop_name, dist)

stop_nodes <- library_data %>%
  select(name, node_type, route_id, trip_id, direction_id,stop_sequence,
         stop_id, arrival_time, arrival_time_sec, node_lat, node_lon)

speed <- 1.5 # m/s

library_in_nodes <- library_data %>%
  mutate(name = paste(library_id, "in", sep = "-"),
         node_type = "library_in",
         arrival_time_sec = arrival_time_sec + round(dist / speed),
         arrival_time = arrival_time + round(dist / speed)) %>%
  select(name, node_type, library, arrival_time, arrival_time_sec,
         node_lat = library_lat, node_lon = library_lon) %>%
  arrange(library, arrival_time_sec)

library_out_nodes <- library_data %>%
  mutate(node_type = "library_out",
         arrival_time_sec = if_else(lead(library) == library,
                                lead(arrival_time_sec) - round(lead(dist) / speed),
                                NA),
         arrival_time = if_else(lead(library) == library,
                                    lead(arrival_time) - round(lead(dist) / speed),
                                    NA),
         name = if_else(!is.na(arrival_time_sec),
                        paste(lead(library_id), "out", sep = "-"),
                        NA)) %>%
  select(name, node_type, library, arrival_time, arrival_time_sec,
         node_lat = library_lat, node_lon = library_lon) %>%
  arrange(library, arrival_time_sec) %>%
  drop_na()

library_incoming_edges <- library_data %>%
  mutate(to = library_in_nodes$name[row_number()],
         edge_type = "to_library",
         weight = round(dist)) %>%
  select(from = name, to, edge_type, weight, distance = dist)

library_outgoing_edges <- library_data %>%
  mutate(from = if_else(!grepl("-1$", library_id),
                        paste(library_id, "out", sep = "-"),
                        NA),
         edge_type = "from_library",
         weight = round(dist)) %>%
  select(from, to = name, edge_type, weight, distance = dist) %>%
  drop_na()

library_waiting_edges <- library_out_nodes %>%
  arrange(library, arrival_time_sec) %>%
  mutate(to = if_else(lead(library) == library,
                      lead(name),
                      NA),
         weight = lead(arrival_time_sec) - arrival_time_sec,
         edge_type = "library_waiting",
         distance = 0) %>%
  select(from = name, to, edge_type, weight, distance) %>%
  drop_na()

library_busy_edges <- library_in_nodes %>%
  rowwise() %>%
  mutate(to = library_out_nodes$name[library_out_nodes$library == library &
                                       library_out_nodes$arrival_time_sec > arrival_time_sec][1]) %>%
  ungroup() %>%
  left_join(library_out_nodes, by = join_by(to == name)) %>%
  mutate(weight = arrival_time_sec.y - arrival_time_sec.x,
         distance = 0,
         edge_type = "library_busy") %>%
  select(from = name, to, edge_type, weight, distance) %>%
  drop_na()

library_nodes <- stop_nodes %>%
  full_join(library_in_nodes) %>%
  full_join(library_out_nodes)

library_edges <- library_incoming_edges %>%
  full_join(library_busy_edges) %>%
  full_join(library_waiting_edges) %>%
  full_join(library_outgoing_edges)

library_graph <- tbl_graph(nodes = library_nodes, edges = library_edges)

full_graph <- graph_join(bus_and_walk, library_graph)

rm(bus_and_walk, library_busy_edges, library_info, library_data, library_edges,
   library_in_nodes, library_incoming_edges, library_nodes, library_out_nodes,
   library_outgoing_edges, library_waiting_edges, library_graph, stop_nodes,
   speed, library_stats, bus_stops, taxicab_dist)

full_nodes <- full_graph %>%
  activate("nodes") %>%
  as_tibble() %>%
  arrange(arrival_time)

# Testing Full Graph ------------------------------------------------------

# # Directions without libraries
#
# # playing with routing between two random bus stops
# start_nodes <- full_graph %>%
#   activate("nodes") %>%
#   as_tibble() %>%
#   filter(stop_id == 21285) %>% # Jefferson Mall
#   arrange(arrival_time_sec) %>%
#   pull(name)
#
# target_nodes <- full_graph %>%
#   activate("nodes") %>%
#   as_tibble() %>%
#   filter(stop_id == 8290) %>% # St. Matthews Mall
#   arrange(arrival_time_sec) %>%
#   pull(name)
#
# # find shortest paths between every possible connection between the two stops
# dist_matrix <- distances(full_graph, v = start_nodes, to = target_nodes,
#                          mode = "out")
# min(dist_matrix)
# ind <- arrayInd(which.min(dist_matrix), dim(dist_matrix))
#
# # grab the two nodes that represent the starting and finishing stops
# best_start <- rownames(dist_matrix)[ind[,1]]
# best_finish <- colnames(dist_matrix)[ind[,2]]
#
# # get the full path between the two
# res <- full_graph %>%
#   shortest_paths(from = best_start, to = best_finish,
#                  mode = "out", output = "both")
#
# node_list <- names(Filter(function(x) length(x) > 0, res$vpath)[[1]])
#
# node_path <- graph_data %>%
#   filter(node_id %in% node_list) %>%
#   arrange(arrival_time_sec)
#
# rm(dist_matrix, ind, node_path, res, best_finish, best_start,
#    node_list, start_nodes, target_nodes)

# seems to still work fine with non-library directions

# # Directions with libraries
#
# start_node <- "Main-1-in"
#
# target_nodes <- full_graph %>%
#   activate("nodes") %>%
#   as_tibble() %>%
#   filter(library == "Northeast",
#          node_type == "library_out") %>%
#   pull(name)
#
# res <- full_graph %>%
#   shortest_paths(from = start_node, to = target_nodes, mode = "out",
#                  output = "both")
#
# node_list <- names(Filter(function(x) length(x) > 0, res$vpath)[[1]])
#
# node_path <- full_graph %>%
#   activate("nodes") %>%
#   as_tibble() %>%
#   filter(name %in% node_list) %>%
#   arrange(arrival_time_sec)
#
# # also seems to be routing to/from libraries correctly!
#
# rm(node_path, res, node_list, start_node, target_nodes)

# # TSP Setup ---------------------------------------------------------------
#
# # Pull distance matrix between all library-in nodes
# library_nodes <- full_nodes %>%
#   filter(node_type == "library_in") %>%
#   arrange(library, arrival_time_sec) %>%
#   pull(name)
#
# library_indices <- full_nodes %>%
#   filter(node_type == "library_in") %>%
#   arrange(library, arrival_time_sec) %>%
#   count(library) %>%
#   mutate(end = cumsum(n),
#          start = if_else(row_number() == 1, 1, lag(end) + 1)) %>%
#   select(library, start, end)
#
# system.time({
#   dist_matrix <- full_graph %>%
#     distances(v = library_nodes, to = library_nodes, mode = "out")
# })
#
# dist_matrix[dist_matrix == 0] <- Inf
#
# # Noon-Bean Transformation for Generalized TSP
# #
# # See Ben-Arieh, et al. "Transformations of generalized ATSP into ATSP" (2003)
# # for details on this transformation (and others)
#
# L <- nrow(library_indices)
# N <- length(library_nodes)
# M <- 1e6 # arbitrarily large "penalty" constant
#
# # add penalty to all edges (will only affect inter-cluster edges in the end)
# NB_dist_matrix <- (dist_matrix + M)
#
# for (i in 1:L) {
#   a <- library_indices$start[i]
#   b <- library_indices$end[i]
#
#   # clear all intra-cluster edges
#   NB_dist_matrix[a:b, a:b] <- Inf
#
#   # create intra-cluster cycle of weight 0
#   NB_dist_matrix[b, a] <- 0
#   for (j in a:(b - 1)) {
#     NB_dist_matrix[j, j + 1] <- 0
#   }
#
#   col_list <- c()
#   if (i < L) {
#     col_list <- c((b + 1):N, col_list)
#   }
#   if (i > 1) {
#     col_list <- c(1:(a - 1), col_list)
#   }
#
#   # shift all inter_cluster edges to account for intra-cluster cycle
#   for (k in col_list) {
#     NB_dist_matrix[a:b, k] <- lead(NB_dist_matrix[a:b, k],
#                                   default = NB_dist_matrix[a, k])
#   }
# }
#
# rm(a, b, i, j, k, col_list)
#
# # change Infs to 1000*M (for tracking)
# NB_dist_matrix[is.infinite(NB_dist_matrix)] <- 1000*M
#
# # create "dummy city" to enable Hamiltonian path optimization
# dummy <- rep(100*M, N)
# NB_dist_matrix <- cbind(NB_dist_matrix, dummy)
#
# dummy <- c(rep(0, N), 1000*M)
# NB_dist_matrix <- rbind(NB_dist_matrix, dummy)
#
# rm(dummy, dist_matrix)
#
# # TSP "Solution" ----------------------------------------------------------
#
# books_and_buses <- ATSP(NB_dist_matrix)
#
# # start_node <- which(library_nodes == "Main-1-in")
# #
# system.time({
#   tour <- solve_TSP(books_and_buses, method = "cheapest_insertion")
# })
# tour_duration <- tour_length(tour)
#
# route <- tour %>%
#   as_tibble(rownames = "node") %>%
#   filter(node != "dummy")
#
# start_node <- first(route$node)
#
# order <- route %>%
#   pull(node) %>%
#   str_split(pattern = "-") %>%
#   as_tibble_col() %>%
#   rowwise() %>%
#   mutate(library = first(value)) %>%
#   select(library) %>%
#   distinct()
#
# cost <- rep(NA, N)
#
# for (i in 1:(N - 1)) {
#   cost[i] <- NB_dist_matrix[tour[i], tour[i + 1]]
# }
#
# route <- route %>%
#   cbind(cost) %>%
#   mutate(start = node, finish = lead(node)) %>%
#   select(start, finish, cost) %>%
#   filter(cost != 0)
#
# route_nodes <- c(start_node, route$finish)
#
# route_info <- full_nodes %>%
#   filter(name %in% route_nodes) %>%
#   mutate(arrival_time_sec = hms::hms(arrival_time_sec)) %>%
#   arrange(arrival_time_sec)
#
# rm(order, route, cost, dummy, i, route_nodes, start_node,
#    dist_matrix, NB_dist_matrix, tour_duration)
#
# # seems like a plausible route for the secondary goal!
#
# node_list <- c()
#
# start_node <- full_nodes %>%
#   filter(library == route_info$library[1],
#          node_type == "library_in") %>%
#   pull(name) %>%
#   first()
#
# for (i in 1:(L - 1)) {
#   target_nodes <- full_nodes %>%
#     filter(library == route_info$library[i + 1],
#            node_type == "library_in") %>%
#     pull(name)
#
#   res <- full_graph %>%
#     shortest_paths(from = start_node, to = target_nodes,
#                    mode = "out", output = "both")
#
#   node_list <- c(node_list,
#                  names(Filter(function(x) length(x) > 0, res$vpath)[[1]]))
#
#   start_node <- last(node_list)
# }
#
# node_path <- full_nodes %>%
#   filter(name %in% node_list) %>%
#   arrange(arrival_time_sec)
#
# (last(node_path$arrival_time) - first(node_path$arrival_time)) %>%
#   seconds_to_period() %>%
#   hms::hms() %>%
#   print()
#
# rm(node_list, start_node, i, target_nodes, M, route_info, res,
#    library_indices, tour)

# # Testing TSP Solutions ---------------------------------------------------
#
# system.time({
# all_routes <- library_nodes %>%
#   as_tibble() %>%
#   mutate(row = row_number()) %>%
#   rowwise() %>%
#   mutate(raw_length = books_and_buses %>%
#            solve_TSP(method = "nn", start = row) %>%
#            tour_length())
# })
#
# feasible_routes <- all_routes %>%
#   filter(raw_length < 1e9) %>%
#   mutate(length = raw_length - 116e6) %>%
#   select(start_node = value, length) %>%
#   arrange(length)
#
# tour <- solve_TSP(books_and_buses, method = "nn",
#                   start = which(library_nodes == feasible_routes$start_node[1]))
# tour_length(tour)
#
# route <- tour %>%
#   as_tibble(rownames = "node") %>%
#   filter(node != "dummy")
#
# start_node <- first(route$node)
#
# order <- route %>%
#   pull(node) %>%
#   str_split(pattern = "-") %>%
#   as_tibble_col() %>%
#   rowwise() %>%
#   mutate(library = first(value)) %>%
#   select(library) %>%
#   distinct()
#
# cost <- rep(NA, N)
#
# for (i in 1:(N - 1)) {
#   cost[i] <- NB_dist_matrix[tour[i], tour[i + 1]]
# }
#
# route <- route %>%
#   cbind(cost) %>%
#   mutate(start = node, finish = lead(node)) %>%
#   select(start, finish, cost) %>%
#   filter(cost != 0)
#
# route_nodes <- c(start_node, route$finish)
#
# route_info <- full_nodes %>%
#   filter(name %in% route_nodes) %>%
#   arrange(arrival_time_sec)
#
# node_list <- c()
#
# start_node <- route_info$name[1]
#
# for (i in 1:(L - 1)) {
#   target_nodes <- full_nodes %>%
#     filter(library == route_info$library[i + 1],
#            node_type == "library_in") %>%
#     pull(name)
#
#   res <- full_graph %>%
#     shortest_paths(from = start_node, to = target_nodes,
#                    mode = "out", output = "both")
#
#   node_list <- c(node_list,
#                  names(Filter(function(x) length(x) > 0, res$vpath)[[1]]))
#
#   start_node <- last(node_list)
# }
#
# node_path <- full_nodes %>%
#   filter(name %in% node_list) %>%
#   arrange(arrival_time_sec)
#
# (last(node_path$arrival_time) - first(node_path$arrival_time)) %>%
#   seconds_to_period() %>%
#   hms::hms() %>%
#   print()

# Concorde Solvers? -------------------------------------------------------

# books_and_buses %>%
#   reformulate_ATSP_as_TSP() %>%
#   write_TSPLIB(here("data", "books_and_buses.tsp"), precision = -1)

# concorde_path(file.path("C:", "cygwin64", "bin"))

# Open Hours Only ---------------------------------------------------------

library_info <- here("data", "library_info.csv") %>%
  read_csv()

open_nodes <- full_nodes %>%
  filter(node_type == "library_in") %>%
  arrange(library, arrival_time_sec) %>%
  rowwise() %>%
  mutate(open_time = library_info$open_time[library_info$library_name == library],
         close_time = library_info$close_time[library_info$library_name == library]) %>%
  ungroup() %>%
  filter(arrival_time >= open_time,
         arrival_time <= close_time)

library_indices <- open_nodes %>%
  count(library) %>%
  mutate(end = cumsum(n),
         start = if_else(row_number() == 1, 1, lag(end) + 1),
         total = end - start + 1) %>%
  select(library, start, end, total)

library_nodes <- open_nodes %>%
  pull(name)

system.time({
  dist_matrix <- full_graph %>%
    distances(v = library_nodes, to = library_nodes, mode = "out")
})

dist_matrix[dist_matrix == 0] <- Inf

# Noon-Bean Transformation for Generalized TSP
#
# See Ben-Arieh, et al. "Transformations of generalized ATSP into ATSP" (2003)
# for details on this transformation (and others)

L <- nrow(library_indices)
N <- length(library_nodes)
M <- 1e6 # arbitrarily large "penalty" constant

# add penalty to all edges (will only affect inter-cluster edges in the end)
NB_dist_matrix <- (dist_matrix + M)

for (i in 1:L) {
  a <- library_indices$start[i]
  b <- library_indices$end[i]

  # clear all intra-cluster edges
  NB_dist_matrix[a:b, a:b] <- Inf

  # create intra-cluster cycle of weight 0
  NB_dist_matrix[b, a] <- 0
  for (j in a:(b - 1)) {
    NB_dist_matrix[j, j + 1] <- 0
  }

  col_list <- c()
  if (i < L) {
    col_list <- c((b + 1):N, col_list)
  }
  if (i > 1) {
    col_list <- c(1:(a - 1), col_list)
  }

  # shift all inter_cluster edges to account for intra-cluster cycle
  for (k in col_list) {
    NB_dist_matrix[a:b, k] <- lead(NB_dist_matrix[a:b, k],
                                   default = NB_dist_matrix[a, k])
  }
}

rm(a, b, i, j, k, col_list)

# change Infs to 1000*M (for tracking)
NB_dist_matrix[is.infinite(NB_dist_matrix)] <- 1000*M

# create "dummy city" to enable Hamiltonian path optimization
dummy <- rep(100*M, N)
NB_dist_matrix <- cbind(NB_dist_matrix, dummy)

dummy <- c(rep(0, N), 1000*M)
NB_dist_matrix <- rbind(NB_dist_matrix, dummy)

books_and_buses <- ATSP(NB_dist_matrix)

concorde_path('C:/cygwin64/home/mda2894/concorde/LINKERN/')

linkern <- solve_TSP(books_and_buses, method = "linkern", as_TSP = T,
                     exe = 'C:/cygwin64/home/mda2894/concorde/LINKERN/')

# # Time Dependent Distance Matrix ------------------------------------------
#
# library_nodes <- open_nodes %>%
#   arrange(arrival_time) %>%
#   pull(name)
#
# system.time({
#   dist_matrix <- full_graph %>%
#     distances(v = library_nodes, to = library_nodes, mode = "out")
# })
