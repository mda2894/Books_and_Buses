# Create full graph (with estimated walking times)

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

# Add Library Nodes ------------------------------------------------------

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
   speed, bus_stops, taxicab_dist)

full_nodes <- full_graph %>%
  activate("nodes") %>%
  as_tibble() %>%
  arrange(arrival_time)
