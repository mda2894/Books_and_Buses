# Setup -------------------------------------------------------------------

library(conflicted)
library(here)
library(tidyverse)
library(tidytransit)
library(igraph)
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
  mutate(node_id = paste(trip_id, stop_sequence, sep = "-"))

# Trip Graph --------------------------------------------------------------

trip_graph_nodes <- graph_data %>%
  mutate(node_type = "bus_stop") %>%
  select(name = node_id, node_type, route_id, trip_id, direction_id, stop_sequence,
         stop_id, arrival_time, stop_lat, stop_lon) %>%
  arrange(trip_id, stop_sequence)

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
         trip_leg = stop_sequence) %>%
  arrange(trip_id, trip_leg)

# "rooted forest" with each trip a separate tree
trip_graph <- tbl_graph(trip_graph_nodes, trip_graph_edges, directed = T)
# print(trip_graph)

# Add (Same-Stop) Transfers -----------------------------------------------

transfer_graph_nodes <- trip_graph_nodes %>%
  arrange(stop_id, arrival_time)

transfer_graph_edges <- graph_data %>%
  group_by(stop_id) %>%
  arrange(arrival_time) %>%
  mutate(from = node_id,
         to = lead(node_id),
         distance = 0,
         weight = lead(arrival_time) - arrival_time) %>%
  filter(row_number() <= n() - 1) %>%
  ungroup() %>%
  mutate(edge_type = "transfer") %>%
  arrange(stop_id, arrival_time) %>%
  select(from, to, edge_type, weight, distance)

transfer_graph <- tbl_graph(transfer_graph_nodes, transfer_graph_edges,
                            directed = T)
# print(transfer_graph)

bus_only_graph <- graph_join(trip_graph, transfer_graph)
# print(bus_only_graph)

# Bus-Only (Forrest Gump) Routing -----------------------------------------

# graph_data %>%
#   filter(grepl("Mall", stop_name)) %>%
#   View()

start_nodes <- bus_only_graph %>%
  activate("nodes") %>%
  as_tibble() %>%
  filter(stop_id == 21285) %>% # Jefferson Mall
  arrange(arrival_time) %>%
  pull(name)

target_nodes <- bus_only_graph %>%
  activate("nodes") %>%
  as_tibble() %>%
  filter(stop_id == 8290) %>% # St. Matthews Mall
  arrange(arrival_time) %>%
  pull(name)

dist_matrix <- distances(bus_only_graph, v = start_nodes, to = target_nodes,
                         mode = "out")
min(dist_matrix)
ind <- arrayInd(which.min(dist_matrix), dim(dist_matrix))

best_start <- rownames(dist_matrix)[ind[,1]]
best_finish <- colnames(dist_matrix)[ind[,2]]

res <- bus_only_graph %>%
  shortest_paths(from = best_start, to = best_finish,
                 mode = "out", output = "both")

node_list <- names(Filter(function(x) length(x) > 0, res$vpath)[[1]])

node_path <- graph_data %>%
  filter(node_id %in% node_list) %>%
  arrange(arrival_time)

# Walking Graph? ----------------------------------------------------------

stops <- tarc_gtfs$stops
