# Building OTP Cycling graph

# Setup -------------------------------------------------------------------

library(conflicted)
library(here)
library(doParallel)
library(tidyverse)
library(igraph)
library(tidygraph)
library(TSP)
library(reticulate)

# use_virtualenv("r-reticulate")
# elkai <- import("elkai")

conflicts_prefer(
  dplyr::filter,
  dplyr::lag
)

registerDoParallel()

# Constants ---------------------------------------------------------------

DATE <- "10-25-2023"
SIX_AM <- as.POSIXct(paste(DATE, "06:00:00"), format = "%m-%d-%Y %H:%M:%S")
NINE_AM <- as.POSIXct(paste(DATE, "09:00:00"), format = "%m-%d-%Y %H:%M:%S")
MIDNIGHT <- as.POSIXct(paste(DATE, "24:00:00"), format = "%m-%d-%Y %H:%M:%S")
BUSY_TIME <- duration(0, "minutes")
THIRTY_MINUTES <- duration(30, "minutes")

# Data --------------------------------------------------------------------

# otp_data <- tibble()
#
# for (hour in 6:23) {
#   file_name = paste0("otp_cycling_", hour, ".RData")
#   file_path <- here("data", file_name)
#   load(file_path)
#   otp_data <- rbind(otp_data, otp_edges)
#   rm(otp_edges)
# }
#
# save(otp_data, file = here("data", "otp_cycling.RData"))

load(here("data", "otp_cycling.RData"))

graph_data <- cycling_data %>%
  arrange(from, to, start_time) %>%
  group_by(from, to) %>%
  mutate(waiting = (edge_length - lead(edge_length)) == 1,
         walk_node = !waiting & (lead(edge_length) == edge_length | lag(edge_length) == edge_length),
         transit_node = !waiting & !walk_node,
         keep = walk_node | transit_node,
         finish_time = start_time + edge_length,
         from_library = from,
         to_library = to,
         from = paste(from_library, "departure", format(start_time, "%H:%M"), sep = "-"),
         to = paste(to_library, "arrival", format(finish_time, "%H:%M"), sep = "-")) %>%
  ungroup() %>%
  filter(keep,
         edge_length <= 180,
         finish_time < MIDNIGHT) %>%
  select(from, to, edge_length, start_time, finish_time, from_library, to_library) %>%
  arrange(from_library, start_time, to_library)

# Building Graph ----------------------------------------------------------

# Nodes

departure_nodes <- graph_data %>%
  mutate(node_type = "departure") %>%
  select(name = from, time = start_time, library = from_library, node_type) %>%
  distinct(.keep_all = T)

arrival_nodes <- graph_data %>%
  mutate(node_type = "arrival") %>%
  select(name = to, time = finish_time, library = to_library, node_type) %>%
  distinct(.keep_all = T)

all_nodes <- rbind(departure_nodes, arrival_nodes) %>%
  arrange(library, time)

# Edges

travel_edges <- graph_data %>%
  mutate(edge_type = "travel") %>%
  select(from, to, weight = edge_length, everything())

waiting_edges <- departure_nodes %>%
  group_by(library) %>%
  arrange(time) %>%
  mutate(from = name,
         to = lead(name),
         start_time = time,
         finish_time = lead(time),
         weight = finish_time - start_time,
         from_library = library,
         to_library = lead(library),
         edge_type = "waiting") %>%
  ungroup() %>%
  select(from, to, weight, start_time, finish_time, from_library, to_library,
         edge_type) %>%
  drop_na()

busy_edges <- arrival_nodes %>%
  rowwise() %>%
  mutate(to = departure_nodes$name[departure_nodes$library == library & departure_nodes$time >= time + BUSY_TIME][1],
         from = name,
         start_time = time) %>%
  ungroup() %>%
  drop_na() %>%
  select(from, to, start_time, library) %>%
  left_join(departure_nodes, by = join_by(to == name)) %>%
  mutate(weight = time - start_time,
         edge_type = "busy") %>%
  select(from, to, weight, start_time, finish_time = time,
         from_library = library.x, to_library = library.y, edge_type) %>%
  arrange(start_time)

all_edges <- rbind(travel_edges, busy_edges, waiting_edges)

full_graph <- tbl_graph(all_nodes, all_edges)

rm(all_edges, all_nodes, arrival_nodes, busy_edges, departure_nodes, otp_data,
   travel_edges, waiting_edges)

# Distance Matrix ---------------------------------------------------------

full_nodes <- full_graph %>%
  activate("nodes") %>%
  as_tibble() %>%
  arrange(time)

library_info <- here("data", "library_info.csv") %>%
  read_csv() %>%
  mutate(open_time = as.POSIXct(paste(DATE, open_time), format = "%m-%d-%Y %H:%M:%S"),
         close_time = as.POSIXct(paste(DATE, close_time), format = "%m-%d-%Y %H:%M:%S"))

target_nodes_info <- full_nodes %>%
  filter(node_type == "arrival") %>%
  arrange(library, time) %>%
  rowwise() %>%
  mutate(open_time = library_info$open_time[library_info$library_name == library],
         close_time = library_info$close_time[library_info$library_name == library]) %>%
  ungroup() %>%
  filter(time >= open_time,
         time <= close_time)

library_indices <- target_nodes_info %>%
  count(library) %>%
  mutate(end = cumsum(n),
         start = if_else(row_number() == 1, 1, lag(end) + 1),
         total = end - start + 1) %>%
  select(library, start, end, total)

target_nodes <- target_nodes_info %>%
  pull(name)

dist_matrix <- full_graph %>%
  distances(v = target_nodes, to = target_nodes, mode = "out")

dist_matrix[dist_matrix == 0] <- Inf

# Noon-Bean Transformation for Generalized TSP
#
# See Ben-Arieh, et al. "Transformations of generalized ATSP into ATSP" (2003)
# for details on this transformation (and others)

L <- nrow(library_indices)
N <- length(target_nodes)
M <- 1e4 # arbitrarily large "penalty" constant

# add penalty to all edges (will only affect inter-cluster edges in the end)
NB_dist_matrix <- dist_matrix + M

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

dummy <- c(rep(0, N), 100*M)
NB_dist_matrix <- rbind(NB_dist_matrix, dummy)

# TSP ---------------------------------------------------------------------

otp_books_and_buses <- ATSP(NB_dist_matrix)

tour <- solve_TSP(otp_books_and_buses, method = "repetitive_nn")
tour_duration <- tour_length(tour)
