# Building OTP graph

# Setup -------------------------------------------------------------------

library(conflicted)
library(here)
library(doParallel)
library(tidyverse)
library(igraph)
library(tidygraph)
library(TSP)
library(reticulate)

use_virtualenv("r-reticulate")
elkai <- import("elkai")

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
#   file_name = paste0("otp_edges_", hour, ".RData")
#   file_path <- here("data", "OTP", "bnb", file_name)
#   load(file_path)
#   otp_data <- rbind(otp_data, otp_edges)
#   rm(otp_edges)
# }
#
# save(otp_data, file = here("data", "otp_data.RData"))

load(here("data", "otp_data.RData"))

graph_data <-  otp_data %>%
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

# target_nodes_info <- full_nodes %>%
#   filter(node_type == "arrival",
#          library != "Fairdale") %>%
#   arrange(library, time) %>%
#   rowwise() %>%
#   mutate(open_time = library_info$open_time[library_info$library_name == library],
#          close_time = library_info$close_time[library_info$library_name == library]) %>%
#   ungroup() %>%
#   filter(time >= open_time,
#          time <= close_time)
#
# library_indices <- target_nodes_info %>%
#   count(library) %>%
#   mutate(end = cumsum(n),
#          start = if_else(row_number() == 1, 1, lag(end) + 1),
#          total = end - start + 1) %>%
#   select(library, start, end, total)
#
# target_nodes <- target_nodes_info %>%
#   pull(name)
#
# dist_matrix <- full_graph %>%
#   distances(v = target_nodes, to = target_nodes, mode = "out")
#
# dist_matrix[dist_matrix == 0] <- Inf
#
# # Noon-Bean Transformation for Generalized TSP
# #
# # See Ben-Arieh, et al. "Transformations of generalized ATSP into ATSP" (2003)
# # for details on this transformation (and others)
#
# L <- nrow(library_indices)
# N <- length(target_nodes)
# M <- 1e4 # arbitrarily large "penalty" constant
#
# # add penalty to all edges (will only affect inter-cluster edges in the end)
# NB_dist_matrix <- dist_matrix + M
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
#                                    default = NB_dist_matrix[a, k])
#   }
# }
#
# rm(a, b, i, j, k, col_list)
#
# # change Infs to 1000*M (for tracking)
# NB_dist_matrix[is.infinite(NB_dist_matrix)] <- 1000*M
#
#
# # create "dummy city" to enable Hamiltonian path optimization
# dummy <- rep(100*M, N)
# NB_dist_matrix <- cbind(NB_dist_matrix, dummy)
#
# dummy <- c(rep(0, N), 100*M)
# NB_dist_matrix <- rbind(NB_dist_matrix, dummy)
#
# # TSP ---------------------------------------------------------------------
#
# otp_books_and_buses <- ATSP(NB_dist_matrix)
#
# tour <- solve_TSP(otp_books_and_buses, method = "nn", start = 1142)
# tour_duration <- tour_length(tour)

# Elkai -------------------------------------------------------------------

# elkai_dist_matrix <- elkai$DistanceMatrix(r_to_py(NB_dist_matrix)$tolist())
# elkai_solution <- elkai_dist_matrix$solve_tsp()
#
# tour <- elkai_solution[elkai_solution != N] + 1
#
# route <- target_nodes[tour] %>%
#   as_tibble() %>%
#   select(node = value) %>%
#   filter(row_number() < n())
#
# start_node <- first(route$node)
#
# order <- route %>%
#   pull(node) %>%
#   str_split(pattern = "-") %>%
#   as_tibble_col() %>%
#   rowwise() %>%
#   mutate(library = first(value)) %>%
#   ungroup() %>%
#   select(library) %>%
#   distinct()
#
# cost <- rep(NA, N)
#
# for (i in 1:(N - 1)) {
#   cost[i] <- NB_dist_matrix[tour[i], tour[i + 1]]
# }
#
# legs <- route %>%
#   cbind(cost) %>%
#   mutate(start = node, finish = lead(node)) %>%
#   select(start, finish, cost) %>%
#   filter(cost != 0)
#
# route_nodes <- c(start_node, legs$finish)
#
# route_info <- full_nodes %>%
#   filter(name %in% route_nodes) %>%
#   arrange(time)

# Elkai Function ----------------------------------------------------------

get_elkai_solution <- function(start_library, start_time, exclude = NULL) {
  target_nodes_info <- full_nodes %>%
    filter(node_type == "arrival",
           time >= start_time,
           !(library %in% exclude)) %>%
    mutate(library = factor(library) %>% fct_relevel(start_library)) %>%
    arrange(library, time)

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

  # change Infs to 100*M (for tracking)
  NB_dist_matrix[is.infinite(NB_dist_matrix)] <- 100*M

  # create "dummy city" to enable Hamiltonian path optimization
  dummy <- rep(10*M, N)
  NB_dist_matrix <- cbind(NB_dist_matrix, dummy)

  dummy <- c(0, dummy)
  NB_dist_matrix <- rbind(NB_dist_matrix, dummy)

  elkai_dist_matrix <- elkai$DistanceMatrix(r_to_py(NB_dist_matrix)$tolist())
  elkai_solution <- elkai_dist_matrix$solve_tsp()

  tour <- elkai_solution[elkai_solution != N] + 1

  route <- target_nodes[tour] %>%
    as_tibble() %>%
    select(node = value) %>%
    filter(row_number() < n())

  start_node <- first(route$node)

  order <- route %>%
    pull(node) %>%
    str_split(pattern = "-") %>%
    as_tibble_col() %>%
    rowwise() %>%
    mutate(library = first(value)) %>%
    ungroup() %>%
    select(library) %>%
    distinct()

  cost <- rep(NA, N)

  for (i in 1:(N - 1)) {
    cost[i] <- NB_dist_matrix[tour[i], tour[i + 1]]
  }

  legs <- route %>%
    cbind(cost) %>%
    mutate(start = node, finish = lead(node)) %>%
    select(start, finish, cost) %>%
    filter(cost != 0)

  route_nodes <- c(start_node, legs$finish)

  route_info <- full_nodes %>%
    filter(name %in% route_nodes) %>%
    arrange(time)

  return(list(route_info = route_info, legs = legs, cost = sum(legs$cost)))
}

# Big Results List --------------------------------------------------------

# every_half_hour <- list(six_am = list(), six_thirty = list(), seven_am = list(),
#                         seven_thirty = list(), eight_am = list(), eight_thirty = list(),
#                         nine_am = list())
#
# system.time({
#   for (i in 1:7) {
#     start_time <- SIX_AM + (i - 1) * THIRTY_MINUTES
#     for (library_name in library_info$library_name) {
#       elkai_res <- get_elkai_solution(library_name, start_time)
#       every_half_hour[[i]] <- append(every_half_hour[[i]], list(elkai_res))
#     }
#   }
# })
#
# save(every_half_hour, file = here("data", "every_half_hour.RData"))

# load(here("data", "every_half_hour.RData"))
#
# times <- every_half_hour %>%
#   lapply(function(x) x %>% lapply(function(y) y$cost))
#
# best_times <- lapply(times, function(x) min(unlist(x)))
# which_time <- which.min(best_times)
# which_library <- lapply(times, function(x) which.min(unlist(x)))[[which_time]]
#
# best_overall <- every_half_hour[[which_time]][[which_library]]
# best_overall$route_info
#
# nine_am_starts <- lapply(every_half_hour$nine_am, function(x) x$route_info[x$cost < 1000000])
# print(nine_am_starts)

# welp, doesn't seem feasible

# Exclude Libraries -------------------------------------------------------

exclude <- "Fairdale"

lib_list <- library_info %>%
  select(library_name) %>%
  filter(!(library_name %in% exclude)) %>%
  pull(library_name)

no_fairdale <- list()

for (lib in lib_list) {
  elkai_res <- get_elkai_solution(lib, NINE_AM, exclude)
  no_fairdale <- append(no_fairdale, list(elkai_res))
}

for (route in no_fairdale) {
  print(route$route_info)
}

save(no_fairdale, file =  here("data", "no_fairdale.RData"))
