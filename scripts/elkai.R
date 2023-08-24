library(here)

source(here("scripts", "create_full_graph.R"), echo = T)

# All Hours ---------------------------------------------------------

library_info <- here("data", "library_info.csv") %>%
  read_csv()

start_library <- "Southwest"
start_time <- 9*60*60

library_nodes <- full_nodes %>%
  filter(node_type == "library_in",
         arrival_time_sec > start_time) %>%
  mutate(library = factor(library) %>% fct_relevel(start_library)) %>%
  arrange(library, arrival_time_sec)

library_indices <- library_nodes %>%
  count(library) %>%
  mutate(end = cumsum(n),
         start = if_else(row_number() == 1, 1, lag(end) + 1),
         total = end - start + 1) %>%
  select(library, start, end, total)

library_nodes <- library_nodes %>%
  pull(name)

dist_matrix <- full_graph %>%
  distances(v = library_nodes, to = library_nodes, mode = "out")

dist_matrix[dist_matrix == 0] <- Inf

# Noon-Bean Transformation for Generalized TSP
#
# See Ben-Arieh, et al. "Transformations of generalized ATSP into ATSP" (2003)
# for details on this transformation (and others)

L <- nrow(library_indices)
N <- length(library_nodes)
M <- 1e4 # arbitrarily large "penalty" constant

# add penalty to all edges (will only affect inter-cluster edges in the end)
NB_dist_matrix <- round(dist_matrix / 60) + M

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

# change Infs to 100*M (for tracking)
NB_dist_matrix[is.infinite(NB_dist_matrix)] <- 100*M

# create "dummy city" to enable Hamiltonian path optimization
dummy <- rep(10*M, N)
NB_dist_matrix <- cbind(NB_dist_matrix, dummy)

dummy <- c(0, dummy)
NB_dist_matrix <- rbind(NB_dist_matrix, dummy)

# elkai -------------------------------------------------------------------

elkai <- import("elkai")
elkai_dist_matrix <- elkai$DistanceMatrix(r_to_py(NB_dist_matrix)$tolist())
system.time({
  elkai_solution <- elkai_dist_matrix$solve_tsp()
})

tour <- elkai_solution[elkai_solution != N] + 1

route <- library_nodes[tour] %>%
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
  mutate(arrival_time_sec = hms::hms(arrival_time_sec)) %>%
  arrange(arrival_time_sec)
