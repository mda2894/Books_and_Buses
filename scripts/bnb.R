# Writing my own Branch and Bound algorithm

# Setup -------------------------------------------------------------------

library(tidyverse)

# Toy Data ----------------------------------------------------------------

# sim_atsp <- function(n_cities, grid_size = 100, seed = NULL) {
#   if (!is.null(seed)) {set.seed(seed)}
#
#   x_coords <- sample(1:100, n_cities)
#   y_coords <- sample(1:100, n_cities)
#   cities <- cbind(x_coords, y_coords)
#   dists <- as.matrix(dist(cities, method = "manhattan"))
#
#   atsp <- matrix(0, nrow = n_cities, ncol = n_cities)
#
#   for (i in 1:length(dists)) {
#     dist <- ceiling(rnorm(1, mean = dists[i], sd = sqrt(dists[i])))
#     atsp[i] <- ifelse(dist > 1, dist, 1)
#   }
#
#   diag(atsp) <- 0
#
#   return(atsp)
# }
#
# library(here)
#
# # symmetric 5 city toy problem
# test5 <- here("data", "five_d.csv") %>%
#   read_csv(col_name = F) %>%
#   as.matrix()
#
# # symmetric TSP with all 48 contiguous state capitals
# att48 <- here("data", "att48_d.csv") %>%
#   read_csv(col_name = F) %>%
#   as.matrix()
#
# # simulated asymmetric 9 city toy problem
# atsp9 <- sim_atsp(9, seed = 2894)

# Lower Bound -------------------------------------------------------------

calc_lower_bound <- function(dist_matrix, nodes = NULL) {
  # Calculate a simple lower bound for the cost of a Hamiltonian path
  # using distance matrix and (optional) vector of nodes to include in the path
  n <- nrow(dist_matrix)
  if (is.null(nodes)) {
    nodes <- 1:n
  }
  lower_bound <- 0

  if (length(nodes == n)) {
    if (isSymmetric(unname(dist_matrix))) {
      for (node in nodes) {
        edges <- dist_matrix[node,]
        min_edge <- min(edges[edges > 0])

        lower_bound <- lower_bound + min_edge
      }
    } else {
      for (node in nodes) {
        out_edges <- dist_matrix[node,]
        min_out_edge <- min(out_edges[out_edges > 0])

        in_edges <- dist_matrix[,node]
        min_in_edge <- min(in_edges[in_edges > 0])

        lower_bound <- lower_bound + 0.5 * (min_out_edge + min_in_edge)
      }
    }
  } else {
    visited <- setdiff(1:n, nodes)

    for (node in nodes) {
      edges <- dist_matrix[visited, node]
      min_edge <- min(edges[edges > 0])

      lower_bound <- lower_bound + min_edge
    }
  }
  return(as.numeric(lower_bound))
}

# # Testing
# calc_lower_bound(test5)
# calc_lower_bound(att48)
# calc_lower_bound(atsp9)

# Path Cost Calculation ---------------------------------------------------

calc_path_distance <- function(distance_matrix, path = NULL, start = NULL,
                               return_to_start = F) {
  # Calculate the distance of a given path, for a given distance matrix,
  # with the option to return to the start of the path at the end
  if (is.null(path)) {
    path <- 1:nrow(distance_matrix)
  }
  n <- length(path)

  distance <- 0
  for (i in 1:(n - 1)) {
    distance <- distance + distance_matrix[path[i], path[i + 1]]
  }
  if (return_to_start)  {
    if (is.null(start)) {
      distance <- distance + distance_matrix[path[n], path[1]]
    } else {
      distance <- distance + distance_matrix[path[n], start]
    }
  }

  return(as.numeric(distance))
}

# # Testing
# calc_path_distance(test5)
# calc_path_distance(test5, return_to_start = T)
# calc_path_distance(test5, path = c(2, 4))
# calc_path_distance(test5, path = c(2, 4), return_to_start = T)
# calc_path_distance(test5, path = c(2, 4), return_to_start = T, start = 1)

# Nearest Neighbors -------------------------------------------------------

tsp_nn <- function(distance_matrix, start = NULL, return_to_start = F) {
  # Nearest neighbors algorithm for solving a TSP
  #
  # Defaults to starting at the first node in the distance matrix
  # but can also find the best nearest neighbors path from all starting nodes
  # or just the path from a given node
  start <- ifelse(is.null(start), 1, start)
  n <- nrow(distance_matrix)

  if (start == "best") { # calculate all nn paths and return best one
    best <- list(path = c(), distance = Inf)

    for (i in 1:n) {
      current <- distance_matrix %>%
        tsp_nn(start = i, return_to_start = return_to_start)
      if (current$distance < best$distance) {
        best <- current
      }
    }

    return(best)
  }
  else { # just calculate single nn path
    path <- numeric(n)
    visited <- rep(F, n)
    path_distance <- 0

    current_node <- start
    path[1] <- current_node
    visited[current_node] <- T

    for (i in 2:n) {
      distances <- distance_matrix[current_node, ]
      distances[visited] <- Inf
      next_node <- which.min(distances)
      path[i] <- next_node
      visited[next_node] <- T
      path_distance <- path_distance + distance_matrix[current_node, next_node]
      current_node <- next_node
    }

    if (return_to_start) {
      path_distance <- path_distance + distance_matrix[current_node, start]
    }

    return(list(path = path, distance = as.numeric(path_distance)))
  }
}

# # Testing
# tsp_nn(test5)
# tsp_nn(test5, start = 2)
# tsp_nn(test5, start = 2, return_to_start = T)
# tsp_nn(test5, start = "best")
# tsp_nn(att48)
# tsp_nn(att48, start = "best")
# tsp_nn(atsp9)
# tsp_nn(atsp9, start = "best")

# Branch and Bound --------------------------------------------------------

tsp_bnb <- function(distance_matrix, start = NULL, return_to_start = F) {
  # Simple branch and bound algorithm for solving a TSP
  n <- nrow(distance_matrix)
  start <- ifelse(is.null(start), 1, start)

  best <- distance_matrix %>%
    tsp_nn(start = start, return_to_start = return_to_start)

  if (start == "best") {
    for (i in 1:n) {
      current <- distance_matrix %>%
        tsp_bnb(start = i, return_to_start = return_to_start)

      if (current$distance < best$distance) {
        best <- current
      }
    }
  } else {
    queue <- list(list(path = start, lower_bound = 0))

    while (length(queue) > 0) {
      node <- queue[[1]]
      queue <- queue[-1]

      if (node$lower_bound < best$distance) {
        unvisited <- setdiff(1:n, node$path)
        for (city in unvisited) {
          child_path <- c(node$path, city)

          if (length(child_path) == n) {
            total_distance <- distance_matrix %>%
              calc_path_distance(path = child_path, start = start,
                                 return_to_start = return_to_start)
            if (total_distance < best$distance) {
              best$distance <- total_distance
              best$path <- child_path
            }
          } else {
            child_distance <- distance_matrix %>%
              calc_path_distance(path = child_path)
            child_lower_bound <- distance_matrix %>%
              calc_lower_bound(nodes = unvisited)
            lower_bound <- child_distance + child_lower_bound
            child_node <- list(list(path = child_path, lower_bound = lower_bound))
            queue <- append(queue, child_node)
          }
        }
        queue <- queue[order(sapply(queue, function(x) x$lower_bound))]
      }
    }
  }
  return(best)
}

# tsp_bnb(test5)
# tsp_bnb(test5, start = "best")
# tsp_bnb(atsp9)
# tsp_bnb(atsp9, start = "best")
#
# dat <- sim_atsp(10)
# tsp_bnb(dat, start = "best")
