# Theoretical Minimum

# Setup -------------------------------------------------------------------

library(here)
library(tidyverse)
library(TSP)

# Data --------------------------------------------------------------------

load(here("data", "otp_data.RData"))
load(here("data", "otp_cycling.RData"))
lib_names <- unique(otp_data$from)

# Lower Bound (walking) ---------------------------------------------------

min_dist_matrix <- matrix(NA, 17, 17)

for (i in 1:17) {
  for (j in 1:17) {
    min_dist_matrix[i, j] <- otp_data %>%
      filter(from == lib_names[i],
             to == lib_names[j]) %>%
      summarize(min_edge = min(edge_length)) %>%
      pull(min_edge)
  }
}

minimum <- ATSP(min_dist_matrix, labels = lib_names)

best_tour <- NULL
best_time <- Inf

for (i in 1:1000) {
  current_tour <- solve_TSP(minimum)
  current_time <- tour_length(current_tour)
  if (current_time < best_time) {
    best_time <- current_time
    best_tour <- current_tour
  }
}

# Lower bound of about 10.5 hours

# Lower Bound (w/ Cycling) ------------------------------------------------

min_cycling_matrix <- matrix(NA, 17, 17)

for (i in 1:17) {
  for (j in 1:17) {
    min_cycling_matrix[i, j] <- cycling_data %>%
      filter(from == lib_names[i],
             to == lib_names[j]) %>%
      summarize(min_edge = min(edge_length)) %>%
      pull(min_edge)
  }
}

min_cycling <- ATSP(min_cycling_matrix, labels = lib_names)

best_cycling_tour <- NULL
best_cycling_time <- Inf

for (i in 1:1000) {
  current_tour <- solve_TSP(min_cycling)
  current_time <- tour_length(current_tour)
  if (current_time < best_time) {
    best_cycling_time <- current_time
    best_cycling_tour <- current_tour
  }
}

# Lower bound of 6.25 hours. Clearly better (now)
