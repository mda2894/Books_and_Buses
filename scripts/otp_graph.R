# Building OTP graph

# Setup -------------------------------------------------------------------

library(conflicted)
library(here)
library(doParallel)
library(tidyverse)
library(tidygraph)

conflicts_prefer(
  dplyr::filter,
  dplyr::lag
)

registerDoParallel()

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
         keep = walk_node | transit_node) %>%
  ungroup() %>%
  filter(keep,
         edge_length <= 180) %>%
  select(from, to, edge_length, start_time) %>%
  arrange(from, start_time, to)

library_out_nodes <- graph_data %>%
  mutate(node_id = paste(from, "out", format(start_time, "%H:%M"), sep = "-")) %>%
  select(node_id)

library_in_nodes <- graph_data %>%
  mutate(node_id = paste(to, "in", format(start_time + edge_length, "%H:%M"), sep = "-")) %>%
  select(node_id)
