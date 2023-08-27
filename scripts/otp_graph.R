# Building OTP graph

# Setup -------------------------------------------------------------------

library(conflicted)
library(here)
library(doParallel)
library(tidyverse)

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

otp_graph_data <-  otp_data %>%
  arrange(from, to, start_time) %>%
  group_by(from, to) %>%
  mutate(waiting = (edge_length - lead(edge_length)) == 1,
         walk_node = lead(edge_length) == edge_length | lag(edge_length) == edge_length,
         transit_node = !waiting & !walk_node) %>%
  ungroup() %>%
  arrange(from, to, start_time)

keep <- otp_graph_data %>%
  mutate(keep = edge_length - lead(edge_length) != 1) %>%
  filter(keep)

walk_nodes <- otp_graph_data %>%
  filter(walk_node)

transit_nodes <- otp_graph_data %>%
  filter(transit_node)

nine_am <- as.POSIXct("10-25-2023 09:00:00", format = "%m-%d-%Y %H:%M:%S")
nine_pm <- as.POSIXct("10-25-2023 21:00:00", format = "%m-%d-%Y %H:%M:%S")

diff <- full_join(walk_nodes, transit_nodes) %>%
  full_join(keep) %>%
  arrange(from, to, start_time) %>%
  filter(keep & (walk_node | transit_node),
         edge_length <= 180,
         nine_am < start_time & start_time < nine_pm)
