# OpenTripPlanner

# Setup -------------------------------------------------------------------

options(java.parameters = "-Xmx8G")

library(conflicted)
library(here)
library(tidyverse)
library(doParallel)
library(opentripplanner)
library(tmap)
library(osmextract)
library(sf)

# OpenTripPlanner equires Java 8
otp_check_java()

conflicts_prefer(
  dplyr::filter,
  dplyr::lag
)

registerDoParallel()

# # To use a persistent directory for osm files run:
# usethis::edit_r_environ(scope = "project")
# # and add a line containing: OSMEXT_DOWNLOAD_DIRECTORY=/path/to/save/files
# # then restart R session

# Demo --------------------------------------------------------------------

# jar_path <- otp_dl_jar(here("data", "OTP"), cache = F)
#
# data_path <- here("data", "OTP", "demo")
# dir.create(data_path)
# otp_dl_demo(data_path)
#
# log1 <- otp_build_graph(jar_path, data_path)
# log2 <- otp_setup(jar_path, data_path)
# otp_con <- otp_connect(timezone = "Europe/London")
#
# route <- otp_plan(otp_con,
#                   fromPlace = c(-1.17502, 50.64590),
#                   toPlace = c(-1.15339, 50.72266))
#
# tmap_mode("view")
# qtm(route)
#
# otp_stop()

# Data --------------------------------------------------------------------

otp_path <- here("data", "OTP", "otp-1.5.0-shaded.jar")

# # uncomment this block to create file structure
# dir.create(here("data", "OTP", "bnb"))
# dir.create(here("data", "OTP", "bnb", "graphs"))
# dir.create(here("data", "OTP", "bnb", "graphs", "default"))

bnb_path <- here("data", "OTP", "bnb")

# data_path <- here("data", "OTP", "bnb", "graphs", "default")

# # uncomment this block to download new TARC GTFS file
# tarc_feed <- "http://googletransit.ridetarc.org/feed/google_transit.zip"
# download.file(tarc_feed, destfile = here(data_path, "tarc_gtfs.zip"))

# # Download kentucky osm data here. Cropped down to Louisville with osmosis.
# osm_url <- "https://download.geofabrik.de/north-america/us/kentucky-latest.osm.pbf"
# oe_download(osm_url, download_directory = data_path)

# # Build graph
# graph <- otp_build_graph(otp_path, bnb_path)

setup <- otp_setup(otp_path, bnb_path)
otp_con <- otp_connect()

library_info <- read_csv(here("data", "library_info.csv")) %>%
  st_as_sf(coords = c("library_lon", "library_lat"))

# Functions ---------------------------------------------------------------

get_otp_edges <- function(otp_con, from, to, start_time, lib_order) {
  edges <- otp_con %>%
    otp_plan(from, to, fromID = from$library_name, toID = to$library_name,
             date_time = start_time, mode = c("BUS", "WALK"),
             maxWalkDistance = 12000, get_geometry = F, numItineraries = 1,
             distance_balance = T) %>%
    group_by(fromPlace, toPlace) %>%
    arrange(endTime) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    mutate(edge_length = ceiling(endTime - start_time),
           from = factor(fromPlace, levels = lib_order),
           to = factor(toPlace, levels = lib_order),
           start_time = start_time) %>%
    select(from, to, edge_length, start_time) %>%
    arrange(from, to)

  return(edges)
}

# Distance Matrix ---------------------------------------------------------

L <- nrow(library_info)
lib_order <- library_info$library_name

to   = library_info[rep(seq(1, L), times = L),]
from = library_info[rep(seq(1, L), each  = L),]

start <- as.POSIXct("10-25-2023 06:00:00", format = "%m-%d-%Y %H:%M:%S")

otp_edges <- tibble()

for (i in 1:600) {
  offset <- 60 * (i - 1)
  edges <- get_otp_edges(otp_con, from, to, start + offset, lib_order)
  otp_edges <- rbind(otp_edges, edges)
}

otp_edges_nodes <- otp_edges %>%
  arrange(from, to, start_time) %>%
  group_by(from, to) %>%
  mutate(remove = (edge_length - lead(edge_length)) == 1,
         transit_node = lead(edge_length) > edge_length) %>%
  ungroup() %>%
  filter(!remove)

# full_td_matrix <- array(dim = c(L, L, 18*60))
#
# system.time({
#   for (i in 1:60) {
#   offset <- 60 * (i - 1)
#   distance_matrix <- otp_con %>%
#     get_td_distance_matrix(from, to, start + offset, lib_order)
#
#   full_td_matrix[,,i] <- distance_matrix
#   }
# })
#
# save(full_td_matrix, file = here("data", "full_td_matrix.RData"))

otp_stop()
