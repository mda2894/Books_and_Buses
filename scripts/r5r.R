# Third attempt
#
# Using r5r
#
# NOTE: r5r requires Java JDK 11
# Download: https://www.oracle.com/java/technologies/javase-jdk11-downloads.html
#

# Setup  ------------------------------------------------------------------

options(java.parameters = "-Xmx8G")

library(conflicted)
library(here)
library(tidyverse)
library(tidytransit)
library(igraph)
library(tidygraph)
library(sf)
library(sfnetworks)
library(osmextract)
library(r5r)
library(doParallel)

conflicts_prefer(
  dplyr::filter,
  dplyr::lag
)

registerDoParallel()

# # To use a persistent directory for osm files run:
# usethis::edit_r_environ(scope = "project")
# # and add a line containing: OSMEXT_DOWNLOAD_DIRECTORY=/path/to/save/files
# # then restart R session

# Data --------------------------------------------------------------------

r5r_path <- here("data", "R5R")

# # uncomment this block to download new TARC GTFS file
# tarc_feed <- "http://googletransit.ridetarc.org/feed/google_transit.zip"
# download.file(tarc_feed, destfile = here(r5r_path, "tarc_gtfs.zip"))

osm_url <- "https://download.geofabrik.de/north-america/us/kentucky-latest.osm.pbf"
oe_download(osm_url, download_directory = r5r_path)

r5r_core <- setup_r5(r5r_path)

library_info <- read_csv(here("data", "library_info.csv"))

# Travel Time Matrix ------------------------------------------------------

libraries <- library_info %>%
  select(id = library_name, lon = library_lon, lat = library_lat)

tt_matrix <- travel_time_matrix(
  r5r_core,
  origins = libraries,
  destinations = libraries,
  mode = "TRANSIT",
  departure_datetime = as.POSIXct("08-30-2023 09:00:00", format = "%m-%d-%Y %H:%M:%S"),
  max_trip_duration = 600,
  max_rides = 10
)

system.time({
  exp_tt_matrix <- expanded_travel_time_matrix(
    r5r_core,
    origins = libraries,
    destinations = libraries,
    mode = "TRANSIT",
    departure_datetime = as.POSIXct("08-23-2023 06:00:00", format = "%m-%d-%Y %H:%M:%S"),
    time_window = 30,
    max_trip_duration = 600,
    max_walk_time = Inf,
    max_rides = 10,
    breakdown = F
  )
})

exp_tt_matrix %>%
  filter(to_id == "Fairdale") %>%
  View() # all walking for some reason

exp_tt_matrix %>%
  filter(from_id == "Fairdale") %>%
  View() # actually uses buses
