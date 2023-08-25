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

setup <- otp_setup()
