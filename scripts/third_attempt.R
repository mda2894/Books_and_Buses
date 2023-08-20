# Third attempt
#
# Using r5r
#
# NOTE: r5r requires Java JDK 11
#

# Setup  ------------------------------------------------------------------

options(java.parameters = "-Xmx2G")

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

oe_download_directory()

# To use a permanent directory for osm files:
# run usethis::edit_r_environ()
# and add a line containing: OSMEXT_DOWNLOAD_DIRECTORY=/path/to/save/files

# Data --------------------------------------------------------------------

# # uncomment this block to download new TARC GTFS file
# tarc_file <- here("data", "tarc_gtfs.zip")
# tarc_feed <- "http://googletransit.ridetarc.org/feed/google_transit.zip"
# download.file(tarc_feed, destfile = tarc_file)

osm_url <- "https://download.geofabrik.de/north-america/us/kentucky-latest.osm.pbf"
osm_path <- oe_download(osm_url)

setup_r5(here("data"))
