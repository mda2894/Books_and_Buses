# gtfsrouter

# Setup -------------------------------------------------------------------

library(conflicted)
library(here)
library(doParallel)
library(tidyverse)
library(gtfsrouter)

conflicts_prefer(
  dplyr::filter,
  dplyr::lag
)

registerDoParallel()

# Data --------------------------------------------------------------------

tarc_gtfs <- extract_gtfs(filename = here("data", "tarc_gtfs.zip"))
tarc_gtfs$transfers <- gtfs_transfer_table(tarc_gtfs, d_limit = 1000)
tarc_gtfs <- gtfs_timetable(tarc_gtfs, date = 20230830)

tt <- tarc_gtfs %>%
  gtfs_traveltimes(
    from = "1338",
    start_time_limits = c(32400, 36000),
    from_is_id = T
  )
