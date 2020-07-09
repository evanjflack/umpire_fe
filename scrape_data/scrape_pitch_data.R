# Header -----------------------------------------------------------------------
# Proj: Umpire Fixed Effects
# Author: Evan Flack (evanjflack@gmail.com)
# Desc: Creates (or updates) a database of MLBs pitchFx
# ------------------------------------------------------------------------------

# Libraries --------------------------------------------------------------------
# library(dplyr)
# library(dbplyr)
library(RSQLite)
# library(curl)
library(RCurl)
library(pitchRx)
library(tictoc)

# source("../supporting_code/define_functions.R")

# start_log_file(T, "log/scrape_pitch_data")

# Scrape PitchRX ---------------------------------------------------------------
# Determine if database exists in desired location or not
db_exists <- file.exists("../Data/pitchfx.sqlite3")
message("Data Base Exists: ", db_exists)

# Open database if exists, creates if does not exist
db <- tbl("../Data/pitchfx.sqlite3", create = !db_exists)

dat <- scrape(start = "2016-06-01", end = "2016-06-01")

# If the database doesn't exist, scrape all pitchfx data
# If the database does exist, updtae with missing dates
if (!db_exists) {
  scrape(start = "2008-05-01", end = "2008-05-10", connect = db$con)
}

# End --------------------------------------------------------------------------
end_log_file(T)
