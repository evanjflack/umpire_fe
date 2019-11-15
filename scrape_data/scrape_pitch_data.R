# Header -----------------------------------------------------------------------

# Libraries --------------------------------------------------------------------
library(dplyr)
library(pitchRx)
library(tictoc)

source("supporting_code/define_functions.R")

start_log_file(T, "scrape_data/log/scrape_pitch_data")

# Scrape PitchRX ---------------------------------------------------------------
# indicator if db exists in desired location or not
db_exists <- file.exists("Data/pitchfx.sqlite3")

# open database if exists, creates if does not exist
my_db <- src_sqlite("Data/pitchfx.sqlite3", 
                    create = !db_exists)

# if the database doesn't exist, scrape all pitchfx data
# if the database does exist, updtae with missing dates
if (!db_exists) {
  scrape(start = "2008-01-01", end = "2008-05-01", connect = db$con)
} else {
  
}


end_log_file(T)
