# Header -----------------------------------------------------------------------

# Libraries --------------------------------------------------------------------
library(dplyr)
library(pitchRx)
library(tictoc)

source("~/Documents/umpire_fe/define_functions.R")

start_log_file(T, "scrape_pitch_data")

# Scrape PitchRX ---------------------------------------------------------------
# Place all data from June 2016 in a database
my_db <- src_sqlite("~/Documents/umpire_fe/Data/pitchRx_2016.sqlite3", 
                    create = T)
scrape(start = "2016-01-01", end = "2016-12-31", connect = my_db$con)

end_log_file(T)
