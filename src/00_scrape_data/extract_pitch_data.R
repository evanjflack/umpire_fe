# Header -----------------------------------------------------------------------
# Proj: Umpire Fixed Effects
# Author: Evan Flack (evanjflack@gmail.com)
# Desc: Extracts pitch and atbat data from sqlite3 database generated in 
#   scrape_pitch_data.R
# ------------------------------------------------------------------------------

# Libraries --------------------------------------------------------------------
library(pitchRx)
library(magrittr)
library(tictoc)
library(dplyr)
library(DBI)
library(data.table)

source("../supporting_code/define_functions.R")

# start_log_file(T, "log/subset_pitch_data")

# Inputs -----------------------------------------------------------------------
# variables to keep from the scapred savant data
# pitch 
game_vars <- c("gameday_link", "num")
play_vars <- c("count", "des")
loc_vars <- c("sz_top", "sz_bot", "px", "pz")
pitch_dt_vars <- c(game_vars,  play_vars, loc_vars)

# # atbat
# player_vars <- c("pitcher", "batter", "pitcher_name", "batter_name")
# atbat_dt_vars <- c(game_vars, player_vars)

# Scrpae Pitch Data  -----------------------------------------------------------
# Data base where pitch data is stored
db <- src_sqlite("../Data/pitchfx.sqlite3")

# query pitch_dt from the 'pitch' table for my_db
pitch_vars_sql <- paste(pitch_dt_vars, collapse = ", ")
pitch_query <- paste0('select ', pitch_vars_sql, ' from pitch')
pitch_dt <- dbGetQuery(db$con, pitch_query) %>% 
  as.data.table() %>% 
  .[, home_team := substr(gameday_link, 16, 18)] %>% 
  .[, away_team := substr(gameday_link, 23, 25)]

# # Don't need atbat yet
# atbat_vars_sql <- paste(atbat_dt_vars, collapse = ", ")
# atbat_query <- paste0('select ', atbat_vars_sql, ' from atbat')
# atbat_dt <- dbGetQuery(my_db$con, atbat_query) %>%
#   as.data.table()
# # merge pitch and atbat data
# both_dt <- merge(pitch_dt, atbat_dt, by = game_vars, all.x = T)

# Export  ----------------------------------------------------------------------
fwrite(pitch_dt, "../Data/extracted_pitch_data.csv")

# end_log_file(T)
