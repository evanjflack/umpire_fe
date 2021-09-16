# Header -----------------------------------------------------------------------
# Proj: Umpire FE
# Author: Evan Flack (evanjflack@gmail.com)
# Desc: Reads in and combines pitch, at-bat, and game data sets downloaded from
#       Kaggle

# Libraries --------------------------------------------------------------------
library(data.table)
library(magrittr)
library(stringr)
library(tictoc)

# Define user functions
source("../supporting_code/define_functions.R")

start_log_file("log/01_combine_data_sources")

# Read in Data -----------------------------------------------------------------

# All from 2015-2018
# Pitch level
pitch_dt <- fread("../../data/kaggle_data/pitches.csv")
# At-bat level
atbat_dt <- fread("../../data/kaggle_data/atbats.csv")
# Game level
game_dt <- fread("../../data/kaggle_data/games.csv")

# Prep Data --------------------------------------------------------------------

# Separate date information
game_dt %<>%
  .[, `:=`(year = substr(date, 1, 4), 
           month = substr(date, 6, 7), 
           day = substr(date, 9, 10))]

# Game level variables to keep
game_dt_final <- game_dt %>% 
  .[, .(g_id, month, day, year, home_team, away_team, umpire_HP)]

pitch_dt_final <- pitch_dt %>% 
  .[, .(ab_id, pitch_num, b_count, s_count, outs, b_score, on_1b, on_2b, on_3b,
        sz_bot, sz_top,  px, pz, zone, code, type, pitch_type)]

atbat_dt_final <- atbat_dt %>% 
  .[, .(g_id, ab_id, batter_id, inning, top)]

# Merge Data -------------------------------------------------------------------
# Merge all 3 data sets, check to see no observations were lost

n1 <- nrow(pitch_dt_final)
model_dt <- pitch_dt_final %>% 
  merge(atbat_dt_final, by = "ab_id") %>% 
  merge(game_dt_final, by = "g_id")
n2 <- nrow(model_dt)

# Export -----------------------------------------------------------------------

if (n1 != n2) {
  message("OBSERVATIONS LOST IN MERGING, DATA NOT EXPORTED!!")
} else if (n1 == n2) {
  message("Data exported to csv.")
  fwrite(model_dt, paste0("../../data/out/reg_season_data_2015_2018.csv"))
}

end_log_file()
