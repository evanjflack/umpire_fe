# ------------------------------------------------------------------------------
# Proj: Umpire Fixed Effects
# Author: Evan Flack (evanjflack@gmail.com)
# Desc: Reads in and combines pitch, atbat, and game datasets downloaded from
#       Kaggle
# ------------------------------------------------------------------------------

# Libraries --------------------------------------------------------------------
library(data.table)
library(magrittr)
library(stringr)
library(cfo.behavioral)

source("../supporting_code/define_functions.R")

start_log_file("log/01_combine_data_sources")

# Read in Data -----------------------------------------------------------------
message("Reading in data...")

# Pitch level
pitch_dt <- fread("../../data/kaggle_data/pitches.csv")
# At-bat level
atbat_dt <- fread("../../data/kaggle_data/atbats.csv")
# Game level
game_dt <- fread("../../data/kaggle_data/games.csv")

# Prep Data --------------------------------------------------------------------
message("Prepping data...")

# Separate date infomation
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
  .[, .(g_id, ab_id, inning, top)]

# Merge Data -------------------------------------------------------------------
message("Merging data...")

model_dt <- pitch_dt_final %>% 
  merge(atbat_dt_final, by = "ab_id") %>% 
  merge(game_dt_final, by = "g_id")

# Export -----------------------------------------------------------------------
message("Exporting...")

fwrite(model_dt, paste0("../../data/out/reg_season_data_2015_2018.csv"))

end_log_file()
