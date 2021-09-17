# Header -----------------------------------------------------------------------
# Proj: Umpire Fixed Effects
# Author: Evan Flack (evanjflack@gmail.com)
# Desc: Calculates team records on a given day/year (to be used in randomization 
#       check)

# Libraries --------------------------------------------------------------------
library(data.table)
library(magrittr)
library(lubridate)
library(tictoc)

source("../supporting_code/define_functions.R")

start_log_file("log/01_calc_team_records")

# Read in Data -----------------------------------------------------------------
message("Reading in data...")

# Game data from 2015-2018
game_dt <- fread("../../data/raw/kaggle_data/games.csv")

# Team Records -----------------------------------------------------------------
message("Calculating team records...")

wins_long <- game_dt %>% 
  # Define binary variables for if the home team or away team won
  .[, away_win := ifelse(away_final_score > home_final_score, 1, 0)] %>% 
  .[, home_win := 1 - away_win] %>% 
  .[, .(g_id, date, away_win, home_win)] %>% 
  melt(id.var = c("g_id", "date"), value.name = "win") %>% 
  .[, team1 := ifelse(grepl("away", variable), "away", "home")] %>% 
  .[, variable := NULL]

# Determine which team was home/away for each game
teams_long <- game_dt %>% 
  .[, .(g_id, date, away_team, home_team)] %>% 
  melt(id.var = c("g_id", "date"), value.name = "team") %>% 
  .[, team1 := ifelse(grepl("away", variable), "away", "home")] %>% 
  .[, variable := NULL]

# Combine the team names with the wins, reshape to wide, and calculate the
# win percentage of each team before the game in question
teams_wins_long <- teams_long %>% 
  merge(wins_long, by = c("g_id", "date", "team1")) %>% 
  .[, year := year(date)] %>% 
  .[order(team, year, g_id)] %>% 
  .[, cum_wins := ave(win, team, year, FUN=cumsum)] %>% 
  .[cum_wins > 0, cum_wins := cum_wins - 1] %>% 
  .[, games := seq(1, .N), by = .(team, year)] %>% 
  .[games > 1, games := games - 1] %>% 
  .[, win_perc := cum_wins/games]

# Cast to wide
team_wins_wide <- teams_wins_long %>%
  dcast(g_id ~ team1, value.var = c("cum_wins", "win_perc"))

# Export -----------------------------------------------------------------------
message("Exporting to csv.")

fwrite(team_wins_wide, paste0("../../data/med/team_wins.csv"))

end_log_file()
