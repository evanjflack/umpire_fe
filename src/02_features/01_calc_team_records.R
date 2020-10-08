# ------------------------------------------------------------------------------
# Proj: Umpire Fixed Effects
# Author: Evan Flack (evanjflack@gmail.com)
# Desc: Calculates team records on a given day/year (to be used in randomization 
#       check)
# ------------------------------------------------------------------------------

# Libraries --------------------------------------------------------------------
library(data.table)
library(magrittr)
library(lubridate)
library(cfo.behavioral)

source("../supporting_code/define_functions.R")

start_log_file("log/01_calc_team_records")

# Read in Data -----------------------------------------------------------------
message("Reading in data...")

game_dt <- fread("../../data/kaggle_data/games.csv")

# Team Records -----------------------------------------------------------------
message("Calculating team records...")
# Reshaping teams.wins to long
wins_long <- game_dt %>% 
  .[, away_win := ifelse(away_final_score > home_final_score, 1, 0)] %>% 
  .[, home_win := 1 - away_win] %>% 
  .[, .(g_id, date, away_win, home_win)] %>% 
  melt(id.var = c("g_id", "date"), value.name = "win") %>% 
  .[, team1 := ifelse(grepl("away", variable), "away", "home")] %>% 
  .[, variable := NULL]

teams_long <- game_dt %>% 
  .[, .(g_id, date, away_team, home_team)] %>% 
  melt(id.var = c("g_id", "date"), value.name = "team") %>% 
  .[, team1 := ifelse(grepl("away", variable), "away", "home")] %>% 
  .[, variable := NULL]

teams_wins_long <- teams_long %>% 
  merge(wins_long, by = c("g_id", "date", "team1")) %>% 
  .[, year := year(date)] %>% 
  .[order(team, year, g_id)] %>% 
  .[, cum_wins := ave(win, team, year, FUN=cumsum)] %>% 
  .[, games := seq(1, .N), by = .(team, year)] %>% 
  .[, win_perc := cum_wins/games]


team_wins_wide <- teams_wins_long %>% 
  dcast(g_id ~ team1, value.var = c("cum_wins", "win_perc"))

# Export -----------------------------------------------------------------------
message("Exporting...")

fwrite(team_wins_wide, paste0("../../data/out/team_wins.csv"))

end_log_file()
