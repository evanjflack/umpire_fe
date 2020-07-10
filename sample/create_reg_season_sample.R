# Header -----------------------------------------------------------------------
# Proj: Umpire Fixed Effects
# Author: Evan Flack (evanjflack@gmail.com)
# Desc: Creates sample of all 2016 regular season games
# ------------------------------------------------------------------------------

library(data.table)
library(magrittr)

# Read In Data -----------------------------------------------------------------
# Pitches DT (regular season 2016)
pitch_dt <- fread("Data/sample_pitch_data.csv")

# Subset to Regular Season -----------------------------------------------------
# Extract dates from gameday_link
reg_season_dt <- pitch_dt %>%
  .[, year := substr(gameday_link, 5, 8)] %>% 
  .[, month := as.numeric(substr(gameday_link, 10, 11))] %>% 
  .[, day := as.numeric(substr(gameday_link, 13, 14))]

# Subset based on date
reg_season_dt %<>% 
  # after opening day
  .[(month == 4 & day >= 3) | month > 4, ] %>% 
  # before the playoffs
  .[(month == 10 & day <= 3) | month < 10] %>% 
  # remove all star game
  .[home_team != "nas", ]

# Games by Teams ---------------------------------------------------------------
games <- reg_season_dt %>% 
  .[, .SD[1], by = gameday_link, .SDcols = c("home_team", "away_team", "month", "day")] %>% 
  .[order(month, day), ]

count_home <- games[, .(count_home = .N), by = home_team] %>% 
  setnames("home_team", "team")
count_away <- games[, .(count_away = .N), by = away_team] %>% 
  setnames("away_team", "team")
count_both <- count_home %>% 
  merge(count_away, by = "team") %>% 
  .[, count_total := count_home + count_away]

# Export -----------------------------------------------------------------------
fwrite(reg_season_dt, "Data/pitch_data_2016_reg_season.csv")
