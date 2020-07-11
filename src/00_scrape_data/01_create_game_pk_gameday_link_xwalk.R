# ------------------------------------------------------------------------------
# Proj: umpire_fe
# Author: Evan Flack (evanjflack@gmail.com)
# Desc: Scrapes mlb.com to create an xwalk from game_pk (umpire data) to 
#       gameday_link (pitch data)
# ------------------------------------------------------------------------------

# Libraries --------------------------------------------------------------------
library(pitchRx)
library(baseballr)
library(data.table)
library(lubridate)
library(XML2R)
library(stringr)
library(magrittr)
library(tictoc)

source("../supporting_code/define_functions.R")

# start_log_file(T, "create-game_pk_gamday_link_xwalk")

# Read in Data -----------------------------------------------------------------
message("Reading in umpire data")
umpire_dt <- fread("../../data/raw/umpires_ids_game_pk.csv")

# Create Xwalk from game_pk to gameday_link ------------------------------------
# Unique dates from umpire_dt
message("Identifying unique dates in umpire data")
dates <- umpire_dt %>% 
  .[, .(game_date)] %>% 
  unique() %>% 
  .[, game_date := ymd(game_date)] %>% 
  .[, year := year(game_date)] %>% 
  .[, month := month(game_date)] %>%
  .[, month := str_pad(month, 2, pad = "0")] %>% 
  .[, day := day(game_date)] %>% 
  .[, day := str_pad(day, 2, pad = "0")] %>% 
  .[year <= 2016, ]

message("Querying for game_pks and gameday link...")
pk_link_xwalk <- data.table()
for(i in 1:nrow(dates)) {
  print(i)
  u <- paste0("http://gd2.mlb.com/components/game/mlb/year_", dates[i, year], 
              "/month_", dates[i, month], "/day_", dates[i, day], 
              "/miniscoreboard.xml")
  obs <- XML2Obs(u) 
  gms <- obs[grepl("^games//game$", names(obs))]
  xwalk1 <- collapse_obs(gms)[, c("game_pk", "gameday_link")] %>% 
    data.table(stringsAsFactors = FALSE) %>% 
    .[, date := dates[i, game_date]]
  pk_link_xwalk  %<>% rbind(xwalk1)
}

# Export -----------------------------------------------------------------------
message("Exporting...")

fwrite(pk_link_xwalk, "../../data/out/pk_link_xwalk.csv")

# end_log_file(T)