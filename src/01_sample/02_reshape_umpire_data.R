# ------------------------------------------------------------------------------
# Proj: umpire_fe
# Author: Evan Flack (evanjflack@gmail.com)
# Desc: Reshapes umpire data set from long to wide and merges in gameday_link 
#       xwalk
# ------------------------------------------------------------------------------

# Libraries --------------------------------------------------------------------
library(data.table)
library(magrittr)

# Read In Data -----------------------------------------------------------------
message("Reading in data...")
umpire_dt <- fread("../../data/raw/umpires_ids_game_pk.csv")

pk_link_xwalk <- fread("../../data/out/pk_link_xwalk.csv") %>%
  setnames("date", "game_date")

# Format Umpire Data -----------------------------------------------------------
message("Transforming umpire data to wide...")
umpire_dt_wide <- umpire_dt %>% 
  # only keep main 4 positions
  .[position %chin% c("HP", "1B", "2B", "3B")] %>%
  # only keep one observation per game
  .[, obs := .N, by = .(game_pk, game_date)] %>% 
  # reshape to wide
  dcast(game_pk + game_date ~ position, value.var = c("id", "name")) %>% 
  setnames(names(.)[-c(1, 2)], paste0("umpire_", names(.)[-c(1, 2)])) %>% 
  # merge in gameday_link
  merge(pk_link_xwalk, by = c("game_pk", "game_date"))

# Export -----------------------------------------------------------------------
message("Exporting...")

fwrite(umpire_dt_wide, "../../data/out/umpire_data_clean.csv")

