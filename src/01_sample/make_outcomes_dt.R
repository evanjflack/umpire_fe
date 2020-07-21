# ------------------------------------------------------------------------------
# Proj: Umpire FE
# Author: Evan Flack (evanjflack@gmail.com)
# Desc: Creates outcomes of at-bat based on the decription of he last pitch in 
#       the at-bat
# ------------------------------------------------------------------------------

# Libraries --------------------------------------------------------------------
library(data.table)
library(magrittr)

source("../supporting_code/define_functions.R")

# User inputs
on_base_outcomes <- c("In play, no out", "In play, run(s)", "Hit By Pitch", 
                      "Intent Ball")

# Read In Data -----------------------------------------------------------------
message("Reading in data...")

pitch_dt <- fread("../../data/out/pitch_data_2016_reg_season.csv")

# Outcomes ---------------------------------------------------------------------
message("Identifying outcomes...")

# Last pitch in at bat
outcome_dt <- pitch_dt %>% 
  .[, balls := as.numeric(substr(count, 1, 1))] %>% 
  .[, strikes := as.numeric(substr(count, 3, 3))] %>% 
  .[, pitches := balls + strikes + 1] %>% 
  .[order(gameday_link, num, pitches)] %>% 
  .[, .SD[.N], by = .(gameday_link, num)]

# Binary outcomes 
outcome_dt %<>% 
  .[, on_base := ifelse(des %chin% on_base_outcomes, 1, 0)] %>% 
  .[, .(gameday_link, num, on_base)]

# Export -----------------------------------------------------------------------
message("Exporting...")

fwrite(outcome_dt, "../../data/at_bat_outcomes.csv")
