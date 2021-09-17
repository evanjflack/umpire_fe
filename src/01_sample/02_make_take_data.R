# Header -----------------------------------------------------------------------
# Proj: Umpire FE
# Author: Evan Flack (evanjflack@gmail.com)
# Desc: Estimates umpire fixed effects for tendency to call strikes on all 
#       pitches as well as by pitch location

# Libraries --------------------------------------------------------------------
library(data.table)
library(magrittr)
library(tictoc)
library(stringr)

# Define user functions and plot themes
source("../supporting_code/define_functions.R")

# User Inputs
# Epsilons for marginal strike zone definition
eps_out <- .2
eps_in <- .1

start_log_file("log/02_make_take_data")

# Read In Data -----------------------------------------------------------------
message("Reading in data...")

# All regular season pitches 2015-2018
pitches_dt <- fread("../../data/out/reg_season_data_2015_2018.csv")

# Prep Data --------------------------------------------------------------------

# Only keep taken pitches
take_dt <- pitches_dt %>% 
  .[, take := ifelse(code %chin% c("*B", "B", "C"), 1, 0)] %>% 
  .[take == 1, ]

# Drop observations with no position, or home plate umpire
n1 <- nrow(take_dt)
take_dt %<>% 
  .[!(is.na(px) | is.na(pz) | is.na(umpire_HP))]
n2 <- nrow(take_dt)  
message(n1 - n2, " out of ", n1, "(", round((n1-n2)/n1, 4), 
        ") observations dropped. ", n2, " Obs left")

# Define Indicators
take_dt %<>%
  # Indicator for outcome (called strike)
  .[, called_strike := ifelse(code == "C", 1, 0)] %>% 
  # Indicator for if a pitch was on the margin or not (within some epsilon of 
  # the zone)
  .[, on_margin := define_marginal(px, pz, bot = sz_bot, top = sz_top, 
                                   eps_out = eps_out, eps_in = eps_in)]

# Export -----------------------------------------------------------------------

fwrite(take_dt, "../../data/out/take_data.csv")

end_log_file()
