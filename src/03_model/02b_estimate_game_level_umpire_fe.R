# Header -----------------------------------------------------------------------
# Proj: Umpire FE
# Author: Evan Flack (evanjflack@gmail.com)
# Desc: Estimates game-level leave out umpire strike propensities by excluding 
#       any observations for the game in question.

# Libraries --------------------------------------------------------------------
library(data.table)
library(magrittr)
library(tictoc)

# Define user functions
source("../supporting_code/define_functions.R")

# User Inputs
# Epsilons for marginal strike zone definition
eps_out <- .2
eps_in <- .1

start_log_file("log/02b_estimate_game_level_fe")

# Read In Data -----------------------------------------------------------------

# Regular season pitches 2015-2018
model_dt <- fread("../../data/out/reg_season_data_2015_2018.csv")

# Prep Data --------------------------------------------------------------------
message("Prepping data...")

# Remove pitches wih no location, define take, location, and called strike
# Only keep taken pitches
model_dt %<>% 
  .[, take := ifelse(code %chin% c("*B", "B", "C"), 1, 0)] %>% 
  .[take == 1, ]

# Drop observations with no position, or home plate umpire
n1 <- nrow(model_dt)
model_dt %<>% 
  .[!(is.na(px) | is.na(pz) | is.na(umpire_HP))]
n2 <- nrow(model_dt)  
message(n1 - n2, " out of ", n1, "(", round((n1-n2)/n1, 4), 
        ") observations dropped. ", n2, " Obs left")

# Define Indicators
model_dt %<>% 
  # Indicator for outcome (called strike)
  .[, called_strike := ifelse(code == "C", 1, 0)] %>% 
  # Indicator for if a pitch was on the margin or not (within some epsilon of 
  # the zone)
  .[, on_margin := define_marginal(px, pz, bot = sz_bot, top = sz_top, 
                                   eps_out = eps_out, eps_in = eps_in)]

# Estimate LOO FEs -------------------------------------------------------------

# All pitches
loo_fe_all <- model_dt %>%
  # Number of strikes called by the umpire in all games
  .[, strike_ump := sum(called_strike), by = .(umpire_HP)] %>% 
  # Number of strikes called by the umpire in the given game
  .[, strike_game := sum(called_strike), by = .(g_id)] %>% 
  # Number of pitches seen by the umpire in all games
  .[, obs_ump := .N, by = .(umpire_HP)] %>% 
  # Number of pitches seen by the umpire in the given game
  .[, obs_game := .N, by = .(g_id)] %>% 
  # Only need one of these observations per game
  .[, .SD[1], by = .(g_id), .SDcols = c("umpire_HP", "strike_ump", 
                                        "strike_game", "obs_ump", 
                                        "obs_game")] %>% 
  # Calculate the expected number of strikes from an umpire only using the data 
  # from the other games
  .[, loo_ump_fe := (strike_ump - strike_game)/(obs_ump - obs_game)] %>% 
  .[, on_margin := "All"] %>% 
  .[, .(g_id, on_margin, loo_ump_fe)]

# Same as above but also condition on the location of the pitch
loo_fe_marg <- model_dt %>%
  .[, strike_ump := sum(called_strike), by = .(umpire_HP, on_margin)] %>% 
  .[, strike_game := sum(called_strike), by = .(g_id, on_margin)] %>% 
  .[, obs_ump := .N, by = .(umpire_HP, on_margin)] %>% 
  .[, obs_game := .N, by = .(on_margin, g_id)] %>% 
  .[, .SD[1], by = .(g_id, on_margin), 
    .SDcols = c("umpire_HP", "strike_ump", "strike_game", "obs_ump", 
                "obs_game")] %>% 
  .[, loo_ump_fe := (strike_ump - strike_game)/(obs_ump - obs_game)] %>% 
  .[, .(g_id, on_margin, loo_ump_fe)] %>% 
  .[!(is.na(on_margin)), ]

# Combine
loo_fe <- rbind(loo_fe_all, loo_fe_marg) %>% 
  .[order(g_id, on_margin), ]

# Export -----------------------------------------------------------------------
message("Exporting estimates to csv.")

fwrite(loo_fe, "../../data/out/game_level_umpire_fe.csv")

end_log_file()
