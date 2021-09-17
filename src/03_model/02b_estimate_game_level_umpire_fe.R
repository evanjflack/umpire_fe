# Header -----------------------------------------------------------------------
# Proj: Umpire FE
# Author: Evan Flack (evanjflack@gmail.com)
# Desc: Estimates game-level leave out umpire strike propensities by excluding 
#       any observations for the game in question.

# Libraries --------------------------------------------------------------------
library(data.table)
library(magrittr)
library(tictoc)
library(broom)

# Define user functions
source("../supporting_code/define_functions.R")

# User Inputs
# Epsilons for marginal strike zone definition
eps_out <- .2
eps_in <- .1

start_log_file("log/02b_estimate_game_level_fe")

# Read In Data -----------------------------------------------------------------

# Taken pitches
take_dt <- fread("../../data/med/take_data.csv") %>% 
  .[, .(g_id, umpire_HP, called_strike, on_margin)]

# Estimate LOO FEs -------------------------------------------------------------

# All pitches
loo_fe_all <- take_dt %>%
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
  .[, on_margin := "all"] %>% 
  .[, .(g_id, on_margin, loo_ump_fe)]

# Same as above but also condition on the location of the pitch
loo_fe_marg <- take_dt %>%
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

# Combine and reshape to wide
loo_fe <- rbind(loo_fe_all, loo_fe_marg) %>%
  dcast(g_id ~ on_margin, value.var = "loo_ump_fe") %>% 
  setnames(names(.)[-1], paste0("loo_ump_fe_", names(.)[-1]))

# Export -----------------------------------------------------------------------
message("Exporting estimates to csv.")

fwrite(loo_fe, "../../data/med/game_level_umpire_fe.csv")

end_log_file()
