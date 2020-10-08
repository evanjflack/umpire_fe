# ------------------------------------------------------------------------------
# Proj: Umpire FE
# Author: Evan Flack (evanjflack@gmail.com)
# Desc: Estimates game-level leave out umpire strike propensities, excluding 
#       any observations for the game in question.
# ------------------------------------------------------------------------------

# Libraries --------------------------------------------------------------------
library(data.table)
library(magrittr)
library(cfo.behavioral)
source("../supporting_code/define_functions.R")

# User Inputs
eps_out <- .2
eps_in <- .1

start_log_file("log/02b_estimate_game_level_tendencies")

# Read In Data -----------------------------------------------------------------
message("Reading in data...")

pitch_dt <- fread("../../data/out/reg_season_data_2015_2018.csv")

# Prep Data --------------------------------------------------------------------
message("Prepping data...")

# Remove pitches wih no location, define take, location, and called strike
pitch_dt %<>% 
  .[!(is.na(px) | is.na(pz)), ] %>% 
  .[, take := ifelse(code %chin% c("*B", "B", "C"), 1, 0)] %>% 
  .[, on_margin := define_marginal(px, pz, bot = sz_bot, top = sz_top, 
                                   eps_out = eps_out, eps_in = eps_in)] %>% 
  .[, called_strike := ifelse(code == "C", 1, 0)]

# Estimate LOO First Stage -----------------------------------------------------
message("Estimating loo first stage...")

# All pitches
first_stage_dt_all <- pitch_dt %>% 
  .[take == 1, ] %>% 
  .[, strike_ump := sum(called_strike), by = .(umpire_HP)] %>% 
  .[, strike_game := sum(called_strike), by = .(g_id)] %>% 
  .[, obs_ump := .N, by = .(umpire_HP)] %>% 
  .[, obs_game := .N, by = .(g_id)] %>% 
  .[, .SD[1], by = .(g_id), 
    .SDcols = c("umpire_HP", "strike_ump", "strike_game", "obs_ump", 
                "obs_game")] %>% 
  .[, loo_strike_perc_all 
    := (strike_ump - strike_game)/(obs_ump - obs_game)] %>% 
  .[, .(g_id, loo_strike_perc_all)]

# By location
first_stage_dt_marg <- pitch_dt %>% 
  .[take == 1, ] %>% 
  .[, strike_ump := sum(called_strike), by = .(umpire_HP, on_margin)] %>% 
  .[, strike_game := sum(called_strike), by = .(g_id, on_margin)] %>% 
  .[, obs_ump := .N, by = .(umpire_HP, on_margin)] %>% 
  .[, obs_game := .N, by = .(on_margin, g_id)] %>% 
  .[, .SD[1], by = .(g_id, on_margin), 
    .SDcols = c("umpire_HP", "strike_ump", "strike_game", "obs_ump", 
                "obs_game")] %>% 
  .[, loo_strike_perc_marg := 
      (strike_ump - strike_game)/(obs_ump - obs_game)] %>% 
  .[, .(g_id, on_margin, loo_strike_perc_marg)] %>% 
  .[!(is.na(on_margin)), ]

# Export -----------------------------------------------------------------------
message("Exporting...")

fwrite(first_stage_dt_all, "../../data/out/first_stage_game_all.csv")

fwrite(first_stage_dt_marg, "../../data/out/first_stage_game_marg.csv")

end_log_file()
