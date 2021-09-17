# Header -----------------------------------------------------------------------
# Proj: Umpire FE
# Author: Evan Flack (evanjflack@gmail.com)
# Desc: Estimates strike calls on the umpire fixed effect calculated with
#       observations from all other games

# Libraries --------------------------------------------------------------------
library(data.table)
library(magrittr)
library(tictoc)
library(estimatr)

source("../supporting_code/define_functions.R")

start_log_file("log/04a_estimate_first_stage")

# Read In Data -----------------------------------------------------------------

# Regular season pitches 2015-2018
take_dt <- fread("../../data/med/take_data.csv") %>% 
  .[, .(g_id, umpire_HP, called_strike, on_margin)]

loo_fe <- fread("../../data/med/game_level_umpire_fe.csv")

take_dt %<>% 
  merge(loo_fe, by = "g_id")

take_dt %<>% 
  .[, loo_ump_fe_loc := ifelse(on_margin == "clear_strike", 
                              loo_ump_fe_clear_strike, 
                              ifelse(on_margin == "clear_ball", 
                                     loo_ump_fe_clear_ball, 
                                     loo_ump_fe_on_margin))]

# Estimate First Stage ---------------------------------------------------------

# All Pitches
fit_all <- lm_robust(called_strike ~ loo_ump_fe_all - 1, data = take_dt)
dt_fit_all <- tidy_dt(fit_all) %>% 
  .[, on_margin := "all"]

# By location
fit_marg <- lm_robust(called_strike ~ loo_ump_fe_loc:factor(on_margin) - 1, 
                      data = take_dt)
dt_fit_marg <- tidy_dt(fit_marg) %>% 
  .[, on_margin := gsub("loo_ump_fe_loc:factor\\(on_margin\\)", "", term)]

dt_fit <- rbind(dt_fit_all, dt_fit_marg)
print(dt_fit)

# Export -----------------------------------------------------------------------

fwrite(dt_fit, "../../data/out/first_stage_reg_results.csv")

end_log_file()
