# ------------------------------------------------------------------------------
# Proj: Umpire FE
# Author: Evan Flack (evanjflack@gmail.com)
# Desc: Estimates umpire fixed effects for tendancy to call strikes on all 
#       pitches as well as marginal pitches
# ------------------------------------------------------------------------------

# Libraries --------------------------------------------------------------------
library(ggplot2)
library(data.table)
library(magrittr)
library(broom)
library(knitr)
library(stringr)
library(estimatr)

source("../supporting_code/define_functions.R")

# Read In Data -----------------------------------------------------------------
message("Reading in data...")

pitch_dt <- fread("../../data/out/pitch_data_2016_reg_season.csv")

umpire_dt <- fread("../../data/out/umpire_data_clean.csv")

umpire_fe <- fread("../../Data/umpire_fe.csv")

outcomes_dt <- fread("../../data/at_bat_outcomes.csv")

# Prep Data --------------------------------------------------------------------
message("Prepping data...")

# Umpire FE
umpire_fe %<>% 
  .[, .(umpire, on_margin, estimate)] %>% 
  setnames("estimate", "umpire_perc") %>% 
  dcast(umpire ~ on_margin, value.var = "umpire_perc")
  
# Home plate umpire
hp_umpire_dt <- umpire_dt %>% 
  .[, .(gameday_link,umpire_name_HP)] %>% 
  .[, gameday_link := paste0("gid_", gameday_link)] %>% 
  merge(umpire_fe, by.x = "umpire_name_HP", by.y = "umpire") %>% 
  melt(id.var = c("umpire_name_HP", "gameday_link"), 
       variable.name = "on_margin", value.name = "umpire_perc")

model_dt <- pitch_dt %>% 
  .[count == "0-0"] %>% 
  .[des %in% c("Called Strike", "Ball")]

# Drop observations w/o pitch positions or umpires
n1 <- nrow(model_dt)
model_dt %<>% 
  .[!(is.na(px) | is.na(pz))]
n2 <- nrow(model_dt)  
message(n1 - n2, " out of ", n1, "(", round((n1-n2)/n1, 4), 
        ") observations dropped.")

# Define binary first stage outcome (strike)
model_dt %<>% 
  .[, take := ifelse(des %in% c("Called Strike", "Ball"), 1, 0)]

model_dt %<>% 
  .[, on_margin := define_marginal(px, pz, bot = sz_bot, top = sz_top, 
                                   eps_out = .1, eps_in = .2)] %>% 
  .[, strike := ifelse(des == "Called Strike", 1, 0)]

model_dt %<>% 
  merge(hp_umpire_dt, by = c("gameday_link", "on_margin"))

# Estimatye Second Stage -------------------------------------------------------
message("Estimating umpire FEs...")
# LOO First Stage
model_dt %<>%
  .[, sum_ump:= sum(strike), by = .(umpire_name_HP, on_margin)] %>% 
  .[, sum_game := sum(strike), by = .(gameday_link, on_margin)] %>% 
  .[, obs_ump := .N, by = .(umpire_name_HP, on_margin)] %>% 
  .[, obs_game := .N, by = .(gameday_link, on_margin)] %>% 
  .[, loo_strike_perc := (sum_ump - sum_game)/(obs_ump - obs_game)]

fit_fs <- lm(strike ~ loo_strike_perc:factor(on_margin) + factor(on_margin), 
             data = model_dt)

summary(fit_fs)
# second stage
model_dt %<>% 
  merge(outcomes_dt, by = c("gameday_link", "num"))

fit_rf <- lm(on_base ~ loo_strike_perc:factor(on_margin) + factor(on_margin), 
             data = model_dt)

summary(fit_rf)

fit_iv <- iv_robust(on_base ~ strike + factor(on_margin) | loo_strike_perc*factor(on_margin), 
                    data = model_dt)

summary(fit_iv)

fit_ols <- lm(on_base ~ strike, data = model_dt[on_margin == "On Margin"])
summary(fit_ols)


fit_take <- lm_robust(take ~ umpire_perc:factor(on_margin) + factor(on_margin) - 1, 
               data = model_dt, se_type = "stata")

