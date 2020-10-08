# ------------------------------------------------------------------------------
# Proj: Umpire FE
# Author: Evan Flack (evanjflack@gmail.com)
# Desc: Estimates how players react to umpires tendancies, by whether or not 
#       they take the first pitch based on if an umpire likes to call strikes or 
#       not
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

# User Inputs
eps_out <- .2
eps_in <- .1

# Read In Data -----------------------------------------------------------------
message("Reading in data...")

pitch_dt <- fread("../../data/out/pitch_data_2016_reg_season.csv")

umpire_dt <- fread("../../data/out/umpire_data_clean.csv")


# Home plate umpires
hp_umpire_dt <- umpire_dt %>% 
  .[, .(gameday_link,umpire_name_HP)] %>% 
  .[, gameday_link := paste0("gid_", gameday_link)]

pitch_dt %<>% 
  .[, take := ifelse(des %in% c("Called Strike", "Ball"), 1, 0)] %>% 
  .[, on_margin := define_marginal(px, pz, bot = sz_bot, top = sz_top, 
                                   eps_out = eps_out, eps_in = eps_in)] %>% 
  .[, called_strike := ifelse(des == "Called Strike", 1, 0)]

pitch_dt %<>% 
  merge(hp_umpire_dt, by = "gameday_link")


take_dt <- pitch_dt %>% 
  .[take == 1, ] %>% 
  .[, balls := as.numeric(substr(count, 1, 1))] %>% 
  .[, strikes := as.numeric(substr(count, 3, 3))] %>% 
  # .[strikes == 2, ] %>% 
  .[, rule_strike := define_strike(px, pz, bot = sz_bot, top = sz_top)] %>% 
  .[, rule_strike := ifelse(rule_strike == "Strike", 1, 0)] %>% 
  .[!(is.na(px) | is.na(pz))]

dtp1 <- take_dt %>% 
  .[, .(obs = .N), by = .(called_strike, rule_strike, strikes, on_margin)]
dtp2 <- take_dt %>% 
  .[, .(obs1 = .N), by = .(strikes, on_margin)]

dtp <- dtp1 %>% 
  merge(dtp2, by = c("strikes", "on_margin")) %>% 
  .[, perc := obs/obs1] %>% 
  .[, called_strike := factor(called_strike, labels = c("Called Ball", "Called Strike"))] %>% 
  .[, rule_strike := factor(rule_strike, labels = c("Rule Ball", "Rule Strike"))] %>% 
  .[, type := paste0(called_strike, "\n", rule_strike)]


ggplot(dtp[strikes == 2, ]) + 
  aes(x = type, y = perc) + 
  geom_bar(stat = "identity") +
  facet_wrap(~ on_margin)


take_dt <- pitch_dt %>% 
  .[, balls := as.numeric(substr(count, 1, 1))] %>% 
  .[, strikes := as.numeric(substr(count, 3, 3))] %>% 
  .[strikes == 2, ] %>% 
  .[, rule_strike := define_strike(px, pz, bot = sz_bot, top = sz_top)] %>% 
  .[, rule_strike := ifelse(rule_strike == "Strike", 1, 0)] %>% 
  .[!(is.na(px) | is.na(pz))]

dtp1 <- take_dt %>% 
  .[, .(obs = .N), by = .(called_strike, rule_strike, strikes, take)]
dtp2 <- take_dt %>% 
  .[, .(obs1 = .N), by = .(strikes, rule_strike)]

dtp <- dtp1 %>% 
  merge(dtp2, by = c("strikes")) %>% 
  .[, perc := obs/obs1] %>% 
  .[, called_strike := factor(called_strike, labels = c("Called Ball", "Called Strike"))] %>% 
  .[, rule_strike := factor(rule_strike, labels = c("Rule Ball", "Rule Strike"))] %>% 
  .[, type := paste0(called_strike, "\n", rule_strike)]








  .[, perc := obs/nrow(take_dt)]


head(dtp)

Vie


mean(take_dt$called_strike == 1 & take_dt$rule_strike == 0, na.rm = T)

mean(take_dt$called_strike == 0 & take_dt$rule_strike == 1, na.rm = T)


sum(is.na(take_dt$rule_strike))
mean(take_dt$called_strike)
mean(take_dt$rule_strike, na.rm = T)

mean(model_dt$take)
