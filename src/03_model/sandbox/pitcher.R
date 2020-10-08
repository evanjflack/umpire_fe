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

# Prep Data --------------------------------------------------------------------
message("Prepping data...")

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

# Estimate LOO First Stage -----------------------------------------------------

first_stage_dt_marg <- pitch_dt %>% 
  .[take == 1, ] %>%
  .[, strike_ump := sum(called_strike), by = .(umpire_name_HP, on_margin)] %>% 
  .[, strike_game := sum(called_strike), by = .(gameday_link, on_margin)] %>% 
  .[, obs_ump := .N, by = .(umpire_name_HP, on_margin)] %>% 
  .[, obs_game := .N, by = .(on_margin, gameday_link)] %>% 
  .[, .SD[1], by = .(gameday_link, on_margin), 
    .SDcols = c("umpire_name_HP", "strike_ump", "strike_game", "obs_ump", 
                "obs_game")] %>% 
  .[, loo_strike_perc_marg := (strike_ump - strike_game)/(obs_ump - obs_game)] %>% 
  .[on_margin == "On Margin", ] %>% 
  .[, .(gameday_link, loo_strike_perc_marg)]

first_stage_dt_all <- pitch_dt %>% 
  .[take == 1, ] %>% 
  .[, strike_ump := sum(called_strike), by = .(umpire_name_HP)] %>% 
  .[, strike_game := sum(called_strike), by = .(gameday_link)] %>% 
  .[, obs_ump := .N, by = .(umpire_name_HP)] %>% 
  .[, obs_game := .N, by = .(gameday_link)] %>% 
  .[, .SD[1], by = .(gameday_link), 
    .SDcols = c("umpire_name_HP", "strike_ump", "strike_game", "obs_ump", 
                "obs_game")] %>% 
  .[, loo_strike_perc_all := (strike_ump - strike_game)/(obs_ump - obs_game)] %>% 
  .[, .(gameday_link, loo_strike_perc_all)]

model_dt <-pitch_dt %>% 
  .[count == "0-0"] %>% 
  .[, .(gameday_link, take, on_margin, called_strike, count)] %>% 
  .[, swing := 1 - take] %>% 
  .[, on_margin := ifelse(on_margin == "On Margin", 1, 0)]

model_dt %<>% 
  merge(first_stage_dt_all, by = "gameday_link") %>% 
  merge(first_stage_dt_marg, by = "gameday_link")


fit_all <- lm_robust(on_margin ~ loo_strike_perc_all, data = model_dt, 
                     se_type = "stata")

summary(fit_all)


dt_fit_all <- tidy(fit_all) %>% 
  as.data.table() %>% 
  .[, coeff := c("Intercept", "Umpire Strike Avg")] %>% 
  .[, type := "All"] %>% 
  .[, term := NULL]

fit_marg <- lm_robust(on_margin ~ loo_strike_perc_marg, data = model_dt, 
                      se_type = "stata")

summary(fit_marg)

dt_fit_marg <- tidy(fit_marg) %>% 
  as.data.table() %>%
  .[, coeff := ifelse(grepl("loo", term), "Umpire Strike Avg", "Intercept")] %>% 
  .[, term := gsub("factor\\(on_margin\\)", "", term)] %>% 
  .[, term := gsub("loo_strike_perc_marg:", "", term)] %>%
  setnames("term", "type")

obs_mean_marg <- model_dt %>% 
  .[, .(obs = .N, perc_take = mean(take)), by = on_margin]

obs_mean_all<- model_dt %>% 
  .[, .(obs = .N, perc_take = mean(take))] %>% 
  .[, on_margin := "All"]

obs_mean <- obs_mean_marg %>% 
  rbind(obs_mean_all) %>% 
  setnames("on_margin", "type")


dt_fit <- rbind(dt_fit_all, dt_fit_marg) %>%  
  merge(obs_mean, by = "type") %>% 
  .[, lapply(.SD, signif, digits = 3), by = .(type, coeff), .SDcols = c("estimate", "std.error", "p.value", "perc_take", "obs")] %>% 
  .[, stars := ifelse(p.value <= .01, "***", ifelse(p.value <= .05, "**", ifelse(p.value <= .1, "*", "")))] %>% 
  .[, estimate := paste0(estimate, stars)] %>% 
  .[, est_se := paste0(estimate, " (", std.error, ")")] %>%
  .[, .(type, coeff, est_se, obs, perc_take)] %>% 
  .[, `:=`(obs = as.character(obs), perc_take = as.character(perc_take))] %>% 
  .[c(2, 4, 6, 8), `:=`(type = "", obs = "", perc_take = "")] %>% 
  setnames(names(.), c("Pitch Location", "Coefficient", "Estimate", "Observations", "% Takes"))

fwrite(dt_fit, paste0("../../output/take_first_pitch_table.csv"))

summary(fit_all)

fit_marg <- lm_robust(take ~ loo_strike_perc_marg:factor(on_margin) + factor(on_margin) - 1, data = model_dt, 
                      se_type = "stata")

summary(fit_marg)

dt_fit2 <- tidy(fit2) %>%
  as.data.table() %>% 
  .[grepl("loo", term), ] %>% 
  .[, on_margin := gsub("loo_strike_perc:factor\\(on_margin\\)", "", term)]

ggplot(dt_fit2) + 
  aes(x = on_margin, y = estimate, ymin = conf.low, ymax = conf.high) + 
  geom_point() + 
  geom_errorbar() + 
  geom_hline(yintercept = 0) +
  labs(x = "Pitch Type", y = "RF Estimate on Swing")