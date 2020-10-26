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
library(xtable)
library(cfo.behavioral)
library(foreach)
library(tidyr)
source("../supporting_code/define_functions.R")
source("../supporting_code/define_plot_theme.R")

# User Inputs
eps_out <- .2
eps_in <- .1

# Read In Data -----------------------------------------------------------------
message("Reading in data...")

pitch_dt <- fread("../../data/out/reg_season_data_2015_2018.csv")

times_faced_umpire <- fread(paste0("../../data/out/times_faced_umpire_per_year.csv"))

# Prep Data --------------------------------------------------------------------
message("Prepping data...")

pitch_dt %<>% 
  .[, take := ifelse(code %chin% c("*B", "B", "C"), 1, 0)] %>% 
  .[, on_margin := define_marginal(px, pz, bot = sz_bot, top = sz_top, 
                                   eps_out = eps_out, eps_in = eps_in)] %>% 
  .[, called_strike := ifelse(code == "C", 1, 0)]


# Estimate LOO First Stage -----------------------------------------------------
first_stage_dt_marg <- pitch_dt %>% 
  .[take == 1, ] %>% 
  .[, strike_ump := sum(called_strike), by = .(umpire_HP, on_margin)] %>% 
  .[, strike_game := sum(called_strike), by = .(g_id, on_margin)] %>% 
  .[, obs_ump := .N, by = .(umpire_HP, on_margin)] %>% 
  .[, obs_game := .N, by = .(on_margin, g_id)] %>% 
  .[, .SD[1], by = .(g_id, on_margin), 
    .SDcols = c("umpire_HP", "strike_ump", "strike_game", "obs_ump", 
                "obs_game")] %>% 
  .[, loo_strike_perc_marg := (strike_ump - strike_game)/(obs_ump - obs_game)] %>% 
  .[, .(g_id, on_margin, loo_strike_perc_marg)] %>% 
  .[!(is.na(on_margin)), ]

fwrite(first_stage_dt_marg, "../../data/out/first_stage_game_marg.csv")

first_stage_dt_marg_wide <- first_stage_dt_marg %>% 
  dcast(g_id ~ on_margin, value.var = "loo_strike_perc_marg") %>% 
  setnames(names(.)[-1], paste0("loo_strike_perc_", tolower(names(.)[-1]))) %>% 
  setnames(names(.), gsub(" ", "_", names(.))) 

head(first_stage_dt_marg_wide)

first_stage_dt_all <- pitch_dt %>% 
  .[take == 1, ] %>% 
  .[, strike_ump := sum(called_strike), by = .(umpire_HP)] %>% 
  .[, strike_game := sum(called_strike), by = .(g_id)] %>% 
  .[, obs_ump := .N, by = .(umpire_HP)] %>% 
  .[, obs_game := .N, by = .(g_id)] %>% 
  .[, .SD[1], by = .(g_id), 
    .SDcols = c("umpire_HP", "strike_ump", "strike_game", "obs_ump", 
                "obs_game")] %>% 
  .[, loo_strike_perc_all := (strike_ump - strike_game)/(obs_ump - obs_game)] %>% 
  .[, .(g_id, loo_strike_perc_all)]

fwrite(first_stage_dt_all, "../../data/out/first_stage_game_marg.csv")

model_dt <- pitch_dt %>% 
  .[b_count == 0 & s_count == 0] %>% 
  .[, .(g_id, take, on_margin, called_strike, inning, batter_id)] %>% 
  .[, swing := 1 - take] %>% 
  .[, stage := ifelse(inning <= 3, "1-3", ifelse(inning <= 6, "4-6", "6-9"))] %>% 
  .[inning <= 9, ]

model_dt %<>% 
  merge(first_stage_dt_all, by = "g_id") %>% 
  merge(first_stage_dt_marg, by = c("g_id", "on_margin")) %>% 
  merge(first_stage_dt_marg_wide, by = "g_id")

model_dt %<>% 
  .[!is.na(loo_strike_perc_all)]

model_dt %<>% 
  merge(times_faced_umpire, by = c("g_id", "batter_id"))

model_dt %<>% 
  .[, seen_before := ifelse(ump_times > 1, 1, 0)]


fit_exp <- lm(swing ~ loo_strike_perc_on_margin, data = model_dt[on_margin == "On Margin"])

fit_exp <- lm(swing ~ loo_strike_perc_on_margin:factor(seen_before) + factor(seen_before), data = model_dt[on_margin == "On Margin"])

summary(fit_exp)


# By Stage of Game -------------------------------------------------------------
fit_stage <- lm_robust(swing ~ loo_strike_perc_all:factor(stage) + factor(stage),
                       data = model_dt, 
                       se_type = "stata")

dt_fit_stage <- tidy(fit_stage) %>% 
  as.data.table() %>%
  .[, coeff := ifelse(grepl("loo", term), "Umpire Strike Avg", "Intercept")] %>% 
  .[, term := gsub("factor\\(stage\\)", "", term)] %>% 
  .[, term := gsub("loo_strike_perc_marg:", "", term)] %>%
  setnames("term", "stage") %>% 
  .[, on_margin := "All"]

ggplot(dt_fit_stage[coeff == "Umpire Strike Avg", ]) + 
  aes(x = stage, y = estimate, ymin = conf.low, ymax = conf.high) + 
  geom_point() +
  geom_errorbar() +
  labs(x = "Inning", y = "Estimate on Umpire Stike Avg") + 
  my_theme



fit_marg_stage <- lm_robust(swing ~ loo_strike_perc_marg:factor(on_margin):factor(stage)  + 
                              factor(on_margin):factor(stage) - 1, data = model_dt,  se_type = "stata")

dt_fit_marg_stage <- tidy(fit_marg_stage) %>% 
  as.data.table() %>%
  .[, coeff := ifelse(grepl("loo", term), "Umpire Strike Avg", "Intercept")] %>% 
  .[, term := gsub("factor\\(on_margin\\)", "", term)] %>% 
  .[, term := gsub("factor\\(stage\\)", "", term)] %>% 
  .[, term := gsub("loo_strike_perc_marg:", "", term)] %>%
  .[, on_margin := str_split_fixed(term, ":", 2)[, 1]] %>% 
  .[, stage := str_split_fixed(term, ":", 2)[, 2]] %>% 
  .[, term := NULL] 


obs_all_stage <- model_dt[, .(obs = .N, perc_swing = mean(swing)), by = stage] %>% 
  .[, on_margin := "All"]

obs_marg_stage <- model_dt[, .(obs = .N, perc_swing = mean(swing)), by = .(stage, on_margin)]

obs <- rbind(obs_all_stage, obs_marg_stage)

dt_fit1 <- rbind(dt_fit_stage, dt_fit_marg_stage) %>% 
  .[coeff != "Intercept", ] %>% 
  clean_fit_dt(id_vars = c("on_margin", "stage")) %>% 
  .[, stage := gsub("loo_strike_perc_all:", "", stage)] %>% 
  merge(obs, by = c("on_margin", "stage")) %>% 
  .[, .(on_margin, stage, obs, perc_swing, est_se)] %>% 
  .[c(2, 3, 5, 6, 8, 9, 11, 12), on_margin := ""] %>% 
  .[, obs := prettyNum(obs, big.mark = ",")]

print(xtable(dt_fit1), sanitize.text.function = force, 
      include.rownames = F)

dt_fit_marg_stage_

ggplot(dt_fit_marg_stage[coeff == "Umpire Strike Avg" & on_margin == "On Margin"]) + 
  aes(x = stage, y = estimate, ymin = conf.low, ymax = conf.high) + 
  geom_point() +
  geom_errorbar() +
  facet_wrap(~ on_margin) + 
  labs(x = "Inning", y = "Estimate on Umpire Stike Avg") + 
  my_theme

summary(fit_marg_stage)

fit_marg_inning<- lm_robust(swing ~ loo_strike_perc_marg:factor(on_margin)*inning  + 
                              factor(on_margin)*inning, data = model_dt,  se_type = "stata")


summary(fit_marg_inning)


obs_mean_marg <- model_dt %>% 
  .[, .(obs = .N, perc_swing = mean(swing)), by = on_margin]

obs_mean_all<- model_dt %>% 
  .[, .(obs = .N, perc_swing = mean(swing))] %>% 
  .[, on_margin := "All"]

obs_mean <- obs_mean_marg %>% 
  rbind(obs_mean_all) %>% 
  setnames("on_margin", "type")


dt_fit <- rbind(dt_fit_all, dt_fit_marg) %>%  
  merge(obs_mean, by = "type") %>% 
  .[, lapply(.SD, signif, digits = 3), by = .(type, coeff), .SDcols = c("estimate", "std.error", "p.value", "perc_swing", "obs")] %>% 
  .[, stars := ifelse(p.value <= .01, "***", ifelse(p.value <= .05, "**", ifelse(p.value <= .1, "*", "")))] %>% 
  .[, estimate := paste0(estimate, stars)] %>% 
  .[, est_se := paste0(estimate, " (", std.error, ")")] %>%
  .[, .(type, coeff, est_se, obs, perc_swing)] %>% 
  .[, `:=`(obs = as.character(obs), perc_swing = as.character(perc_swing))] %>% 
  .[c(2, 4, 6, 8), `:=`(type = "", obs = "", perc_swing = "")] %>% 
  setnames(names(.), c("Pitch Location", "Coefficient", "Estimate", "Observations", "% Swing"))

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