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

first_stage_dt_all <- fread("../../data/out/first_stage_game_all.csv")

first_stage_dt_marg <- fread("../../data/out/first_stage_game_marg.csv")

# Prep Data --------------------------------------------------------------------
message("Prepping data...")

pitch_dt %<>% 
  .[, take := ifelse(code %chin% c("*B", "B", "C"), 1, 0)] %>% 
  .[, on_margin := define_marginal(px, pz, bot = sz_bot, top = sz_top, 
                                   eps_out = eps_out, eps_in = eps_in)] %>% 
  .[, called_strike := ifelse(code == "C", 1, 0)]

model_dt <- pitch_dt %>% 
  .[b_count == 0 & s_count == 0] %>% 
  .[, .(g_id, take, on_margin, called_strike, inning)] %>% 
  .[, swing := 1 - take] %>% 
  .[, stage := ifelse(inning <= 3, "1-3", ifelse(inning <= 6, "4-6", "6-9"))] %>% 
  .[inning <= 9, ]


first_stage_dt_marg_wide <- first_stage_dt_marg %>% 
  dcast(g_id ~ on_margin, value.var = "loo_strike_perc_marg") %>% 
  setnames(names(.)[-1], paste0("loo_strike_perc_", tolower(names(.)[-1]))) %>% 
  setnames(names(.), gsub(" ", "_", names(.))) 

model_dt %<>% 
  merge(first_stage_dt_all, by = "g_id") %>% 
  merge(first_stage_dt_marg, by = c("g_id", "on_margin")) %>% 
  merge(first_stage_dt_marg_wide, by = "g_id")

n1 <- nrow(model_dt)
model_dt %<>% 
  .[!is.na(loo_strike_perc_all)]
n2 <- nrow(model_dt)

message(n1 - n2, " observations dropped for no game level stike propensity")

model_dt %<>% 
  .[, on_margin1 := tolower(gsub(" ", "_", on_margin))]

# First Stage ------------------------------------------------------------------
take_dt <- model_dt %>%
  .[take == 1, ]

dt_fit_fs <- data.table()
for (i in c("all", unique(take_dt$on_margin1))) {
  if (i != "all") {
    fit_dt <- take_dt[on_margin1 == i]
  } else {
    fit_dt <- take_dt
  }
  
  fit_dt[, loo_perc := get(paste0("loo_strike_perc_", i))]
  fit_fs <- lm(called_strike ~ loo_perc, data = fit_dt)

  
 fstat <- summary(fit_fs)$fstatistic["value"]

  dt_fit_fs1 <- fit_to_dt(fit_fs, "loo_perc") %>% 
    .[, on_margin1 := i] %>% 
    .[, fstat := fstat]
  
  dt_fit_fs %<>% rbind(dt_fit_fs1)
      
}

dt_fit_fs_clean <- dt_fit_fs %>% 
  clean_fit_dt(id_vars = c("on_margin1", "fstat"), sig = T) %>% 
  .[, on_margin := str_to_title(gsub("_", " ", on_margin1))] %>%
  .[, on_margin1 := NULL]

obs_mean_marg <- take_dt %>% 
  .[, .(obs = .N, perc_strike = mean(called_strike)), by = on_margin]

obs_mean_all<- take_dt %>% 
  .[, .(obs = .N, perc_strike = mean(called_strike))] %>% 
  .[, on_margin := "All"]

obs_mean <- obs_mean_marg %>% 
  rbind(obs_mean_all)

dt_fit_fs_print <- dt_fit_fs_clean %>% 
  merge(obs_mean, by = "on_margin") %>% 
  .[, .(on_margin, obs, perc_strike, est_se, fstat)] %>% 
  .[, obs := prettyNum(obs, big.mark = ",")] %>% 
  .[, perc_strike := as.character(round(perc_strike, 3))] %>%
  .[, fstat := round(fstat, 1)]

print(xtable(dt_fit_fs_print),
      sanitize.text.function = force,
      include.rownames = F)

# Visualization ----------------------------------------------------------------
n_quant <- 10
model_dt %<>% 
  .[, loo_all_cut := cut(loo_strike_perc_all, 
                         breaks = c(-Inf, quantile(loo_strike_perc_all, 
                                                   seq(0, 1, 1/n_quant))[2:(n_quant)], Inf), 
                         labels = seq(1, n_quant))] %>% 
  .[, loo_marg_cut := cut(loo_strike_perc_marg, 
                          breaks = c(-Inf, quantile(loo_strike_perc_marg, 
                                                    seq(0, 1, 1/n_quant))[2:(n_quant)], Inf), 
                         labels = seq(1, n_quant)), by = on_margin]

dtp_all <- calc_cmean(model_dt, "swing", "loo_all_cut", se = T) %>% 
  .[, on_margin := "All"] %>% 
  setnames("loo_all_cut", "loo_cut")
  
dtp_marg <- calc_cmean(model_dt, "swing", c("loo_marg_cut", "on_margin"), 
                           se = T) %>% 
  setnames("loo_marg_cut", "loo_cut")
  

dtp <- rbind(dtp_all, dtp_marg)  %>% 
  .[, loo_cut := as.numeric(as.character(loo_cut))]

ggplot(dtp[!is.na(on_margin)]) + 
  aes(x = loo_cut, y = mean, ymin = lb, ymax = ub) + 
  geom_point(size = .5) + 
  geom_errorbar(width = .5, alpha = .5) + 
  facet_wrap(~ on_margin, scale = "free_y") + 
  labs(x = "Umpire Strike Percentage Decile", y = "Batter Swing Percentage") + 
  scale_x_continuous(breaks = seq(2, 10, 2)) + 
  my_theme

ggsave("../../output/reduced_form_by_margin.png", width = 6, height = 4)

# Estimation -------------------------------------------------------------------
message("Estimating reduced form...")

type1 <- c("all", "on_margin", "clear_ball", "clear_strike")
type2 <- c("all", "on_margin", "clear_ball", "clear_strike")

grid <- as.data.table(crossing(type1, type2))

dt_fit <- foreach(type1 = grid$type1, 
                  type2 = grid$type2, 
                  .combine = "rbind") %do% 
  {
    if (type1 == "all") {
      fit_dt <- model_dt
    } else {
      fit_dt <- model_dt[on_margin1 == type1]
    }
    
    fit_dt[, loo_perc := get(paste0("loo_strike_perc_", type2))]
    
    fit <- lm_robust(swing ~ loo_perc, data = fit_dt, 
                     se_type = "stata")
    
  tidy(fit) %>% 
      as.data.table() %>% 
      .[, coeff := c("Intercept", "Ump Strike Avg")] %>% 
      .[, type1 := type1] %>% 
      .[, type2 := type2]
  }

# Baseline/Obs
obs_mean_marg <- model_dt %>% 
  .[, .(obs = .N, perc_swing = mean(swing)), by = on_margin1]

obs_mean_all<- model_dt %>% 
  .[, .(obs = .N, perc_swing = mean(swing))] %>% 
  .[, on_margin1 := "all"]

obs_mean <- obs_mean_marg %>% 
  rbind(obs_mean_all) %>% 
  setnames("on_margin1", "type1")

dt_fit_print <- clean_fit_dt(dt_fit, c("coeff", "type1", "type2")) %>% 
  .[coeff == "Ump Strike Avg"] %>% 
  dcast(type1  ~ type2, value.var = "est_se") %>% 
  merge(obs_mean, by = "type1") %>% 
  .[, .(type1, obs, perc_swing, all, on_margin, clear_ball, clear_strike)] %>% 
  .[, obs := prettyNum(obs, big.mark = ",")] %>% 
  .[, perc_swing := round(perc_swing, 3)] %>% 
  .[, type1 := factor(type1, levels = c("all", "on_margin", "clear_ball", "clear_strike"), 
                      labels = c("All", "On Margin", "Clear Ball", "Clear Strike"))] %>% 
  .[order(type1), ]

print(xtable(dt_fit_print), align = c("l", "l", rep("c", 6)), 
      sanitize.text.function = force,
      include.rownames = F)


# Corelations Between Tendencies -----------------------------------------------
messgage("Calculating correlations between tendencies...")
dt_cor <- cor(first_stage_dt_marg_wide[complete.cases(first_stage_dt_marg_wide), -1]) %>% 
  as.data.table(keep.rownames = T) %>% 
  .[, rn := gsub("loo_strike_perc_", "", rn)] %>% 
  setnames(names(.), gsub("loo_strike_perc_", "", names(.))) %T>% 
  print()
  




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