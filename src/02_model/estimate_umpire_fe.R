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

source("../supporting_code/define_functions.R")

eps_out <- .2
eps_in <- .05

# Read In Data -----------------------------------------------------------------
message("Reading in data...")

pitch_dt <- fread("../../data/out/pitch_data_2016_reg_season.csv")

umpire_dt <- fread("../../data/out/umpire_data_clean.csv")

# Prep Data --------------------------------------------------------------------
message("Prepping data...")

# Home plate umpire
hp_umpire_dt <- umpire_dt %>% 
  .[, .(gameday_link, umpire_id_HP, umpire_name_HP)] %>% 
  .[, gameday_link := paste0("gid_", gameday_link)]

# Only pitches that are taken
take_dt <- pitch_dt %>%
  .[des %in% c("Called Strike", "Ball")]

model_dt <- take_dt %>% 
  merge(hp_umpire_dt, by = "gameday_link", all.x = T)

# Drop observations w/o pitch positions or umpires
n1 <- nrow(model_dt)
model_dt %<>% 
  .[!(is.na(px) | is.na(pz) | is.na(umpire_id_HP))]
n2 <- nrow(model_dt)  
message(n1 - n2, " out of ", n1, "(", round((n1-n2)/n1, 4), 
        ") observations dropped.")

# Define binary first stage outcome (strike)
model_dt %<>% 
  .[, strike := ifelse(des == "Called Strike", 1, 0)] %>% 
# Define "close pitches" 
  .[, on_margin := define_marginal(px, pz, bot = sz_bot, top = sz_top, 
                                   eps_out = eps_out, eps_in = eps_in)] %>% 
  .[, rule_strike := define_strike(px, pz, bot = sz_bot, top = sz_top)]

# Estimatye Umpire FE ----------------------------------------------------------
message("Estimating umpire FEs...")
# All
fit_umpire <- lm(strike ~ factor(umpire_name_HP) - 1, data = model_dt)
dt_fit_umpire_all <- tidy(fit_umpire) %>% 
  as.data.table() %>% 
  .[, umpire := gsub("factor\\(umpire_name_HP\\)", "", term)] %>% 
  .[, on_margin := "All"]

# By marginal status
fit_umpire_marg <- lm(strike ~ factor(umpire_name_HP):factor(on_margin) - 1, 
                      data = model_dt)
dt_fit_umpire_marg <- tidy(fit_umpire_marg) %>% 
  as.data.table()  %>% 
  .[, term := gsub("factor\\(umpire_name_HP\\)", "", term)] %>% 
  .[, term := gsub("factor\\(on_margin\\)", "", term)] %>% 
  .[, umpire := str_split_fixed(term, ":", 2)[, 1]] %>% 
  .[, on_margin := str_split_fixed(term, ":", 2)[, 2]]

dt_fit_umpire <- rbind(dt_fit_umpire_all, dt_fit_umpire_marg) %>% 
  .[order(on_margin, estimate), ] %>% 
  .[, ord := seq(1, .N), by = on_margin] %>% 
  .[, `:=`(lb = estimate - 1.96*std.error, ub = estimate + 1.96*std.error)] %>% 
  .[, on_margin := factor(on_margin, levels = c('All', "Clear Strike", 
                                                "Clear Ball", "On Margin"))] %>% 
  .[, med := median(estimate), by = on_margin]

ggplot(dt_fit_umpire) + 
  aes(x = ord, y = estimate, ymin = lb, ymax = ub, color = factor(on_margin)) + 
  geom_point(size = .75) + 
  geom_line() + 
  geom_errorbar(alpha = .5, width = 0) +
  geom_hline(yintercept = c(0, 1)) + 
  geom_hline(aes(yintercept = med, color = factor(on_margin)), 
             linetype = 2, data = dt_fit_umpire[, .SD[1], by = on_margin]) + 
  labs(x = "Umpire (in order of stike %)", y = "Strike % (given take)", 
       color = "Pitch Location") +
  scale_y_continuous(breaks = seq(0, 1, .2)) + 
  my_theme + 
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank())

ggsave("../../output/umpire_fe_by_marg.png", width = 8, height = 4)

# By strike status
fit_umpire_strike <- lm(strike ~ factor(umpire_name_HP):factor(rule_strike) - 1, 
                      data = model_dt)
dt_fit_umpire_strike <- tidy(fit_umpire_strike) %>% 
  as.data.table()  %>% 
  .[, term := gsub("factor\\(umpire_name_HP\\)", "", term)] %>% 
  .[, term := gsub("factor\\(rule_strike\\)", "", term)] %>% 
  .[, umpire := str_split_fixed(term, ":", 2)[, 1]] %>% 
  .[, rule_strike := str_split_fixed(term, ":", 2)[, 2]]

dt_fit_umpire1 <- dt_fit_umpire_all %>% 
  copy() %>% 
  .[, on_margin := NULL] %>% 
  .[, rule_strike := "All"] %>% 
  rbind(dt_fit_umpire_strike) %>% 
  .[order(rule_strike, estimate), ] %>% 
  .[, ord := seq(1, .N), by = rule_strike] %>% 
  .[, `:=`(lb = estimate - 1.96*std.error, ub = estimate + 1.96*std.error)] %>% 
  .[, med := median(estimate), by = rule_strike]

ggplot(dt_fit_umpire1) + 
  aes(x = ord, y = estimate, ymin = lb, ymax = ub, color = factor(rule_strike)) + 
  geom_point(size = .75) + 
  geom_line() + 
  geom_errorbar(alpha = .5, width = 0) +
  geom_hline(yintercept = c(0, 1)) + 
  geom_hline(aes(yintercept = med, color = factor(rule_strike)), 
             linetype = 2, data = dt_fit_umpire1[, .SD[1], by = rule_strike]) + 
  labs(x = "Umpire (in order of stike %)", y = "Strike % (given take)", 
       color = "True Call") +
  scale_y_continuous(breaks = seq(0, 1, .2)) + 
  my_theme + 
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank())
  
ggsave("../../output/umpire_fe_by_truth.png", width = 8, height = 4)

# Leave One Out First Stage ----------------------------------------------------
# Umpire stike percentage (excluding current game)

# All Pitches
model_dt %<>%
  .[, sum_ump_all := sum(strike), by = umpire_name_HP] %>% 
  .[, sum_game_all := sum(strike), by = gameday_link] %>% 
  .[, obs_ump_all := .N, by = umpire_name_HP] %>% 
  .[, obs_game_all := .N, by = gameday_link] %>% 
  .[, loo_strike_perc_all := (sum_ump_all - sum_game_all)/(obs_ump_all - obs_game_all)]

fit_fs_all <- lm(strike ~ loo_strike_perc_all, data = model_dt)

dt_fs_all <- tidy(fit_fs_all) %>% 
  as.data.table() %>% 
  .[, coeff := c("Intercept", "Umpire Strike Avg")] %>% 
  .[, type := "All"] %>% 
  .[, term := NULL]

# By marginal status
model_dt %<>% 
  # .[on_margin == "On Margin", ] %>% 
  .[, sum_ump_marg := sum(strike), by = .(umpire_name_HP, on_margin)] %>% 
  .[, sum_game_marg := sum(strike), by = .(gameday_link, on_margin)] %>% 
  .[, obs_ump_marg := .N, by = .(umpire_name_HP, on_margin)] %>% 
  .[, obs_game_marg := .N, by = .(gameday_link, on_margin)] %>% 
  .[, loo_strike_perc_marg := (sum_ump_marg - sum_game_marg)/(obs_ump_marg - obs_game_marg)]

fit_fs_marg <- lm(strike ~ loo_strike_perc_marg:factor(on_margin) + factor(on_margin) - 1, data = model_dt_marg)

summary(fit_fs_marg)

dt_fs_marg <- tidy(fit_fs_marg) %>% 
  as.data.table() %>%
  .[, coeff := ifelse(grepl("loo", term), "Umpire Strike Avg", "Intercept")] %>% 
  .[, term := gsub("factor\\(on_margin\\)", "", term)] %>% 
  .[, term := gsub("loo_strike_perc_marg:", "", term)] %>%
  setnames("term", "type")
  .[, type := "On Margin"]

obs_mean_marg <- model_dt %>% 
  .[, .(obs = .N, perc_strike = mean(strike)), by = on_margin]

obs_mean_all<- model_dt %>% 
  .[, .(obs = .N, perc_strike = mean(strike))] %>% 
  .[, on_margin := "All"]

obs_mean <- obs_mean_marg %>% 
  rbind(obs_mean_all) %>% 
  setnames("on_margin", "type")

dt_fs <- rbind(dt_fs_all, dt_fs_marg) %>%  
  merge(obs_mean, by = "type") %>% 
  .[, lapply(.SD, signif, digits = 3), by = .(type, coeff)] %>% 
  .[, stars := ifelse(p.value <= .01, "***", ifelse(p.value <= .05, "**", ifelse(p.value <= .1, "*", "")))] %>% 
  .[, estimate := paste0(estimate, stars)] %>% 
  .[, est_se := paste0(estimate, " (", std.error, ")")] %>%
  .[, .(type, coeff, est_se, obs, perc_strike)] %>% 
  .[, `:=`(obs = as.character(obs), perc_strike = as.character(perc_strike))] %>% 
  .[c(2, 4, 6, 8), `:=`(type = "", obs = "", perc_strike = "")] %>% 
  setnames(names(.), c("Pitch Location", "Coefficient", "Estimate", "Observations", "% Strike"))

fwrite(dt_fs, paste0("../../output/umpire_fe_table.csv"))
