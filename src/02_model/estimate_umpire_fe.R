# ------------------------------------------------------------------------------
# Proj: Umpire Fixed Effects
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
                                   eps_out = .3, eps_in = .3)]

# Estimatye Umpire FE ----------------------------------------------------------
message("Estimating umpire FEs...")

fit_umpire <- lm(strike ~ factor(umpire_name_HP) - 1, data = model_dt)
dt_fit_umpire_all <- tidy(fit_umpire) %>% 
  as.data.table() %>% 
  .[, umpire := gsub("factor\\(umpire_name_HP\\)", "", term)] %>% 
  .[, on_margin := 0]


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
  .[, on_margin := factor(on_margin, levels = c(0, 2, 3, 1), 
                          labels = c('All', "Clear Strike", "Clear Ball", "On Margin"))] %>% 
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

ggsave("../../output/umpire_fe.png", width = 8, height = 4)


# Leave One Out First Stage ----------------------------------------------------
# Umpire stike percentage (excluding current game)
# All Pitches
model_dt %<>%
  .[, sum_ump:= sum(strike), by = umpire_name_HP] %>% 
  .[, sum_game := sum(strike), by = gameday_link] %>% 
  .[, obs_ump := .N, by = umpire_name_HP] %>% 
  .[, obs_game := .N, by = gameday_link] %>% 
  .[, loo_strike_perc := (sum_ump - sum_game)/(obs_ump - obs_game)]

fit_fs_all <- lm(strike ~ loo_strike_perc, data = model_dt)

# Marginal
model_dt_marg <- model_dt %>% 
  .[on_margin == 1, ] %>% 
  .[, sum_ump:= sum(strike), by = .(umpire_name_HP, on_margin)] %>% 
  .[, sum_game := sum(strike), by = .(gameday_link, on_margin)] %>% 
  .[, obs_ump := .N, by = .(umpire_name_HP, on_margin)] %>% 
  .[, obs_game := .N, by = .(gameday_link, on_margin)] %>% 
  .[, loo_strike_perc := (sum_ump - sum_game)/(obs_ump - obs_game)]

fit_fs_marg <- lm(strike ~ loo_strike_perc, data = model_dt_marg)

# Clear Balls
model_dt_ball <- model_dt %>% 
  .[on_margin == 3, ] %>% 
  .[, sum_ump:= sum(strike), by = .(umpire_name_HP, on_margin)] %>% 
  .[, sum_game := sum(strike), by = .(gameday_link, on_margin)] %>% 
  .[, obs_ump := .N, by = .(umpire_name_HP, on_margin)] %>% 
  .[, obs_game := .N, by = .(gameday_link, on_margin)] %>% 
  .[, loo_strike_perc := (sum_ump - sum_game)/(obs_ump - obs_game)]

fit_fs_ball <- lm(strike ~ loo_strike_perc, data = model_dt_ball)

# Clear Stikes
model_dt_strike <- model_dt %>% 
  .[on_margin == 2, ] %>% 
  .[, sum_ump:= sum(strike), by = .(umpire_name_HP, on_margin)] %>% 
  .[, sum_game := sum(strike), by = .(gameday_link, on_margin)] %>% 
  .[, obs_ump := .N, by = .(umpire_name_HP, on_margin)] %>% 
  .[, obs_game := .N, by = .(gameday_link, on_margin)] %>% 
  .[, loo_strike_perc := (sum_ump - sum_game)/(obs_ump - obs_game)]

fit_fs_strike <- lm(strike ~ loo_strike_perc, data = model_dt_strike)

dt_fs_all <- tidy(fit_fs_all) %>% 
  as.data.table() %>% 
  .[, term := c("Intercept", "Umpire Strike Avg (LOO)")] %>% 
  .[, type := 0]

dt_fs_marg <- tidy(fit_fs_marg) %>% 
  as.data.table() %>%
  .[, term := c("Intercept", "Umpire Strike Avg (LOO)")]  %>% 
  .[, type := 1]

dt_fs_ball <- tidy(fit_fs_ball) %>% 
  as.data.table() %>%
  .[, term := c("Intercept", "Umpire Strike Avg (LOO)")] %>% 
  .[, type := 3]

dt_fs_strike <- tidy(fit_fs_strike) %>% 
  as.data.table() %>%
  .[, term := c("Intercept", "Umpire Strike Avg (LOO)")] %>% 
  .[, type := 2]


obs_mean_marg <- model_dt %>% 
  .[, .(obs = .N, perc_strike = mean(strike)), by = on_margin]

obs_mean_all<- model_dt %>% 
  .[, .(obs = .N, perc_strike = mean(strike))] %>% 
  .[, on_margin := 0]

obs_mean <- obs_mean_marg %>% 
  rbind(obs_mean_all) %>% 
  setnames("on_margin", "type")

dt_fs <- rbind(dt_fs_all, dt_fs_marg, dt_fs_strike, dt_fs_ball) %>% 
  merge(obs_mean, by = "type") %>% 
  .[, lapply(.SD, signif, digits = 3), by = .(type, term)] %>% 
  .[, stars := ifelse(p.value <= .01, "***", ifelse(p.value <= .05, "**", ifelse(p.value <= .1, "*", "")))] %>% 
  .[, estimate := paste0(estimate, stars)] %>% 
  .[, est_se := paste0(estimate, " (", std.error, ")")] %>% 
  .[, type := c("All Pitches", "", "Marginal Pitches", "", "Clear Strikes", "", "Clear Balls", "")] %>% 
  .[, .(type, term, est_se, obs, perc_strike)] %>% 
  .[, `:=`(obs = as.character(obs), perc_strike = as.character(perc_strike))] %>% 
  .[c(2, 4, 6, 8), `:=`(obs = "", perc_strike = "")] %>% 
  setnames(names(.), c("", "Term", "Estimate", "Observations", "% Strike"))

kable(dt_fs)

# Export -----------------------------------------------------------------------
head(sample00_dt)

