# Header -----------------------------------------------------------------------
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
# Pitches DT (regular season 2016)
pitch_dt <- fread("../Data/pitch_data_2016_reg_season.csv")

# Umpire DT
umpire_dt <- fread("../Data/umpire_data_clean.csv")

# Prep Data --------------------------------------------------------------------
# Home PLate Umpire
hp_umpire_dt <- umpire_dt %>% 
  .[, .(gameday_link, umpire_id_HP, umpire_name_HP)] %>% 
  .[, gameday_link := paste0("gid_", gameday_link)]

take_dt <- pitch_dt %>%
  # .[count == "0-0", ] %>% 
  .[des %in% c("Called Strike", "Ball")]

model_dt <- take_dt %>% 
  merge(hp_umpire_dt, by = "gameday_link", all.x = T)

# Drop observations w/o pitch positions or umpires
n1 <- nrow(model_dt)
model_dt %<>% 
  .[!(is.na(px) | is.na(pz) | is.na(umpire_id_HP))]
n2 <- nrow(model_dt)  
message(n1 - n2, " out of ", n1, " observations dropped.")

# Define binary first stage outcome (strike)
model_dt %<>% 
  .[, strike := ifelse(des == "Called Strike", 1, 0)]

# Define "close pitches" 
model_dt %<>% 
  .[, on_margin := define_marginal(px, pz, bot = sz_bot, top = sz_top, 
                                   eps_out = .3, eps_in = .3)]


# Estimatye Umpire FE ----------------------------------------------------------
# All pitches
umpire_strike_all <- model_dt %>% 
  .[, .(perc_strike = mean(strike), sd = sd(strike), obs = .N), by = umpire_name_HP] %>% 
  .[, se := sd/sqrt(obs)] %>% 
  .[, `:=`(lb = perc_strike - 1.96*se, ub = perc_strike + 1.96*se)] %>% 
  .[order(perc_strike)] %>% 
  .[, ord := seq(1, .N)] %>% 
  .[, type := "All Pitches"]

fit_umpire <- lm(strike ~ factor(umpire_name_HP) - 1, data = model_dt)

dt_fit_umpire <- tidy(fit_umpire) %>% 
  as.data.table() %>% 
  .[, umpire := gsub("factor\\(umpire_name_HP\\)", "", term)] %>% 
  .[, on_margin := "All"] %>% 
  .[order(estimate)] %>% 
  .[, ord := seq(1, .N)] %>% 
  .[, `:=`(lb = estimate - 1.96*std.error, ub = estimate + 1.96*std.error)] %>% 
  .[, type := "All"]


fit_umpire_marg <- lm(strike ~ factor(umpire_name_HP):factor(on_margin) - 1, data = model_dt)

dt_fit_umpire_marg <- tidy(fit_umpire_marg) %>% 
  as.data.table()  %>% 
  .[, term := gsub("factor\\(umpire_name_HP\\)", "", term)] %>% 
  .[, term := gsub("factor\\(on_margin\\)", "", term)] %>% 
  .[, umpire := str_split_fixed(term, ":", 2)[, 1]] %>% 
  .[, on_margin := str_split_fixed(term, ":", 2)[, 2]] %>% 
  .[order(on_margin, estimate), ] %>% 
  .[, ord := seq(1, .N), by = on_margin] %>% 
  .[, `:=`(lb = estimate - 1.96*std.error, ub = estimate + 1.96*std.error)] %>% 
  .[, on_margin := ifelse(on_margin == 1, "On Margin", ifelse(on_margin == 2, "Clear Strike", "Clear Ball"))] %>% 
  .[, type := "By Margin"]


dt_fit <- rbind(dt_fit_umpire, dt_fit_umpire_marg)

ggplot(dt_fit_umpire_marg) + 
  aes(x = ord, y = estimate, ymin = lb, ymax = ub, color = factor(on_margin)) + 
  geom_point(size = .75) + 
  geom_line() + 
  geom_errorbar(alpha = .5, width = 0) +
  labs(x = "Umpire (in order of stike %)", y = "Strike % (given take)", 
       color = "Pitch Location") + 
  my_theme + 
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank())

ggplot(dt_fit_umpire) + 
  aes(x = ord, y = estimate, ymin = lb, ymax = ub) + 
  geom_point(size = .75) + 
  geom_errorbar(alpha = .5, width = 0) +
  facet_wrap(~ type) + 
  labs(x = "Umpire (in order of stike %)", y = "Strike % (given take)") + 
  my_theme + 
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank())



# Marginal Pitches
umpire_strike_marg <- model_dt %>% 
  .[on_margin == 1, ] %>% 
  .[, .(perc_strike = mean(strike), sd = sd(strike), obs = .N), by = umpire_name_HP] %>% 
  .[, se := sd/sqrt(obs)] %>% 
  .[, `:=`(lb = perc_strike - 1.96*se, ub = perc_strike + 1.96*se)] %>% 
  .[order(perc_strike)] %>% 
  .[, ord := seq(1, .N)] %>% 
  .[, type := "Marginal Pitches"]
  
umpire_strike <- rbind(umpire_strike_all, 
                       umpire_strike_marg)

ggplot(umpire_strike) + 
  aes(x = ord, y = perc_strike, ymin = lb, ymax = ub) + 
  geom_point(size = .75) + 
  geom_errorbar(alpha = .5, width = 0) +
  facet_wrap(~ type) + 
  labs(x = "Umpire (in order of stike %)", y = "Strike % (given take)") + 
  my_theme + 
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank())

ggsave("../output/umpire_fe.png", width = 8, height = 4)


# Leave One Out First Stage ----------------------------------------------------
# Umpire stike percentage (excluding current game)
# All Pitches
model_dt %<>% 
  .[, sum_ump:= sum(strike), by = umpire_name_HP] %>% 
  .[, sum_game := sum(strike), by = gameday_link] %>% 
  .[, obs_ump := .N, by = umpire_name_HP] %>% 
  .[, obs_game := .N, by = gameday_link] %>% 
  .[, loo_strike_perc := (sum_ump - sum_game)/(obs_ump - obs_game)]

# Marginal
model_dt_marg <- model_dt %>% 
  # .[on_margin == 1, ] %>% 
  .[, sum_ump:= sum(strike), by = .(umpire_name_HP, on_margin)] %>% 
  .[, sum_game := sum(strike), by = .(gameday_link, on_margin)] %>% 
  .[, obs_ump := .N, by = .(umpire_name_HP, on_margin)] %>% 
  .[, obs_game := .N, by = .(gameday_link, on_margin)] %>% 
  .[, loo_strike_perc := (sum_ump - sum_game)/(obs_ump - obs_game)]


fit_fs_all <- lm(strike ~ loo_strike_perc, data = model_dt)
fit_fs_marg <- lm(strike ~ loo_strike_perc:factor(on_margin) + factor(on_margin) - 1, data = model_dt_marg)


summary(fit_fs_marg)

dt_fs_all <- tidy(fit_fs_all) %>% 
  as.data.table() %>% 
  .[, term := c("Intercept", "Umpire Strike Avg (LOO)")] %>% 
  .[, type := "All Pitches"]

dt_fs_marg <- tidy(fit_fs_marg) %>% 
  as.data.table() %>%
  .[, term := c("Intercept", "Umpire Strike Avg (LOO)")] %>% 
  .[, type := "Marginal Pitches"]

dt_fs<- rbind(dt_fs_all, dt_fs_marg) %>% 
  .[, lapply(.SD, signif, digits = 3), by = .(type, term)] %>% 
  .[, stars := ifelse(p.value <= .01, "***", ifelse(p.value <= .05, "**", ifelse(p.value <= .1, "*", "")))] %>% 
  .[, estimate := paste0(estimate, stars)] %>% 
  .[, est_se := paste0(estimate, " (", std.error, ")")] %>% 
  .[, type := c("All Pitches", "", "Marginal Pitches", "")] %>% 
  .[, .(type, term, est_se)] %>% 
  setnames(names(.), c("", "Term", "Estimate"))

kable(dt_fs)

# Export -----------------------------------------------------------------------
head(sample00_dt)

