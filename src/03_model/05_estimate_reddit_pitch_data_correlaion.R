# ------------------------------------------------------------------------------
# Proj: Umpire FE
# Author: Evan Flack (evanjflack@gmail.com)
# Desc: Estimates correlations between actual ummpire "correctness" and the 
#       amount of time they are brought up on reddit
# ------------------------------------------------------------------------------

# Libraries --------------------------------------------------------------------
library(ggplot2)
library(data.table)
library(magrittr)
library(broom)
library(knitr)
library(stringr)
library(cfo.behavioral)

source("../supporting_code/define_plot_theme.R")

# Read in Data -----------------------------------------------------------------
message("Reading in data...")

# URLs of reddit posts about each umpire
ump_urls <- fread("../../data/out/reddit_umpire_urls.csv")

# Correctness fixed effects
dt_correct <- fread("../../data/out/umpire_correct_call_fe.csv")
# Zone fixed effects
dt_zone <- fread("../../data/out/umpire_zone_fe.csv")


# Reddit Counts ----------------------------------------------------------------
url_counts <- ump_urls[, .(obs_reddit = .N), by = umpire_HP] %>% 
  .[order(obs_reddit), ] %>% 
  .[, lag_obs := c(NA, obs_reddit[-.N])] %>% 
  .[, add1 := ifelse(lag_obs != obs_reddit, 1, 0)] %>% 
  .[1, add1 := 1] %>% 
  .[, ord_reddit := ave(add1, FUN = cumsum)] %>% 
  .[, ord_reddit1 := seq(1, .N)]

ggplot(url_counts) + 
  aes(x = ord_reddit1, y = obs_reddit) + 
  geom_point() + 
  labs(x = "Umpire (in order of reddit mentions)",  y = "Reddit Menions") + 
  my_theme

# Correlation ------------------------------------------------------------------
dt_correct %<>% 
  merge(url_counts, by = "umpire_HP") %>% 
  .[order(rule_strike, perc_correct), ] %>% 
  .[, ord_correct := seq(1, .N), by = rule_strike]


fit_correct <- lm(ord_reddit1 ~ ord_correct:factor(rule_strike) + factor(rule_strike), 
          data = dt_correct)

summary(fit_correct)

dt_correct %<>% 
  .[, pred := predict(fit, .)] %>% 
  .[, se := predict(fit, ., se.fit = T)$se.fit] %>% 
  .[, `:=`(lb = pred - 1.96*se, ub = pred + 1.96*se)]

ggplot(dt_correct) + 
  aes(x = ord_correct, y = ord_reddit, ymin = lb, ymax = ub) + 
  geom_point(alpha = .5) + 
  geom_line(aes(y = pred)) + 
  geom_ribbon(color = NA, alpha = .5) + 
  # geom_errorbar(width = 0, alpha = .25) + 
  facet_wrap(~ rule_strike) + 
  # geom_hline(aes(yintercept = med), linetype = 2, data = dt_correct[, .SD[1], by = rule_strike]) + 
  # scale_y_continuous(limits = c(.84, .96), breaks = seq(.84, .96, .02)) + 
  labs(x = "Actual Correctness Ranking", y = "Reddit Mention Ranking") + 
  my_theme

dt_zone %<>% 
  setnames(c("umpire", "ord"), c("umpire_HP", "ord_zone")) %>% 
  merge(url_counts, by = "umpire_HP")

fit_zone <- lm(ord_reddit ~ ord_zone:factor(on_margin) + factor(on_margin), 
              data = dt_zone)

dt_zone %<>% 
  .[, pred := predict(fit_zone, .)] %>% 
  .[, se := predict(fit_zone, ., se.fit = T)$se.fit] %>% 
  .[, `:=`(lb = pred - 1.96*se, ub = pred + 1.96*se)]


ggplot(dt_zone) + 
  aes(x = ord_zone, y = ord_reddit, ymin = lb, ymax = ub) + 
  geom_point(alpha = .5) + 
  geom_line(aes(y = pred)) + 
  geom_ribbon(color = NA, alpha = .5) + 
  # geom_errorbar(width = 0, alpha = .25) + 
  facet_wrap(~ on_margin) + 
  # geom_hline(aes(yintercept = med), linetype = 2, data = dt_correct[, .SD[1], by = rule_strike]) + 
  # scale_y_continuous(limits = c(.84, .96), breaks = seq(.84, .96, .02)) + 
  labs(x = "Strike Percentage", y = "Reddit Mention Ranking") + 
  my_theme
