# ------------------------------------------------------------------------------
# Proj: Umpire FE
# Author: Evan Flack (evanjflack@gmail.com)
# Desc: Estimates umpire fixed effects for tendancy to call strikes on all 
#       pitches as well as by pitch location
# ------------------------------------------------------------------------------

# Libraries --------------------------------------------------------------------
library(ggplot2)
library(data.table)
library(magrittr)
library(broom)
library(knitr)
library(stringr)
library(cfo.behavioral)

source("../supporting_code/define_functions.R")
source("../supporting_code/define_plot_theme.R")

eps_out <- .2
eps_in <- .1

start_log_file("log/02a_estimate_umpire_fe_all.R")

# Read In Data -----------------------------------------------------------------
message("Reading in data...")

pitch_dt <- fread("../../data/out/reg_season_data_2015_2018.csv")

# Prep Data --------------------------------------------------------------------
message("Prepping data...")

model_dt <- pitch_dt %>%
  .[, take := ifelse(code %chin% c("*B", "B", "C"), 1, 0)] %>% 
  .[take == 1, ]

# Drop observations w/o pitch positions or umpires
n1 <- nrow(model_dt)
model_dt %<>% 
  .[!(is.na(px) | is.na(pz) | is.na(umpire_HP))]
n2 <- nrow(model_dt)  
message(n1 - n2, " out of ", n1, "(", round((n1-n2)/n1, 4), 
        ") observations dropped.")

# Define binary first stage outcome (strike)
model_dt %<>% 
  .[, called_strike := ifelse(code == "C", 1, 0)] %>% 
# Define "close pitches" 
  .[, on_margin := define_marginal(px, pz, bot = sz_bot, top = sz_top, 
                                   eps_out = eps_out, eps_in = eps_in)] %>% 
  .[, rule_strike := define_strike(px, pz, bot = sz_bot, top = sz_top)] %>% 
  .[, rule_strike1 := ifelse(rule_strike == "Strike", 1, 0)] %>% 
  .[, correct := ifelse(called_strike == rule_strike1, 1, 0)]

# Wrong Calls ------------------------------------------------------------------
message("Wrong calls...")

dt_correct1 <- model_dt[, .(perc_correct = mean(correct), obs = .N, 
                           sd = sd(correct)), by = umpire_HP] %>% 
  .[, rule_strike := "All"]

dt_correct2 <- model_dt[, .(perc_correct = mean(correct), obs = .N, 
                            sd = sd(correct)), by = .(umpire_HP, rule_strike)]

dt_correct <- rbind(dt_correct1, dt_correct2)

dt_correct %<>% 
  .[order(rule_strike, perc_correct), ] %>% 
  .[obs >= 1000, ] %>% 
  .[, ord := seq(1, .N), by = rule_strike] %>% 
  .[, se := sd/sqrt(obs)] %>% 
  .[, `:=`(lb = perc_correct - 1.96*se, ub = perc_correct + 1.96*se)] %>% 
  .[, med := median(perc_correct), by = rule_strike]

p <- ggplot(dt_correct) + 
  aes(x = ord, y = perc_correct, ymin = lb, ymax = ub) + 
  geom_point() + 
  geom_errorbar(width = 0, alpha = .25) + 
  facet_wrap(~ rule_strike) + 
  geom_hline(aes(yintercept = med), linetype = 2, data = dt_correct[, .SD[1], by = rule_strike]) + 
  # scale_y_continuous(limits = c(.84, .96), breaks = seq(.84, .96, .02)) + 
  labs(x = "Umpire (ranked by %)", y = "% Correct Calls") + 
  my_theme

ggsave(paste0("../../output/correct_calls.png"), p, 
       width = 6, height = 3)

# Estimatye Umpire FE ----------------------------------------------------------
message("Estimating umpire FEs...")

# All pitches
fit_umpire <- lm(called_strike ~ factor(umpire_HP) - 1, data = model_dt)

dt_fit_umpire_all <- tidy(fit_umpire) %>% 
  as.data.table() %>% 
  .[, umpire := gsub("factor\\(umpire_HP\\)", "", term)] %>% 
  .[, on_margin := "All"]

# By marginal status
fit_umpire_marg <- lm(called_strike ~ factor(umpire_HP):factor(on_margin) - 1, 
                      data = model_dt)
dt_fit_umpire_marg <- tidy(fit_umpire_marg) %>% 
  as.data.table()  %>% 
  .[, term := gsub("factor\\(umpire_HP\\)", "", term)] %>% 
  .[, term := gsub("factor\\(on_margin\\)", "", term)] %>% 
  .[, umpire := str_split_fixed(term, ":", 2)[, 1]] %>% 
  .[, on_margin := str_split_fixed(term, ":", 2)[, 2]]

# Combine
dt_fit_umpire <- rbind(dt_fit_umpire_all, dt_fit_umpire_marg) %>% 
  .[order(on_margin, estimate), ] %>% 
  .[, ord := seq(1, .N), by = on_margin] %>% 
  .[, `:=`(lb = estimate - 1.96*std.error, ub = estimate + 1.96*std.error)] %>% 
  .[, on_margin := factor(on_margin, levels = c('All', "Clear Strike", 
                                                "Clear Ball", "On Margin"))] %>% 
  .[, med := median(estimate), by = on_margin]

# Plot
p <- ggplot(dt_fit_umpire) + 
  aes(x = ord, y = estimate, ymin = lb, ymax = ub, color = factor(on_margin)) + 
  geom_point(size = .75) + 
  geom_line() + 
  geom_errorbar(alpha = .5, width = 0) +
  geom_hline(yintercept = c(0, 1)) + 
  geom_hline(aes(yintercept = med, color = factor(on_margin)), 
             linetype = 2, data = dt_fit_umpire[, .SD[1], by = on_margin]) + 
  labs(x = "Umpire (in order of stike %)", y = "Percentage Called Strikes", 
       color = "Pitch Location") +
  scale_y_continuous(breaks = seq(0, 1, .2)) + 
  my_theme + 
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank())

ggsave("../../output/umpire_fe_by_marg.png", p, width = 6, height = 5)

# End --------------------------------------------------------------------------
end_log_file()
