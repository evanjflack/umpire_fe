# ------------------------------------------------------------------------------
# Proj: Umpire Fixed Effects
# Author: Evan Flack (evanjflack@gmail.com)
# Desc: Makes explanatory figure of strike zone and marginal area
# ------------------------------------------------------------------------------

# Libraries --------------------------------------------------------------------
library(ggplot2)
library(data.table)
library(magrittr)
library(broom)
library(knitr)

source("../supporting_code/define_functions.R")

# Read In Data -----------------------------------------------------------------
# Pitches DT (regular season 2016)
pitch_dt <- fread("../../data/out/pitch_data_2016_reg_season.csv")

# Prep Data --------------------------------------------------------------------
take_dt <- pitch_dt %>%
  .[des %in% c("Called Strike", "Ball")]

take_dt %<>% 
  .[!(is.na(px) | is.na(pz))]

# Define "close pitches" 
take_dt %<>% 
  .[, on_margin := define_marginal(px, pz, eps_out = .3, eps_in = .3)] %>% 
  .[, strike := ifelse(des == "Called Strike", 1, 0)]

# Make Figure ------------------------------------------------------------------
sz <- define_sz_rect(.3, .3)

pitch_sample <- sample(1:nrow(take_dt), 100)
p1 <- ggplot(take_dt[pitch_sample]) + 
  aes(x = px, y = pz, shape = factor(des)) + 
  geom_point() + 
  sz$rect + 
  # sz$rect_in + 
  # sz$rect_out + 
  my_theme +
  scale_x_continuous(limits = c(-2, 2)) + 
  labs(x = "Horizontal Position", 
       y = "Vertical Position", 
       title = "Strike Zone") + 
  theme(legend.title = element_blank(), 
        legend.position = "bottom")

p2 <- ggplot(take_dt[pitch_sample]) + 
  aes(x = px, y = pz, shape = factor(rule_strike)) + 
  geom_point() + 
  sz$rect_light + 
  sz$rect_in + 
  sz$rect_out + 
  my_theme +
  scale_x_continuous(limits = c(-2, 2)) + 
  labs(x = "Horizontal Position", 
       y = "Vertical Position", 
       title = "Strike Zone with Marginal Area") + 
  theme(legend.title = element_blank(), 
        legend.position = "bottom")


p_both <- gridExtra::grid.arrange(p1, p2, nrow = 1)

ggsave("../../output/strike_zone.png", p_both, width = 8, height = 5)

# Expore Optimal Margin --------------------------------------------------------
dt_marg <- data.table()
for (i in seq(0, .2, .05)) {
  for (j in seq(0, .2, .05)) {
    take_dt %<>% 
      .[, rule_strike := define_strike(px = px, pz = pz, bot = sz_bot, top = sz_top)] %>% 
      .[, on_margin := define_marginal(px = px, pz = pz, bot = sz_bot, top = sz_top, 
                                       eps_in = i, eps_out = j)]
    
    dt_marg1 <- take_dt[, .(perc_strike = mean(strike)), by = .(on_margin, rule_strike)] %>% 
      .[, `:=`(eps_in = i, eps_out = j)]
    dt_marg %<>% rbind(dt_marg1)
  }
}

ggplot(dt_marg[on_margin == "On Margin"]) + 
  aes(x = eps_out, y = perc_strike, color = factor(eps_in)) + 
  geom_line() + 
  facet_wrap(~ rule_strike) + 
  labs(x = "Outter Epsilon", color = "Inner Epsilon", 
       y = "% Called Strikes") + 
  my_theme

ggplot(dt_marg) + 
  aes(x = eps_out, y = perc_strike, color = factor(eps_in)) + 
  geom_line() + 
  facet_wrap(~ on_margin) + 
  labs(x = "Outter Epsilon", color = "Inner Epsilon", 
       y = "% Called Strikes") + 
  my_theme

ggplot(dt_marg) + 
  aes(x = eps_out, y = perc, color = factor(eps_in)) + 
  geom_line() + 
  labs(x = "Outter Epsilon", color = "Inner Epsilon", 
       y = "% Called Strikes", 
       title = "Optimal Marginal Strike Area") + 
  my_theme

ggsave("../output/strike_by_eps.png", 
       width = 8, height = 4)
