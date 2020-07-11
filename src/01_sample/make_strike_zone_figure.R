# Header -----------------------------------------------------------------------
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
pitch_dt <- fread("../Data/pitch_data_2016_reg_season.csv")

# Prep Data --------------------------------------------------------------------
take_dt <- pitch_dt %>%
  # .[count == "0-0", ] %>% 
  .[des %in% c("Called Strike", "Ball")]

take_dt %<>% 
  .[!(is.na(px) | is.na(pz))]

# Define "close pitches" 
take_dt %<>% 
  .[, on_margin := define_marginal(px, pz, eps_out = .15, eps_in = .05)]

# Make Figure ------------------------------------------------------------------
sz <- define_sz_rect(.15, .15)

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
        legend.position = c(.3, .9), 
        legend.box.background = element_rect(colour = "black", size = 1))

p2 <- ggplot(take_dt[pitch_sample]) + 
  aes(x = px, y = pz, shape = factor(des), color = factor(on_margin)) + 
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
        legend.position = "none")


p_both <- gridExtra::grid.arrange(p1, p2, nrow = 1)

ggsave("../output/strike_zone.png", p_both, width = 8, height = 5)

# Expore Optimal Margin --------------------------------------------------------
dt_marg <- data.table()
for (i in seq(0, .2, .05)) {
  for (j in seq(0, .2, .05)) {
    take_dt %<>% 
      .[, on_margin := define_marginal(px = px, pz = pz, bot = sz_bot, top = sz_top, 
                                       eps_in = i, eps_out = j)] %>% 
      .[, strike := ifelse(des == "Called Strike", 1, 0)]
    
    perc_strike <- mean(take_dt[on_margin == 1, strike], na.rm = T)
    dt_marg1 <- data.table(eps_in = i, eps_out = j, perc = perc_strike)
    dt_marg %<>% rbind(dt_marg1)
  }
  
}

ggplot(dt_marg) + 
  aes(x = eps_out, y = perc, color = factor(eps_in)) + 
  geom_line() + 
  labs(x = "Outter Epsilon", color = "Inner Epsilon", 
       y = "% Called Strikes", 
       title = "Optimal Marginal Strike Area") + 
  my_theme

ggsave("../output/strike_by_eps.png", 
       width = 8, height = 4)



