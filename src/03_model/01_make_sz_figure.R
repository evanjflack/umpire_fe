# Header -----------------------------------------------------------------------
# Proj: Umpire FE
# Author: Evan Flack (evanjflack@gmail.com)
# Desc: Makes explanatory figure of strike zone and marginal area

# Libraries --------------------------------------------------------------------
library(ggplot2)
library(data.table)
library(magrittr)
library(tictoc)
library(cowplot)

source("../supporting_code/define_functions.R")
source("../supporting_code/define_plot_theme.R")

start_log_file("log/01_make_sz_figure")

# Read In Data -----------------------------------------------------------------
message("Reading in data ...")

# All regular season pitches 205-2018
pitch_dt <- fread("../../data/out/reg_season_data_2015_2018.csv")

# Prep Data --------------------------------------------------------------------

# Only pitches that are taken and have non-missing position
take_dt <- pitch_dt %>%
  .[, take := ifelse(code %chin% c("*B", "B", "C"), 1, 0)] %>% 
  .[take == 1, ] %>% 
  .[!(is.na(px) | is.na(pz))]

# Define marginal pitches
take_dt %<>% 
  .[, on_margin := define_marginal(px, pz, eps_out = .3, eps_in = .3)] %>% 
  .[, strike := ifelse(code == "C", 1, 0)]

# Make Figure ------------------------------------------------------------------

# Strike zone rectangles
sz <- define_sz_rect(.1, .1)

# Sample of 100 pitches (not in dirt)
pitch_sample <- sample(1:nrow(take_dt), 100)
sample_dt <- take_dt %>% 
  .[code != "*B"] %>% 
  .[pitch_sample, ] %>% 
  copy() %>% 
  .[, code := factor(code, levels = c("C", "B"), 
                     labels = c("Called Strike", "Called Ball"))]

# Strike zone 
p1 <- ggplot(sample_dt[!is.na(code)]) + 
  aes(x = px, y = pz, shape = factor(code)) + 
  geom_point(alpha = .75) + 
  sz$rect + 
  my_theme +
  scale_x_continuous(limits = c(-2, 2)) + 
  labs(x = "Horizontal Position", 
       y = "Vertical Position", 
       subtitle = "Strike Zone") + 
  theme(legend.title = element_blank(), 
        legend.position = "bottom")

# Strike zone w/ marginal area
p2 <- ggplot(sample_dt[!is.na(code)]) + 
  aes(x = px, y = pz, shape = factor(code)) + 
  geom_point(alpha = .75) + 
  sz$rect_light + 
  sz$rect_in + 
  sz$rect_out + 
  my_theme +
  scale_x_continuous(limits = c(-2, 2)) + 
  labs(x = "Horizontal Position", 
       y = "Vertical Position", 
       subtitle = "Strike Zone with Marginal Area") + 
  theme(legend.title = element_blank(), 
        legend.position = "bottom")

# Combine plots
p_both <- plot_grid(p1, p2, nrow = 1)

# Export -----------------------------------------------------------------------

ggsave("../../output/figures/strike_zone.png", p_both, 
       width = 6, height = 4, units = "in")

end_log_file()
