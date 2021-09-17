# Header -----------------------------------------------------------------------
# Proj: Umpire FE
# Author: Evan Flack (evanjflack@gmail.com)
# Desc:

# Libraries --------------------------------------------------------------------
library(data.table)
library(magrittr)
library(tictoc)
library(ggplot2)
library(stringr)

# Define user functions and plot themes
source("../supporting_code/define_functions.R")
source("../supporting_code/define_plot_theme.R")

start_log_file("log/02c_plot_umpire_fe")

# Read In/Prep Estimates -------------------------------------------------------

dt_ump_fe <- fread(paste0("../../data/out/umpire_fe_by_zone_all_pitches.csv"))

dt_ump_fe %<>% 
  .[order(on_margin, estimate), ] %>% 
  .[, ord := seq(1, .N), by = on_margin] %>% 
  .[, med := median(estimate), by = on_margin]

# Plot Estimates ---------------------------------------------------------------

ggplot(dt_ump_fe[obs >= 500, ]) +
  aes(x = ord, y = estimate, ymin = lb, ymax = ub, color = factor(on_margin)) +
  geom_point(size = .75) +
  geom_line() +
  # facet_wrap(~ on_margin, scales = "free_y") +
  geom_errorbar(alpha = .5, width = 0) +
  geom_hline(yintercept = c(0, 1)) +
  geom_hline(aes(yintercept = med, color = factor(on_margin)),
             linetype = 2, data = dt_ump_fe[, .SD[1], by = on_margin]) +
  labs(x = "Umpire (in order of stike %)", y = "Percentage Called Strikes",
       color = "Pitch Location") +
  scale_y_continuous(breaks = seq(0, 1, .2)) +
  annotate(geom = "text", x = 50, y = .07, label = "Clear Balls", size = 3) +
  annotate(geom = "text", x = 50, y = .35, label = "All Pitches", size = 3) +
  annotate(geom = "text", x = 50, y = .55, label = "On Margin", size = 3) +
  annotate(geom = "text", x = 50, y = .9, label = "Clear Strikes", size = 3) +
  my_theme +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none")

ggsave("../../output/figures/umpire_fe_by_location.png", width = 6, height = 4)

# End --------------------------------------------------------------------------

end_log_file()

# # Summary Statistics
# sum_ump <- dt_fit_umpire %>% 
#   .[obs >= 1000, ] %>% 
#   .[, .(min = min(estimate), max = max(estimate), var = var(estimate), 
#         med = median(estimate), 
#         first = quantile(estimate, .25), 
#         third = quantile(estimate, .75)), 
#     by = on_margin] %>% 
#   .[, range := max - min] %>% 
#   .[, iqr := third - first] %>% 
#   .[, .(on_margin, med, min, max, range, iqr, var)] %>% 
#   .[, lapply(.SD, signif, digits = 2), by = on_margin] %>% 
#   .[, lapply(.SD, as.character), by = on_margin] 
# 
# print(xtable(sum_ump, align = c("l", "l", rep("c", 6))), 
#       sanitize.text.function = force,
#       include.rownames = F)
