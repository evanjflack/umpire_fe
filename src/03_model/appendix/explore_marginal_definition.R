# ------------------------------------------------------------------------------
# Proj: Umpire FE
# Author: Evan Flack (evanjflack@gmail.com)
# Desc:
# ------------------------------------------------------------------------------

# Libraries --------------------------------------------------------------------
library(ggplot2)
library(data.table)
library(magrittr)
library(cfo.behavioral)

source("../../supporting_code/define_functions.R")
source("../../supporting_code/define_plot_theme.R")

# start_log_file("log/02a_estimate_umpire_fe_all.R")

# Read In Data -----------------------------------------------------------------
message("Reading in data...")

pitch_dt <- fread("../../../data/out/reg_season_data_2015_2018.csv")

# Prep Data --------------------------------------------------------------------
message("Prepping data...")

model_dt <- pitch_dt


n1 <- nrow(model_dt)
model_dt %<>% 
  .[!(is.na(px) | is.na(pz) | is.na(umpire_HP))]
n2 <- nrow(model_dt)  
message(n1 - n2, " out of ", n1, "(", round((n1-n2)/n1, 4), 
        ") observations dropped. ", n2, " Obs left")


model_dt %<>% 
  .[, take := ifelse(code %chin% c("*B", "B", "C"), 1, 0)] %>% 
  .[take == 1, ]

model_dt %<>% 
  .[, called_strike := ifelse(code == "C", 1, 0)]

nrow(model_dt)

eps_in <- seq(0, .3, .05)
eps_out <- seq(0, .3, .05)

dtp <- data.table()
for (i in eps_in) {
  for (j in eps_out) {
    message(i, j)
    model_dt %<>% 
      .[, on_margin := define_marginal(px, pz, bot = sz_bot, top = sz_top, 
                                       eps_out = j, eps_in = i)]
    
    dtp1 <- calc_cmean(model_dt, "called_strike", "on_margin") %>% 
      .[, `:=`(eps_in = i, eps_out = j)]
    dtp %<>% rbind(dtp1)
  }
}

dtp %<>% 
  .[, spec := ifelse(eps_in == .1 & eps_out == .2, "Main Spec", "Other")] %>% 
  .[, spec := factor(spec, levels = c("Other", "Main Spec"))]


ggplot(dtp[on_margin == "On Margin"]) + 
  aes(x = eps_out, y = mean, color = factor(eps_in)) + 
  geom_line() + 
  geom_point(aes(size = factor(spec)), shape = 1, color = "black") + 
  geom_hline(yintercept = .5, linetype = 2) + 
  labs(color = "Inner Margin", y = "Percent Caled Strike", 
       x = "Outter Margin", size = "") + 
  scale_size_manual(values = c(0, 6)) + 
  my_theme

ggsave("../../../output/marginal_area_perc_strike.png", width = 6, height = 3)


# End --------------------------------------------------------------------------

  

print

library(xtable)

