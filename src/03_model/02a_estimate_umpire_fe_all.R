# Header -----------------------------------------------------------------------
# Proj: Umpire FE
# Author: Evan Flack (evanjflack@gmail.com)
# Desc: Estimates umpire fixed effects for tendency to call strikes on all 
#       pitches as well as by pitch location

# Libraries --------------------------------------------------------------------
library(data.table)
library(magrittr)
library(tictoc)
library(broom)
library(stringr)

# Define user functions and plot themes
source("../supporting_code/define_functions.R")

start_log_file("log/02a_estimate_umpire_fe_all")

# Read In Data -----------------------------------------------------------------

# All regular season pitches 2015-2018
take_dt <- fread("../../data/med/take_data.csv") %>% 
  .[, .(g_id, umpire_HP, called_strike, on_margin)]

# Prep Data --------------------------------------------------------------------

# Estimate Umpire FE -----------------------------------------------------------
# Estimate each umpires propensity to call a strike

# All pitches (remove intercept to get the mean for each umpire)
fit_umpire <- lm(called_strike ~ factor(umpire_HP) - 1, data = take_dt)

dt_fit_umpire_all <- tidy(fit_umpire) %>% 
  as.data.table() %>% 
  .[, umpire := gsub("factor\\(umpire_HP\\)", "", term)] %>% 
  .[, on_margin := "All"] %>% 
  .[order(estimate), ]

# By marginal status
# Same as above, by stratify whether the pitch was a (1) clear strike, (2) on 
# the margin, or (3) a clear ball.
fit_umpire_marg <- lm(called_strike ~ factor(umpire_HP):factor(on_margin) - 1, 
                      data = take_dt)

dt_fit_umpire_marg <- tidy(fit_umpire_marg) %>% 
  as.data.table()  %>% 
  .[, term := gsub("factor\\(umpire_HP\\)", "", term)] %>% 
  .[, term := gsub("factor\\(on_margin\\)", "", term)] %>% 
  .[, umpire := str_split_fixed(term, ":", 2)[, 1]] %>% 
  .[, on_margin := str_split_fixed(term, ":", 2)[, 2]]

# Number of observations per umpire
obs_umpire_all <- take_dt[, .(obs = .N), by = umpire_HP] %>% 
  .[, on_margin := "all"]
obs_umpire_margin <- take_dt[, .(obs = .N), by = .(umpire_HP, on_margin)]
obs_umpire <- rbind(obs_umpire_all, obs_umpire_margin) %>% 
  setnames("umpire_HP", "umpire")

# Combine on the two DTs of estimates
dt_fit_umpire <- rbind(dt_fit_umpire_all, dt_fit_umpire_marg) %>% 
  merge(obs_umpire, by = c("umpire", "on_margin")) %>% 
  .[, `:=`(lb = estimate - 1.96*std.error, ub = estimate + 1.96*std.error)] %>% 
  .[, .(umpire, on_margin, obs, estimate, std.error, statistic, p.value, lb, 
        ub)] %>% 
  .[order(umpire, on_margin), ]

# Export -----------------------------------------------------------------------

fwrite(dt_fit_umpire, 
       paste0("../../data/out/umpire_fe_by_zone_all_pitches.csv"))

end_log_file()
