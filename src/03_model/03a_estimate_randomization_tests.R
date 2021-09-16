# Header -----------------------------------------------------------------------
# Proj: FE
# Author: Evan Flack (evanjflack@gmail.com)
# Desc: Tests random assignment of umpires by regressing umpire tendency on 
#       game level covariates
# ------------------------------------------------------------------------------

# Libraries --------------------------------------------------------------------
library(data.table)
library(magrittr)
library(stringr)
suppressMessages(library(lubridate, quietly = T))
library(broom)
library(tictoc)

# Define user functions
source("../supporting_code/define_functions.R")

start_log_file("log/03a_estimate_randomization_tests")

# Read in Data -----------------------------------------------------------------

# LOO game level umpiore strike FEs
ump_fe <- fread("../../data/out/game_level_umpire_fe.csv")

# Game level data
game_dt <- fread("../../data/kaggle_data/games.csv") %>% 
  .[, .(g_id, attendance)]

# Team winning perctages each game
team_wins <- fread(paste0("../../data/out/team_wins.csv")) %>% 
  .[, .(g_id, win_perc_home, win_perc_away)]

# AT-bat level data
atbat_dt <- fread("../../data/kaggle_data/atbats.csv")

# Prep Data --------------------------------------------------------------------

# Reshapes fixed effects to wide
ump_fe %<>% 
  .[, on_margin := factor(on_margin, levels = c("All", "Clear Ball", 
                                                "Clear Strike", "On Margin"), 
                          labels = c("all", "clear_ball", "clear_strike", 
                                     "on_margin"))] %>% 
  dcast(g_id ~ on_margin, value.var = "loo_ump_fe") %>% 
  setnames(names(.)[-1], paste0("loo_ump_fe_", names(.)[-1]))

# Handedness of starting pitcher
pitcher_hand <- atbat_dt %>% 
  .[, .SD[1], by = g_id, .SDcols = "p_throws"] %>% 
  .[, p_left := ifelse(p_throws == "L", 1, 0)] %>% 
  .[, .(g_id, p_left)]

# Change units of attendance to thousands
game_dt %<>% 
  .[, attendance := attendance/1000]

# Merge fixed effects and features
model_dt <- game_dt %>% 
  merge(ump_fe, by = "g_id") %>% 
  merge(team_wins, by = "g_id") %>% 
  merge(pitcher_hand, by = "g_id")

# Randomization Tests ----------------------------------------------------------

# Different locations for a pitch (sperate regressions for each)
location <- c("all", "clear_ball", "clear_strike", "on_margin")
# RHS variables/names
c_vars <- c("attendance", "win_perc_home", "win_perc_away", "p_left")
c_var_names <- c("Attendance", "Home Team Win Perc", "Away Team Win Perc",
                 "SP Left Handed")
dt_names <- data.table(variable = c_vars, 
                       name = c_var_names)

# Formula for joint estimation for all variables
form <- paste0("y ~ ", paste(c_vars, collapse = " + ")) %>% 
  as.formula()

dt_fit <- data.table()
# Loop through the different pitch locations
for (j in location) {
  # Set outcome to umpire FE for that pitch regression
  model_dt %<>% 
    .[, y := get(paste0("loo_ump_fe_", j))]
  
  # Mean of each variable
  dt_means <- model_dt[, lapply(.SD, mean), .SDcols = c_vars]
  dt_means <- suppressWarnings(melt(dt_means, value.name = "mean"))
  
  # Regression with all variables on RHS
  fit_all <- lm(form, data = model_dt)
  dt_fit_all <- tidy(fit_all) %>% 
    as.data.table() %>% 
    .[, type := "All"] %>% 
    .[, variable := term]
  fstat <- summary(fit_all)$fstatistic["value"]
  
  # Loop through each RHS variable to test by itself
  dt_fit_pair <- data.table()
  for (i in c_vars) {
    # Set RHS variable
    model_dt[, x := get(i)]
    # Estimate regression
    fit_pair <- lm(y ~ x, data = model_dt)
    dt_fit_pair1 <- tidy(fit_pair) %>% 
      as.data.table() %>% 
      .[, type := "Pair"] %>% 
      .[, variable := i]
    dt_fit_pair %<>% rbind(dt_fit_pair1)
  }
  
  # Combine pairwise and joint regression estimates
  dt_fit1 <- rbind(dt_fit_all, dt_fit_pair) %>% 
    .[term != "(Intercept)"] %>% 
    merge(dt_means, by = "variable") %>% 
    merge(dt_names, by = "variable") %>% 
    .[, location := j] %>% 
    .[, fstat := fstat]
  dt_fit %<>% rbind(dt_fit1)
}

# Export -----------------------------------------------------------------------

fwrite(dt_fit, paste0("../../data/out/game_level_randomization_tests.csv"))

end_log_file()
