# ------------------------------------------------------------------------------
# Proj: Umpire Fixed Effects
# Author: Evan Flack (evanjflack@gmail.com)
# Desc: Tests random assignment of umpires by regressing umpire tendency on 
#       game level covariates
# ------------------------------------------------------------------------------

# Libraries --------------------------------------------------------------------
library(data.table)
library(magrittr)
library(stringr)
library(lubridate)
library(broom)
library(xtable)
library(cfo.behavioral)

options(scipen = 999)

source("../supporting_code/define_functions.R")

# Read in Data -----------------------------------------------------------------
message("Reading in data...")

atbat_dt <- fread("../../data/kaggle_data/atbats.csv")

game_dt <- fread("../../data/kaggle_data/games.csv")

wins_dt <- fread(paste0("../../data/out/team_wins.csv"))

dt_fs_all <- fread("../../data/out/first_stage_game_all.csv")


dt_fs_marg <- fread("../../data/out/first_stage_game_marg.csv")


# # Prep Data ------------------------------------------------------------------
message("Prepping data...")

# Make marginal first pitch estaimtes dt wide
dt_fs_marg %<>% 
  .[, on_margin1 := factor(on_margin, levels = c("Clear Ball", "Clear Strike", 
                                                 "On Margin"), 
                           labels = c("clear_ball", "clear_strike", 
                                      "on_margin"))] %>% 
  .[, on_margin := NULL] %>% 
  dcast(g_id ~ on_margin1, value.var = "loo_strike_perc_marg") %>% 
  setnames(names(.)[-1], paste0("loo_strike_perc_", names(.)[-1]))

# Handedness of starting pitchers
pitcher_hand <- atbat_dt %>% 
  .[, .SD[1], by = g_id, .SDcols = "p_throws"] %>% 
  .[, p_left := ifelse(p_throws == "L", 1, 0)]

# Combines game-level features
featues_dt <- game_dt %>% 
  .[, .(g_id, attendance)] %>% 
  merge(wins_dt, by = "g_id") %>% 
  merge(pitcher_hand, by = "g_id")

# Merge in game-level features w/ game-level umpire tendency
ump_dt <- dt_fs_all %>% 
  merge(dt_fs_marg, by = "g_id") %>% 
  merge(featues_dt, by = "g_id")

# Randomization Tests ----------------------------------------------------------
message("Estimating randomization tests...")
location <- c("all", "clear_ball", "clear_strike", "on_margin")
c_vars <- c("attendance", "win_perc_home", "win_perc_away", "p_left")
c_var_names <- c("Attendance", "Home Team Win Perc", "Away Team Win Perc",
                 "SP Left Handed")
dt_names <- data.table(variable = c_vars, 
                       name = c_var_names)

# Formula for join estimation
form <- paste0("y ~ ", paste(c_vars, collapse = " + ")) %>% 
  as.formula()

dt_fit <- data.table()
for (j in location) {
  ump_dt %<>% 
    .[, y := get(paste0("loo_strike_perc_", j))]
  
  dt_means <- ump_dt[, lapply(.SD, mean), .SDcols = c_vars]
  dt_means <- suppressWarnings(melt(dt_means, variable.name = "variable", value.name = "Mean")) %>% 
    .[, Mean := ifelse(Mean <= 1, round(Mean, 2), prettyNum(round(Mean, 0), big.mark = ","))]
  
  # All Together
  fit_all <- lm(form, data = ump_dt)
  
  dt_fit_all <- tidy(fit_all) %>% 
    as.data.table() %>% 
    .[, type := "All"] %>% 
    .[, variable := term]
  
  fstat <- summary(fit_all)$fstatistic["value"]
  
  # Estimate Pair Wise
  dt_fit_pair <- data.table()
  for (i in c_vars) {
    ump_dt[, x := get(i)]
    fit_pair <- lm(y ~ x, data = ump_dt)
    
    dt_fit_pair1 <- tidy(fit_pair) %>% 
      as.data.table() %>% 
      .[, type := "Pair"] %>% 
      .[, variable := i]
    dt_fit_pair %<>% rbind(dt_fit_pair1)
  }
  
  dt_fit1 <- rbind(dt_fit_all, dt_fit_pair) %>% 
    .[term != "(Intercept)"] %>% 
    merge(dt_means, by = "variable") %>% 
    merge(dt_names, by = "variable") %>% 
    .[, location := j] %>% 
    .[, fstat := fstat]
  dt_fit %<>% rbind(dt_fit1)
}

# Export -----------------------------------------------------------------------

fwrite(dt_fit, paste0("../../data/out/rand_test.csv"))

end_log_file()
