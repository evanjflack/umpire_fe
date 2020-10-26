# ------------------------------------------------------------------------------
# Proj: Umpire FE
# Author: Evan Flack (evanjflack@gmail.com)
# Desc: Calculates the number of times a batter faces an umpire in a given year
#       (chronologically)
# ------------------------------------------------------------------------------

# Libraries --------------------------------------------------------------------
library(data.table)
library(magrittr)
library(stringr)
library(cfo.behavioral)

start_log_file("log/03_determine_times_faced_umpire")

# Read In Data -----------------------------------------------------------------
message("Reading in data...")

pitch_dt <- fread("../../data/out/reg_season_data_2015_2018.csv")

# Times Faced Umpire -----------------------------------------------------------
message("Times Faced Umpire...")

times_faced_umpire <- pitch_dt %>% 
  .[, .SD[1], by = .(g_id, batter_id, umpire_HP), 
    .SDcols = c("year")] %>% 
  .[order(batter_id, g_id), ] %>% 
  .[, ord := seq(1, .N), by = .(batter_id, year)] %>% 
  .[, ump_list := mapply(function(x, y) paste0(x[1:y], collapse=","), .SD, ord), 
    .SDcols = "umpire_HP", by = .(batter_id, year)] %>% 
  .[, ump_list1 := strsplit(ump_list, ",")] %>% 
  .[, ump_times := mapply(function(x, y) sum(x == y), umpire_HP, ump_list1)] %>% 
  .[, .(batter_id, g_id, umpire_HP, ump_times)]

# Export -----------------------------------------------------------------------
message("Exporting...")
fwrite(times_faced_umpire, 
       paste0("../../data/out/times_faced_umpire_per_year.csv"))

end_log_file()
