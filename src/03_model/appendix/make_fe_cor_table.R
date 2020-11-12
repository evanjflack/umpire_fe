# ------------------------------------------------------------------------------
# Proj: Umpire FE
# Author: Evan Flack (evanjflack@gmail.com)
# Desc: Makes correlation table between different location fixed effects
# ------------------------------------------------------------------------------

# Libraries --------------------------------------------------------------------
library(data.table)
library(xtable)
library(magrittr)


# Read In Data -----------------------------------------------------------------
message("Reading in data...")

umpire_zone_fe <- fread("../../../data/out/umpire_zone_fe.csv")

# Prep Data --------------------------------------------------------------------
umpire_zone_fe %<>% 
  dcast(umpire ~ on_margin, value.var = c("estimate", "obs")) %>% 
  .[obs_All >= 1000] %>% 
  .[, c("umpire", grep("estimate_", names(.), value = T)), with = F] %>% 
  setnames(names(.), gsub("estimate_", "", names(.))) %>% 
  .[, All := NULL]

# Correlation ------------------------------------------------------------------
cor_dt <- cor(umpire_zone_fe[, -1]) %>% 
  as.data.table(keep.rownames = T) %>% 
  .[, lapply(.SD, round, digits = 3), by = "rn"] %>% 
  setnames("rn", "")

# Print ------------------------------------------------------------------------
print(xtable(cor_dt), include.rownames = F)

# End --------------------------------------------------------------------------