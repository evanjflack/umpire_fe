# Header -----------------------------------------------------------------------
# Proj: Umpire Fixed Effects
# Author: Evan Flack (evanjflack@gmail.com)
# Desc: Creates sample of 0-0 counts with close pitches
# ------------------------------------------------------------------------------

library(data.table)
library(magrittr)
library(estimatr)

# Functions --------------------------------------------------------------------

define_sz_rect <- function(eps) {
  sz <- c(-0.83, 0.83, 1.52, 3.42)
  eps_out <- c(-1, 1, -1, 1)
  eps_in <- eps_out*(-1)
  sz_out <- sz + eps_out*eps
  sz_in <- sz + eps_in*eps
  
  rect <- geom_rect(xmin = sz[1], xmax = sz[2], ymin = sz[3], ymax = sz[4],
                    color = "black", show.legend = F, fill = NA, 
                    linetype = 2, alpha = .25)
  rect_out <- geom_rect(xmin = sz_out[1], xmax = sz_out[2], 
                        ymin = sz_out[3], ymax = sz_out[4],
                        color = "black", show.legend = F, fill = NA, 
                        linetype = 1)
  rect_in <- geom_rect(xmin = sz_in[1], xmax = sz_in[2], 
                       ymin = sz_in[3], ymax = sz_in[4],
                       color = "black", show.legend = F, fill = NA, 
                       linetype = 1)
  
  return(list(rect = rect, rect_out = rect_out, rect_in = rect_in))
}

define_marginal <- function(px, pz, left = -0.83, right = 0.83, bot = 1.52, 
                            top = 3.42, eps = .1) {
  left_out <- left - eps
  left_in <- left + eps
  right_in <- right - eps
  right_out <- right + eps
  
  bot_out <- bot - eps
  bot_in <- bot + eps
  top_in <- top - eps
  top_out <- top + eps
  
  marg_left <- ifelse(px >= left_out & px <= left_in, 1, 0)
  marg_right <- ifelse(px >= right_in & px <= right_out, 1, 0)
  marg_left_right <- ifelse(marg_left | marg_right, 1, 0)
  strike_left_right <- ifelse(px >= left_out & px <= right_out, 1, 0)
  
  marg_bot <- ifelse(pz >= bot_out & pz <= bot_in, 1, 0)
  marg_top <- ifelse(pz >= top_in & pz <= top_out, 1, 0)
  marg_bot_top <- ifelse(marg_bot | marg_top, 1, 0)
  strike_bot_top <- ifelse(pz >= bot_out & pz <= top_out, 1, 0)
  
  # marg <- ifelse((marg_left_right & strike_bot_top), 1, 0)
  marg <- ifelse((marg_bot_top & strike_left_right) | (marg_left_right & strike_bot_top), 1, 0)
  
  return(marg)
}

# Read In Data -----------------------------------------------------------------
# pitches DT
pitch_dt <- fread("../Data/sample_pitch_data.csv") %>% 
  .[, year := substr(gameday_link, 5, 8)] %>% 
  .[, month := as.numeric(substr(gameday_link, 10, 11))] %>% 
  .[, day := as.numeric(substr(gameday_link, 13, 14))] %>% 
  .[(month == 4 & day >= 3) | month > 4, ]


unique(pitch_dt$des)

head(pitch_dt)


# Umpire DT
umpire_dt <- fread("../Data/umpire_data_clean.csv")


# Prep Data --------------------------------------------------------------------
# Home PLate Umpire
hp_umpire_dt <- umpire_dt %>% 
  .[, .(gameday_link, umpire_id_HP, umpire_name_HP)] %>% 
  .[, gameday_link := paste0("gid_", gameday_link)]
  
sample00_dt <- pitch_dt %>%
  .[count == "0-0", ] %>% 
  .[des %in% c("Called Strike", "Ball")]

outcome_dt <- pitch_dt %>% 
  .[order(gameday_link, num), ] %>% 
  .[, .SD[.N], by = .(gameday_link, num)] %>% 
  .[, safe := ifelse(des %chin% c("In play, no out", "In play, run(s)", 
                                  "Ball", "Hit By Pitch", "Intent Ball"), 1, 0)] %>% 
  .[, .(gameday_link, num, safe)]

# Optimal Margin ---------------------------------------------------------------

# Optimal Margin
dt_marg <- data.table()
for (i in seq(.01, .2, .01)) {
  sample00_dt %<>% 
    .[, on_margin := define_marginal(px = px, pz = pz, bot = sz_bot, top = sz_top, 
                                     eps = i)] %>% 
    .[, strike := ifelse(des == "Called Strike", 1, 0)]
  
  perc_strike <- mean(sample00_dt[on_margin == 1, strike], na.rm = T)
  dt_marg1 <- data.table(eps = i, perc = perc_strike)
  dt_marg %<>% rbind(dt_marg1)
}


ggplot(dt_marg) + 
  aes(x = eps, y = perc) + 
  geom_point()


# Define Margin (based on optimal) ---------------------------------------------


sample00_dt %<>% 
  .[, on_margin := define_marginal(px = px, pz = pz, bot = sz_bot, top = sz_top)] %>% 
  .[, strike := ifelse(des == "Called Strike", 1, 0)]

DT <- sample00_dt %>% 
  merge(hp_umpire_dt, by = "gameday_link", all.x = T) %>% 
  .[!(is.na(px) | is.na(pz) | is.na(sz_bot) | is.na(sz_top) | is.na(umpire_id_HP))]


umpire_strike <- DT %>% 
  .[on_margin == 1, ] %>% 
  .[, .(perc_strike = mean(strike), sd = sd(strike), obs = .N), by = umpire_name_HP] %>% 
  .[, se := sd/sqrt(obs)] %>% 
  .[, `:=`(lb = perc_strike - 1.96*se, ub = perc_strike + 1.96*se)] %>% 
  .[order(perc_strike)] %>% 
  .[, ord := seq(1, .N)]

ggplot(umpire_strike) + 
  aes(x = ord, y = perc_strike, ymin = lb, ymax = ub) + 
  geom_point() + 
  geom_errorbar() +
  labs(x = "Umpire (in order of stike %)", y = "Strike %")



# Leave One Out First Stage ----------------------------------------------------
DT_marg <- DT %>% 
  .[on_margin == 1, ] %>% 
  .[, sum_ump := sum(strike), by = umpire_name_HP] %>% 
  .[, obs_ump := .N, by = umpire_name_HP] %>% 
  .[, loo_strike_perc := (sum_ump - strike)/(obs_ump - 1)]

DT_marg %<>% 
  merge(outcome_dt, by = c("gameday_link", "num"), all.x = T)

fit_ols <- lm(safe ~ strike, data = DT_marg)
summary(fit_ols)

fit_iv <- iv_robust(safe ~ strike | loo_strike_perc, data = DT_marg, 
                    se_type = "stata")
summary(fit_iv)

fit_fs <- lm(strike ~ loo_strike_perc, data = DT_marg)
fit_rf <- lm(safe ~ loo_strike_perc, data = DT_marg)

summary(fit_rf)

summary(fit_fs)

# Plot Results -----------------------------------------------------------------
sz <- define_sz(.1)
ggplot(sample00_dt[1:100]) + 
  aes(x = px, y = pz, shape = factor(des)) + 
  geom_point() + 
  sz$rect + 
  sz$rect_in + 
  sz$rect_out + 
  my_theme




# Export -----------------------------------------------------------------------
head(sample00_dt)

