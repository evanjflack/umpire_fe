# Header -----------------------------------------------------------------------
# Proj: Umpire Fixed Effects
# Author: Evan Flack (evanjflack@gmail.com)
# Desc: Creates sample of marginal pitches
# ------------------------------------------------------------------------------

library(data.table)
library(magrittr)
library(estimatr)

lib_base <- "~/Documents/projects/umpire_fe/"

# Functions --------------------------------------------------------------------
define_sz_rect <- function(eps1, eps2) {
  sz <- c(-0.83, 0.83, 1.52, 3.42)
  eps_out <- c(-1, 1, -1, 1)
  eps_in <- eps_out*(-1)
  sz_out <- sz + eps_out*eps1
  sz_in <- sz + eps_in*eps2
  
  rect <- geom_rect(xmin = sz[1], xmax = sz[2], ymin = sz[3], ymax = sz[4],
                    color = "black", show.legend = F, fill = NA, 
                    linetype = 1, alpha = .25)
  
  rect_light <- geom_rect(xmin = sz[1], xmax = sz[2], ymin = sz[3], ymax = sz[4],
                    color = "gray", show.legend = F, fill = NA, 
                    linetype = 1, alpha = .25)
  
  
  rect_out <- geom_rect(xmin = sz_out[1], xmax = sz_out[2], 
                        ymin = sz_out[3], ymax = sz_out[4],
                        color = "black", show.legend = F, fill = NA, 
                        linetype = 2)
  rect_in <- geom_rect(xmin = sz_in[1], xmax = sz_in[2], 
                       ymin = sz_in[3], ymax = sz_in[4],
                       color = "black", show.legend = F, fill = NA, 
                       linetype = 2)
  
  return(list(rect = rect, rect_out = rect_out, rect_in = rect_in, rect_light = rect_light))
}

define_marginal <- function(px, pz, left = -0.83, right = 0.83, bot = 1.52, 
                            top = 3.42, eps_out = .1, eps_in = .1) {
  left_out <- left - eps_out
  left_in <- left + eps_in
  right_in <- right - eps_in
  right_out <- right + eps_out
  
  bot_out <- bot - eps_out
  bot_in <- bot + eps_in
  top_in <- top - eps_in
  top_out <- top + eps_out
  
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
# pitches DT (regular season 2016)
pitch_dt <- fread("../Data/sample_pitch_data.csv") %>% 
  .[, year := substr(gameday_link, 5, 8)] %>% 
  .[, month := as.numeric(substr(gameday_link, 10, 11))] %>% 
  .[, day := as.numeric(substr(gameday_link, 13, 14))] %>% 
  .[(month == 4 & day >= 3) | month > 4, ] %>% 
  .[(month == 10 & day <= 3) | month < 10] %>% 
  .[home_team != "nas", ]

# Umpire DT
umpire_dt <- fread("../Data/umpire_data_clean.csv")

# Prep Data --------------------------------------------------------------------
# Home PLate Umpire
hp_umpire_dt <- umpire_dt %>% 
  .[, .(gameday_link, umpire_id_HP, umpire_name_HP)] %>% 
  .[, gameday_link := paste0("gid_", gameday_link)]
  
take_dt <- pitch_dt %>%
  # .[count == "0-0", ] %>% 
  .[des %in% c("Called Strike", "Ball")]

outcome_dt <- pitch_dt %>% 
  .[order(gameday_link, num), ] %>% 
  .[, .SD[.N], by = .(gameday_link, num)] %>% 
  .[, safe := ifelse(des %chin% c("In play, no out", "In play, run(s)", 
                                  "Ball", "Hit By Pitch", "Intent Ball"), 1, 0)] %>% 
  .[, .(gameday_link, num, safe)]

# Strike Zone Picture ----------------------------------------------------------
sz <- define_sz_rect(.2, .05)

ggplot(sample00_dt[1:100]) + 
  aes(x = px, y = pz, shape = factor(des)) + 
  geom_point() + 
  sz$rect_light + 
  # sz$rect_in + 
  sz$rect_out + 
  my_theme +
  labs(x = "Horizontal Position", 
       y = "Vertical Position") + 
  theme(legend.title = element_blank(), 
        legend.position = c(.4, .9))

ggsave(paste0(lib_base, "output/strike_zone.png"), 
       width = 4, height = 5)


# Optimal Margin ---------------------------------------------------------------
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
       y = "% Called Strikes")

ggsave(paste0(lib_base, "output/strike_by_eps.png"), 
       width = 5, height = 4)

# Define Margin (based on optimal) ---------------------------------------------
take_dt %<>% 
  .[, on_margin := define_marginal(px = px, pz = pz, bot = sz_bot, top = sz_top, 
                                   eps_in = 0, eps_out = .1)] %>% 
  .[, strike := ifelse(des == "Called Strike", 1, 0)]

DT <- take_dt %>% 
  merge(hp_umpire_dt, by = "gameday_link", all.x = T) %>% 
  .[!(is.na(px) | is.na(pz) | is.na(sz_bot) | is.na(sz_top) | is.na(umpire_id_HP))]

umpire_strike_marg <- DT %>% 
  .[on_margin == 1, ] %>% 
  .[, .(perc_strike = mean(strike), sd = sd(strike), obs = .N), by = umpire_name_HP] %>% 
  .[, se := sd/sqrt(obs)] %>% 
  .[, `:=`(lb = perc_strike - 1.96*se, ub = perc_strike + 1.96*se)] %>% 
  .[order(perc_strike)] %>% 
  # .[obs >= 100, ] %>% 
  .[, ord := seq(1, .N)] %>%
  .[, .(umpire_name_HP, perc_strike)] %>% 
  setnames("perc_strike", "perc_strike_margin")

umpire_strike_all <- DT %>% 
  .[, .(perc_strike = mean(strike), sd = sd(strike), obs = .N), by = umpire_name_HP] %>% 
  .[, se := sd/sqrt(obs)] %>% 
  .[, `:=`(lb = perc_strike - 1.96*se, ub = perc_strike + 1.96*se)] %>% 
  .[order(perc_strike)] %>% 
  # .[obs >= 100, ] %>% 
  .[, ord := seq(1, .N)] %>% 
  .[, .(umpire_name_HP, perc_strike, ord)]

umpire_strike <- umpire_strike_all %>% 
  merge(umpire_strike_marg, by = "umpire_name_HP")

ggplot(umpire_strike) + 
  aes(x = ord, y = perc_strike) + 
  geom_point(aes(y = perc_strike, color = "All")) + 
  geom_point(aes(y = perc_strike_margin, color = "Margin")) + 
  # geom_errorbar() +
  labs(x = "Umpire (in order of marginal stike %)", y = "Marginal Strike %")

ggsave(paste0(lib_base, "output/umpire_fe.png"), 
       width = 5, height = 4)

dt <- data.table(quant = seq(1, 100)) %>% 
  .[, mc := 30 - .2*quant] %>% 
  .[, dem := 40 - .3*quant] %>% 
  .[, tc:= ave(mc, FUN=cumsum)] %>% 
  .[, ac := tc/quant]


ggplot(dt) + 
  aes(x = quant) + 
  geom_line(aes(y = mc, linetype = "MC")) + 
  geom_line(aes(y = ac, linetype = "AC")) +
  geom_line(aes(y = dem, linetype = "D")) 


# Leave One Out First Stage ----------------------------------------------------
DT_marg <- DT %>% 
  .[on_margin == 1, ] %>% 
  .[, sum_ump := sum(strike), by = umpire_name_HP] %>% 
  .[, sum_game := sum(strike), by = gameday_link] %>% 
  .[, obs_ump := .N, by = umpire_name_HP] %>% 
  .[, obs_game := .N, by = gameday_link] %>% 
  .[, loo_strike_perc := (sum_ump - sum_game)/(obs_ump - obs_game)]


fit_fs <- lm(strike ~ loo_strike_perc, data = DT_marg)
install.packages("broom")

dt_fs <- broom::tidy(fit_fs) %>% 
  as.data.table() %>% 
  .[, term := c("Intercept", "Umpire Strike Avg (LOO)")] %>% 
  .[, lapply(.SD, signif, digits = 3), by = term]


xtable::xtable(dt_fs, )
install.packages("xtable")

library(knitr)

kable(dt_fs)

summary(fit_fs)




fit_rf <- lm(safe ~ loo_strike_perc, data = DT_marg)





summary(fit_rf)

summary(fit_fs)

# Plot Results -----------------------------------------------------------------




# Export -----------------------------------------------------------------------
head(sample00_dt)

