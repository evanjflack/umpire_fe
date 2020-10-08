# Zones ------------------------------------------------------------------------
sz_left <- -.839
sz_right <- .839
sz_bot <- 1.52
sz_top <- 3.42


strike_zones <- data.table(
  x1 = rep(seq(sz_left, sz_right - sz_right/3, 2*sz_right/3), each = 3), 
  x2 = rep(seq(sz_left + 2*sz_right/3, sz_right, 2*sz_right/3), each = 3), 
  y1 = rep(seq(sz_bot , sz_top - (sz_top - sz_bot)/3, (sz_top - sz_bot)/3), 3), 
  y2 = rep(seq(sz_bot + (sz_top - sz_bot)/3, sz_top, (sz_top - sz_bot)/3), 3), 
  z = factor(c(7, 4, 1, 8, 5, 2, 9, 6, 3))
)


ggplot(strike_zones) +
  geom_rect(aes(xmin = x1, xmax = x2, ymin = y2, ymax = y1, fill = z), 
            color = "grey20", alpha = .5) +
  geom_point(data = sample_dt, aes(x = px, y = pz, shape = factor(code))) + 
  # geom_text(aes(x = x1 + (x2 - x1)/2, y = y1 + (y2 - y1)/2, label = z),
  #           size = 7, fontface = 2, color = I("grey20")) +
  scale_x_continuous(limits = c(-2, 2)) + 
  my_theme + 
  # scale_x_continuous(limits = c(-1.5, 1.5), breaks = seq(-1, 1)) + 
  # scale_y_continuous(limits = c(.5, 4), breaks = seq(1, 4)) + 
  labs(x = "", y = "") + 
  theme(legend.position = "none")


# Expore Optimal Margin --------------------------------------------------------
dt_marg <- data.table()
for (i in seq(0, .2, .05)) {
  for (j in seq(0, .2, .05)) {
    take_dt %<>% 
      .[, rule_strike := define_strike(px = px, pz = pz, bot = sz_bot, top = sz_top)] %>% 
      .[, on_margin := define_marginal(px = px, pz = pz, bot = sz_bot, top = sz_top, 
                                       eps_in = i, eps_out = j)]
    
    dt_marg1 <- take_dt[, .(perc_strike = mean(strike)), by = .(on_margin, rule_strike)] %>% 
      .[, `:=`(eps_in = i, eps_out = j)]
    dt_marg %<>% rbind(dt_marg1)
  }
}

ggplot(dt_marg[on_margin == "On Margin"]) + 
  aes(x = eps_out, y = perc_strike, color = factor(eps_in)) + 
  geom_line() + 
  facet_wrap(~ rule_strike) + 
  labs(x = "Outter Epsilon", color = "Inner Epsilon", 
       y = "% Called Strikes") + 
  my_theme

ggplot(dt_marg) + 
  aes(x = eps_out, y = perc_strike, color = factor(eps_in)) + 
  geom_line() + 
  facet_wrap(~ on_margin) + 
  labs(x = "Outter Epsilon", color = "Inner Epsilon", 
       y = "% Called Strikes") + 
  my_theme

p <- ggplot(dt_marg) + 
  aes(x = eps_out, y = perc, color = factor(eps_in)) + 
  geom_line() + 
  labs(x = "Outter Epsilon", color = "Inner Epsilon", 
       y = "% Called Strikes", 
       title = "Optimal Marginal Strike Area") + 
  my_theme

ggsave("../output/strike_by_eps.png", p, 
       width = 8, height = 4)