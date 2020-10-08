
# Simulation

install.packages("mvtnorm")

library(mvtnorm)

N <- 100000
mu_e <- 0
mu_u <- 0
s_e <- 1
s_u <- 1
s_eu <- 0.5

sigma <- matrix(c(s_e, s_eu, s_eu, s_u), nrow = 2)

draw <- rmvnorm(n = N, mean = c(mu_e, mu_u), sigma = sigma) %>% 
  as.data.table() %>% 
  setnames(c("y_til", "y_hat")) %>% 
  .[, take := ifelse(y_til >= 0, 1, 0)] %>% 
  .[, call_strike := ifelse(y_hat <= 0, 1, 0)]

# ggplot(draw) + 
#   aes(x = y_til, y = y_hat, color = factor(take)) +
#   geom_point()

calls <- draw[, .(perc_strike = mean(call_strike)), by = take] %>% 
  .[order(take), ] %T>% 
  print()

pnorm(0, mu_u, s_u)




