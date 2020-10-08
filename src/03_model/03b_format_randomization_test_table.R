
head(dt_fit)

dt_print <- dt_fit %>% 
  clean_fit_dt(c("location", "name", "type", "fstat")) %>% 
  dcast(location + name + fstat ~ type, value.var = "est_se")

head(dt_print)

# Print 
print(xtable(dt_fit_both), sanitize.text.function = force, 
      include.rownames = F)
