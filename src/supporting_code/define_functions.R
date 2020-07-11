# start log file -----
# desc: (1) starts log file, (2) prints script name, (3) prints start time
# arg: log_file - logical, if T starts the log file
#      file_name - string, desired name name of log file (without .log)
# return: none
start_log_file <- function(log_file, file_name) {
  if (log_file == T) {
    full_name <- paste0(file_name, ".log")
    con <- file(full_name)
    sink(con)
    sink(con, type="message")
    message(full_name)
    message(Sys.time())
    tic()
  }
}

# end_log_file -----
# desc: (1) ends log file if one has been started
# arg: log_file - logical, if T there is a log file started that needs to
#         be stopped
# return: none
end_log_file <- function(log_file) {
  if (log_file == T) {
    message("Done.")
    message(Sys.time())
    toc()
    sink() 
    sink(type="message")
  }
}

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
  marg <- ifelse((marg_bot_top & strike_left_right) | (marg_left_right & strike_bot_top), 1, 
                 ifelse(strike_left_right & strike_bot_top, 2, 3))
  
  return(marg)
}


# my_theme -----
# desc: base theme for all ggplot objects
my_theme <- theme_bw() +
  theme(panel.grid = element_blank(),
        strip.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = .5, size = 15,
                                  margin = ggplot2::margin(t = 0, r = 0,
                                                           b = 20, l = 0)),
        plot.subtitle = element_text(hjust = .5, size = 10,
                                     margin = ggplot2::margin(t = 0, r = 0,
                                                              b = 10, l = 0)),
        axis.title = element_text(hjust = .5, size = 10),
        axis.text = element_text(hjust = .5, size = 7),
        strip.text = element_text(hjust = .5, size = 10),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 10,
                                                             b = 0, l = 0)),
        axis.title.x = element_text(margin = ggplot2::margin(t = 10, r = 0,
                                                             b = 0, l = 0)))

