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

# Strike Zone
define_sz <- function(eps) {
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
  
  return(list(sz_out = sz_out, sz_in = sz_in, rect = rect, rect_out = rect_out, 
              rect_in = rect_in))
}


# my_theme -----
# desc: base theme for all ggplot objects
my_theme <- theme_bw() +
  theme(panel.grid = element_blank(),
        strip.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = .5, size = 25,
                                  margin = ggplot2::margin(t = 0, r = 0, 
                                                           b = 20, l = 0)),
        plot.subtitle = element_text(hjust = .5, size = 20,
                                     margin = ggplot2::margin(t = 0, r = 0, 
                                                              b = 10, l = 0)),
        axis.title = element_text(hjust = .5, size = 20),
        axis.text = element_text(hjust = .5, size = 15),
        strip.text = element_text(hjust = .5, size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 10, 
                                                             b = 0, l = 0)),
        axis.title.x = element_text(margin = ggplot2::margin(t = 10, r = 0, 
                                                             b = 0, l = 0)))

