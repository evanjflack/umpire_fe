define_sz_rect <- function(eps1, eps2) {
  sz <- c(-0.839, 0.839, 1.52, 3.42)
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

define_marginal <- function(px, pz, left = -0.839, right = 0.839, bot = 1.52, 
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
  marg <- ifelse((marg_bot_top & strike_left_right) | 
                   (marg_left_right & strike_bot_top), "on_margin", 
                 ifelse(strike_left_right & strike_bot_top, "clear_strike", 
                        "clear_ball"))
  
  return(marg)
}

define_strike <- function(px, pz, left = -0.839, right = 0.839, bot = 1.52, 
                            top = 3.42) {
  strike <- ifelse(px >= left & px <= right & pz >= bot & pz <= top, "Strike", "Ball")
  return(strike)
}


# start a .log file
start_log_file <- function(file_name = NULL, log_file = TRUE, print = TRUE) {
  if (log_file == TRUE) {
    if (is.null(file_name)) {
      stop("File name required")
    }
    full_name <- paste0(file_name, ".log")
    con <- file(full_name)
    sink(con)
    sink(con, type = "message")
    message(paste(rep("-", 80), collapse = ""))
    message(full_name)
    tic()
    message(Sys.time())
    message("")
  }
}

# End a .log file
end_log_file <- function() {
  if (length(showConnections(all = FALSE))) {
    message("")
    toc()
    message(Sys.time())
    message(paste(rep("-", 80), collapse = ""))
    sink()
    sink(type="message")
  }
}
