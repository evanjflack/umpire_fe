# ------------------------------------------------------------------------------
# Proj: Umpire FE
# Author: Evan Flack
# Desc: Scrapes r/baseball for posts with omments abou umpires
# ------------------------------------------------------------------------------

# Libraries --------------------------------------------------------------------
library(RedditExtractoR)
library(data.table)
library(magrittr)
library(foreach)
library(iterators)
library(parallel)
library(doParallel)
library(cfo.behavioral)

# Start log file
start_log_file("log/scrape_reddit_posts")

# Umpire Names  ----------------------------------------------------------------
message("Getting list of umire names...")

pitch_dt <- fread("../../data/out/reg_season_data_2015_2018.csv")

umpire_dt <- pitch_dt %>% 
  .[, .SD[1], by = g_id, .SDcols = "umpire_HP"] %>% 
  .[, .(obs = .N), by = umpire_HP] %>% 
  .[order(-obs), ]

# Scraping Reddit --------------------------------------------------------------
message("Scrpaing reddit...")

registerDoParallel(cores = 4)
ump_urls <- foreach(ump = umpire_dt$umpire_HP[1:99], 
                    .combine = "rbind", 
                    .multicombine = T) %dopar% 
  {
    ump_urls1 <- reddit_urls(search_terms = ump, 
                             subreddit = "baseball", cn_threshold = 0, 
                             sort_by = "new", 
                             page_threshold = 50) %>% 
      as.data.table() %>% 
      .[, umpire_HP := ump]
  }
stopImplicitCluster()

# Export -----------------------------------------------------------------------
message("Exporting...")
fwrite(ump_urls, "../../data/out/reddit_umpire_urls.csv")

end_log_file()