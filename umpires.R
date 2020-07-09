library(XML2R)
library(pitchRx)
library(RSQLite)
library(dplyr)
library(DBI)

urls <- makeUrls(start = "2011-06-01", end = "2011-06-01")
sub("http://gd2.mlb.com/components/game/mlb/", "", head(urls))

files <- paste0(urls, "/inning/inning_all.xml")
obs <- XML2Obs(files, url.map = TRUE, quiet = TRUE)
table(names(obs))


umps <- XML2Obs("http://gd2.mlb.com/components/game/mlb/year_2011/month_02/day_26/gid_2011_02_26_phimlb_nyamlb_1/players.xml")

hi <- as.data.frame(umps)

obs %>%
  unlist()


db <- src_sqlite("GamedayDB.sqlite3", create = TRUE)
# Collect and store all PITCHf/x data from 2008 to now

scrape(start = "2008-07-06", end = "2008-07-07",
       suffix = "inning/inning_all.xml", connect = db$con)


library(pitchRx)
files <- c("inning/inning_all.xml", "inning/inning_hit.xml",
           "miniscoreboard.xml", "players.xml")
dat <- scrape(start = "2011-06-01", end = "2011-06-01", suffix = files)

