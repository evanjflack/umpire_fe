library(pitchRx)


data(gids, package = "pitchRx")

# Games on august first
ids <- gids[grepl("2014_08_01", gids)]

paths <- makeUrls(gids = ids)[1:2]


paths[1]

library(XML)
library(XML2R)
urls <- paste0(paths, "/rawboxscore.xml")
obs <- XML2Obs(urls)

urls


http://gdx.mlb.com/components/game/mlb/year_2014/month_08/day_01/gid_2014_08_01_anamlb_tbamlb_1/rawboxscore.xml