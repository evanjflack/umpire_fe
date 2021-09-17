# umpire_fe

### Overview
This project uses Major League Baseball (MLB) pitch data and an umpire fixed effects methodology to study various phenomenon in baseball. For example, how do batters learn about and react to the strike zone of an umpire?

### Maintainers
* Evan Flack (evanjflack at gmail dot com), PhD Student, Stanford University, Department of Economics

### Contents

#### src
* `01_sample:` scripts to combine different data sets and make analtic samples (e.g. rgular season only)
* `02_features`: scripts to create features from pitch/game data (e.g. team records)
* `03_model:` scripts to estimate umpire fixed effects and their effect on player decisions
* `supporting code`: scripts to define functions/plot themes

#### output
* `figures`
* `tables`

### Data (not in repository)

I use the MLB ptich data from the 2015-2018 regular seasons, available on [Kaggle](https://www.kaggle.com/pschale/mlb-pitch-data-20152018). Thanks to Paul Schale for scraping and compiling this data! The src code above uses data from the following structure:
* `raw`
  * `kaggle_data:` data downloaded from the link above (same names for each data set)
* `med:` Folder for data sets from intermediate steps
* `out:` Output data used in figures/tables
