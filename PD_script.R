install.packages("pacman", repos = "http://cran.us.r-project.org")
pacman::p_load(tidyverse, RODBC, tibble, rlist, xtable, jsonlite, leaflet, ggrepel)

## Import functions
source("functions.R")

## Get data
source("get_data.R")

## Run statistics
source("statistics.R")
