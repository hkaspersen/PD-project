library(tidyverse)
library(RODBC)
library(tibble)

## Import functions
source("functions.R")

## Get data
source("get_data.R")

# Data wrangling
rawdata_clean <- remove_whitespace_data(rawdata)

filtered_data <- data_wrangle(rawdata_clean)

