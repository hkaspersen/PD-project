library(RODBC)
library(tidyverse)
library(rlist)

## SQL queries
# Creates a connection to the journal_rapp
journal_rapp <- odbcDriverConnect(
  paste(
    "DRIVER=",
    "SQL Server",
    ";Database=",
    "journal_rapp",
    ";Server=",
    "sqlpjs",
    ";Port=",
    "",
    ";PROTOCOL=TCPIP",
    ";UID=",
    scan(
      "PJS_password.txt",
      what = character(),
      n = 1L,
      quiet = T
    ),
    ";PWD=",
    scan(
      "PJS_password.txt",
      what = character(),
      n = 1L,
      skip = 1,
      quiet = T
    ),
    sep = ""
  )
)

# Fetch available tables from connection, and also county and municipality data
conv_tables <- fetch_sql_tables(journal_rapp) %>%
  lapply(., clean_sql_tables) %>%
  filter_analytes(.) %>%
  filter_methods(.)

# Data queries
myQuery <- "SELECT * 
FROM V1_SAK_M_RES_EIER 
WHERE aar = 2018 AND
(hensiktkode LIKE '02001%' OR 
hensiktkode LIKE '07%' OR 
hensiktkode LIKE '01001%' OR 
hensiktkode LIKE '08001%')
AND (artkode LIKE '01%' OR 
artkode LIKE '02%' OR 
artkode LIKE '04%')"

rawdata <- sqlQuery(journal_rapp, query = myQuery, as.is = TRUE)

# Close connection
odbcCloseAll()

# Data wrangling
rawdata_clean <- rawdata %>%
  remove_whitespace_data(.) %>%
  create_report(.)
