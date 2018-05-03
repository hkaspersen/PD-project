library(tidyverse)
library(RODBC)


# Functions

rmv_wht <- function(column) gsub('\\s+', '', column)


# Store username and password
usr <- .rs.askForPassword("Username: ")
pwd <- .rs.askForPassword("Password: ")


# Creates a connection to the journal_rapp
journal_rapp<-odbcDriverConnect(paste("DRIVER=", "SQL Server",
                                      ";Database=", "journal_rapp",
                                      ";Server=", "sqlpjs",
                                      ";Port=", "",
                                      ";PROTOCOL=TCPIP",
                                      ";UID=", usr,
                                      ";PWD=", pwd, sep = ""))


# Fetch available tables from connection

available_tables <- sqlTables(journal_rapp)

analyttkoder <- sqlFetch(journal_rapp, sqtable = "analytt") 

analyttkoder$analyttkode <- rmv_wht(analyttkoder$analyttkode)

analytt_conv <- analyttkoder %>%
  filter(analyttkode %in% c("1220104",
                            "122010402",
                            "122010403",
                            "1502010235")) %>%
  select(analyttkode, analyttnavn)


# Data queries


myQuery <- "SELECT * FROM v2_sak_m_res WHERE konkl_analyttkode IN('1220104','122010402','122010403','1502010235') AND aar = 2018"



rawdata <- sqlQuery(journal_rapp, query = myQuery, as.is = TRUE)

count(rawdata, konkl_analyttkode)
# Close connection
odbcCloseAll()

# Data wrangling

rawdata$analyttkode_funn <- rmv_wht(rawdata$analyttkode_funn)

filtered_data <- rawdata %>%
  filter(analyttkode_funn == "1220104")




