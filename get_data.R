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
  filter_methods(.)

analyttkoder <- sqlFetch(journal_rapp, sqtable = "analytt") %>%
  clean_sql_tables(.)


# Data queries
myQuery <- "SELECT * 
FROM V1_SAK_M_RES_EIER 
WHERE aar = 2018 AND
(hensiktkode LIKE '02001%' OR 
hensiktkode LIKE '07%' OR 
hensiktkode LIKE '01001%' OR 
hensiktkode LIKE '08001%')
OR (artkode LIKE '01%' OR 
artkode LIKE '02%' OR 
artkode LIKE '04%')
OR (metodekode = '070070' OR
metodekode = '070231' OR
metodekode = '070024' OR
metodekode = '060265' OR
metodekode = '010092' OR
metodekode = '010057')
OR (analyttkode_funn = '1220104' OR
analyttkode_funn = '0122010402' OR
analyttkode_funn = '0122010403' OR
analyttkode_funn = '1502010235')
OR (konkl_analyttkode = '1220104' OR
konkl_analyttkode = '0122010402' OR
konkl_analyttkode = '0122010403' OR
konkl_analyttkode = '1502010235')"

rawdata <- sqlQuery(journal_rapp, query = myQuery, as.is = TRUE)

# Close connection
odbcCloseAll()

# Data wrangling

rawdata_clean <- rawdata %>%
  remove_whitespace_data(.) %>%
  mutate(saksnr = paste(aar, ansvarlig_seksjon, innsendelsesnummer, sep = "-"),
         unik_id = paste(
           saksnr,
           provenummer,
           delprovenummer,
           undersokelsesnummer,
           resultatnummer,
           sep = "-"
         ))

data_list <- split(rawdata_clean, rawdata_clean$unik_id)
filtered_data_list <- lapply(data_list, function(x) select_analytes(x))
filtered_data <- do.call(rbind, filtered_data_list)

final_report <- create_report(filtered_data)

stats_df <- final_report %>%
  filter(!is.na(konklusjonnavn)) %>%
  mutate(month_number = as.character(format(mottatt_dato, "%m"))) %>%
  left_join(., month_names, by = "month_number")
