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

conv_tables$kjennelse_res <- conv_tables$konklusjon %>%
  rename("resultatnavn" = konklusjonnavn,
         "kjennelse_resultat" = konklusjonkode)

analyttkoder <- sqlFetch(journal_rapp, sqtable = "analytt") %>%
  clean_sql_tables(.)


# Data queries
myQuery <- "SELECT * 
FROM V1_SAK_M_RES_EIER 
WHERE 
aar = 2018 AND
(artkode LIKE '01%' OR 
artkode LIKE '02%' OR 
artkode LIKE '04%')
AND(
(hensiktkode = '0100109003' OR 
hensiktkode = '0100108018' OR 
hensiktkode = '0200150' OR 
hensiktkode = '0100111003' OR
hensiktkode = '0800109' OR
hensiktkode = '08001')
OR(metodekode = '070070' OR
metodekode = '070231' OR
metodekode = '070024' OR
metodekode = '060265' OR
metodekode = '010092' OR
metodekode = '010057')
OR(analyttkode_funn LIKE '01220104%' OR
analyttkode_funn = '1502010235')
OR(konkl_analyttkode LIKE '01220104%' OR
konkl_analyttkode = '1502010235')
)"

rawdata <- sqlQuery(journal_rapp, query = myQuery, as.is = TRUE)

# Close connection
odbcCloseAll()

# Data wrangling
rawdata_filtered <- filter_and_create_no(rawdata)
data_list <- split(rawdata_filtered, rawdata_filtered$provenr)
filtered_data_list <- lapply(data_list, function(x) select_analytes(x))
filtered_data <- do.call(rbind, filtered_data_list)
report <- create_report(filtered_data)
report_filtered <- fix_report(report)