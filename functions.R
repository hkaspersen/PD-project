fetch_sql_tables <- function(connection) {
  analyttkoder <- sqlFetch(connection, sqtable = "analytt")
  artkoder <- sqlFetch(connection, sqtable = "art")
  hensikt <- sqlFetch(connection, sqtable = "hensikt")
  metode <- sqlFetch(connection, sqtable = "metode")
  konklusjon <-
    sqlFetch(connection, sqtable = "konklusjonsregister")
  provemateriale <- sqlFetch(connection, sqtable = "provemateriale")
  oppstalling <- sqlFetch(connection, sqtable = "oppstalling")
  table_list <-
    list(analyttkoder,
         artkoder,
         hensikt,
         metode,
         konklusjon,
         provemateriale,
         oppstalling)
  names(table_list) <- c()
  return(table_list)
}

clean_sql_tables <- function(df) {
  code_column <- grep("kode$", colnames(df), value = TRUE)
  key <- gsub("(.*?)kode$", "\\1", code_column)
  name_column <- paste0(key, "navn")
  
  df <- df %>%
    mutate_at(.vars = code_column, .funs = rmv_wht) %>%
    select_at(.vars = c(code_column, name_column)) %>%
    mutate_at(.vars = code_column, .funs = as.character)
  return(df)
}

import_counties <- function() {
  counties <-
    read.csv(
      "https://register.geonorge.no/subregister/sosi-kodelister/kartverket/kommunenummer.csv?",
      sep = ";",
      encoding = "UTF-8"
    )
  
  counties_conv <- counties %>%
    mutate(Kodeverdi = sprintf("%04d", Kodeverdi)) %>%
    rename(kommunenr = Kodeverdi,
           kommunenavn = X.U.FEFF.Navn) %>%
    select(kommunenr, kommunenavn, Status, Oppdatert) %>%
    filter(Status == "Gyldig")
  return(counties_conv)
}

fix_codes <- function(df) {
  code_column <- grep("kode$", colnames(df), value = TRUE)
  
  df <- df %>%
    mutate_at(.vars = code_column, .funs = funs(paste0("0", .)))
  return(df)
}

filter_analytes <- function(df) {
  df <- df %>%
    filter(analyttkode %in% c("1220104",
                              "122010402",
                              "122010403",
                              "1502010235"))
  return(df)
}

filter_methods <- function(df) {
  method_df <- method_df %>%
    mutate(
      metodenavn_kort = case_when(
        grepl("immun", metodenavn, ignore.case = T) == TRUE ~ "immunhistochem",
        grepl("cellekultur", metodenavn, ignore.case = T) == TRUE ~ "cell_culture",
        grepl("antistoffer", metodenavn, ignore.case = T) == TRUE ~ "neutralization_AB",
        grepl("RT-PCR med sekvensering", metodenavn, ignore.case = T) == TRUE ~ "RT_PCR_Seq",
        grepl("påvisning med real-time RT-PCR", metodenavn, ignore.case = T) == TRUE ~ "RT_PCR",
        grepl("Virusanalyser med PCR", metodenavn, ignore.case = T) == TRUE ~ "PCR",
        grepl("sekvensering for karakterisering", metodenavn, ignore.case = T) == TRUE ~ "Seq",
        grepl("Histopatologi", metodenavn, ignore.case = T) == TRUE ~ "hist"
      )
    )
}







