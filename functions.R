# Remove whitespace in column(s)
rmv_wht <- function(column) gsub('\\s+', '', column)

# Paste together unique values from columns
func_paste <- function(x) paste(unique(x))

# Import municipality data from kartverket
import_municipality <- function() {
  municipality <-
    read.csv(
      "https://register.geonorge.no/subregister/sosi-kodelister/kartverket/kommunenummer.csv?",
      sep = ";",
      encoding = "UTF-8"
    )
  
  municipality <- municipality %>%
    mutate(Kodeverdi = sprintf("%04d", Kodeverdi)) %>%
    rename(kommunekode = Kodeverdi,
           kommunenavn = X.U.FEFF.Navn) %>%
    select(kommunekode, kommunenavn, Status, Oppdatert) %>%
    filter(Status == "Gyldig") %>%
    select(kommunekode, kommunenavn)
  return(municipality)
}

# Import county data from table
import_counties <- function() {
  counties <- read.table("fylkeliste.txt", sep = "\t", stringsAsFactors = F, header = TRUE) %>%
    select(Fylkesnummer,Fylker..18.stk.) %>%
    rename(fylkekode = Fylkesnummer,
           fylkenavn = Fylker..18.stk.) %>%
    group_by(fylkekode) %>%
    summarise_all(funs(func_paste)) %>%
    mutate(fylkekode = gsub("Nr. (.*?)", "\\1", fylkekode))
  return(counties)
}

# Fetch sql tables from connection
fetch_sql_tables <- function(connection) {
  analyttkoder <- sqlFetch(connection, sqtable = "analytt")
  artkoder <- sqlFetch(connection, sqtable = "art")
  hensikt <- sqlFetch(connection, sqtable = "hensikt")
  metode <- sqlFetch(connection, sqtable = "metode")
  konklusjon <-
    sqlFetch(connection, sqtable = "konklusjonsregister")
  provemateriale <- sqlFetch(connection, sqtable = "provemateriale")
  oppstalling <- sqlFetch(connection, sqtable = "oppstalling")
  driftsform <- sqlFetch(connection, sqtable = "driftsform")
  municipality <- import_municipality()
  counties <- import_counties()
  table_list <-
    lst(
      analyttkoder,
      artkoder,
      hensikt,
      metode,
      konklusjon,
      provemateriale,
      oppstalling,
      driftsform,
      municipality,
      counties
    )
  return(table_list)
}

# Clean sql tables
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

# Fix codes in "kode" column
fix_codes <- function(df) {
  code_column <- grep("kode$", colnames(df), value = TRUE)
  
  df <- df %>%
    mutate_at(.vars = code_column, .funs = funs(paste0("0", .)))
  return(df)
}

# Filter correct analytes
filter_analytes <- function(list) {
  list[["analyttkoder"]] <- list[["analyttkoder"]] %>%
    filter(analyttkode %in% c("1220104",
                              "122010402",
                              "122010403",
                              "1502010235"))
  return(list)
}

# Filter correct methods
filter_methods <- function(list) {
  list[["metode"]] <- list[["metode"]] %>%
    mutate(
      metodenavn_kort = case_when(
        grepl("immun", metodenavn, ignore.case = T) == TRUE ~ "immunhistochem",
        grepl("cellekultur", metodenavn, ignore.case = T) == TRUE |
          grepl("Bakterier og sopp", metodenavn, ignore.case = T) == TRUE ~ "cell_culture",
        grepl("antistoffer", metodenavn, ignore.case = T) == TRUE ~ "neutralization_AB",
        grepl("RT-PCR med sekvensering", metodenavn, ignore.case = T) == TRUE ~ "RT_PCR_Seq",
        grepl("påvisning med real-time RT-PCR", metodenavn, ignore.case = T) == TRUE ~ "RT_PCR",
        grepl("Virusanalyser med PCR", metodenavn, ignore.case = T) == TRUE ~ "PCR",
        grepl("sekvensering for karakterisering", metodenavn, ignore.case = T) == TRUE ~ "Seq",
        grepl("Histopatologi", metodenavn, ignore.case = T) == TRUE ~ "hist"
      )
    )
  return(list)
}

# Removes whitespace from data frame and converts all columns to character
remove_whitespace_data <- function(df) {
  df <- as.data.frame(lapply(df, rmv_wht)) %>%
    mutate_all(as.character)
  return(df)
}

# Remove leading zero from column
remove_zero_code <- function(column) {
  x <- as.character(gsub("^0(.*?)$", "\\1", column))
  return(x)
}

# Wrangle data frame to report
create_report <- function(df) {
  df <- df %>%
    filter(konkl_analyttkode %in% conv_tables[["analyttkoder"]][,1]) %>%
    mutate_at(
      .vars = c(
        "artkode",
        "metodekode",
        "konklusjonkode",
        "provematerialekode",
        "oppstallingkode",
        "hensiktkode"
      ),
      .funs = remove_zero_code
    ) %>%
    mutate(fylkekode = gsub("^(.*?)[0-9][0-9]$", "\\1", kommunenr)) %>%
    rename(kommunekode = kommunenr) %>%
    filter(metodekode %in% conv_tables[["metode"]][,1]) %>%
    mutate(
      analyttkode = case_when(
        analyttkode_funn == konkl_analyttkode ~ analyttkode_funn,
        TRUE ~ konkl_analyttkode
      )
    ) %>%
    reduce(conv_tables, left_join, .init = .) %>%
    mutate(
      artnavn = factor(artnavn),
      analyttnavn = factor(analyttnavn),
      metodenavn = factor(metodenavn),
      konklusjonnavn = factor(konklusjonnavn),
      provematerialenavn = factor(provematerialenavn),
      saksnr = paste(aar, ansvarlig_seksjon, innsendelsesnummer, sep = "-"),
      mottatt_dato = as.Date(mottatt_dato, "%d.%m.%y"),
      avsluttet_dato = as.Date(avsluttet_dato, "%d.%m.%y"),
      sak_avsluttet = as.Date(sak_avsluttet, "%d.%m.%y"),
      unik_id = paste(
        saksnr,
        provenummer,
        delprovenummer,
        undersokelsesnummer,
        resultatnummer,
        sep = "-"
      ),
      method_id = paste(
        saksnr,
        provenummer,
        sep = "-"
      )
    ) %>%
    select(
      unik_id,
      aar,
      mottatt_dato,
      avsluttet_dato,
      saksnr,
      method_id,
      eier_lokalitetnr,
      NAVN,
      fylkekode,
      fylkenavn,
      kommunenavn,
      hensiktnavn,
      provematerialenavn,
      metodenavn,
      metodenavn_kort,
      artnavn,
      oppstallingnavn,
      driftsformnavn,
      analyttnavn,
      resultatnummer,
      konklusjonnavn
    ) %>%
    group_by(unik_id) %>%
    mutate(
      analysis_count = 1:n(),
      singleton = n() == 1,
      ind = row_number()
    ) %>%
    ungroup() %>%
    filter(singleton | analysis_count == 2) %>%
    select(-analysis_count, -singleton, -ind) %>%
    group_by(metodenavn_kort, konklusjonnavn, analyttnavn) %>%
    spread(metodenavn_kort, konklusjonnavn) %>%
    mutate(
      PD = case_when(
        cell_culture == "Påvist" ~ 1,
        hist == "Påvist" ~ 1,
        immunhistochem == "Påvist" ~ 1,
        RT_PCR == "Påvist" ~ 1,
        Seq == "Påvist" ~ 1,
        TRUE ~ 0
      )
    )
  return(df)
}
