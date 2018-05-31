## Functions for PD_script.R

## -------------------------------- Output Paths ----------------------------------

stats_output <- "K:\\FAG\\EksterneDatakilder\\Fiskesykdom\\PD\\Formaterte data\\Statistikk_R\\"

report_output <- "K:\\FAG\\EksterneDatakilder\\Fiskesykdom\\PD\\Formaterte data\\Rapport_R\\"

## ---------------------------------- General -------------------------------------

# Remove whitespace in column(s)
rmv_wht <- function(column)
  gsub('\\s+', '', column)

# Paste together unique values from columns
func_paste <- function(x)
  paste(unique(x))

month_names <-
  data.frame(
    month_number = c(
      '01',
      '02',
      '03',
      '04',
      '05',
      '06',
      '07',
      '08',
      '09',
      '10',
      '11',
      '12'
    ),
    month_name = month.name
  ) %>%
  mutate(month_number = as.character(month_number),
         month_name = as.character(month_name))

## ------------------------------ Importing Data ----------------------------------

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
  counties <-
    read.table(
      "fylkeliste.txt",
      sep = "\t",
      stringsAsFactors = F,
      header = TRUE
    ) %>%
    select(Fylkesnummer, Fylker..18.stk.) %>%
    rename(fylkekode = Fylkesnummer,
           fylkenavn = Fylker..18.stk.) %>%
    group_by(fylkekode) %>%
    summarise_all(funs(func_paste)) %>%
    mutate(fylkekode = gsub("Nr. (.*?)", "\\1", fylkekode))
  return(counties)
}

# Fetch sql tables from connection
fetch_sql_tables <- function(connection) {
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

## ----------------------------- Data Wrangling --------------------------------------

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
filter_analytes <- function(df) {
  df <- df %>%
    filter(analyttkode %in% c("1220104",
                              "122010402",
                              "122010403",
                              "1502010235")) %>%
    mutate(analyttkode = case_when(analyttkode == "1220104" |
                                     analyttkode == "122010402" |
                                     analyttkode == "122010403" ~ paste0("0",analyttkode),
                                   TRUE ~ analyttkode))
  return(df)
}

# Filter correct methods
filter_methods <- function(list) {
  list[["metode"]] <- list[["metode"]] %>%
    filter(metodekode %in% c("70070",
                             "70231",
                             "70024",
                             "70152",
                             "30068",
                             "30020",
                             "60265",
                             "10002",
                             "10092",
                             "10057")) %>%
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

# Function for correct filtering of analyte codes
select_analytes <- function(x) {
  if (nrow(x) == 1)
    return(x)
  LV <- logical(nrow(x))
  for (i in 1:nrow(x)) {
    if(is.na(x[i,]$analyttkode_funn) | is.na(x[i,]$konkl_analyttkode)){
      LV[i] <- T
    } else{
      LV[i] <- !(x[i, ]$analyttkode_funn %in% x$konkl_analyttkode)
      if (x[i, ]$analyttkode_funn == x[i, ]$konkl_analyttkode) 
        LV[i] <- T
    }
  }
  return(x[LV,])
}


# Wrangle data frame to report
create_report <- function(df) {
  df <- df %>%
    mutate_at(
      .vars = c(
        "analyttkode_funn",
        "konkl_analyttkode",
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
    reduce(conv_tables, left_join, .init = .) %>%
    left_join(., analyttkoder, by = c("konkl_analyttkode" = "analyttkode")) %>%
    rename(konkl_analyttnavn = analyttnavn) %>%
    left_join(., analyttkoder, by = c("analyttkode_funn" = "analyttkode")) %>%
    rename(analyttnavn_funn = analyttnavn) %>%
    filter(analyttkode_funn %in% c("1220104",
                                   "122010402",
                                   "122010403",
                                   "1502010235",
                                   NA) &
             konkl_analyttkode %in% c("1220104",
                                      "122010402",
                                      "122010403",
                                      "1502010235",
                                      NA)) %>%
    mutate(
      artnavn = factor(artnavn),
      konkl_analyttnavn = factor(konkl_analyttnavn),
      analyttnavn_funn = factor(analyttnavn_funn),
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
      )
    ) %>%
    select(
      unik_id,
      aar,
      mottatt_dato,
      avsluttet_dato,
      saksnr,
      eier_lokalitetnr,
      NAVN,
      fylkekode,
      fylkenavn,
      kommunenavn,
      hensiktnavn,
      provematerialenavn,
      artnavn,
      oppstallingnavn,
      driftsformnavn,
      konkl_analyttnavn,
      analyttnavn_funn,
      resultatnummer,
      metodenavn,
      metodenavn_kort,
      konklusjonnavn
    )
  
  return(df)
}

## -------------------------------- Statistics ----------------------------------------------

# Calculates 95 % confidence intervals
get_binCI <- function(x, n) as.numeric(setNames(binom.test(x,n)$conf.int*100,
                                                c("lwr", "upr")))

# Calculate basic statistics on selected values
calc_stats <- function(df, group_var1, group_var2 = NULL) {
  count_cols <- as.character(unique(df$konklusjonnavn))
  
  if (is.null(group_var2)) {
  df <- df %>%
    group_by_at(.vars= vars(group_var1, konklusjonnavn)) %>%
    count() %>%
    spread(konklusjonnavn, n, fill = 0) %>%
    ungroup() %>%
    mutate(total = rowSums(.[,count_cols]),
           pPD = Påvist/total*100) %>%
    rowwise() %>%
    mutate(lwr = get_binCI(Påvist, total)[1],
           upr = get_binCI(Påvist, total)[2])
  } else {
    df <- df %>%
      group_by_at(.vars= vars(group_var1, group_var2, konklusjonnavn)) %>%
      count() %>%
      spread(konklusjonnavn, n, fill = 0) %>%
      ungroup() %>%
      mutate(total = rowSums(.[,count_cols]),
             pPD = Påvist/total*100) %>%
      rowwise() %>%
      mutate(lwr = get_binCI(Påvist, total)[1],
             upr = get_binCI(Påvist, total)[2])
  }
  return(df)
}

## ------------------------ Saving Data Frames to Disk -------------------------------------

# Saves each df in list as a .txt file in specified directory
save_df_from_list <- function(list, output_dir) {
  lapply(names(list), function(x)
    write.table(
      list[[x]],
      file = paste0(output_dir, x, ".txt"),
      sep = "\t",
      row.names = F
    ))
}