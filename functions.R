## Functions for PD_script.R

## -------------------------------- Output Paths ----------------------------------

stats_output <- "K:\\FAG\\EksterneDatakilder\\Fiskesykdom\\PD\\Formaterte data\\Statistikk_R\\"

report_output <- "K:\\FAG\\EksterneDatakilder\\Fiskesykdom\\PD\\Formaterte data\\Rapport_R\\"

## ---------------------------------- General -------------------------------------

# Inverts %in%
"%not_in%" <- Negate("%in%") 

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
                             "10057"))
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
# select_analytes <- function(x) {
#   if (nrow(x) == 1)
#     return(x)
#   LV <- logical(nrow(x))
#   for (i in 1:nrow(x)) {
#     if(is.na(x[i,]$analyttkode_funn) | is.na(x[i,]$konkl_analyttkode)){
#       LV[i] <- T
#     } else{
#       LV[i] <- !(x[i, ]$analyttkode_funn %in% x$konkl_analyttkode)
#       if (x[i, ]$analyttkode_funn == x[i, ]$konkl_analyttkode) 
#         LV[i] <- T
#     }
#   }
#   return(x[LV,])
# }

filter_and_create_no <- function(df) {
  df <- df %>%
    # removes whitespace from all columns
    remove_whitespace_data(.) %>%
    # test for correct analyte codes
    mutate(analyttkode = paste(analyttkode_funn, konkl_analyttkode, sep = "_"),
           test = case_when(grepl("01220104", analyttkode) == TRUE |
                              grepl("1502010235", analyttkode) == TRUE ~ 1,
                            TRUE ~ 0)) %>%
    filter(test == 1) %>%
    mutate(konklusjonkode = ifelse(grepl("01220104", konkl_analyttkode) == TRUE |
                                     grepl("1502010235", konkl_analyttkode) == TRUE,
                                   konklusjonkode, NA),
           kjennelse_resultat = ifelse(grepl("01220104", analyttkode_funn) == TRUE |
                                         grepl("1502010235", analyttkode_funn) == TRUE,
                                       kjennelse_resultat, NA),
           analyttkode_funn = ifelse(grepl("01220104", analyttkode_funn) == TRUE |
                                       grepl("1502010235", analyttkode_funn) == TRUE,
                                     analyttkode_funn, NA),
           konkl_analyttkode = ifelse(grepl("01220104", konkl_analyttkode) == TRUE |
                                        grepl("1502010235", konkl_analyttkode) == TRUE,
                                      konkl_analyttkode, NA)) %>%
    select(-test) %>%
    mutate(id_no = 1:n(),
           saksnr = paste(aar, ansvarlig_seksjon, innsendelsesnummer, sep = "-"),
           provenr = paste(saksnr, provenummer, sep = "-"))
  return(df)
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
        "hensiktkode",
        "kjennelse_resultat"
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
      sak_avsluttet = as.Date(sak_avsluttet, "%d.%m.%y")
    ) %>%
    select(
      provenr,
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
      konklusjonnavn,
      analyttnavn_funn,
      resultatnavn,
      metodenavn
    )
  return(df)
}

# collapses data frame on similar values, paste dissimilar
func_paste2 <- function(x) paste(unique(x[!is.na(x)]), collapse = ", ")

# scans through string of dates and returns the newest date
scan_max <- function(x) max(scan(text = x, what = "", sep = ",", quiet = TRUE, strip.white = TRUE))

# creates one line per sample in data frame, with conclusion for each method used
fix_report <- function(df) {
  df <- df %>%
    mutate(konklusjonnavn = paste(konklusjonnavn, konkl_analyttnavn, sep = " "),
           resultatnavn = paste(resultatnavn, analyttnavn_funn, sep = " ")) %>%
    mutate_at(vars(c(konklusjonnavn, resultatnavn)),
              funs(gsub(" NA", "", .))) %>%
    mutate_at(vars(c(konklusjonnavn, resultatnavn)),
              funs(gsub("NA", NA, .))) %>%
    select(-c(konkl_analyttnavn, analyttnavn_funn)) %>%
    mutate(metodenavn = case_when(grepl("RT-PCR", metodenavn) == TRUE ~ "RT-PCR",
                                  grepl("sekvensering", metodenavn) == TRUE ~ "Sekvensering",
                                  grepl("Cellekultur", metodenavn) == TRUE ~ "Cellekultur",
                                  grepl("immun", metodenavn) == TRUE ~ "Immunhistokjemi",
                                  grepl("Histopatologi", metodenavn) == TRUE ~ "Histopatologi")) %>%
    group_by(provenr) %>%
    mutate(id = 1:n(),
           RT_PCR_avsl_dato = ifelse(metodenavn == "RT-PCR",
                                     as.character(avsluttet_dato), NA)) %>%
    spread(metodenavn, resultatnavn, fill = NA) %>%
    select(-`<NA>`) %>%
    summarise_all(funs(func_paste2)) %>%
    select(-c(id, konklusjonnavn)) %>%
    mutate_at(vars(c(avsluttet_dato,
                     RT_PCR_avsl_dato,
                     `RT-PCR`,
                     Sekvensering,
                     Cellekultur,
                     Immunhistokjemi,
                     Histopatologi)),
              funs(gsub("^$", NA, .))) %>%
    mutate(avsluttet_dato = as.Date(sapply(avsluttet_dato, scan_max)))
  return(df)
}

## TODO: add function that creates report for all data so far in current year. 
## Create file: report_DATE, with extra column with final result for each saksnr.
## If one method == påvist, then new column == "Mistanke om". If two methods == påvist,
## then new column == Påvist, and all earlier results change to Påvist for that saksnr.
## Reset results when the fish have been slaughtered. Renew report once a day in the folder 
## "Aktuell_rapport" under maanedsrapporter (overwrite), and also in its respective folder
## for that current month and date.

# Filter out samples with wrong registering in PJS
filter_errors <- function(df) {
  df <- df %>%
    mutate(test = case_when(is.na(avsluttet_dato) == FALSE &
                              is.na(Cellekultur) == TRUE &
                              is.na(Histopatologi) == TRUE &
                              is.na(Immunhistokjemi) == TRUE &
                              is.na(`RT-PCR`) == TRUE ~ 1,
                            TRUE ~ 0)) %>%
    filter(test == 1)
  return(df)
}

# Create one row per saksnr, with summarised results per method
create_saksnr_report <- function(df) {
  df <- df %>%
    group_by(saksnr) %>%
    summarise_all(funs(func_paste2)) %>%
    ungroup() %>%
    rename("antall_prover" = provenr) %>%
    mutate(
      antall_prover = antall_prover %>%
        strsplit(split = ",") %>%
        sapply(function(x)
          length(unique(x))),
      Cellekultur = case_when(
        grepl("Mistanke", Cellekultur) == TRUE ~ "Mistanke",
        grepl("Påvist", Cellekultur) == TRUE ~ "Påvist",
        grepl("Ikke påvist", Cellekultur) == TRUE ~ "Ikke påvist",
        Cellekultur == "" ~ "Ikke utført"
      ),
      Histopatologi = case_when(
        grepl("Mistanke", Histopatologi) == TRUE ~ "Mistanke",
        grepl("Påvist", Histopatologi) == TRUE ~ "Påvist",
        grepl("Ikke påvist", Histopatologi) == TRUE ~ "Ikke påvist",
        Histopatologi == "" ~ "Ikke utført"
      ),
      Immunhistokjemi = case_when(
        grepl("Mistanke", Immunhistokjemi) == TRUE ~ "Mistanke",
        grepl("Påvist", Immunhistokjemi) == TRUE ~ "Påvist",
        grepl("Ikke påvist", Immunhistokjemi) == TRUE ~ "Ikke påvist",
        Immunhistokjemi == "" ~ "Ikke utført"
      ),
      `RT-PCR` = case_when(
        grepl("Mistanke", `RT-PCR`) == TRUE ~ "Mistanke",
        grepl("Påvist", `RT-PCR`) == TRUE ~ "Påvist",
        grepl("Ikke påvist", `RT-PCR`) == TRUE ~ "Ikke påvist",
        `RT-PCR` == "" ~ "Ikke utført"
      )
    ) %>%
    mutate(
      methods = apply(.[c("Cellekultur", "Histopatologi", "Immunhistokjemi", "RT-PCR")], 1, function(x)
        sum(x == "Påvist")),
      Resultat = ifelse(methods > 1, "Påvist PD", "Mistanke om PD"),
      Sekvensering = ifelse(Sekvensering == "", "Ikke utført", str_extract(Sekvensering, "SAV[0-9]"))
    ) %>%
    select(-methods)
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