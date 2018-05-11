library(tidyverse)
library(RODBC)

## Functions
# Remove whitespace in column(s)
rmv_wht <- function(column) gsub('\\s+', '', column)
func_paste <- function(x) paste(unique(sum(x, na.rm = T)))

## SQL queries
# Creates a connection to the journal_rapp
journal_rapp<-odbcDriverConnect(paste("DRIVER=", "SQL Server",
                                      ";Database=", "journal_rapp",
                                      ";Server=", "sqlpjs",
                                      ";Port=", "",
                                      ";PROTOCOL=TCPIP",
                                      ";UID=", scan("PJS_password.txt",what = character(), n = 1L, quiet = T),
                                      ";PWD=", scan("PJS_password.txt",what = character(), n = 1L, skip = 1, quiet = T), sep = ""))


# Fetch available tables from connection
available_tables <- sqlTables(journal_rapp)

analyttkoder <- sqlFetch(journal_rapp, sqtable = "analytt")

analyttkoder$analyttkode <- rmv_wht(analyttkoder$analyttkode)

analytt_conv <- analyttkoder %>%
  filter(analyttkode %in% c("1220104",
                            "122010402",
                            "122010403",
                            "1502010235")) %>%
  select(analyttkode, analyttnavn) %>%
  mutate(analyttkode = paste0("0",analyttkode))

artkoder <- sqlFetch(journal_rapp, sqtable = "art")
artkoder$artkode <- rmv_wht(artkoder$artkode)
artkoder_conv <- artkoder %>%
  select(artkode, artnavn) %>%
  mutate(artkode = as.character(artkode))

hensikt <- sqlFetch(journal_rapp, sqtable = "hensikt")
metode <- sqlFetch(journal_rapp, sqtable = "metode")

metode$metodekode <- rmv_wht(metode$metodekode)

metode_conv <- metode %>%
  select(metodekode, metodenavn) %>%
  mutate(metodekode = as.character(metodekode)) %>%
  filter(metodekode %in% c('70070','70231','70024','70152','30068','30020','60265','10002','10092','10057'))

konklusjon <- sqlFetch(journal_rapp, sqtable = "konklusjonsregister")

konklusjon_conv <- konklusjon %>%
  mutate(konklusjonkode = as.character(konklusjonkode)) %>%
  select(konklusjonkode, konklusjonnavn)

provemateriale <- sqlFetch(journal_rapp, sqtable = "provemateriale")

provemateriale_conv <- provemateriale %>%
  mutate(provematerialekode = as.character(provematerialekode)) %>%
  select(provematerialekode, provematerialenavn)

resultat <- sqlFetch(journal_rapp, sqtable = "resultat")

# Data queries
myQuery <- "SELECT * 
FROM V1_SAK_M_RES_EIER 
WHERE aar = 2018 AND
(hensiktkode LIKE '02001%' OR hensiktkode LIKE '07%' OR hensiktkode LIKE '01001%' OR hensiktkode LIKE '08001%')
AND (artkode LIKE '01%' OR artkode LIKE '02%' OR artkode LIKE '04%')"

rawdata <- sqlQuery(journal_rapp, query = myQuery, as.is = TRUE)

# Close connection
odbcCloseAll()

# Data wrangling

rawdata_clean <- as.data.frame(lapply(rawdata,rmv_wht)) %>%
  mutate_all(as.character)

filtered_data <- rawdata_clean %>%
  filter(konkl_analyttkode %in% analytt_conv$analyttkode) %>%
  mutate(artkode = as.character(gsub("^0(.*?)$","\\1", artkode)),
         metodekode = as.character(gsub("^0(.*?)$", "\\1", metodekode)),
         konklusjonkode = as.character(gsub("^0(.*?)$", "\\1", konklusjonkode)),
         provematerialekode = as.character(gsub("^0(.*?)$", "\\1", provematerialekode))) %>%
  filter(metodekode %in% metode_conv$metodekode) %>%
  left_join(., artkoder_conv, by = "artkode") %>%
  left_join(., metode_conv, by = "metodekode") %>%
  left_join(., konklusjon_conv, by = "konklusjonkode") %>%
  left_join(., provemateriale_conv, by = "provematerialekode") %>%
  mutate(analyttkode = case_when(analyttkode_funn == konkl_analyttkode ~ analyttkode_funn,
                                 TRUE ~ konkl_analyttkode)) %>%
  left_join(., analytt_conv, by = "analyttkode") %>%
  mutate(artnavn = factor(artnavn),
         analyttnavn = factor(analyttnavn),
         metodenavn = factor(metodenavn),
         konklusjonnavn = factor(konklusjonnavn),
         provematerialenavn = factor(provematerialenavn),
         saksnr = paste(aar, ansvarlig_seksjon, innsendelsesnummer, sep = "-"),
         mottatt_dato = as.Date(mottatt_dato, "%d.%m.%y"),
         avsluttet_dato = as.Date(avsluttet_dato, "%d.%m.%y"),
         sak_avsluttet = as.Date(sak_avsluttet, "%d.%m.%y"),
         unik_id = paste(saksnr,provenummer,delprovenummer,undersokelsesnummer,resultatnummer,sep = "-")) %>%
  select(unik_id, aar,saksnr,provenummer,delprovenummer,provematerialenavn,mottatt_dato,sak_avsluttet,
         avsluttet_dato,eier_lokalitetnr,eier_lokalitetstype,
         eierreferanse,provematerialekode,metodenavn,artnavn,analyttnavn,
         resultatnummer,konklusjonnavn) %>%
  group_by(metodenavn, konklusjonnavn,analyttnavn) %>%
  mutate(ind = row_number()) %>%
  spread(metodenavn,konklusjonnavn) %>%
  ungroup() %>%
  mutate(PD = )

test <- reshape(filtered_data, timevar = "provenummer", idvar = "saksnr", direction = "wide")


filtered_data$saksnr <- factor(filtered_data$saksnr)
count(filtered_data, saksnr)




