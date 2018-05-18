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



test <- rawdata_clean %>%
  filter(konkl_analyttkode %in% analyttkoder$analyttkode) %>%
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
  mutate(fylkenr = gsub("^(.*?)[0-9][0-9]$", "\\1", kommunenr)) %>%
  filter(metodekode %in% metode$metodekode) %>%
  Reduce(conv_tables, function(x) left_join(x))
  left_join(., artkoder, by = "artkode") %>%
  left_join(., metode, by = "metodekode") %>%
  left_join(., konklusjon, by = "konklusjonkode") %>%
  left_join(., provemateriale, by = "provematerialekode") %>%
  left_join(., oppstalling, by = "oppstallingkode") %>%
  left_join(., subset(municipality, select = c(kommunenr, kommunenavn)), by = "kommunenr") %>%
  left_join(., driftsform, by = "driftsformkode") %>%
  left_join(., counties, by = "fylkenr") %>%
  left_join(., hensikt, by = "hensiktkode") %>%
  mutate(
    analyttkode = case_when(
      analyttkode_funn == konkl_analyttkode ~ analyttkode_funn,
      TRUE ~ konkl_analyttkode
    )
  ) %>%
  left_join(., analyttkoder, by = "analyttkode") %>%
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
    fylkenr,
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