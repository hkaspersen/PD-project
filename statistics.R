## Statistics

# Basic statistics

pPD_methods <- final_report %>%
  group_by(metodenavn_kort, konklusjonnavn) %>%
  count() %>%
  spread(konklusjonnavn, n, fill = 0) %>%
  ungroup() %>%
  mutate(total = rowSums(.[2:7]),
         pPD = Påvist/total*100) %>%
  rowwise() %>%
  mutate(lwr = get_binCI(Påvist, total)[1],
         upr = get_binCI(Påvist, total)[2])

pPD_over_time <- final_report %>%
  filter(!is.na(avsluttet_dato),
         !is.na(konklusjonnavn)) %>%
  mutate(month_number = as.character(format(mottatt_dato, "%m"))) %>%
  left_join(., month_names, by = "month_number") %>%
  group_by(month_name, konklusjonnavn) %>%
  count() %>%
  spread(konklusjonnavn, n, fill = 0) %>%
  ungroup() %>%
  mutate(total = rowSums(.[2:6]),
         pPD = Påvist/total*100) %>%
  rowwise() %>%
  mutate(lwr = get_binCI(Påvist, total)[1],
         upr = get_binCI(Påvist, total)[2])

pPD_county <- final_report %>%
  filter(!is.na(avsluttet_dato),
         !is.na(konklusjonnavn)) %>%
  group_by(fylkenavn, konklusjonnavn) %>%
  count() %>%
  spread(konklusjonnavn, n, fill = 0) %>%
  ungroup() %>%
  mutate(total = rowSums(.[2:6]),
         pPD = Påvist/total*100) %>%
  rowwise() %>%
  mutate(lwr = get_binCI(Påvist, total)[1],
         upr = get_binCI(Påvist, total)[2])

pPD_municipality <- final_report %>%
  filter(!is.na(avsluttet_dato),
         !is.na(konklusjonnavn)) %>%
  group_by(kommunenavn, konklusjonnavn) %>%
  count() %>%
  spread(konklusjonnavn, n, fill = 0) %>%
  ungroup() %>%
  mutate(total = rowSums(.[2:6]),
         pPD = Påvist/total*100) %>%
  rowwise() %>%
  mutate(lwr = get_binCI(Påvist, total)[1],
         upr = get_binCI(Påvist, total)[2])






