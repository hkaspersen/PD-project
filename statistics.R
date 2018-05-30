## Statistics

# Basic statistics

stat_tables <- lst(pPD_total = calc_stats(stats_df, "konklusjonnavn"),
                    pPD_methods = calc_stats(stats_df, "metodenavn_kort"),
                    pPD_over_time = calc_stats(stats_df, "month_name"),
                    pPD_county = calc_stats(stats_df, "fylkenavn"),
                    pPD_municipality = calc_stats(stats_df, "kommunenavn"),
                    pPD_locality = calc_stats(stats_df, "NAVN"))

invisible(save_df_from_list(stat_tables, stats_output))


