get_summary_stats_by_period2 <- function(df, var, time_var, gather = FALSE, pretty_names = TRUE) {

  x <- deparse(substitute(time_var))

  var <- enquo(var)
  time_var <- enquo(time_var)

  summary_stats <- df %>%
    filter(!is.na(!! var)) %>%
    group_by(!! time_var) %>%
    summarise(mean = mean(!! var, na.rm = TRUE),
              percentile25 = quantile(!! var, 0.25, na.rm = TRUE),
              median = median(!! var, na.rm = TRUE),
              percentile75 = quantile(!! var, 0.75, na.rm = TRUE),
              sd = sd(!! var, na.rm = TRUE),
              n = n()) %>%
    rename('Tempo' = !! time_var) %>%
    mutate('time_var' = x)

  if (pretty_names) {
    summary_stats <- summary_stats %>%
      rename(`Media` = mean,
             `Primeiro Quartil` = percentile25,
             `Mediana` = median,
             `Terceiro Quartil` = percentile75)
  }

  if (gather) {
    summary_stats <- summary_stats %>%
      gather(-Tempo, -time_var, -sd, -n,
             key = statistic, value = valor)
  }

  return(summary_stats)

}
