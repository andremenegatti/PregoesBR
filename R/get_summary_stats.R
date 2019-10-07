get_summary_stats <- function(df, var, ...) {

  var <- enquo(var)

  summary_stats <- df %>%
    filter(!is.na(!! var)) %>%
    group_by(...) %>%
    summarise(mean = mean(!! var, na.rm = TRUE),
              sd = sd(!! var, na.rm = TRUE),
              percentile25 = quantile(!! var, 0.25, na.rm = TRUE),
              median = median(!! var, na.rm = TRUE),
              percentile75 = quantile(!! var, 0.75, na.rm = TRUE),
              n = as.double(n())) %>%
    ungroup()

  return(summary_stats)

}
