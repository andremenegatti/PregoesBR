get_summary_stats_by_period <- function(df, var, time_var) {
  
  var <- enquo(var)
  time_var <- enquo(time_var)
  
  summary_stats <- df %>%
    filter(!is.na(!! var)) %>% 
    group_by(!! time_var) %>% 
    summarise(mean = mean(!! var, na.rm = TRUE),
              sd = sd(!! var, na.rm = TRUE),
              percentile25 = quantile(!! var, 0.25, na.rm = TRUE),
              median = median(!! var, na.rm = TRUE),
              percentile75 = quantile(!! var, 0.75, na.rm = TRUE),
              n = n())
  
  return(summary_stats)

}
