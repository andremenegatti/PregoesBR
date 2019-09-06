make_lineplot <- function(df, var, time_var, stat = 'all', return_stats = FALSE) {
  
  var <- enquo(var)
  var_name <- quo_name(var)
  
  time_var <- enquo(time_var)
  time_var_name <- quo_name(time_var)
  
  summary_stats <- get_summary_stats_by_period(df, !! var, !! time_var)
  
  summary_stats_gathered <- summary_stats %>% 
    gather(-!! time_var, -n, -sd, key = statistic, value = valor)
  
  if (stat != 'all') {
    summary_stats_gathered <- summary_stats_gathered %>% 
      filter(statistic %in% stat)
  }
 
  plot <- ggplot(data = summary_stats_gathered,
                 mapping = aes(x = !! time_var, group = statistic, color = statistic, y = valor)) + 
    geom_line(size = 1) +
    ggtitle(var_name, subtitle = time_var_name) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
  
  if (time_var_name == "inicio_trimestre") {
    plot <- plot +
      scale_x_date(breaks = as.Date(str_c(2008:2017, '-01-01')), labels = str_c(2008:2017, ' - Q1'), date_minor_breaks = '3 months')
  } else {
    plot <- plot +
      scale_x_date(breaks = as.Date(str_c(2008:2017, '-01-01')), labels = as.character(2008:2017))
  }

  if (!return_stats) return(plot)
  else return(list(plot = plot, summary_stats = summary_stats))
                   
}

# make_lineplot(df = df_intervalo, var = below_10_sec, time_var = inicio_trimestre, stat = 'all')
