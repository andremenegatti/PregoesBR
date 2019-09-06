trim_df_grouped <- function(df, vars, group = NA, perc = 5, tail = 'both') {
  if (is.na(group)) {
    return(trim_df(df = df, vars = vars, perc = perc, tail = tail))
  } else { 
    df %>% nest(-group) %>% 
      mutate(data = map(.x = data, .f = ~ trim_df(df = .x, vars = vars, perc = perc, tail = tail))) %>% 
      unnest()
    }
}