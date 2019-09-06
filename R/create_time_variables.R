create_time_variables <- function(df, time_var = abertura_lances) {

  time_var <- enquo(time_var)

  df %>%
    mutate(ano = year(!!time_var) %>% factor(),
           inicio_ano = str_c(as.character(year(!!time_var)), '-01-01') %>% as.Date(),
           inicio_mes = str_c(as.character(year(!!time_var)), '-', str_pad(month(!!time_var), 2, 'left', '0'), '-01') %>% as.Date(),
           inicio_semana = str_c(as.character(year(!!time_var)), '-01-01') %>% as.Date() + 7*(week(!!time_var) - 1),
           inicio_bimestre = case_when(month(!!time_var) %in% 1:2 ~ str_c(as.character(year(!!time_var)), '-01-01'),
                                       month(!!time_var) %in% 3:4 ~ str_c(as.character(year(!!time_var)), '-03-01'),
                                       month(!!time_var) %in% 5:6 ~ str_c(as.character(year(!!time_var)), '-05-01'),
                                       month(!!time_var) %in% 7:8 ~ str_c(as.character(year(!!time_var)), '-07-01'),
                                       month(!!time_var) %in% 9:10 ~ str_c(as.character(year(!!time_var)), '-09-01'),
                                       month(!!time_var) %in% 11:12 ~ str_c(as.character(year(!!time_var)), '-11-01')) %>% as.Date(),
           # inicio_bimestre = if_else(month(!!time_var) %% 2 == 0, str_c(as.character(year(!!time_var)), '-', str_pad(month(!!time_var) - 1, 2, 'left', '0'), '-01') %>% as.Date(), inicio_mes),
           inicio_trimestre = case_when(month(!!time_var) %in% 1:3 ~ str_c(as.character(year(!!time_var)), '-01-01'),
                                        month(!!time_var) %in% 4:6 ~ str_c(as.character(year(!!time_var)), '-04-01'),
                                        month(!!time_var) %in% 7:9 ~ str_c(as.character(year(!!time_var)), '-07-01'),
                                        month(!!time_var) %in% 10:12 ~ str_c(as.character(year(!!time_var)), '-10-01')) %>% as.Date(),
           inicio_semestre = if_else(month(!!time_var) %in% 1:6,
                                     str_c(as.character(year(!!time_var)), '-01-01'),
                                     str_c(as.character(year(!!time_var)), '-07-01')) %>% as.Date() )
}
