calcular_intervalo_lances <- function(data_hora1, data_hora2 = NULL) {

  if (is.null(data_hora2)) {
    difftime(data_hora1,
             lag(data_hora1),
             units = 'secs') %>%
      as.numeric() %>%
      return()
  } else {
    difftime(data_hora1,
             data_hora2,
             units = 'secs') %>%
      as.numeric()
  }
}
