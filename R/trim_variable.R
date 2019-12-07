trim_variable <- function(x, perc = 5, tail = 'both', same_length = TRUE) {

  if (class(perc) != "numeric") {
    stop("'perc' must be a number betweeen 0 and 100")
  } else if (perc <= 0 | perc >= 100) {
    stop("'perc' must be a number betweeen 0 and 100")
  }

  if (!tail %in% c('both', 'lower', 'higher')) {
    stop("'tail' must be one of the following: 'both', 'lower', 'higher'.")
  }

  if (!"numeric" %in% class(x)) {
    stop("'x' must be a numeric vector")
  }




  # Caso 1 (default): remover outliers de ambos os lados da distribuicao
  if (tail == 'both') {

    # Como cortaremos de ambos os lados, dividimos por 2
    cut <- perc / 100 / 2

    # Definindo valores de corte
    upper <- quantile(x, 1 - cut, na.rm = TRUE)
    lower <- quantile(x, cut, na.rm = TRUE)

    trimmed_x <- ifelse(x > lower & x < upper, x, NA)


    # Caso 2: cortar apenas de um lado da distribuicao
  } else {
    cut <- perc / 100

    # Definindo valores de corte
    upper <- quantile(x, 1 - cut, na.rm = TRUE)
    lower <- quantile(x, cut, na.rm = TRUE)

    # Sub-caso 2.1: cortar apenas do lado esquerdo da distribuicao
    if (tail == 'lower') {

      trimmed_x <- ifelse(x > lower, x, NA)

      # Sub-caso 2.2: cortar apenas do lado direito da distribuicao
    } else {

      trimmed_x <- ifelse(x < upper, x, NA)

    }
  }

  if (!same_length) trimmed_x <- trimmed_x[!is.na(trimmed_x)]

  return(trimmed_x)

}
