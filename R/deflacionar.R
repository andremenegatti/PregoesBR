deflacionar <- function(x,
                        indice_referencia = 4916.46, # Referencia: dez./2017
                        indice_no_periodo) {
  x * indice_referencia / indice_no_periodo
}
