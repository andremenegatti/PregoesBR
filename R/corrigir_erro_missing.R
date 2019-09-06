corrigir_erro_missing <- function(quantidade_resultado,
                                  valor,
                                  quantidade) {

  if_else(is.na(quantidade_resultado), valor / quantidade, valor)

}
