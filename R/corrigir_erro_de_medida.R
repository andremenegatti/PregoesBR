corrigir_erro_de_medida <- function(valor,
                                    corte,
                                    quantidade) {

  if_else(valor > corte, valor / quantidade, valor)

}
