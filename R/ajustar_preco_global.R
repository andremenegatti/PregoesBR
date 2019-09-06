ajustar_preco_global <- function(preco, menor_proposta, menor_proposta_global, quantidade) {

  global <- checar_preco_global(menor_proposta = menor_proposta,
                                menor_proposta_global = menor_proposta_global,
                                lance_referencia = preco)

  ifelse(global, preco / quantidade, preco)

}
