# Funcao para verificar se os precos informados referem-se ao valor total do contrato (TRUE)
# ou ao valor por unidade (FALSE)
checar_preco_global <- function(menor_proposta, menor_proposta_global, lance_referencia) {
  diff_unitaria <- abs(menor_proposta - lance_referencia)
  diff_global <- abs(menor_proposta_global - lance_referencia)

  resultado <- ifelse(diff_unitaria > diff_global, TRUE, FALSE)

  resultado

}
