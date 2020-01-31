# Funcao para contar UASGs selecionadas e nao selecionadas
checar_selecao <- function(df) {
  df %>%
    select(-id_item, -num_unidade_compradora) %>%
    mutate_all(.funs = ~ ifelse(. == 'Outra', 'Nao selecionado', 'Selecionado')) %>%
    gather(key = 'metodo_selecao', value = 'resultado_selecao') %>%
    count(metodo_selecao, resultado_selecao)
}
