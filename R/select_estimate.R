# Análise Placebo
# Função para simplificar a extracao de coeficientes dos DFs de resultados
select_estimate <- function(df, treat_var) {
  filter(df, term == treat_var) %>%
    select(estimate) %>%
    unlist()
}
