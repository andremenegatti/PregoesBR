# Análise Placebo
# Função para simplificar a extracao de erros-padrão dos DFs de resultados
select_se <- function(df, treat_var) {
  filter(df, term == treat_var) %>%
    select(std.error) %>%
    unlist()
}
