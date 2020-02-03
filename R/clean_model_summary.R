# Análise Placebo
clean_model_summary <- function(df) {

  df %>%
    broom::tidy() %>%
    filter(term %in% c('..1', '..2', '..3')) %>%
    mutate(
      term = case_when(
        term == '..1' ~ 'treat1',
        term == '..2' ~ 'treat2',
        term == '..3' ~ 'treat_placebo',
        !term %in% c('..1', '..2', '..3') ~ term
      )
    )

}
