# Análise Placebo
# Função para automatizar extração de resultados dos modelos
get_tidy_estimates <- function(df) {
  # Extraindo coeficientes e erros padrao de 'treat1' e 'treat2'
  df %>%
    mutate(
      treat1_est = map_dbl(
        .x = model_summary,
        .f = ~ select_estimate(.x, 'treat1')
      ),
      treat2_est =  map_dbl(
        .x = model_summary,
        .f = ~ select_estimate(.x, 'treat2')
      ),
      treat_placebo_est = map_dbl(
        .x = model_summary,
        .f = ~ select_estimate(.x, 'treat_placebo')
      ),
      treat1_std = map_dbl(
        .x = model_summary,
        .f = ~ select_se(.x, 'treat1')
      ),
      treat2_std = map_dbl(
        .x = model_summary,
        .f = ~ select_se(.x, 'treat2')
      ),
      treat_placebo_std = map_dbl(
        .x = model_summary,
        .f = ~ select_se(.x, 'treat_placebo')
      )
    ) %>%
    # Calculando limites inferior e superior dos intervalos de confianca
    ## Nota: erros padrao convencionais
    mutate(treat1_upper = treat1_est + 2*treat1_std,
           treat1_lower = treat1_est - 2*treat1_std,
           treat2_upper = treat2_est + 2*treat2_std,
           treat2_lower = treat2_est - 2*treat2_std,
           treat_placebo_upper = treat_placebo_est + 2*treat_placebo_std,
           treat_placebo_lower = treat_placebo_est - 2*treat_placebo_std)
}
