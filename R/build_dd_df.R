build_dd_df <- function(df) {
  df %>%
    mutate(treat1 = if_else(comprasnet == 1 & abertura_lances >= data_20s & abertura_lances < data_3s, 1, 0),
           treat2 = if_else(comprasnet == 1 & abertura_lances > data_3s, 1, 0),
           bimestre = factor(inicio_bimestre), mes = factor(inicio_mes), semana = factor(inicio_semana),
           unidade_compradora = as.factor(str_c('unidade_', unidade_compradora)),
           marca_vencedor = as.factor(str_c('marca_', marca_vencedor_principais)),
           indice_mes = dense_rank(inicio_mes)
    ) %>%
    mutate(regime_juridico = case_when(abertura_lances < data_20s ~ 1,
                                       abertura_lances >= data_20s & abertura_lances < data_3s ~ 2,
                                       abertura_lances >= data_3s ~ 3)
    ) %>%
    group_by(regime_juridico) %>%
    mutate(indice_mes_por_regime = dense_rank(inicio_mes))
}
