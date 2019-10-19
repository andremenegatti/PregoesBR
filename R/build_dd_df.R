build_dd_df <- function(df) {
  df %>%
    mutate(treat1 = if_else(comprasnet == 1 & abertura_lances >= data_20s & abertura_lances < data_3s, 1, 0),
           treat2 = if_else(comprasnet == 1 & abertura_lances > data_3s, 1, 0),
           bimestre = factor(inicio_bimestre),
           mes = factor(inicio_mes),
           semana = factor(inicio_semana),
           unidade_compradora = as.factor(str_c('unidade_', unidade_compradora)),
           marca_vencedor = as.factor(str_c('marca', marca_vencedor_principais)))
}
