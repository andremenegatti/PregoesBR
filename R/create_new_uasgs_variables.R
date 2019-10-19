create_new_uasg_variables <- function(df, selection_results_df) {
  df %>%
    mutate(fwd_adjr2 = ifelse(unidade_compradora %in% selection_results_df$selected_vars[[1]], unidade_compradora, 'Outra') %>% factor(),
           fwd_bic = ifelse(unidade_compradora %in% selection_results_df$selected_vars[[2]], unidade_compradora, 'Outra') %>% factor(),
           fwd_cp = ifelse(unidade_compradora %in% selection_results_df$selected_vars[[3]], unidade_compradora, 'Outra') %>% factor(),
           bwd_adjr2 = ifelse(unidade_compradora %in% selection_results_df$selected_vars[[4]], unidade_compradora, 'Outra') %>% factor(),
           bwd_bic = ifelse(unidade_compradora %in% selection_results_df$selected_vars[[5]], unidade_compradora, 'Outra') %>% factor(),
           bwd_cp = ifelse(unidade_compradora %in% selection_results_df$selected_vars[[6]], unidade_compradora, 'Outra') %>% factor())
}
