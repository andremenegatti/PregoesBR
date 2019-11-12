add_1st_stage_residuals <- function(df, formula = 'win_bid ~ unidade_compradora', residuals_column_name = 'res_uasgs') {

  lm_uasgs <- lm(formula = formula, data = df)

  df$residuals_column <- lm_uasgs$residuals

  names(df)[which(names(df) == 'residuals_column')] <- residuals_column_name

  return(df)

}

