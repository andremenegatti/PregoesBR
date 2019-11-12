get_robust_std_errors <- function(model, HC = 'HC1') {

  lm_robust <- lmtest::coeftest(model,
                                vcov = sandwich::vcovHC(model, type = HC))

  tibble(coef = rownames(lm_robust),
             estimate = lm_robust[, 1],
             std_error = lm_robust[, 2],
             t_statistic = lm_robust[, 3],
             p_value = lm_robust[, 4])

}
