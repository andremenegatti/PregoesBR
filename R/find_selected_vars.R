find_selected_vars <- function(step_model, criterion, model_summary = NULL, include_intercept = FALSE) {

  if (is.null(model_summary)) {
    model_summary <- summary(step_model)
  }

  if (criterion == 'adjr2') {
    selected_vars <- coef(step_model, which.max(model_summary$adjr2)) %>% names() %>% str_remove("\\.data_")
  } else if (criterion == 'bic') {
    selected_vars <- coef(step_model, which.min(model_summary$bic)) %>% names() %>% str_remove("\\.data_")
  } else if (criterion == 'cp') {
    selected_vars <- coef(step_model, which.min(model_summary$cp)) %>% names() %>% str_remove("\\.data_")
  }

  if (selected_vars[1] == "(Intercept)" & !include_intercept) {
    selected_vars <- selected_vars[2:length(selected_vars)]
  }

  selected_vars

}
