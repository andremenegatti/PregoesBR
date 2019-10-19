get_selection_results <- function(step_model_forward, step_model_backward, fwd_summary = NULL, bwd_summary = NULL, include_intercept = FALSE) {

  if (is.null(fwd_summary)) {
    fwd_summary <- summary(step_model_forward)
  }

  if (is.null(bwd_summary)) {
    bwd_summary <- summary(step_model_backward)
  }

  if (!is.null(step_model_forward)) {
    forward_results_table <- tibble(selection = 'forward',
                                    criterion = c('adjr2', 'bic', 'cp'),
                                    selected_vars = map(.x = criterion,
                                                        .f = ~ find_selected_vars(step_model = step_model_forward,
                                                                                  criterion = .x,
                                                                                  model_summary = fwd_summary,
                                                                                  include_intercept = FALSE)
                                                        ),
                                    no_selected_vars = map_dbl(.x = selected_vars, .f = ~ length(.x)))

  } else {
    forward_results_table <- tibble()
  }

  if (!is.null(step_model_backward)) {
    backward_results_table <- tibble(selection = 'backward',
                                    criterion = c('adjr2', 'bic', 'cp'),
                                    selected_vars = map(.x = criterion,
                                                        .f = ~ find_selected_vars(step_model = step_model_backward,
                                                                                                  criterion = .x,
                                                                                                  model_summary = bwd_summary,
                                                                                                  include_intercept = FALSE)
                                                        ),
                                    no_selected_vars = map_dbl(.x = selected_vars, .f = ~ length(.x)))

  } else {
    backward_results_table <- tibble()
  }

  bind_rows(forward_results_table, backward_results_table)

}
