# Funcao para criar dummies
dummify <- function(categorical_vector) {
  dummies <- dummy_cols(categorical_vector,
                        remove_first_dummy = TRUE)
  dummies[2:ncol(dummies)]
}
