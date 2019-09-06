# Funcao para retornar o 'minimo cumulativo' de um vetor numerico
# Retorna vetor de mesmo comprimento do vetor original
find_lowest <- function(x) {
  index = 1:length(x)
  map_dbl(.x = index,
          .f = ~ min(x[1:.x]))
}
