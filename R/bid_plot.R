bid_plot <- function(data, lines = FALSE, bestFit = FALSE) {
  my_plot <- data %>%
    plot_ly(x = ~ data_hora, y = ~ valor_lance,
            hoverinfo = "text",
            text = ~ paste("<b>Fornecedor:</b> ", Fornecedor,
                           "<br><b>Data/Hora:</b> ", data_hora,
                           "<br><b>Lance:</b> R$", valor_lance %>% format(decimal.mark = ',', big.mark = '.'),
                           "<br><b>Lance/kg:</b> R$", round(valor_lance_corr_defl, digits = 2) %>% format(decimal.mark = ',', big.mark = '.'),
                           "<br><b>Desconto total:</b>: R$", round(-1*desconto_bruto, digits = 3) %>% format(decimal.mark = ',', big.mark = '.'),
                           "<br><b>Desconto/kg:</b> R$", round(-1*increment, digits = 3) %>% format(decimal.mark = ',', big.mark = '.'),
                           "<br><b>Intervalo lance anterior:</b> ", round(intervalo_lance_anterior, 3) %>% format(decimal.mark = ',', big.mark = '.'), "s",
                           "<br><b>Intervalo menor lance:</b> ", round(intervalo_menor_lance, 3) %>% format(decimal.mark = ',', big.mark = '.'), "s",
                           "<br><b>Intervalo lance proprio:</b> ", round(intervalo_lance_proprio, 3) %>% format(decimal.mark = ',', big.mark = '.'), "s"
            )
    )  %>%
      add_markers(color = ~ Fornecedor, opacity = 0.8, showlegend = FALSE) %>%
      add_segments(x = ~ mean(inicio_fase_aleatoria), xend = ~ mean(inicio_fase_aleatoria), y = ~min(valor_lance), yend = ~max(valor_lance),
                   color = I("red"), opacity = 0.8, name = 'Inicio fase aleatória') %>%
      layout(xaxis = list(title = 'Horario de registro do lance'),
             yaxis = list(title = 'Valor do lance (reais)'))

  if (lines) {
    my_plot <- my_plot %>%
      add_lines(color = ~ Fornecedor, opacity = 0.4)
  }

  if (bestFit) {
    m <- lm(valor_lance ~ data_hora, data = data)
    my_plot <- my_plot %>%
      add_lines(y = ~ fitted(m), color = I('blue'))
  }

  my_plot

}
