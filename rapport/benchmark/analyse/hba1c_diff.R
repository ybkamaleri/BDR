## Sammenlikne hba1c sentral og lokal

hbaAker <- "lab_HbA1cAkerVerdi"
hbaLokal <- "lab_HbA1c"

hbaPlot <- ggplot(dt1, aes(get(hbaLokal), get(hbaAker)), stat = 'identity', position = 'identity') +
  geom_point(aes(color = hosKort,
    text = paste(
      ' Sykehus: ', hosKort, '</br>',
      '</br> Lokalt: ', get(hbaLokal), '</br>',
      'Sentral: ', get(hbaAker)
      ))) +
  scale_y_continuous(name = "HbA1c målt sentral (Aker)", limits = c(5, 15)) +
  scale_x_continuous(name = "HbA1c målt lokalt", limits = c(5, 15)) +
  geom_smooth(method = 'lm', color = "red") +
  scale_color_viridis_d() + theme_classic() +
  theme(legend.position = "none")

## ggplot(dt1, aes({{ hbaAker }}, {{ hbaLokal}}), position = 'identity') +
##   geom_point()

hbaInt <- plotly::ggplotly(hbaPlot, tooltip = c("text"))
