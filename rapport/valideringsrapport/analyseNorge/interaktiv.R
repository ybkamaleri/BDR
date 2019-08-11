library(pier)

data <- data.frame(label = c('Environment','Education','Business','Community'),
                   value = c(104,119,638,1250),
                   color = RColorBrewer::brewer.pal(4, 'Spectral'))

simple.pie <- pier(data)

# Advance pie
advanced.pie <- data %>%
    pier() %>%
    pie.size(inner=70, outer=100, width = 600, height = 450) %>%
    pie.header(text='Segments', font='Impact', location='pie-center') %>%
    pie.subtitle(text='by Type') %>%
    pie.footer(text='Economic Segments using fake data.',
               location = 'bottom-left') %>%
    pie.tooltips()
