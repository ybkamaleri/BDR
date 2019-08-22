## Konverterer R script til html fil

library(knitr)

## Some examples
## htmlFILE
spin("type_diabetes.R", knit = FALSE, format = "Rhtml")
## mdFILE
spin("type_diabetes.R", knit = FALSE, format = "Rmd")



setwd("~/Git-work/bdr/rapport/valideringsrapport/analyse")
rfiles <- list.files()[3:5]

sapply(rfiles, function(x) knitr::spin(x, knit = FALSE, precious = TRUE), USE.NAMES = TRUE)





library('rmarkdown')
rmarkdown::render('type_diabetes.R')

sapply(rfiles, function(x) rmarkdown::render(x, output_format = "html_document"))
