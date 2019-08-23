## Konverterer R script til html fil

library(knitr)

## Some examples
## htmlFILE
spin("type_diabetes.R", knit = FALSE, format = "Rhtml")
## mdFILE
spin("type_diabetes.R", knit = FALSE, format = "Rmd")



setwd("~/Git-work/bdr/rapport/valideringsrapport/analyse")
rfiles <- list.files()[3:5]

lapply(rfiles, function(x) knitr::spin(x, knit = FALSE))

rmdfiles <- list.files(pattern = ".Rmd")
lapply(rmdfiles, function(x) rmarkdown::render(x, output_format = "html_document"))
lapply(rmdfiles, function(x) knitr::knit2html(x))

## move files
library(filesstrings)
lapply(list.files(pattern = ".Rmd"), function(x) file.move(x, "./html/", overwrite = TRUE))




library('rmarkdown')
rmarkdown::render('type_diabetes.R')

lapply(rfiles, function(x) rmarkdown::render(x, output_format = "html_document"))
