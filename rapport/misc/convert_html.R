
library(knitr)

rfiles <- list.files(paste0(getwd(), "/analyseBK/"), pattern = ".R$")
setwd("/home/bdr/Git-work/bdr/rapport/valideringsrapport/analyseBK")
sapply(rfiles, function(x) knitr::spin(x), USE.NAMES = TRUE)
