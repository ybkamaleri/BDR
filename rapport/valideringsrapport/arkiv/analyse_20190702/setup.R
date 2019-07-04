## Pakker
inspak <- function(pkg){
  nypkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(nypkg))
    install.packages(nypkg, dependencies = TRUE, repos = "http://cran.rstudio.com")
  sapply(pkg, require, character.only = TRUE)
}

pkgs = c("data.table", "stringi", "validate", "ggplot2", "lubridate", "readxl", "sqldf")

inspak(pkgs)



## Hente data for årskontroll 2018
## --------------------------------
# dt2018 <- readRDS(file.path("../.."), "validert_arskontroll2018.rds")
dataSti <- "K:\\Sensitivt\\Forskning02\\Barnediabetesregisteret_2015-5739\\Barnediabetes og kvalitet\\Datafiler\\2018"
ars2018 <- readRDS(file.path(dataSti, "validert_arskontroll2018.rds", fsep = "\\"))



## Bare pasienter med DT1
## -----------------------
dt1 <- subset(ars2018, diabetes_Type1 == "Ja")

## Spørsmål
## ----------
## Hva med ikke Samtykke pasienter??

samtykke <- ars2018[, .N, by=.(Samtykke_Pasient)]
