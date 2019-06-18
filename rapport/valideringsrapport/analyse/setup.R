## Pakker
pkgs = c("data.table", "stringi", "validate", "ggplot2", "lubridate", "readxl", "sqldf")
sapply(pkgs, library, character.only = TRUE)[,1]

## Hente data for årskontroll 2018
## --------------------------------
# dt2018 <- readRDS(file.path("../.."), "validert_arskontroll2018.rds")
ars2018 <- readRDS("validert_arskontroll2018.rds")

## Bare pasienter med DT1
## -----------------------
dt1 <- subset(ars2018, diabetes_Type1 == "Ja")

## Spørsmål
## ----------
## Hva med ikke Samtykke pasienter??

samtykke <- ars2018[, .N, by=.(Samtykke_Pasient)]
