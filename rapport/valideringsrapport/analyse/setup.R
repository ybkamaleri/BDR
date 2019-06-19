## Pakker
pkgs = c("data.table", "rreg", "stringi", "validate", "ggplot2", "lubridate", "readxl", "sqldf")
## sapply(pkgs, library, character.only = TRUE)[,1]

pkgIn <- function(x){
  if (!sapply(x, require, character.only = TRUE)) {
    sapply(x, install.packages, character.only = TRUE)
  }
  invisible(sapply(x, library, character.only = TRUE))
}

for (x in pkgs) pkgIn(x)

## if (!require("data.table")) install.packages("data.table"); library(data.table)

setwd("~/avid/bdr")

## Hente data for årskontroll 2018
## --------------------------------
## dt2018 <- readRDS(file.path("../.."), "validert_arskontroll2018.rds")
## ars2018 <- readRDS("validert_arskontroll2018.rds")
ars2018 <- readRDS("annonym_dt2018.RDS")

## Bare pasienter med DT1
## -----------------------
dt1 <- subset(ars2018, diabetes_Type1 == "Ja")

## Spørsmål
## ----------
## Hva med ikke Samtykke pasienter??

samtykke <- ars2018[, .N, by=.(Samtykke_Pasient)]
