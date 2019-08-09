## Analyser for indikatorer i resultat portalen
rm(list = ls())

inspak <- function(pkg){
  nypkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(nypkg)) install.packages(nypkg, repos = "http://cran.rstudio.com")
  sapply(pkg, require, character.only = TRUE)
}

pkgs = c("data.table", "stringi", "lubridate", "sqldf","huxtable", "dplyr","rio")

inspak(pkgs)

## hente resh id som brukes i portalen
resh <- import("sykehus_resh_id.xlsx")
