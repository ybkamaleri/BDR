## Run enkel rapport
## -----------------

rm(list = ls())

inspak <- function(pkg){
  nypkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(nypkg))
    install.packages(nypkg, dependencies = TRUE, repos = "http://cran.rstudio.com")
  sapply(pkg, require, character.only = TRUE)
}

pkgs = c("data.table", "stringi", "validate", "ggplot2", "lubridate", "readxl", "sqldf")

inspak(pkgs)

## kilde
kilde <- "K:\\Sensitivt\\Forskning02\\Barnediabetesregisteret_2015-5739\\Barnediabetes og kvalitet\\Datafiler\\2018\\valideringsrapport\\rapport"

## Setup
## -------
# source(file.path(kilde, "setup.R", fsep = "\\"))
## Hente data for årskontroll 2018
## --------------------------------
# dt2018 <- readRDS(file.path("../.."), "validert_arskontroll2018.rds")
dataSti <- "K:\\Sensitivt\\Forskning02\\Barnediabetesregisteret_2015-5739\\Barnediabetes og kvalitet\\Datafiler\\2018"
ars2018 <- readRDS(file.path(dataSti, "validert_arskontroll2018.rds", fsep = "\\"))

## hospital koder
hospKoder <- unique(ars2018$hospID)


## Rapport fil - RMD
rmdFil <- paste(file.path(kilde, "index.Rmd", fsep = "\\"))


## Kjør rapporten
## ------------------
for (hosp in hospKoder) {
  
  hospTitle <- ars2018[hospID == hosp, .(hospital)][[1]][1]
  pdfTitle <- ars2018[hospID == hosp, .(hosKort)][[1]][1]
  
  bookdown::render_book(input = rmdFil,
                        output_format = "bookdown::gitbook",
                        params = list(
                          nyTitle = hospTitle,
                          nyDate =  format(Sys.Date(), '%d %B %Y'),
                          nySubTitle = "Årskontroller i BDR for 2018 data"
                        )
  )
}


bookdown::render_book(input = rmdFil,
                      output_format = bookdown::gitbook(lib_dir = "test-book"))
