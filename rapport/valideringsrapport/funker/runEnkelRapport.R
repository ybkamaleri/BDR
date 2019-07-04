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


# ## Test to create file
# hosp1 <- "Aker"
# bookFN <- paste0("book_filename:", ' "test.Rmd"')
# outDir <- paste0("output_dir:", ' "book-out"')
# write(bookFN, file = "_test.yml", append = TRUE)
# write(outDir, file = "_test.yml", append = TRUE)
# # with writeLines
# testFil <- file("test2.yml", "w")
# writeLines("book_filename: 'test.Rmd'", con = testFil, sep = "\n")
# writeLines("output_dir: 'book-out'", con = testFil)
# close(testFil)
# 



## Kjør rapporten som HTML
## ------------------------
for (hosp in hospKoder[5:6]) {

  hospTitle <- ars2018[hospID == hosp, .(hospital)][[1]][1]
  pdfTitle <- ars2018[hospID == hosp, .(hosKort)][[1]][1]
 
  ## lokal
  lokal2018 <- subset(ars2018, hospID == hosp)
  
  ## _bookdown.yml
  filBKD <- "_bookdown.yml"
  if (file.exists(filBKD)) file.remove(filBKD)
  bookFN <- paste0("book_filename: '", pdfTitle, ".Rmd'")
  outDir <- paste0("output_dir: '", pdfTitle, "_html'")
  write(bookFN, file = filBKD, append = TRUE)
  write(outDir, file = filBKD, append = TRUE)
  
  bookdown::render_book(input = "index.Rmd",
                        output_format = "bookdown::gitbook",
                        params = list(
                          nyTitle = hospTitle,
                          nyDate =  format(Sys.Date(), '%d %B %Y'),
                          nySubTitle = "Årskontroller i BDR for 2018 data"
                          )
                        )
  }



## Kjør rapporten som PDF
## ----------------------
for (hosp in hospKoder[5:6]) {
  
  hospTitle <- ars2018[hospID == hosp, .(hospital)][[1]][1]
  pdfTitle <- ars2018[hospID == hosp, .(hosKort)][[1]][1]
  
  ## lokal
  lokal2018 <- subset(ars2018, hospID == hosp)
  
  ## _bookdown.yml
  filBKD <- "_bookdown.yml"
  if (file.exists(filBKD)) file.remove(filBKD)
  bookFN <- paste0("book_filename: '", pdfTitle, ".Rmd'")
  outDir <- paste0("output_dir: '", pdfTitle, "_pdf'")
  write(bookFN, file = filBKD, append = TRUE)
  write(outDir, file = filBKD, append = TRUE)
  
  bookdown::render_book(input = "index.Rmd",
                        output_format = "bookdown::pdf_book",
                        params = list(
                          nyTitle = hospTitle,
                          nyDate =  format(Sys.Date(), '%d %B %Y'),
                          nySubTitle = "Årskontroller i BDR for 2018 data"
                          )
                        )
  }

