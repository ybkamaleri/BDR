## Run enkel rapport
## -----------------
rm(list = ls())

inspak <- function(pkg){
  nypkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(nypkg)) install.packages(nypkg, repos = "http://cran.rstudio.com")
  sapply(pkg, require, character.only = TRUE)
}

pkgs = c("data.table", "stringi", "validate", "ggplot2", "lubridate", "readxl", "sqldf","huxtable", "dplyr", "kableExtra", "bookdown", "rreg", "colorspace")

inspak(pkgs)

# ## kableExtra older version for R3.5.0
# kext_url <- "https://cran.r-project.org/src/contrib/Archive/kableExtra/kableExtra_0.7.0.tar.gz"
# # kext_url <- "https://cran.r-project.org/src/contrib/Archive/R.methodsS3/R.methodsS3_1.7.0.tar.gz"
# install.packages(kext_url, repos = NULL, type = "source", dependencies = TRUE)
# 

## Setup
## -------
# source(file.path(kilde, "setup.R", fsep = "\\"))
## Hente data for årskontroll 2018
## --------------------------------

## ## RStudio
## # dt2018 <- readRDS(file.path("../.."), "validert_arskontroll2018.rds")
## dataSti <- "K:\\Sensitivt\\Forskning02\\Barnediabetesregisteret_2015-5739\\Barnediabetes og kvalitet\\Datafiler\\2018"
## ars2018 <- readRDS(file.path(dataSti, "validert_arskontroll2018.rds", fsep = "\\"))


## EMACS
dataSti <- "~/avid/bdr"
ars2018raw <- readRDS(file.path(dataSti, "validert_arskontroll2018_yb.rds"))

## ## Lage alder kategori med 5 interval
## source("./analyse/ageCat.R")
## ars2018raw[!is.na(alder), agecat := ageCat(alder, 0, 15, by = 5)]
## gpAge <- unique(as.character(ars2018raw$agecat))
## ## agecat er for factor mens agekat er contineous
## ars2018raw[.(agecat = gpAge, to = 4:1), on = "agecat", agekat := i.to]
##
## saveRDS(ars2018raw, file.path(dataSti, "validert_arskontroll2018_yb.rds"))

## hospital koder
hospKoder <- unique(ars2018raw$hospID)

## Blodtrykk kobling
bpp <- readRDS(file.path(dataSti,"bloodpressure.RDS"))
bpSub <- subset(bpp, select = c("id", "stage"))
ars2018 <- merge(ars2018raw, bpSub, by.x = "Pnr", by.y = "id", all.x = TRUE)

## data type1 fra 2007 - 2017
bdrB4 <- readRDS(file.path(dataSti, "allBDRtype1.rds"))

## output directory
outDir <- "output"
if (!dir.exists(outDir)) {dir.create(outDir)} else {print("Output dir finnes allerede")}

## Farge valg
## hcl_palettes(plot = TRUE) #får å se mulige fargevalg pallettes
valgCol <- sequential_hcl(4, "Blues 3")


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

## ## For testing
## hospKoder = c(8, 14, 22)

## Dato
valgDato <- Sys.Date()


## Kjør rapporten som PDF
## ----------------------

for (hosp in hospKoder) {

  hospTitle <- ars2018[hospID == hosp, .(hospital)][[1]][1]
  pdfTitle <- ars2018[hospID == hosp, .(hosKort)][[1]][1]

  ## file delete if exists pga. error fra førrige kompilering
  filehosp <- paste0(pdfTitle, ".Rmd")
  if (file.exists(filehosp)) file.remove(filehosp)

  ## lokal
  lokal2018 <- subset(ars2018, hospID == hosp)

  ## lokal diabetes Type 1
  lok2018dt1 <- subset(ars2018, hospID == hosp &  diabetes_Type1 == "Ja")

  ## _bookdown.yml
  filBKD <- "_bookdown.yml"
  if (file.exists(filBKD)) file.remove(filBKD)
  bookFN <- paste0("book_filename: '", pdfTitle, ".Rmd'")
  outDir <- paste0("output_dir: './output/", pdfTitle, "'")
  write(bookFN, file = filBKD, append = TRUE)
  write(outDir, file = filBKD, append = TRUE)

  bookdown::render_book(input = "index.Rmd",
    output_format = "bookdown::pdf_document2",
    params = list(
      nyTitle = hospTitle,
      nyDate =  format(valgDato, '%d %B %Y'),
      nySubTitle = "Årskontroller i BDR for 2018 data"
    )
  )

  file.remove(filBKD)

}



## Kjør rapporten som HTML
## ------------------------
for (hosp in hospKoder) {

  hospTitle <- ars2018[hospID == hosp, .(hospital)][[1]][1]
  pdfTitle <- ars2018[hospID == hosp, .(hosKort)][[1]][1]

  ## file delete if exists pga. error fra førrige kompilering
  filehosp <- paste0(pdfTitle, ".Rmd")
  if (file.exists(filehosp)) file.remove(filehosp)

  ## lokal
  lokal2018 <- subset(ars2018, hospID == hosp)

  ## lokal diabetes Type 1
  lok2018dt1 <- subset(ars2018, hospID == hosp &  diabetes_Type1 == "Ja")
  
  ## _bookdown.yml
  filBKD <- "_bookdown.yml"
  if (file.exists(filBKD)) file.remove(filBKD)
  bookFN <- paste0("book_filename: '", pdfTitle, ".Rmd'")
  outDir <- paste0("output_dir: './output/", pdfTitle, "'")
  write(bookFN, file = filBKD, append = TRUE)
  write(outDir, file = filBKD, append = TRUE)

  bookdown::render_book(input = "index.Rmd",
    output_format = "bookdown::gitbook",
    params = list(
      nyTitle = hospTitle,
      nyDate =  format(valgDato, '%d %B %Y'),
      nySubTitle = "Årskontroller i BDR for 2018 data"
    )
  )

  file.remove(filBKD)

}
