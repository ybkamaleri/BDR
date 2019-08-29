## Nyoppdaget rapport
rm(list = ls())

## pakker
pkg <- c("data.table", "ggplot2", "rreg", "huxtable", "bookdown", "knitr", "rmarkdown", "colorspace", "pier", "stringi", "plotly")
lapply(pkg, library, character.only = TRUE)[[1]]

## Data path
dataSti <- "~/avid/bdr"
## Load all data
DT <- readRDS(file.path(dataSti, "nyoppdatet2018alle.rds"))

dim(DT)

## ## Annonym pasienter fra tilbakemelding fra sykehus
## DT[FNavn == 'Annonym', annonym := 1]
## DT[Pnr == 11070897565, annonym := 1]

DT[, .N, by = annonym]


## Alder
library("lubridate")
DT[, alder := as.numeric(round(as.period(interval(FDato, inn_DiagDato)/duration(n=1, unit="years")), digits = 1))]


## Figur Antall
library(ggplot2)

nDT <- DT[, .N, by = .(hosKort)]
nDT

library(rreg)
(figAntall <- regbar(nDT[!is.na(hosKort)], hosKort, N, ylab = "Antall pasienter"))
ggsave("figAntall.png")


## Figure alderfordeling

dim(dt)
diabDT[, age := round(alder, digits = 0)]
ageDT <- diabDT[,.N, by = age]
diabDT[age == 1, .(Pnr, hosKort, inn_DiagDato)]
diabDT[age == 2, .(Pnr, hosKort, inn_DiagDato)]

regbar(ageDT, age, N, flip = FALSE)


## Alder i grupperinger og antall

## Lager kategorisering for alder
ageCat <- function(x, lower, upper, by, sep = "-") {
  ## Finne høyeste kategori
  kat <- paste0(seq(lower + by - 1, upper - 1, by = by))
  indTop <- max(length(kat))
  top <- as.numeric(kat[indTop])

  labs <- paste0(c(paste(seq(lower, upper - by, by = by),
    seq(lower + by - 1, upper - 1, by = by),
    sep = sep),
    paste(top + 1, "+", sep = "")))
  cut(floor(x), breaks = c(seq(lower, upper, by = by), Inf),
    include.lowest = TRUE, right = FALSE, labels = labs)
}

dt[, agegp := ageCat(alder, 0, 15, 5), by = Pnr]
rollup(dt, j = .(n = .N), by = "agegp")


## HbA1c
dim(dt)
dt1 <- dt[is.na(annonym) & diabetes_Type1 == "Ja", ]
dim(dt1)
hba <- "lab_HbA1c_Sentralt"
hba2 = "lab_HbA1c"
dt[, hba1c := get(hba)][is.na(hba1c), hba1c := get(hba2)]
dt1[is.na(hba1c), .N]
dt1[is.na(get(hba)), .N]

dt[hba1c > 18, .N]
dt1[hba1c > 30, .(Pnr, hosKort, hba1c)]



rollup(dt1,
  j = .(
    mhb = mean(hba1c, na.rm = T),
    n = .N,
    min = min(hba1c, na.rm = T),
    max = max(hba1c, na.rm = T),
    nmiss = sum(is.na(hba1c))
  ), by = "agegp" )


dim(dt)
dt[Pnr == 27060595769, hba1c := 13.6]
dt[Pnr == 2030884615 , hba1c := 11.7]
dt[Pnr == 27080595668 , hba1c := 16.8]
dt[Pnr == 30110595936, hba1c := 15.9]
dt[Pnr == 1041080473, hba1c := 12.1]
dt[Pnr == 24050983176, hba1c := 11.1]
dt[Pnr == 9011482128, hba1c := 7.9]
dt[Pnr == 30061399522, hba1c := 12.3]
dt[Pnr == 29041099621, hba1c := 14.2]
dt[Pnr == 8111180254, hba1c := 10.9]
