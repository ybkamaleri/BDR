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
