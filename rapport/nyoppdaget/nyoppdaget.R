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
DT[, .N, by = agegp]


## Alder
## library("lubridate")
## DT[, alder := as.numeric(round(as.period(interval(FDato, inn_DiagDato)/duration(n=1, unit="years")), digits = 1))]

## Functions
source("~/Git-work/bdr/rapport/nyoppdaget/dbType.R")
## DTtype() for diabetestype
## ageCat() for alderfordeling

## Figur Antall
library(ggplot2)

nDT <- DT[, .N, by = .(hosKort)]
nDT

library(rreg)
(figAntall <- regbar(nDT[!is.na(hosKort)], hosKort, N, ylab = "Antall pasienter"))
ggsave("figAntall.jpg")


## Antall generell
##--------------------
diabDT <- DTtype(DT)
## diabDT[]

utDT <- dt[!diabDT, on = .(Pnr, hospID)] #De uten DT type
dim(utDT)
utDT[, c(demoVar, diabetesVar, "FNavn"), with = F]


hosp.diab <- groupingsets(diabDT,
  j = .(
    td1 = sum(dbtype == 1),
    td2 = sum(dbtype == 2),
    mody = sum(dbtype == 3),
    annen = sum(dbtype == 4)
  ),
  by = c("hosKort"),
  sets = list(c("hosKort"), character(0)))

hosp.dttot <- cube(diabDT, .(antall = sum(!is.na(dbtype))),
  by = c("hosKort"))

## Beregning
diabDT[, .N, by = diabType]


## -----------------------
## Alder
##-----------------------
library("lubridate")
## dt <- allnyDT
## dt[, alder := as.numeric(round(as.period(interval(FDato, inn_DiagDato)/duration(n=1, unit="years")), digits = 1))]

over15 <- subset(diabDT, alder >= 15)
dim(over15)
over15[, .N, by = diabType]

diabDT[dbtype == 1 & alder < 5, .N]


under15 <- subset(diabDT, alder < 15)
dim(under15)
under15[, .N, by = diabType]

diabDT[dbtype == 1 & alder < 5, .N]



## Figure alderfordeling
##------------------------
dim(DT)
DT[, age := round(alder, digits = 0)]
ageDT <- diabDT[,.N, by = age]
diabDT[age == 1, .(Pnr, hosKort, inn_DiagDato)]
diabDT[age == 2, .(Pnr, hosKort, inn_DiagDato)]

regbar(ageDT, age, N, flip = FALSE)


## Alder i grupperinger og antall
## DT[, agegp := ageCat(alder, 0, 15, 5), by = Pnr]
rollup(DT, j = .(n = .N), by = "agegp")



## KUN Type 1 diabetes
##----------------------
## HbA1c
dim(DT)
dt1 <- DT[is.na(annonym) & diabetes_Type1 == "Ja", ]
dim(dt1)
hba <- "lab_HbA1c_Sentralt"
hba2 = "lab_HbA1c"
dt1[, hba1c := get(hba)][is.na(hba1c), hba1c := get(hba2)]
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


## dim(DT)
## DT[Pnr == 27060595769, hba1c := 13.6]
## DT[Pnr == 2030884615 , hba1c := 11.7]
## DT[Pnr == 27080595668 , hba1c := 16.8]
## DT[Pnr == 30110595936, hba1c := 15.9]
## DT[Pnr == 1041080473, hba1c := 12.1]
## DT[Pnr == 24050983176, hba1c := 11.1]
## DT[Pnr == 9011482128, hba1c := 7.9]
## DT[Pnr == 30061399522, hba1c := 12.3]
## DT[Pnr == 29041099621, hba1c := 14.2]
## DT[Pnr == 8111180254, hba1c := 10.9]
