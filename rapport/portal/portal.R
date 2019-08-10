## Analyser for indikatorer i resultat portalen
rm(list = ls())

inspak <- function(pkg){
  nypkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(nypkg)) install.packages(nypkg, repos = "http://cran.rstudio.com")
  sapply(pkg, require, character.only = TRUE)
}

pkgs = c("data.table", "openxlsx", "dplyr","rio")

inspak(pkgs)

## hente resh id som brukes i portalen
resh <- import("sykehus_resh_id.xlsx")

## Hent data fra filen 'emacs_path.R'
source("~/avid/bdr/emacs_path.R")
## Merge resh koder til datasettet
DT <- merge(ars2018raw, resh, by.x = "hospID", by.y = "kode", all.x = TRUE)

dt <- subset(DT, !is.na(resh))

## KiB - Indikator B
## Antall målt HbA1c
## dt[!is.na(lab_HbA1cAkerVerdi), .N, by = .(resh)]
hba1c <- "lab_HbA1cAkerVerdi"

KiB <- dt[,
  list(
    n = sum(!is.na(get(hba1c))),
    N = length(get(hba1c))),
  by = .(resh)]


## KiC - HbA1c < 7.0
## -----------------
KiC <- dt[,
  .(n = length(which(get(hba1c) < 7)),
    N = sum(!is.na(get(hba1c)))),
  by = .(resh)]


## KiD - HbA1c < 7.5
## -----------------
KiD <- dt[,
  .(n = length(which(get(hba1c) < 7.5)),
    N = sum(!is.na(get(hba1c)))),
  by = .(resh)]


## KiE - HbA1c >= 9
## -----------------
KiE <- dt[,
  .(n = length(which(get(hba1c) >= 9)),
    N = sum(!is.na(get(hba1c)))),
  by = .(resh)]


## KiF - Tatt urinprøve iht. ISPAD
## ------------------------------
## ISPD guidelines
dt[, ispad := 0] %>%
  .[alder >= 10 & diagVar >= 2, ispad  := 1] %>%
  .[alder < 10 & diagVar >= 5, ispad := 1]

KiF <- dt[ispad == 1,
  list(
    n = sum(!is.na(lab_res_1prove)),
    N = .N
  ),
  by = resh] #utført


## KiG - øye undersøkelse
## ----------------------
KiG <- dt[ispad == 1,
  list(
    n = length(which(und_Oye == 'Ja')),
    N = .N),
  by = resh]


## Målt lipider
## -------------
## Total kolesterol. Bruk ikke fastende, hvis missing bruk fastende
dt[, tkl2 := lab_lip_totkol_2] %>%
  .[is.na(lab_lip_totkol_2), tkl2  := lab_lip_totkol]

KiH <- dt[, list(
  n = sum(!is.na(tkl2)),
  N = length(tkl2)),
  by = resh]


## Målt bloodtrykk
## ----------------
## begge Systolisk og diastolisk må blir besvart
dt[!is.na(inn_Blodtrykk_s) & !is.na(inn_Blodtrykk_d),
  blood := 1L, by = PasientID]

KiI <- dt[, list(
  n = sum(blood, na.rm = TRUE),
  N = length(blood)),
  by = resh]


## Infiltrater
## ----------------
dt[und_infiltrater  %in% c("Ja", "Nei"), infil := 1L, by = PasientID]

KiJ <- dt[, list(
  n = sum(infil, na.rm = TRUE),
  N = length(infil)),
  by = resh]

## TSH
## -------------
dt[und_syk_hypo %in% c("Ja", "Nei"), tsh := 1L, by = PasientID]

KiK <- dt[, list(
  n = sum(tsh, na.rm = TRUE),
  N = length(tsh)),
  by = resh]


## Cøliakiantistoffer
## -------------------
dt[und_syk_col %in% c("Ja", "Nei"), coliaki := 1L, by = PasientID]

KiL <- dt[, list(
  n = sum(coliaki, na.rm = TRUE),
  N = length(coliaki)),
  by = resh]


## ikke er innlagt med DKA
## ------------------------
KiM <- dt[, list(
  n = length(which(und_ketoacidose == "Nei")),
  N = length(which(und_ketoacidose  %in% c("Ja", "Nei")))),
  by = resh]

## Ikke har hatt insulinsjokk
##---------------------------
KiN <- dt[, list(
  n = length(which(und_inssjokk == "Nei")),
  N = length(which(und_inssjokk %in% c("Ja", "Nei")))),
  by = resh]


## Convert til excel
indK <- grep("^Ki", ls(), value = TRUE)
pathXL <- "~/Git-work/bdr/rapport/portal/excel/"
sapply(indK, function(x) write.xlsx(get(x), paste0(pathXL, x, "_", format(Sys.Date(), "%Y%m%d"), ".xlsx")), USE.NAMES = TRUE)

## library(openxlsx)
## write.xlsx(KiB, paste0(pathXL,"KiB.xlsx"))
## write.xlsx(KiC, paste0(pathXL,"KiC.xlsx"))
## write.xlsx(KiD, paste0(pathXL,"KiD.xlsx"))
## write.xlsx(KiE, paste0(pathXL,"KiE.xlsx"))
## write.xlsx(KiF, paste0(pathXL,"KiF.xlsx"))
## write.xlsx(KiG, paste0(pathXL,"KiG.xlsx"))
## write.xlsx(KiH, paste0(pathXL,"KiH.xlsx"))
## write.xlsx(KiI, paste0(pathXL,"KiI.xlsx"))
## write.xlsx(KiJ, paste0(pathXL,"KiJ.xlsx"))
## write.xlsx(KiK, paste0(pathXL,"KiK.xlsx"))
## write.xlsx(KiL, paste0(pathXL,"KiL.xlsx"))
## write.xlsx(KiM, paste0(pathXL,"KiM.xlsx"))
## write.xlsx(KiN, paste0(pathXL,"KiN.xlsx"))
