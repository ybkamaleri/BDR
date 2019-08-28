## Insulindose per aldersgrupper

lokalDT <- dt1

## Mltiinjeksjon
## -------------
mulVar <- c("beh_ins_beh_hurtig_ie_dogn", "beh_ins_beh_lang_ie_dogn")

## Antall som bruker dvs. har svart minst en av dem i 'mulVar'
lokalDT[, multN := NA_integer_]
lokalDT[!is.na(get(mulVar[1])), multN := 1L]
lokalDT[!is.na(get(mulVar[2])), multN := 1L]


## Totalt av mulVar
lokalDT[!is.na(get(mulVar[1])) & !is.na(get(mulVar[2])),
  multot := rowSums(.SD, na.rm = TRUE), .SDcols = mulVar,
  by = .(PasientID)]
## Hvis en av dem er NA så ikke telle
lokalDT[is.na(get(mulVar[1])) | is.na(get(mulVar[2])), multot := NA]
## Total multiinjeksjon del med kroppsvekt
lokalDT[!is.na(multot) & !is.na(inn_Vekt), mulkg := multot / inn_Vekt, by = .(PasientID)]
lokalDT[is.na(multot) & is.na(inn_Vekt), mulkg  := NA_real_, by = .(PasientID)]

## 1 til multiinjeksjon og 2 til insulinpumpe (se videre nede)
lokalDT[!is.na(mulkg), insbeh := 1L]



## Insulinpumpe
## ------------
insVar <- c("beh_ins_type_ie_basal", "beh_ins_type_ie_bolus")

## Antall som bruker dvs. har svart minst en av dem i 'insVar'
lokalDT[, insN := NA_integer_]
lokalDT[!is.na(get(insVar[1])), insN := 1L]
lokalDT[!is.na(get(insVar[2])), insN := 1L]


## Totalt av insVar
lokalDT[!is.na(insVar[1]) & !is.na(insVar[2]),
  instot := rowSums(.SD, na.rm = TRUE), .SDcols = insVar,
  by = .(PasientID)]
## Hvis en av dem er NA så teller ikke
lokalDT[is.na(get(insVar[1])) | is.na(insVar[2]), instot := NA]
## Total multiinjeksjon del med kroppsvekt
lokalDT[!is.na(instot) & !is.na(inn_Vekt), inskg := instot / inn_Vekt, by = .(PasientID)]
lokalDT[is.na(instot) & is.na(inn_Vekt), inskg := NA_real_, by = .(PasientID)]

## 2 til insulinpumpe
lokalDT[!is.na(inskg), insbeh := 2L]

## Hvis har svar på begge beholder bare en behandling dvs. insulinpumpe
lokalDT[!is.na(mulkg) & !is.na(inskg), mulkg := NA_real_]


## Aggrigerer tallene
## ----------------------
## Mulitinjuksjon
tabMult <- cube(lokalDT[insbeh == 1, ],
  j = list(
    nmult = sum(!is.na(mulkg)),
    mmult = round(mean(mulkg, na.rm = TRUE), digits = 2)
  ),
  by = c("hosKort", "agekat")
)

tabMult[, mmult := sprintf("%0.2f", mmult)] #beholder 2 digits
longMult <- dcast(tabMult, hosKort ~ agekat, value.var = "mmult")
setorder(longMult, hosKort, na.last = TRUE)
longMult[is.na(hosKort), hosKort := "Hele landet"]
## gir nytt colnavn
oldNavn <- names(longMult)[-1]
setnames(longMult, oldNavn, paste0("v", names(longMult)[-1]))

colNvn <- names(longMult)
longMult[, (colNvn) := lapply(.SD, as.character), .SDcols = colNvn]

for (j in seq_len(ncol(longMult))){
  set(longMult, which(is.na(longMult[[j]])), j = j, value = " -")
}

setcolorder(longMult, c("hosKort", "v1", "v2", "v3", "v4", "vNA"))
nyNavn <- c("", "<5 år", "5-9 år", "10-14 år", ">14 år", "HF")
setnames(longMult, names(longMult), nyNavn)

tabOutMult <- exp.tabel(longMult, "Mulitinjuksjon (penn): gjennomsnitt",
  ncol = 6, size = 0.9, total = 2, rowHeight = .015, mixCol = 2:6)

## quick_pdf(tabOutMult, file = "test.pdf")



## Insulinpumpe
tabIns <- cube(lokalDT[insbeh == 2, ],
  j = list(
    ninsu = sum(!is.na(inskg)),
    minsu = round(mean(inskg, na.rm = TRUE), digits = 2)),
  by = c("hosKort", "agekat")
)

tabIns[, mmult := sprintf("%0.2f", minsu)] #beholder 2 digits
longIns <- dcast(tabIns, hosKort ~ agekat, value.var = "minsu")
setorder(longIns, hosKort, na.last = TRUE)
longIns[is.na(hosKort), hosKort := "Hele landet"]
## gir nytt colnavn
oldNavn <- names(longIns)[-1]
setnames(longIns, oldNavn, paste0("v", names(longIns)[-1]))


colNvn <- names(longIns)
longIns[, (colNvn) := lapply(.SD, as.character), .SDcols = colNvn]

for (j in seq_len(ncol(longIns))){
  set(longIns, which(is.na(longIns[[j]])), j = j, value = " -")
}

setcolorder(longIns, c("hosKort", "v1", "v2", "v3", "v4", "vNA"))
nyNavn <- c("", "<5 år", "5-9 år", "10-14 år", ">14 år", "HF")
setnames(longIns, names(longIns), nyNavn)

tabOutIns <- exp.tabel(longIns, "Insulinpumpe: gjennomsnitt",
  ncol = 6, size = 0.9, total = 2, rowHeight = .015, mixCol = 2:6)

## quick_pdf(tabOutIns, file = "test.pdf")
