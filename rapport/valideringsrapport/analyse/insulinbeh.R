## Insulin behandling

## lokalDT <- copy(ars2018)
lokalDT <- lok2018dt1


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


## Antall og Andel for multiinjeksjon og
## insulinpumpe som svarte minst en av målingene
## ---------------------------------------------
multN <- lokalDT[, sum(multN, na.rm = TRUE)]
insN <- lokalDT[, sum(insN, na.rm = TRUE)]
behNtot <- multN + insN
multPros <- round(multN / behNtot * 100, digits = 1)
insPros <- round(insN / behNtot * 100, digits = 1)



## rollup(lokalDT,
##   .(mmult = mean(mulkg, na.rm = TRUE),
##     minsu = mean(inskg, na.rm = TRUE)),
##   by = c("agecat", "agekat"))

## Aggrigerer tallene
innTabelRaw <- groupingsets(
   lokalDT,
  j = list(
    nmult = sum(!is.na(mulkg)),
    mmult = round(mean(mulkg, na.rm = TRUE), digits = 2),
    ninsu = sum(!is.na(inskg)),
    minsu = round(mean(inskg, na.rm = TRUE), digits = 2)),
  by = c("agekat", "insbeh"),
  sets = list(
    c("agekat", "insbeh"),
    character(0)
  ))


setkey(innTabelRaw, insbeh, agekat)


## Multiinjeksjon Tabell
mulTabVar <- c("nmult", "mmult")
multTabel <- innTabelRaw[insbeh == 1, .SD, keyby = .(insbeh, agekat), .SDcols = mulTabVar]
## Insulinpumpe Tabell
insTabVar <- c("ninsu", "minsu")
insTabel <- innTabelRaw[insbeh == 2, .SD, keyby = .(insbeh, agekat), .SDcols = insTabVar]
## insulin behandling tabell
behTabRaw <- merge(insTabel, multTabel, by = "agekat", all = TRUE)


## Dummy agekat  tabell
dummyAge <- data.table(agekat = 1:4, agecat = c("0-4", "5-9", "10-14", "15+"))

## Final Tabell
behTabRaw <- merge(dummyAge, behTabRaw, by = "agekat", all.x = TRUE)

## Totalt beh
behTotal  <- innTabelRaw[is.na(agekat), ]

## Alle Tabell
innTabel <- rbindlist(list(behTabRaw, behTotal), use.names = TRUE, fill = TRUE)
innTabel[, c("insbeh.x", "insbeh.y", "insbeh") := NULL]


## Tabell
innTabel[is.na(agecat), agecat := "Alle"]
innTabel[is.na(agekat), agekat := nrow(innTabel) * 2] #sikre at totalt ligger nedest når rader er sortert
setorder(innTabel, agekat) #sorterer alder kategorier
innTabel[, agekat := NULL]

## Erstarter NA i antall til 0 hvis finnes
for (j in c(2L, 4L)){
  set(innTabel, which(is.na(innTabel[[j]])), j = j, value = 0)
  }

## For å beholder all 2 decimaler selv om det er 0
innTabel[, mmult := sprintf("%0.2f", mmult)]
innTabel[, minsu := sprintf("%0.2f", minsu)]

## Erstarter NA i mean til "-" hvis finnes
for (j in c(3L, 5L)){
  set(innTabel, which(innTabel[[j]] == "NA"), j = j, value = "-")
}

## ## gir strek hvis missing
## behVar <- c(mulTabVar, insTabVar)
## innTabel[, (behVar) := lapply(.SD, as.character), .SDcol=behVar]
## for (j in behVar){
##   set(innTabel, which(is.na(innTabel[[j]])), j = j, value = "-")
## }


## ny kolonnenavn
nyNavn <- c("Alder", "n", "gj.snitt", "n", "gj.snitt")
setnames(innTabel, names(innTabel), nyNavn)

## For å beholder all 2 decimaler selv om det er 0


## Tabell
outTab <- tabHux(innTabel, size = 0.8, total = TRUE, del = c(.2, .15, .2, .15, .2))

## lage over titel
bottom_border(outTab)[1, ] <- FALSE
outTab <- rbind(c("", "Insulinpumpe", "", "Multiinjeksjon", ""), outTab)

outTab <- outTab %>%
  set_bottom_border(2,, TRUE) %>%
  set_bottom_border(1, 2:5, 0.5) %>%
  set_top_border(1,, TRUE) %>%
  set_right_border(, 3, 0.4) %>%
  set_align(2:nrow(outTab), c(3, 5), "center")

outTab <- merge_cells(outTab, 1, c(2:3))
outTab <- merge_cells(outTab, 1, c(4:5))

wrap(outTab) <- TRUE

## outTab <- merge_cells(outTab, 1, c(2:3, 4:5))


## ## --- Sjekk var----- ##
## lokalDT[is.na(inskg),
##   c("Pnr", insVar, "inn_Vekt", "instot", "inskg", "agecat"), with = F]
