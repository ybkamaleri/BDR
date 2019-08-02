## Behandling av diabetes

lokalDT <- lok2018dt1

## Mltiinjeksjon
mulVar <- c("beh_ins_beh_hurtig_ie_dogn", "beh_ins_beh_lang_ie_dogn")
## Totalt av mulVar
lokalDT[, multot := rowSums(.SD, na.rm = TRUE), .SDcols = mulVar]
## Total multiinjeksjon del med kroppsvekt
lokalDT[, mulkg := multot / inn_Vekt, by = .(PasientID)]


## Insulinpumpe
insVar <- c("beh_ins_type_ie_basal", "beh_ins_type_ie_bolus")
## Totalt av insVar
lokalDT[, instot := rowSums(.SD, na.rm = TRUE), .SDcols = insVar]
## Total multiinjeksjon del med kroppsvekt
lokalDT[, inskg := instot / inn_Vekt, by = .(PasientID)]


## rollup(lokalDT,
##   .(mmult = mean(mulkg, na.rm = TRUE),
##     minsu = mean(inskg, na.rm = TRUE)),
##   by = c("agecat", "agekat"))

## Aggrigerer tallene
innTabel <- groupingsets(
   lokalDT,
  j = list(
    nmult = sum(!is.na(mulkg)),
    mmult = round(mean(mulkg, na.rm = TRUE), digits = 2),
    ninsu = sum(!is.na(inskg)),
    minsu = round(mean(inskg, na.rm = TRUE), digits = 2)),
  by = c("agecat", "agekat"),
  sets = list(
    c("agecat", "agekat"),
    character(0)
  ))


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
  set(innTabel, which(is.na(innTabel[[j]])), j = j, value = "-")
  }

## ny kolonnenavn
nyNavn <- c("Alder", "n", "gj.snitt", "n", "gj.snitt")
setnames(innTabel, names(innTabel), nyNavn)

## For å beholder all 2 decimaler selv om det er 0


## Tabell
outTab <- tabHux(innTabel, size = 0.8, total = TRUE, del = c(.2, .15, .2, .15, .2))

## lage over titel
bottom_border(outTab)[1, ] <- FALSE
outTab <- rbind(c("", "Multiinjeksjon", "", "Insulinpumpe", ""), outTab)

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
