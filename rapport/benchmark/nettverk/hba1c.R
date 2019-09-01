## HbA1c og aldersgruppe
valgVar <- "lab_HbA1cAkerVerdi"

## dt1 <- DT

tabraw <- cube(dt1,
  j = .(
    nmis = sum(is.na(get(valgVar))),
    min = min(get(valgVar), na.rm = TRUE),
    max = max(get(valgVar), na.rm = TRUE),
    mean = round(mean(get(valgVar), na.rm = TRUE), digits = 1)
  ),
  by = c("agekat"))

tabraw
tabraw[is.na(agekat), agekat := 5]
## tabw <- dcast(tabraw, hosKort ~ agekat, value.var = "mean", )
setkey(tabraw, agekat)

## ## reorder row
## setorder(tabw, hosKort, na.last = TRUE)

## tabw[is.na(hosKort), hosKort := "Hele landet"]

## gir nytt colnavn
oldNavn <- names(tabraw)
## setnames(tabraw, oldNavn, paste0("v", names(tabw)[-1]))
setnames(tabraw, oldNavn, c("agegroup", "missing", "min", "max", "gjennomsnitt"))

ageKode <- dt1[, .N, by = .(agecat, agekat)][, N := NULL][]
setkey(ageKode, agekat)
ageUt <- ageKode[tabraw, on = c(agekat = "agegroup")]
ageUt[is.na(agecat), agecat := "Totalt"]
ageUt[, agekat := NULL]
ageUt

ageTab <- exp.tabel(ageUt, "Hba1c", ncol = 5, total = 1)
quick_pdf(ageTab, file = "hba1c_alder.pdf")

## ## beholder bare to digits inkludert 0
## tabw[, `:=`(
##   u1 = sprintf("%0.2f", v1),
##   u2 = sprintf("%0.2f", v2),
##   u3 = sprintf("%0.2f", v3),
##   u4 = sprintf("%0.2f", v4),
##   u5 = sprintf("%0.2f", v5)
## )]

## ## del ubrukte colnavn
## bortCol <- paste0("v", oldNavn)
## tabw[, (bortCol) := NULL]

## ## Bytt NA as.character til "-"
## for (j in seq_len(ncol(tabw))[-1]){
##   set(tabw, which(tabw[[j]] == 'NA'), j = j, value = "-")
## }


## ## Gir nytt navn til slutt tabell
## getCol <- names(tabw)
## nyNavn <- c("", "<5 år", "5-9 år", "10-14 år", ">14 år", "HF gj.snitt")
## setnames(tabw, getCol, nyNavn)

## ## Tabell
## tabOut <- exp.tabel(tabw, "Gj.snitt HbA1c pr. aldersgrupper",
##   ncol = 6, del = c(.2, .15, .15, .15, .15, .2),
##   size = 0.9, total = 2, rowHeight = .025, mixCol = 2:5)
