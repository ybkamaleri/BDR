## HbA1c og aldersgruppe
valgVar <- "lab_HbA1cAkerVerdi"

## ddt1 <- subset(dt1, !is.na(valgVar))

tabraw <- cube(dt1[!is.na(get(valgVar)), ],
  j = .(
    mean = round(mean(get(valgVar), na.rm = TRUE), digits = 2),
    n = .N
  ),
  by = c("hosKort", "agekat"))

tabraw[is.na(agekat), agekat := 5]
tabw <- dcast(tabraw, hosKort ~ agekat, value.var = "mean", )
tabn <- dcast(tabraw, hosKort ~ agekat, value.var = "n", )

## reorder row
setorder(tabw, hosKort, na.last = TRUE)
setorder(tabn, hosKort, na.last = TRUE)

tabw[is.na(hosKort), hosKort := "Hele landet"]
tabn[is.na(hosKort), hosKort := "Hele landet"]

## gir nytt colnavn
oldNavn <- names(tabw)[-1]
setnames(tabw, oldNavn, paste0("v", names(tabw)[-1]))
setnames(tabn, oldNavn, paste0("v", names(tabn)[-1]))

## beholder bare to digits inkludert 0
tabw[, `:=`(
  u1 = sprintf("%0.2f", v1),
  u2 = sprintf("%0.2f", v2),
  u3 = sprintf("%0.2f", v3),
  u4 = sprintf("%0.2f", v4),
  u5 = sprintf("%0.2f", v5)
)]

## del ubrukte colnavn
bortCol <- paste0("v", oldNavn)
tabw[, (bortCol) := NULL]

## Bytt NA as.character til "-"
for (j in seq_len(ncol(tabw))[-1]){
  set(tabw, which(tabw[[j]] == 'NA'), j = j, value = "-")
}



## Legger totalt antall pasienter
tabnF <- tabn[, c(1, 6)]
## tabnF
tabwn <- tabw[tabnF, on = "hosKort"]
tabwn

## Gir nytt navn til slutt tabell
getCol <- names(tabwn)
nyNavn <- c("", "<5 år", "5-9 år", "10-14 år", ">14 år", "HF gj.snitt", "Antall (N)")
setnames(tabwn, getCol, nyNavn)

## library(huxtable)

## Tabell
tabOut <- exp.tabel(tabwn, "Gj.snitt HbA1c pr. aldersgrupper",
  ncol = 7, del = c(.2, .15, .15, .15, .15, .1, .1),
  size = 0.9, total = 2, rowHeight = .025, mixCol = 2:5)


## huxtable::quick_rtf(tabOut, file = "hba1c_alder.rtf")
## huxtable::quick_xlsx(tabOut, file = "hba1c_alder.xlsx")
