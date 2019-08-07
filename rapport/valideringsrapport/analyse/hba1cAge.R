## HbA1c og alder

lokalDT <- lok2018dt1

## Antall med svar
hbN <- lokalDT[!is.na(lab_HbA1cAkerVerdi), .N]

## Aggregate
## hbaTab <- lokalDT[,
##   .(
##     n = .N,
##     mean = mean(lab_HbA1cAkerVerdi, na.rm = T),
##     median = median(lab_HbA1cAkerVerdi, na.rm = T),
##     sd = sd(lab_HbA1cAkerVerdi, na.rm = T),
##     min = min(lab_HbA1cAkerVerdi, na.rm = T),
##     max = max(lab_HbA1cAkerVerdi, na.rm = T)
##   ),
##   by = .(agecat, agekat)]

hbaTab <- rollup(lokalDT,
  j = list(
    n = .N,
    mean = mean(lab_HbA1cAkerVerdi, na.rm = TRUE),
    median = median(lab_HbA1cAkerVerdi, na.rm = TRUE),
    sd = sd(lab_HbA1cAkerVerdi, na.rm = TRUE),
    min = min(lab_HbA1cAkerVerdi, na.rm = TRUE),
    max = max(lab_HbA1cAkerVerdi, na.rm = TRUE)
  ),
  by = c("agecat", "agekat"))

## Totalt skal være nedest
hbaTab[is.na(agecat), agekat := nrow(hbaTab) * 2]

hbaTab <- hbaTab[!is.na(agekat), ]

setorder(hbaTab, agekat)
hbaTab[is.na(agecat), agecat := "Totalt"]
hbaTab[, agekat := NULL]

valgNavn <- names(hbaTab)[3:ncol(hbaTab)]

for (i in valgNavn){
 hbaTab[, (i) :=  sprintf("%0.2f", get(i))]
}

setnames(hbaTab, c("agecat", "mean", "max"), c("Alder", "gj.snitt", "maks"))

outTab <- tabHux(hbaTab, total = TRUE)

outTab <- outTab %>%
  set_align(, c(3, 5), "right")
