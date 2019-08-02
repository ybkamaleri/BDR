## HbA1c og alder

lokalDT <- lok2018dt1

## Antall med svar
hbN <- lokalDT[!is.na(lab_HbA1cAkerVerdi), .N]

## Aggregate
hbaTab <- lokalDT[,
  .(
    n = .N,
    mean = mean(lab_HbA1cAkerVerdi, na.rm = T),
    median = median(lab_HbA1cAkerVerdi, na.rm = T),
    sd = sd(lab_HbA1cAkerVerdi, na.rm = T),
    min = min(lab_HbA1cAkerVerdi, na.rm = T),
    max = max(lab_HbA1cAkerVerdi, na.rm = T)
  ),
  by = .(agecat, agekat)]


setorder(hbaTab, agekat)

hbaTab[, agekat := NULL]

valgNavn <- names(hbaTab)[3:ncol(hbaTab)]

for (i in valgNavn[c(1, 3)]){
 hbaTab[, (i) :=  sprintf("%0.2f", get(i))]
}

setnames(hbaTab, c("agecat", "mean", "max"), c("Alder", "gj.snitt", "maks"))

outTab <- tabHux(hbaTab)

outTab <- outTab %>%
  set_align(, c(3, 5), "right")
