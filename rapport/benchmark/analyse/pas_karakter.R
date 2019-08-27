## Kun DT1 pasienter
## -----------------
## Datasettet som skal brukes er dt1
## for bare lokal sykehus bruker lokal2018

## datakilde er dt1

## Pasient karakteristika
varKar <- c("hosKort", "PasientID", "alder", "diagAlder", "diagVar", "bmi")

rawTab <- rollup(dt1, j = .(
  age.mean = mean(alder, na.rm = TRUE),
  age.sd = sd(alder, na.rm = TRUE),
  age.n = sum(!is.na(alder)),
  aged.mean = mean(diagAlder, na.rm = TRUE),
  aged.sd = sd(diagAlder, na.rm = TRUE),
  aged.n = sum(!is.na(diagAlder)),
  last.mean = mean(diagVar, na.rm = TRUE),
  last.sd = sd(diagVar, na.rm = TRUE),
  last.n = sum(!is.na(diagVar)),
  bmi.mean = mean(bmi, na.rm = TRUE),
  bmi.sd = sd(bmi, na.rm = TRUE),
  bmi.n = sum(!is.na(bmi))
),
by = c("hosKort"))


rawTab[, `:=` (
  age = sprintf("%0.1f(%0.1f) [%s]", age.mean, age.sd, age.n),
  agediag = sprintf("%0.1f(%0.1f) [%s]", aged.mean, aged.sd, aged.n),
  varig= sprintf("%0.1f(%0.1f) [%s]", last.mean, last.sd, last.n),
  bmi = sprintf("%0.1f(%0.1f) [%s]", bmi.mean, bmi.sd, bmi.n)
)]

selVar <- c("hosKort", "age", "agediag", "varig", "bmi")
tabClean <- rawTab[, ..selVar]
tabClean[is.na(hosKort), hosKort := "Totalt"]

nyNavn <- c("", "Alder", "Diagnose alder", "Sykdomsvarighet", "BMI")
setnames(tabClean, selVar, nyNavn)

tabOut <- exp.tabel(
  tabClean,
  "Gjennomsnitt(SD)[Antall]",
  ncol = 5,
  size = 0.9,
  rap = FALSE,
  rowHeight = 0.015,
  total = 1,
  del = c(.2, .22, .22, .22, .22),
  valgCol = 2:5,
  valgAlign = "left",
  mixCol = 2:5
  )
