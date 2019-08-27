## Justert HbA1c for hele datasettet

## I 2017 blir det bare gjort justering for nordiske barn, men hvorfor justeringen gjort basert fra hele datasettet?

## Nordiske land
nordLand <- c("Norge", "Danmark", "Finland", "Sverige", "Island")

## Nasjonalitet variabler - Nasjonalitet, mor- og farfødeland
dt1[!is.na(fodelandMor) , nordiskMor := ifelse(fodelandMor %in% nordLand, 1L, 0L)]
dt1[!is.na(fodelandFar) , nordiskFar := ifelse(fodelandFar %in% nordLand, 1L, 0L)]

## nordiskBarn
dtNok <- dt1[, {
  mor = ifelse(is.na(nordiskMor), 0, nordiskMor);
  far = ifelse(is.na(nordiskFar), 0L, nordiskFar);
  barn = mor + far;
  nordiskBarn = ifelse(is.na(nordiskMor) & is.na(nordiskFar), NA, barn);
  list(
    alder = alder,
    kjonn = Kjonn,
    dtvar = diagVar,
    hosKort = hosKort,
    nordkid = as.integer(nordiskBarn),
    hba1c = lab_HbA1cAkerVerdi
  )}]

dtNok[nordkid > 0, norsk := 1] %>%
  .[nordkid == 0, norsk := 0]

## rekode kjonn
dtNok <- dtNok[.(kjonn = c("Gutt", "Jente"), to = 1:2), on = "kjonn", sex := i.to]

## Coefficient
nokMod <- lm(hba1c ~ alder + sex + dtvar, data = dtNok)
## summary(nokMod)
adjAlder <- summary(nokMod)$coefficients[2,1]
adjSex <- summary(nokMod)$coefficients[3,1]
adjVar <- summary(nokMod)$coefficients[4,1]

## Mean and andel
meanNok <- dtNok[, .(
  mAlder = mean(alder, na.rm = TRUE),
  mSex = (sum(sex == 2) / nrow(dtNok)) * 100,
  mVar = mean(dtvar, na.rm = TRUE)
  )]

mAlder <- meanNok$mAlder
mSex <- meanNok$mSex / 100 + 1
mVar <- meanNok$mVar

## Adjusted verdi
dtNok[, hbAdj := hba1c +
          adjAlder * (mAlder - alder) +
          adjSex * (mSex - sex) +
          adjVar * (mVar - dtvar)]

## Tabell
tabHb <- rollup(dtNok,
  j = .(
    hb1 = mean(hba1c, na.rm = TRUE),
    adjHb1 = mean(hbAdj, na.rm = TRUE),
    medAdjHb = median(hbAdj, na.rm = TRUE),
    age = mean(alder, na.rm = TRUE),
    varig = mean(dtvar, na.rm = TRUE)
  ),
  by = "hosKort")

tabHb[is.na(hosKort), hosKort := "Hele landet"]

## Prosen jente
proJente <- rollup(dtNok,
  j = .(proj = sum(sex == 2) / .N * 100),
  by = "hosKort")

proJente[is.na(hosKort), hosKort := "Hele landet"]

## merge
tabAll <- tabHb[proJente, on = "hosKort"]

tabAll[, `:=`(
  hb1 = sprintf("%0.2f", hb1),
  adjHb1 = sprintf("%0.2f", adjHb1),
  medAdjHb = sprintf("%0.2f", medAdjHb)
)]

roundVar <-  c("age", "varig", "proj")
tabAll[, (roundVar) := round(.SD, digits = 1), .SDcols = roundVar]

tabNavn <- c("", "Ujustert", "Justert", "Justert median", "Alder", "Syk.varighet", "Jenter (%)")

setnames(tabAll, names(tabAll), tabNavn)

tabOut <- exp.tabel(tabAll,
  xcol = c("", "HbA1c", "", "", "Gj.snitt", "", "Prosent"), rowHeight = 0.015,
  size = 0.9, total = 1, del = c(.2, .15, .15, .15, .1, .15, .1))

## quick_pdf(tabOut, file = "test.pdf")
