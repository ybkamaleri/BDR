## nasjonalitet og kjønn
## ---------------------

nordLand <- c("Norge", "Danmark", "Finland", "Sverige", "Island")

## Data kilder
nasDT <- lok2018dt1

## nasDT3 <- copy(nasDT)
## nasDT <- copy(nasDT2)

nasDT[!is.na(fodelandMor) , nordiskMor := ifelse(fodelandMor %in% nordLand, 1L, 0L)]
nasDT[!is.na(fodelandFar) , nordiskFar := ifelse(fodelandFar %in% nordLand, 1L, 0L)]

## nordisk barn
## Minst en av foreldrene er født i de nordiske landene
nasDT[, .N, by=.(nordiskFar)]

nordBarn <- nasDT[, {mor = ifelse(is.na(nordiskMor), 0, nordiskMor);
  far = ifelse(is.na(nordiskFar), 0, nordiskFar);
  barn = mor + far;
  nordiskBarn = ifelse(is.na(nordiskMor) & is.na(nordiskFar), NA, barn);
  list(barn = barn,
    nordiskBarn = nordiskBarn,
    mor = nordiskMor,
    far = nordiskFar,
    kjonn = Kjonn,
    Pnr = Pnr
  )}]

nordBarn[.(nordiskBarn = 1:2, to = 1), on = "nordiskBarn", nordiskBarn := i.to]
# nordBarn[nordiskBarn %in% 1:2, nordiskBarn := 1]

# ukjent nasjonalitet
nordBarn[is.na(nordiskBarn), nordiskBarn := 3]

## Tabell long
barnLg <- nordBarn[, .N, by = .(nordiskBarn, kjonn)]

## Legger prosent
barnLg[, pros := round(N / sum(N, na.rm = T) * 100, digits = 1)]

## Mix antall og prosent
barnLg[, n := sprintf("%s (%0.1f%%)", N, pros)]

## Snu til wide
barnWt <- dcast(barnLg, kjonn ~ nordiskBarn, value.var = "n")

## Sjekk antall kolonner fordi ikke alle har annen nasjonaltitet
colN <- ncol(barnWt)
colNavn <- names(barnWt)
kol1 <- grep("kjonn", colNavn, value = T)
kol2 <- grep("1", colNavn, value = T)
kol3 <- grep("0", colNavn, value = T)
kol4 <- grep("3", colNavn, value = T)

## Kjønn skal være fleretall
barnWt[.(kjonn = c("Gutt", "Jente"), to = c("Gutter", "Jenter")), on = "kjonn", kjonn := i.to]


if (colN == 4) {
  ## reorder columns
  setcolorder(barnWt, c(kol1, kol2, kol3, kol4))

  ## legger riktig colnavn
  setnames(barnWt, names(barnWt), c(" ", "Nordisk", "Ikke Nordisk", "Ukjent"))
}

if (colN == 3){
  ## reorder columns
  setcolorder(barnWt, c(kol1, kol2, kol3))

  ## legger riktig colnavn
  setnames(barnWt, names(barnWt), c(" ", "Nordisk", "Ikke Nordisk"))

}
