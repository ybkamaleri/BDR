## nasjonalitet og kjønn
## ---------------------
## Definition: Land hvor minst en av foreldrene er født,
## hvis missing bruk barnes nasjonalitet

## Data kilder
nasDT <- lok2018dt1

## Nordiske land
nordLand <- c("Norge", "Danmark", "Finland", "Sverige", "Island")

## Nasjonalitet variabler - Nasjonalitet, mor- og farfødeland
nasDT[!is.na(fodelandMor) , nordiskMor := ifelse(fodelandMor %in% nordLand, 1L, 0L)]
nasDT[!is.na(fodelandFar) , nordiskFar := ifelse(fodelandFar %in% nordLand, 1L, 0L)]

## nordisk barn
## Minst en av foreldrene er født i de nordiske landene
## nasDT[, .N, by=.(nordiskFar)]

nordBarn <- nasDT[, {mor = ifelse(is.na(nordiskMor), 0, nordiskMor);
  far = ifelse(is.na(nordiskFar), 0L, nordiskFar);
  barn = mor + far;
  nordiskBarn = ifelse(is.na(nordiskMor) & is.na(nordiskFar), NA, barn);
  list(barn = barn,
    nasj = Nasjonalitet,
    barnland = FodeLand,
    nordkid = as.integer(nordiskBarn),
    mor = nordiskMor,
    far = nordiskFar,
    kjonn = Kjonn,
    PasientID = PasientID
  )}]

nordBarn[.(nordkid = 1L:2L, to = 1L), on = "nordkid", nordiskBarn := i.to]
## nordBarn[nordkid %in% 1:2, nordkid := 1]

## Barns nasjonalitet
## nordBarn[, nasj := trimws(nasj)]
## nordnasj <- c("Norsk", "norsk", "Dansk", "dansk", "Finsk", "Svensk", "svensk", "Island", "Islandsk")
## nordBarn[, barnNasj := ifelse(nasj %in% nordnasj, 1L, 0L)]

## ## Mix barn og sine foreldre nasjonalitet, hvis missing bruk barns sitt.
## nordBarn[is.na(nordiskBarn), nordiskBarn := barnNasj, by = PasientID]

## Ikke nordisk
nordBarn[nordkid == 0, nordiskBarn := 0]

# ukjent nasjonalitet
nordBarn[is.na(nordiskBarn), nordiskBarn := 3]

## Aggrigerer
nasAgg <- groupingsets(nordBarn,
  j = .(
    nordisk = length(which(nordiskBarn == 1)),
    ikke = length(which(nordiskBarn == 0)),
    ukjent = length(which(nordiskBarn == 3))
  ),
  by = c("kjonn"),
  sets = list(
    c("kjonn"),
    character(0)
  ))

## Ingen Ukjent?
noUkjent <- nasAgg[is.na(kjonn), ukjent == 0]

## Totalt
nasAgg[is.na(kjonn), kjonn := "Totalt"]
nasAgg[, N := rowSums(nasAgg[, -1])]

## Prosent
nasAgg[, `:=`(
  npro = round(nordisk / N * 100, digits = 1),
  ipro = round(ikke / N * 100, digits = 1),
  upro = round(ukjent / N * 100, digits = 1)
),
by = .(kjonn)]

## Mix antall og prosent
nasAgg[, `:=`(
  nord_N = sprintf("%s (%0.1f%%)", nordisk, npro),
  ikke_N = sprintf("%s (%0.1f%%)", ikke, ipro),
  ukjent_N = sprintf("%s (%0.1f%%)", ukjent, upro)
),
by = .(kjonn)]

## Hvis 0 så byttes med strek
nasAgg[nordisk == 0, nord_N := "-"]
nasAgg[ikke == 0, ikke_N := "-"]
nasAgg[ukjent == 0, ukjent_N := "-"]


## Beholder det relevante
bortVar <- c("nordisk", "ikke", "ukjent", "npro", "ipro", "upro")
nasAgg[,(bortVar) := NULL ]

## ## bytt NA med strekk
## for (j in seq_len(ncol(nasAgg))){
##   set(nasAgg, which(is.na(nasAgg[[j]])), j = j, value = "  -")
## }

## Sjekk antall kolonner fordi ikke alle har annen nasjonaltitet
colN <- ncol(nasAgg)
colNavn <- names(nasAgg)
kol1 <- grep("kjonn", colNavn, value = T)
koln <- grep("^N", colNavn, value = T)
kol2 <- grep("nord", colNavn, value = T)
kol3 <- grep("ikke", colNavn, value = T)
kol4 <- grep("ukjent", colNavn, value = T)

barnWt <- nasAgg

## Kjønn skal være fleretall
barnWt[.(kjonn = c("Gutt", "Jente"), to = c("Gutter", "Jenter")), on = "kjonn", kjonn := i.to]

## Tar bort kolonne "Ukjent" hvis totalen er 0
if (noUkjent) barnWt[, Ukjent := NULL]

## Gir kollonnavn
if (colN == 5) {
  ## reorder columns
  setcolorder(barnWt, c(kol1, koln, kol2, kol3, kol4))

  ## legger riktig colnavn
  setnames(barnWt, names(barnWt), c("kjonn", "N", "Nordisk", "Ikke Nordisk", "Ukjent"))
}

if (colN == 4){
  ## reorder columns
  setcolorder(barnWt, c(kol1, koln, kol2, kol3))

  ## legger riktig colnavn
  setnames(barnWt, names(barnWt), c("kjonn", "N", "Nordisk", "Ikke Nordisk"))

}

if (colN == 3){
  ## reorder columns
  setcolorder(barnWt, c(kol1, koln, kol2))

  ## legger riktig colnavn
  setnames(barnWt, names(barnWt), c("kjonn","N", "Nordisk"))
}

setnames(barnWt, "kjonn", "")
barnWt[, N := as.character(N)] #problem med tall vises som scientific format

## utTabell <- tabHux(barnWt, total = TRUE, del = c(.1, .15, .22, .22, .22), autoformat = TRUE)

utTabell <- tabFunx(barnWt, total = TRUE, navn = "n (%)",
  del = c(.1, .15, .22, .22, .22), mix = 3:5, center = 3)
