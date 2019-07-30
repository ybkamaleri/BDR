## Gjennomsnitt hba1c målt sentralt
## --------------------------------

## ## Datakilder
## ## gamle data "bdrB4" uploaded fra runEnkelRapport
## bdrold <- subset(bdrB4, select = c("hba1c", "yr", "hospid", "age", "gender"))
## bdrold

## ## Lokal data
## dtlokal <- ars2018

## valgVar <- c("lab_HbA1cAkerVerdi", "Kjonn", "yr", "hospID", "alder")
## dtlokal[, str(.SD), .SDcols = valgVar]

## valgDT <- subset(dtlokal, select = valgVar)
## valgDT[.(Kjonn = c("Gutt", "Jente"), to = 1:2), on = "Kjonn", gender := i.to]
## valgDT[, Kjonn := NULL]

## ## gir standard navn
## stdNavn <- c("hba1c", "yr", "hospid", "alder", "kjonn")
## setnames(valgDT, names(valgDT), stdNavn)
## setnames(bdrold, names(bdrold), stdNavn)

## ## Merge begge
## alldt <- rbindlist(list(bdrold, valgDT), use.names = TRUE)

## ## Lage file til raskere upload
## saveRDS(alldt, file.path(dataSti, "all07til18.rds"))


## upload data
alldt <- readRDS(file.path(dataSti, "all07til18.rds"))

## subset fra loop data
dtvalg <- subset(alldt, hospid == hosp)
