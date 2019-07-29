## Alle BDR datasettet
## ---------------------

bdr.all <- readRDS(file.path(dataSti,"BDR_all_yr.rds")) #all files in list
bdr.raw <- readRDS(file.path(dataSti,"Allebdr_utvalg.RDS"))

## Felles kode for sykehus
hoskode <- rio::import(file.path(dataSti, "sykehus_felles.xlsx"))
## Sykehus brukte navn
sykID <- fread(file.path(dataSti, "sykehusID.csv"), encoding = "Latin-1")

## Legge sykehuskode fra felles kode
bdr.rev <- merge(bdr.raw, hoskode, by.x = "kodeOld", by.y = "Kode", all.x = TRUE)

## legge til komplett navn til sykehus
bdr <- merge(bdr.rev, sykID, by.x = "Felles", by.y = "hospID", all.x = TRUE)

## Endre var
setnames(bdr, c("Felles", "hospital"), c("hospid", "hosp"))


### Diabetestype
## Ingen info om diabetes type i 2006, derfor alle data fra 2003 til 2006 eksludert




## CHECK
b17 <- bdr.all[["dt2017"]]
names(b17)
b17[, .N, by = type_diab]
