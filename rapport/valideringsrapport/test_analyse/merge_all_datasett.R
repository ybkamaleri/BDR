## koble alle datasettet
## ---------------------
rm(list = ls())
if(!require(rio)) install.packages("rio")
library(rio)

library(haven)

library(data.table)

datamappe <- "K:/Sensitivt/Forskning02/Barnediabetesregisteret_2015-5739/Barnediabetes og kvalitet/Datafiler"

# list.dirs(path = file.path(datamappe), recursive = F)

filmappe <- dir(file.path(datamappe), pattern = "^20", full.names = TRUE)
filmappe

## Trenger R 3.6 for å kunne bruke rio/haven
## ------------------------------------
# dt2001 <- import(file.path(filmappe[1], "Data_2001.sav"))
# dt2002 <- import(file.path(filmappe[2], " "))
dt2003 <- import(file.path(filmappe[3], "data_03.sav"))
dt2004 <- import(file.path(filmappe[4], "Barndiab_04.sav"))
dt2005 <- import(file.path(filmappe[5], "BenchmarkingData_2005.sav"))
dt2006 <- import(file.path(filmappe[6], "Data2006_bench.rapporter.sav"))
dt2007 <- import(file.path(filmappe[7], "Alle data 2007 pr 040308.sav"))
dt2008 <- import(file.path(filmappe[8], "Alle data 2008 26.03.09.sav"))
dt2009 <- import(file.path(filmappe[9], "Data2009_april.sav"))
dt2010 <- import(file.path(filmappe[10], "Datafil_120411_2010_analysefil.sav"))
dt2011 <- import(file.path(filmappe[11], "Analysefil_2011_180412_org.sav"))
dt2012 <- import(file.path(filmappe[12], "Analysefil_2012_230513.sav"))
dt2013 <- import(file.path(filmappe[13], "Analysefil_2013_140414.sav"))
dt2014 <- import(file.path(filmappe[14], "Gjeldenede kobling/2014_Analysefil250515.sav"))
dt2015 <- import(file.path(filmappe[15], "Endelig_analysefil_2015.sav"))
dt2016 <- import(file.path(filmappe[16], "Årskontroll_2016_april.sav"))
dt2017 <- import(file.path(filmappe[17], "Årskontroll17.sav"))


## Haven package
# dt2001 <- import(file.path(filmappe[1], "Data_2001.sav"))
# dt2002 <- haven::read_spss(file.path(filmappe[2], " "))
dt2003 <- haven::read_spss(file.path(filmappe[3], "data_03.sav"))
dt2004 <- haven::read_spss(file.path(filmappe[4], "Barndiab_04.sav"))
dt2005 <- haven::read_spss(file.path(filmappe[5], "BenchmarkingData_2005.sav"))
dt2006 <- haven::read_spss(file.path(filmappe[6], "Data2006_bench.rapporter.sav"))
dt2007 <- haven::read_spss(file.path(filmappe[7], "Alle data 2007 pr 040308.sav"))
dt2008 <- haven::read_spss(file.path(filmappe[8], "Alle data 2008 26.03.09.sav"))
dt2009 <- haven::read_spss(file.path(filmappe[9], "Data2009_april.sav"))
dt2010 <- haven::read_spss(file.path(filmappe[10], "Datafil_120411_2010_analysefil.sav"))
dt2011 <- haven::read_spss(file.path(filmappe[11], "Analysefil_2011_180412_org.sav"))
dt2012 <- haven::read_spss(file.path(filmappe[12], "Analysefil_2012_230513.sav"))
dt2013 <- haven::read_spss(file.path(filmappe[13], "Analysefil_2013_140414.sav"))
dt2014 <- haven::read_spss(file.path(filmappe[14], "Gjeldenede kobling/2014_Analysefil250515.sav"))
dt2015 <- haven::read_spss(file.path(filmappe[15], "Endelig_analysefil_2015.sav"))
dt2016 <- haven::read_spss(file.path(filmappe[16], "Årskontroll_2016_april.sav"))
dt2017 <- haven::read_spss(file.path(filmappe[17], "Årskontroll17.sav"))

## konvertere til data.table
# dt.filer <- ls(pattern = "^dt20") #enklere men ikke helt presis
dt.filer <- grep("^(dt200|dt201)", ls(), ignore.case = T, value = T) #sikre presisjon

## Legge alle data som en LIST
# for (i in dt.filer) setDT(get(i)),
# allDT <- lapply(dt.filer, function(x) setDT(get(x))) # lage list men tar ikke med seg navnet
allDT <- sapply(dt.filer, function(x) setDT(get(x)), simplify = FALSE, USE.NAMES = TRUE) #beholder filenavn i list
## gir kilderdata dvs. årskontroll år
for (i in dt.filer) allDT[[i]][, kilder := gsub("^dt", "", i)]


# gjør direkte konvertering til data.table
invisible(sapply(ls(pattern = "^dt20"), function(x) setDT(get(x))))

# lage filen
saveRDS(allDT, "BDR_all_yr.rds")
allBDR <- readRDS("BDR_all_yr.rds")

## --------------------------
## Finne HbA1c variabler
## --------------------------

## Sjekk variabelnavn for HbA1c
# sprintf("%s variabelnavn: %s", ls(pattern = "dt2003"), grep("analyse_hba1c", names(dt2003), ignore.case = T, value = T ))
(listNavn <- sapply(dt.filer, function(x) grep("analyse_hba1c", names(get(x)), ignore.case = T, value = T ), USE.NAMES = TRUE))

## empty character
kosong <- character(0)
## velge bare datasett som har variabel som i listNavn
TF <- lapply(listNavn, function(x) !identical(x, kosong))
## velge index fra listNavn
hbaTRUE <- which(unlist(TF))
## lage hba1c variable for alle som har samme variabel navn som i listNavn
for (i in dt.filer[hbaTRUE]){
  assign(i, get(i)[, hba1c := Analyse_HbA1c_verdi])
}


## for 2016 og 2017 heter det "lab_HbA1cAkerVerdi"
listNavn2 <- sapply(dt.filer, function(x) grep("lab_hba1cAkerVerdi", names(get(x)), ignore.case = T, value = T ), USE.NAMES = TRUE)
listNavn2
## empty character
kosong <- character(0)
## velge bare datasett som har variabel som i listNavn
TF2 <- lapply(listNavn2, function(x) !identical(x, kosong))
## velge index fra listNavn
hbaTRUE2 <- which(unlist(TF2))
## lage hba1c variable for alle som har samme variabel navn som i listNavn
for (i in dt.filer[hbaTRUE2]){
  assign(i, get(i)[, hba1c := lab_HbA1cAkerVerdi])
}

## type diabetes
sapply(dt.filer, function(x) grep("type", names(get(x)), ignore.case = T, value = T ), USE.NAMES = TRUE)
sapply(dt.filer, function(x) grep("type_diab", names(get(x)), ignore.case = T, value = T ), USE.NAMES = TRUE) #2017
sapply(dt.filer, function(x) grep("Diabetestype", names(get(x)), ignore.case = T, value = T ), USE.NAMES = TRUE)

for( i in dt.filer[-c(1:4, 15)]){
  assign(i, get(i)[, dbtype := Diabetestype])
}

for( i in dt.filer[c(15)]){
  assign(i, get(i)[, dbtype := type_diab])
}

sapply(dt.filer, function(x) grep("dbtype", names(get(x)), ignore.case = T, value = T ), USE.NAMES = TRUE)


## Pasient id
sapply(dt.filer, function(x) grep("pasientnr", names(get(x)), ignore.case = T, value = T ), USE.NAMES = TRUE)
sapply(dt.filer, function(x) grep("fnr", names(get(x)), ignore.case = T, value = T ), USE.NAMES = TRUE)

## kjønn - match exact som "Kjønn"
sapply(dt.filer, function(x) grep("\\bKjønn\\b", names(get(x)), value = T ), USE.NAMES = TRUE)
sapply(dt.filer, function(x) grep("\\bKjonn\\b", names(get(x)), value = T ), USE.NAMES = TRUE) #2016 og 2017

## Kjønn til gender
for (i in dt.filer[-c(14,15)]){
  assign(i, get(i)[, gender := Kjønn])
}
## Kjonn til gender
for (i in dt.filer[14:15]){
  assign(i, get(i)[, gender := Kjonn])
}

## Alder
sapply(dt.filer, function(x) grep("\\bAlder\\b", names(get(x)), value = T ), USE.NAMES = TRUE)

for (i in dt.filer){
  assign(i, get(i)[, age := Alder])
}

## Alder diagnose
sapply(dt.filer, function(x) grep("alder_diagnose", names(get(x)), ignore.case = T, value = T ), USE.NAMES = TRUE)
sapply(dt.filer, function(x) grep("alder_diag", names(get(x)), ignore.case = T, value = T ), USE.NAMES = TRUE)

# Diagnose alder ikke finnes for 2003 og 2004
library(lubridate)
dt2003[, alder_diagnose := as.numeric(round(as.period(interval(Fødselsdato, Diagnosedato)/duration(n=1, units = "years")), digits = 1))]
dt2004[, alder_diagnose := as.numeric(round(as.period(interval(Fødselsdato, Ddato)/duration(n=1, units = "years")), digits = 1))]

for (i in dt.filer){
  dnavn <- sapply(i, function(x) grep("alder_diag", names(get(x)), ignore.case = T, value = T), USE.NAMES = T)
  assign(i, get(i)[, diagage := get(dnavn)])
}


## Sykdomvarighet
sapply(dt.filer, function(x) grep("\\bsykdomsvarighet\\b", names(get(x)), ignore.case = T, value = T ), USE.NAMES = TRUE)
sapply(dt.filer, function(x) grep("sykvarig", names(get(x)), ignore.case = T, value = T ), USE.NAMES = TRUE) #2005
dt2005[, Sykdomsvarighet := Sykvarig]

for (i in dt.filer){
  assign(i, get(i)[, sykvarig := Sykdomsvarighet])
}


## Sykehus
sapply(dt.filer, function(x) grep("\\bsykehus\\b", names(get(x)), ignore.case = T, value = T ), USE.NAMES = TRUE)
sapply(dt.filer, function(x) grep("Beh_sykehus", names(get(x)), value = T ), USE.NAMES = TRUE) #2003 har bare beh_sykehus
dt2003[, hospital := as.character(Beh_sykehus)]

## -- Loope og lage nye variable 'hospital' fordi "Sykehus" har forskjellige bokstaver dvs. stor og liten
for (i in dt.filer[-1]){
  snavn <- sapply(i, function(x) grep("\\bsykehus\\b", names(get(x)), ignore.case = T, value = T), USE.NAMES = T)
  assign(i, get(i)[, hospital := get(snavn)])
}


## LDL - spør Siv Janne hvilken variabel skal brukes til LDL
sapply(dt.filer, function(x) grep("LDL", names(get(x)), ignore.case = T, value = T), USE.NAMES = T)



## Datakilde
## lager "årskontroll år" for hver kilde data
for (i in dt.filer){
  assign(i, get(i)[, yr := gsub("^dt", "", i)])
}


### Antall pasienter
### --------------------
## Pasientnr variable
dtPasientNR <- dt.filer[-c(5,14,15)]
psnr <- "Pasientnr"
sapply(dtPasientNR, function(x) get(x)[, .(unik = length(unique(get(psnr))), dup = sum(duplicated(get(psnr))))], USE.NAMES = T )
dim(dt2015)

## 2007
dt2007[, .(unik = length(PID), dup = sum(duplicated(PID)))]
dim(dt2007)

## Fnr variabel
dtFNR <- c("dt2015", "dt2016", "dt2017")
sapply(dtFNR, function(x) get(x)[, .(unik = length(unique(Fnr)), dup = sum(duplicated(Fnr)))], USE.NAMES = T )


## Antall pasienter
sapply(dt.filer, function(x) dim(get(x))[1])


## -----------------------
## Uttrekk
## -----------------------

## Fil fra 2004 til 2006 er eksludert pga. manglende type diabetes

valgVar <- c("hospital", "yr", "hba1c", "sykvarig", "age", "diagage", "gender", "dbtype")
dt.filer.in <- dt.filer[-c(1:4)]

DTListAll <- sapply(dt.filer.in, function(x) subset(get(x), select = valgVar), simplify = F, USE.NAMES = T)

### Koble allesammen
AlleDB <- rbindlist(DTListAll, fill = TRUE, use.names = TRUE)
# AlleDB <- do.call(rbind, DTListAll))



## legger sykehusnavn
## ------------------

hospkode <- fread(file.path(dataSti, "kode_hospital.csv"), header = FALSE, encoding = "Latin-1")
setDT(hospkode)
hospkode[, V2 := tolower(V2)]
setkey(hospkode, V2)
setkey(AlleDB, hospital)


setkey(hospkode, V1)
hospkode


# sykehus ID mine standard id for analyse
hosID <- read.csv("sykehusID.csv")


# BDR <- hospkode[AlleDB, on = c(V2 = "hospital")]
BDR <- merge(AlleDB, hospkode, by.x = "hospital", by.y = "V2", all.x = TRUE)
BDR
setnames(BDR, c("V1", "hospital"), c("sykehusOld", "kodeOld"))
saveRDS(BDR, file.path(dataSti, "Allebdr_utvalg.RDS"))
BDR <- readRDS("Allebdr_utvalg.RDS")







## ----- MISC ------ ##
names(dt2003)

hb2003 <- c("Analyse_HbA1c_verdi", "Hb1Ac_verdi")

dt2003[, .(Analyse_HbA1c_verdi, Hb1Ac_verdi)]
dt2003[, lapply(.SD, function(x) sum(is.na(x))), .SDcols=hb2003]

# Valg variabler
names(dt2004)
dt2004[, lapply(.SD, function(x) sum(is.na(x))), .SDcols=hb2003]
var2004ldl <- grep("ldl", names(dt2004), ignore.case = T, value = T)

var2004 <- c("Tot_kol", "HDL_kol", "LDL_kol")
dt2004[, lapply(.SD, str), .SDcols=var2004ldl]
dt2004[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = var2004ldl]

## 2005
dt2005[, lapply(.SD, function(x) sum(is.na(x))), .SDcols=hb2003]


## justering hba1c
## ---------------

## andel kjønn i hele materielle
(jenter <- dt2017[, .N, by = .(Kjonn)])

dt2017[, {tot = dim(dt2017)[1];
          jente = dt2017[Kjonn == 1, .N];
          proJen = (jente/tot)*100;
          list(jente = proJen)}]

dt2017[, .N, by=.(Kjonn)]
dim(dt2017)
(1528/2822)*100

dt2017[, sex := ifelse(Kjonn == 2, 1, 0)]

hbFit <- lm(lab_HbA1cAkerVerdi ~ sex + Alder + Sykdomsvarighet, data = dt2017)
summary(hbFit)
coefficients(hbFit)



####################
## CHECK
bdr.all <- readRDS(file.path(dataSti,"BDR_all_yr.rds")) #all files in list

## Unlist all data to Global env.
list2env(bdr.all, envir = .GlobalEnv)


## type diabetes
sapply(dt.filer, function(x) grep("type_diab", names(get(x)), ignore.case = T, value = T ), USE.NAMES = TRUE)
sapply(dt.filer, function(x) grep("alder_diag", names(get(x)), ignore.case = T, value = T ), USE.NAMES = TRUE)

d2006 <- bdr.all[["dt2006"]]
names(d2006)

d2004 <- bdr.all[["dt2004"]]
d2004[, .N, by = Dtype2]
