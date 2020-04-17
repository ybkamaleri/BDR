####################################################
## Nyoppdaget pasienter i 2018                    ##
## Diabetes type KUN for førstegangsregistrering  ##
####################################################

## NB!-------------------------------------------------------------------------------
## Denne filen er bare til å lage nyoppdaget liste av pasienter for hvert sykehus
## som skal sendes for doublesjekk hos sykehusene. Alle videre arbeid for retting
## basert på tilbakemelding fra sykehus gjøres i filen "data_vask2018.Rmd"
## ----------------------------------------------------------------------------------

rm(list=ls())
# Sti K:\Sensitivt\Forskning02\Barnediabetesregisteret_2015-5739\Barnediabetes og kvalitet\Datafiler\2018
filSti <- gsub("\\\\", "/", readline())

## load data. OBS! Sjekk nyeste datasettet
DT2018 <- readRDS(file.path(filSti, "bdr2018.rds", fsep = "/"))
## valg bare førstegangsregisrering
dtfirst <- subset(DT2018, kontroll %like% "registrering")

## diagnose år velges for å telle ny oppdaget i 2018
dtfirst[diagyr == 2018][!duplicated(Pnr), .N]
dtfirst[diagyr == 2018 & !duplicated(Pnr), .N]

# -------------------------------------------------------------------------------------------------------------------------------------
# INFO:
# Alle som ble registeret i eReg kan ikke ha flere diagnose dato siden det ble registeret bare en gang ved førstegangsregistrering
# lab_pH og lab_Bikarbonat ble også bare registeret kun en gang i førstegangsregistrering
# -------------------------------------------------------------------------------------------------------------------------------------

## PH og bikarbonat
grep("ph", names(DT2018), ignore.case = T, value = T)
grep("bikarbonat", names(DT2018), ignore.case = T, value = T)

## Type diabetes
grep("diabetes", names(dtfirst), ignore.case = T, value = T)
diabetesVar = c("diabetes_Type1", "diabetes_Type2", "diabetes_Mody", "diabetes_Kir62", "diabetes_SekDiabetes", "diabetes_AnnenDiabetes", "diabetes_UkjentDiabetes")

# diagnose år i 2018 - Bare de som er Nyoppdaget dvs. "Førstegangsreg"
dtfirst[diagyr == 2018, diabetesVar, with=FALSE]
dtfirst[diagyr == 2018, .N, by = .(hospital)] #diagnoset 2018 per sykehus

diabType <- dtfirst[diagyr == 2018 , 
                   .SD, .SDcols = c("PasientID", "Pnr", "hospital", 
                                    "FNavn", "ENavn", "FDato", "FNr", 
                                    "diagDato", "inn_DiagDato", diabetesVar, 
                                    "lab_pH", "lab_BiKarbonat", "Sykehus", 
                                    "inn_Sykehus", "inn_Sykehus1", "kontroll")]

dim(diabType)
diabType[!duplicated(Pnr), .N]
diabType[!duplicated(PasientID), .N]

diabType[is.na(inn_DiagDato), .(PasientID, Pnr, hospital, diagDato, lab_pH, lab_BiKarbonat, diabetes_Type1, diabetes_Type2, diabetes_Mody)]

# # ta bort når DiagDato er missing. Duplikat person som allered har diagDato
# missDiagDato <- diabType[is.na(inn_DiagDato), .(PasientID)][[1]] #convert to vector list missing diagose dato
# # ser av de med misssing diag dato hvis det noe info forsvinner ved å eksludere dem
# DT2018[PasientID %in% missDiagDato, .(PasientID, Pnr, hospital, inn_DiagDato, lab_pH, lab_BiKarbonat, diabetes_Type1, diabetes_Type2, diabetes_Mody, diagyr)]
# 
# nyDT <- diabType[!is.na(inn_DiagDato), ] ## bort med missing Diag_dato
# dim(nyDT)
# 
# nyDT[!duplicated(Pnr), .N]


################################# TEST START ##############################
dtTest <- diabType[PasientID %in% c(2881, 3302), ]
dtTest

dtTestLong <- melt(dtTest, id.vars = c("PasientID", "Pnr", "hospital", "FNavn", "ENavn", "FDato", "FNr", "diagDato", "lab_pH", "lab_BiKarbonat", "Sykehus", "inn_Sykehus", "inn_Sykehus1", "kontroll"),
                   measure.vars = diabetesVar,
                   variable.name = "diabType",
                   value.name = "JaNei")

setkey(dtTestLong, Pnr)
dtTestLong
################################ TEST END #################################



## ------------------------------
## Transformerer data til Long
## ------------------------------
# rm(diabLong)
# reshape
diabLong <- melt(diabType, id.vars = c("PasientID", "Pnr", "hospital", "FNavn", "ENavn", "FDato", "FNr", "diagDato", "lab_pH", "lab_BiKarbonat", "Sykehus", "inn_Sykehus", "inn_Sykehus1", "kontroll"),
                 measure.vars = diabetesVar,
                 variable.name = "diabType",
                 value.name = "JaNei")


## Valg mellom PesientID eller Personnr
## Når det gjelder tilbakemelding til Sykehus for pasienter som flyttet til annent sykehus for behandling dvs.
## Pasienter med to PasientID
setkey(diabLong, PasientID)
diabLong[!duplicated(PasientID), .N]
setkey(diabLong, Pnr)
diabLong[!duplicated(Pnr), .N]
diabLong[!duplicated(Pnr), .N, by=.(diabType)]

diabLong
## Omkode diabetes type
diabLong[, diabKode := NA_integer_][
  JaNei == "Ja" & diabType == "diabetes_Type1", diabKode := 1L][
    JaNei == "Ja" & diabType == "diabetes_Type2", diabKode := 2L][
      JaNei == "Ja" & diabType == "diabetes_Mody", diabKode := 3L][
        JaNei == "Ja" & diabType == "diabetes_Kir62", diabKode := 4L][
          JaNei == "Ja" & diabType == "diabetes_SekDiabetes", diabKode := 5L][
            JaNei == "Ja" & diabType == "diabetes_AnnenDiabetes", diabKode := 6L][
              JaNei == "Ja" & diabType == "diabetes_UkjentDiabetes", diabKode := 7L][]

# diabLong[, diabKode := as.factor(diabKode)]
diabLong[, .N, by=.(diabKode)]

## Sjekk de med DT2 eller Mody er virkelig riktig kodet
# diabLong[, .N, by=.(diabJa)]
dt2 <- diabLong[diabKode==2, .(PasientID), keyby=PasientID][[1]]
diabLong[PasientID %in% dt2, .(PasientID, diabType, JaNei, diabKode, hospital, kontroll, diagDato)] ## sjekk diab type er riktig
mody <- diabLong[diabKode==3, .(PasientID), keyby=PasientID][[1]]
diabLong[PasientID %in% mody, .(PasientID, diabType, JaNei, diabKode, hospital, kontroll, diagDato)] ## sjekk diab type er riktig


## Rekode
diabLong[, diabNavn := NA_character_][
  diabKode == 1, diabNavn := "DT1"][
    diabKode == 2, diabNavn := "DT2"][
      diabKode == 3, diabNavn := "Mody"][
        diabKode == 4, diabNavn := "Kir62"][
          diabKode == 5, diabNavn := "SekDiabetes"][
            diabKode == 6, diabNavn := "Annen"][
              diabKode == 7, diabNavn := "Ukjent"][]

diabLong[!duplicated(PasientID), .N, by=.(diabNavn)]
diabLong[PasientID %in% dt2, ]

# legge til kode for missing per PasientID for at kunne beholder bare en linje per pasient uten å utelate noe info
diabLong[, diabNavn := diabNavn[!is.na(diabNavn)][1], by = .(PasientID)][]

# legge ph verdi til alle rad per PasientID hvis missing
diabLong[is.na(lab_pH) & !duplicated(PasientID), .N]
diabLong[, ph := lab_pH[!is.na(lab_pH)][1], by = .(PasientID)][]
diabLong[is.na(lab_pH) & !duplicated(Pnr), .(ph)] #Alle skal ha NA fordi de har missing lab_pH

# legge bikarbonat hvis missing per PASIENTiD
diabLong[, bikarbonat := lab_BiKarbonat[!is.na(lab_BiKarbonat)][1], by = .(PasientID)][]


## Valg variabler for nyoppdaget Tabell
diabTab <- diabLong[!duplicated(PasientID), .(Etternavn = toupper(ENavn), 
                                              FNavn, 
                                              Fødselsdato = as.character(FDato), 
                                              Diagnosedato = as.character(diagDato),
                                              DiagNavn = as.factor(diabNavn),
                                              PH = lab_pH, 
                                              Bikarbonat = lab_BiKarbonat, 
                                              hospital,
                                              # PasientID,
                                              # Sykehus,
                                              Pnr
)]

## sjekke
diabTab[!duplicated(Pnr), .N]
diabTab[!duplicated(Pnr), .N, by=.(DiagNavn)]

(mody=grep("Mody", diabTab$DiagNavn, value = T)[1])
diabTab[ DiagNavn == mody, ]
diabTab[, .N, by = .(DiagNavn)]


# Fornavn med hver første bokstaver er stor bokstave
# Funksjon for fornavn med capital første bukstave
# firstup <- function(x) {
#   x <- tolower(x)
#   substr(x, 1, 1) <- toupper(substr(x, 1, 1))
#   x
# }

## bedre funksjon
capstr <- function(x) {
  y = strsplit(tolower(x), " ")[[1]]
  paste(toupper(substr(y, 1, 1)), substring(y, 2), #substring har default for stop = 1000000L mens substr må spsifisere start og stop
        sep = "", collapse = " ")
}


# diabTab[diabTab[, paste(firstup(unlist(strsplit(FNavn, " "))), collapse = " "), by = Pnr], on = "Pnr", Fornavn := i.V1] #V1 er automastisk variabelnavn lages som skal endres til Fornavn
diabTab[, Fornavn := capstr(FNavn), by=.(Pnr)][]

diabTab
dim(diabTab)
diabTab[!duplicated(Pnr), .N, by=.(DiagNavn)]

### Save nyoppdaget fil
### Denne filen inneholder ny variabel "DiagNavn" hvor diabetes type samlet i en variabel
filStiNy <- "K:/Sensitivt/Forskning02/Barnediabetesregisteret_2015-5739/Barnediabetes og kvalitet/Datafiler/2018/nyoppdaget"

# filStiNy <- gsub("\\\\", "/", readline())
# saveRDS(diabTab, file.path(filStiNy, "20190327_nyoppdaget2018.rds", fsep = "/"))
# diabTab <- readRDS(file.path(filStiNy, "20190327_nyoppdaget2018.rds", fsep = "/"))





## Restrukturer variabelnavn til innsending tabell
tabVar <- c("Etternavn", "Fornavn", "Fødselsdato", "Diagnosedato", "Diabetes Type", "PH", "Bikarbonat")



#### TEST SYKEHUS

haugesund <- grep("Haugesund", diabTab$hospital, ignore.case = T, value = T)[1]

haugTab <- diabTab[hospital == haugesund, .(ENavn, FNavn, FDato, diagDato, diabNavn, lab_pH, lab_BiKarbonat)]
haugTab[, `:=` (ENavn = toupper(ENavn), FNavn = firstup(FNavn), FDato = as.character(FDato), diagDato = as.character(diagDato)) ]
openxlsx::write.xlsx(haugTab, file = paste(validSti, "haugesund2018.xlsx", sep = "/"), asTable = TRUE)


(stvg <- grep("stavanger", diabTab$hospital, ignore.case = T, value = T)[1])

stvgTab <- diabTab[hospital == stvg, .(ENavn, FNavn, FDato, diagDato, diabNavn, lab_pH, lab_BiKarbonat)]
stvgTab[, `:=` (ENavn = toupper(ENavn), FNavn = firstup(FNavn), FDato = as.character(FDato), diagDato = as.character(diagDato)) ]
openxlsx::write.xlsx(stvgTab, file = paste(validSti, "stavanger2018.xlsx", sep = "/"), asTable = TRUE)



## -----------------------------------------------------
## Pasienter med flere enn 1 Førstegangsregistrering
## -----------------------------------------------------
diabTypeFirst <- copy(diabType)
nfirst <- diabTypeFirst[, .(nfirstreg = length(kontroll)), by=Pnr]
nfirst[, .N, by = .(nfirstreg)]
(pnrfirst <- nfirst[nfirstreg == 2, .(Pnr)][[1]])

(doubleFirstReg <- diabTypeFirst[Pnr %in% pnrfirst, .SD, by=.(Pnr)])

## OBS!! Dette er midler tidig renaming for å lage tillegg info av pasienter med to førstegangsregiter
## ved bruk 'diabLong' så jeg slipper å lage alle koder på nytt og kan bare kjøre alt fra 'melting' i 'diabLong'

##rename fordi jeg ikke har lagt funksjon til denne og kan låne transformasjon der oppe for diabType
diabLong <- melt(doubleFirstReg, id.vars = c("PasientID", "Pnr", "hospital", "FNavn", "ENavn", "FDato", "FNr", "diagDato", "lab_pH", "lab_BiKarbonat", "Sykehus", "inn_Sykehus", "inn_Sykehus1", "kontroll"),
                 measure.vars = diabetesVar,
                 variable.name = "diabType",
                 value.name = "JaNei")

## Legge manglende info for pasienter som har to førstegangsregisteringer når det finnes

## Denne gjøres etter at 
setkey(diabLong, PasientID)
diabLong[!duplicated(PasientID), .N]
setkey(diabLong, Pnr)
diabLong[!duplicated(Pnr), .N]
diabLong[!duplicated(Pnr), .N, by=.(diabType)]

diabLong
## Omkode diabetes type
diabLong[, diabKode := NA_integer_][
  JaNei == "Ja" & diabType == "diabetes_Type1", diabKode := 1L][
    JaNei == "Ja" & diabType == "diabetes_Type2", diabKode := 2L][
      JaNei == "Ja" & diabType == "diabetes_Mody", diabKode := 3L][
        JaNei == "Ja" & diabType == "diabetes_Kir62", diabKode := 4L][
          JaNei == "Ja" & diabType == "diabetes_SekDiabetes", diabKode := 5L][
            JaNei == "Ja" & diabType == "diabetes_AnnenDiabetes", diabKode := 6L][
              JaNei == "Ja" & diabType == "diabetes_UkjentDiabetes", diabKode := 7L][]

# diabLong[, diabKode := as.factor(diabKode)]
diabLong[, .N, by=.(diabKode)]

## Sjekk de med DT2 eller Mody er virkelig riktig kodet
# diabLong[, .N, by=.(diabJa)]
dt2 <- diabLong[diabKode==2, .(PasientID), keyby=PasientID][[1]]
diabLong[PasientID %in% dt2, .(PasientID, diabType, JaNei, diabKode, hospital, kontroll, diagDato)] ## sjekk diab type er riktig
mody <- diabLong[diabKode==3, .(PasientID), keyby=PasientID][[1]]
diabLong[PasientID %in% mody, .(PasientID, diabType, JaNei, diabKode, hospital, kontroll, diagDato)] ## sjekk diab type er riktig


## Rekode
diabLong[, diabNavn := NA_character_][
  diabKode == 1, diabNavn := "DT1"][
    diabKode == 2, diabNavn := "DT2"][
      diabKode == 3, diabNavn := "Mody"][
        diabKode == 4, diabNavn := "Kir62"][
          diabKode == 5, diabNavn := "SekDiabetes"][
            diabKode == 6, diabNavn := "Annen"][
              diabKode == 7, diabNavn := "Ukjent"][]

diabLong[!duplicated(PasientID), .N, by=.(diabNavn)]
diabLong[PasientID %in% dt2, ]

# legge til kode for missing per PasientID for at kunne beholder bare en linje per pasient uten å utelate noe info
diabLong[, diabNavn := diabNavn[!is.na(diabNavn)][1], by = .(PasientID)][]

# legge ph verdi til alle rad per PasientID hvis missing
diabLong[is.na(lab_pH) & !duplicated(PasientID), .N]
diabLong[, ph := lab_pH[!is.na(lab_pH)][1], by = .(PasientID)][]
diabLong[is.na(lab_pH) & !duplicated(Pnr), .(ph)] #Alle skal ha NA fordi de har missing lab_pH

# legge bikarbonat hvis missing per PASIENTiD
diabLong[, bikarbonat := lab_BiKarbonat[!is.na(lab_BiKarbonat)][1], by = .(PasientID)][]


## Valg variabler for nyoppdaget Tabell
diabTab <- diabLong[!duplicated(PasientID), .(Etternavn = toupper(ENavn), 
                                              FNavn, 
                                              Fødselsdato = as.character(FDato), 
                                              Diagnosedato = as.character(diagDato),
                                              DiagNavn = as.factor(diabNavn),
                                              PH = lab_pH, 
                                              Bikarbonat = lab_BiKarbonat, 
                                              hospital,
                                              PasientID,
                                              Sykehus,
                                              Pnr)]


## bedre funksjon
capstr <- function(x) {
  y = strsplit(tolower(x), " ")[[1]]
  paste(toupper(substr(y, 1, 1)), substring(y, 2), #substring har default for stop = 1000000L mens substr må spsifisere start og stop
        sep = "", collapse = " ")
}


# diabTab[diabTab[, paste(firstup(unlist(strsplit(FNavn, " "))), collapse = " "), by = Pnr], on = "Pnr", Fornavn := i.V1] #V1 er automastisk variabelnavn lages som skal endres til Fornavn
diabTab[, Fornavn := capstr(FNavn), by=.(PasientID)][]

diabTab
dim(diabTab)
diabTab[!duplicated(Pnr), .N, by=.(DiagNavn)]

## Restrukturer variabelnavn til innsending tabell
tabVar <- c("Etternavn", "Fornavn", "Fødselsdato", "Diagnosedato", "Diabetes Type", "PH", "Bikarbonat")

# lage
tilmappe <- "K:\\Sensitivt\\Forskning02\\Barnediabetesregisteret_2015-5739\\Barnediabetes og kvalitet\\Datafiler\\2018\\nyoppdaget\\tillegg"
# saveRDS(diabTab, file.path(tilmappe, "tillegg.Rds", fsep = "\\"))


