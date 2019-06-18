## kilde
kilde <- "./valideringsrapport/analyse"

## Setup
## -------
source(file.path(kilde, "setup.R"))

## samtykke pasienter bør tas bort?
samtykke

## Utvalgt sykehus
hosp2018 <- ars2018[hospID == 1, ]


## Type diabetes
## ---------------
source(file.path(kilde, "type_diabetes.R"))
# Antall diabetes
diab.lokal
antall.lokal
diab.nasj 
antall.nasj

## Sjekk pasienter som ikke blitt registeret på noe type diabetes
## Pasienter som har diabetes type 
saveRDS(diabJa, "diabJa2018.rds")
diabJa <- readRDS("diabJa2018.rds")
dim(diabJa)

saveRDS(diabLg, "diabAlleType.RDS") #alle med diabetes
diabLg <- readRDS("diabAlleType.RDS")

otherPnr <- diabLg[!(Pnr %in% diabJa$Pnr), .(Pnr)]
dim(otherPnr)
noDiaType <- unique(otherPnr)[["Pnr"]]
noDiaType
saveRDS(noDiaType, "noDiaType.RDS")

## Demografisk variabler
demoVar <-  c("PasientID", "Pnr", "hospital", "hospID", "hosKort", "alder", "Kjonn", "diagVar", "diagAlder", "diagDato", "inn_Dato")

## diabetes variabler
diabetesVar = c("diabetes_Type1", "diabetes_Type2", "diabetes_Mody", "diabetes_Kir62", "diabetes_SekDiabetes", "diabetes_AnnenDiabetes", "diabetes_UkjentDiabetes")

ars2018[Pnr %in% noDiaType, c(demoVar, diabetesVar), with=F] #liste uten diabetes type


