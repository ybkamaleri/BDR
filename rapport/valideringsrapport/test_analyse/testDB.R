## Finner ut pasienter som ikke har diabetes diagnose
## ---------------------------------------------------

## Demografisk variabler
demoVar <-  c("PasientID", "Pnr", "hospital", "hospID", "hosKort", "alder", "Kjonn", "diagVar", "diagAlder")

## diabetes variabler
diabetesVar = c("diabetes_Type1", "diabetes_Type2", "diabetes_Mody", "diabetes_Kir62", "diabetes_SekDiabetes", "diabetes_AnnenDiabetes", "diabetes_UkjentDiabetes")

## lage subset
diabDT <- ars2018[, c(demoVar, diabetesVar), with = F]

## trim whitespace
##-----------------------
for (j in diabetesVar){
  if(class(diabDT[[j]]) == 'character')
    set(diabDT, j = j, value = trimws(diabDT[[j]]))
}

## ## vanlig loop for whitespace deletion. OBS!! Treggere enn loop set
## for (j in diabetesVar){
##  diabDT[, (j) := trimws(get(j))]
## }


## Check whitespace er borte
diabDT[Pnr == 7090197481, .(diabetes_Kir62)][[1]]

## ## måtte gjøre det nesten manuelt siden trimws for alle kolonne i data.table ikke
## ## funker som det bør være
## diabDT[!is.na(diabetes_Kir62), diabetes_Kir62  := trimws(diabetes_Kir62)]

## Lage en long dataset
diabLg <- melt(data = diabDT,
  id.vars = demoVar,
  measure.vars = diabetesVar,
  variable.name = "diabType",
  value.name = "janei")

diabLL <- copy(diabLg)

## omkode all som svarte Ja til diabetes Type til 1
diabLL[.(janei = "Ja", diabType = diabetesVar, to = 1L),
  on = c("janei", "diabType"), diab := i.to]

## Konvertere til wide
diabWide <- dcast(data = diabLL,
  formula = Pnr + hospID + hosKort ~ diabType, value.var = "diab")

### Sjekk
diabWide
diabWide[diabetes_Kir62 == 1, .(Pnr, hospID, hosKort, diabetes_Kir62)]

## bytt NA til 0
for (j in diabetesVar){
  set(diabWide, which(is.na(diabWide[[j]])), j, value = 0L)
}


## Summere for å finne missing for alt diabetes type
diabWide[, hvem := rowSums(.SD), .SDcols = diabetesVar ]

diabWide[, .N, by = hvem]

## som har ingen type diabetes
## ----------------------------
diaN <- diabWide[hvem == 0, .(Pnr)][[1]]

diaNoN <- list()
for (j in diaN){
  nv <- paste0("a", j) #må hav valid navn og ikke bare tall
  dd <- ars2018[Pnr == j, c("Pnr", "hospID", "hosKort", diabetesVar), with = F]
  diaNoN[[nv]] <- dd
}

diaNoNperson <- rbindlist(diaNoN)
diaNoNperson
diaNoNperson[, .(Pnr, hospID, hosKort, diabetes_Kir62)]

vetIkkeDiab <- diaNoNperson[[1]]


## Siv Janne har kontrollet og disse er resultat som må rettes i datasettet
vetdt1 <- vetIkkeDiab[c(1:2, 4:5, 8:12)] #DT1
vetMody <- vetIkkeDiab[6] #Mody

diaNoDiag <- copy(diaNoNperson)
diaNoDiag[Pnr %in% vetdt1, diabetes_Type1 := "Ja"] #DT1
diaNoDiag[Pnr == vetMody, diabetes_Mody := "Ja"]
diaNoDiag

## replace main datasets value with the corrected values
diabVar1 <- paste0("i.", diabetesVar)
ars2018[diaNoDiag, (diabetesVar) := mget(diabVar1), on = "Pnr"]



## pasienter med to type diabetes
## -----------------------------
dia2 <- diabWide[hvem == 2, .(Pnr)][[1]]

dia2DT <- list()
for (j in dia2){
  nv <- paste0("a", j) #må hav valid navn og ikke bare tall
  dd <- ars2018[Pnr == j, c("Pnr", "hospID", "hosKort", diabetesVar), with = F]
  dia2DT[[nv]] <- dd
}

dia2DTperson <- rbindlist(dia2DT)
dia2DTperson

diabVar1 = c("diabetes_Type2", "diabetes_Mody", "diabetes_Kir62", "diabetes_SekDiabetes", "diabetes_AnnenDiabetes", "diabetes_UkjentDiabetes")

dia2DTperson[diabetes_Type1 == "Ja", (diabVar1)  := NA]
## personer med flere diagnoser
dt1person <- dia2DTperson[, Pnr]
dt1person

## De som har DT1 og annen type blir bare med DT1.
perdt1 <- dt1person[-c(1:2)] #DT1
perkir <- dt1person[2] #kir62
perann <- dt1person[1] #annen

## Tømme alle verdie for de med minst to så legge tilbake det riktige etter at
## Siv Janne har kontorllert
dia2diag <- copy(dia2DTperson)
dia2diag[, (diabetesVar) := NA]
dia2diag[Pnr  %in% perdt1, diabetes_Type1 := "Ja"] #DT1
dia2diag[Pnr == perkir, diabetes_Kir62 := "Ja"] #Kira62
dia2diag[Pnr == perann, diabetes_AnnenDiabetes := "Ja"] #Annen
dia2diag

## bytte verdi i hoved datasettet med korrigert verdi
ars2018[dia2diag, (diabetesVar) := mget(diabVar1), on = "Pnr"]
