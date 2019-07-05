## Finner ut pasienter som ikke har diabetes diagnose
## ---------------------------------------------------

## Demografisk variabler
demoVar <-  c("PasientID", "Pnr", "hospital", "hospID", "hosKort", "alder", "Kjonn", "diagVar", "diagAlder")

## diabetes variabler
diabetesVar = c("diabetes_Type1", "diabetes_Type2", "diabetes_Mody", "diabetes_Kir62", "diabetes_SekDiabetes", "diabetes_AnnenDiabetes", "diabetes_UkjentDiabetes")

## lage subset
diabDT <- ars2018[, c(demoVar, diabetesVar), with = F]

## trim whitespace
for (j in names(diabDT)){
  set(diabDT,  j = j, value = diabDT[[trimws(j)]])
}

## måtte gjøre det nesten manuelt siden trimws for alle kolonne i data.table ikke
## funker som det bør være
diabDT[!is.na(diabetes_Kir62), diabetes_Kir62  := trimws(diabetes_Kir62)]

## Lage en long dataset
diabLg <- melt(data = diabDT,
  id.vars = demoVar,
  measure.vars = diabetesVar,
  variable.name = "diabType",
  value.name = "janei")



diabLL <- copy(diabLg)
## trim whitespace
for (j in names(diabLL)){
  set(diabLL,  j = j, value = diabLL[[trimws(j)]])
}


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
