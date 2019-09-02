## Analysen til nettverksmøte

rm(list = ls())

## pakker
pkg <- c("data.table", "ggplot2", "rreg", "huxtable", "bookdown", "knitr", "rmarkdown", "colorspace", "pier", "stringi", "plotly")
lapply(pkg, library, character.only = TRUE)[[1]]

## Data path
dataSti <- "~/avid/bdr"
## Load all data
DT <- readRDS(file.path(dataSti, "dt2018medBlodTrykk.RDS"))

## ## Diabetes variabler bort med whitespace
## diabetesVar = c("diabetes_Type1", "diabetes_Type2", "diabetes_Mody", "diabetes_Kir62", "diabetes_SekDiabetes", "diabetes_AnnenDiabetes", "diabetes_UkjentDiabetes")
## for (j in diabetesVar){
##   if(class(DT[[j]]) == 'character')
##     set(DT, j = j, value = trimws(DT[[j]]))
## }

## ## Missing pasienter
## DT[PasientID == 3071, diabetes_Mody := "Ja"] #Østfold
## DT[PasientID == 6205, diabetes_UkjentDiabetes := "Ja"] #St. Olav
## saveRDS(DT, file.path(dataSti, "dt2018medBlodTrykk.RDS"))


## Bare TD1 og 'Sandessjøen' er eksludert pga. har bare 1 pasient
dt1 <- subset(DT, hospID != 5 & diabetes_Type1 == "Ja")

## data type1 fra 2007 - 2017
## dtB4 <- readRDS(file.path(dataSti, "allBDRtype1.rds"))
## dtB4[kodeOld == "ø", hospid := 5] #Sandessjoen
## saveRDS(dtB4, file.path(dataSti, "allBDR2007til2017.rds"))
dtB4 <- readRDS(file.path(dataSti, "allBDR2007til2017.rds"))

## hospital koder
sykehusID <- DT[!duplicated(hospID), .(hosKort, hospital), keyby = hospID]

idx <- c("Ostfold", "Ulleval", "Alesund", "Bodo", "Forde", "Nord_norge", "Gjovik", "Sandessjoen")
idRett <- c("Østfold", "Ullevål", "Ålesund", "Bodø", "Førde", "Tromsø", "Gjøvik", "Sandessjøen")

sykehusID <- sykehusID[.(hosKort = idx, to = idRett), on = "hosKort", hosKort := i.to]
## gir norsk borstaver til datasettet
dt1 <- dt1[.(hosKort = idx, to = idRett), on = "hosKort", hosKort := i.to]
DT <- DT[.(hosKort = idx, to = idRett), on = "hosKort", hosKort := i.to]
DT <- DT[hospID == 23, hosKort  :=  "Tromsø"]
dt1 <- dt1[hospID == 23, hosKort  :=  "Tromsø"]

## sort per hosKort
setkey(dt1, hosKort)
setkey(DT, hosKort)



#Helse Foretaket
dt1[hospID  %in% c(22, 1, 25, 13, 18, 20, 21, 19, 16, 15, 14), rhf := 1] #Hels S-Ø
dt1[hospID  %in% c(11, 17, 26, 7, 8, 9), rhf := 2] #Helse Midt
dt1[hospID %in% c(12, 3, 4, 2), rhf := 3] #Helse Vest
dt1[hospID  %in% c(10, 23, 6, 5, 24), rhf := 4]

rhfNavn <- c("HSØ", "HMidt", "HVest", "HNord")
dt1[.(rhf = 1:4, to = rhfNavn), on = "rhf", rhfN := i.to]


DT[, .N, by = .(hosKort, hospID)]
dt1[, .N, by = ]


## Aldersfordeling

dt1[, age := round(alder, digits = 0)]
dt1[, .(age)]
dt1[age == 20, .(hosKort)]
ageDT <- dt1[, .N, keyby = age]
ageDT

AlderFig <- ggplot(ageDT, aes(as.factor(age), N)) +
   geom_bar(stat = "identity", fill = "#004499")  +
  labs(y = "Antall", x = "Alder", title = "Aldersfordeling T1D årskontroll 2018") +
  theme_classic() + scale_y_continuous(expand = c(0, 0), limits = c(0, 350)) + #begynt på 0
  theme(
    axis.text = element_text(size = 12),
    panel.grid.major.y = element_line(size = .4, linetype = "dashed", color = "grey70"),
    ## panel.grid.minor.y = element_line(size = .2, linetype = "dashed", color = "grey70"),
    panel.grid.major.x = element_blank()
  )

AlderFig
ggsave("alder_fig.jpg", plot = AlderFig, width = 15, height = 8, units = "cm")


## Diabetestype
##-----------------

## Type diabetes
## ----------------

fun.dtype <- function(x){

  ## Demografisk variabler
  demoVar <-  c("PasientID", "Pnr", "hospital", "hospID", "hosKort", "alder", "Kjonn", "diagVar", "diagAlder", "bmi", "lab_HbA1cAkerVerdi")

  ## diabetes variabler
  diabetesVar = c("diabetes_Type1", "diabetes_Type2", "diabetes_Mody", "diabetes_Kir62", "diabetes_SekDiabetes", "diabetes_AnnenDiabetes", "diabetes_UkjentDiabetes")

  ## lage subset
  diabDT <- x[, c(demoVar, diabetesVar), with = F]


  ## --- Pasienter uten diagnoser ----
  diabDTx <- copy(diabDT)
  diabDTx[, (diabetesVar) := lapply(.SD, function(x) ifelse(x == "Ja", 1, 0)), .SDcols = diabetesVar]

  diabDTx[, diaSum := sum(.SD, na.rm = TRUE), .SDcols = diabetesVar, by = Pnr]

  ## diabDTx[diaSum == 0, .(Pnr, hosKort)]

  ## Lage en long dataset
  diabLg <- melt(data = diabDT,
                 id.vars = demoVar,
                 measure.vars = diabetesVar,
                 variable.name = "diabType",
                 value.name = "janei")


  ## Omkode diabetes type til tall fra 1 til 7
  diabLg[.(janei = "Ja", diabType = diabetesVar, to = 1:7), on = c("janei", "diabType"), dbt := i.to]

  ## Beholder bare de som svarte Ja
  diabJa <-  diabLg[janei == "Ja", ]

  ## rekode annen type diabetes
  ## 1 = DT1, 2 = DT2, 3 = Mody, 4 = Annen type
  diabJa[, dbtype := dbt][dbt > 3, dbtype := 4]

  return(diabJa)

}


DTtype1 <- fun.dtype(DT)
DTtype1


DTtype1[dbtype == 2, list(
  kjonn = sum(Kjonn == "Gutt"),
  Alder = mean(alder, na.rm = TRUE),
  Varighet = mean(diagVar, na.rm = TRUE),
  debut = mean(diagAlder, na.rm = TRUE),
  bmi = mean(bmi, na.rm = TRUE),
  hba1c = mean(get(valgVar), na.rm = TRUE)
)]

DTtype1[dbtype == 3, list(
  kjonn = sum(Kjonn == "Gutt"),
  Alder = mean(alder, na.rm = TRUE),
  Varighet = mean(diagVar, na.rm = TRUE),
  debut = mean(diagAlder, na.rm = TRUE),
  bmi = mean(bmi, na.rm = TRUE),
  hba1c = mean(get(valgVar), na.rm = TRUE)
)]

DTtype1[dbtype == 4, list(
  kjonn = sum(Kjonn == "Gutt"),
  Alder = mean(alder, na.rm = TRUE),
  Varighet = mean(diagVar, na.rm = TRUE),
  debut = mean(diagAlder, na.rm = TRUE),
  bmi = mean(bmi, na.rm = TRUE),
  hba1c = mean(get(valgVar), na.rm = TRUE)
)]





## Datakilde
ispDT <- dt1

ispDT[, ispad := 0] %>%
  .[alder >= 10 & diagVar >= 2, ispad  := 1] %>%
  .[alder < 10 & diagVar >= 5, ispad := 1]

## Antall kvalifisert til ISPAD definisjon
ispadN <- ispDT[ispad == 1, .N, by = hosKort]


## Øye undersøkelse
oye <- ispDT[ispad == 1 & und_Oye == "Ja", .(var = "oye", ja = .N)]
ispDT[ispad == 1, .N, by=.(und_Oye)] #sjekk tallene er riktig

## Urin
urin.isp <- ispDT[ispad == 1, sum(!is.na(lab_res_1prove))] #utført
## ispDT[ispad == 1, sum(is.na(lab_res_1prove))]
urin <- data.table(var = "urin", ja = urin.isp)

## ispad tabell
ispTab <- rbindlist(list(oye, urin))
ispTab[, tot := ispadN]
ispTab[, pros := round(ja / tot * 100, digits = 1)]

ispTab[.(var = c("oye", "urin"), to = c("Øye", "Urin")), on = "var", var := i.to]
setcolorder(ispTab, c("var", "ja", "pros", "tot"))
setnames(ispTab, names(ispTab), c("Undersøkelser", "Antall", "Andel", "N"))
