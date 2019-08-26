## Type diabetes
## ----------------

fun.dtype <- function(x){

  ## Demografisk variabler
  demoVar <-  c("PasientID", "Pnr", "hospital", "hospID", "hosKort", "alder", "Kjonn", "diagVar", "diagAlder")

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

diabDT <- fun.dtype(DT)
## diabDT[]

hosp.diab <- groupingsets(diabDT,
  j = .(
    td1 = sum(dbtype == 1),
    td2 = sum(dbtype == 2),
    mody = sum(dbtype == 3),
    annen = sum(dbtype == 4)
  ),
  by = c("hosKort"),
  sets = list(c("hosKort"), character(0)))

hosp.dttot <- cube(diabDT, .(antall = sum(!is.na(dbtype))),
  by = c("hosKort"))

tab.dtype <- hosp.diab[hosp.dttot, on = "hosKort"]

tab.dtype[is.na(hosKort), hosKort := "Totalt"]

tabNavn <- c("", "Type I", "Type II", "Mody", "Annen", "Totalt")
setnames(tab.dtype, names(tab.dtype), tabNavn)

tabOut <- exp.tabel(tab.dtype, "Diabetes type", ncol = 6, total = 2, mixCol = 2:5)
