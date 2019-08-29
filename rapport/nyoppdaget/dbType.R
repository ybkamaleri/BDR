## Diabetes typer
##---------------
DTtype <- function(x){

  ## Demografisk variabler
  demoVar <-  c("PasientID", "Pnr", "hospital", "hospID", "hosKort", "Kjonn", "alder", "inn_DiagDato")

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


## Alder kategorier
##------------------------
## Lager kategorisering for alder
ageCat <- function(x, lower, upper, by, sep = "-") {
  ## Finne høyeste kategori
  kat <- paste0(seq(lower + by - 1, upper - 1, by = by))
  indTop <- max(length(kat))
  top <- as.numeric(kat[indTop])

  labs <- paste0(c(paste(seq(lower, upper - by, by = by),
    seq(lower + by - 1, upper - 1, by = by),
    sep = sep),
    paste(top + 1, "+", sep = "")))
  cut(floor(x), breaks = c(seq(lower, upper, by = by), Inf),
    include.lowest = TRUE, right = FALSE, labels = labs)
}
