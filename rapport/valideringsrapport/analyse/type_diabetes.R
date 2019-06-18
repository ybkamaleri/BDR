## Type diabetes
## ----------------

diaTabell <- function(x){
  
  ## Demografisk variabler
  demoVar <-  c("PasientID", "PasientID", "hospital", "hospID", "hosKort", "alder", "Kjonn", "diagVar", "diagAlder")
  
  ## diabetes variabler
  diabetesVar = c("diabetes_Type1", "diabetes_Type2", "diabetes_Mody", "diabetes_Kir62", "diabetes_SekDiabetes", "diabetes_AnnenDiabetes", "diabetes_UkjentDiabetes")
  
  ## lage subset
  diabDT <- x[, c(demoVar, diabetesVar), with = F]
  
  ## Lage en long dataset
  diabLg <- melt(data = diabDT, 
                 id.vars = demoVar,
                 measure.vars = diabetesVar,
                 variable.name = "diabType",
                 value.name = "janei")
  
  
  ## Omkode diabetes type
  diabLg[.(janei = "Ja", diabType = diabetesVar, to = 1:7), on = c("janei", "diabType"), dbt := i.to]
  
  ## Beholder bare de som svarte Ja
  diabJa <-  diabLg[janei == "Ja", ]
  
  ## rekode annen type diabetes
  ## 1 = DT1, 2 = DT2, 3 = Mody, 4 = Annen type
  diabJa[, dbtype := dbt][dbt > 3, dbtype := 4]
  
  
  ## Tabell
  ## -------
  ## Tabell mal
  tabMal <- data.table(dbtype = rep(1:4, each = 2), Kjonn = rep(c("Gutt", "Jente"), 2))
  
  ## tabell diabetes
  tabDiaHosp <- diabJa[, .N, by=.(dbtype, Kjonn)]
  
  ## sjekk hvis det er 0 i antall pasient ved å sammenlikne sykehus tabell mot tabell malen
  diaUt <- tabDiaHosp[, .(dbtype, Kjonn)]
  
  # tabell for rad som er forskjellige fra tabell malen
  # tabDiff <-  sqldf::sqldf('SELECT * FROM tabMal EXCEPT SELECT * FROM diaUt')
  tabDiff <- data.table::fsetdiff(tabMal, diaUt)
  radN <- dim(tabDiff)[1]

  # Final tabell 
  if (radN > 0) {
    
    tabDiff[, N := 0]
    tabDia <- rbindlist(list(tabDiaHosp, tabDiff))
    
  } else {
    
    tabDia = tabDiaHosp
    
  }
  
  
  ## Snu til wide
  tabWd <- dcast(tabDia, Kjonn ~ dbtype, value.var = "N")
  
  ## Legger Totalen
  tabTot <- tabWd[, lapply(.SD, function(x) sum(x, na.rm = T)), .SDcols = names(tabWd)[-1]]
  tabTot[, Kjonn := "Totalt"]
  
  ## legge alle i en tabell
  diab.tab <- rbindlist(list(tabWd, tabTot), use.names = TRUE)
  
  diab.tab[.(Kjonn = c("Gutt", "Jente"), to = c("Gutter", "Jenter")), on = "Kjonn", Kjonn := i.to]
  
  ## legge kolonnavn til tabellen
  tabNavn <-c(" ", "DT1", "DT2", "MODY", "Annen")
  setnames(diab.tab, names(diab.tab), tabNavn)
  
  ## Antall pasienter
  diab.n <- dim(diabJa)[1]
  
  utData <- list(diab.tab = diab.tab, diab.n = diab.n)
  
  return(invisible(utData))

}


## Lokal sykehus
utLocal <- diaTabell(hosp2018)
diab.lokal <- utLocal[["diab.tab"]][]
antall.lokal <- utLocal[["diab.n"]]


## Legger Nasjonale tall
utNasj <- diaTabell(ars2018)
diab.nasj <- utNasj[["diab.tab"]][]
antall.nasj <- utNasj[["diab.n"]]


