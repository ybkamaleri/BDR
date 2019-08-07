## Annen behandling

## lokalDT <- copy(ars2018)
lokalDT <- lok2018dt1

undVar <- c("beh_ace", "beh_blodtrykk_med", "beh_statin", "beh_anti_epil")

## lokalDT[, str(.SD), .SDcols = undVar]


## Antall som ikke er missing
undVarNon <- lokalDT[, lapply(.SD, function(x) sum(!is.na(x))), .SDcols = undVar]
undNonLg <- melt(undVarNon,
  measure.vars = undVar,
  variable.name = "var",
  value.name = "n")

## ## For annen sykedommer så skal andelen sammenligne mot total pasienter
## antallAlle <- as.integer(nrow(lokalDT))
## ## attr(undNonLg$var, "ATT") <- NULL
## undNonLg[, var := as.character(var, stringAsFactor = FALSE)] #convert factor to string
## undNonLg[var %like% "sykannet", n  := .(antallAlle)]

## Antal som svarte Ja
mList <- list()

for (i in undVar){
  misInd <- lokalDT[get(i)=="Ja", .N]
  mList[[i]] <- misInd
}

undVarTall <- as.data.table(mList)

undVarLg <- melt(undVarTall,
  measure.vars = undVar,
  variable.name = "var",
  value.name = "ja")

## Merge table
undAll <- merge(undVarLg, undNonLg, by = "var")

## Prosent
undAll[, pros := round(ja / n * 100, digits = 1)]

## Pyntting til tabellen
undAll[, id := .I]

nyNavn <- c(
  "ACE-hemmer",
  "Blodtrykk medikamenter",
  "Satiner",
  "Anti epileptika"
)


undAll[.(id = 1:8, to = nyNavn), on = "id", navn := i.to]

## Beholder 1 decimal selv om 0
undAll[, pro := as.character(sprintf("%0.1f", pros))]
## Hvis 0 så blir bare strek
undAll[pros == 0, pro := "- "]
## Bort ubrukte kollone
undAll[, c("var", "pros", "id") := NULL]
## Reorder
setcolorder(undAll, c("navn", "ja", "pro", "n"))


setnames(undAll, names(undAll),
  c(" ", "Antall", "Andel", "N"))

outTab <- tabFun(undAll, "Behandling", rap = TRUE)
