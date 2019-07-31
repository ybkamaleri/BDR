## Diabetes ketoacidose (DKA)

## Datakilde
lokalDT <- lok2018dt1

lokalDT[, .N, by = und_ketoacidose]

## Aggrigerer
dkaTab <- groupingsets(lokalDT,
  j = .(N = sum(!is.na(und_ketoacidose)),
       ja = length(which(und_ketoacidose == "Ja"))),
  by = c("agecat", "agekat"),
  sets = list(c("agecat", "agekat"), character(0))
)

dkaTab[, pros := as.character(round(ja / N * 100, digits = 1))]
dkaTab[ja == 0, pros := "-"]
dkaTab[is.na(agecat), agecat := "Totalt"]

## lager id til sortering
## dkaTab[, id := .I]
dkaTab[is.na(agekat), agekat := nrow(dkaTab)]
setorder(dkaTab, agekat) #reorder rows

## Justeringer
dkaTab[, agekat := NULL]
setcolorder(dkaTab, c("agecat", "ja", "pros", "N"))
setnames(dkaTab, names(dkaTab), c("Alder kategorier", "Antall", "Andel", "N"))

## Tabell
dkaTabell <- tabFun(dkaTab, "Har hatt ketoacidose", total = TRUE)
