## Diabetes ketoacidose (DKA)

## Datakilde
lokalDT <- ars2018

lokalDT[, .N, by = und_ketoacidose]

## minAge <- floor(as.numeric(min(lokalDT$alder, na.rm = TRUE)))
## maxAge <- ceiling(as.numeric(max(lokalDT$alder, na.rm = TRUE)))

lokalDT[!is.na(alder), agecat := ageCat(alder, 0, 15, by = 5)]
## lokalDT[, .N, by = agecat]
## lokalDT[agecat == "15+", .(alder, agecat)]

dkaTab <- groupingsets(lokalDT,
  j = .(N = sum(!is.na(und_ketoacidose)),
       ja = length(which(und_ketoacidose == "Ja"))),
  by = c("agecat"),
  sets = list(c("agecat"), character(0))
)

dkaTab[, pros := round(ja / N * 100, digits = 1)]
dkaTab[ja == 0, pros := "-"]

## lager id til sortering
dkaTab[, id := .I]
dkaTab[is.na(agecat), id := 0]
setorder(dkaTab, -id)

dkaTab[is.na(agecat), agecat := "Totalt"]
dkaTab[, id := NULL]

dkaTabell <- tabFun(dkaTab, "Har hatt ketoacidose", total = TRUE)
