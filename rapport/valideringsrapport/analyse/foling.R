## Føinger siste 4 uker for pasienter >= 5 år

## Datakilde
lokalDT <- subset(lok2018dt1, agekat != 1)

lokalDT[, .N, by = und_foling]

## Aggrigerer
folTab <- groupingsets(lokalDT,
  j = .(N = sum(!is.na(und_foling)),
       ja = length(which(und_foling == "Ja"))),
  by = c("agecat", "agekat"),
  sets = list(c("agecat", "agekat"), character(0))
)

folTab[, pros := as.character(round(ja / N * 100, digits = 1))]
folTab[ja == 0, pros := "-"]
folTab[is.na(agecat), agecat := "Totalt"]

## lager id til sortering
## folTab[, id := .I]
folTab[is.na(agekat), agekat := nrow(folTab)]
setorder(folTab, agekat) #reorder rows

## Justeringer
folTab[, agekat := NULL]
setcolorder(folTab, c("agecat", "ja", "pros", "N"))
setnames(folTab, names(folTab), c("Alder kategorier", "Antall", "Andel", "N"))

## Tabell
folTabell <- tabFun(folTab, "Har hatt føling", total = TRUE)
