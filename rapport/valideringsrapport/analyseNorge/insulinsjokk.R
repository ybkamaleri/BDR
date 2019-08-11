## Insulinsjokk med kramper og eller bevisstløshet

## Datakilde
lokalDT <- lok2018dt1

## lokalDT[, .N, by = und_inssjokk]

## Aggrigerer
insjTab <- groupingsets(lokalDT,
  j = .(N = sum(!is.na(und_inssjokk)),
       ja = length(which(und_inssjokk == "Ja"))),
  by = c("agecat", "agekat"),
  sets = list(c("agecat", "agekat"), character(0))
)

insjTab[, pros := as.character(round(ja / N * 100, digits = 1))]
insjTab[ja == 0, pros := "- "]
insjTab[is.na(agecat), agecat := "Totalt"]

## lager id til sortering
## insjTab[, id := .I]
insjTab[is.na(agekat), agekat := nrow(insjTab) * 2] #for å sikre det høyeste tall så ligger nedest
setorder(insjTab, agekat) #reorder rows

## Justeringer
insjTab[, agekat := NULL]
setcolorder(insjTab, c("agecat", "ja", "pros", "N"))
setnames(insjTab, names(insjTab), c("Alder kategorier", "Antall", "Andel", "N"))

## Tabell
insjTabell <- tabFun(insjTab, "Har hatt insulinsjokk", total = TRUE)
