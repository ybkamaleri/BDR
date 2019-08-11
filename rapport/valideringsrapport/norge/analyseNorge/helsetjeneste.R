## Gjennomsnitt bruk av helsetjeneste

## Datakilde
lokalDT <- lok2018dt1

## Vagte variabler
valgVar <- c("und_kons_ant", "und_lege_ant", "und_sykepl_ant", "und_andre_ant", "und_felles_ant", "und_uteblitt_ant")

## lokalDT[, str(.SD), .SDcols = valgVar]

## Lager long data
dtLong <- melt(lokalDT, id.vars = c("PasientID", "Kjonn"),
  measure.vars = valgVar,
  variable.name = "valg",
  value.name = "antall")

## dtLong[, .N, by = .(valg, antall)]

## Missing antall NA antas som 0 dvs. ingen konsultasjoner
## dtLong[is.na(antall), antall := 0]

## Lager tabellen
dtTabell <- dtLong[,
  list(
    n = .N,
    mean = round(mean(antall, na.rm = TRUE), digits = 1),
    median = round(median(antall, na.rm = TRUE), digits = 1),
    maks = max(antall, na.rm = TRUE),
    na = length(which(is.na(antall)))),
  by = .(valg)
]

## Rename valg variabel
toVar <- c(
  "Innleggelser på sykehus pga. diabetes",
  "Konsultasjoner hos lege",
  "Konsultasjoner hos sykepleier",
  "Konsultasjoner hos andre medlemmer i diabetesteamet",
  "Felles konsultasjoner",
  "Uteblitte konsultasjoner"
)

dtTabell[.(valg = valgVar, to = toVar), on = "valg", navn := i.to]

## Resturkturerer tabellen
lastcol <- ncol(dtTabell)
setcolorder(dtTabell, c("navn", names(dtTabell)[-lastcol]))
dtTabell[, valg := NULL]

setnames(dtTabell, names(dtTabell),
  c(" ", "antall", "gj.snitt", "median", "maks", "ubesvart"))


## utTabell <- tabHux(dtTabell, size = 0.9, rap = TRUE)
utTabell <- tabHux(dtTabell, size = 0.9, rap = TRUE, del = c(.4, .1, .1, .1, .1, .1))
