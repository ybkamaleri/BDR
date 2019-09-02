## Kompletthet
## -----------
## Datakilde
useDT <- dt1

## useDT[is.na(und_syk_col), .N]
## useDT[is.na(lab_HbA1cAkerVerdi), .N]
## resVar <- c("und_syk_col", "und_syk_hypo", "und_syk_hype")

## Bruk ikke fastende, hvis missing bruk fastende
useDT[, ldl2 := lab_lip_LDL_2] %>%
  .[is.na(lab_lip_LDL_2), ldl2 := lab_lip_LDL]


## TSK / Coliaki
## useDT[, ]


## Valgte variabler
komVar <- c("lab_HbA1c", "lab_HbA1cAkerVerdi",  "und_inssjokk", "und_ketoacidose", "inn_Blodtrykk_s", "inn_Blodtrykk_d", "und_infiltrater", "inn_Lengde", "inn_Vekt", "ldl2", "und_syk_col", "und_syk_hypo", "und_syk_hype")

# Antall ikke missing
komWt <- useDT[, lapply(.SD, function(x) sum(!is.na(x))), .SDcols=komVar]

## sum total
komTot <- nrow(useDT)

## snu til long
komLg <- melt(komWt, measure.vars = komVar, variable.name = "var", value.name = "n")

## legger ved totallen
komLg[, tot := komTot]


## Fysisk aktivitet for bare de fra 6 år og eldre
aktDT <- subset(useDT, alder >=6)
## aktVar <- c("Inn_Akt", "Inn_Akt_Timer_Uke")
## aktiv[, lapply(.SD, function(x) sum(!is.na(x))), .SDcols=aktVar]
aktN <- aktDT[, .(var = "aktiv", n = sum(!is.na(Inn_Akt)), tot = .N)]

## Merge alle DT
komAll <- rbindlist(list(komLg, aktN), use.names = TRUE)

## prosent
komAll[, pros := round(n / tot * 100, digits = 1)]

## Gir nytt navn til var
komAll[, nr := .I]
komNavn <- c(
  "HbA1c eget lab",
  "HbA1c sentral",
  "Insulinsjokk",
  "DKA",
  "BT Systolisk",
  "BT Diastolisk",
  "Infiltrater",
  "Høyde",
  "Vekt",
  "LDL",
  "Coliaki",
  "Hypothyreose",
  "Hyperthyreose",
  "Fysisk aktivitet"
  )

komAll
komAll[.(nr = 1:14, to = komNavn), on = "nr", var2 := i.to]
## komAll[, c("var", "nr") := NULL]

(fig.komp <- regbar(komAll, y = pros, x = var2, num = n, ylab = "Andel (%)",
  title = "Data kompletthet årskontroll 2018"))

fig.komp
ggsave("komplethet.jpg", plot = fig.komp, width = 15, height = 20, units = "cm")
