## Blodtrykk
## Etter at dataene koblet. Se koder i filen bloodtrykkCovert.R i analayse_alldt folder

## datakilde
valgDT <- lok2018dt1

## 'elevated' er bp mellom 90 og 94 percentil
## 'stage' begge 1 og 2 betyr bp mer eller lik 95 percentil

## normal
## bp90 <- valgDT[stage == "normal", .(var = 0, n = .N)]

## elevated
bp94 <- valgDT[stage == "elevated", .(var = 1, n = .N)]

## stage
bp95 <- valgDT[stage  %in% c("stage1", "stage2"), .(var = 2, n = .N)]

## merge
bpTab <- rbind(bp94, bp95)

## Tabell
## Eksluderer alle under 2 år
## antall uten manglende informasjon for å beregne blodtrykk
pbValid <- valgDT[alder > 1.999 & !is.na(stage), .N]

bpTab[, tot := pbValid]
bpTab[, pros := round(n / tot * 100, digits = 1)]

bpNavn <- c("90-94 perc.", ">= 95 perc.")

bpTab[.(var = 1:2, to = bpNavn), on = "var", bt := i.to]
bpTab[, c("var", "tot") := NULL]
setcolorder(bpTab, c("bt","n", "pros"))
setnames(bpTab, names(bpTab), c("Blodtrykk", "Antall", "Andel"))

## Huxtabel
lastLine <- nrow(bpTab) + 1
bp.htab <- as_hux(bpTab, add_colnames = TRUE)

bp.htab <- bp.htab %>%
  set_bold(1,, TRUE) %>%
  set_bottom_border(1,, TRUE) %>%
  set_bottom_border(lastLine,, TRUE) %>%
  set_position("left") %>%
  map_background_color(by_rows("grey95", "white"))
