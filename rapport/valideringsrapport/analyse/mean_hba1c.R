## Gjennomsnitt hba1c målt sentralt
## --------------------------------

## ## Datakilder
## gamle data "bdrB4" uploaded fra runEnkelRapport
bdrold <- subset(bdrB4, select = c("hba1c", "yr", "hospid", "age", "gender"))

## Nasjonal data brukes
norgeDT <- ars2018dt1

## Antall pasienter
norgeN <- nrow(norgeDT)


## Utvalg av variabler
## -------------------
valgVar <- c("lab_HbA1cAkerVerdi", "Kjonn", "yr", "hospID", "alder")
## norgeDT[, str(.SD), .SDcols = valgVar]

valgDT <- subset(norgeDT, select = valgVar)
valgDT[.(Kjonn = c("Gutt", "Jente"), to = 1:2), on = "Kjonn", gender := i.to]
valgDT[, Kjonn := NULL]

## gir standard navn
stdNavn <- c("hba1c", "yr", "hospid", "alder", "kjonn")
setnames(valgDT, names(valgDT), stdNavn)
setnames(bdrold, names(bdrold), stdNavn)

## Merge begge
alldt <- rbindlist(list(bdrold, valgDT), use.names = TRUE)

## ## Lage file til raskere upload
## saveRDS(alldt, file.path(dataSti, "all07til18.rds"))


## - - - - - - -ANALYSE - - - - - - -

## ## upload data
## alldt <- readRDS(file.path(dataSti, "all07til18.rds"))

## Tar bort attributes inherits fra SPSS
alldt[] <- lapply(alldt, function(x) {attributes(x) <- NULL; x})

## mean hba1c for hvert år
alldt[, meanAll := round(mean(hba1c, na.rm = TRUE), digits = 1), keyby = .(yr)]

## mean hba1c for all years hver sykehus
## alldt[, meanhbc := round(mean(hba1c, na.rm = TRUE), digits = 1), keyby = .(yr, hospid)]

## Beholder bare en rad på hvert år
norgeTab <- alldt[alldt[, .I[1], by = yr]$V1]
norgeTab[, yr := as.numeric(yr)]


## Velge Lokal sykehus
## -----------------------
## subset fra loop data
dtvalg <- subset(alldt, hospid == hosp)

## gjennomsnitt hba1c
## dtvalg[, meanhbc  := round(mean(hba1c, na.rm = TRUE), digits = 1), by = yr]

## antall per år
## dtvalg[, N := .N, by = yr]
## dtvalg[, .N, by = yr]

## Beholder bare den første rad per år
## dtplot <- dtvalg[dtvalg[, .I[1], by = yr]$V1]
## dtplot <- dtvalg[, head(.SD, 1), yr] #alternativ med litt treg

dbaggr <- rollup(dtvalg,
  j = list(
    hba = mean(hba1c, na.rm = TRUE),
    n = .N),
  by = c("yr", "kjonn"))

## legger totallen dvs. 3. Kjønn er søm flgende:
## 1 = Gutter
## 2 = Jenter
## 3 = Totalt
dbaggr[is.na(kjonn), kjonn := 3]
dbaggr[.(kjonn = 1:3, to = c("Gutter", "Jenter", "Alle")), on = "kjonn", sex := i.to]

## tar bort Grand Total
dbaggr <- dbaggr[!is.na(yr), ]

## konverterer yr til nummeric ellers må bruke group=1
dbaggr[, yr := as.numeric(yr)]


## Plotting
## ---------

## Farge
col3 <- valgCol[1:3]

## Theme
ptheme <- theme(legend.title = element_blank(),
  legend.text = element_text(size = 9),
  legend.key = element_rect(fill = "white"),
  axis.text = element_text(size = 9, color = "black"), #text for x og y axis
  axis.ticks.y = element_blank(),
  axis.line.x = element_line(size = 0.5),
  axis.line.y = element_blank(),
  axis.title.y = element_text(size = 11),
  axis.title.x = element_text(size = 11),
  panel.background = element_rect(fill = "white"),
  panel.border = element_rect(linetype = 1, fill = NA, color = "white"),
  panel.grid.minor.x = element_blank(),
  panel.grid.major.y = element_line(linetype = 2, color = "grey"))

miny <- round(min(dbaggr$hba, na.rm = TRUE), digits = 1)
maxy <- round(max(dbaggr$hba, na.rm = TRUE), digits = 1)
exty <- (maxy - miny) * .2

plotHbc <- ggplot() +
  ## linje skal ikke kuttes hvis det er NA
  geom_line(data = dbaggr[!is.na(hba), ],
    aes(yr, hba, group = sex, color = sex), size = 1) +
  geom_line(data = norgeTab, aes(yr, meanAll, color = "Norge"),
    linetype = "twodash", size = 1) +
  geom_point(data = dbaggr, aes(yr, hba, group = sex, shape = sex), size = 2) +
  geom_point(data = norgeTab, aes(yr, meanAll, shape = "Norge"), size = 2) +
  scale_x_continuous(breaks = unique(dbaggr$yr)) +
  scale_shape_manual(values = c(16, 1, 4, 18),
    breaks = c("Gutter", "Jenter", "Alle", "Norge"), ) +
  scale_color_manual(values = c(col3, "#FF55ee"), breaks = c("Gutter", "Jenter", "Alle", "Norge")) +
  scale_y_continuous(breaks = seq(miny, maxy, by = 0.2),
    limits = c(miny - exty, maxy + exty)) +
  ylab("HbA1c verdi i %") +
  xlab(" ") +
  ptheme


guttHb <- round(dbaggr[yr == 2018 & kjonn == 1, hba], digits = 1)
guttN <- round(dbaggr[yr == 2018 & kjonn == 1, n])
jenteHb <- round(dbaggr[yr == 2018 & kjonn == 2, hba], digits = 1)
jenteN <- round(dbaggr[yr == 2018 & kjonn == 2, n])
alleHb <- round(dbaggr[yr == 2018 & kjonn == 3, hba], digits = 1)
alleN <- round(dbaggr[yr == 2018 & kjonn == 3, n])
norgeHb <- round(norgeTab[yr == 2018, meanAll], digits = 1)
