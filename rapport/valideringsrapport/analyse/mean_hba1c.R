## Gjennomsnitt hba1c målt sentralt
## --------------------------------

## ## Datakilder
## gamle data "bdrB4" uploaded fra runEnkelRapport
bdrold <- subset(bdrB4, select = c("hba1c", "yr", "hospid", "age", "gender"))
bdrold

## Lokal data
dtlokal <- lok2018dt1

valgVar <- c("lab_HbA1cAkerVerdi", "Kjonn", "yr", "hospID", "alder")
## dtlokal[, str(.SD), .SDcols = valgVar]

valgDT <- subset(dtlokal, select = valgVar)
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


## subset fra loop data
dtvalg <- subset(alldt, hospid == hosp)

## gjennomsnitt hba1c
dtvalg[, meanhbc  := round(mean(hba1c, na.rm = TRUE), digits = 1), by = yr]

## antall per år
dtvalg[, N := .N, by = yr]
## dtvalg[, .N, by = yr]

dtplot <- dtvalg[dtvalg[, .I[1], by = yr]$V1]
## dtplot <- dtvalg[, head(.SD, 1), yr] #alternativ med litt treg

dbaggr <- rollup(dtvalg, j = .(hba = mean(hba1c, na.rm = TRUE),
  n = .N), by = c("yr", "kjonn"))

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

miny <- round(min(dbaggr$hba), digits = 1)
maxy <- round(max(dbaggr$hba), digits = 1)
exty <- (maxy - miny) * .2

plotHbc <- ggplot(dbaggr, aes(yr, hba, group = sex)) +
  geom_line(aes(color = sex), size = 1) +
  geom_point(aes(shape = sex), size = 3.5) +
  scale_x_continuous(breaks = unique(dbaggr$yr)) +
  scale_shape_manual(values = c(17, 1, 16), breaks = c("Gutter", "Jenter", "Alle")) +
  scale_color_manual(values = col3, breaks = c("Gutter", "Jenter", "Alle")) +
  scale_y_continuous(breaks = seq(miny, maxy, by = 0.2),
    limits = c(miny - exty, maxy + exty)) +
  ylab( "HbA1c verdi i %") +
  ptheme
