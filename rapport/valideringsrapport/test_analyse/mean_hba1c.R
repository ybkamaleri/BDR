## Gjennomsnitt hba1c målt sentralt
## --------------------------------

## ## Datakilder
## ## gamle data "bdrB4" uploaded fra runEnkelRapport
## bdrold <- subset(bdrB4, select = c("hba1c", "yr", "hospid", "age", "gender"))
## bdrold

## ## Lokal data
## dtlokal <- ars2018

## valgVar <- c("lab_HbA1cAkerVerdi", "Kjonn", "yr", "hospID", "alder")
## dtlokal[, str(.SD), .SDcols = valgVar]

## valgDT <- subset(dtlokal, select = valgVar)
## valgDT[.(Kjonn = c("Gutt", "Jente"), to = 1:2), on = "Kjonn", gender := i.to]
## valgDT[, Kjonn := NULL]

## ## gir standard navn
## stdNavn <- c("hba1c", "yr", "hospid", "alder", "kjonn")
## setnames(valgDT, names(valgDT), stdNavn)
## setnames(bdrold, names(bdrold), stdNavn)

## ## Merge begge
## alldt <- rbindlist(list(bdrold, valgDT), use.names = TRUE)

## ## Lage file til raskere upload
## saveRDS(alldt, file.path(dataSti, "all07til18.rds"))


## upload data
alldt <- readRDS(file.path(dataSti, "all07til18.rds"))

## subset fra loop data
dtvalg <- subset(alldt, hospid == hosp)

## gjennomsnitt hba1c
dtvalg[, meanhbc  := round(mean(hba1c, na.rm = TRUE), digits = 1), by = yr]

## antall per år
dtvalg[, N := .N, by = yr]
dtvalg[, .N, by = yr]

dtplot <- dtvalg[dtvalg[, .I[1], by = yr]$V1]
## dtplot <- dtvalg[, head(.SD, 1), yr] #alternativ med litt treg

dbaggr <- rollup(dtvalg, j = .(hba = mean(hba1c, na.rm = TRUE),
  n = .N), by = c("yr", "kjonn"))

## legger totallen dvs. 3. Kjønn er søm flgende:
## 1 = Gutter
## 2 = Jenter
## 3 = Totalt
dbaggr[is.na(kjonn), kjonn := 3]

## tar bort Grand Total
dbaggr <- dbaggr[!is.na(yr), ]

## konverterer yr til nummeric ellers må bruke group=1
dbaggr[, yr := as.numeric(yr)]

## Plotting
ggplot(dbaggr, aes(yr, hba, group = kjonn)) +
  geom_line(aes(color = factor(kjonn))) +
  geom_point(aes(shape = factor(kjonn)), stroke = 0) +
  scale_x_continuous(breaks = unique(dbaggr$yr)) +
  labs(title = "Gjennomsnitt HbA1c", x = " ", y = "HbA1c verdi i %")
