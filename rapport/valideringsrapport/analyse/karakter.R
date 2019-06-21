## Kun DT1 pasienter
## -----------------

## Pasient karakteristika

varKar <- c("PasientID", "alder", "diagAlder", "diagVar", "bmi")
## lokal sykehus
dtKar <- subset(dt1, select = varKar)

karLong <- melt(dtKar, id.vars = "PasientID", variable.name = "chr", value.name = "tall")

## Antall
karTall <- karLong[!is.na(tall), .N, by = chr]

## Tabell
karTab <- karLong[, list(
  mean = mean(tall, na.rm = T),
  median = median(tall, na.rm = T),
  min = min(tall, na.rm = T),
  max = max(tall, na.rm = T)), by = .(chr)]

kar.tab <- karTall[karTab, on = "chr"]

karNavn <- names(kar.tab)[3:6]

for (i in karNavn){
  kar.tab[, (i) := round(get(i), digits = 1)]
}

## kar.tab[]
