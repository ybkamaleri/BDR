## Kun DT1 pasienter
## -----------------
## Datasettet som skal brukes er dt1
## for bare lokal sykehus bruker lokal2018

## datakilde
dtDiab1 <- lok2018dt1

## Pasient karakteristika

varKar <- c("PasientID", "alder", "diagAlder", "diagVar", "bmi")

## dtKar <- subset(dt1, select = varKar)
dtKar <- subset(dtDiab1, select = varKar)

karLong <- melt(dtKar, id.vars = "PasientID", variable.name = "chr", value.name = "tall")

## Antall
karTall <- karLong[!is.na(tall), .N, by = chr]

## Tabell
karTab <- karLong[, list(mean = mean(tall, na.rm = T),
               median = median(tall, na.rm = T),
               min = min(tall, na.rm = T),
               maks = max(tall, na.rm = T)), by = .(chr)]

kar.tab <- karTall[karTab, on = "chr"]

karNavn <- names(kar.tab)[3:6]

for (i in karNavn){
  kar.tab[, (i) := round(get(i), digits = 1)]
}

karOld <- varKar[-1]
karNy <- c("Alder (år)", "Alder ved diagnose (år)", "Sykdomsvarighet (år)", "BMI")
kar.tab[.(chr = karOld, to = karNy), on = "chr", chr := i.to]

setnames(kar.tab, c("chr", "N", "mean"), c(" ", "n", "gj.snitt"))
## kar.tab[]
