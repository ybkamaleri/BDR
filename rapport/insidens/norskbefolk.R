library(data.table)
norskBF <- fread("N2018_Norge_1_18.csv", header = TRUE, encoding = "Latin-1")

library(stringr)
## Extract Alder
norskBF[, age := stringr::str_extract(alder, "\\d+")]
## Extract tall for fylke med gsub
norskBF[, fylke := as.numeric(gsub("([0-9]+).*$", "\\1", region))]

## Kjonn
norskBF[.(kjønn = c("Menn", "Kvinner"), to = 1:2), on = "kjønn", kjonn := i.to]

norskBF
saveRDS(norskBF, "norskbefolkingUnd19.Rds")
