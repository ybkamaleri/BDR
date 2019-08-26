## Rådata for HbA1c
valgVar <- "lab_HbA1cAkerVerdi"

rawtab <- rollup(dt1,
  j = list(
    mean1 = mean(get(valgVar), na.rm = TRUE),
    median1 = median(get(valgVar), na.rm = TRUE)
  ),
  by = "hosKort"
)


rawtab[is.na(hosKort), hosKort := "Totalt"]

rawtab[,  `:=`(
  mean = sprintf("%0.2f", mean1),
  median = sprintf("%0.2f", median1)
) ]

delColRaw <- grep("1$", names(rawtab))
rawtab[, (delColRaw) := NULL]

## Totalt
hosTab <- cube(dt1,
  j = list(
    n = .N,
    und7 = sum(get(valgVar) < 7.5, na.rm = TRUE),
    und8 = sum(get(valgVar) < 8, na.rm = TRUE),
    und9 = sum(get(valgVar) >= 9, na.rm = TRUE),
    und10 = sum(get(valgVar) >= 10, na.rm = TRUE)
  ),
  by = "hosKort")

hosTab[, `:=`(
  pro7 = und7 / n * 100,
  pro8 = und8 / n * 100,
  pro9 = und9 / n * 100,
  pro10 = und10 / n * 100
), by = hosKort]


hosTab[, `:=`(
  u7 = sprintf("%0.1f (%s)", pro7, und7),
  u8 = sprintf("%0.1f (%s)", pro8, und8),
  m9 = sprintf("%0.1f (%s)", pro9, und9),
  m10 = sprintf("%0.1f (%s)", pro10, und10)
)]

delCol1 <- grep("^und", names(hosTab), value = TRUE)
delCol2 <- grep("^pro", names(hosTab), value = TRUE)
delCol <- c("n", delCol1, delCol2)

hosTab[, (delCol) := NULL]
hosTab[is.na(hosKort), hosKort := "Totalt"]

## Mix tabell
tabRaw <- rawtab[hosTab, on = "hosKort"]

## git navn
tabRaw[hosKort == "Totalt", hosKort := "Hele landet"]
nyNavn <- c("", "Gj.snitt", "Median", "<7.5", "<8.0", ">=9.0", ">=10")
setnames(tabRaw, names(tabRaw), nyNavn)


## Tabell
tabOut <- exp.tabel(tabRaw,
  "HbA1c : %(n)", ncol = 7,
  size = 0.9, total = 1, valgCol = 2:7, valgAlign = "left",
  rowHeight = .03, mixCol = 2:7)



## ## per sykehus
## hostab <- dt1[, {
##   n = .N;
##   und7 = sum(get(valgVar) < 7.5, na.rm = TRUE);
##   und8 = sum(get(valgVar) < 8, na.rm = TRUE);
##   und9 = sum(get(valgVar) >= 9, na.rm = TRUE);
##   und10 = sum(get(valgVar) >= 10, na.rm = TRUE);
##   pro7 = und7 / n * 100;
##   pro8 = und8 / n * 100;
##   pro9 = und9 / n * 100;
##   pro10 = und10 / n * 100;
##   list(
##     n = n,
##     u7 = und7,
##     u8 = und8,
##     m9 = und9,
##     m10 = und10,
##     p7 = pro7,
##     p8 = pro8,
##     p9 = pro9,
##     p10 = pro10)},
##   by = hosKort]
