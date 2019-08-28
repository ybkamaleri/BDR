## Persisterende microalbuminuri

varCount <- "lab_res_persmikro"

longDT <- rollup(dt1,
  j = list(
    N = sum(!is.na(get(varCount))),
    n = sum(get(varCount) == 'Ja', na.rm = TRUE)
  ),
  by = c("hosKort"))

longDT[is.na(hosKort), hosKort := "Hele landet"]

tabRaw <- longDT[, {
  pros = n / N * 100;
  ut = sprintf("%s (%0.1f)", n, pros);
  list(
    hosKort = hosKort,
    ut = ut,
    N = N
  )
}]

for (j in seq_len(ncol(tabRaw))){
  set(tabRaw, which(tabRaw[[j]] == '0 (0.0)'), j = j, value = " -")
}

tabNavn <- c("", "n (%)", "N")
setnames(tabRaw, names(tabRaw), tabNavn)

tabOut <- exp.tabel(tabRaw, xcol = c("", " ", ""), total = 2, valgCol = 2, valgAlign = "right", rowHeight = 0.025)

## quick_pdf(tabOut, file = "test.pdf")
