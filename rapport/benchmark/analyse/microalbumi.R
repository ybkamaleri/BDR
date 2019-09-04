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


########################
## BESTILLING
########################

## micper <- dt1[get(varCount) == 'Ja', .(mic = lab_res_1prove, hosKort, Pnr, FNavn, ENavn, FDato)]
## micper[mic > 25, .N]
## micP <- micper[mic > 25, .(Pnr, FNavn, ENavn, FDato, hosKort, mic)]
## rio::export(micP, "persisterende_micro.xlsx")

## unRet <- "und_Retinopati"
## unRetV <- "und_Retinopati_valg"

## retino <- dt1[get(unRet) == "Ja", c(unRetV, "hosKort", "Pnr", "FDato", "FNavn", "ENavn"), with = FALSE]

## rio::export(retino, "retinopati.xlsx")

## dt1[, .N, by = und_periferneu]
## dt1[und_periferneu == "Ja", c("hosKort", "Pnr", "FDato", "FNavn", "ENavn"), with = FALSE]
