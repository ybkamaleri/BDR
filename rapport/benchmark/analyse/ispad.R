## ISPAD guideline for øye og urin

dt1[, ispad := 0] %>%
  .[alder >= 10 & diagVar >= 2, ispad  := 1] %>%
  .[alder < 10 & diagVar >= 5, ispad := 1]

## Antall kvalifisert til ISPAD definisjon hele landet
ispadN <- dt1[ispad == 1, .N]


ispDT <- rollup(dt1[ispad == 1, ],
  j = .(
    neye = sum(und_Oye == 'Ja', na.rm = TRUE),
    nurin = sum(!is.na(lab_res_1prove)),
    ntot = .N
  ), by = "hosKort"
  )

ispDT[is.na(hosKort), hosKort := "Hele landet"]

tabRaw <- ispDT[, {
  peye = neye / ntot * 100;
  ueye = sprintf("%s (%0.1f)", neye, peye);
  purin = nurin / ntot * 100;
  uurin = sprintf("%s (%0.1f)", nurin, purin);
  list(
    hosKort = hosKort,
    eye = ueye,
    urin = uurin,
    tot = ntot
  )
}]

nyNavn <- c("", "Øye", "Urin", "Totalt")
setnames(tabRaw, names(tabRaw), nyNavn)

tabOut <- exp.tabel(tabRaw, "Undersøkelse utført: n(%)", ncol = 4, size = 0.9, total = 2, rowHeight = 0.015, mixCol = 2:3, valgCol = 2:3, valgAlign = "right")

## quick_pdf(tabOut, file = "test.pdf")
