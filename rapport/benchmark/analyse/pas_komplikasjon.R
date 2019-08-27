## Akutt komplikasjoner

## DKA og Insulinsjokkj
subDT1 <- rollup(dt1,
  j = list(
    n_dka = sum(!is.na(und_ketoacidose == 'Ja')),
    j_dka = sum(und_ketoacidose == 'Ja', na.rm = TRUE),
    n_ins = sum(!is.na(und_inssjokk == 'Ja')),
    j_ins = sum(und_inssjokk == 'Ja', na.rm = TRUE)
  ),
  by = "hosKort")


## Føling for bare elder enn 5 år
subDT2 <- rollup(dt1[agekat != 1, ],
  j = list(
    n_fol = sum(!is.na(und_foling == 'Ja')),
    j_fol = sum(und_foling == 'Ja', na.rm = TRUE)
  ),
  by = "hosKort")

dtAll <- subDT1[subDT2, on = "hosKort"]
dtAll[is.na(hosKort), hosKort := "Hele landet"]

tabRaw <- dtAll[, {
  dka1 = j_dka / n_dka * 100;
  ins1 = j_ins / n_ins * 100;
  fol1 = j_fol / n_fol * 100;
  dka = sprintf("%0.1f (%s)", dka1, n_dka);
  ins = sprintf("%0.1f (%s)", ins1, n_ins);
  fol = sprintf("%0.1f (%s)", fol1, n_fol);
  list(
    hosp = hosKort,
    dka = dka,
    ins = ins,
    fol = fol
  )
}]

giNavn <- c("", "Ketoacidose", "Insulinsjokk", "Føling")
setnames(tabRaw, names(tabRaw), giNavn)

tabOut <- exp.tabel(tabRaw, "Prosent (Antall)", ncol = 4, size = 0.9, total = 1, rowHeight = 0.015, mixCol = 2:4)

## quick_pdf(tabOut, file = "test.pdf")
