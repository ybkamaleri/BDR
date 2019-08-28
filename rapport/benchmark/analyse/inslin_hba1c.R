## Insulinsjokk og HbA1c nivå
hbaVar = "lab_HbA1cAkerVerdi"
ins7 <- rollup(dt1[lab_HbA1cAkerVerdi < 7.0, ],
  j = list(
    n_7 = sum(!is.na(und_inssjokk == 'Ja')),
    j_7 = sum(und_inssjokk == 'Ja', na.rm = TRUE)
  ),
  by = "hosKort")

ins75 <- rollup(dt1[lab_HbA1cAkerVerdi < 7.5, ],
  j = list(
    n_75 = sum(!is.na(und_inssjokk == 'Ja')),
    j_75 = sum(und_inssjokk == 'Ja', na.rm = TRUE)
  ),
  by = "hosKort")

insHba <- ins7[ins75, on = "hosKort"]

insHba[is.na(hosKort), hosKort := "Hele landet"]

tabRaw <- insHba[, {
  p7 = j_7 / n_7 * 100;
  p75 = j_75 / n_75 * 100;
  n7 = sprintf("%s (%0.1f)", j_7, p7);
  n75 = sprintf("%s (%0.1f)", j_75, p75);
  list(
    hosKort = hosKort,
    n7 = n7,
    t7 = n_7,
    n75 = n75,
    t75 = n_75
  )
}]


for (j in seq_len(ncol(tabRaw))){
  set(tabRaw, which(tabRaw[[j]] == '0 (0.0)'), j = j, value = " -")
}

tabNavn <- c("", "n (%)", "Total N", "n (%)", "Total N")
setnames(tabRaw, names(tabRaw), tabNavn)

tabOut <- exp.tabel(tabRaw,
  xcol = c("", "HbA1c < 7.0%", "", "HbA1c < 7.5%", ""),
  ncol = 5, del = c(.2, .15, .1, .15, .1), valgCol = c(2, 4), valgAlign = "right",
  size = 0.9, total = 1, rowHeight = .015)

tomCol <- rep(" ", nrow(tabOut))
tabOut <- insert_column(tabOut, tomCol, after = 3)
tabOut <- tabOut %>%
  set_bottom_border(1, 2:3, 0.4) %>%
  set_bottom_border(1, 6:7, 0.4)

## quick_pdf(tabOut, file = "test.pdf")
