## Føling for bare elder enn 5 år
dtsub <- subset(dt1, agekat != 1)
katdt <- cube(dtsub,
  j = list(
    n_dka = sum(!is.na(und_foling == 'Ja')),
    j_dka = sum(und_foling == 'Ja', na.rm = TRUE)
  ),
  by = c("hosKort", "agekat"))


## katdt <- cube(dt1,
##   j = list(
##     n_dka = sum(!is.na(und_ketoacidose == 'Ja')),
##     j_dka = sum(und_ketoacidose == 'Ja', na.rm = TRUE)
##   ),
##   by = c("hosKort", "agekat"))

Nkat <- dcast(katdt,  hosKort ~ agekat, value.var = "n_dka")
setorder(Nkat, hosKort, na.last = TRUE) #legge NA siste rad
Nkat[is.na(hosKort), hosKort := "Hele landet"]
extNavn <- paste0("N", names(Nkat)[3:5])
giNavn <- c("hosp", "N5", extNavn)
setnames(Nkat, names(Nkat), giNavn)

nkat <- dcast(katdt,  hosKort ~ agekat, value.var = "j_dka")
setorder(nkat, hosKort, na.last = TRUE) #legge NA siste rad
nkat[is.na(hosKort), hosKort := "Hele landet"]
extNavn <- paste0("n", names(nkat)[3:5])
giNavn <- c("hosp", "n5", extNavn)
setnames(nkat, names(nkat), giNavn)

katAll <- Nkat[nkat, on = "hosp"]

tabRaw <- katAll[, {
  ## age1 = n1 / N1 * 100;
  age2 = n2 / N2 * 100;
  age3 = n3 / N3 * 100;
  age4 = n4 / N4 * 100;
  age5 = n5 / N5 * 100;
  ## u1 = sprintf("%0.1f (%s)", age1, n1);
  u2 = sprintf("%0.1f (%s)", age2, n2);
  u3 = sprintf("%0.1f (%s)", age3, n3);
  u4 = sprintf("%0.1f (%s)", age4, n4);
  u5 = sprintf("%0.1f (%s)", age5, n5);
  list(
    hosp = hosp,
    ## u1 = u1,
    u2 = u2,
    u3 = u3,
    u4 = u4,
    N = n5
  )
}]

for (j in seq_len(ncol(tabRaw))){
  set(tabRaw, which(tabRaw[[j]] == 'NA (NA)'), j = j, value = " -")
}

for (j in seq_len(ncol(tabRaw))){
  set(tabRaw, which(tabRaw[[j]] == 'NA (0)'), j = j, value = " -")
}

for (j in seq_len(ncol(tabRaw))){
  set(tabRaw, which(tabRaw[[j]] == '0.0 (0)'), j = j, value = " -")
}

tabNavn <- c("", "5-9 år", "10-14 år", ">14 år", "Antall")
setnames(tabRaw, names(tabRaw), tabNavn)

tabRaw[, Antall := as.character(Antall)]

tabOut <- exp.tabel(tabRaw, "Aldersgrupper", ncol = 5,
  size = 0.9, total = 2, rowHeight = .015, mixCol = 2:4, valgCol = 5, valgAlign = "center")
