## ISPAD guidelines
## ----------------

## Datakilde
ispDT <- lok2018dt1

ispDT[, ispad := 0] %>%
  .[alder >= 10 & diagVar >= 5, ispad  := 1]
## .[alder < 10 & diagVar >= 5, ispad := 1]

## Antall kvalifisert til ISPAD definisjon
ispadN <- ispDT[ispad == 1, .N]

## Øye undersøkelse
oye <- ispDT[ispad == 1 & und_Oye == "Ja", .(var = "oye", ja = .N)]
ispDT[ispad == 1, .N, by=.(und_Oye)] #sjekk tallene er riktig

## Urin
urin.isp <- ispDT[ispad == 1, sum(!is.na(lab_res_1prove))] #utført
## ispDT[ispad == 1, sum(is.na(lab_res_1prove))]
urin <- data.table(var = "urin", ja = urin.isp)

## ispad tabell
ispTab <- rbindlist(list(oye, urin))
ispTab[, tot := ispadN]
ispTab[, pros := round(ja / tot * 100, digits = 1)]

ispTab[.(var = c("oye", "urin"), to = c("Øye", "Urin")), on = "var", var := i.to]
setcolorder(ispTab, c("var", "ja", "pros", "tot"))
setnames(ispTab, names(ispTab), c("Undersøkelser", "Antall", "Andel", "N"))

## lage tabell
lastLine <- nrow(ispTab) + 1
isp.htab <- as_hux(ispTab, add_colnames = TRUE)

isp.htab <- isp.htab %>%
  set_bold(1, everywhere, TRUE) %>%
  set_bottom_border(lastLine,, TRUE) %>%
  map_background_color(by_rows("grey95", "white")) %>%
  set_position("left") %>%
  set_latex_float("h") %>%
  set_col_width(c(.4, .15, .15, .2))

isp.htab <- rbind(c("Type", "Undersøkelser utført", "", ""), isp.htab)

isp.htab <- merge_cells(isp.htab, 1, 2:3)
align(isp.htab)[1, 2] <- "center"
bottom_border(isp.htab)[1, 2] <- 0.5
top_border(isp.htab)[3, ] <- TRUE
