## Lipider
## Kilderdata
dt <- dt1

## OBS! Bruk ikke fastende, hvis missing bruk fastende
## Total kolesterol.
dt[, tkl2 := lab_lip_totkol_2] %>%
  .[is.na(lab_lip_totkol_2), tkl2  := lab_lip_totkol]

## HDL
dt[, hdl2 := lab_lip_HDL_2] %>%
  .[is.na(lab_lip_HDL_2), hdl2 := lab_lip_HDL]

## LDL
dt[, ldl2 := lab_lip_LDL_2] %>%
  .[is.na(lab_lip_LDL_2), ldl2 := lab_lip_LDL]

## Triglyserider
dt[, trig2 := lab_lip_trig_2] %>%
  .[is.na(lab_lip_trig_2), trig2 := lab_lip_trig]


lipTab <- rollup(dt,
  j = list(
    tkl = length(which(tkl2 >= 5)),
    hdl = length(which(hdl2 < 1)),
    ldl = length(which(ldl2 >= 2.6000)),
    ldl02 = length(which(ldl2 > 3)),
    trig = length(which(trig2 > 2.5)),
    tklN = sum(!is.na(tkl2)),
    hdlN = sum(!is.na(hdl2)),
    ldlN = sum(!is.na(ldl2)),
    trigN = sum(!is.na(trig2))
  ),
  by = "hosKort")

lipTab[is.na(hosKort), hosKort := "Hele landet"]


tabRaw <- lipTab[, {
  tkl_P = tkl / tklN * 100;
  hdl_P = hdl / hdlN * 100;
  ldl_P = ldl / ldlN * 100;
  ldl02_P = ldl02 / ldlN * 100;
  trig_P = trig / trigN * 100;
  u1 = sprintf("%s (%0.1f)", tkl, tkl_P);
  u2 = sprintf("%s (%0.1f)", hdl, hdl_P);
  u3 = sprintf("%s (%0.1f)", ldl, ldl_P);
  u4 = sprintf("%s (%0.1f)", ldl02, ldl02_P);
  u5 = sprintf("%s (%0.1f)", trig, trig_P);
  list(
    hosKort,
    tkl = u1,
    hdl = u2,
    ldl = u3,
    ldl2 = u4,
    trig = u5
  )
}]



for (j in seq_len(ncol(tabRaw))){
  set(tabRaw, which(tabRaw[[j]] == '0 (0.0)'), j = j, value = " -")
}


## Gir ny colnames
tabSub <- c(" ", ">= 5 mmol/l", "< 1 mmol/l", ">= 2.6 mmol/l", "> 3 mmol/l", "> 2.5 mmol/l")
tabNavn <- c(" ", "Total kol.", "HDL", "LDL 2", "LDL 3", "Triglycerider")
setnames(tabRaw, names(tabRaw), tabSub)

tabOut <- exp.tabel(tabRaw, xcol = tabNavn,
  size = 0.9, total = 1, rowHeight = .01,
  del = c(2.5, 1.5, 1.5, 1.5, 1.5, 1.5),
  valgCol = 2:6, valgAlign = "right")

extNavn <- c("", "Antall og andel pasienter: n (%)", "", "", "", "")
tabOut <- rbind(extNavn, tabOut)
tabOut <- tabOut %>%
  merge_cells(1, 2:6) %>%
  set_bottom_border(1,2:6, 0.3)


## quick_pdf(tabOut, file = "test.pdf")
