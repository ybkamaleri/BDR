## Tabell 7 - Lipider
## ------------------

## Kilderdata
dt <- ars2018

## Total kolesterol. Bruk ikke fastende, hvis missing bruk fastende
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


lipTab <- data.table::cube(dt, j = .(tkl = length(which(tkl2 >= 5)),
  hdl = length(which(hdl2 < 1)),
  ldl = length(which(ldl2 >= 2.6000)),
  ldl02 = length(which(ldl2 > 3)),
  trig = length(which(trig2 > 2.5))),
  by = "Kjonn")

## Total non mising
lipVar <- c("tkl2", "hdl2", "ldl2", "trig2")
lipTot <- dt[, lapply(.SD, function(x) sum(!is.na(x))), .SDcols = lipVar]

## legger til Total
lipTab[, `:=`(tklN = lipTot$tkl2,
  hdlN = lipTot$hdl2,
  ldlN = lipTot$ldl2,
  trigN = lipTot$trig2)]

## Prosent
lipTab[, `:=`(tkl_P_ = tkl / tklN * 100,
  hdl_P_ = hdl / hdlN * 100,
  ldl_P_ = ldl / ldlN * 100,
  ldl02_P_ = ldl02 / ldlN * 100,
  trig_P_ = trig / trigN * 100)]

lipVar2 <- grep("_P_$", names(lipTab), value = TRUE)
lipTab[, (lipVar2) := round(.SD, digits = 1), .SDcols = lipVar2]

lipTab[is.na(Kjonn), Kjonn  := "Totalt"]

## ubrukte variabler
lipVarbort <- grep("N$", names(lipTab), value = TRUE)
valgTab <- lipTab[, -..lipVarbort] #tar bort dem

## kombinere prosent og beholder 0 decimal
valgTab[, `:=`(tklF = sprintf("%s (%0.1f %%)", tkl, tkl_P_),
  hdlF = sprintf("%s (%0.1f %%)", hdl, hdl_P_),
  ldlF = sprintf("%s (%0.1f %%)", ldl, ldl_P_),
  ldl02F = sprintf("%s (%0.1f %%)", ldl02, ldl02_P_),
  trigF = sprintf("%s (%0.1f %%)", trig, trig_P_))]

## valg variabler for tabell
lipVarTab <- grep("F$", names(valgTab), value = TRUE)
lipTabell <- valgTab[, c("Kjonn", lipVarTab), with = F]

## Gir ny colnames
tabNavn <- c("Total", "HDL", "LDL 2", "LDL 3", "Triglycerider")
setnames(lipTabell, names(lipTabell)[-1], tabNavn)







lipLg <- melt(lipTab, measure.vars = c("tkl", "hdl", "ldl", "trig"))











## Grand Total og sub total
cube(dt, sum(!is.na(ldl2)), by = "Kjonn")
rollup(dt, j = lapply(.SD, function(x) sum(!is.na(x))),
  by = c("Kjonn"), .SDcols = c("tkl2", "ldl2"), id = FALSE)
cube(dt, j = lapply(.SD, function(x) sum(!is.na(x))),
  by = c("Kjonn"), .SDcols = c("tkl2", "ldl2"), id = FALSE)
