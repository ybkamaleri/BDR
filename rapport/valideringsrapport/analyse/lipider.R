## Tabell 7 - Lipider
## ------------------

## Test
## dt <- ars2018dt1

## Kilderdata
dt <- lok2018dt1

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


lipTab <- data.table::cube(dt, j = .(tkl = length(which(tkl2 >= 5)),
  hdl = length(which(hdl2 < 1)),
  ldl = length(which(ldl2 >= 2.6000)),
  ldl02 = length(which(ldl2 > 3)),
  trig = length(which(trig2 > 2.5))),
  by = "Kjonn")

## Total non mising
lipVar <- c("tkl2", "hdl2", "ldl2", "trig2")
lipTot <- dt[, lapply(.SD, function(x) sum(!is.na(x))), .SDcols = lipVar]

## total på kjønn
totKjonn <- dt[, lapply(.SD, function(x) sum(!is.na(x))), .SDcols = lipVar, by = Kjonn]


## legger til Total
lipTab[, `:=`(
  tklN = lipTot$tkl2,
  hdlN = lipTot$hdl2,
  ldlN = lipTot$ldl2,
  trigN = lipTot$trig2
)]

## Legger til Total per kjønn
lipTab[Kjonn == "Gutt",
  `:=`(
    tklN = totKjonn$tkl2[totKjonn$Kjonn == "Gutt"],
    hdlN = totKjonn$hdl2[totKjonn$Kjonn == "Gutt"],
    ldlN = totKjonn$ldl2[totKjonn$Kjonn == "Gutt"],
    trigN = totKjonn$trig2[totKjonn$Kjonn == "Gutt"]
  )]

lipTab[Kjonn == "Jente",
  `:=`(
    tklN = totKjonn$tkl2[totKjonn$Kjonn == "Jente"],
    hdlN = totKjonn$hdl2[totKjonn$Kjonn == "Jente"],
    ldlN = totKjonn$ldl2[totKjonn$Kjonn == "Jente"],
    trigN = totKjonn$trig2[totKjonn$Kjonn == "Jente"]
  )]



## Prosent
lipTab[, `:=`(tkl_P_ = tkl / tklN * 100,
  hdl_P_ = hdl / hdlN * 100,
  ldl_P_ = ldl / ldlN * 100,
  ldl02_P_ = ldl02 / ldlN * 100,
  trig_P_ = trig / trigN * 100)]

lipVar2 <- grep("_P_$", names(lipTab), value = TRUE)
lipTab[, (lipVar2) := round(.SD, digits = 1), .SDcols = lipVar2]

## sortere rad hvor Gutter er første rader som alle andre tabeller i dokumenten
lipTab <- lipTab[order(Kjonn, na.last = TRUE)]
lipTab[is.na(Kjonn), Kjonn  := "Totalt"]
lipTab[.(Kjonn = c("Gutt", "Jente"), to = c("Gutter", "Jenter")), on = "Kjonn", Kjonn := i.to]

## ubrukte variabler
lipVarbort <- grep("N$", names(lipTab), value = TRUE)
valgTab <- lipTab[, -..lipVarbort] #tar bort dem

## kombinere prosent og beholder 0 decimal
valgTab[, `:=`(tklF = sprintf("%s (%0.1f %%)", tkl, tkl_P_),
  hdlF = sprintf("%s (%0.1f %%)", hdl, hdl_P_),
  ldlF = sprintf("%s (%0.1f %%)", ldl, ldl_P_),
  ldl02F = sprintf("%s (%0.1f %%)", ldl02, ldl02_P_),
  trigF = sprintf("%s (%0.1f %%)", trig, trig_P_))]


## Bytt 0 i antall til "-"
valgTab[tkl == 0, tklF := " -  "]
valgTab[hdl == 0, hdlF := " -  "]
valgTab[ldl == 0, ldlF := " -  "]
valgTab[ldl02 == 0, ldl02F := " -  "]
valgTab[trig == 0, trigF := " -  "]

## valg variabler for tabell
lipVarTab <- grep("F$", names(valgTab), value = TRUE)
lipTabell <- valgTab[, c("Kjonn", lipVarTab), with = F]

## Gir ny colnames
tabNavn <- c(" ", "Total kol.", "HDL", "LDL 2", "LDL 3", "Triglycerider")
setnames(lipTabell, names(lipTabell), tabNavn)



## Tabell
lip.htab <- as_hux(lipTabell, add_colnames = TRUE)

tabSub <- c(" ", ">= 5 mmol/l", "< 1 mmol/l", ">= 2.6 mmol/l", "> 3 mmol/l", "> 2.5 mmol/l")
lip.htab <- rbind(lip.htab[1, ], tabSub, lip.htab[2:nrow(lip.htab), ])

lastLine <- nrow(lip.htab)

lip.htab <- lip.htab %>%
  set_bold(1,, TRUE) %>%
  set_bold(lastLine,, TRUE) %>%
  ## set_bottom_border(1,, TRUE) %>%
  set_top_border(3,, TRUE) %>%
  set_top_border(lastLine,, TRUE) %>%
  set_align(, 2:5, "right") %>%
  set_align(1:2,, "left") %>%
  map_background_color(by_rows("grey95", "white")) %>%
  set_position("left") %>%
  set_latex_float("h") %>%
  set_col_width(value = c(2.5, 1.5, 1.5, 1.5, 1.5, 1.5)) %>%
  set_width(0.8)

lip.htab
