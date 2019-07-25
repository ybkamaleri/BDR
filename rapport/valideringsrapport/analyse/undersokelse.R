## undersøkelse utført
## -------------------

## Datakilde
undDT <- lok2018dt1

## variablene
undVar <- c("und_Oye", "und_laser", "und_Retinopati", "und_fotter", "und_periferneu", "und_infiltrater", "und_hudforandring", "lab_res_persmikro")

## testDT <- subset(undDT, select = undVar)

## ## Sjekk variabelinnholdt
## varList <- list()
## for (i in undVar){
##   .l2 <- undDT[, .N, by = .(get(i))]
##   varList[[i]] <- .l2
## }
## varList


## Count missing og non-missing
## -----------------------------
## undVarMiss <- undDT[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = undVar]
undVarNon <- undDT[, lapply(.SD, function(x) sum(!is.na(x))), .SDcols = undVar]

## Non missing long table
undNonLg <- melt(undVarNon,
  measure.vars = undVar,
  variable.name = "var",
  value.name = "n")


## Count som svarte Ja
## ----------------------
# library(sqldf)
# sqldf('SELECT COUNT(und_Oye) FROM testDT WHERE und_Oye = "Ja";')

mList <- list()

for (i in undVar){
  misInd <- undDT[get(i)=="Ja", .N]
  mList[[i]] <- misInd
}

undVarTall <- as.data.table(mList)

undVarLg <- melt(undVarTall,
  measure.vars = undVar,
  variable.name = "var",
  value.name = "ja")

## Merge table
undDD <- merge(undVarLg, undNonLg, by = "var")


## urin-albumin og mikroalbuminuri
## undDT[, .N, by=.(lab_res_persmikro)]

## urin-albumin er tall derfor er det å være missing eller ikke
urinNon <- undDT[!is.na(lab_res_1prove), .N]
urinDD <- undDT[, .(var = "lab_res_1prove", ja = urinNon, n = nrow(undDT))]

## bind all table
undAll <- rbindlist(list(undDD, urinDD), use.names = TRUE)

## Prosent
undAll[, pros := round(ja / n * 100, digits = 1)]

## Pyntting til tabellen
undAll[, id := .I]

nyNavn <- c("Øyeundersøkelse", "Laserbehandling", "Påvist retinopati",
  "Føtter", "Påvist klinisk perifer neuropati", "Påvist infiltrater",
  "Påvist hudforandringer", "Påvist Persisterende mikroalbuminuri",
  "Registrert verdi urin-albumin")

undAll[.(id = 1:9, to = nyNavn), on = "id", navn := i.to]

undAll[, pros2 := ifelse(ja == 0, paste0("-"), pros)]

undAll[, c("var", "pros", "id") := NULL]

setcolorder(undAll, c("navn", "ja", "pros2", "n"))

setnames(undAll, names(undAll),
  c(" ", "Antall utført/tilfelle", "Andel utført/tilfelle", "Antall registrerte"))


## Tabell hux
lastLine <- dim(undAll)[1] + 1

und.htab <- as_hux(undAll, add_colnames = TRUE)

und.htab <- und.htab %>%
  set_bottom_border(1,, TRUE) %>%
  set_bold(1,, TRUE) %>%
  set_bottom_border(lastLine,, TRUE) %>%
  map_background_color(by_rows("grey95", "white")) %>%
  set_position("left") %>%
  set_align(, 3, "right") %>%
  set_latex_float("h!")

col_width(und.htab) <- c(.4, .2, .2, .2)
width(und.htab) <- 0.9
wrap(und.htab) <- TRUE
