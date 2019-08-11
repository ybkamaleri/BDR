## Andre sykedommer

lokalDT <- lok2018dt1

undVar <- c("und_syk_col", "und_syk_hypo", "und_syk_hype", "und_syk_adhd",
  "und_syk_down", "und_syk_epi", "und_syk_cys")

## ## Andre sykedommer må omkode til Ja og Nei pga. text
## andre <- "und_syk_annet"
## lokalDT[, sykannet := und_syk_annet] %>%
##   .[!is.na(und_syk_annet), sykannet := "Ja"]

## ## lokalDT[, str(.SD), .SDcols = valgVar]

## ## Ny liste for valgte variabel inkludert sykannet
## undVar <- c(valgVar, "sykannet")


## Antall som ikke er missing
undVarNon <- lokalDT[, lapply(.SD, function(x) sum(!is.na(x))), .SDcols = undVar]
undNonLg <- melt(undVarNon,
  measure.vars = undVar,
  variable.name = "var",
  value.name = "n")

## ## For annen sykedommer så skal andelen sammenligne mot total pasienter
## antallAlle <- as.integer(nrow(lokalDT))
## ## attr(undNonLg$var, "ATT") <- NULL
## undNonLg[, var := as.character(var, stringAsFactor = FALSE)] #convert factor to string
## undNonLg[var %like% "sykannet", n  := .(antallAlle)]

## Antal som svarte Ja
mList <- list()

for (i in undVar){
  misInd <- lokalDT[get(i)=="Ja", .N]
  mList[[i]] <- misInd
}

undVarTall <- as.data.table(mList)

undVarLg <- melt(undVarTall,
  measure.vars = undVar,
  variable.name = "var",
  value.name = "ja")

## Merge table
undAll <- merge(undVarLg, undNonLg, by = "var")

## Prosent
undAll[, pros := round(ja / n * 100, digits = 1)]

## Pyntting til tabellen
undAll[, id := .I]

nyNavn <- c(
  "Cøliaki",
  "Hypothyreose",
  "Hyperthyreose",
  "ADHD",
  "Down syndrom",
  "Epilepsi",
  "Cystisk fibrose"
  )

undAll[.(id = 1:8, to = nyNavn), on = "id", navn := i.to]

## Beholder 1 decimal selv om 0
undAll[, pro := as.character(sprintf("%0.1f", pros))]
## Hvis 0 så blir bare strek
undAll[pros == 0, pro := "- "]
## Bort ubrukte kollone
undAll[, c("var", "pros", "id") := NULL]
## Reorder
setcolorder(undAll, c("navn", "ja", "pro", "n"))


setnames(undAll, names(undAll),
  c(" ", "Antall", "Andel", "N"))


## ## Tabell hux
## lastLine <- dim(undAll)[1] + 1

## und.htab <- as_hux(undAll, add_colnames = TRUE)

## und.htab <- und.htab %>%
##   ## set_bottom_border(1,, TRUE) %>%
##   set_bold(1,, TRUE) %>%
##   set_bottom_border(lastLine,, TRUE) %>%
##   map_background_color(by_rows("grey95", "white")) %>%
##   set_position("left") %>%
##   set_align(, 3, "right") %>%
##   set_latex_float("h!")

## col_width(und.htab) <- c(.5, .15, .15, .2)
## width(und.htab) <- 0.5
## wrap(und.htab) <- TRUE

## und.htab <- rbind(c("", "Sykdommer", "", ""), und.htab)

## und.htab <- merge_cells(und.htab, 1, 2:3)
## align(und.htab)[1, 2] <- "center"
## bottom_border(und.htab)[1, 2] <- 0.5
## bottom_border(und.htab)[2, ] <- TRUE


outTab <- tabFun(undAll, "Sykdommer", rap = TRUE)
