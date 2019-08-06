## Boluskalkulator

lokalDT <- lok2018dt1

bolVar <- c("beh_ins_boluswizzard", "beh_ins_boluswizzard_frekv", "bolkod")

## Bruker det
var1 <- bolVar[1]
var2 <- bolVar[2]
var3 <- bolVar[3]

## lage ny variabel 'bolfrek' for at
## missing for bolus frekv omkodet til "Vet ikke"
lokalDT[, bolfrek := get(var2)]
lokalDT[is.na(get(var2)), bolfrek := "Vet ikke"]

## Omkode var
## lokalDT[, .N, by = bolfrek]
lokalDT[, bolkod := 3] #Vet ikke
lokalDT[bolfrek %like% "<", bolkod := 1]
lokalDT[bolfrek %like% ">", bolkod := 2]

## lokalDT[, .N, by = .(bolfrek, bolkod)]

## aggregere
bolTab1 <- lokalDT[get(var1) == "Ja",
  list(
    mean = mean(lab_HbA1cAkerVerdi, na.rm = T),
    median = median(lab_HbA1cAkerVerdi, na.rm = T),
    n = .N),
  by = .(get(var3))]

bolTab1[, ind := .I]


## Antall all som svart for bruker bolus
nbolus <- length(which(lokalDT$beh_ins_boluswizzard == "Ja"))
## Andell
bolTab1[, pros := round(n / nbolus * 100, digits = 1)]

## Ikke bruke
bolTab2 <- lokalDT[get(var1) == "Nei",
  list(
    get = 5,
    mean = mean(lab_HbA1cAkerVerdi, na.rm = T),
    median = median(lab_HbA1cAkerVerdi, na.rm = T),
    n = .N,
    ind = 0)] #ikke bruke skal liger overst

## Antall som svarte Ja eller Nei
nboikke <- lokalDT[get(var1)  %in% c("Ja", "Nei"), .N]
## Andell
bolTab2[, pros := round(n / nboikke * 100, digits = 1)]


## Merge begge tabellene
inTab <- rbindlist(list(bolTab2, bolTab1))

## round tallene
inTab[, mean := round(mean, digits = 1)]

## reorder
setorder(inTab, ind)
inTab[, ind := NULL]#delete index

## Rename
nyNavn <- c(
  "Bruker ikke",
  "Bruker <50% av tiden",
  "Bruker >50% av tiden",
  "Bruker det, men ukjent brukfrekvense")

dummyDT <- data.table(bolkod = c(5, 1, 2, 3), frek = nyNavn)

## oldNavn <- as.character(inTab$get)

## legger nynavn til filen
allTab <- merge(dummyDT, inTab, by.x = "bolkod", by.y = "get", all.y = TRUE)
setkey(allTab, bolkod)

allTab[, bolkod := NULL]

setcolorder(allTab, c("frek", "n", "pros", "mean", "median"))
setnames(allTab, names(allTab), c("Boluskalkulator","antall", "andel", "gj.snitt", "median"))

## Tabell
outTab <- tabHux(allTab, size = 0.8, rap = TRUE, del = c(.6, .1, .1, .1, .1))

## fancy tabel
bottom_border(outTab)[1, ] <- FALSE
outTab <- rbind(c("", "", "", "HbA1c", ""), outTab)
outTab <- merge_cells(outTab, 1, 4:5)

outTab <- outTab %>%
  set_top_border(3,, TRUE) %>%
  set_bottom_border(1, 4, 0.5) %>%
  set_align(1, 4, "left")
  ## set_right_border(, 3, 0.4) %>%
  ## set_align(2:nrow(outTab), c(3, 5), "center")




## ## Sjekker
## lokalDT[, str(.SD), .SDcols = bolVar]
## var1 <- bolVar[2]
## lokalDT[, .N, keyby = get(var1)]
