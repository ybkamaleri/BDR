## nasjonalitet og kjønn
## ---------------------

nordLand <- c("Norge", "Danmark", "Finland", "Sverige", "Island")

## Data kilder
nasDT <- ars2018

nasDT[!is.na(fodelandMor) , nordiskMor := ifelse(fodelandMor %in% nordLand, 1L, 0L)]
nasDT[!is.na(fodelandFar) , nordiskFar := ifelse(fodelandFar %in% nordLand, 1L, 0L)]

## nordisk barn
## Minst en av foreldrene er født i de nordiske landene
nasDT[, .N, by=.(nordiskFar)]

nordBarn <- nasDT[, {mor = ifelse(is.na(nordiskMor), 0, nordiskMor);
             far = ifelse(is.na(nordiskFar), 0, nordiskFar);
             barn = mor + far;
             nordiskBarn = ifelse(is.na(nordiskMor) & is.na(nordiskFar), NA, barn);
             list(barn = barn, 
                  nordiskBarn = nordiskBarn,
                  mor = nordiskMor,
                  far = nordiskFar,
                  kjonn = Kjonn,
                  Pnr = Pnr
                  )}]

nordBarn[.(nordiskBarn = 1:2, to = 1), on = "nordiskBarn", nordiskBarn := i.to]
# nordBarn[nordiskBarn %in% 1:2, nordiskBarn := 1]

# ukjent nasjonalitet
nordBarn[is.na(nordiskBarn), nordiskBarn := 3]

## Tabell long
barnLg <- nordBarn[, .N, by = .(nordiskBarn, kjonn)]

## Legger prosent
barnLg[, pros := round(N / sum(N, na.rm = T) * 100, digits = 1)]

## Mix antall og prosent
barnLg[, n := sprintf("%s (%s%%)", N, pros)]

## Snu til wide
barnWt <- dcast(barnLg, kjonn ~ nordiskBarn, value.var = "n")

## reorder columns
setcolorder(barnWt, c("kjonn", "1", "0", "3"))

## legger riktig colnavn
setnames(barnWt, names(barnWt), c("Kjønn", "Nordisk", "Ikke Nordisk", "Ukjent"))
