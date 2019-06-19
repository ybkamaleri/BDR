## Kjønn og Nasjonalitet
## ---------------------

names(ars2018)

ars2018[, .N, keyby = .(fodelandMor)]
ars2018[, .N, keyby = .(fodelandFar)]

## pob - place of  birth
nordland <- c("Norge", "Danmark", "Finland", "Sverige", "Island")
ars2018[, pobMor := 0L][fodelandMor %in% nordland, pobMor := 1L]
ars2018[, pobFar := 0L][fodelandFar %in% nordland, pobFar := 1L]

## nordisk
ars2018[, nordisk:= pobMor + pobFar]
ars2018[, .N, by = .(nordisk)]
