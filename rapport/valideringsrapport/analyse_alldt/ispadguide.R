## ISPAD guidelines
## ----------------

ars2018[, ispad := 0]
ars2018[, ispad := ifelse(alder >=10 & diagVar >=2, 1, 0)]
ars2018[, ispad := ifelse(alder < 10 & diagVar >=5, 1, ispad)]

ars2018[, .N, by=ispad]

## Øye undersøkelse
ars2018[ispad == 1, .N, by=.(und_Oye)]

## Urin
ars2018[ispad == 1, sum(!is.na(lab_res_1prove))]
ars2018[ispad == 1, sum(is.na(lab_res_1prove))]
