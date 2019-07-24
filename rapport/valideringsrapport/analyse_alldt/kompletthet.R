## Kompletthet
## -----------

ars2018[is.na(und_syk_col), .N]

komVar <- c("und_syk_col", "und_syk_hypo", "und_syk_hype", "und_inssjokk", "und_ketoacidose", "lab_lip_LDL_2", "inn_Blodtrykk_s", "inn_Blodtrykk_d", "und_infiltrater")

# Antall ikke missing
ars2018[, lapply(.SD, function(x) sum(!is.na(x))), .SDcols=komVar]


## Fysisk aktivitet for bare de fra 6 år og eldre
aktiv <- subset(ars2018, alder >=6)
aktVar <- c("Inn_Akt", "Inn_Akt_Timer_Uke")
aktiv[, lapply(.SD, function(x) sum(!is.na(x))), .SDcols=aktVar]
