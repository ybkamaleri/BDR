## Blodtrykk
## ----------------
## https://apps.cpeg-gcep.net/BPz_cpeg/

## Dette inkluderes bare pasienter mellom 2 til 17.99 år.

## Alder i måned
ars2018[, agemons := as.numeric(round(as.period(interval(dob, inn_Dato)/duration(n=1, unit="months")), digits = 0))]

## Kjønn til 1=Gutt 2=Jente
ars2018[.(Kjonn = c("Gutt", "Jente"), to = 1:2), on = "Kjonn",  sex := i.to]

subVars <- c("alder", "Kjonn", "Pnr", "sex", "agemons", "inn_Lengde", "inn_Blodtrykk_s", "inn_Blodtrykk_d")
bloodtyk <- subset(ars2018, alder >= 2 & alder < 18, select = subVars)

subVarOld <- c("Pnr", "inn_Lengde", "inn_Blodtrykk_s", "inn_Blodtrykk_d")
subVarNew <- c("id", "height", "sbp", "dbp")
colrekk <- c("id", "sex", "Kjonn", "alder", "agemons", "height", "sbp", "dbp")

setnames(bloodtyk, subVarOld, subVarNew)
setcolorder(bloodtyk, colrekk)
bloodtyk

write.csv(bloodtyk, "bloodtyk.csv")

## read output fra cpeg shiny
bpress <- read.csv("out_bloodtyk.csv", sep = ",")

setDT(bpress)

# ## variabelnavn
# syspct: percentiles for systolisk (sbp)
# diapct: percentiles for diastolisk (dbp)

bpress[, .N, by=.(stage)]

## Normal er  <90th SBP/DBP Percentile
## Elevated er mellom 90 og 94 percentile
## Stage 1 og 2 er over og lik 95 percentile dvs. hypertension

persDT <- subset(ars2018, select = c("Pnr", "hospital", "hosKort", "hospID", "inn_Blodtrykk_94", "inn_Blodtrykk_95"))

btAlle <- merge(bpress, persDT, by.x = "id", by.y = "Pnr", all = TRUE)

btAlle[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = c("inn_Blodtrykk_94", "inn_Blodtrykk_95")]
btAlle[, lapply(.SD, function(x) sum(!is.na(x))), .SDcols = c("inn_Blodtrykk_94", "inn_Blodtrykk_95")]


## kontrollerer
btAlle[inn_Blodtrykk_94 == "Ja" | inn_Blodtrykk_95 == "Ja", ][1:20,]
btAlle[stage == "normal", ][1:20, ]

## stage er normal men ikke på eReg
btAlle[inn_Blodtrykk_94 == "Ja" | inn_Blodtrykk_95 == "Ja", ][stage == "normal",]
## 
btAlle[inn_Blodtrykk_94 == "Nei" | inn_Blodtrykk_95 == "Nei", ][stage != "normal" | !is.na(stage),]


#########################
## Bruk bp_pers funksjon
#########################

bsti <- "P:\\OUS_BDR\\blood_pressure\\r-code-master"

source(file.path(bsti, "child_height_percentile_v0.92.r", fsep = "\\"))
source(file.path(bsti, "child_bp_percentile_v0.9.r", fsep = "\\"))

bp2 <- copy(bpress)
bp2[, gender := sex]
bp2[, sex := ifelse(gender == 1, "M", "F")]
bp2[, age := alder]


## create hight percentil with ht_perc function

htpct <- as.data.table(ht_perc(bp2))
bp22 <- cbind(bp2, htpct)

setnames(bp22, "V1", "ht_perc")

## alder er mellom 1 til 17
bp12 <- subset(bp22, age <= 17)
bp00 <- bp_perc(bp12)

bpmix <- cbind(bp12, bp00)
saveRDS(bpmix, "bloodpressure.RDS")
