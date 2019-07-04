## undersøkelse utført
## -------------------

## variablene
undVar <- c("und_Oye", "und_laser", "und_Retinopati", "und_fotter", "und_periferneu", "und_infiltrater", "und_hudforandring", "lab_res_persmikro")
testDT <- subset(ars2018, select = undVar)

## Count missing og non-missing
## -----------------------------
undVarMiss <- ars2018[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = undVar]
undVarNon <- ars2018[, lapply(.SD, function(x) sum(!is.na(x))), .SDcols = undVar]

## Count som svarte Ja
## ----------------------
# library(sqldf)
# sqldf('SELECT COUNT(und_Oye) FROM testDT WHERE und_Oye = "Ja";')

mList <- list()

for (i in undVar){
  misInd <- ars2018[get(i)=="Ja", .N]
  mList[[i]] <- misInd
}

undVarTall <- as.data.table(mList)

rbindlist(list(undVarTall, undVarNon))


## urin-albumin og mikroalbuminuri
# ars2018[, str(.SD), .SDcols=c("lab_res_1prove", "lab_res_persmikro")]
# ars2018[is.na(lab_res_1prove), .N ]
ars2018[, .N, by=.(lab_res_persmikro)]

## urin-albumin er tall derfor er det være missing eller ikke
urv <- c("lab_res_1prove", "lab_res_persmikro")
ars2018[, lapply(.SD, function(x) sum(is.na(x))), .SDcols=urv]
