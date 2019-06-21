## koble alle datasettet
## ---------------------

if(!require(rio)) install.packages("rio")
library(rio)

library(data.table)

datamappe <- "K:/Sensitivt/Forskning02/Barnediabetesregisteret_2015-5739/Barnediabetes og kvalitet/Datafiler"

# list.dirs(path = file.path(datamappe), recursive = F)

filmappe <- dir(file.path(datamappe), pattern = "^20", full.names = TRUE)
filmappe

dt2001 <- import(file.path(filmappe[1], "Data_2001.sav"))
dt2002 <- import(file.path(filmappe[2], " "))
dt2003 <- import(file.path(filmappe[3], "data_03.sav"))
dt2004 <- import(file.path(filmappe[4], "Barndiab_04.sav"))
dt2005 <- import(file.path(filmappe[5], "BenchmarkingData_2005.sav"))
dt2006 <- import(file.path(filmappe[6], "Data2006_bench.rapporter.sav"))
dt2007 <- import(file.path(filmappe[7], "Alle data 2007 pr 040308.sav"))
dt2008 <- import(file.path(filmappe[8], "Alle data 2008 26.03.09.sav"))
dt2009 <- import(file.path(filmappe[9], "Data2009_april.sav"))
dt2010 <- import(file.path(filmappe[10], "Datafil_120411_2010_analysefil.sav"))
dt2011 <- import(file.path(filmappe[11], "Analysefil_2011_180412_org.sav"))
dt2012 <- import(file.path(filmappe[12], "Analysefil_2012_230513.sav"))
dt2013 <- import(file.path(filmappe[13], "Analysefil_2013_140414.sav"))
dt2014 <- import(file.path(filmappe[14], "Gjeldenede kobling/2014_Analysefil250515.sav"))
dt2015 <- import(file.path(filmappe[15], "Endelig_analysefil_2015.sav"))
dt2016 <- import(file.path(filmappe[16], "Årskontroll_2016_april.sav"))
dt2017 <- import(file.path(filmappe[17], "Årskontroll17.sav"))


## konvertere til data.table
dt.filer <- ls(pattern = "^dt20")

# for (i in dt.filer) setDT(get(i)),
lapply(dt.filer, function(x) setDT(get(x)))

# gjør direkte
invisible(sapply(ls(pattern = "^dt20"), function(x) setDT(get(x))))

# Valg variabler
names(dt2004)
var2004ldl <- grep("ldl", names(dt2004), ignore.case = T, value = T)

var2004 <- c("Tot_kol", "HDL_kol", "LDL_kol")
dt2004[, lapply(.SD, str), .SDcols=var2004ldl]
dt2004[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = var2004ldl]
