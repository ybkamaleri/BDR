#######################################
## Funksjon for å hente rettet info
## fra Excel fil
####################################### 

## Path
# hvor <- "./datavask/arkiv/per_sykehus" #her er test fil med ekstra variable "TEST" for Akershus
hvor <- "./datavask/per_sykehus"

## hente filnavn
fil_navn <- list.files(paste(hvor, sep="/"))
fil_tall <- length(fil_navn)
fil_tall

## Antall sheets
sheets_navn <- openxlsx::getSheetNames(file.path(hvor, fil_navn[1]))
sheets_tall <- length(sheets_navn)
sheets_tall

# ## loop list
# for (i in 1:sheets_tall){
#   
#   .fil <- file.path("./datavask/arktiv/per_sykehus", i)
#   .sheets <- openxlsx::getSheetNames(.fil) 
#   .dt <- lapply(.sheets, openxlsx::read.xlsx, xlsxFile = .fil)
# 
# }



# colnames
.colnavn <- names(openxlsx::read.xlsx(file.path(hvor, fil_navn[1]), sheet = 1))

# tom data.table
.utdt <- data.table::data.table(matrix(ncol=length(.colnavn), nrow = 0))
data.table::setnames(.utdt, names(.utdt), .colnavn)



## Merge alle sykehus til hver tematikk
## --------------------------------------
# 
# 
# ## loop fil
# for (i in 1:length(fil_navn)){
# 
#   ## lage tom list
#   utlist <- list()
# 
#   ## henter info i Excel
#   .xlfil <- openxlsx::read.xlsx(file.path(hvor, fil_navn[i]), sheet = 2) #her sheet = 2 for nasjonalitet
#   
#   utlist[[i]] <- .xlfil
# 
#  }
# 
# 
# # kombinere alle uten ekstra variabler
# utDT <- data.table::rbindlist(utlist)
# 
# # Kombinere alle med extra variabler inkludert
# utDTx <- data.table::rbindlist(utlist, fill = TRUE)
# 
# ## Function
# ## ----------
library(data.table)

xlTab <- function(x = fil_navn, sheet, fill = TRUE){
  ## lage tom list
  utlist <- list()
  ## variable to be excluded
  ex.person.var <- c("PasientID", "FDato", "FNavn", "ENavn", "hospID", "hospital")

    ## loop fil
  for (i in 1:length(x)){
    
    ## henter info i Excel
    .xlfil <- openxlsx::read.xlsx(file.path(hvor, x[i]), sheet = sheet) #her sheet = 2 for nasjonalitet
    setDT(.xlfil)
    .xlfil[, (ex.person.var) := NULL] #bort med andre uviktige variablene (person variable)

    varOld <- names(.xlfil)[-1]
    varNew <- paste0(varOld, ".xxl") #add extention .xxl for corrected data
    
    setnames(.xlfil, varOld, varNew)
    
    utlist[[i]] <- .xlfil
    
  }
  
  if (fill){
    utDT <- data.table::rbindlist(utlist, fill = TRUE)
  }
  
  return(utDT)
}

## hba1c sheets_navn
bmi=xlTab(sheet = "bmi_under12")

## kjøre alle sheets_navn 
allXL <- list()
for (i in sheets_navn) {
  .xl <- xlTab(sheet = i)
  allXL[[i]] <- .xl
}

# allXL[["nasjonalitet"]][, Fdato.xxl := NULL]
saveRDS(allXL, "./datavask/renset/sykehus_vasket.RDS")
allXL <- readRDS("./datavask/renset/sykehus_vasket.RDS")


#### Merge til hoved datasettet
##------------------------------

# Give new names to columns other than personal related columns
# Merge to whole dataset
# If not missing in the new columns than replace it with the column in the whole dataset.
person.var <- c("PasientID", "Pnr", "FDato", "FNavn", "ENavn", "hospID", "hospital")

XLinOne <- rbindlist(allXL, fill = T)


## Sjekk duplikater pasienter pga. finner på forskjellige Excel faner
xxlNvn <- names(XLinOne)[-1]
XLinOne[Pnr ==1090182699,  c("Pnr", xxlNvn), with=F]


## Behandle duplikate hvem å fylle opp verdier som er missing med
## tilgjengelige verdier
for (j in names(XLinOne)){
  XLinOne[, (j) := get(j)[!is.na(get(j))][1], by = Pnr]
}

## sjekk at verdier er lagt rigktig før duplikater slettes
# XLinOne[Pnr ==1090182699,  .(Pnr, lab_HbA1cAkerVerdi.xxl) ]
# XLinOne[Pnr ==1090182699,  c("Pnr", xxlNvn), with=F]

## bort med duplikate
XL1line <- XLinOne[!duplicated(Pnr), ]

# XL1line[Pnr ==1090182699,  .(Pnr, lab_HbA1cAkerVerdi.xxl) ]
# XL1line[Pnr ==1090182699,  c("Pnr", xxlNvn), with=F]
XL1line[Pnr ==1090182699,  c("Pnr", xxlNvn), with=F]


## alle master dataset
dt <- readRDS("validert_arskontroll2018.rds")
DT <- copy(dt)
setkey(DT, Pnr)
setkey(XL1line, Pnr)

## Merge all
# allDT <- XL1line[DT, on = "Pnr"]
alldt2018 <- merge(DT, XL1line, by = "Pnr", all.x = TRUE)


## Sjekk pasienter som har mangle data
# DT[Pnr ==1090182699,  .(lab_HbA1cAkerVerdi, Lab_blodprove, lab_urinprove) ]
# alldt2018[Pnr ==1090182699,  .(lab_HbA1cAkerVerdi, Lab_blodprove, lab_urinprove) ]
# 
# XL1line[Pnr ==1090182699,  c("Pnr", xxlNvn), with=F]
# 
# alldt2018[Pnr == 19070490253, .(lab_HbA1cAkerVerdi, Lab_blodprove, lab_urinprove)]
# alldt2018[, str(.SD), .SDcols = c("lab_HbA1cAkerVerdi", "Lab_blodprove", "lab_urinprove")]
# XL1line[Pnr == 19070490253, .(lab_HbA1cAkerVerdi.xxl, Lab_blodprove.xxl, lab_urinprove.xxl)]
# str(XLinOne)
# str(alldt2018$Lab_blodprove)
# 
# alldt2018[Pnr == 30051494233, c(Lab_blodprove, lab_urinprove, Lab_blodprove.xxl, lab_urinprove.xxl)]


## hente navn for rettet file dvs. Excel filer
xxlNvn <- names(XLinOne)[-1]


## lab_uringprove og Lab_blodprove må byttes til "chr" først for å være lik som Excel datasettet
labBytt <- c("Lab_blodprove", "lab_urinprove")
alldt2018[, (labBytt) := as.character(get(labBytt))]


## Bytt verdi som ikke er missing i .xxl til variabel uten .xxl extention
for (j in xxlNvn){
  j2 <- gsub(".xxl$", "", j)
  alldt2018[!is.na(get(j)), (j2) := get(j), by = .(Pnr)]
}


## remove .xxl variabel
alldt2018[, (xxlNvn) := NULL]

saveRDS(alldt2018, "./datavask/renset/rettet_xl2018.rds")
rettetDT <- readRDS("./datavask/renset/rettet_xl2018.rds")

## OBS! Når alt er gjort så skal filen erstarte 'validert_arskontroll2018.rds"
saveRDS(alldt2018, "validert_arskontroll2018.rds")






##################################################################
## TEST 
######################

xlTabTEST <- function(x = fil_navn, sheet, fill = TRUE){
  ## lage tom list
  utlist <- list()
  ## variable to be excluded

  ## loop fil
  for (i in 1:length(x)){
    
    ## henter info i Excel
    .xlfil <- openxlsx::read.xlsx(file.path(hvor, x[i]), sheet = sheet) #her sheet = 2 for nasjonalitet
    setDT(.xlfil)

    utlist[[i]] <- .xlfil
    
  }
  
  if (fill){
    utDT <- data.table::rbindlist(utlist, fill = TRUE)
  }
  
  return(utDT)
}

## hba1c sheets_navn
dtTEST=xlTabTEST(sheet = "nasjonalitet")

dtTEST

########################################################
## Sammenlikne før og etter merging med XL fil
########################################################

bt4 <- readRDS("./arkiv/validert_arskontroll2018.rds")
dt5 <-  readRDS("validert_arskontroll2018.rds")

lapply(list(bt4, dt5), dim)
dim(bt4)
dim(dt5)

bt4[is.na(lab_HbA1cAkerVerdi), .N]
dt5[is.na(lab_HbA1cAkerVerdi), .N]


######################################################
## Random number
######################################################
set.seed(9548)
nr <- floor(runif(30, min=100, max=900))
bok <- stringi::stri_rand_strings(30, 2, pattern = "[A-Z]" )
simpan <- paste(bok, nr, sep = "")
## Base R style
result <- rawToChar(as.raw(sample(c(65:90,97:122), 5, replace=T)))
