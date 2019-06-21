#######################################
## Funksjon for å hente rettet info
## fra Excel fil
####################################### 

## Path
hvor <- "./datavask/arkiv/per_sykehus" #her er test fil med ekstra variable "TEST" for Akershus

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

## lage tom list
utlist <- list()

## loop fil
for (i in 1:length(fil_navn)){

  # henter info i Excel
  .xlfil <- openxlsx::read.xlsx(file.path(hvor, fil_navn[i]), sheet = 2) #her sheet = 2 for nasjonalitet
  
  utlist[[i]] <- .xlfil

}

# kombinere alle uten ekstra variabler
utDT <- data.table::rbindlist(utlist)

# Kombinere alle med extra variabler inkludert
utDTx <- data.table::rbindlist(utlist, fill = TRUE)
