## Load data and setup
##--------------------

list.of.packages <- c("data.table", "stringr", "lubridate")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "https://cloud.r-project.org/")
sapply(list.of.packages, require, character.only = TRUE)

## Diabetes data
## Data path
dataSti <- "~/avid/bdr"
## Load all data årskontroll
DT <- readRDS(file.path(dataSti, "dt2018medBlodTrykk.RDS"))
## Load all data nyoppdaget
DTny <- readRDS(file.path(dataSti, "nyoppdaget20180830.rds"))


## Norsk befolkningsdata
norskBF <- readRDS("norskbefolkingUnd19.Rds")
fylkeList <- norskBF[, .N, by = .(region, fylke)][, N := NULL][]
fylkeList

sykehusList <- DT[, .N, by = .(hosKort, hospID)][, N := NULL][]
sykehusList

## Kobling sykehus og fylke
sykort <- sykehusList[["hosKort"]]
sykID <- data.table(hosKort = sykort)
sykID[, id := .I]
sykID
sykID[id == 1, fylkeid := 2] %>%
  .[id == 2, fylkeid := 9] %>%
  .[id == 3, fylkeid := 18] %>%
  .[id == 4, fylkeid := 6] %>%
  .[id == 5, fylkeid := 4] %>%
  .[id == 6, fylkeid := 20] %>%
  .[id == 7, fylkeid := 14] %>%
  .[id == 8, fylkeid := 5] %>%
  .[id == 9, fylkeid := 19] %>%
  .[id == 10, fylkeid := 11] %>%
  .[id == 11, fylkeid := 12] %>%
  .[id == 12, fylkeid := 10] %>%
  .[id == 13, fylkeid := 15] %>%
  .[id == 14, fylkeid := 50] %>%
  .[id == 15, fylkeid := 5] %>%
  .[id == 16, fylkeid := 15] %>%
  .[id == 17, fylkeid := 50] %>%
  .[id == 18, fylkeid := 19] %>%
  .[id == 19, fylkeid := 18] %>%
  .[id == 20, fylkeid := 11] %>%
  .[id == 21, fylkeid := 8] %>%
  .[id == 22, fylkeid := 50] %>%
  .[id == 23, fylkeid := 3] %>%
  .[id == 24, fylkeid := 7] %>%
  .[id == 25, fylkeid := 15] %>%
  .[id == 26, fylkeid := 1]

idFylker <- sykID[fylkeList, on = c(fylkeid = "fylke")]
sykFylker <- sykehusList[idFylker, on = "hosKort"]

fylkSykKob <- sykFylker[, .(region, hospID, fylkeid)]
## Join sykehus til fylke
fylkSykKob
saveRDS(fylkSykKob, "sykehusFylker.Rds")


## Load fylke og sykhehus ID kobling
fylkeSykId <- readRDS("sykehusFylker.Rds")
fylkeSykId

## Join fylke til sykehus
sykehusList
SykFylkeAlle <- fylkeSykId[sykehusList, on = "hospID"]
SykFylkeAlle[, fylke := fylkeid]
SykFylkeAlle
saveRDS(SykFylkeAlle, "FylkeSykehus.Rds")



## Merge DTs må skjer bare for Sykehus ID fordi fylkes nivå ikke er unik
tbls <- list(norskBF, SykFylkeAlle)
lapply(tbls, function(i) setkey(i, fylke))

mergedDT <- Reduce(function(...) merge(..., all = T), tbls)
