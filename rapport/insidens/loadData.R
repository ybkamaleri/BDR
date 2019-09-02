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
norskBF

fylke <- DT[, .N, by = .(hosKort, hospID)][, N := NULL][]
fylke
