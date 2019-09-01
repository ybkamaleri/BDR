## Lager filer til excel og spss

## Data path
dataSti <- "~/avid/bdr"

## Data for årskontroll
DT <- readRDS(file.path(dataSti, "dt2018medBlodTrykk.RDS"))
dt <- readRDS(file.path(dataSti, "validert_arskontroll2018.rds"))
dim(dt)
dim(DT)

## SPSS
foreign::write.foreign(DT,
  file.path(dataSti, "arskontroll2018.txt"),
    file.path(dataSti, "arskontroll2018.sps"),
      package = "SPSS")

## Excel
rio::export(DT, file.path(dataSti, "arskontroll2018.xlsx"))


## Nyoppdaget
nyDT <- readRDS(file.path(dataSti, "nyoppdaget20180830.rds"))
## SPSS
foreign::write.foreign(nyDT,
  file.path(dataSti, "ny2018.txt"),
    file.path(dataSti, "nyoppdaget2018.sps"),
      package = "SPSS")

## Excel
rio::export(nyDT, file.path(dataSti, "nyoppdagetl2018.xlsx"))
