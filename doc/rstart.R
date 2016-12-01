#########################################
#### Ctr+A og Ctr+R for første kjøring ##
#########################################
rm(list=ls())
# library(devtools)
# library(roxygen2)
# kode <- as.package("~/Git-work/nbReg")
# load_all(kode)
# document(kode)
#
# ## Lage paken
# check(kode)
# ##build(kode, manual=TRUE, path = "~/Git-work/Packages")

library(bdr)
setwd("~/OUS/BDR/Rapport/Data/")
##install("~/Git-work/BDR")
Filnavn <- "annonym20161107.sav" 	# *.sav, *.csv, *.xlsx

##################################
#### Filter for neste kjøring ####
##################################

MinAlder = 2
MaxAlder = 20
DBType = 1  # 1:Type1 2:Alle
Kjonn  = 3  		# 1:gutt 2:jente 3:alle
DataValg = 4 		# 1:F?rstegangsreg 2:?rskontroll 3:Poliklinisk 4:Alle
DatoFra = "2000-01-01"  # YYYY-MM-DD
DatoTil = "2016-12-31"  # YYYY-MM-DD

#### Rapport ####

Sykehus = 1
RapportValg = 3 		# 1:Landet 2:Lokal 3:Lokal mot andre sykh.

YAksen = 2 			# 1:prosent, 2:antall, 3:hbalc 4:diabetesVarighet

ValgtVar = "alderkat"  	# Valg variablene p? listen nedenfor
## alder, alderkat, kjonn,

## Kjør
rfigur()
