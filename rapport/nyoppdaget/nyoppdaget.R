## Nyoppdaget rapport
rm(list = ls())

## pakker
pkg <- c("data.table", "ggplot2", "rreg", "huxtable", "bookdown", "knitr", "rmarkdown", "colorspace", "pier", "stringi", "plotly")
lapply(pkg, library, character.only = TRUE)[[1]]

## Data path
dataSti <- "~/avid/bdr"
## Load all data
DT <- readRDS(file.path(dataSti, "nyoppdaget20180830.rds"))
## dt <- readRDS(file.path(dataSti, "RDSbackup/nyoppdatet2018alle.rds"))

dim(DT)

## ## ## Annonym pasienter fra tilbakemelding fra sykehus
## DT[FNavn == 'Annonym', annonym := 1]
## DT[Pnr == 11070897565, annonym := 1]

DT[, .N, by = annonym]
DT[, .N, by = agegp]

## Alder
## library("lubridate")
## DT[, alder := as.numeric(round(as.period(interval(FDato, inn_DiagDato)/duration(n=1, unit="years")), digits = 1))]

## Functions
source("~/Git-work/bdr/rapport/nyoppdaget/dbType.R")
## DTtype() for diabetestype
## ageCat() for alderfordeling

## Figur Antall
library(ggplot2)

nDT <- DT[, .N, by = .(hosKort)]
nDT

library(rreg)
(figAntall <- regbar(nDT[!is.na(hosKort)], hosKort, N, ylab = "Antall pasienter",
  title = "Nyoppdaget pasienter pr. sykehus"))
ggsave("figAntall.jpg")


## Felles Var
demoVar <-  c("PasientID", "Pnr", "hospital", "hospID", "hosKort", "Kjonn", "alder", "inn_DiagDato")

## diabetes variabler
diabetesVar = c("diabetes_Type1", "diabetes_Type2", "diabetes_Mody", "diabetes_Kir62", "diabetes_SekDiabetes", "diabetes_AnnenDiabetes", "diabetes_UkjentDiabetes")



## Antall generell
##--------------------
diabDT <- DTtype(DT)
## diabDT[]
dim(diabDT)
diabDT[, .N, by = diabType]

## Har ingen diagnose registeret
utDT <- DT[!diabDT, on = .(Pnr, hospID)] #De uten DT type
dim(utDT)
utDT[, c(demoVar, diabetesVar, "FNavn"), with = F]


hosp.diab <- groupingsets(diabDT,
  j = .(
    td1 = sum(dbtype == 1),
    td2 = sum(dbtype == 2),
    mody = sum(dbtype == 3),
    annen = sum(dbtype == 4)
  ),
  by = c("hosKort"),
  sets = list(c("hosKort"), character(0)))

hosp.diab

hosp.dttot <- cube(diabDT, .(antall = sum(!is.na(dbtype))),
  by = c("hosKort"))

## Beregning
diabDT[, .N, by = diabType]


## -----------------------
## Alder
##-----------------------
library("lubridate")
## dt <- allnyDT
## dt[, alder := as.numeric(round(as.period(interval(FDato, inn_DiagDato)/duration(n=1, unit="years")), digits = 1))]

over15 <- subset(diabDT, alder >= 15)
dim(over15)
over15[, .N, by = diabType]

diabDT[dbtype == 1 & alder < 5, .N]


under15 <- subset(diabDT, alder < 15)
dim(under15)
under15[, .N, by = diabType]

diabDT[dbtype == 1 & alder < 5, .N]


## Figure alderfordeling
##------------------------
dim(DT)
DT[, age := round(alder, digits = 0)]
ageDT <- DT[,.N, keyby = age]

DT[, .N, by = agegp]

DT[age == 1, .(Pnr, hosKort, inn_DiagDato, FDato)]
DT[age == 2, .(Pnr, hosKort, inn_DiagDato, FDato)]

## regbar(ageDT, age, N, flip = FALSE)
ageDT[]

(AlderFig <- ggplot(ageDT, aes(as.factor(age), N)) +
  geom_bar(stat = "identity", fill = "#004499") +
  labs(y = "Antall", x = "Alder", title = "Aldersfordeling for nyoppdaget: Alle typer") +
  theme_classic() + scale_y_continuous(expand = c(0, 0), limits = c(0, 60)) + #begynt på 0
  theme(
    axis.text = element_text(size = 12),
    panel.grid.major.y = element_line(size = .4, linetype = "dashed", color = "grey70"),
    ## panel.grid.minor.y = element_line(size = .2, linetype = "dashed", color = "grey70"),
    panel.grid.major.x = element_blank()
  ))

ggsave("alder_fig.jpg", plot = AlderFig, width = 15, height = 8, units = "cm")



## Alder i grupperinger og antall
## DT[, agegp := ageCat(alder, 0, 15, 5), by = Pnr]
rollup(DT, j = .(n = .N), by = "agegp")



## KUN Type 1 diabetes
##----------------------
## HbA1c
dim(DT)
dt1 <- DT[is.na(annonym) & diabetes_Type1 == "Ja", ]
dim(dt1)
hba <- "lab_HbA1c_Sentralt"
hba2 = "lab_HbA1c"
DT[, hba1c := get(hba)][is.na(hba1c), hba1c := get(hba2)]
dt1[is.na(hba1c), .N]
dt1[is.na(get(hba)), .N]

DT[hba1c > 18, .(Pnr, hosKort, hba1c)]
dt1[hba1c > 30, .(Pnr, hosKort, hba1c)]

## Endres hba1c verdi % til mol
## Hba1c verdi
DT[Pnr == 27060595769, hba1c := 13.6]
DT[Pnr == 2030884615 , hba1c := 11.7]
DT[Pnr == 27080595668 , hba1c := 16.8]
DT[Pnr == 30110595936, hba1c := 15.9]
DT[Pnr == 1041080473, hba1c := 12.1]
DT[Pnr == 24050983176, hba1c := 11.1]
DT[Pnr == 9011482128, hba1c := 7.9]
DT[Pnr == 30061399522, hba1c := 12.3]
DT[Pnr == 29041099621, hba1c := 14.2]
DT[Pnr == 8111180254, hba1c := 10.9]


rollup(dt1,
  j = .(
    mhb = mean(hba1c, na.rm = T),
    n = .N,
    min = min(hba1c, na.rm = T),
    max = max(hba1c, na.rm = T),
    nmiss = sum(is.na(hba1c))
  ), by = "agegp" )


## dim(DT)

## Diabetes type II
## -----------------
diabDT
diabDT[dbtype == 2 & alder < 15, .N]


## DKA
##--------
dim(dt1)
d1N <- nrow(dt1)

rollup(dt1,
  j = .(
    n = .N,
  ), by = "agegp"
  )


blo = "lab_Blodglukose"
kar = "lab_BiKarbonat"
ph = "lab_pH"

## dkaID <- dt1[get(blo) > 11, ] %>%
##   .[get(kar) < 15 | get(ph) < 7.3, .(Pnr)]


## dt1[get(blo) > 11, dka := 1]
dt1[get(kar) < 15, dka := 1]
dt1[get(ph) < 7.3, dka := 1]
dim(dt1)


## missing minst en dka
dt1[is.na(get(kar)) & is.na(get(ph)), misdk := 1]
dt1[is.na(misdk), misdk := 2]
dt1[, .N, by = misdk]


rollup(dt1[misdk == 1, ],
  j = .(
    n = .N
    ), by = "agegp"
  )

## DKA var
dt1[lab_BiKarbonat < 15 | lab_pH < 7.3, dka := 1]
dt1[is.na(dka), dka := 2] #missing
dt1[, .N, by = dka]

rollup(dt1[dka == 1, ],
  j = .(
    n = .N
    ), by = "agegp"
  )

rollup(dt1[dka == 2, ],
  j = .(
    n = .N
    ), by = "agegp"
  )


dt1[dka == 1 & get(ph) < 7, c("Pnr", ph, kar, "hosKort", "FNavn", "ENavn"), with = F]


## DKA Figur
dkaFil <- dt1[dka == 1, .N, by = hosKort]
dkaFil[, N := round(N, digits = 0)]
dkaFil[, N1 := sprintf("%0.0f", N)]

dkaFig <- regbar(dkaFil, x = hosKort, y = N, title = "Andel med DKA ved diagnose", ylab = "Antall")
dkaFig <- dkaFig + theme(
    axis.text = element_text(size = 12)
    ## panel.grid.major.y = element_line(size = .4, linetype = "dashed", color = "grey70"),
    ## panel.grid.minor.y = element_line(size = .2, linetype = "dashed", color = "grey70"),
    ## panel.grid.major.x = element_blank()
  )

ggsave("dkaFigur.jpg", plot = dkaFig, width = 15, height = 15, units = "cm")


dkaFig

## ## dt1[is.na(get(blo)), .N]
## ## dt1[is.na(get(kar)), .N]
## ## dt1[is.na(get(ph)), .N]

## dkaFil

## dt1[Pnr  %in% dkaID[[1]], dka := 1]
## dt1[, .N, by = dka]

## dim(dka)

## test <- DT[get(blo) > 11, c(kar, ph), with = F]
## test[15:25, ]

## DT[, .(lab_BiKarbonat, lab_pH)]


## RHF
## ------
dt1[, .N, keyby = .(rhf, hosKort, hospID)]

#Helse sør øst
dt1[hospID  %in% c(22, 1, 25, 13, 18, 20, 21, 19, 16, 15), rhf := 1]
dt1[hospID  %in% c(11, 17, 26, 7), rhf := 2] #Helse Midt
dt1[hospID %in% c(12, 3, 4, 2), rhf := 3] #Helse Vest
dt1[hospID  %in% c(10, 23, 6), rhf := 4]

hfDT <- rollup(dt1[dka == 1],
  j = .(n = .N), by = "rhf")

hfDT[is.na(rhf), rhf := 5]
hfDT[, pro := round(n / 383 * 100, digits = 0)]
hfDT

rhfNavn <- c("HSØ", "HMidt", "HVest", "HNord", "Norge")
hfDT[.(rhf = 1:5, to = rhfNavn), on = "rhf", rhf2 := i.to]

dkaRHF <- regbar(hfDT, rhf2, pro, num = n, comp = "Norge",
  ylab = "Andel (%)", title = "DKA ved nyoppdaget T1D")

ggsave("dkaFigurRHF.jpg", plot = dkaRHF, width = 15, height = 15, units = "cm")


## DKA Alvorlighet grad
## -----------------------
## ## dt1[get(blo) > 11, dka := 1]
## #Mild
## dt1[get(kar) < 15, dka := 1]
## dt1[get(ph) < 7.3, dka := 1]
## dt1[, .N, by = dka]


## ## Moderat
## dt1[, dkaMod := NULL]
## dt1[dka == 1 & get(kar) < 10, dkaMod := 1]
## dt1[dka == 1 & get(ph) < 7.2, dkaMod := 1]
## dt1[, .N, by = dkaMod]
## dt1[dkaMod == 1, .N, by = agegp]




## ## Alvorlig
## dt1[, dkaAlvo := NULL]
## dt1[dka == 1 & get(kar) < 5, dkaAlvo := 1]
## dt1[dka == 1 & get(ph) < 7.1, dkaAlvo := 1]
## dt1[, .N, by = dkaAlvo]
## dt1[dkaAlvo == 1, c(ph, kar), with = FALSE]
## dt1[dkaAlvo == 1, .N, by = agegp]


## dt1[get(blo) > 11, dka := 1]
#Mild
dt1[, dkaGd := NULL]
dt1[get(kar) < 15, dkaGd := 1]
dt1[get(ph) < 7.3, dkaGd := 1]

## Moderat
dt1[get(kar) < 10, dkaGd := 2]
dt1[get(ph) < 7.2, dkaGd := 2]

## Alvorlig
dt1[get(kar) < 5, dkaGd := 3]
dt1[get(ph) < 7.1, dkaGd := 3]
dt1[, .N, by = dkaGd]

dt1[!is.na(dkaGd), {
  n = .N;
  pro = n / 109;
  list(
    n = n,
    pro = pro
  )
}, by = agegp]

dt1[!is.na(dkaGd), .N]

## Mild
dt1[dkaGd == 1, {
  n = .N;
  pro = n / 109 * 100;
  list(
    n = n,
    pro = pro
  )
}, by = agegp]

## Moderat
dt1[dkaGd == 2, {
  n = .N;
  pro = n / 109 * 100;
  list(
    n = n,
    pro = pro
  )
}, by = agegp]

## Alvorlig
dt1[dkaGd == 3, {
  n = .N;
  pro = n / 109 * 100;
  list(
    n = n,
    pro = pro
  )
}, by = agegp]


## Insidens
norge <-  fread("~/avid/bdr/Personer1.csv", encoding = "Latin-1")
names(norge)
setnames(norge, names(norge)[4], "N")
norge[, .N, by = kjønn]

norgeBF <- norge[, sum(N)]
norgeBF

## Anntall 2018 med td1 under 15 år
diab017  <- 96 + 586 + 1222
DTA[alder < 15, .N]
diab017




denomN <- norgeBF - diab017
denomN

antallAlder <- DTA[, .N, by=.(agecat,agekat)]
saveRDS(antallAlder, "antallPerAlder.rds")

dt1N <- dt1[alder < 15, .N]
dt1N

Insidens <- dt1N / denomN * 100000

norge[kjønn %like% "Menn", kjonn := 1]
norge[kjønn %like% "Kvinner", kjonn := 2]
norge[, .N, by = kjonn]

norge
norge[, kk := .I, by = kjønn]
norge[kk %in%  1:5, age := 1]
norge[kk %in%  16:20, age := 1]
norge[kk %in%  6:10, age := 2]
norge[kk %in%  21:25, age := 2]
norge[kk %in%  11:15, age := 3]
norge[kk %in%  26:30, age := 3]
norge[, .N, by = age]

norge[, nage := sum(N), by = .(kjonn, age)]
## saveRDS(norge, "befolkning0_14.rds")
## Previous pasienter som er allerede syke
age15 <- antallAlder[agekat != 4, ]
age15
insDTraw <- merge(age15, norge, by.x = "agekat", by.y = "age")
insDTraw[, demon := nage - N.x]
insDTraw


nydd <-  dt1[, .N, by = agegp]
nydd[, kode := .I]
nydd <- nydd[kode != 4, ]
nydd

insDT  <- merge(insDTraw, nydd, by.x = "agekat", by.y = "kode")
saveRDS(insDT, "insidensAge.rds")
insDT[, ins := N / demon * 100000, by = .(agekat, kjonn)]
=======
dt1[, dkaAlvo := NULL]
dt1[dka == 1 & get(kar) < 5, dkaAlvo := 1]
dt1[dka == 1 & get(ph) < 7.1, dkaAlvo := 1]
dt1[, .N, by = dkaAlvo]
dt1[dkaAlvo == 1, c(ph, kar), with = FALSE]


## Insidence for 0 - 14 yrs
##----------------------------
norgeAll <- readRDS(file.path(dataSti, "befolkning0_14.rds"))
insDT <- readRDS(file.path(dataSti, "insidensAge.rds"))
insData <- readRDS(file.path(dataSti, "Insidens2018data.rds"))
## insDT[, kk := NULL]
## insDT[, c("agegp", "N") := NULL]
## saveRDS(InsDDT, file.path(dataSti, "insidensAge.rds"))
## saveRDS(norgeAll, file.path(dataSti, "befolkning0_14.rds"))
## names(insDT)
## setnames(insDT, c("N.x", "N.y", "nage", "demon"), c("agegpDT", "ageNok", "agegpTot", "agegpDenom"))
## setnames(insDT, "agegpDT", "ageKjonn")
insDT

## setkeyv(insDT, c('kjonn', 'agekat'))

insGutt <- insDT[kjonn == 1, ][!duplicated(agekat), ]
insJente <- insDT[kjonn == 2, ][!duplicated(agekat), ]

dt1[, .N, by = .(Kjonn, agegp)]

## Alle DT1 for 2018 for å beregne insidensen
dt1[, .N, by = .(agegp)]
dt1[, .N, by = .(Kjonn, agegp)]

dt12018fra0_14 <- rollup(dt1[agegp != "15+"],
  j = .(
    N = .N
  ), by = c("Kjonn", "agegp"))

dt12018fra0_14
dt12018fra0_14[is.na(agegp) & Kjonn == 'Jente', agegp := "Alle"] #Total jente
dt12018fra0_14[is.na(agegp) & Kjonn == 'Gutt', agegp := "Alle"] #Total gutt
dt12018fra0_14[is.na(agegp), agegp := "Alle"] #Total
dt12018fra0_14[is.na(Kjonn), Kjonn := "Total"]
dt12018fra0_14[Kjonn == "Gutt", kjonn := 1] %>%
  .[Kjonn == "Jente", kjonn := 2]
dt12018fra0_14[.(agegp = c("0-4", "5-9", "10-14"), to = 1:3), on = "agegp", agekat := i.to]
dt12018fra0_14[is.na(agekat) & kjonn  %in%  1:2, agekat := 5] #for total
setkeyv(dt12018fra0_14, c("kjonn", "agekat"))


## Fra Årskontroll 2018
DTars <- readRDS(file.path(dataSti, "dt2018medBlodTrykk.RDS"))
DTars[, .N, by = .(agekat)]
DTars[, .N, by = .(Kjonn, agekat)]

all2018fra0_14 <- rollup(DTars[agekat != 4],
  j = .(
    N = .N
  ), by = c("Kjonn", "agekat"))

all2018fra0_14
all2018fra0_14[is.na(agekat) & Kjonn == 'Jente', agekat := 5] #Total jente
all2018fra0_14[is.na(agekat) & Kjonn == 'Gutt', agekat := 5] #Total gutt
all2018fra0_14[is.na(agekat), agekat := 5] #Total
all2018fra0_14[is.na(Kjonn), Kjonn := "Total"]
all2018fra0_14[Kjonn == "Gutt", kjonn := 1] %>%
  .[Kjonn == "Jente", kjonn := 2]
setkeyv(all2018fra0_14, c('kjonn', 'agekat'))


## Merge insidens data
InsGutt <- merge(insGutt, all2018fra0_14[kjonn == 1, ], by = "agekat")
igutt <- merge(InsGutt, dt12018fra0_14[kjonn == 1, ], by = "agekat")
setnames(igutt, c("agegpTot", "N.x", "N.y"), c("ageKjonn", "agegp2018", "ageNyopp"), skip_absent = TRUE)
igutt[]
# agegpDT -

dt12018fra0_14

insJente

norgeAll
setkeyv(norgeAll, c("kjonn", "age"))

## sum per kjønn
setkeyv(norgeAll, c("kjonn", "age"))
norgeAll[, nkjonn := sum(N), by = kjonn]
norgeAll[, nagegp := sum(N), by = .(kjonn, age)]

all2018fra0_14
InsDT <- all2018fra0_14[norgeAll]
setkeyv(InsDT, c("kjonn", "agekat"))
InsDDT <- dt12018fra0_14[InsDT]

toChg <- c("N", "i.N", "i.N.1", "nage", "nkjonn")
nyChg <- c("nDT1", "nUt", "ageN", "agegpN", "kjonnN")
setnames(InsDDT, toChg, nyChg)
InsDDT[, c("i.Kjonn", "kjønn") := NULL]


## Lager denominator
InsDDT <- insDT
InsDDT
InsDDT[, id := as.numeric(paste0(kjonn, agekat))]
InsDDT[, agegpD := agegpN - nUt] #agegroup denominator

InsDDT[!duplicated(id), utKjonn := sum(nUt), by = .(kjonn)]
InsDDT[!duplicated(id), kjonnD := kjonnN - utKjonn, by = kjonn] #agegroup denominator

## sum insidense per kjønn
## ------------------------
InsDDT[!duplicated(id), nKjonn := sum(nDT1), by = kjonn]
InsDDT


## bort irrelevant column
insData <- InsDDT[!duplicated(id)]
saveRDS(insData, file.path(dataSti, "Insidens2018data.rds"))
insData

insData[, `:=`(
  insAge = nDT1 / agegpD * 100000,
  insKjonn = nKjonn / kjonnD * 100000
)]

insData

tabIns <- insData[, .(Kjonn, agegp, insAge, insKjonn)]
selvar <- c('insAge', 'insKjonn')
tabIns[, lapply(.SD, function(x) round(x, digits = 1)), .SDcols = selvar]
tabIns[, (selvar) := lapply(.SD, function(x) round(x, digits = 1)), .SDcols = selvar]
tabIns

tabhux <- as_hux(tabIns, add_colnames = TRUE) %>%
  set_bottom_border(1,, TRUE)
quick_docx(tabhux)
quick_pdf(tabhux, file = "insidense.pdf")
## norgeAll
## norgeAll[kjonn == 1, Nage := sum(N), by = age]
## norgeAll[kjonn == 2, Nage := sum(N), by = age]
## norgeAll[, kjN := sum(N), by = kjonn]

## InsDDT
## gutt = 50 + 321 + 634
## 481709 - gutt
>>>>>>> eba38aa4137d56fb3a2dc39f0270a414c5c1c15c
