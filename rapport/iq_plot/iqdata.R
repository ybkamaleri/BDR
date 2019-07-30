## IQ prosjekt

pkg <- c("data.table", "readxl", "stringi", "lubridate")
lapply(pkg, library, character.only = TRUE)[1]

dt <- read_xlsx("2019.05.08_IQ.xlsx", sheet = "Ark1")
head(dt)

setDT(dt)

setnames(dt, names(dt), c("avd", "date", "n", "hbpro", "hbmol", "til"))
str(dt)
dt[,.N, by=avd]
dt

## Uten Haugesund, Elverum og Ålesund
## -----------------------------------
DT = dt[!(avd %in% c("Haugesund", "Ålesund", "Elverum")), ]
DT[, .N, by = avd]

DT[, str(.SD)]
DT[, dato := as.IDate(as.numeric(`date`), origin="1899-12-30")]
DT[, n:= as.numeric(n)]
varDEL = c("date", "til")
DT[, (varDEL) := NULL]
setkey(DT, avd)
str(DT)


## De med bare kvartal
## --------------------
dt2 = dt[avd %like% "Haugesund" | avd %like% "Ålesund" | avd %like% "Elverum", ]
dt2[, til := NULL]

dt2[, antallgj := as.numeric(n)/3]
dt2[avd %like% "Ålesund" & `date` %like% "2018", antallgj := 114/12 ]
dt2


haugDT = data.table( avd = "Haugesund",
                  dato = c(seq(ymd('2018-01-01'), ymd('2018-12-01'), by='months'), seq(ymd('2019-01-01'), ymd('2019-04-01'), by='months')))

setkey(haugDT, "avd")
haugDT[1:3, `:=`(hbpro = 7.4, hbmol = 57, n = 29.5)][
    4:6,  `:=`(hbpro = 7.3, hbmol = 57, n = 6)][
    7:9,  `:=`(hbpro = 7.3, hbmol = 57, n = 28.25)][
    10:12,  `:=`(hbpro = 7.0, hbmol = 53, n = 25.5)][
    13:16,  `:=`(hbpro = 6.8, hbmol = 50, n = 12.25)]

haugDT

## Ålesund
alsundDT = data.table( avd = "Ålesund",
                       dato = c(seq(ymd('2017-10-01'), ymd('2017-12-01'), by='months'),
                                seq(ymd('2018-01-01'), ymd('2018-12-01'), by='months'),
                                seq(ymd('2019-01-01'), ymd('2019-03-01'), by='months')))

setkey(alsundDT, "avd")
alsundDT[1:3, `:=`(hbpro = 7.7, hbmol = 60)][
  4:6, `:=`(hbpro = 7.6, hbmol = 59, n = 9.5)][
    7:9, `:=`(hbpro = 7.3, hbmol = 57, n = 9.5)][
      10:12, `:=`(hbpro = 7.5, hbmol = 58, n = 9.5)][
        13:15, `:=`(hbpro = 7.4, hbmol = 57, n = 9.5)][
          16:18, `:=`(hbpro = 7.4, hbmol = 57, n = NA)]

alsundDT

## Elverum
elvDT = data.table( avd = "Elverum",
                       dato = c(seq(ymd('2017-10-01'), ymd('2017-12-01'), by='months'),
                                seq(ymd('2018-01-01'), ymd('2018-12-01'), by='months'),
                                seq(ymd('2019-01-01'), ymd('2019-03-01'), by='months')))

setkey(elvDT, "avd")
elvDT[1:3, `:=`(hbpro = 7.8, hbmol = 62, n = 11)][
  4:6, `:=`(hbpro = 8.1, hbmol = 65, n = 100)][
    7:9, `:=`(hbpro = 7.8, hbmol = 61, n = 112)][
      10:12, `:=`(hbpro = 7.5, hbmol = 58, n = 53)][
        13:15, `:=`(hbpro = 7.7, hbmol = 60, n = 46)][
          16:18, `:=`(hbpro = 7.4, hbmol = 57, n = NA)]

elvDT

dt3 <- rbindlist(list(haugDT, alsundDT, elvDT))
dt3[, dato := as.IDate(dato)]
setkey(dt3, avd)
dt3
## Alle data med måned
## -------------------

dtMnd = rbindlist(list(DT, dt3), use.names = TRUE)
# dtMnd[, mnd := format(dato, "%b %y")]
dtMnd

## lage månedvis data
# saveRDS(dtMnd, "iqmonth.rds")


## Sum for alle verdi wide format
## -------------------------------
dtMnd2 = dcast(dtMnd, avd ~ dato, value.var = c("n", "hbpro", "hbmol"))
names(dtMnd2)
dtMnd2

hbNvn = grep("^hb", names(dtMnd2), value = TRUE)
hbNvn
dthb = dtMnd2[, lapply(.SD, mean, na.rm = TRUE), .SDcols=hbNvn]
dthb[, avd := "Alle"]
dthb

sumNvn = grep("^n", names(dtMnd2), value = T)
sumNvn
dtsum = dtMnd2[, lapply(.SD, sum, na.rm = TRUE), .SDcols=sumNvn]
dtsum
alleWide = cbind(dthb, dtsum)
setkey(alleWide, avd)


## dt med Alle wide format
## ------------------------

dtFinal = rbindlist(list(dtMnd2, alleWide), use.names = TRUE)

## Lage wide formart med summen verdi
# saveRDS(dtFinal, "medSumWide.rds")


## dt Long format
## ---------------
DTalle = melt(dtFinal, measure.vars = patterns("^n", "hbp", "hbm"), value.name = c("n", "hbpro", "hbmol"))
setkey(DTalle, avd)
DTalle

## Add måned
##----------
listMnd = dtMnd[, .N, keyby=dato]
listMnd[, id := 1:nrow(listMnd)]
varBort = "N"
listMnd[, (varBort) := NULL]

## kvartalvis
## -----------
listMnd[1:3, kvart := "4.kvart 2017"][
  4:6, kvart := "1.kvart 2018"][
    7:9, kvart := "2.kvart 2018"][
      10:12, kvart := "3.kvart 2018"][
        13:15, kvart := "4.kvart 2018"][
          16:18, kvart := "1.kvart 2019"][
            19, kvart := "2.kvart 2019"]

listMnd[, gp := .GRP, by=kvart]
listMnd


setkey(listMnd, id)

longAlle = listMnd[DTalle, on = c(id = "variable")]
longAlle
keyCols = c("avd", "dato")
longAlle[, Tid := format(dato, "%b %y")]
setkeyv(longAlle, keyCols)

longAlle[, `:=` (hbpro_gp = mean(hbpro, na.rm = T),
                 hbmol_gp = mean(hbmol, na.rm = T),
                 n_gp = sum(n, na.rm = T)), by=list(avd, kvart)]

## vise bare 2 digits
digVar = grep("hb", names(longAlle), value = T)
for (col in digVar) set(longAlle, j=col, value = round(longAlle[[col]], digits = 2))

nVar = c("n", "n_gp")
for (col in nVar) set(longAlle, j=col, value = round(longAlle[[col]], digits = 0))


# setnames(longAlle, c("hbpro", "hbmol", "hbpro_gp", "hbmol_gp"), c("HbA1c (%)", "HbA1c (mol)", "HbA1c (%) kvart.", "HbA1c (mol) kvart."))
longAlle


## Data per måned
## ---------------
saveRDS(longAlle, "iqmnd2.rds")
# longAlle = readRDS("iqmnd.rds")



## Plotting
## ---------

library(ggplot2)

iqthemes <- theme(legend.title = element_blank(),
                  legend.text = element_text(size = 9),
                  legend.key = element_rect(fill = "white"),
                  axis.text = element_text(size = 9, color = "black"), #text for x og y axis
                  axis.ticks.y = element_blank(),
                  axis.line.x = element_line(size = 0.5),
                  axis.line.y = element_blank(),
                  axis.title.y = element_text(size = 11),
                  axis.title.x = element_text(size = 11),
                  panel.background = element_rect(fill = "white"),
                  panel.border = element_rect(linetype = 1, fill = NA, color = "white"),
                  panel.grid.minor.x = element_blank(),
                  panel.grid.major.y = element_line(linetype = 2, color = "grey"))

## data bare alle
alleDT <- longAlle[avd == "Alle", ]

## HbA1c %
hbGgAlle = ggplot(alleDT, aes(dato, hbpro, color = avd, label = Tid, text = paste('Sykehus: ', avd))) + 
  geom_point(aes(size=n),  stroke = 0, shape = 16) + 
  geom_line() +
  labs(title = "HbA1c (%) per sykehus i IQ prosjekt", x = " ", y = "HbA1c verdi i %") +
  scale_x_date(labels = function(x) format(x, "%b %y"), breaks = '2 months') +
  scale_size_continuous(range = c(1,10)) +
  # scale_colour_brewer(palette = "Set1") +
  iqthemes


hbGgAlle + guides(size = FALSE)
ggsave("hba1c_mnd_alle.png")


## HbA1c %
hbGg = ggplot(longAlle, aes(dato, hbpro, color = avd, label = Tid, text = paste('Sykehus: ', avd))) + 
  geom_point(aes(size=n),  stroke = 0, shape = 16) + 
  geom_line() +
  labs(title = "HbA1c (%) per sykehus i IQ prosjekt", x = " ", y = "HbA1c verdi i %") +
  scale_x_date(labels = function(x) format(x, "%b %y"), breaks = '2 months') +
  scale_size_continuous(range = c(1,10)) +
  # scale_colour_brewer(palette = "Set1") +
  iqthemes

hbGg + guides(size = FALSE)
ggsave("hba1c_mnd2.png")

hbGplot=plotly::ggplotly(hbGg, tooltip = c("hbpro", "Tid", "text", "n"))
hbGplot

library(htmlwidgets)
htmlwidgets::saveWidget(hbGplot, "hba1c2.html")

## HbA1c mol
hbmolGg = ggplot(longAlle, aes(dato, hbmol, color = avd, label = Tid, text = paste('Sykehus: ', avd))) + 
  geom_point(aes(size=n),  stroke = 0, shape = 16) + 
  geom_line() +
  labs(title = "HbA1c (mol) per sykehus i IQ prosjekt", x = " ", y = "HbA1c verdi i mol") +
  scale_x_date(labels = function(x) format(x, "%b %y"), breaks = '2 months') +
  scale_size_continuous(range = c(1,8)) +
  iqthemes

hbmolGg
hbmolGplot=plotly::ggplotly(hbmolGg, tooltip = c("hbmol", "Tid", "text", "n"))
hbmolGplot

htmlwidgets::saveWidget(hbmolGplot, "hba1cmol2.html")

## Kvartaler
## ---------------
## HbA1c %

## Alle 
alleDT

hbGgkvtAlle = ggplot(alleDT, aes(reorder(kvart, gp), hbpro_gp, color = avd, group = avd, text = paste('Sykehus: ', avd))) + 
  geom_point(aes(size=n_gp),  stroke = 0, shape = 16) + 
  geom_line() + geom_text(aes(label = hbpro_gp), color = "black", hjust = 0, nudge_x = 0.1) +
  ylim(7, 8) +
  labs(title = "HbA1c (%) deltagende avdelinger i IQ prosjekt", x = "Kvartaler", y = "HbA1c verdi i %") +
  # scale_x_date(labels = function(x) format(x, "%b %y"), breaks = '2 months') +
  scale_size_continuous(range = c(1,8)) +
  iqthemes



hbGgkvtAlle + guides(size = FALSE) + theme(legend.position = 'none')
ggsave("hba1c_kvart2_alle.png")

# hbGgAlle = ggplot(alleDT, aes(reorderdato, hbpro, color = avd, label = Tid, text = paste('Sykehus: ', avd))) + 
#   geom_point(aes(size=n),  stroke = 0, shape = 16) + 
#   geom_line() +
#   labs(title = "HbA1c (%) per sykehus i IQ prosjekt", x = " ", y = "HbA1c verdi i %") +
#   scale_x_date(labels = function(x) format(x, "%b %y"), breaks = '2 months') +
#   scale_size_continuous(range = c(1,10)) +
#   # scale_colour_brewer(palette = "Set1") +
#   iqthemes
# 
# 
# hbGgAlle + guides(size = FALSE)
# ggsave("hba1c_mnd_alle.png")


## Per syhekus
## -------------

# ggplot(longAlle, aes(reorder(kvart, gp), hbpro_gp, color = avd, group = avd)) +
#   geom_point() +
#   geom_line()

hbGgkvt = ggplot(longAlle, aes(reorder(kvart, gp), hbpro_gp, color = avd, group = avd, text = paste('Sykehus: ', avd))) + 
  geom_point(aes(size=n_gp),  stroke = 0, shape = 16) + 
  geom_line() +
  labs(title = "HbA1c (%) per sykehus i IQ prosjekt", x = "Kvartaler", y = "HbA1c verdi i %") +
  # scale_x_date(labels = function(x) format(x, "%b %y"), breaks = '2 months') +
  scale_size_continuous(range = c(1,8)) +
  iqthemes
  

hbGgkvt + guides(size = FALSE)
ggsave("hba1c_kvart2.png")


hbGplotkvt=plotly::ggplotly(hbGgkvt, tooltip = c("hbpro_gp", "kvart", "text", "n_gp"))
hbGplotkvt

htmlwidgets::saveWidget(hbGplotkvt, "hba1ckvart2.html")


## Combine % og mol
## ----------------

# rescaling <- max(longAlle$hbpro_gp, na.rm = T) / max(longAlle$hbmol_gp, na.rm = T)

## lage grense
ylim.sec <- c(6, 8.5)
ylim.prim <- c(48, 70)

b <- diff(ylim.prim)/diff(ylim.sec)
b
a <- b*(ylim.prim[1] - ylim.sec[1])
a

p <- ggplot(longAlle, aes(x = reorder(kvart, gp)))
p <- p + geom_line(aes(y = hbmol_gp, color = avd, group = avd))
p
# p <- p + geom_line(aes(y = hbmol_gp, color = avd))
# p
p <- p + scale_y_continuous(sec.axis = sec_axis(~./8.2, name = "HbA1c prosent", breaks = seq(5, 10, 0.5)))
p

ggplot(longAlle, aes(x = reorder(kvart, gp))) +
  geom_point(aes(y = hbmol_gp, color = avd))


ggplot(longAlle, aes(x = reorder(kvart, gp))) +
  geom_point(aes(y = hbpro_gp, color = avd))

## HbA1c mol
# 
# ggplot(longAlle, aes(reorder(kvart, gp), hbmol_gp, color = avd, group = avd)) +
#   geom_point() +
#   geom_line()

hbmolGgkvt = ggplot(longAlle, aes(reorder(kvart, gp), hbmol_gp, color = avd, group = avd, text = paste('Sykehus: ', avd))) + 
  geom_point(aes(size=n_gp),  stroke = 0, shape = 16) + 
  geom_line( ) +
  labs(title = "HbA1c (mmol/mol) per sykehus i IQ prosjekt", x = "Kvartaler", y = "HbA1c verdi i mmol/mol") +
  # scale_x_date(labels = function(x) format(x, "%b %y"), breaks = '2 months') +
  scale_size_continuous(range = c(1,8)) +
  iqthemes


hbmolGgkvt + guides(size = FALSE)
ggsave("hba1cmol_kvart2.png")

hbmolGplotkvt=plotly::ggplotly(hbmolGgkvt, tooltip = c("hbmol_gp", "kvart", "text", "n_gp"))
hbmolGplotkvt

htmlwidgets::saveWidget(hbmolGplotkvt, "hba1cMolkvart2.html")



###################################
## time series

# load data
longAlle <- readRDS("iqmnd2.rds")

# select relevant variables ie. Alle
dtts <- longAlle[avd == "Alle", .(dato, avd, hbpro_gp, hbmol_gp, n_gp), by=kvart]
dtts


## Cox stuart test
cox.stuart.test =  function (x)  {
    method = "Cox-Stuart test for trend analysis"
    leng = length(x)
    apross = round(leng) %% 2
    if (apross == 1) {
      delete = (length(x)+1)/2
      x = x[ -delete ] 
    }
    half = length(x)/2
    x1 = x[1:half]
    x2 = x[(half+1):(length(x))]
    difference = x1-x2
    signs = sign(difference)
    signcorr = signs[signs != 0]
    pos = signs[signs>0]
    neg = signs[signs<0]
    if (length(pos) < length(neg)) {
      prop = pbinom(length(pos), length(signcorr), 0.5)
      names(prop) = "Increasing trend, p-value"
      rval <- list(method = method, statistic = prop)
      class(rval) = "htest"
      return(rval)
    }
    else {
      prop = pbinom(length(neg), length(signcorr), 0.5)
      names(prop) = "Decreasing trend, p-value"
      rval <- list(method = method, statistic = prop)
      class(rval) = "htest"
      return(rval)
    }
  }

names(dtts)
hbproVec <- dtts[!duplicated(kvart), .(hbpro_gp)][[1]]
str(hbproVec)

testtrend=cox.stuart.test(hbproVec)
testtrend

# linear reg.
dtt2 <- dtts[!duplicated(kvart), ]
dtt2[, rnk := seq(1, nrow(dtt2))]
dtt2
model1 = lm(hbpro_gp ~ dato, data = dtt2)
model2 = lm(hbpro_gp ~ rnk, data=dtt2)
summary(model1)
summary(model2)




## Cox Stuart test function 2
o.Cox.Stuart.test <- function(x, alternative=c("two.sided" ,"left.sided", "right.sided")){
  
  dname <- deparse(substitute(x))
  
  alternative <- match.arg(alternative)
  
  stopifnot(is.numeric(x))
  
  n0 <- length(x)
  
  if (n0 < 2){
    stop("sample size must be greater than 1")
  }
  
  n0 <- round(length(x)) %% 2
  
  if (n0 == 1) {
    remove <- (length(x)+1)/2
    x <- x[ -remove ] 
  }
  
  half <- length(x)/2
  x1 <- x[1:half]
  x2 <- x[(half+1):(length(x))]
  n <- sum((x2-x1)!=0)
  t <- sum(x1<x2)
  
  if (alternative == "left.sided") {
    p.value <- pbinom(t, n, 0.5)
    alternative <- "decreasing trend"
  }
  
  if (alternative == "right.sided"){
    p.value <- 1-pbinom(t-1, n, 0.5)
    alternative <- "increasing trend"
  }
  
  if (alternative == "two.sided"){
    alternative <- "any type of trend, either decreasing or increasing" 
    if (1-pbinom(t-1, n, 0.5) == pbinom(t, n, 0.5)) {
      pdist <- dbinom(0:n, n, 0.5)
      p.value <- sum(pdist[pdist <= t+1])
    }
    else {
      p.value <- 2*min( 1-pbinom(t-1, n, 0.5), pbinom(t, n, 0.5))
    } 
  }
  
  rval <- list(statistic=c("Test Statistic"=t), 
               alternative=alternative, 
               p.value=p.value, 
               method="Cox & Stuart Test for Trend Analysis", 
               parameter=c("Number of Untied Pairs"=n),
               data.name=dname)
  
  class(rval) <- "htest"
  return(rval)
}



o.Cox.Stuart.test(hbproVec)


## cox stuart test with randtest package
library(randtests)

randtests::cox.stuart.test(hbproVec)
hbproVec

#######################
## Use Mann-Kendall

# monthly
mk_hbpro <- longAlle[avd == "Alle", .(hbpro)][[1]]
str(mk_hbpro)
longAlle[avd == "Alle", ]

randtests::cox.stuart.test(mk_hbpro)
o.Cox.Stuart.test(mk_hbpro)

# create time series data
ts_hbpro=ts(mk_hbpro, start = c(2017, 10), end = c(2019, 4), frequency = 12)
plot(ts_hbpro)

plot(ts_hbpro, col="blue")
lines(lowess(time(ts_hbpro), ts_hbpro), col="red", lwd=2)

# check serial correlation
dev.off()
par(mfrow=c(2,1))
acf(ts_hbpro)
pacf(ts_hbpro)

mk_result=Kendall::MannKendall(ts_hbpro)
print(mk_result)
summary(mk_result)

trend::mk.test(ts_hbpro)

#################
# quarterly
kv_hbpro=ts(hbproVec)
kv_hbpro
plot(kv_hbpro, col="blue")
lines(lowess(time(kv_hbpro), kv_hbpro), col="red", lwd=2)

mk_resKV=Kendall::MannKendall(kv_hbpro)
print(mk_resKV)
summary(mk_resKV)


tsDT <- dtts[!duplicated(kvart), .(kvart, hbpro_gp)]
kvarVec <- tsDT[, .(hbpro_gp)][[1]]
kvarVec
ts(kvarVec, start = c(2017, 10))


##################
## trend package

trend::mk.test(kv_hbpro)
