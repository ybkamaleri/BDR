##' Velger variabel til figur
##'
##' Her skal utvalgte variabel videre behandlet for figuren er lages
##'
##' @param data Datasettet
##' @param valgtVar Utvalgte variabel til figuren
##' @param figTx Tekst som beskriver data i figuren om funksjonen brukes direkte
##' @param minx Minimum tall for x-aksen
##' @param maxx Maksimum tall for x-aksen
##'
##' @import dplyr
##'
##' @export


rselect <- function(data = NULL, valgtVar = NULL, figTx = "none", minx = 0, maxx = 99) {

    ##Hente data
    if (is.null(data)) {
        data <- rfilter()
        RegData <- data$fdata
        figTxt <- data$figTxt
        minX <- data$minX
        maxX <- data$maxX
    } else {
        if (!is.data.frame(data)) {stop (data, "Should be an R data.frame format!", call. = FALSE)}
        RegData <- data
        figTxt <- figTx
        minX <- minx
        maxX <- maxx
    }

    if (is.null(valgtVar)) {
        valgtVar <- ValgtVar
    } else {
        valgtVar <- valgtVar
    }

################################
### Sammenligne sykehus funksjon
################################

    sykSamlikFn <- function(x) {
        sykSamlik = c("SykehusKode", "SykehusNavn", "hba", "diaVarighet", "Variabel")
        RegData$Variabel <- RegData[,x]
        RegDataValg <- RegData %>%
            select_(.dots = sykSamlik)
        return(RegDataValg)
    }


##########################
### Valgt Variabel
##########################

    ## === Kontinuelle Variabler (xScale==1) === ##

    if (valgtVar == "alder") {
        xScale = 1
        valgtVar = "Alder"
        RegDataValg <- sykSamlikFn(valgtVar)
        figT <- "Fordeling av alder"
        xLab = "Alder (år)"
        xBreaks = c(minX:maxX)
    }


    ## === Kategoriske Variabler (xScale==2) === ##

    if (valgtVar == "alderkat") {
        xScale = 2
        valgtVar = "AlderKat"
        RegDataValg <- sykSamlikFn(valgtVar)
        figT <- "Fordeling av alder i kategorier"
        xLab = "Alderskategorier (år)"
        xBreaks = levels(RegDataValg$Variabel)
    }

    if (valgtVar == "kjonn") {
        xScale = 2
        RegDataValg <- sykSamlikFn(valgtVar)
        levels = 1:2
        labels = c("Gutt", "Jente")
        RegDataValg$Variabel <- factor(RegDataValg$Variabel, levels = levels, labels = labels)
        figT <- "Fordeling av kjønn"
        xLab = "Kjønn"
        xBreaks = levels(RegDataValg$Variabel)
    }


#############################
### Overføre nødvendige data
#############################

    data <- list(data = RegDataValg,
                 xScale = xScale,
                 figT = figT,
                 figTxt = figTxt,
                 xLab = xLab,
                 xBreaks = xBreaks)

    return(invisible(data))

}
