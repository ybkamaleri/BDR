##' Velger variabel til figur
##'
##' Her skal utvalgte variabel videre behandlet for figuren er lages
##'
##' @param data Datasettet
##' @param valgtVar Utvalgte variabel til figuren
##'
##' @import dplyr
##'
##' @export


rselect <- function(data = NULL, valgtVar = NULL) {

    ##Hente data
    if (is.null(data)) {
        utData <- rfilter()
    } else {
        if (!is.data.frame(data)) {stop (data, "Should be an R data.frame format!", call. = FALSE)}
        utData <- data
    }

    RegData <- utData$fdata
    figTxt <- utData$figTxt
    minX <- utData$minX
    maxX <- utData$maxX

    ## Ifelse %||%
    "%||%" <- function(a,b) if (!is.null(a)) a else b

    ## Omdefinere variablene
    valgtVar    <- valgtVar %||% ValgtVar


################################
### Sammenligne sykehus funksjon
################################

    sykSamlikFn <- function(valgtValg) {
        sykSamlik = c("SykehusKode", "SykehusNavn", "hba", "diaVarighet", "Variabel")
        RegData$Variabel <- RegData[ , valgtValg]
        RegDataValg <- RegData %>%
            select_(.dots = sykSamlik)
        return(RegDataValg)
    }


##########################
### Valgt Variabel
##########################

    ##--- Kontinuelle Variabler (xScale==1) ---##

    if (valgtVar == "alder") {
        xScale = 1
        valgtVar = "Alder"
        RegDataValg <- sykSamlikFn(valgtVar)
        figT <- "Fordeling av alder"
        xLab = "Alder (år)"
        xBreaks = c(minX:maxX)
    }


    ## --- Kategoriske Variabler (xScale==2) ---##

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


    utData <- list(data = RegDataValg, xScale = xScale,
                   figT = figT, figTxt = figTxt, xLab = xLab, xBreaks = xBreaks)

    return(invisible(utData))

}
