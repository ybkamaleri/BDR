##' Filtrer datasett til videre analyser
##'
##' Produserer et datasett basert på utvalgte parametrer før videre analyser
##' eller visualisering.
##'
##' @note \code{rfile} og \code{rclean} funksjoner er kjørt før datasettet
##' blir brukt på denne funksjonen
##'
##' @param minAlder Minimum alder
##' @param maxAlder Maksimum alder
##' @param datoFra Tidligste dato i utvalget
##' @param datoTil Seneste dato i utvalget
##' @param kjonn Kjønn
##' @param dbType Diabetes type
##' @param dataValg Valg av data f.eks. Førstegangsregister, Årskontroll etc
##' @return List datasett
##' @details Denne funksjonen produserer disse datasetter:
##' \itemize{
##'  \item \code{fileSelect} {Utvalgte parametre for filtrering}
##'  \item \code{*vec} {Vektor for utvalgte parametrene}
##' }
##'
##'
##' @export

rfilter <- function(data = NULL, minAlder = NULL, maxAlder = NULL, datoFra = NULL, datoTil = NULL,
                     kjonn = NULL, dbType = NULL, dataValg = NULL) {


    if (is.null(data)) {
        data <- rclean()
    } else {
        if (!is.data.frame(data)) {stop (data, "Should be an R data.frame format!", call. = FALSE)}
        data <- data
    }

    ## Ifelse %||%
    "%||%" <- function(a,b) if (!is.null(a)) a else b

    ## Omdefinere variabel
    minAlder <- minAlder %||% MinAlder
    maxAlder <- maxAlder %||% MaxAlder
    datoFra <- datoFra %||% DatoFra
    datoTil <- datoTil %||% DatoTil
    kjonn <- kjonn %||% Kjonn
    dbType <- dbType %||% DBType
    dataValg <- dataValg %||% DataValg

    ##------------------
    ## Filter
    ##------------------

    ## Alder
    alderVec = minAlder:maxAlder

    ## Kjonn
    kjonnVec <- if (kjonn %in% 1:2) {kjonn} else { kjonnVec = 1:2 }

    ## Dato (datoFra, datoTil)
    ## datoVec <- seq(as.Date(datoFra, format = "%Y-%m-%d", origin = "1899-12-30"),
    ##                as.Date(datoTil, format = "%Y-%m-%d", origin = "2899-12-30"), "day")
    datoVec <- seq(as.Date(datoFra, format = "%Y-%m-%d"),
                   as.Date(datoTil, format = "%Y-%m-%d"), "day")

    ## Diabetes type 1
    diaVec <- if (dbType == 1) {dbType} else {diaVec = 1:2}

    ## Registrering (dataValg) Førstegangreg, årskontroll, poliklinisk etc
    regVec <- if (dataValg %in% 1:3) {dataValg} else {regVec <- 1:3}

    fileSelect <- lazyeval::interp(~var1 %in% alderVec &
                                       var2 %in% kjonnVec &
                                       var3 %in% datoVec &
                                       var4 %in% regVec &
                                       var5 %in% diaVec,
                                   .values = list(var1 = as.name("Alder"),
                                                  var2 = as.name("kjonn"),
                                                  var3 = as.name("innYear"),
                                                  var4 = as.name("regValg"),
                                                  var5 = as.name("diaType1")))

    ## Filtret data
    fdata <- dplyr::filter_(.data = data, .dots = fileSelect)
    ndata <- dim(fdata)[1]

    ##-------------------
    ## Figurtekst
    ##-------------------

    ## For scale-x values
    minX <- as.integer(if (min(fdata$Alder, na.rm = T) > minAlder) {min(fdata$Alder, na.rm = T)} else {minAlder})
    maxX <- as.integer(if (max(fdata$Alder, na.rm = T) < maxAlder) {max(fdata$Alder, na.rm = T)} else {maxAlder})

    ## Tekst
    figTxt <- c(paste0('Data', if (dataValg %in% 1:4){paste0(': ', c('Førstegangsregistrering',
                                                                     'Årskontroll',
                                                                     'Poliklinisk',
                                                                     'Alle type kontroller')[dataValg])}),
                if ((minAlder>0) | (maxAlder<100))
                {paste0('Pasienter fra ', minAlder, ' til ', maxAlder, ' år ')},

                if (kjonn %in% 1:2)
                {paste0('Kjønn: ', c('Gutter', 'Jenter')[kjonn])},

                if (dbType %in% 1:2)
                {paste0('Diabetes: ', c('Type 1', 'Alle typer')[dbType])},

                if (ndata>0)
                {paste0('Periode: ', if (min(fdata$innYear, na.rm = T) > datoFra) {
                                         min(fdata$innYear, na.rm = T)
                                     } else {datoFra}, ' til ',
                        if (max(fdata$innYear, na.rm = T) < datoTil) {
                            max(fdata$innYear, na.rm = T)
                        } else {datoTil})}
                )


    data <- list(fdata = fdata, figTxt = figTxt, minX = minX, maxX = maxX)
    return(invisible(data))
}
