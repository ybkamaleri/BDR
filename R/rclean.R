##' Clean dataset
##'
##' Denne funksjonen skal filtrere datasettet som er spesifisert i datautvalget
##'
##' @param data Datasettet som er ferdig konvertert til R format
##' @return Renset datasettet til videre behandling
##'
##' @export


rclean <- function(data = NULL) {

    if (is.null(data)) {
        impData <- rfile()
        RegData <- impData$dataFile
        filType <- impData$filType
    } else {
        if (!is.data.frame(data)) {stop (data, "Should be an R data.frame format!", call. = FALSE)}
        RegData <- data
        filType <- 1
    }


    ## white space
    trim <- function( x ) {
        gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
    }


    ## Convert dates for different file format
    if (filType == "sav") {

        ## År
        ## RegData$Year <- as.numeric(format(as.POSIXct(RegData$inn_Dato, format = "%Y-%m-%d %H:%M"), "%Y"))
        RegData$Year <- as.numeric(format(as.POSIXct(RegData$inn_Dato, origin = "1899-12-30 0:00:00",
                                                     format ="%Y-%m-%d %H:%M"), "%Y"))
        RegData$innYear <- as.Date(format(as.POSIXct(RegData$inn_Dato, origin = "1899-12-30 0:00:00",
                                                     format ="%Y-%m-%d %H:%M"), "%Y-%m-%d"))
        RegData$diagYear <- as.Date(format(as.POSIXct(RegData$inn_DiagDato, origin = "1899-12-30 0:00:00",
                                                      format ="%Y-%m-%d %H:%M"), "%Y-%m-%d"))

        ## Fødselsdato
        RegData$FDato1 <- as.POSIXct(RegData$FDato, origin = "1899-12-30 0:00:00")

    } else {

        RegData$Year <- as.numeric(format(as.POSIXct(RegData$inn_Dato, format ="%Y-%m-%d %H:%M"), "%Y"))
        RegData$innYear <- as.Date(format(as.POSIXct(RegData$inn_Dato, format ="%Y-%m-%d %H:%M"), "%Y-%m-%d"))
        RegData$diagYear <- as.Date(format(as.POSIXct(RegData$inn_DiagDato, format ="%Y-%m-%d %H:%M"), "%Y-%m-%d"))

        ## Fødselsdato
        RegData$FDato1 <- as.POSIXct(RegData$FDato)

    }

    ## Inn_Type = regValg
    RegData$ValgtData2 <- trim(RegData$inn_Type)
    ## RegData$ValgtData <- gsub(" $", "", RegData$inn_Type, perl = T)
    RegData$regValg[RegData$ValgtData2=="Førstegangsregistrering"] <- 1
    RegData$regValg[RegData$ValgtData2=="Årskontroll"] <- 2
    RegData$regValg[RegData$ValgtData2=="Poliklinisk kontroll"] <- 3
    RegData$regValg <- as.factor(RegData$regValg)

    ## lab_HbA1cAkerVerdi = hba
    RegData$hba <- as.numeric(trim(RegData$lab_HbA1cAkerVerdi))

    ## Diabetes varighet
    RegData$diaVarighet <- floor(difftime(RegData$innYear, RegData$diagYear, units = "days")/365)
    RegData$diaVarighet <- as.numeric(RegData$diaVarighet)

    ## Kjønn = kjonn
    RegData$kjonn1 <- as.factor(trim(RegData$Kjonn))
    RegData$kjonn <- NA
    RegData$kjonn[RegData$kjonn1=="Gutt"] <- 1
    RegData$kjonn[RegData$kjonn1=="Jente"] <- 2
    RegData$kjonn <- factor(RegData$kjonn)

    ## Diabetes Type 1
    RegData$diabetes_Type1<- trim(RegData$diabetes_Type1)
    RegData$diaType1 <- ifelse(RegData$diabetes_Type1 == "Ja", 1, 2) #1:Type1 2:AndreType

    ## Alder
    RegData$Alder <- as.integer(floor(difftime(Sys.time(),
                                               as.POSIXct(RegData$FDato1,
                                                          format = "%Y-%m-%d %H:%M"), units = "days")/365))

    ##  Alder del i kategorier
    alder.kat <- function(x, lower, upper, by,
                          sep = "-") {
        labs <- c(paste(seq(lower, upper - by, by = by),
                        seq(lower + by - 1, upper - 1, by = by),
                        sep = sep),
                  paste(upper, "+", sep = ""))
        cut(floor(x), breaks = c(seq(lower, upper, by = by), Inf),
            include.lowest = TRUE, right = FALSE, labels = labs)
    }

    RegData$AlderKat <- alder.kat(RegData$Alder, 0, 30, 5)


    ## SykehusNavn , SykehusKode
    RegData$SykehusNavn <- trim(RegData$Sykehus)
    RegData$SykehusKode <- NA
    RegData$SykehusKode[RegData$SykehusNavn=="Ullevål universitetssykehus"] <- 1
    RegData$SykehusKode[RegData$SykehusNavn=="Haugesund sjukehus"] <- 18
    RegData$SykehusKode[RegData$SykehusNavn=="Sykehuset i Vestfold, Tønsberg"] <- 15
    RegData$SykehusKode <- factor(RegData$SykehusKode)

    return(invisible(RegData))
}
