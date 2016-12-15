##' Konvertering datasettet til R format
##'
##' Denne funksjon skal konvertere datafil fra forskjellige formater f.eks csv eller sav
##' til en format som R kan håndtere
##'
##' @param filnavn Filnavn hentet fra register database
##' @return Et dataset i R data.frame format
##'
##' @export

rfile <- function(filnavn = NULL) {

    ## Hente fil og konvertert til R data.frame
    if (is.null(filnavn)) { filnavn = Filnavn }

    filType <- tools::file_ext(filnavn)

    ## SPSS file
    if (filType == "sav") {
        dataFile <- foreign::read.spss(filnavn, to.data.frame = TRUE, reencode = "UTF-8")
    }

    ## DAT eller TXT fil - tab-delimited
    if (filType %in% c("dat", "txt")) {
        dataFile <- read.table(filnavn,
                               header = TRUE,
                               encoding = 'UTF-8',
                               stringsAsFactors = TRUE,
                               na.strings = "EMPTY")
    }

    ## CSV file
    if (filType == "csv") {
        dataFile <- read.csv(filnavn,
                            header = TRUE,
                            encoding = 'UTF-8',
                            sep=";",
                            strip.white = TRUE,
                            stringsAsFactors = TRUE,
                            na.strings = "EMPTY") #indicate empty as NA
    }

    ## EXCEL file
    if (filType %in% c("xlsx", "xls")) {
        stop ("Data has wrong format. Only csv, sav, dat or txt format is accepted", call. = FALSE)
    }

    rData <- list(dataFile = dataFile, filType = filType)
    return(invisible(rData))
}
