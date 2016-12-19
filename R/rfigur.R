##' Beregn og Figur
##'
##' Her skal det beregne antall, andel eller andre typer beregninger som trenges før figuren skal lages
##'
##' @param data Datasettet
##' @param sykehus Utvalgte sykehus (lokal)
##' @param rapValg Rapport valgt dvs. 1:Lokal, 2:Landet og 3:Lokal vs. øvrige
##' @param yAksen Y aksen for figuren
##' @param xScale Skale å velge dvs. 1 = kontinuell og 2 = kategorisk
##' @param figT Title for figur
##' @param figTxt Tekst info om inholde i data for figuren
##' @param xBreaks Navnene for variabler i x-aksen
##' @param save Filnavn for å lage figuren
##' @param format Figur format i.e "pdf", "jpg" eller "png"
##' @param path Hvor skal figuren lagres evt. hjemme område
##'
##'
##' @import dplyr
##' @import ggplot2
##'
##' @export

rfigur <- function(data = NULL, sykehus = NULL, rapValg = NULL, yAksen = 2,
                   xScale = 2, figT = "none", figTxt = "none", xBreaks = NULL,
                   save = NULL, format = "pdf", path = NULL) {

    if (is.null(data)) {
        data <- rselect()
        RegData <- data$data
        xScale <- data$xScale
        figT <- data$figT
        figTxt <- data$figTxt
        xLab <- data$xLab
        xBreaks <- data$xBreaks
    } else {
        RegData <- data
        xScale = xScale
        figT = figT
        figTxt = figTxt
        xLab = xLab
        xBreaks = xBreak
    }

    ## Ifelse %||%
    "%||%" <- function(a,b) if (is.null(a)) 1 else b

    ## Omdefinere variablene
    sykehus     <- sykehus %||% Sykehus
    rapvalg <- rapValg %||% RapportValg
    yAksen      <- yAksen %||% YAksen


##########################################################
### Data utvalg for å sammenligne lokal sykehus mot andre
##########################################################

    ## Valg av sykehuset 1:Lokal 2:Andre
    samSyk <- lazyeval::interp(~f(var1 %in% sykehus, 1, 2),
                               .values = list(f = as.name("ifelse"),
                                              var1 = as.name("SykehusKode")))
    group <- "SykehusValg"
    rapdata <- dplyr::mutate_(.data = RegData,
                              .dots = setNames(list(samSyk), group))


    ## =======================
    ## Lokal og andre sykehus
    ## =======================


    ## Lokal vs. andre sykehus datasettet
    rapdata1 <- dplyr::filter(rapdata, SykehusValg == 1) #lokal
    rapdata2 <- dplyr::filter(rapdata, SykehusValg == 2) #andre


    if (rapvalg %in% c(1,3) & yAksen %in% 1:2) {

        ## -- LOKAL -- ##
        ## prosent
        figdata <- rapdata1 %>%
            group_by(Variabel) %>%
            tally %>%
            mutate(yAksen = (100*n/sum(n)),
                   samSyk = 1)


        ## antall
        if (yAksen == 2) {
            figdata <- figdata %>%
                select(Variabel, samSyk, yAksen = n)
        } else {
            figdata <- figdata
        }


        ## -- ANDRE -- ##
        if (rapvalg == 3 ) {
            ## prosent
            figdata2 <- rapdata2 %>%
                group_by(Variabel) %>%
                tally %>%
                mutate(yAksen = (100*n/sum(n)),
                       samSyk = 2)


            ## antall
            if (yAksen == 2) {
                figdata2 <- figdata2 %>%
                    select(Variabel, samSyk, yAksen = n)
            } else {
                figdata2 <- figdata2
            }
        }

    }


    ## =========
    ## Hele landet
    ## =========

    if (rapvalg == 2 & yAksen %in% 1:2) {

        ## prosent
        figdata <- rapdata %>%
            group_by(Variabel) %>%
            tally %>%
            mutate(yAksen = (100*n/sum(n)),
                   samSyk = 1)


        ## antall
        if (yAksen == 2) {
            figdata <- figdata %>%
                select(Variabel, samSyk, yAksen = n)
        } else {
            figdata <- figdata
        }
    }

    ## ================
    ## Sykehus navn og N
    ## =================

    if (rapvalg %in% c(1,3)) {
        sykNavn <- rapdata1$SykehusNavn[rapdata1$SykehusKode == sykehus][1]

        N <- dim(rapdata1)[1]
        andreN <- dim(rapdata2)[1]

        sykehusNavn <- paste0(sykNavn, " (N = ", N, ") ")
        sykehusAndre <- paste0("Øvrige sykehus (N = ", andreN, ") ")
    }

    if (rapvalg == 2) {
        N <- dim(rapdata)[1]
        sykehusNavn <- paste0("Hele landet (N = ", N, ") ")
    }

    ## ===============
    ## Tekst til Figur
    ## ===============

    titBlank <- ""
    figTitle <- c(figT, figTxt)
    figSubT = paste(figTitle, collapse = "\n")
    txtSpace = length(figTxt)
    txtSpace1 = length(figTxt) + 2

    if (yAksen == 1) yLab="Prosent (%)"
    if (yAksen == 2) yLab="Antall pasienter"


    if (yAksen == 3) {
        maalvar = "hba"
        rapdata <- dplyr::select(rapdata, -diaVarighet)
        yLab = "Gjennomsnitt HbA1c (95% CI)"
    }

    if (yAksen == 4) {
        maalvar = "diaVarighet"
        rapdata <- dplyr::select(rapdata, -hba)
        yLab = "Diabetesvarighet (år)"
    }

    ## ====================
    ## Figur - Felles
    ## ====================

    col1 <- "#6699CC"
    col2 <- "#000099"

    ## theme uten sammenligne
    theme1 <- ggplot2::theme(
                           axis.text.y = element_text(size = 9, color = "black"),
                           plot.margin = unit(c(0,2,0.5,0.5), "cm"),
                           plot.title = element_text(size = 10),
                           panel.background = element_blank(),
                           panel.border = element_blank(),
                           panel.grid.major.x = element_blank(),
                           panel.grid.minor.y = element_blank(),
                           panel.grid.major.x = element_blank(),
                           axis.ticks.y = element_blank(),
                           axis.line.x = element_line(size = 0.5),
                           axis.title.y = element_text(size = 9),
                           axis.title.x = element_text(size = 9))



    ## ===================
    ## Figure - Kategorisk
    ## ===================

    if (xScale == 2) {

        if (yAksen==1) RegDataPA$yAksen=as.numeric(sprintf("%.1f", RegDataPA$yAksen))
        if (yAksen==2) RegDataPA$yAksen=as.numeric(sprintf("%.f", RegDataPA$yAksen))

        RegFig <- ggplot(RegDataPA, aes(x=Variabel, y=yAksen, fill = factor(samSyk))) +
            geom_bar(stat = "identity") +
            expand_limits(x = 0, y = 0) +
            ylab(yLab) + xlab(xLab) +
            scale_fill_manual(name="", values = "#6699CC", label=sykehusNavn) +
        geom_text(aes(label=yAksen), vjust=-0.25, colour = "black") +
            ggtitle(bquote(atop(.(titBlank),atop(.(figSubT), "")))) +

    theme(plot.margin = unit(c(txtSpace,1,1,1), "lines"),
          plot.title = element_text(hjust = 0, size=18),
          axis.title = element_text(face = "bold", size = 12),
          legend.position = 'top',
          legend.text = element_text(size = 12),
          legend.title = element_blank(),
          axis.text = element_text(size = 11),
          axis.line = element_line(size =.3, color = "#333333"),
          panel.grid.major = element_line(color = "#CCCCCC",
                                          linetype = 2),
          panel.border = element_blank())



        figtest <- ggplot(figdata, aes(x = Variabel, y = yAksen, fill = factor(samSyk))) +
            geom_bar(stat = "identity") +
            scale_fill_manual(name = "", values = col1, label = sykehusNavn) +
            labs(x =xLab, y = yLab) +
            ggtitle(bquote(atop(.(paste0(figT, "\n\n")),atop(.(figsub), "")))) +
            theme(plot.margin = unit(c(txtSpace1, 1,1,1), "lines"),
                  plot.title = element_text(hjust = 0, size=18),
          axis.title = element_text(face = "bold", size = 12),
          legend.position = 'top',
          legend.text = element_text(size = 12),
          legend.title = element_blank(),
          axis.text = element_text(size = 11),
          axis.line = element_line(size =.3, color = "#333333"),
          panel.grid.major = element_line(color = "#CCCCCC",
                                          linetype = 2),
          panel.border = element_blank())


    }




}
