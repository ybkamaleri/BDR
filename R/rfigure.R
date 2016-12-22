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
##' @param fname Filnavn for å lage figuren
##' @param format Figur format i.e "pdf", "jpg" eller "png"
##' @param path Hvor skal figuren lagres evt. hjemme område
##'
##'
##' @import dplyr
##' @import ggplot2
##'
##' @export

rfigure <- function(data = NULL, sykehus = NULL, rapValg = NULL, yAksen = 2,
                   xScale = 2, figT = "none", figTxt = "none", xBreaks = NULL,
                   fname = NULL, format = "pdf", path = NULL) {

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

    ## Dummy for figur data
    figdata  <- NULL #for lokal og land
    figdata2 <- NULL #for øvrige


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


    ## ===========
    ## Hele landet
    ## ===========

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

    ## =================
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
    figsubT = paste(figTitle, collapse = "\n")
    txtSpace = length(figTxt)
    ##txtSpace1 = length(figTxt) + 1
    ##figsub = paste(figTxt, collapse = "\n")

    ## if (txtSpace < 4) {
    ##     figtt <- paste0(figT, "\n\n")
    ## } else {
    ##     figtt <- paste0(figT, "\n\n\n" )
    ## }

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

    ## ========================
    ## Figur - Felles parameter
    ## ========================

    col1 <- "#6699CC"
    col2 <- "#000099"
    coll <- "#999999" #line color

    ## -- ymax for y-aksen -- ##
    maxx <-  max(figdata$yAksen, na.rm = TRUE)
    ym <- maxx/6
    ymax <- maxx + ym

    if (!is.null(figdata2)) {
        max1 <- max(figdata2$yAksen, na.rm = TRUE)

        if (max1 > maxx) {
            ym1 <- max1/6
            ymax <- max1 + ym1
        } else {
            ymax <- ymax
        }
    }


    ## figur
    ffig <- ggplot2::ggplot(NULL, aes(x = Variabel, y = yAksen)) +
        ggplot2::geom_bar(data = figdata, aes(fill = sykehusNavn), stat = "identity") +
        ggplot2::scale_fill_manual(name = " ", values = col1) +
        ggplot2::coord_cartesian(ylim = c(1,ymax)) +
        ggplot2::scale_y_continuous(expand = c(0,0)) +
        ggplot2::labs( x =xLab, y = yLab) +
        ggplot2::ggtitle(bquote(atop(.(""),atop(.(figsubT), ""))))

    ## theme uten sammenligne
    theme1 <- ggplot2::theme(plot.margin = unit(c(txtSpace, 1,1,1), "lines"),
                             plot.title = element_text(hjust = 0, size=15),
                             legend.position = 'top',
                             legend.text = element_text(size = 11),
                             legend.title = element_blank(),
                             legend.box = "horizontal",
                             panel.background = element_blank(),
                             panel.border = element_blank(),
                             panel.grid.major.y = element_line(color = coll, size = .3, linetype = "dashed"),
                             axis.title = element_text(face = "bold", size = 11),
                             axis.ticks.y = element_line(size = .3, color = coll),
                             axis.ticks.x = element_blank(),
                             axis.text = element_text(size = 10),
                             axis.text.y = element_text(vjust = 0),
                             axis.line.x = element_line(size =.5))


    ## ============================
    ## Figure for prosent og antall
    ## ============================


    ## -- Kategoriske variabler --##

    if (rapvalg %in% 1:2 & yAksen == 1 & xScale == 2) {

        figdata$yAksen=as.numeric(sprintf("%.1f", figdata$yAksen))

        regfig <- ffig +
            ggplot2::geom_text(data = figdata, aes(label = yAksen, vjust = -0.25)) +
            theme1
    }

    if (rapvalg %in% 1:2 & yAksen == 2 & xScale == 2) {

        figdata$yAksen=as.numeric(sprintf("%.f", figdata$yAksen))

        regfig <- ffig +
            ggplot2::geom_text(data = figdata, aes(label = yAksen, vjust = -0.25)) +
            theme1

    }

    if (rapvalg == 3 & yAksen == 1 & xScale == 2) {

        figdata2$yAksen=as.numeric(sprintf("%.1f", figdata2$yAksen))

        regfig <- ffig +
            ggplot2::geom_point(data = figdata2, aes(color = sykehusAndre),
                                shape = 18, size = 6, stat = "identity" ) +
            ggplot2::scale_color_manual(name = " ", values = col2) +
            ggplot2::guides(color = guide_legend(order = 2),
                            fill = guide_legend(order = 1)) +
            theme1

    }

    if (rapvalg == 3 & yAksen == 2 & xScale == 2) {

        figdata2$yAksen=as.numeric(sprintf("%.f", figdata2$yAksen))


        regfig <- ffig +
            ggplot2::geom_point(data = figdata2, aes(color = sykehusAndre),
                                shape = 18, size = 6, stat = "identity" ) +
            ggplot2::scale_color_manual(name = " ", values = col2) +
            ggplot2::guides(color = guide_legend(order = 2),
                            fill = guide_legend(order = 1)) +
            theme1

    }



    ## ================
    ## OUTPUT
    ## ================


    if (is.null(fname)) {
        print(regfig)
        ##ggsave(Save)
        ##dev.off()
    } else {
        wdir <- getwd()
        path <-  path %||% wdir
        ggplot2::ggsave(filename = fname, device = format)
        ##print(RegFig)
    }
}
