##' Figur for antall og prosent
##'
##' @inheritParams rfigure
##' @param vs Sammenligne lokal og landet
##' @param figdata Lokal sykehus datasett for figur
##' @param figdata2 Øvrige sykehus datasettet for figur
##' @param Variabel Variabel for x-aksen
##' @param yAksen Variabel for y-aksen
##' @param N Antall for lokal sykehus inkludert i analysen
##' @param andreN Antall for øvrige sykehus inkludert i analysen
##' @param sykehusNavn Tekst til legend for lokal sykehus
##' @param sykehusAndre Tekst til legend for øvrige sykehus
##' @param ymax Max for y-aksen
##' @param xLab Label for x-aksen
##' @param yLab Label for y-aksen
##' @param figsubT Tekst for title til figuren
##' @param txtSpace Linje å lage til figur title
##' @param col1 Farge for søyle
##' @param col2 Farge for point
##' @param coll Farge for linje i figur
##'
##' @import ggplot2
##'
##' @export

rFigAntallProsent <- function(vs = TRUE, figdata = figdata, figdata2 = figdata2,
                              yAksen11 = yAksen, N = N, andreN = NULL,
                              sykehusNavn = sykehusNavn, sykehusAndre = NULL,
                              ymax = ymax, yLab = yLab, xLab = xLab,
                              figsubT = figsubT, txtSpace = txtSpace,
                              col1 = col1, col2 = col2, coll = coll) {

    ## justere desimal
    if (yAksen11 == 1) {
        figdata$yAksen=as.numeric(sprintf("%.1f", figdata$yAksen))
    } else {
        figdata$yAksen=as.numeric(sprintf("%.f", figdata$yAksen))
    }

    ## if (yAksen11 == 1) figdata$yAksen=as.numeric(sprintf("%.1f", figdata$yAksen))
    ## if (yAksen11 == 2) figdata$yAksen=as.numeric(sprintf("%.f", figdata$yAksen))

    if (!is.null(figdata2) & yAksen11 == 1) figdata2$yAksen=as.numeric(sprintf("%.1f", figdata2$yAksen))
    if (!is.null(figdata2) & yAksen11 == 2) figdata2$yAksen=as.numeric(sprintf("%.1f", figdata2$yAksen))

    ## basis figur
    ffig <- ggplot2::ggplot(data = figdata, aes(x = Variabel, y = yAksen11), environment = .e) +
        ggplot2::geom_bar(aes(fill = sykehusNavn), stat = "identity") +
        ggplot2::scale_fill_manual(name = " ", values = col1) +
        ggplot2::coord_cartesian(ylim = c(1,ymax)) +
        ggplot2::scale_y_continuous(expand = c(0,0)) +
        ggplot2::labs( x =xLab, y = yLab) +
        ggplot2::ggtitle(bquote(atop(.(""),atop(.(figsubT), ""))))

    ## theme uten sammenligne
    theme1 <- ggplot2::theme(plot.margin = unit(c(txtSpace, 1,1,1), "lines"),
                             plot.title = element_text(hjust = 0, size=1),
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



    list(

        regfig <- ffig +
            ggplot2::geom_text(data = figdata, aes(label = yAksen11, vjust = -0.25)) +
            theme1,

        if (vs)

            regfig <- ffig +
            ggplot2::geom_point(data = figdata2, aes(color = sykehusAndre),
                                shape = 18, size = 6, stat = "identity") +
            ggplot2::scale_color_manual(name = " ", values = col2) +
            ggplot2::guides(color = guide_legend(order = 2),
                            fill = guide_legend(order = 1)) +
            theme1
    )

    return(invisible(regfig))
}
