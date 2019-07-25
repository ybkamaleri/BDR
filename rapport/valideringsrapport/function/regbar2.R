regbar2 <- function (data, x, y, comp, num, aim = NULL, split = NULL, sort,
                     title, ylab, col1, col2, col3, flip = TRUE, ...) {
  if (missing(data)) {
    stop("'data' must be provided", call. = FALSE)
  }
  if (missing(x) | missing(y)) {
    stop("Both 'x' and 'y' should be specified", call. = FALSE)
  }
  data$xvar <- data[, as.character(substitute(x))]
  data$yvar <- data[, as.character(substitute(y))]
  data$sortvar <- data[, as.character(substitute(sort))]

  if (missing(title)) {
    title <- ""
  } else {
    title = title
  }
  if (missing(num)) {
    data$.xname <- data$xvar
  }  else {
    num <- as.character(substitute(num))
    data$.xname <- sprintf("%s (N=%s)", data$xvar, data[,
      num])
  }
  if (missing(ylab)) {
    ylab <- substitute(y)
  }  else {
    ylab = ylab
  }
  ptheme <- theme_bw() + theme(axis.text = element_text(size = 10),
    axis.ticks.y = element_blank(), axis.line.x = element_line(size = 0.5),
    axis.title.y = element_blank(), axis.title.x = element_text(size = 12),
    plot.margin = unit(c(0, 2, 1, 1), "cm"), plot.title = element_text(size = 14),
    panel.background = element_blank(), panel.border = element_blank(),
    panel.grid.major.y = element_blank(), panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank())
  if (missing(col1)) {
    col1 <- "lightblue"
  } else {
    col1 = col1
  }
  if (missing(col2)) {
    col2 <- "#6baed6"
    colmix <- c(col1, col2)
  } else {
    colmix <- c(col1, col2)
  }
  if (is.null(split)) {
    ysplit <- with(data, 0.1 * max(yvar))
  } else {
    ysplit = with(data, split * max(yvar))
  }
  data$ypos <- ifelse(data$yvar > ysplit, 1, 0)
  position = position_dodge(width = 0.8)
  width = 0.8

  if (missing(sort)) {
    data$.xname <- with(data, factor(.xname, levels = .xname[order(yvar)]))
  } else {
    data$.xname <- with(data, factor(.xname, levels = .xname[order(sortvar)]))
  }

  p <- ggplot(data, aes(.xname, yvar))
  if (missing(col3)) {
    col3 = "blue"
  } else {
    col3 = col3
  }
  if (!is.null(aim)) {
    p <- p + geom_hline(yintercept = aim, color = col3, size = 1,
      linetype = "dashed")
  }
  if (missing(comp)) {
    p <- p + geom_bar(width = width, stat = "identity", fill = col1,
      position = position)
  } else {
    comp <- grep(comp, data$.xname, value = TRUE)
    p <- p + geom_bar(width = width, stat = "identity", aes(fill = .xname ==
                                                              comp), position = position)
  }
  p <- p + geom_text(data = data[which(data$ypos == 1), ],
    aes(label = yvar), hjust = 1.5, position = position,
    size = 3.5) + geom_text(data = data[which(data$ypos ==
                                                0), ], aes(label = yvar), hjust = -0.5, position = position,
                                                size = 3.5)
  p <- p + labs(title = title, y = ylab, x = "") + scale_fill_manual(values = colmix,
    guide = "none") + scale_y_continuous(expand = c(0, 0)) +
    ptheme
  if (flip) {
    p <- p + coord_flip()
  }
  return(p)
}
