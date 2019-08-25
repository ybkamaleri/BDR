## Lager tabell med overordnet tittel
tabFunx <- function(dt, navn, size = 0.7, rap = FALSE, total = FALSE,
                    del = NULL,
                    tbo = NULL,
                    right = NULL,
                    center = NULL,
                    mix = 2:3){

  #navn - header label
  #rap - to fix with LaTeX column width
  #total -if Total is in the tabel to be bolded
  #del - kolomnsdeling
  #tbo - kolumn for ordering
  #right - kolumn for right margin
  #center - kolumn for center margin
  #mix - which columns to merge

  lastLine <- nrow(dt) + 1
  tabhx <- as_hux(dt, add_colnames = TRUE)

  ## order colone
  if (!is.null(tbo)){
    tabhx <- tabhx[order(tabhx[[tbo]],decreasing = T),]
  }

  tabhx <- tabhx %>%
    set_bold(1,, TRUE) %>%
    map_background_color(by_rows("grey95", "white")) %>%
    set_position("left") %>%
    set_latex_float("h")

  if (!is.null(del)){
    col_width(tabhx) <- del
  }

  if (!is.null(right)){
    tabhx <- set_align(tabhx, right, "right")
    }

  tabhx <- rbind(c("", "", navn, "", ""), tabhx)

  tabhx <- merge_cells(tabhx, 1, mix)

  tabhx <- tabhx %>%
    set_align(1, center, "center") %>%
    set_bottom_border(1, center, 0.4) %>%
    set_top_border(3,, TRUE)

  if (total){
    bold(tabhx)[lastLine + 1, ] <- TRUE
    top_border(tabhx)[lastLine + 1, ] <- TRUE
  } else {
    bottom_border(tabhx)[lastLine + 1,] <- TRUE
  }

  width(tabhx) <- size

  if (rap){
  ## for at width funker i PDF så må 'wrap' være TRUE
  wrap(tabhx) <- TRUE
  }

  return(tabhx)

}
