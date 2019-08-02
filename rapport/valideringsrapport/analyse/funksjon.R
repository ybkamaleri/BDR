## Samling av forskjellige funksjoner som skal brukes i analysen

## Lager kategorisering for alder
ageCat <- function(x, lower, upper, by, sep = "-") {
  ## Finne høyeste kategori
  kat <- paste0(seq(lower + by - 1, upper - 1, by = by))
  indTop <- max(length(kat))
  top <- as.numeric(kat[indTop])

  labs <- paste0(c(paste(seq(lower, upper - by, by = by),
    seq(lower + by - 1, upper - 1, by = by),
    sep = sep),
    paste(top + 1, "+", sep = "")))
  cut(floor(x), breaks = c(seq(lower, upper, by = by), Inf),
    include.lowest = TRUE, right = FALSE, labels = labs)
}


## Lager tabell med 4 kolonner
tabFun <- function(dt, navn, size = 0.7, rap = FALSE, total = FALSE){

  lastLine <- nrow(dt) + 1
  tabhx <- as_hux(dt, add_colnames = TRUE)

  tabhx <- tabhx %>%
    set_bold(1,, TRUE) %>%
    map_background_color(by_rows("grey95", "white")) %>%
    set_position("left") %>%
    set_latex_float("h") %>%
    set_align(, 3, "right") %>%
    set_col_width(c(.4, .2, .2, .3))

  tabhx <- rbind(c("", navn, "", ""), tabhx)

  tabhx <- merge_cells(tabhx, 1, 2:3)

  tabhx <- tabhx %>%
    set_align(1, 2, "center") %>%
    set_bottom_border(1, 2, 0.5) %>%
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


## Lager tabell med 5 kolonner
tabHux <- function(dt, size = 0.7, rap = FALSE, total = FALSE, del = NULL){

  lastLine <- nrow(dt) + 1
  tabhx <- as_hux(dt, add_colnames = TRUE)

  tabhx <- tabhx %>%
    set_bold(1,, TRUE) %>%
    set_bottom_border(1,, TRUE) %>%
    map_background_color(by_rows("grey95", "white")) %>%
    set_position("left") %>%
    set_latex_float("h")



  if (total){
    bold(tabhx)[lastLine, ] <- TRUE
    top_border(tabhx)[lastLine, ] <- TRUE
  } else {
    bottom_border(tabhx)[lastLine,] <- TRUE
  }

  if (!is.null(del)){
    tabhx <- tabhx %>%
      set_col_width(del)
    ## col_width(tabhx) <- del
  }

  width(tabhx) <- size

  if (rap){
    ## for at width funker i PDF så må 'wrap' være TRUE
    wrap(tabhx) <- TRUE
  }

  return(tabhx)

}
