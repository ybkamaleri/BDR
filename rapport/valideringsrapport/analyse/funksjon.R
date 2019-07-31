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
tabFun <- function(dt, navn, total = FALSE){

  lastLine <- nrow(dt) + 1
  tabhx <- as_hux(dt, add_colnames = TRUE)

  tabhx <- tabhx %>%
    set_bold(1,, TRUE) %>%
    set_bottom_border(lastLine,, TRUE) %>%
    map_background_color(by_rows("grey95", "white")) %>%
    set_position("left") %>%
    set_latex_float("h")

  tabhx <- rbind(c("", navn, "", ""), tabhx)

  tabhx <- merge_cells(tabhx, 1, 2:3)

  tabhx <- tabhx %>%
    set_align(1, 2, "center") %>%
    set_bottom_border(1, 2, 0.5) %>%
    set_top_border(3,, TRUE)

  if (total){
    bold(tabhx)[lastLine, ] <- TRUE
    bottom_border(tabhx)[lastLine,] <- FALSE
    top_border(tabhx)[lastLine, ] <- TRUE
  }

  return(tabhx)

}
