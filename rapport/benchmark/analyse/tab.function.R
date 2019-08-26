## Lager tabell med overordnet tittel
exp.tabel <- function(dt, navn, ncol = NULL,
                      size = 0.7, rap = FALSE, total = 0,
                      del = NULL,
                      tbo = NULL,
                      valgCol = NULL,
                      valgAlign = "right",
                      rowHeight = NULL,
                      mixCol = NULL){

  #navn - header label
  #ncol - 5 or 6 columns
  #rap - to fix with LaTeX column width
  #total -if Total=1 bottom Totalt else Total=2 bottom og right column
  #del - kolomnsdeling
  #tbo - kolumn for ordering
  #right - kolumn for right margin
  #valgCol - select coloumn to be align eg. c(1,3,5)
  #valgAlign - Align for valgCol
  #mixCol - which column to merge and underline

  tabXX <- as_hux(dt, add_colnames = TRUE)
  tabXX <- map_background_color(tabXX, by_rows("grey90", "white"))


  lastRow <- nrow(tabXX)
  lastCol <- ncol(tabXX)

  ## Adjust width eg. with 4 columns del = c(.4, .2, .2, .3)
  if (!is.null(del)){
    col_width(tabXX) <- del
  }

  ## Totalt position 1 - bottom only and 2 - bottom and end column
  if (total == 1) {
    bold(tabXX)[lastRow, ] <- TRUE
  }

  if (total == 2){
    bold(tabXX)[lastRow, ] <- TRUE
    bold(tabXX)[, lastCol] <- TRUE
  }

  ## Cells to merge e.g 2:5
  valgCell <- mixCol

  ## Number of columns
  if (!is.null(navn)){
    if (ncol == 7){
      tabXX <- rbind(c("", navn, "", "", "", "", ""), tabXX)
      bottom_border(tabXX)[1, valgCell] <- 0.5
      tabXX <- merge_cells(tabXX, 1, valgCell)
    }

    if (ncol == 6){
      tabXX <- rbind(c("", navn, "", "", "", ""), tabXX)
      bottom_border(tabXX)[1, valgCell] <- 0.5
      tabXX <- merge_cells(tabXX, 1, valgCell)
    }

    if (ncol == 5){
      tabXX <- rbind(c("", navn, "", "", ""), tabXX)
      bottom_border(tabXX)[1, valgCell] <- 0.5
      tabXX <- merge_cells(tabXX, 1, valgCell)
    }
  }


  ## Aligning columns
  if (!is.null(valgCol)){
    align(tabXX)[, valgCol] <- valgAlign
    }

  ## order colone
  if (!is.null(tbo)){
    tabXX <- tabXX[order(tabXX[[tbo]],decreasing = T),]
  }

  #tittle border and align
  bottom_border(tabXX)[2, ] <- TRUE
  top_border(tabXX)[lastRow + 1, ] <- TRUE
  align(tabXX)[1, ] <- "center"

  width(tabXX) <- size

  ## Row hight
  if (!is.null(rowHeight)){
    row_height(tabXX) <- rowHeight
  }

  if (rap){
  ## for at width funker i PDF så må 'wrap' være TRUE
  wrap(tabXX) <- TRUE
  }

  return(tabXX)

}



## tab.dtype

## dtypeTab <- as_hux(tab.dtype, add_colnames = TRUE)
## tabXX <- dtypeTab

## tabXX <- map_background_color(tabXX, by_rows("grey90", "white"))

## lastRow <- nrow(tabXX)
## lastCol <- ncol(tabXX)
## bold(tabXX)[lastRow, ] <- TRUE
## bold(tabXX)[, lastCol] <- TRUE

## tabXX <- rbind(c("", "Diabetes type", "", "", "", ""), tabXX)
## bottom_border(tabXX)[1, 2:5] <- 0.5
## bottom_border(tabXX)[2, ] <- TRUE #tittle border
## top_border(tabXX)[lastRow + 1, ] <- TRUE

## tab11 <- tabXX


## cellMix <- 2:5
## tab11 <- merge_cells(tab11, 1, cellMix)
## align(tab11)[1,] <- "center"

## quick_pdf(tab11, file = "test.pdf")
