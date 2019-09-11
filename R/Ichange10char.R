change10char <- function(xnms){

  ## DESCRIPTION: Changes a vector of names to a vector of names (<= 10 characters).
  ##		Used before exporting shapefiles.
  ## ARGUMENTS:
  ## x	- Character vector
  ## VALUE:
  ## vector of names (<= 10 characters)


  ## Default title if title.estcv = NULL.

  xnms2 <- {}
  for (nm in xnms) {
    if (nchar(nm) > 10) {
      xnm <- substr(nm, 0, 10)
      i <- 2
      while (xnm %in% xnms2) {
        xnm <- paste0(substr(xnm, 0, 9), i)
        i <- i + 1
      }
    } else {
      xnm <- nm
    }
    xnms2 <- c(xnms2, xnm)
  }
  return(xnms2)
}


