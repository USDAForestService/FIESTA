groupEstunit <- function(x, minplotnum) {
  ## DESCRIPTION: Groups estimation units with total plots <= minplotnum.
  ## Estimation units that have total plots <= minplotnum are combined with the 
  ## estimation unit next in order (numeric or alphabetical). If there are no
  ## estimation units next in order, it is combined with the estimation unit
  ## previous in order. 
 
  ## set global variables
  unitvar <- NULL

  unitltmin <- unique(x[x$n.total <= minplotnum, unitvar])
  unitgtmin <- unique(x[x$n.total > minplotnum, unitvar])
  x[unitvar %in% unitgtmin][["unitnew"]] <- x[unitvar %in% unitgtmin][["unitvar"]] 

  agunits <- {}
  for (unit in unitltmin) {
    if (!unit %in% agunits) {
      agunits <- {unit}
      maxag <- sum(x[unitvar %in% agunits][["n.strata"]])
      while (maxag <= minplotnum) {
        if (any(x$unitvar > max(agunits))) {
          unitag <- min(x$unitvar[x$unitvar > max(agunits)])
          agunits <- c(agunits, unitag)
          agnm <- paste(agunits, collapse="-")
        } else {
          unitag <- max(x$unitvar[x$unitvar < min(agunits)])
          unit2 <- unique(x[unitvar == unitag][["unitnew"]]) 
          if (!unit2 %in% c(-1, unitag)) {
            agunits <- c(agunits, strsplit(unit2, "-")[[1]])
          } else {
            agunits <- c(agunits, unitag)
          }
          agnm <- paste(agunits, collapse="-")
        } 
        maxag <- sum(x[unitvar %in% agunits][["n.strata"]])          
        x[unitvar %in% agunits][["unitnew"]] <- agnm 
      }
    }
  }
  return(x)
}      


groupStrata <- function(x, minplotnum) {
  ## DESCRIPTION: Groups strata with total plots <= minplotnum.
  ## Strata that have less than minplotnum are combined with the strata
  ## next in order (numeric or alphabetical). If there are no strata
  ## next in order, it is combined with the strata previous in order.
  ## NOTE: minplotnum must not be greater than the minimun number
  ##		or plots by estimation unit plus 1.

  ## set global variables
  strat=stratnew <- NULL
print(x)

  if (any(x$n.strata < minplotnum)) {
    strats <- x$strat
    agstrats <- {}
    for (stratum in strats) {
      if (!stratum %in% agstrats) {
        agstrats <- c(stratum)
        if (x[strat %in% stratum][["n.strata"]] >= minplotnum) {
          x[strat %in% stratum][["stratnew"]] <- stratum
        } else {
          maxag <- sum(x[strat %in% stratum][["n.strata"]])
          while (maxag < minplotnum) { 
            if (any(x$strat > max(agstrats))) {
              stratag <- min(x$strat[x$strat > max(agstrats)])
              agstrats <- c(agstrats, stratag)
            } else {
              stratag <- max(x$strat[x$strat < min(agstrats)])
              stratnewcd <- x[strat == stratag][["stratnew"]]
              stratag <- x[stratnew == as.character(stratnewcd)][["strat"]]     
              agstrats <- c(stratag, agstrats)
            }
            agstratsnm <- paste(agstrats, collapse="-")
            maxag <- sum(x[strat %in% agstrats][["n.strata"]])          
            x[strat %in% agstrats][["stratnew"]] <- agstratsnm
          }
        }
      }
    }

  } else {
    x$stratnew <- as.character(x$strat)
  }
  return(x)
}

