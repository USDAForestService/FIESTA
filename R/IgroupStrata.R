groupEstunit <- function(x, unitvar, unitvarnew, minplotnum, unitgrpnm=TRUE,
	n="n.strata") {
  ## DESCRIPTION: Groups estimation units with total plots <= minplotnum.
  ## Estimation units that have total plots <= minplotnum are combined with the 
  ## estimation unit next in order (numeric or alphabetical). If there are no
  ## estimation units next in order, it is combined with the estimation unit
  ## previous in order. 
 
  x[, unitvarnew] <- -1

  unitltmin <- unique(x[x$n.total <= minplotnum, unitvar])
  unitgtmin <- unique(x[x$n.total > minplotnum, unitvar])
  x[x[[unitvar]] %in% unitgtmin, unitvarnew] <- x[x[[unitvar]] %in% unitgtmin, 
		unitvar]

  agunits <- {}
  for (unit in unitltmin) {
    if (!unit %in% agunits) {
      agunits <- {unit}
      while (sum(x[x[[unitvar]] %in% agunits, n]) <= minplotnum) {
        if (any(x[[unitvar]] > max(agunits))) {
          unitag <- min(x[[unitvar]][x[[unitvar]] > max(agunits)])
          agunits <- c(agunits, unitag)
          agnm <- ifelse (unitgrpnm, paste(agunits, collapse="-"), unitag)
        } else {
          unitag <- max(x[[unitvar]][x[[unitvar]] < min(agunits)])
          unit2 <- unique(x[x[[unitvar]] == unitag, unitvarnew]) 
          if (!unit2 %in% c(-1, unitag)) {
            agunits <- c(agunits, strsplit(unit2, "-")[[1]])
          } else {
            agunits <- c(agunits, unitag)
          }
          agnm <- ifelse (unitgrpnm, paste(agunits, collapse="-"), unitag)
        } 
        x[x[[unitvar]] %in% agunits, unitvarnew] <- agnm 
      }
    }
  }
  return(x)
}      

groupStrata <- function(x, strvar, strvarnew, minplotnum) {
  ## DESCRIPTION: Groups strata with total plots <= minplotnum.
  ## Strata that have less than minplotnum are combined with the strata
  ## next in order (numeric or alphabetical). If there are no strata
  ## next in order, it is combined with the strata previous in order.
  ## NOTE: minplotnum must not be greater than the minimun number
  ##		or plots by estimation unit plus 1.
  x[, strvarnew] <- -1

  if (any(x[["n.strata"]] <= minplotnum)) {
    strats <- unique(x[[strvar]])

    agstrats <- {}
    for (strat in strats) {
      if (!strat %in% agstrats) {
        agstrats <- {strat}
        if (x[x[[strvar]] %in% strat, "n.strata"] > minplotnum) {
          x[x[[strvar]] %in% strat, strvarnew] <- strat
        } else {
          while (sum(x[x[[strvar]] %in% agstrats, "n.strata"]) <= minplotnum) { 
            if (any(x[[strvar]] > max(agstrats))) {
              stratag <- min(x[[strvar]][x[[strvar]] > max(agstrats)])
              agstrats <- c(agstrats, stratag)
            } else {
              stratag <- max(x[[strvar]][x[[strvar]] < min(agstrats)])
              #stratag <- x[x[[strvar]] == stratag, strvarnew]
              stratvarnewcd <- x[x[[strvar]] == stratag, strvarnew]
              stratag <- x[x[[strvarnew]] == stratvarnewcd, strvar]
              agstrats <- c(stratag, agstrats)
            }

            agstratsnm <- paste(agstrats, collapse="-")
            maxag <- max(x[x[[strvar]] %in% agstrats, "n.strata"])
            #maxstrat <- x[x[[strvar]] %in% agstrats & x[["n.strata"]] %in% maxag, strvar]
          
            x[x[[strvar]] %in% agstrats, strvarnew] <- agstratsnm 
          }
        }
      }
    }
  } else {
    x[[strvarnew]] <- x[[strvar]]
  }
  return(x)
}

