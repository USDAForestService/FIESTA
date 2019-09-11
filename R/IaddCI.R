addCI <- function(x, estnm, senm=NULL, conf.level=c(99, 95, 68), gainloss=FALSE){
  ## DESCRIPTION: calculate left and right tails of confidence interval
  ## ARGUMENTS:
  ## x		data.frame of estimates
  ## estnm 	name of estimate variable
  ## senm		name of standard error variable
  ## conf.level	numeric vector of 1 or more numbers for conf.level

  ##################################################################
  ## Check parameters
  ##################################################################

  ## Get standard error (se) variable from dataset. If no se, calculate from variance (var)
  if (is.null(senm))
    senm <- paste(estnm, "se", sep=".")
  
  if (!senm %in% names(x)) {
    varnm <- paste(estnm, "var", sep=".")
    if (varnm %in% names(x)) {
      x[, senm] <- sqrt(x[, varnm])
    } else {
      warning(paste("no confidence intervals will be calculated", 
			"there is no se or var in dataset"))
    }
  }

  ## calculate left and right tail for the given confidence interval(s)
  ## Using a normal approximation

  for (level in conf.level) {
    level <- level / 100
    if (level <= 0 || level >= 1) 
      stop(paste("level", level, "must be between 0 and 1"))

    ## Define constant for given confidence level
    zval <- stats::qnorm((1 - level)/2, lower.tail = FALSE)
 
    leftvar <- paste0("CI", level * 100, "left")
    rightvar <- paste0("CI", level * 100, "right")

    x[[leftvar]] <- x[, estnm] - (zval * x[, senm])

    if (!gainloss)
      x[is.na(x[[leftvar]]) | x[[leftvar]] < 0, leftvar] <- 0
    
    x[, rightvar] <- x[, estnm] + (zval * x[, senm])
    
  #  }, x, estnm, senm)
  }

  return(x)
}
