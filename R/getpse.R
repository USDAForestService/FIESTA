getpse <- function(xdat, esttype="AREA", areavar=NULL,
	nhatcol="nhat", nhatcol.var="nhat.var", 
	dhatcol="dhat", dhatcol.var="dhat.var"){

  ########################################################################################
  ## DESCRIPTION: Calculates the following variables
  ## estn		- estimated acres of land covered by condition, for numerator { nhatcol*ACRES }
  ## estd		- estimated acres of land covered by condition, for denominator { dhat*ACRES }
  ## estn.var	- variance of estimated acres, for numerator { nhatcol.var*ACRES }
  ## estn.se	- standard error of estimated acres, for numerator { sqrt(estn.var) }
  ## estn.cv	- coefficient of variance of estimated acres, for numerator { estn.se/estn }
  ## estn.pse	- percent sampling error, for numerator { estn.cv*100 }
  ## estd.var	- variance of estimated acres, for denominator { dhat.var*ACRES }
  ## estd.se	- standard error of estimated acres, for denominator { sqrt(estd.var) }
  ## estd.cv	- coefficient of variance of estimated acres, for denominator { estd.se/estd }
  ## estd.pse	- percent sampling error, for denominator { estd.cv*100 }
  ## est.cover	- covariance of estimated acres for numerator/denominator
  ########################################################################################

  ## Set global variables
  estn=estd=estn.var=estn.se=estn.cv=estn.pse=estd.var=estd.se=estd.cv=estd.pse=est.covar=
	covar <- NULL


  ## ESTIMATED ACRES OR PER ACRE (for ratio)
  if (!is.null(nhatcol)) {
    if (!is.null(areavar)) {
      xdat[, estn := get(nhatcol) * get(areavar)]
    } else {
      xdat[, estn := get(nhatcol)]
    }    
  }

  if (esttype == "RATIO") {
    if (!is.null(dhatcol)) {
      if (!is.null(areavar)) {
        xdat[, estd := get(dhatcol) * get(areavar)]
      } else {
        xdat[, estd := get(dhatcol)]
      }
    }
  }

  if (!is.null(nhatcol.var)) {
    ## Calculate variance of estimated acres, for numerator
    if (!is.null(areavar)) {
      xdat[, estn.var := get(nhatcol.var) * get(areavar)^2]
    } else {
      xdat[, estn.var := get(nhatcol.var)]
    }

    ## Calculate standard error (se), coefficient of variation (cv), and
    ##	percent sampling error (pse). for numerator
    suppressWarnings(
    xdat[,	estn.se := sqrt(estn.var)][,
		estn.cv := estn.se/estn][,
		estn.pse := estn.cv*100] )

    if (esttype == "RATIO" && !is.null(dhatcol.var)) {
      ## Calculate variance of estimated acres, for denominator
      if (!is.null(areavar)) {
        xdat[, estd.var := get(dhatcol.var) * get(areavar)^2]
      } else {
        xdat[, estd.var := get(dhatcol.var)]
      }

      ## Calculate standard error (se), coefficient of variation (cv), and
      ##	percent sampling error (pse). for denominator
      suppressWarnings(
      xdat[, 	estd.se := sqrt(estd.var)][,
			estd.cv := estd.se/estd][,
			estd.pse := estd.cv*100] )

      if ("covar" %in% names(xdat)) {
        ## Calculate covariance of estimated acres for numerator/denominator
        if (!is.null(areavar)) {
          xdat[, est.covar := covar * get(areavar)^2]
        }
      }
    }
  }
  if (esttype != "RATIO") {
    rawnames <- c("estn", "estn.var", "estn.se", "estn.cv")
    rawnames.new <- sub("n", "", rawnames)
    setnames(xdat, rawnames, rawnames.new)
    setnames(xdat, "estn.pse", "pse")
  }

  ## Change all na values to 0 values
  #xdat[is.na(xdat)] <- 0

  return(data.table(xdat))
}
