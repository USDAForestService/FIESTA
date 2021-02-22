datLUTclass <- function(x, xvar=NULL, LUT=NULL, minvar=NULL, maxvar=NULL, 
	cutbreaks=NULL, cutlabels=NULL, LUTclassnm=NULL, label.dec=1, 
	NAto0=FALSE, vars2keep=NULL, keepcutbreaks=FALSE, savedata=FALSE, 
	outfolder=NULL, outfn="datlut"){
  #################################################################################
  ## DESCRIPTION: Function to get variable name from a table stored within FIESTA 
  ##      or a look-up table (*.csv FILE).
  #################################################################################

  ## CHECK GUI - IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- ifelse(nargs() == 0, TRUE, FALSE)

  if (gui) x=xvar=LUT=xLUT=LUTvar=LUTnewvar=varclass=minvar=maxvar=VALUE <- NULL 

  ## Adds to file filters to Cran R Filters table.
  if (.Platform$OS.type=="windows") 
    Filters <- rbind(Filters, csv=c("Comma-delimited files (*.csv)", "*.csv"))

  ##################################################################
  ## CHECK INPUT PARAMETERS
  ##################################################################

  ## Check datx
  ########################################################
  datx <- FIESTA::pcheck.table(x, gui=gui, caption="Data table?", returnDT=TRUE)
  issf <- ifelse ("sf" %in% class(datx), TRUE, FALSE)
  if (issf) datx <- setDT(datx)    

  ## Check xvar
  ##########################################
  datnmlst <- names(datx)
  xvar <- FIESTA::pcheck.varchar(xvar, "xvar", datnmlst, gui=gui,
		caption="Join variable in dat", stopifnull=TRUE)
  if (!is.numeric(datx[[xvar]])) stop("xvar must be a numeric vector in x")

  ## Get min and max values for xvar in dat
  xvar.min <- min(datx[[xvar]], na.rm=TRUE)
  xvar.max <- max(datx[[xvar]], na.rm=TRUE)


  ## Check LUT
  ########################################################
  if (is.vector(LUT) && length(LUT) > 1) {
    LUTx <- data.table(LUT)
    setnames(LUTx, xvar)
  } else {
    LUTx <- pcheck.table(LUT, gui=gui, tabnm="LUT", caption="Look up table?")
  }

  if (is.null(LUTx)) {
    if (is.null(cutbreaks)) {
      stop("must include LUTx or cutbreaks")
    } else if (!is.vector(cutbreaks) || !is.numeric(cutbreaks)) {
      stop("cutbreaks must be a numeric vector")
    } else if (all(cutbreaks < xvar.min) || all(cutbreaks > xvar.max)) {
      stop("all cutbreaks values are outside range of xvar values")
    } else {
      if (is.null(cutlabels)) {
        val <- as.numeric(ifelse(label.dec == 0, 0, 
		ifelse(label.dec == 1, 0.1, paste0(".", rep(0, label.dec-1), 1))))
        maxbreaks <- c(cutbreaks[-1] - val) 
        cutlabels <- paste(cutbreaks[-length(cutbreaks)], maxbreaks, sep="-")
        cutlabels[length(cutlabels)] <- paste0(cutbreaks[length(cutbreaks)-1],"+")
      } else {
        if (length(cutlabels) != length(cutbreaks)-1)
          stop("cutlabels must be length ", length(cutbreaks)-1)
      }
    }
    if (!is.null(LUTclassnm))
      if (!is.character(LUTclassnm)) stop("LUTclassnm must be character")

  } else {

    ## Check minvar and maxvar
    ########################################################
    LUTnmlst <- names(LUTx)
    if (is.null(minvar) && grepl("MIN", LUTnmlst, ignore.case=TRUE)) {
      minvar <- "MIN"
    }
    minvar <- FIESTA::pcheck.varchar(minvar, "minvar", LUTnmlst, gui=gui,
		caption="LUT min variable", stopifnull=TRUE)
    if (all(LUTx[[minvar]] > xvar.max)) {
      stop("all minvar values are greter than max xvar value")
    }
    LUTnmlst <- LUTnmlst[LUTnmlst != minvar]
    if (is.null(maxvar) && grepl("MAX", LUTnmlst, ignore.case=TRUE)) {
      maxvar <- "MAX"
    }
    maxvar <- FIESTA::pcheck.varchar(maxvar, "maxvar", LUTnmlst, gui=gui,
		caption="LUT max variable", stopifnull=FALSE)
    if (!is.null(maxvar)) {
      if (all(LUTx[[maxvar]] < xvar.min)) 
        stop("all maxvar values are greater than min xvar value")
    } else {
      val <- as.numeric(ifelse(label.dec == 0, 0, 
		ifelse(label.dec == 1, 0.1, paste0(".", rep(0, label.dec-1), 1))))
      LUTx$MAX <- c(LUTx[[minvar]][-1] - val, round(xvar.max + 1, label.dec)) 
      maxvar <- "MAX"
    }  
    cutbreaks <- c(LUTx[[minvar]], LUTx[[maxvar]][nrow(LUTx)])

    ## Check LUTclassnm - get cutlabels
    ########################################################
    LUTnmlst <- LUTnmlst[LUTnmlst != maxvar]
    LUTclassnm <- FIESTA::pcheck.varchar(LUTclassnm, "LUTclassnm", LUTnmlst, gui=gui,
		caption="LUT class name")

    ## If LUTclassnm=NULL, create a class
    if (is.null(LUTclassnm)) {
      cutlabels <- paste(LUTx[[minvar]], LUTx[[maxvar]], sep="-")
    } else {
      cutlabels <- LUTx[[LUTclassnm]]
    }
  }

  ### Check NAto0
  NAto0 <- FIESTA::pcheck.logical(NAto0, varnm="NAequal10", 
		title="Change NA values to 0?", first="YES", gui=gui)

  ## Check vars2keep
  if (!is.null(vars2keep)) {
    if (!is.character(vars2keep)) stop("vars2keep must be character")
    missvar <- vars2keep[!vars2keep %in% names(LUTx)]
    if (length(missvar) > 0) 
      stop("vars2keep variables invalid: ", toString(vars2keep))
  }

  ### Check savedata 
  savedata <- FIESTA::pcheck.logical(savedata, varnm="savedata", title="Save data tables?", 
		first="NO", gui=gui)

  ## GET OUTFOLDER IF NULL
  if (savedata) 
    outfolder <- FIESTA::pcheck.outfolder(outfolder, gui)


  ############################################################################
  ## DO THE WORK 
  ############################################################################
  if (NAto0) {
    datx[is.na(datx[[xvar]]), (xvar) := 0] 
  }

  ## Test values
  vals <- unique(na.omit(datx[[xvar]]))
  if (any(vals < min(cutbreaks)) || any(vals >= max(cutbreaks))) {
      gtvals <- sort(unique(vals[which(vals < min(cutbreaks) | vals >= max(cutbreaks))]))
      message(paste("values are outside of cutbreaks range:", paste(gtvals, collapse=", ")))
  }

  if (is.null(LUTclassnm)) {
    LUTclassnm <- paste0(xvar, "CL")
  }
  datxnames <- names(datx) 
  datx[, (LUTclassnm) := cut(datx[[xvar]], breaks=cutbreaks, labels=cutlabels, 
		right=FALSE)]  
  LUTx2 <- data.table(cutbreaks[-length(cutbreaks)], cutlabels)
  names(LUTx2) <- c(paste0(xvar, "_cutbreaks"), LUTclassnm)  

  if (!is.null(vars2keep)) {
    datx <- merge(datx, LUTx[, c(LUTclassnm, vars2keep), with=FALSE], by=LUTclassnm,
			all.x=TRUE)
    setcolorder(datx, c(datxnames, vars2keep))

    navals <- sum(is.na(datx[[LUTclassnm]]))
    if (length(navals) > 0)
      message("there are ", navals, " NA values in ", xvar, " that did not get classified")
  
    LUTx2 <- merge(LUTx2, LUTx[, c(LUTclassnm, vars2keep), with=FALSE], by=LUTclassnm)
  } 
  if (keepcutbreaks) {
    datx <- merge(datx, LUTx2, by=LUTclassnm, all.x=TRUE)
    setcolorder(datx, c(datxnames, paste0(xvar, "_cutbreaks")))
    setnames(datx, paste0(xvar, "_cutbreaks"), paste0(LUTclassnm, "_brks"))
  }    
       
  ## Output list
  ########################################################
  if (issf) {
    datx <- sf::st_as_sf(datx, stringsAsFactors=FALSE)
  } else {
    datx <- setDF(datx)
  }

  xLUTlst <- list(xLUT=datx)
  xLUTlst$LUTclassnm <- LUTclassnm
  xLUTlst$LUT <- LUTx2

  
  if (savedata) {
    if ("sf" %in% class(datx)) {
      spExportSpatial(datx, outfolder=outfolder, out_layer=outfn)
    } else {
      ## WRITE DATA TO OUTFOLDER
      write2csv(datx, outfilenm=outfn, outfolder=outfolder)
    }
  }

  return(xLUTlst)
}
