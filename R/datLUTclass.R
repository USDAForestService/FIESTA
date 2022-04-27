#' Data - Create a variable with classified values.
#' 
#' Merge a look-up table to define categories of continuous data in x (e.g.,
#' DIA).  Adds a variable (LUTclassnm) to x, defining as: xvar >= MIN (and xvar
#' <= MAX).
#' 
#' Use datLUTclass() to prompt for input.
#' 
#' @param x Data frame or comma-delimited file (*.csv). The data table with
#' variable to classify.
#' @param xvar String. Name of variable in the data table to join to.
#' @param LUT Data frame or comma-delimited file (*.csv). Name of the look-up
#' table with collapsed classes. Lookup table should include minimum values for
#' classes, maximum values for classes, and a name of class (i.e., LUTclassnm).
#' Maximum values and names are optional.
#' @param minvar String. If LUT is not null, name of variable with minimimum
#' class value (>= minvar).
#' @param maxvar String. Optional. If LUT is not null, name of variable with
#' maximum class value (<= maxvar).
#' @param cutbreaks Numeric vector. Vector of numbers for minimum class values.
#' @param cutlabels String vector. Optional. Vector of names for classes. If
#' NULL, class names are generated from cutbreaks.
#' @param LUTclassnm String. Optional. Name of classified variable in x. If LUT
#' is not null and class names are included, this is the name of variable with
#' class names. If NULL, a class names are generated from minvar or minvar and
#' maxvar with default name equal to 'xvar'CL.
#' @param label.dec Integer. Number of decimals to include in label.
#' @param NAto0 Logical. If TRUE, converts NA values to 0 before
#' classification.
#' @param vars2keep String vector. Variable names from LUT to keep (append) to
#' x.
#' @param keepcutbreaks Logical. If TRUE, the cutbreaks used for creating
#' classes are appended to dataset.
#' @param savedata Logical. If TRUE, saves data to outfolder.
#' @param savedata_opts List. See help(savedata_options()) for a list
#' of options. Only used when savedata = TRUE. If out_layer = NULL,
#' default = 'datlutcl'. 
#' @param gui Logical. If gui, user is prompted for parameters.
#'
#' @return \item{xLUT}{ Input data table with look-up table variable(s). }
#' \item{LUTclassnm}{ Name of the classified variable. } \item{LUT}{ Look-up
#' table with categories. }
#' 
#' If savedata = TRUE, a comma-delimited file is output to the outfolder as
#' outfn.  If outfn = NULL, the name of the file will be datlut_'date'.csv.
#' @note The look-up table format must be similar to the following table Set
#' LUTclassnm = VARCLNM. MAX and VALCLNM columns are optional.
#' 
#' \tabular{llll}{ \tab \bold{MIN} \tab \bold{MAX} \tab \bold{VARCLNM} \cr \tab
#' 1.0 \tab 4.9 \tab 1 \cr \tab 5.0 \tab 9.9 \tab 2 \cr \tab 10.0 \tab 15.0
#' \tab 3 \cr \tab 15.0 \tab 19.9 \tab 4 \cr \tab 20.0 \tab 24.9 \tab 5 \cr
#' \tab 25.0 \tab 49.9 \tab 6 \cr }
#' @author Tracey S. Frescino
#' @keywords data
#' @examples
#' head(FIESTA::ref_diacl2in)
#' WYtreelut <- datLUTclass(FIESTA::WYtree,
#'                          xvar = "DIA",
#'                          LUT = FIESTA::ref_diacl2in,
#'                          LUTclassnm = "DIACL2IN")
#' names(WYtreelut)
#' head(WYtreelut$xLUT)
#' table(WYtreelut$xLUT$DIACL2IN)
#' 
#' WYtreelut2 <- datLUTclass(FIESTA::WYtree,
#'                           xvar = "DIA",
#'                           cutbreaks = c(1, 5, 25, 50, 100),
#'                           LUTclassnm = "DIACL2IN")
#' names(WYtreelut2)
#' head(WYtreelut2$xLUT)
#' table(WYtreelut2$xLUT$DIACL2IN)
#' 
#' #' Create look-up table of stand age classes
#' MIN <- c(0, 20, 40, 60, 80, 101)
#' STDAGENM <- c("0-20", "21-40", "41-60", "61-80", "81-100", "101+")
#' stdagelut <- data.frame(MIN = MIN, STDAGENM = STDAGENM)
#' stdagelut
#' 
#' WYcondlut <- datLUTclass(FIESTA::WYcond,
#'                          xvar = "STDAGE",
#'                          LUT = stdagelut,
#'                          LUTclassnm = "STDAGENM")
#' names(WYcondlut)
#' head(WYcondlut$xLUT)
#' table(WYcondlut$xLUT$STDAGENM)
#' @export datLUTclass
datLUTclass <- function(x, 
                        xvar = NULL, 
                        LUT = NULL, 
                        minvar = NULL, 
                        maxvar = NULL, 
	                      cutbreaks = NULL, 
                        cutlabels = NULL, 
                        LUTclassnm = NULL, 
                        label.dec = 1, 
	                      NAto0 = FALSE, 
                        vars2keep = NULL, 
                        keepcutbreaks = FALSE, 
                        savedata = FALSE,
	                      savedata_opts = NULL, 
                        gui = FALSE){
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
  ## CHECK PARAMETER NAMES
  ##################################################################
  
  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- names(formals(datLUTclass)) 
  if (!all(input.params %in% formallst)) {
    miss <- input.params[!input.params %in% formallst]
    stop("invalid parameter: ", toString(miss))
  }
  
  ## Check parameter lists
  pcheck.params(input.params, savedata_opts=savedata_opts)
  
  ## Set savedata defaults
  savedata_defaults_list <- formals(FIESTA::savedata_options)[-length(formals(FIESTA::savedata_options))]
  
  for (i in 1:length(savedata_defaults_list)) {
    assign(names(savedata_defaults_list)[[i]], savedata_defaults_list[[i]])
  }
  
  ## Set user-supplied savedata values
  if (length(savedata_opts) > 0) {
    if (!savedata) {
      message("savedata=FALSE with savedata parameters... no data are saved")
    }
    for (i in 1:length(savedata_opts)) {
      if (names(savedata_opts)[[i]] %in% names(savedata_defaults_list)) {
        assign(names(savedata_opts)[[i]], savedata_opts[[i]])
      } else {
        stop(paste("Invalid parameter: ", names(savedata_opts)[[i]]))
      }
    }
  }
  
  ##################################################################
  ## CHECK PARAMETER INPUTS
  ##################################################################

  ## Check datx
  ########################################################
  datx <- pcheck.table(x, gui=gui, caption="Data table?", returnDT=TRUE)
  issf <- ifelse ("sf" %in% class(datx), TRUE, FALSE)
  if (issf) datx <- data.table(datx)    

  ## Check xvar
  ##########################################
  datnmlst <- names(datx)
  xvar <- pcheck.varchar(xvar, "xvar", datnmlst, gui=gui,
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
    if (is.null(minvar) && any(grepl("MIN", LUTnmlst, ignore.case=TRUE))) {
      minvar <- "MIN"
    }
    minvar <- pcheck.varchar(minvar, "minvar", LUTnmlst, gui=gui,
		caption="LUT min variable", stopifnull=TRUE)
    if (all(LUTx[[minvar]] > xvar.max)) {
      stop("all minvar values are greter than max xvar value")
    }
    LUTnmlst <- LUTnmlst[LUTnmlst != minvar]
    if (is.null(maxvar) && any(grepl("MAX", LUTnmlst, ignore.case=TRUE))) {
      maxvar <- "MAX"
    }
    maxvar <- pcheck.varchar(maxvar, "maxvar", LUTnmlst, gui=gui,
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
    LUTclassnm <- pcheck.varchar(LUTclassnm, "LUTclassnm", LUTnmlst, gui=gui,
		caption="LUT class name")

    ## If LUTclassnm=NULL, create a class
    if (is.null(LUTclassnm)) {
      cutlabels <- paste(LUTx[[minvar]], LUTx[[maxvar]], sep="-")
    } else {
      cutlabels <- LUTx[[LUTclassnm]]
    }
  }

  ### Check NAto0
  NAto0 <- pcheck.logical(NAto0, varnm="NAequal10", 
		title="Change NA values to 0?", first="YES", gui=gui)

  ## Check vars2keep
  if (!is.null(vars2keep)) {
    if (!is.character(vars2keep)) stop("vars2keep must be character")
    missvar <- vars2keep[!vars2keep %in% names(LUTx)]
    if (length(missvar) > 0) 
      stop("vars2keep variables invalid: ", toString(vars2keep))
  }

  ## Check savedata 
  savedata <- pcheck.logical(savedata, varnm="savedata", title="Save data table?", 
                             first="NO", gui=gui)
  
  ## Check output parameters
  if (savedata) {
    outlst <- pcheck.output(outfolder=outfolder, out_dsn=out_dsn, 
        out_fmt=out_fmt, outfn.pre=outfn.pre, outfn.date=outfn.date, 
        overwrite_dsn=overwrite_dsn, overwrite_layer=overwrite_layer,
        add_layer=add_layer, append_layer=append_layer, gui=gui)
    outfolder <- outlst$outfolder
    out_dsn <- outlst$out_dsn
    out_fmt <- outlst$out_fmt
    overwrite_layer <- outlst$overwrite_layer
    append_layer <- outlst$append_layer
    outfn.date <- outlst$outfn.date
    outfn.pre <- outlst$outfn.pre
    if (is.null(out_layer)) {
      out_layer <- "datlutcl"
    }
  }

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

  
  #### WRITE TO FILE 
  #############################################################
  if (savedata) {
    if ("sf" %in% class(datx)) {
      spExportSpatial(datx, 
            savedata_opts=list(outfolder=outfolder, 
                                  out_fmt=outsp_fmt, 
                                  out_dsn=out_dsn, 
                                  out_layer=out_layer,
                                  outfn.pre=outfn.pre, 
                                  outfn.date=outfn.date, 
                                  overwrite_layer=overwrite_layer,
                                  append_layer=append_layer, 
                                  add_layer=TRUE))
    } else {
      datExportData(datx, 
            savedata_opts=list(outfolder=outfolder, 
                                  out_fmt=out_fmt, 
                                  out_dsn=out_dsn, 
                                  out_layer=out_layer,
                                  outfn.pre=outfn.pre, 
                                  outfn.date=outfn.date, 
                                  overwrite_layer=overwrite_layer,
                                  append_layer=append_layer,
                                  add_layer=TRUE))
      
    }
  }

  return(xLUTlst)
}
