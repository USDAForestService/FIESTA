check.PROP <- function(treex, condx, cuniqueid="PLT_CN", checkNA=TRUE,
	areawt="CONDPROP_UNADJ", diavar="DIA",
	MICRO_BREAKPOINT_DIA=5, MACRO_BREAKPOINT_DIA=NULL,
	areawt_micr="MICRPROP_UNADJ", areawt_subp="SUBPPROP_UNADJ",
	areawt_macr="MACRPROP_UNADJ"){

  ## Global variables
  TPROP_BASIS=SUBPPROP_UNADJ=MICRPROP_UNADJ=MACRPROP_UNADJ=DIA=PROP_BASIS <- NULL


  ###################################################################################
  ## DESCRIPTION: Check for necessary proportion variables.
  ## VALUE: Vector of PROPORTION variables in dataset.
  ###################################################################################
  condnmlst <- names(condx)


  ## Check MICRO_BREAKPOINT_DIA  and MACRO_BREAKPOINT_DIA
  if (!is.null(MICRO_BREAKPOINT_DIA) && !is.numeric(MICRO_BREAKPOINT_DIA) &&
 		length(MICRO_BREAKPOINT_DIA) != 1) {
    stop("supb.mindia must be a numeric vector of length 1")
  }
  if (!is.null(MACRO_BREAKPOINT_DIA) && !is.numeric(MACRO_BREAKPOINT_DIA) &&
 		length(MACRO_BREAKPOINT_DIA) != 1) {
    stop("MACRO_BREAKPOINT_DIA must be a numeric vector of length 1")
  }

  treex[, TPROP_BASIS := "SUBP"]
  if (!areawt_subp %in% names(condx)) {
    if (nrow(condx) == length(unique(condx[[cuniqueid]]))) {
      ## If areawt_subp not in cond table and only 1 condition per plot,
      ## 	add areawt_subp and set = 1 (100 percent)
      condx[, (areawt_subp) := 1]
    } else {
      message(areawt_subp, " not in cond table... using ", areawt, " for subplots")
      areawt_subp <- areawt
    }
  }
  propvars <- areawt_subp
  tpropnames <- "SUBP"

  micro_breakpoint <- findnm("MICRO_BREAKPOINT_DIA", names(condx), returnNULL=TRUE)
  if ((!is.null(MICRO_BREAKPOINT_DIA) && any(treex[[diavar]] < MICRO_BREAKPOINT_DIA)) ||
	(is.null(MICRO_BREAKPOINT_DIA) && !is.null(micro_breakpoint) &&
	micro_breakpoint %in% names(condx) && sum(!is.na(condx[[micro_breakpoint]]) > 0))) {
    areawt_micr <- findnm(areawt_micr, names(condx), returnNULL=TRUE)
    if (is.null(areawt_micr)) {
      if (nrow(condx) == length(unique(condx[[cuniqueid]]))) {
        ## If areawt_micr not in cond table and only 1 condition per plot,
        ## 	add areawt_micr and set = 1 (100 percent)
        condx[, (areawt_micr) := 1]
      } else {
        message(areawt_micr, " not in cond table... using ", areawt, " for microplots")
        areawt_micr <- areawt
      }
    }
    if (is.null(MICRO_BREAKPOINT_DIA)) {
      if (!is.null(micro_breakpoint)) {
        if ("PROP_BASIS" %in% names(condx)) {
          treex[condx, TPROP_BASIS := ifelse(PROP_BASIS == "MICR" &
		!is.na(get(diavar)) & get(diavar) < get(micro_breakpoint), "MICR", TPROP_BASIS)]
        } else {
          treex[condx, TPROP_BASIS := ifelse(!is.na(get(diavar)) &
		get(diavar) >= get(micro_breakpoint), "MICR", TPROP_BASIS)]
        }
      }
    } else {
      treex[!is.na(get(diavar)) & get(diavar) < MICRO_BREAKPOINT_DIA, TPROP_BASIS := "MICR"]
    }
    propvars <- c(propvars, areawt_micr)
    tpropnames <- c(tpropnames, "MICR")
  } else {
    areawt_micr <- findnm(areawt_micr, names(condx), returnNULL=TRUE)
    if (!is.null(areawt_micr)) {
      if ((is.null(MICRO_BREAKPOINT_DIA) || is.na(MICRO_BREAKPOINT_DIA)) &&
		!all(is.na(condx[[areawt_micr]]))) {
        message(areawt_micr, " exists but no MICRO_BREAKPOINT_DIA provided")
      }
      propvars <- c(propvars, areawt_micr)
      tpropnames <- c(tpropnames, "MICR")
    }
  }

  macro_breakpoint <- findnm("MACRO_BREAKPOINT_DIA", names(condx), returnNULL=TRUE)
  if ((!is.null(MACRO_BREAKPOINT_DIA) && any(treex[[diavar]] >= MACRO_BREAKPOINT_DIA)) ||
	(is.null(MACRO_BREAKPOINT_DIA) && !is.null(macro_breakpoint) &&
	macro_breakpoint %in% names(condx) && sum(!is.na(condx[[macro_breakpoint]])) > 0)) {

    areawt_macr <- findnm(areawt_macr, names(condx), returnNULL=TRUE)
    if (is.null(areawt_macr)) {
      if (nrow(condx) == length(unique(condx[[cuniqueid]]))) {
        ## If areawt_macr not in cond table and only 1 condition per plot,
        ## 	add areawt_macr and set = 1 (100 percent)
        condx[, (areawt_macr) := 1]
      } else {
        message(areawt_macr, " not in cond table... using ", areawt, " for microplots")
        areawt_macr <- areawt
      }
    }

    if (is.null(MACRO_BREAKPOINT_DIA)) {
      if (!is.null(macro_breakpoint)) {
        if ("PROP_BASIS" %in% names(condx)) {
          treex[condx, TPROP_BASIS := ifelse(PROP_BASIS == "MACR" &
		!is.na(get(diavar)) & get(diavar) >= get(macro_breakpoint), "MACR", TPROP_BASIS)]
        } else {
          treex[condx, TPROP_BASIS := ifelse(!is.na(get(diavar)) &
		get(diavar) >= get(macro_breakpoint), "MACR", TPROP_BASIS)]
        }
      }

    } else {
      treex[!is.na(get(diavar)) & get(diavar) >= MACRO_BREAKPOINT_DIA, TPROP_BASIS := "MACR"]
    }
    propvars <- c(propvars, areawt_macr)
    tpropnames <- c(tpropnames, "MACR")
  } else {
    areawt_macr <- findnm(areawt_macr, names(condx), returnNULL=TRUE)
    if (!is.null(areawt_macr)) {
      if ((is.null(MACRO_BREAKPOINT_DIA) || is.na(MACRO_BREAKPOINT_DIA)) && !all(is.na(condx[[areawt_macr]]))) {
        if (any(!is.na(condx[[areawt_macr]])) || sum(condx[[areawt_macr]]) != 0) {
          message(areawt_macr, " exists but no MACRO_BREAKPOINT_DIA provided")
        }
      }
      propvars <- c(propvars, areawt_macr)
      tpropnames <- c(tpropnames, "MACR")
    }
  }

  tpropvars <- as.list(propvars)
  names(tpropvars) <- tpropnames

  if (checkNA) {
    ## Check for missing propvars and NA values in propvars
    cmissvars <- propvars[which(!propvars %in% condnmlst)]
    if (length(cmissvars) > 0)
      stop("missing necessary variables in cond: ", paste(cmissvars, collapse=", "))

    ## Check for NA values in necessary variables in cond table
    condx.na <- sapply(propvars,
		function(x, condx){ sum(is.na(condx[,x, with=FALSE])) }, condx)
    if (any(condx.na) > 0)
      warning(condx.na[condx.na > 0], " NA values in variable: ",
		paste(names(condx.na[condx.na > 0]), collapse=", "))
  }

  ## check for numeric
  suppressWarnings(condx[, (propvars) := lapply(.SD, check.numeric), .SDcols=propvars])

  return(list(condx=condx, treex=treex, tpropvars=tpropvars))
}
