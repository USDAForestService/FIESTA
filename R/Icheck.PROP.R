check.PROP <- function(treex, condx, cuniqueid="PLT_CN", checkNA=TRUE, 
	tpavar="TPA_UNADJ", SUBP_BREAKPOINT_DIA=5, MACRO_BREAKPOINT_DIA=NULL){

  ## Global variables
  TPROP_BASIS=SUBPPROP_UNADJ=MICRPROP_UNADJ=TPA_UNADJ=MACRPROP_UNADJ=DIA=PROP_BASIS <- NULL


  ###################################################################################
  ## DESCRIPTION: Check for necessary proportion variables.
  ## VALUE: Vector of PROPORTION variables in dataset.
  ###################################################################################
  if (!tpavar %in% names(treex)) stop("must include ", tpavar, " in tree table")
  condnmlst <- names(condx)
  PROPvars <- NULL

  ## Check if TPA values exist for SUBPLOT, MICROPLOT, AND MACROPLOT
#  TPAvals <- unique(treex$TPA_UNADJ)[!is.na(unique(treex$TPA_UNADJ))]
#  subadj <- ifelse(any(TPAvals > 5 & TPAvals < 10), TRUE, FALSE)
#  micadj <- ifelse(any(TPAvals > 50), TRUE, FALSE)
#  macadj <- ifelse(any(TPAvals > 0 & TPAvals < 5), TRUE, FALSE)


  ## Check SUBP_BREAKPOINT_DIA  and MACRO_BREAKPOINT_DIA    
  if (!is.null(SUBP_BREAKPOINT_DIA ) && !is.numeric(SUBP_BREAKPOINT_DIA ) &&
 		length(SUBP_BREAKPOINT_DIA ) != 1)
    stop("supb.mindia must be a numeric vector of length 1")
  if (!is.null(MACRO_BREAKPOINT_DIA) && !is.numeric(MACRO_BREAKPOINT_DIA) &&
 		length(MACRO_BREAKPOINT_DIA) != 1)
    stop("MACRO_BREAKPOINT_DIA must be a numeric vector of length 1")

  treex[, TPROP_BASIS := "SUBP"]
  if (!"SUBPPROP_UNADJ" %in% names(condx)) {
    if (nrow(condx) == length(unique(condx[[cuniqueid]]))) {
      ## If SUBPPROP_UNADJ not in cond table and only 1 condition per plot, 
      ## 	add SUBPPROP_UNADJ and set = 1 (100 percent)
      condx[, SUBPPROP_UNADJ := 1]
    } else {
      stop("SUBPPROP_UNADJ must be in cond table")
    }
  } 
 
  if (!"MICRPROP_UNADJ" %in% names(condx)) {
    if (nrow(condx) == length(unique(condx[[cuniqueid]]))) {
      ## If MICRPROP_UNADJ not in cond table and only 1 condition per plot, 
      ## 	add MICRPROP_UNADJ and set = 1 (100 percent)
      condx[, MICRPROP_UNADJ := 1]
    } else {
      stop("MICRPROP_UNADJ must be in cond table")
    }
  }
  treex[!is.na(DIA) & DIA < SUBP_BREAKPOINT_DIA , TPROP_BASIS := "MICR"]

  if (!"MACRPROP_UNADJ" %in% names(condx)) {
    if (nrow(condx) == length(unique(condx[[cuniqueid]]))) {
      ## If MACRPROP_UNADJ not in cond table and only 1 condition per plot, 
      ## 	add MACRPROP_UNADJ and set = 1 (100 percent)
      condx[, MACRPROP_UNADJ := 1]
    } else {
      stop("MACRPROP_UNADJ must be in cond table")
    }
  }
  
  if ("DIA" %in% names(treex)) {
    if (is.null(MACRO_BREAKPOINT_DIA) && "MACRO_BREAKPOINT_DIA" %in% names(condx)) {
      treex[condx, TPROP_BASIS := ifelse(PROP_BASIS == "MACR" & 
		!is.na(DIA) & DIA >= MACRO_BREAKPOINT_DIA, "MACR", TPROP_BASIS)] 
    } else if (!is.null(MACRO_BREAKPOINT_DIA)) {
      treex[!is.na(DIA) & DIA >= MACRO_BREAKPOINT_DIA, TPROP_BASIS := "MACR"]
    }
  } else {
    treex[, TPROP_BASIS := "MICR"]
  }  
  
  PROPvars <- c("SUBPPROP_UNADJ", "MICRPROP_UNADJ", "MACRPROP_UNADJ")
 
  if (checkNA) {
    ## Check for missing PROPvars and NA values in PROPvars
    cmissvars <- PROPvars[which(!PROPvars %in% condnmlst)]
    if (length(cmissvars) > 0)
      stop("missing necessary variables in cond: ", paste(cmissvars, collapse=", "))

    ## Check for NA values in necessary variables in cond table
    condx.na <- sapply(PROPvars, 
		function(x, condx){ sum(is.na(condx[,x, with=FALSE])) }, condx)
    if (any(condx.na) > 0) 
      warning(condx.na[condx.na > 0], " NA values in variable: ", 
		paste(names(condx.na[condx.na > 0]), collapse=", "))
  }

  ## check for numeric
  suppressWarnings(condx[, (PROPvars) := lapply(.SD, check.numeric), .SDcols=PROPvars])

  return(list(condx=condx, treex=treex, propvars=PROPvars))
}
