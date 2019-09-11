check.PROP <- function(treex, condx, cuniqueid="PLT_CN", checkNA=TRUE){

  ## Global variables
  PROP_BASIS=SUBPPROP_UNADJ=MICRPROP_UNADJ=TPA_UNADJ=MACRPROP_UNADJ <- NULL


  ###################################################################################
  ## DESCRIPTION: Check for necessary proportion variables.
  ## VALUE: Vector of PROPORTION variables in dataset.
  ###################################################################################
  if (!"TPA_UNADJ" %in% names(treex)) stop("must include TPA_UNADJ in tree table")
  condnmlst <- names(condx)
  PROPvars <- NULL

  ## Check if TPA values exist for SUBPLOT, MICROPLOT, AND MACROPLOT
  TPAvals <- unique(treex$TPA_UNADJ)[!is.na(unique(treex$TPA_UNADJ))]
  subadj <- ifelse(any(TPAvals > 5 & TPAvals < 10), TRUE, FALSE)
  micadj <- ifelse(any(TPAvals > 50), TRUE, FALSE)
  macadj <- ifelse(any(TPAvals > 0 & TPAvals < 5), TRUE, FALSE)

  treex[, PROP_BASIS := "SUBP"]
  if (subadj) {
    if (!"SUBPPROP_UNADJ" %in% condnmlst) {
      if (nrow(condx) == length(unique(condx[[cuniqueid]]))) {
        ## If SUBPPROP_UNADJ not in cond table and only 1 condition per plot, 
        ## 	add SUBPPROP_UNADJ and set = 1 (100 percent)
        condx[, SUBPPROP_UNADJ := 1]
      } else {
        stop("SUBPPROP_UNADJ must be in cond table")
      }
    } 
    PROPvars <- c(PROPvars, "SUBPPROP_UNADJ")
  }    
  if (micadj) {
    if (!"MICRPROP_UNADJ" %in% condnmlst) {
      if (nrow(condx) == length(unique(condx[[cuniqueid]]))) {
        ## If MICRPROP_UNADJ not in cond table and only 1 condition per plot, 
        ## 	add MICRPROP_UNADJ and set = 1 (100 percent)
        condx[, MICRPROP_UNADJ := 1]
      } else {
        stop("MICRPROP_UNADJ must be in cond table")
      }
    }
    treex[TPA_UNADJ > 50, PROP_BASIS := "MICR"]
    PROPvars <- c(PROPvars, "MICRPROP_UNADJ")
  }
  if (macadj) {
    if (!"MACRPROP_UNADJ" %in% condnmlst) {
      if (nrow(condx) == length(unique(condx[[cuniqueid]]))) {
        ## If MACRPROP_UNADJ not in cond table and only 1 condition per plot, 
        ## 	add MACRPROP_UNADJ and set = 1 (100 percent)
        condx[, MACRPROP_UNADJ := 1]
      } else {
        stop("MACRPROP_UNADJ must be in cond table")
      }
    }
    treex[TPA_UNADJ > 0 & TPA_UNADJ < 5, PROP_BASIS := "MACR"]
    PROPvars <- c(PROPvars, "MACRPROP_UNADJ")
  }

  if (checkNA) {
    ## Check for missing PROPvars and NA values in PROPvars
    cmissvars <- PROPvars[which(!PROPvars %in% condnmlst)]
    if (length(cmissvars) > 0)
      stop("missing necessary variables in cond: ", paste(cmissvars, collapse=", "))

    ## Check for NA values in necessary variables in cond table
    condx.na <- sapply(PROPvars, function(x, condx){ sum(is.na(condx[,x, with=FALSE])) }, condx)
    if (any(condx.na) > 0) 
      warning(condx.na[condx.na > 0], " NA values in variable: ", 
		paste(names(condx.na[condx.na > 0]), collapse=", "))
  }

  return(PROPvars)
}
