check.auxiliary <- function(pltmodelx, puniqueid, auxlut, prednames=NULL,
	PSstrvar=NULL, predfac=NULL, unitvars=NULL, minplotnum=10, checkcnt=TRUE, 
	na.rm=TRUE){

  ##################################################################################
  ## DESCRIPTION: Check auxiliary information. 
  ##################################################################################

  ## Set global variables
  ONEUNIT=npixels <- NULL
  gui <- FALSE

  ## Check auxlut
  ############################################################################
  auxlut <- FIESTA::pcheck.table(auxlut, gui=gui, tabnm="auxlut",
 	caption="Strata table?", nullcheck=TRUE, stopifnull=TRUE)
  auxnmlst <- names(auxlut)

  ## Check for a total value in the last row of table..  If exists, exclude.
  lastrow <- auxlut[nrow(auxlut),]
  if (length(grep("Total", lastrow, ignore.case=TRUE)) > 0)
    auxlut <- auxlut[-nrow(auxlut)]

  ## Check unitvars in auxlut
  ############################
  if (any(unitvars == "ONEUNIT") && !"ONEUNIT" %in% names(auxlut)) 
    auxlut[, ONEUNIT := 1]
  setkeyv(auxlut, unitvars)

  ## Check if class of unitvars in auxlut matches class of unitvars in pltmodelx
  tabs <- FIESTA::check.matchclass(pltmodelx, auxlut, unitvars,
		tab1txt="pltmodel", tab2txt="auxlut")
  pltmodelx <- tabs$tab1
  auxlut <- tabs$tab2


  ## Check npixels
  if ("npixels" %in% auxnmlst) 
    npixels <- unique(auxlut[, c(unitvars, "npixels"), with=FALSE])


  ## Check continuous variables
  ############################################################################
  predcon <- prednames[!prednames %in% predfac]

  ## Check for missing variables in auxlut
  missvars <- predcon[which(!predcon %in% auxnmlst)]
  if (length(missvars) > 0) 
    stop("auxvar not in tables: ", paste(missvars, collapse=", "))
  
  ## Check for NA values in continuous variables in auxlut
  aux.na <- sapply(predcon, function(x, auxlut){ sum(is.na(auxlut[,x, with=FALSE])) }, auxlut)
  if (any(aux.na) > 0) {
    message(aux.na[aux.na > 0], " NA values in variable: ", 
		paste(names(aux.na[aux.na > 0]), collapse=", "))
    if (na.rm) 
      auxlut <- na.omit(auxlut, cols=predcon)
  }

  ## Check values of continuous variable in prednames...  make sure in range of pltmodelx
  ## Still workin on
  #pmin <- pltmodelx[, lapply(.SD, min), by=unitvars, .SDcols=predcon]
  #pmax <- pltmodelx[, lapply(.SD, max), by=unitvars, .SDcols=predcon]

  
  ## Check categorical variables
  ############################################################################
  if (!is.null(PSstrvar)) {
    if (!PSstrvar %in% predfac) predfac <- c(PSstrvar, predfac) 
    PScols <- lapply(predfac, function(x, auxnmlst) auxnmlst[grep(x, auxnmlst)], auxnmlst)[[1]]
    PSvalslst <- sapply(strsplit(PScols, paste0(PSstrvar, ".")), "[[", 2)

    auxlut <- data.table(PSvalslst, auxlut[, t(.SD), by=unitvars, .SDcols=PScols])
    setnames(auxlut, c(PSstrvar, unitvars, "Prop"))
    setcolorder(auxlut, c(unitvars, PSstrvar, "Prop"))
  } else {
    ## Check for missing variables in auxlut
    for (fac in predfac) {
      pltvals <- sort(unique(pltmodelx[[fac]]))
      facnmlst <- auxnmlst[grep(fac, auxnmlst)]
      missvars <- facnmlst[!facnmlst %in% paste0(fac, ".", pltvals)]
      if (length(missvars) > 0) 
        stop("auxvar not in tables: ", paste(missvars, collapse=", "))
    }
  }


  ###################################################################################
  ## Check number of plots by unitvar(s)
  ##	 (including partially sampled plots - COND_STATUS_CD=5) 
  ##	 (excluding totally nonsampled plots - PLOT_STATUS_CD=1) 
  ###################################################################################
  ## If < 2 plots, an error occurs, must collapse plots.
  ## If 2-10 plots, a warning is displayed, suggesting to collapse plots. 
  ## Returns:
  ## - auxlut including number of plots by domain (n.total).
  ## - error tab with warnings.
  #################################################################################
  stopiferror <- ifelse(checkcnt, TRUE, FALSE)
  showwarnings <- ifelse(checkcnt, TRUE, FALSE)
  chkdat <- FIESTA::check.pltcnt(pltx=pltmodelx, puniqueid=puniqueid, unitlut=auxlut,
		unitvars=unitvars, strvars=PSstrvar, stopiferror=stopiferror, 
		showwarnings=showwarnings)
  auxlut <- chkdat$unitlut
  
  
  returnlst <- list(pltmodelx=pltmodelx[,c(puniqueid, unitvars, prednames, PSstrvar), with=FALSE], 
			auxlut=auxlut, npixels=npixels)

  return(returnlst)
}
