check.auxiliaryWWW <- function(pltassgn, 
                               module = "GB", 
	                             unitvar = "domain_unit", 
                               unitarea = NULL, 
                               areavar = "total_acres",
	                             auxlut = NULL, 
                               strata = FALSE,
                               strvar = NULL, 
                               prednames = NULL, 
                               predfac = NULL, 
	                             minplotnum.unit = 2, 
                               unit.action = "remove", 
                               minplotnum.strat = 2, 
                               removeifnostrata = FALSE,
                               P2POINTCNT = NULL) {

  ##################################################################################
  ## DESCRIPTION:
  ## Check auxiliary table.
  ## If strata, aggregate variables (getwtvar, npixelvar, strwtvar) to unitvar(s), strvar
  ## If more than one unitvar, concatenate into 1 unitvar
  ## Check number of plots by unitvar and strvar (if strata=TRUE)
  ## - If number of plots < minplotnum.strat (2), an error occurs, must collapse plots
  ## - If number of plots between minplotnum.strat and minplotnum.unit (10), a warning
  ##		is displayed, suggesting to collapse plots
  ## - If removeifnostrata, remove plots that have a stratum assignment not in auxlut
  ## - Collapse strata if number of plots in strata is less than minplotnum.strat and
  ## 		stratcombine=TRUE
  ## - Collapse units if number of plots in units is less than minplotnum.unit and
  ##	 	unit.action='combine'
  ## If module = GB,PB, and getwt=TRUE, calculate strat weights (i.e., proportions)
  ##################################################################################
  
  ## Set global variables
  unitstrgrplut=vars2combine=unitlessthan=errtyp=unitlevels <- NULL

  ## Set default parameters
  pltassgnid <- "plt_cn"
  puniqueid <- pltassgnid
  npixelvar=getwtvar <- "pixel_count"
  stratcombine <- TRUE
  pivotstrat <- FALSE
  removeifnostrata <- TRUE
  auxtext <- "auxlut"
  removetext <- "unitarea"
  standardize <- TRUE
  makedummy <- TRUE
  AOI <- TRUE
  
  
  ## Make factors
  #######################################################################
  ## Make unitvar a factor for retaining order for collapsing{
  pltx <- copy(pltassgn)
  unitlevels <- unique(auxlut[[unitvar]])
  auxlut[[unitvar]] <- factor(auxlut[[unitvar]], levels = unitlevels)
  pltx[[unitvar]] <- factor(pltx[[unitvar]], levels = unitlevels)
  
  
  
  ## Check strata
  #######################################################################
  if (strata) {

    ## Make strvar a factor for retaining order for collapsing
    stratlevels <- unique(auxlut[[strvar]])
    auxlut[[strvar]] <- factor(auxlut[[strvar]], levels = stratlevels)
    pltx[[strvar]] <- factor(pltx[[strvar]], levels = stratlevels)
    nullcols <- prednames[prednames != strvar]
    if (length(nullcols) > 0) {
      pltx[, (nullcols) := NULL]
    }
    setkeyv(pltx, puniqueid)
    
#    ## Define sumvars to aggregate
#    sumvars <- c(getwtvar, strwtvar, npixelvar)
#    
#    ## Aggregate strata by estimation unit to make sure no duplicate values exist
#    sumvars <- unique(sumvars[sumvars %in% names(auxlut)])
#    auxlut <- auxlut[, lapply(.SD, sum, na.rm=TRUE),
#                       by=c(unitvar, strvar), .SDcols=sumvars]
#    setnames(auxlut, c(unitvar, strvar, sumvars))
    
    #setkeyv(auxlut, unitvars)
    strunitvars <- c(unitvar, strvar)
  } 
  
  
  if (module == "MA") {
    ## Create data frame of number of pixels by estimation unit
    npixels <- unique(auxlut[, c(unitvar, npixelvar), with=FALSE])
    names(npixels) <- c(unitvar, "npixels")
  }
  ## Get order of unitvar for collapsing
  if (!is.null(unitlevels) && !is.factor(auxlut[[unitvar]])) {
    auxlut[[unitvar]] <- factor(auxlut[[unitvar]], levels = unitlevels)
    setorderv(auxlut, c(unitvar, unitvar2))
  }
  

  ## Merge P2POINTCNT to auxlut
  ##################################################
  p2pointcntnm <- findnm("P2POINTCNT", names(auxlut), returnNULL = TRUE)
  if (!is.null(P2POINTCNT) && !is.null(p2pointcntnm)) {
    
    ## Check if class of unitvar in auxlut matches class of unitvar in P2POINTCNT
    tabs <- check.matchclass(P2POINTCNT, auxlut, strunitvars,
                             tab1txt="P2POINTCNT", tab2txt="auxlut")
    P2POINTCNT <- tabs$tab1
    auxlut <- tabs$tab2
    
    ## Check that the strunitvars in pltx are all in auxlut
    auxlut <- merge(auxlut, P2POINTCNT, by=strunitvars, all.x=TRUE)
    auxlut[is.na(auxlut)] <- 0
    auxlut$NBRPLOTS <- NULL
  }
  
  ## Redefine strunitvars
  strunitvars <- c(unitvar, strvar)
  
  
  ###################################################################################
  ## Check number of plots by unitvar
  ###################################################################################
  ## If number of plots < minplotnum.strat, an error occurs, must collapse plots.
  ## If number of plots between minplotnum.strat and minplotnum.unit, a warning
  ##		is displayed, suggesting to collapse plots.
  ## Returns:
  ## - auxlut including number of plots by strata (n.strata) and/or domain (n.total).
  ## - error tab with warnings.
  ## - nostrat included if there are plot strata values with no strata in auxlut
  #################################################################################
  pltcnts <- check.pltcnt(pltx = pltx, puniqueid = puniqueid,
		                      unitlut = copy(auxlut), 
		                      unitvars = unitvar, strvars = strvar,
		                      stopiferror = FALSE, showwarnings = TRUE, 
		                      minplotnum.unit = minplotnum.unit,
		                      minplotnum.strat = minplotnum.strat)
  auxlut <- pltcnts$unitlut
  errtab <- pltcnts$errtab
  nostrat <- pltcnts$nostrat

  ## If unit.action="remove", remove estimation with less than minplotnum.unit plots
  ## If unit.action="keep", return estimation units with less than minplotnum.unit as NA
  unitltmin <- 0
  
  if (any(auxlut$n.total < minplotnum.unit)) {
    unitltmin <- unique(auxlut[[unitvar]][auxlut$n.total < minplotnum.unit])
    if (length(unitltmin) == 0) unitltmin <- NULL
    if (!is.null(unitltmin)) {
      if (unit.action %in% c("remove", "keep")) {
        
        if (unit.action == "remove") {
          message("removing domains with plots less than ", minplotnum.unit,
                  ": ", toString(unitltmin))
        } else {
          message("there are ", length(unitltmin), " units with less than minplotnum.unit (", 
                  minplotnum.unit, ") plots:\n", 
                  toString(unitltmin)) 
          message("returning NA values for these units...")
          message("if want to combine units that are less than minplotnum.unit, ",
                  "set unit.action='combine' in unit.opts parameter... ",
                  "\ncheck returned object, stratwarnlut\n")	
          
        }
        auxlut <- auxlut[!auxlut[[unitvar]] %in% unitltmin,]
        unitltmin <- unique(auxlut[[unitvar]][auxlut$n.total < minplotnum.unit])
        if (length(unitltmin) == 0) unitltmin <- NULL
      }
    }
  }

  ## Remove plots that have a stratum assignment that is not in auxlut
  if (!is.null(nostrat) && removeifnostrata && !is.null(strvar)) {
    pltx <- pltx[pltx[[strvar]] %in% unique(auxlut[[strvar]]),]
    message("removing plots with invalid strata assignments")
  }

  ###################################################################################
  ## Collapse strata and/or estimation unit classes if errtab warnings
  ###################################################################################
  if (any(errtab$errtyp == "warn")) {
    auxlut <- auxlut[auxlut[[unitvar]] %in% errtab[[unitvar]],]
    if (any(c(getwtvar, npixelvar, strwtvar) %in% names(auxlut))) {
      vars2combine <- unique(c(vars2combine, c(getwtvar, npixelvar, strwtvar)))
      vars2combine <- vars2combine[vars2combine %in% names(auxlut)]
    }

    if (minplotnum.strat > minplotnum.unit) {
      minplotnum.strat <- minplotnum.unit
    }
    unitcombine <- ifelse(unit.action == 'combine', TRUE, FALSE)
    collapse <- strat.collapse(stratacnt = auxlut, 
                               pltstratx = pltx, 
                               minplotnum.unit = minplotnum.unit, 
                               minplotnum.strat = minplotnum.strat, 
                               unitarea = unitarea, areavar = areavar, 
                               unitvar = unitvar, strvar = strvar, 
                               stratcombine = stratcombine, 
                               unitcombine = unitcombine, 
                               vars2combine = vars2combine)						 

    if ((stratcombine || unitcombine) && !is.null(collapse$unitstrgrplut)) {
	    message("check strata groups in returned object, stratcombinelut\n")
      unitstrgrplut <- collapse$unitstrgrplut
	    unitstrgrplut <- merge(errtab, unitstrgrplut, by=strunitvars)
	    if (!is.null(unitvar2)) {
	      unitstrgrplut[, (unitvars) := tstrsplit(get(unitvar), "-", fixed=TRUE)]
	    }
    }

    ## Get new variable definitions
    auxlut <- collapse$strlut
    unitvar <- collapse$unitvar
    strvar <- collapse$strvar
    pltx <- collapse$pltstratx
    unitarea <- collapse$unitarea
 
    if (unitvar == "unitnew") {
      unitvars <- unitvar
	  }
    strunitvars <- c(unitvar, strvar)
  }

  ###################################################################################
  ## Check categorical (predfac) variables
  ###################################################################################
  if (module %in% c("MA", "SA")) {
    
    predvariance <- pltx[, lapply(.SD, var, na.rm=TRUE), .SDcols=prednames]
    ## Remove predictors with variance = 0
    if (any(predvariance == 0)) {
      predvariance0 <- names(predvariance)[predvariance == 0]
      message("predictor has variance equal to 0... removing from analysis: ",
		           toString(predvariance0))
      prednames <- prednames[!prednames %in% predvariance0]
      predfac <- predfac[!predfac %in% predvariance0]
    }
    auxnmlst <- names(auxlut)
    predfac <- unique(c(strvar, predfac))

    if (length(predfac) > 0) {
      ## Check for missing variables and factor values
      for (fac in predfac) {
        pltvals <- sort(unique(pltx[[fac]]))
        facnmlst <- auxnmlst[grep(fac, auxnmlst)]
        if (length(facnmlst) == 0) {
          message("auxvar not in tables: ", paste(fac, collapse=", "))
        } else {
          pivotstrat <- TRUE
        }

        ## Set up dummy variables for strvar
        if (makedummy) {
          ## Get factor levels
          fac.levels <- as.numeric(sapply(strsplit(facnmlst,
			                   paste0(fac,".")), '[', 2))
          pltx[[fac]] <- factor(pltx[[fac]], levels=fac.levels)

          ## Set factor levels to keep and delete from auxlut.
          fac.unitcol.keep <- paste(fac, fac.levels[-1], sep=".")
          fac.unitcol.del <- paste(fac, fac.levels[1], sep=".")
          auxlut[[fac.unitcol.del]] <- NULL

          ## Rename factor variables and add names to predictor list
          facs <- paste0(fac, fac.levels[-1])
          names(auxlut)[names(auxlut) %in% fac.unitcol.keep] <- facs
          unitpreds <- unique(c(prednames[prednames != fac], facs))

          ## Create dummy variables for factor levels - 1
          dtfac <- pltx[, as.data.table(model.matrix(~.,
				                  data=pltx[, fac, with=FALSE]))][,-1]
          pltx <- cbind(pltx, dtfac)
          #pltx[, (fac) := NULL]

          ## Remove old name and add new names to predictor list
          prednames <- unique(c(prednames[prednames != fac], facs))
        }
        if (pivotstrat) {
          ## Pivot strata table
          if (!is.null(strvar) && fac == strvar) {
            auxlut <- strat.pivot(x=auxlut, strvar=strvar, unitvars=unitvars,
			                   strwtvar="Prop", strat.levels=fac.levels)
          }
        }
        strvars <- strvar
      }
    }
  }

  ##################################################################################
  ## Check estimation unit values from auxlut with unitarea
  ##################################################################################
  subsetrows <- ifelse(unit.action == "remove", TRUE, FALSE)
#  if (!is.null(unitarea)) {
#    auxlut <- check.matchval(auxlut, unitarea, unitvar, tab1txt=auxtext,
#			                tab2txt=removetext, subsetrows=subsetrows)
#  }

  ##################################################################################
  ## Calculate weights
  ##################################################################################
  #if (module %in% c("GB", "PB")) {
#  if (strata) {
#    setkeyv(auxlut, c(unitvar, strvar))
#    suppressWarnings(auxlut[, strwt := prop.table(get(getwtvar)), by=unitvar])
#    strwtvar <- "strwt"
#  } 

  ## Set key to strlut and unitarea
  setkeyv(auxlut, strunitvars)
  setkeyv(pltx, puniqueid)
  setcolorder(auxlut, c(strunitvars, names(auxlut)[!names(auxlut) %in% strunitvars]))
  
    
  if (module != "GB" && !is.null(prednames) && standardize) {
    standardized <- preds.standardize(plt=pltx, aux=auxlut, prednames=prednames)
    pltx <- standardized$plt
    auxlut <- standardized$aux
  }

  returnlst <- list(pltx = data.table(pltx),
		                auxlut = data.table(auxlut),
		                unitvar = unitvar, unitvars = unitvar,
		                prednames = prednames, predfac = predfac)

  if (!is.null(unitarea)) {
    setkeyv(unitarea, unitvar)
    returnlst$unitarea <- data.table(unitarea)
  }

  if (strata) {
    returnlst$pltxGB <- data.table(pltx)
    returnlst$strvar <- strvar
    if (length(unitlessthan) > 0) {
      returnlst$unitNA <- unitlessthan
    }
    if (!is.null(unitstrgrplut)) {
      returnlst$stratcombinelut <- data.frame(unitstrgrplut, check.names=FALSE)
    } else if (!is.null(errtab)) {
	    returnlst$stratwarnlut <- errtab
	  }
  }
  returnlst$unitltmin <- unitltmin
  
  if (module == "MA") {
    returnlst$npixels <- data.table(npixels)
  }
  
  
  
  return(returnlst)
}
