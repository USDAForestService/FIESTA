check.auxiliary <- function(pltx, puniqueid, module="GB", strata=FALSE,
	unitvar=NULL, unitvar2=NULL, unitarea=NULL, areavar=NULL,
	auxlut=NULL, prednames=NULL, strvar=NULL, predfac=NULL, makedummy=FALSE,
	nonresp=FALSE, RHGlut=NULL, getwt=FALSE, getwtvar=NULL,
	strwtvar='strwt', P2POINTCNT=NULL, npixelvar=NULL, stratcombine=FALSE,
	minplotnum.unit=10, unit.action="keep", unitlevels=NULL, 
	minplotnum.strat=2, na.rm=TRUE, removeifnostrata=FALSE, 
 	auxtext="auxlut", removetext="unitarea",
	pvars2keep=NULL, standardize=TRUE, AOI=FALSE, 
	keepadjvars=FALSE, adjvars=NULL) {

  ##################################################################################
  ## DESCRIPTION:
  ## Check auxiliary table.
  ## If module = GB,PB
  ## - if strata=TRUE,
  ##     - check strvar
  ##     - check for total row in auxlut and remove
  ##     - if auxlut is NULL, generate based on unitvars in pltx
  ##     - if nonresp, create table of plot counts by sampled and nonsampled plots
  ## - if strata=FALSE, add ONESTRAT=1 to auxlut and plt table
  ## - check getwt... if TRUE, check getwtvar
  ## If module != GB,PB
  ## - check npixelvar
  ## - check continuous prednames - missing variables and NA values
  ## - check categorical prednames (predfac) - missing variables, factor values
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
  ONEUNIT=npixels=nonsampplots=strvars=PLOT_STATUS_CD=strwt=testlt1=
    pixels=unitstrgrplut=vars2combine=unitlessthan=errtyp <- NULL
  gui=pivotstrat <- FALSE
  unitvars <- c(unitvar2, unitvar)
  strunitvars <- c(unitvars)
  stratalevels <- NULL
  
  ## Check auxlut
  #stopifnull <- ifelse((module == "SA" || (module == "MA" && any(MAmethod != "HT"))),
  #				TRUE, FALSE)
  auxlut <- pcheck.table(auxlut, gui=gui, tabnm="auxlut",
                         caption="Strata table?", nullcheck=TRUE)
  P2POINTCNT <- pcheck.table(P2POINTCNT)
  
  ## Subset auxiliary data to AOI = 1
  if (AOI && "AOI" %in% names(auxlut)) {
    auxlut <- auxlut[auxlut$AOI == 1, ]
    
    if (!is.null(unitarea)) {
      unitarea <- unitarea[unitarea[[unitvar]] %in% auxlut[[unitvar]], ]
    }
  }

  #######################################################################
  ## Check strata
  #######################################################################
  if (strata && module != "SA") {
    auxnmlst <- names(auxlut)
    strvar <- pcheck.varchar(var2check=strvar, varnm="strvar",
                             gui=gui, checklst=c("NONE", names(auxlut)), caption="Strata variable?",
                             warn="strata variable not in stratalut", stopifnull=TRUE)
    strvars <- strvar
    
    ## Check for a total value in the last row of table..  If exists, exclude.
    lastrow <- auxlut[nrow(auxlut),]
    if (length(grep("Total", lastrow, ignore.case=TRUE)) > 0) {
      auxlut <- auxlut[-nrow(auxlut)]
    }
    
    ## Check for NA values in strvar, remove if there are any
    auxlut <- auxlut[!is.na(auxlut[[strvar]]),]
    
    ## Make strvar a factor for retaining order for collapsing
#    if (!is.factor(auxlut[[strvar]])) {
#      auxlut[[strvar]] <- factor(auxlut[[strvar]])
#    }
    
    ## If auxlut is NULL, generate based on unitvars in pltx
    #############################################################
    if (is.null(auxlut)) {
      auxlut <- unique(pltx[, c(unitvar2, unitvar), with=FALSE])
    } else {
      if (any(grepl("ONEUNIT", unitvars))) {
        unittest <- unitvars[any(grepl("ONEUNIT", unitvars))]
        if (length(unittest) > 1) {
          stop("more than one ONEUNIT variable")
        }
        if (!unittest %in% names(auxlut)) {
          auxlut[, (unittest) := 1]
        }
      }
    }
    ## Keep adjustment variables in auxlut (i.e., from FIADB)
    if (keepadjvars && !is.null(adjvars)) {
      adjvars <- adjvars[adjvars %in% names(auxlut)]
      if (length(adjvars) > 0) {
        auxlutadj <- auxlut[, c(unitvars, strvars, adjvars), with=FALSE]
      } else {
        keepadjvars <- FALSE
      }
    }
    
    ## Define sumvars to aggregate
    sumvars <- c(getwtvar, strwtvar, npixelvar)
    
    ## Aggregate strata by estimation unit to make sure no duplicate values exist
    sumvars <- unique(sumvars[sumvars %in% names(auxlut)])
    if (length(sumvars) > 0) {
      auxlut[, (sumvars) := lapply(.SD, as.numeric), .SDcols=sumvars]
      auxlut <- auxlut[, lapply(.SD, sum, na.rm=TRUE),
                       by=c(unitvars, strvars), .SDcols=sumvars]
      setnames(auxlut, c(unitvars, strvars, sumvars))
    }

    #setkeyv(auxlut, unitvars)
    strunitvars <- c(unitvars, strvars)
    
    if (keepadjvars && nrow(auxlut) == nrow(auxlutadj)) {
      auxlut <- merge(auxlut, auxlutadj, by=strunitvars)
    }  
   
    ## Check if class of unitvar in auxlut matches class of unitvar in pltx
    tabs <- check.matchclass(pltx, auxlut, c(unitvars, strvars),
                             tab1txt="pltassgn", tab2txt="auxlut")
    pltx <- tabs$tab1
    auxlut <- tabs$tab2
    
    ## Check that the strunitvars in pltx are all in auxlut
    pltx <- check.matchval(tab1=pltx, tab2=auxlut, var1=c(unitvars, strvars),
                           tab1txt="plt", tab2txt=auxtext, stopifmiss=FALSE, subsetrows=TRUE)

    ## Check that the strunitvars in pltx are all in auxlut
    pltx <- check.matchval(tab1=pltx, tab2=auxlut, var1=c(unitvars, strvars),
                           tab1txt="plt", tab2txt=auxtext, stopifmiss=FALSE)
    
    ## Check that the strunitvars in auxlut are all in pltx
    auxlut <- check.matchval(tab1=auxlut, tab2=pltx, var1=c(unitvars, strvars),
                             tab1txt=auxtext, tab2txt="plt", stopifmiss=FALSE)
    
    
    ## Check getwt and calculate strata weights (proportion by estimation unit)
    ###################################################################################
    getwt <- pcheck.logical(getwt, varnm="getwt", title="Get strata weights?",
                            first="YES", gui=gui, stopifnull=TRUE)
    
    if (getwt) {
      ## Check getwtvar from strata table.
      getwtvar <- pcheck.varchar(var2check=getwtvar, varnm="getwtvar", gui=gui,
                                 checklst=names(auxlut), caption="Acre variable?", stopifinvalid=FALSE)
      if (is.null(getwtvar) || !getwtvar %in% names(auxlut)) {
        if (strwtvar %in% names(auxlut)) {
          #message("using strwtvar column for strata weights")
          getwt <- FALSE
        } else {
          stop("getwtvar not in stratalut")
        }
      }
    }
  } else if (module %in% c("GB", "PB")) {
    
    ## Add a column to identify one strata class for GB or PB modules
    #message("no strata")
    strvar <- checknm("ONESTRAT", names(pltx))
    strwtvar <- "strwt"
    pltx[, (strvar) := 1]
    
    auxlut <- unique(pltx[, c(unitvar2, unitvar), with=FALSE])
    auxlut[, (strvar) := 1]
    auxlut[, (strwtvar) := 1]
    
    if (!is.null(P2POINTCNT)) {
      P2POINTCNT[, (strvar) := 1]
    }
    getwt <- FALSE
    getwtvar <- NULL
    strvars <- c(strvars, strvar)
    strata <- TRUE
    strunitvars <- c(unitvars, strvars)
    
  } else {
    auxnmlst <- names(auxlut)
    missvars <- {}
    
    if (is.null(auxlut)) {
      auxlut <- unique(pltx[, c(unitvar2, unitvar), with=FALSE])
    } else {
      if (any(grepl("ONEUNIT", unitvars))) {
        unittest <- unitvars[any(grepl("ONEUNIT", unitvars))]
        if (length(unittest) > 1) {
          stop("more than one ONEUNIT variable")
        }
        if (!unittest %in% names(auxlut)) {
          auxlut[, (unittest) := 1]
        }
      }
    }
    
    ## Check predictors
    ############################################################################
    if (length(c(strvar, predfac)) > 0) {
      missvars <- c(missvars,
                    predfac[sapply(c(strvar, predfac),
                                   function(x) sum(grepl(x, auxnmlst)) == 0)])
    }
    
    ## Check continuous variables
    ############################################################################
    predcon <- prednames[!prednames %in% predfac]
    if (length(predcon) > 0) {
      missvars <- c(missvars, predcon[which(!predcon %in% auxnmlst)])
    }
    
    if (length(missvars) > 0) {
      for (mvar in missvars) {
        if (any(grepl(mvar, auxnmlst))) {
          mvarnm <- auxnmlst[grepl(mvar, auxnmlst)]
          if (all(grepl("\\.", mvarnm)) && mvar %in% names(pltx)) {
            message(toString(mvarnm), " exists in auxlut... setting ", mvar, " to predfac")
            predfac <- c(predfac, mvar)
            predcon <- prednames[!prednames %in% predfac]
          } else {
            stop("missing predictor variables in auxlut: ", toString(missvars))
          }
        }
      }
    }
    
    ## Check for NA values in continuous variables in auxlut
    ############################################################################
    aux.na <- sapply(predcon,
                     function(x, auxlut){ sum(is.na(auxlut[,x, with=FALSE])) }, auxlut)
    if (any(aux.na) > 0) {
      message(aux.na[aux.na > 0], " NA values in variable: ",
              paste(names(aux.na[aux.na > 0]), collapse=", "))
      if (na.rm)
        auxlut <- na.omit(auxlut, cols=predcon)
    }
  }

  if (module == "MA") {
    auxnmlst <- names(auxlut)
    ## Check npixelvar from strata table.
    ############################################################################
    npixelvar <- pcheck.varchar(var2check=npixelvar, varnm="npixelvar", gui=gui,
                                checklst=auxnmlst, caption="Acre variable?", stopifinvalid=TRUE)
    
    ## Create data frame of number of pixels by estimation unit
    ############################################################################
    if (npixelvar %in% auxnmlst) {
      npixels <- unique(auxlut[, c(unitvar, npixelvar), with=FALSE])
      auxlut[[npixelvar]] <- NULL
    }
  }
  
  ## Get order of unitvar for collapsing
  if (!is.null(unitlevels) && !is.factor(auxlut[[unitvar]])) {
    auxlut[[unitvar]] <- factor(auxlut[[unitvar]], levels = unitlevels)
    setorderv(auxlut, c(unitvar, unitvar2))
    
    if (!is.null(unitarea)) {
      unitarea[[unitvar]] <- factor(unitarea[[unitvar]], levels = unitlevels)
      setorderv(unitarea, c(unitvar, unitvar2))
    }
  }

  ##################################################################################
  ## If more than one unitvar, concatenate into 1 unitvar
  ##################################################################################
  if (length(unitvars) > 1) {
    unitvar21 <- paste(unitvar2, unitvar, sep="-")
    
    if (!is.numeric(unitvar)) {
      unitvar.nchar <- max(nchar(as.character(auxlut[[unitvar]])))
      unitvar.nchar <- formatC(unitvar.nchar, width=2, flag="0")
      sprintfd <- paste0("%", unitvar.nchar, "d")
    
    auxlut[[unitvar21]] <- paste(auxlut[[unitvar2]], auxlut[[unitvar]], sep="-")
##      auxlut[[unitvar21]] <- paste(auxlut[[unitvar2]],
##                                   sprintf(sprintfd, auxlut[[unitvar]]), sep="-")

    #auxlut[, c(unitvar, unitvar2) := NULL]
    pltx[[unitvar21]] <- paste(pltx[[unitvar2]], pltx[[unitvar]], sep="-")
##      pltx[[unitvar21]] <- paste(pltx[[unitvar2]],
##                                 sprintf(sprintfd, auxlut[[unitvar]]), sep="-")
    } else {
      auxlut[[unitvar21]] <- paste(auxlut[[unitvar2]], auxlut[[unitvar]], sep="-")
      pltx[[unitvar21]] <- paste(pltx[[unitvar2]], pltx[[unitvar]], sep="-")
    }
  
    if (!is.null(unitarea)) {
      unitarea[[unitvar21]] <- paste(unitarea[[unitvar2]], unitarea[[unitvar]], sep="-")
      unitarea[, c(unitvar, unitvar2) := NULL]
    }

    if (!is.null(RHGlut)) {
      RHGlut[[unitvar21]] <- paste(RHGlut[[unitvar2]], RHGlut[[unitvar]], sep="-")
      RHGlut[, c(unitvar, unitvar2) := NULL]
    }

    unitvar <- unitvar21
    unitlevels <- unique(auxlut[[unitvar]])
    auxlut[[unitvar]] <- factor(auxlut[[unitvar]], levels = unitlevels)
    pltx[[unitvar]] <- factor(pltx[[unitvar]], levels=unitlevels)
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

  ## Make unitvar a factor for retaining order for collapsing{
  if (!is.factor(auxlut[[unitvar]])) {
    if (is.null(unitlevels)) {
      unitlevels <- unique(auxlut[[unitvar]])
    }
    auxlut[[unitvar]] <- factor(auxlut[[unitvar]], levels=unitlevels)
    pltx[[unitvar]] <- factor(pltx[[unitvar]], levels=unitlevels)
  }
  
  # ## Make strvar a factor for retaining order for collapsing
  # if (!is.factor(auxlut[[strvar]])) {
  #   if (is.null(stratalevels)) {
  #     stratalevels <- sort(unique(auxlut[[strvar]]))
  #   }
  #   auxlut[[strvar]] <- factor(auxlut[[strvar]], levels=stratalevels)
  #   pltx[[strvar]] <- factor(pltx[[strvar]], levels=stratalevels)
  # }
  
  
  ###################################################################################
  ## Check number of plots by unitvar
  ##	 (including partially sampled plots - COND_STATUS_CD=5)
  ##	 (excluding totally nonsampled plots - PLOT_STATUS_CD=1)
  ###################################################################################
  ## If number of plots < minplotnum.strat, an error occurs, must collapse plots.
  ## If number of plots between minplotnum.strat and minplotnum.unit, a warning
  ##		is displayed, suggesting to collapse plots.
  ## Returns:
  ## - auxlut including number of plots by strata (n.strata) and/or domain (n.total).
  ## - error tab with warnings.
  ## - nostrat included if there are plot strata values with no strata in auxlut
  #################################################################################
  if (minplotnum.unit < 2) {
    warning("minplotnum.unit should be at least 2")
    #minplotnum.unit <- 2
  }
  
  if (strata) {
    if (minplotnum.strat > minplotnum.unit) {
      message("minplotnum.unit (", minplotnum.unit, 
              ") is less than minplotnum.strat (", minplotnum.strat, ")... ",
              "changing minplotnum.unit to ", minplotnum.strat)
      minplotnum.strat <- minplotnum.unit
    }
  } else {
    minplotnum.strat <- 0
  }

  pltcnts <- check.pltcnt(pltx = pltx, puniqueid = puniqueid,
		                      unitlut = copy(auxlut), 
		                      unitvars = unitvar, strvars = strvar,
		                      stopiferror = FALSE, showwarnings = TRUE, 
		                      minplotnum.unit = minplotnum.unit,
		                      minplotnum.strat = minplotnum.strat)
  auxlut <- pltcnts$unitlut
  errtab <- pltcnts$errtab
  nostrat <- pltcnts$nostrat
  
  ## order by unitvar and strvar
  if (module == "GB" && !is.null(strvar)) {
    errtab <- errtab[order(errtab[[unitvar]], errtab[[strvar]]),]
    auxlut <- auxlut[order(auxlut[[unitvar]], auxlut[[strvar]]),]
  } else {
    errtab <- errtab[order(errtab[[unitvar]]),]
    auxlut <- auxlut[order(auxlut[[unitvar]]),]
  }

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
          warntab <- ifelse (module == "GB", "stratwarnlut", "unitwarnlut")
          message("there are ", length(unitltmin), " units with less than minplotnum.unit (", 
                  minplotnum.unit, ") plots:\n", 
                  toString(unitltmin)) 
          message("returning NA values for these units...")
          message("if want to combine units that are less than minplotnum.unit, ",
                  "set unit.action='combine' in unit.opts parameter... ",
                  "\ncheck returned object, ", warntab, "\n")	
          
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
  if (!nonresp && any(errtab$errtyp == "warn")) {
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
    } else {
	    ## If more than one unitvar, 
      ## split the concatenated unitvar variable to keep original columns
      if (!is.null(unitvar2)) {
        auxlut[, (unitvars) := tstrsplit(get(unitvar), "-", fixed=TRUE)]
      }
	  }
    strunitvars <- c(unitvar, strvar)
  }

  ###################################################################################
  ## Check categorical (predfac) variables
  ###################################################################################
  if (!module %in% c("GB", "PB") && !strata) {
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
  if (!is.null(unitarea)) {
    auxlut <- check.matchval(auxlut, unitarea, unitvar, tab1txt=auxtext,
			                tab2txt=removetext, subsetrows=subsetrows)
  }

  ##################################################################################
  ## Calculate weights
  ##################################################################################
  #if (module %in% c("GB", "PB")) {
  if (strata) {
    if (getwt) { 
      ## Caculate weight
      if (is.character(auxlut[[getwtvar]]) && sum(grepl(",", auxlut[[getwtvar]]) > 0)) {
        auxlut[[getwtvar]] <- as.numeric(gsub(",", "", auxlut[[getwtvar]]))
      }
      setkeyv(auxlut, c(unitvar, strvar))
      suppressWarnings(auxlut[, strwt := prop.table(get(getwtvar)), by=unitvar])
      strwtvar <- "strwt"

    } else {
      ## Check for strwt
      if (!strwtvar %in% names(auxlut)) {
        stop(strwtvar, " not in stratalut... include getwtvar and getwt=TRUE in strata_opts parameter")
      }
      ## Check to see if sum(strwt) = 1
      test <- auxlut[, round(sum(get(strwtvar), na.rm=TRUE)), by=unitvar]
      if (any(testlt1$V1) > 0) {
        stop("strwts should add to 1")
      } else {
        getwt <- FALSE
      }
      getwtvar <- NULL
    }

  } else {
    if ("n.strata" %in% auxlut) {
      auxlut[["n.strata"]] <- NULL
    }
  }

  ## Set key to strlut and unitarea
  setkeyv(auxlut, strunitvars)
  setkeyv(pltx, puniqueid)

  ## Set column order
  #if (length(unitvars) > 1) {
  #  setcolorder(auxlut, c(unitvars, strunitvars, 
  #			names(auxlut)[!names(auxlut) %in% c(unitvars, strunitvars)]))
  #} else {
    setcolorder(auxlut, c(strunitvars, names(auxlut)[!names(auxlut) %in% strunitvars]))
  #}
    
  if (!is.null(prednames) && standardize) {
    standardized <- preds.standardize(plt=pltx, aux=auxlut, prednames=prednames)
    pltx <- standardized$plt
    auxlut <- standardized$aux
  }
    

  returnlst <- list(pltx = as.data.table(pltx),
		                auxlut = as.data.table(auxlut),
		                unitvar = unitvar, unitvars = unitvars,
		                prednames = prednames, predfac = predfac)

  if (!is.null(unitarea)) {
    setkeyv(unitarea, unitvar)
    returnlst$unitarea <- data.table(unitarea)
  }
  if (!is.null(npixelvar)) {
    returnlst$npixels <- data.table(npixels)
    returnlst$npixelvar <- npixelvar
  }

  if (strata) {
    returnlst$strvar <- strvar
    if (module %in% c("GB", "PB")) {
      returnlst$strwtvar <- strwtvar
    }
    if (length(unitlessthan) > 0) {
      returnlst$unitNA <- unitlessthan
    }
    if (nonresp) {

      ## Check that the class of c(unitvars, strvars) in RHGlut match auxlut
      matchcl <- check.matchclass(tab1=auxlut, tab2=RHGlut, matchcol=c(unitvar, strvars),
		                   tab1txt=auxtext, tab2txt="RHGlut")
      auxlut <- matchcl$tab1
      RHGlut <- matchcl$tab2

      ## Check that th2 values of c(unitvars, strvars) in RHGlut match auxlut
      RHGlut <- check.matchval(tab1=RHGlut, tab2=auxlut, var1=c(unitvar, strvars),
		                   tab1txt="RHGlut", tab2txt=auxtext, stopifmiss=FALSE)
      setcolorder(RHGlut, c(strunitvars, names(RHGlut)[!names(RHGlut) %in% strunitvars]))
      setkeyv(RHGlut, strunitvars)

      returnlst$RHGlut <- RHGlut
      returnlst$nonsampplots <- nonsampplots
    }
    if (!is.null(getwtvar)) {
      returnlst$getwtvar <- getwtvar
    }
  }

  if (!is.null(unitstrgrplut)) {
    returnlst$stratcombinelut <- data.frame(unitstrgrplut, check.names=FALSE)
  } 
  if (!is.null(errtab)) {
	  returnlst$stratwarnlut <- errtab
  }
  #if (length(unitltmin) > 0 &&  unit.action == "keep") {
    returnlst$unitltmin <- unitltmin
  #}
  
  return(returnlst)
}
