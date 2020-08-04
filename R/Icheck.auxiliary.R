check.auxiliary <- function(pltx, puniqueid, module="GB", MAmethod=NULL, 
	unitvar=NULL, unitvar2=NULL, unitarea=NULL, areavar=NULL, unitcombine=FALSE, 
	auxlut=NULL, prednames=NULL, strata=FALSE, PSstrvar=NULL, predfac=NULL, 
	nonresp=FALSE, substrvar=NULL, getwt=FALSE, getwtvar=NULL, P2POINTCNT=NULL, 
	npixelvar=NULL, stratcombine=FALSE, minplotnum.unit=10, minplotnum.strat=2, 
	na.rm=TRUE, removeifnostrata=FALSE, pvars2keep=NULL){

  ##################################################################################
  ## DESCRIPTION: 
  ## Check auxiliary table. 
  ## - if (module == "SA" || (module == "MA" && MAmethod != "HT"), must not be NULL
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
  ## If strata, aggregate variables (getwtvar, npixelvar, 'strwt') to unitvar(s), strvar
  ## If more than one unitvar, concatenate into 1 unitvar
  ## Check number of plots by unitvar and strvar (if strata=TRUE)
  ## - If number of plots < minplotnum.strat (2), an error occurs, must collapse plots
  ## - If number of plots between minplotnum.strat and minplotnum.unit (10), a warning 
  ##		is displayed, suggesting to collapse plots
  ## - If removeifnostrata, remove plots that have an stratum assignment not in auxlut
  ## - Collapse strata if number of plots in strata is less than minplotnum.strat and
  ## 		stratcombine=TRUE
  ## - Collapse units if number of plots in units is less than minplotnum.unit and
  ##	 	unitcombine=TRUE
  ## If module = GB,PB, and getwt=TRUE, calculate strat weights (i.e., proportions)
  ##################################################################################

  ## Set global variables
  ONEUNIT=ONESTRAT=npixels=nonsampplots=strvars=PLOT_STATUS_CD=strwt=testlt1=
		pixels=unitstrgrplut <- NULL
  gui <- FALSE
  unitvars <- c(unitvar, unitvar2)

  ## Check auxlut
  stopifnull <- ifelse((module == "SA" || (module == "MA" && MAmethod != "HT")),
				TRUE, FALSE)
  auxlut <- FIESTA::pcheck.table(auxlut, gui=gui, tabnm="auxlut",
 		caption="Strata table?", nullcheck=TRUE, stopifnull=stopifnull)

 
  if (module %in% c("GB", "PB")) {
    if (strata) {
      auxnmlst <- names(auxlut)
      PSstrvar <- FIESTA::pcheck.varchar(var2check=PSstrvar, varnm="PSstrvar", 
		gui=gui, checklst=c("NONE", names(auxlut)), caption="Strata variable?", 
		warn="strata variable not in auxlut", stopifnull=TRUE) 
      strvars <- c(strvars, PSstrvar) 

      ## Check for a total value in the last row of table..  If exists, exclude.
      lastrow <- auxlut[nrow(auxlut),]
      if (length(grep("Total", lastrow, ignore.case=TRUE)) > 0)
        auxlut <- auxlut[-nrow(auxlut)]

      ## If auxlut is NULL, generate based on unitvars in pltx
      #############################################################
      if (is.null(auxlut)) {
        auxlut <- unique(pltx[, c(unitvar2, unitvar), with=FALSE]) 
      } else {
        if (any(unitvars == "ONEUNIT") && !"ONEUNIT" %in% names(auxlut))
          auxlut[, ONEUNIT := 1]
      }

      ## Check substrvar (if nonresp)
      #############################################################
      if (module =="GB" && nonresp) {
        ## Check substrvar (if nonresp)
        #############################################################
        ## Remove PSstrvar from strlutnmlst
        auxnmlst <- auxnmlst[which(!auxnmlst %in% PSstrvar)]
        substrvar <- FIESTA::pcheck.varchar(var2check=substrvar, varnm="substrvar", 
		gui=gui, checklst=auxnmlst, caption="Substrata variable?", 
		warn="substrata variable not in strata table", stopifnull=TRUE)
        strvars <- c(strvars, substrvar)

        ## Check number of plots and concatenate to 1 variable
        nbrplots <- pltx[, .N, by=strvars]
        if (any(nbrplots$N < minplotnum.strat)) {
          stop("not enough plots in substrata... consider collapsing")
        } else {
          auxlut[["STRATASUB"]] <- paste(auxlut[[PSstrvar]], auxlut[[substrvar]], sep="-")
          pltx[["STRATASUB"]] <- paste(pltx[[PSstrvar]], pltx[[substrvar]], sep="-")
          PSstrvar <- "STRATASUB"
        }
      } 

    } else {
      message("no strata")
      PSstrvar <- "ONESTRAT"
      pltx[, ONESTRAT := 1]

      auxlut <- unique(pltx[, c(unitvar2, unitvar), with=FALSE]) 
      auxlut[, ONESTRAT := 1]
      auxlut[, strwt := 1]
      getwt <- FALSE
      getwtvar <- NULL
    }

    ## Check getwt and calculate strata weights (proportion by estimation unit)
    ###################################################################################
    getwt <- FIESTA::pcheck.logical(getwt, varnm="getwt", title="Get strata weights?", 
		first="YES", gui=gui, stopifnull=TRUE)

    if (getwt) {
      ## Check getwtvar from strata table.
      getwtvar <- pcheck.varchar(var2check=getwtvar, varnm="getwtvar", gui=gui, 
		checklst=names(auxlut), caption="Acre variable?", stopifinvalid=FALSE)
      if (is.null(getwtvar) || !getwtvar %in% names(auxlut)) {
        if ("strwt" %in% names(auxlut)) {
          message("using strwt column for strata weights")
          getwt <- FALSE
        } else {
          stop("getwtvar not in auxlut")
        }
      }
    }
  } else { ## !module %in% c("GB", "PB")

    auxnmlst <- names(auxlut)
    if (module == "MA") {
      ## Check npixelvar from strata table.
      ############################################################################
      npixelvar <- FIESTA::pcheck.varchar(var2check=npixelvar, varnm="npixelvar", gui=gui, 
		checklst=auxnmlst, caption="Acre variable?", stopifinvalid=TRUE)

      ## Create data frame of number of pixels by estimation unit
      ############################################################################
      if (npixelvar %in% auxnmlst) 
        npixels <- unique(auxlut[, c(unitvar, npixelvar), with=FALSE])
    }

    ## Check continuous variables
    ############################################################################
    predcon <- prednames[!prednames %in% predfac]
 
    ## Check for missing variables in auxlut
    missvars <- predcon[which(!predcon %in% auxnmlst)]
    if (length(missvars) > 0) 
      stop("auxvar not in auxlut: ", paste(missvars, collapse=", "))
 
  
    ## Check for NA values in continuous variables in auxlut
    ############################################################################
    aux.na <- sapply(predcon, function(x, auxlut){ sum(is.na(auxlut[,x, with=FALSE])) }, auxlut)
    if (any(aux.na) > 0) {
      message(aux.na[aux.na > 0], " NA values in variable: ", 
		paste(names(aux.na[aux.na > 0]), collapse=", "))
      if (na.rm) 
        auxlut <- na.omit(auxlut, cols=predcon)
    }

    ## Check values of continuous variable in prednames...  make sure in range of pltx
    ## Still workin on
    #pmin <- pltx[, lapply(.SD, min), by=unitvar, .SDcols=predcon]
    #pmax <- pltx[, lapply(.SD, max), by=unitvar, .SDcols=predcon]
 
    ## Check categorical (predfac) variables
    ############################################################################
    predfac <- unique(c(PSstrvar, predfac))
    if (length(predfac) > 0) {
      notfac <- predfac[!pltx[, lapply(.SD, is.factor), .SDcols=predfac][[1]]]
      if (length(notfac) > 0) {
        for (fac in notfac) {
          fac.levels <- sort(unique(pltx[[fac]]))
          pltx[[fac]] <- factor(pltx[[fac]], levels=fac.levels) 
        }
      }
 
      ## Check for missing variables and factor values
      for (fac in predfac) {
        pltvals <- sort(unique(pltx[[fac]]))
        facnmlst <- auxnmlst[grep(fac, auxnmlst)]
        missvars <- facnmlst[!facnmlst %in% paste0(fac, ".", pltvals)]
        if (length(missvars) > 0) 
          message("auxvar not in tables: ", paste(missvars, collapse=", "))
      }

      ## Set up dummy variables for PSstrvar
      if (!is.null(PSstrvar)) {
        PScols <- lapply(predfac, function(x, auxnmlst) 
		auxnmlst[grep(x, auxnmlst)], auxnmlst)[[1]]
        PSvalslst <- sapply(strsplit(PScols, paste0(PSstrvar, ".")), "[[", 2)

        auxlut <- data.table(PSvalslst, auxlut[, t(.SD), by=unitvar, .SDcols=PScols])
        setnames(auxlut, c(PSstrvar, unitvars, "Prop"))
        setcolorder(auxlut, c(unitvar, PSstrvar, "Prop"))
      }
    }
  }
 
  if (strata) {
    ## Aggregate variables to unitvar (and strvars)
    ############################
    sumvars <- c(getwtvar, npixelvar, "strwt")
    sumvars <- sumvars[sumvars %in% names(auxlut)]
    if (length(sumvars) > 0) {
      auxlut <- auxlut[, sum(get(sumvars), na.rm=TRUE), by=c(unitvars, strvars)]
      setnames(auxlut, c(unitvars, PSstrvar, substrvar, sumvars))
    }
    setkeyv(auxlut, unitvars)

    ## Check if class of unitvar in auxlut matches class of unitvar in pltx
    tabs <- FIESTA::check.matchclass(pltx, auxlut, c(unitvars, strvars),
		tab1txt="pltmodel", tab2txt="auxlut")
    pltx <- tabs$tab1
    auxlut <- tabs$tab2

    ## Check that the strunitvars in pltx are all in auxlut
    pltx <- check.matchval(tab1=pltx, tab2=auxlut, var1=c(unitvars, strvars), 
		tab1txt="plt", tab2txt="auxlut", stopifmiss=TRUE)

    ## Check that the strunitvars in auxlut are all in pltx
    auxlut <- check.matchval(tab1=auxlut, tab2=pltx, var1=c(unitvars, strvars), 
		tab1txt="auxlut", tab2txt="plt", stopifmiss=FALSE)
  }


  ## Merge P2POINTCNT to auxlut
  ##################################################
  if (!P2POINTCNT %in% names(auxlut) && !is.null(P2POINTCNT)) {

    ## Check if class of unitvar in auxlut matches class of unitvar in P2POINTCNT
    tabs <- FIESTA::check.matchclass(P2POINTCNT, auxlut, c(unitvars, strvars),
		tab1txt="P2POINTCNT", tab2txt="auxlut")
    P2POINTCNT <- tabs$tab1
    auxlut <- tabs$tab2
    
    ## Check that the strunitvars in pltx are all in auxlut
    auxlut2 <- merge(auxlut, P2POINTCNT, by=c(unitvars, strvars))
    if (nrow(auxlut2) == nrow(auxlut))
      auxlut <- auxlut2
  }

  ##################################################################################
  ## If more than one unitvar, concatenate into 1 unitvar
  ##################################################################################
  if (length(unitvars) > 1) {
    unitvar12 <- paste(unitvar2, unitvar, sep="-")
    auxlut[[unitvar12]] <- paste(auxlut[[unitvar2]], auxlut[[unitvar]], sep="-")
    auxlut[, c(unitvar, unitvar2) := NULL]

    pltx[[unitvar12]] <- paste(pltx[[unitvar2]], pltx[[unitvar]], sep="-")
    if (!is.null(unitarea)) {
      unitarea[[unitvar12]] <- paste(unitarea[[unitvar2]], unitarea[[unitvar]], sep="-")
      unitarea[, c(unitvar, unitvar2) := NULL]
    }
    unitvar <- unitvar12
  }


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
  pltcnts <- check.pltcnt(pltx=pltx, puniqueid=puniqueid, 
		unitlut=auxlut, unitvars=unitvar, strvars=PSstrvar, 
		stopiferror=FALSE, showwarnings=TRUE, minplotnum.unit=minplotnum.unit, 
		minplotnum.strat=minplotnum.strat)
  auxlut <- pltcnts$unitlut
  errtab <- pltcnts$errtab
  nostrat <- pltcnts$nostrat

  ## Remove plots that have a stratum assignment that is not in auxlut
  if (!is.null(nostrat) && removeifnostrata && !is.null(PSstrvar)) {
    pltx <- pltx[pltx[[PSstrvar]] %in% unique(auxlut[[PSstrvar]]),]
    message("removing plots with invalid strata assignments")
  }
   
  ###################################################################################
  ## Collapse strata and/or estimation unit classes if errtab warnings
  ###################################################################################
  if (any(errtab$errtyp == "warn")) {
    collapse <- strat.collapse(stratacnt=auxlut, errtab=errtab, pltstratx=pltx, 
		minplotnum.unit=minplotnum.unit, minplotnum.strat=minplotnum.strat, 
		unitarea=unitarea, areavar=areavar, unitvar=unitvar, unitvar2=unitvar2,
		strvar=PSstrvar, stratcombine=stratcombine, unitcombine=unitcombine,
 		vars2combine=c(getwtvar, npixelvar, "strwt"))
    auxlut <- collapse$strlut
    unitvar <- collapse$unitvar
    PSstrvar <- collapse$strvar
    pltx <- collapse$pltstratx

    if (stratcombine)
      unitstrgrplut <- collapse$unitstrgrplut
  } 

 
  if (module %in% c("GB", "PB")) {
    if (getwt) {
      ## Caculate weight
      if (is.character(auxlut[[getwtvar]]) && sum(grepl(",", auxlut[[getwtvar]]) > 0))
        auxlut[[getwtvar]] <- as.numeric(gsub(",", "", auxlut[[getwtvar]]))
      auxlut[, strwt:=prop.table(get(getwtvar)), by=unitvar]
#     auxlut <- auxlut[, list(strwt = sum(strwt/.N, na.rm=TRUE)), by=c(unitvar, PSstrvar)]

    } else {
      ## Check for strwt
      if (!"strwt" %in% names(auxlut)) {
        if ("STRWT" %in% names(auxlut)) {
          setnames(auxlut, "STRWT", "strwt") 
        } else {
          stop("strwt variable not in auxlut... include getwtvar and getwt=TRUE")
        }
      } 
      ## Check to see if sum(strwt) = 1
      test <- auxlut[, round(sum(strwt, na.rm=TRUE)), by=unitvar]
      if (any(testlt1$V1) > 0) {
        stop("strwts should add to 1")
      } else {
        getwt <- FALSE
      }
      getwtvar <- NULL
    } 
  }

  ## Set key to strlut and unitarea
  strunitvars <- c(unitvar, PSstrvar)
  setkeyv(auxlut, strunitvars)
  setkeyv(pltx, puniqueid)

  if (!is.null(unitarea)) 
    setkeyv(unitarea, unitvar)


#  returnlst <- list(pltx=pltx[,c(puniqueid, unitvar, prednames, PSstrvar), with=FALSE], 
#			auxlut=auxlut, unitarea=unitarea, unitvar=unitvar, PSstrvar=PSstrvar,
#			prednames=prednames, predfac=predfac)

  returnlst <- list(pltx=pltx, auxlut=auxlut, unitarea=unitarea, unitvar=unitvar, 
			PSstrvar=PSstrvar, prednames=prednames, predfac=predfac)

  if (!is.null(npixelvar)) {
    returnlst$npixels <- npixels
    returnlst$npixelvar <- npixelvar
  }
  if (nonresp)
    returnlst$nonsampplots <- nonsampplots

  if (!is.null(unitstrgrplut)) 
    returnlst$stratcombinelut <- unitstrgrplut


  return(returnlst)
}
