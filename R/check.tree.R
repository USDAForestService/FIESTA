check.tree <- function(gui, treef, seedf=NULL, estseed="none", condf=NULL,
	plt=NULL, bycond=TRUE, bytdom, tuniqueid=NULL, cuniqueid=NULL,
	condid="CONDID", puniqueid=NULL, esttype="TREE", ratiotype="PERACRE",
	estvarn=NULL, estvarn.TPA=TRUE, estvarn.filter=NULL, estvarn.name=NULL,
	esttotn=TRUE, estvard=NULL, estvard.TPA=TRUE, estvard.filter=NULL,
	estvard.name=NULL, esttotd=TRUE, tdomvar=NULL, tdomvar2=NULL,
	adjtree=FALSE, adjvar="tadjfac", adjtpa=FALSE, metric=FALSE, ACI=FALSE) {

  ###################################################################################
  ### GETS ESTIMATION DATA FROM TREE TABLE
  ###################################################################################

  ## Set global variables
  tdomvarlstn=estunitsd <- NULL

  ## SET ESTIMATION VARIABLE LIST
  estvarlst.not <- c("TREE", "CONDID", "PREV_TRE_CN", "SUBP")
  estvarlst <- names(treef)[!names(treef) %in% estvarlst.not] 	## Estimation variables

  ## GET TREE ESTIMATION VARIABLE AND CHECK IF IN TREE DATA SET (NUMERATOR)
  varnm <- ifelse(esttype == "TREE", "estvar", "estvarn")
  estvarn <- pcheck.varchar(var2check=estvarn, varnm=varnm, gui=gui,
		checklst=estvarlst, caption=paste0("Est variable (NUM)"),
		warn=paste(estvarn, "not in tree table"), stopifnull=TRUE)

  ## GETS TPA (NUMERATOR)
  estvarn.TPA <- pcheck.logical(estvarn.TPA, varnm="estvarn.TPA",
		title=paste0("Calculate TPA (NUM)"), first="YES", gui=gui)
  if (!grepl("TPA", estvarn)) {
    if (estvarn.TPA) message("multiplying ", estvarn, " by TPA")
  }
  ## GET NAME FOR ESTIMATION VARIABLE FOR ALL TREE DOMAINS
#  if (is.null(estvarn.name)) {
#    estvarn.name <- paste0(estvarn, "_SUM")
#  } else if (!is.character(estvarn.name)) {
  if (!is.null(estvarn.name) && !is.character(estvarn.name)) {
     stop("invalid estvarn.name.. must be a string")
  }
  ## CHECK FOR NA VALUES IN TPA_UNADJ
#  if (sum(is.na(unique(treef$TPA_UNADJ))) > 0) {
#    nbrna <- nrow(treef[is.na(treef$TPA_UNADJ),])
#    stop(paste("check tree table.. there are", nbrna, "NA values in TPA_UNADJ variable"))
#  }

  ## TO INCREASE TREES PER ACRE IF ONLY USING 1 SUBPLOT
  if (adjtpa && "SUBP" %in% names(treef)) {
    if (length(unique(treef$SUBP)) == 1) {
      adjTPA <- 4
    } else if (length(unique(treef$SUBP)) == 2) {
      adjTPA <- 2
    } else {
      stop("can only adjust subplots with 1 or 2 subplots")
    }
  } else {
    adjTPA <- 1
  }

  if (estseed == "only") {
    seedonly <- TRUE
    addseed <- FALSE
  } else if (estseed == "add") {
    addseed <- TRUE
    seedonly <- FALSE
  } else {
    seedonly=addseed <- FALSE
  }


  ### GET TREE DATA (& TREE DOMAIN DATA) AGGREGATED TO CONDITION (NUMERATOR)
  #####################################################################################
  if (bytdom) {
    pivot <- ifelse(esttype == "RATIO", TRUE, FALSE)

    suppressWarnings(
    tdomdata <- datSumTreeDom(tree=treef, seed=seedf, cond=condf, plt=plt, 
           tuniqueid=tuniqueid, cuniqueid=cuniqueid, puniqueid=puniqueid, 
           bycond=bycond, condid=condid, tsumvar=estvarn, TPA=estvarn.TPA, 
           tdomtot=esttotn, tdomtotnm=estvarn.name, tfilter=estvarn.filter, 
		tdomvar=tdomvar, tdomvar2=tdomvar2, adjtree=adjtree,
		adjvar=adjvar, adjTPA=adjTPA, checkNA=FALSE, pivot=pivot, metric=metric,
           addseed=addseed, seedonly=seedonly))
    if (is.null(tdomdata)) return(NULL)
    tdomdat <- tdomdata$tdomdat
    if (!pivot) {
      tdomdat <- tdomdat[!is.na(tdomdat[[tdomvar]]),]
    }
    tdomvarn <- tdomdata$tdomtotnm
    tdomvarlstn <- tdomdata$tdomlst
    estunitsn <- tdomdata$estunits

  } else {
 
    suppressWarnings(
    treedata <- datSumTree(tree=treef, seed=seedf, cond=condf, plt=plt, 
           tuniqueid=tuniqueid, cuniqueid=cuniqueid, puniqueid=puniqueid, 
           bycond=bycond, condid=condid,
 		tsumvarlst=estvarn, tsumvarnmlst=estvarn.name, TPA=estvarn.TPA,
		tfilter=estvarn.filter, adjtree=adjtree, adjvar=adjvar, checkNA=FALSE,
 		metric=metric, addseed=addseed, seedonly=seedonly))
    if (is.null(treedata)) return(NULL)
    tdomdat <- treedata$treedat
    tdomvarn <- treedata$sumvars
    estunitsn <- treedata$estunits
  }

  ###################################################################################
  ### GET ESTIMATION DATA (& TREE DOMAIN DATA) FROM TREE TABLE AND
  ### AGGREGATE TO CONDITION (DENOMINATOR)
  ###################################################################################

  if (ratiotype == "PERTREE") {

    #################################################################################
    ### GETS ESTIMATION DATA (DENOMINATOR)
    #################################################################################

    ## GET TREE ESTIMATION VARIABLE (DENOMINATOR) AND CHECK IF IN TREE DATA SET
    if (is.null(estvard))
      estvard <- pcheck.varchar(var2check=estvard, varnm="estvard", gui=gui,
		checklst=estvarlst, caption=paste0("Est variable (DEN)"),
		warn=paste(estvard, "not in tree table"), stopifnull=TRUE)

    ## GETS TPA (NUMERATOR)
    estvard.TPA <- estvarn.TPA

    ## GET NAME FOR ESTIMATION VARIABLE FOR ALL TREE DOMAINS
    if (!is.null(estvard.name) && !is.character(estvard.name))
      stop("invalid estvard.name.. must be a string")

    ### GET TREE DATA (& TREE DOMAIN DATA) AGGREGATED TO CONDITION (DENOMINATOR)
    #################################################################################
    if (bytdom) {
      pivot <- ifelse(esttype == "RATIO", TRUE, FALSE)

      suppressWarnings(
      tdomdata <- datSumTreeDom(tree=treef, seed=seedf, cond=condf, plt=plt, 
           tuniqueid=tuniqueid, cuniqueid=cuniqueid, puniqueid=puniqueid, 
           bycond=bycond, condid=condid,
		tsumvar=estvarn, TPA=estvarn.TPA, tdomtot=esttotn, tdomtotnm=estvarn.name,
		tfilter=estvarn.filter, tdomvar=tdomvar, tdomvar2=tdomvar2, adjtree=adjtree,
		adjvar=adjvar, adjTPA=adjTPA, checkNA=FALSE, pivot=pivot, metric=metric,
           addseed=addseed, seedonly=seedonly))
      if (is.null(tdomdata)) {
        message("invalid denominator... returning null")
        return(NULL)
      }
      tdomdatd <- tdomdata$tdomdat
      if (!pivot) {
        tdomdatd <- tdomdatd[!is.na(tdomdatd[[tdomvar]]),]
      }
      tdomvard <- tdomdata$tdomtotnm
      tdomvarlstd <- tdomdata$tdomlst
      estunitsd <- tdomdata$estunits

      tdombadlst <- tdomvarlstn[which(!tdomvarlstn %in% tdomvarlstd)]
      if (length(tdombadlst) > 0) {
        warning("there are more tree domains in the numerator than in the denominator")
      }

      ## Change names in denominator
      names(tdomdatd)[names(tdomdatd) %in% tdomvarlstd] <- paste(tdomvarlstd, "d", sep=".")
      names(tdomdatd)[names(tdomdatd) == tdomvard] <- paste(tdomvard, "d", sep=".")
      tdomvard <- paste(tdomvard, "d", sep=".")
      tdomvarlstd <- paste(tdomvarlstd, "d", sep=".")

    } else {

      suppressWarnings(
      treedata <- datSumTree(tree=treef, seed=seedf, cond=condf, plt=plt, 
           tuniqueid=tuniqueid, cuniqueid=cuniqueid, puniqueid=puniqueid, 
           bycond=bycond, condid=condid,
 		tsumvarlst=estvard, tsumvarnmlst=estvard.name, TPA=estvard.TPA,
		tfilter=estvard.filter, adjtree=adjtree, checkNA=FALSE, metric=metric,
           addseed=addseed, seedonly=seedonly))
      if (is.null(treedata)) return(NULL)
      tdomdatd <- treedata$treedat
      tdomvard <- treedata$sumvars
      estunitsd <- treedata$estunits

      names(tdomdatd)[names(tdomdatd) == tdomvard] <- paste(tdomvard, "d", sep=".")
      tdomvard <- paste(tdomvard, "d", sep=".")
      tdomvarlstd <- NULL
    }

    ## Merge table with denominator to table with numerator
    tdomdat <- merge(tdomdat, tdomdatd, by=names(condf))

  } else {
    tdomvard <- NULL
    tdomvarlstd <- NULL
  }

  if (esttype == "RATIO") {
    treedat <- list(tdomdat=tdomdat, estvarn=estvarn, estvarn.name=tdomvarn,
		estvarn.filter=estvarn.filter, tdomvarlstn=tdomvarlstn, estvard=estvard,
 		estvard.name=tdomvard, estvard.filter=estvard.filter, tdomvarlstd=tdomvarlstd,
		estunitsn=estunitsn, estunitsd=estunitsd)
  } else {
    treedat <- list(tdomdat=tdomdat, estvar=estvarn, estvar.name=tdomvarn,
		estvar.filter=estvarn.filter, tdomvarlst=tdomvarlstn, estunits=estunitsn)
  }
}

