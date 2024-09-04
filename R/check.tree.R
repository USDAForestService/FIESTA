check.tree <- 
  function(gui, treex, seedx = NULL, estseed = "none", 
           condx = NULL, pltx = NULL, bycond = TRUE, tuniqueid = NULL,  
           cuniqueid = NULL, condid = "CONDID", puniqueid = NULL, 
           esttype = "TREE", ratiotype = "PERACRE",
	         estvarn = NULL, estvarn.TPA = TRUE, estvarn.filter = NULL, 
           estvarn.name = NULL, esttotn = TRUE, 
           estvard = NULL, estvard.TPA = TRUE, estvard.filter = NULL,
	         estvard.name = NULL, esttotd = TRUE, 
           bytdom, tdomvar = NULL, tdomvar2 = NULL,
           bydomainlst = NULL,
	         adjtree = FALSE, adjvar = "tadjfac", 
           pltassgn = NULL, adjTPA = 1, tderive = NULL, metric = FALSE, 
	         ACI = FALSE, woodland = "Y", dbconn = NULL, 
           pltidsWITHqry = NULL, pcwhereqry = NULL) {

  ###################################################################################
  ### GETS ESTIMATION DATA FROM TREE TABLE
  ###################################################################################

  ## Set global variables
  tdomvarlstn=estunitsd=tclassify=tderive <- NULL  

  if (estseed == "only") {
    seedonly <- TRUE
    addseed <- FALSE
  } else if (estseed == "add") {
    addseed <- TRUE
    seedonly <- FALSE
  } else {
    seedonly=addseed <- FALSE
  }

  if (bytdom) {
    pivot <- ifelse(esttype == "RATIO", TRUE, FALSE)
    tdomdata <- 
      datSumTreeDom(tree = treex, seed = seedx, 
                    cond = condx, plt = pltx, 
                    tuniqueid = tuniqueid, 
                    cuniqueid = cuniqueid, puniqueid = puniqueid, 
                    bycond = bycond, condid = condid,
                    tsumvar = estvarn, 
                    TPA = estvarn.TPA,
                    tdomtot = esttotn, 
                    tdomtotnm = estvarn.name, 
                    tfilter = estvarn.filter,
                    tdomvar = tdomvar, tdomvar2 = tdomvar2, 
                    bydomainlst = bydomainlst,
                    adjtree = adjtree,
                    adjvar = adjvar, 
                    adjTPA = adjTPA, 
                    tclassify = tclassify, tderive = tderive,
                    pivot = pivot, 
                    metric = metric,
                    addseed = addseed, 
                    seedonly = seedonly, 
                    woodland = woodland,
                    tround = 12,
                    dbconn = dbconn, 
                    pltidsWITHqry = pltidsWITHqry,
                    pcwhereqry = pcwhereqry)
    if (is.null(tdomdata)) return(NULL)
    tdomdat <- data.table(tdomdata$tdomdat)
    treeqry <- tdomdata$treeqry
    tdomainlst <- tdomdata$tdomainlst
    pcdomainlst <- tdomdata$pcdomainlst
    
    tsumvarn <- tdomdata$tdomtotnm
    tdomvarlstn <- tdomdata$tdomlst
    estunitsn <- tdomdata$estunits
    tsumuniqueid <- tdomdata$tsumuniqueid

    if (pivot) {
      ## Transpose back to rows
      tdomdat <- transpose2row(tdomdat, uniqueid = c(tsumuniqueid, pcdomainlst),
                             tvars = tdomvarlstn, na.rm = FALSE)
      setnames(tdomdat, c("variable", "value"), c(tdomvar, tsumvarn))

      if (!is.null(tdomvar2)) {
        tdomdat <- data.table(tdomdat, tdomdat[, tstrsplit(variable, "#", fixed=TRUE)])
        setnames(tdomdat, c("V1", "V2"), c(tdomvar, tdomvar2))
        tdomdat$variable <- NULL
      }
    }

  } else {  
    
    ## Get summed tree data
    treedata <- 
      datSumTree(tree = treex, seed = seedx, 
                 cond = condx, plt = pltx, 
                 tuniqueid = tuniqueid, 
                 cuniqueid = cuniqueid, puniqueid = puniqueid, 
                 bycond = bycond, condid = condid,
                 tsumvarlst = estvarn, 
                 tsumvarnmlst = estvarn.name, 
                 TPA = estvarn.TPA,
                 tfilter = estvarn.filter,
                 bydomainlst = bydomainlst,
                 adjtree = adjtree, 
                 adjvar = adjvar, 
                 adjTPA = adjTPA,
                 tderive = tderive,
                 metric = metric, 
                 addseed = addseed, 
                 seedonly = seedonly, 
                 woodland = woodland,
                 tround = 6,
                 dbconn = dbconn, 
                 pltidsWITHqry = pltidsWITHqry,
                 pcwhereqry = pcwhereqry)
    if (is.null(treedata)) return(NULL)
    tdomdat <- treedata$treedat
    tsumvarn <- treedata$sumvars
    estunitsn <- treedata$estunits
    treeqry <- treedata$treeqry
    tdomainlst <- treedata$tdomainlst
    pcdomainlst <- treedata$pcdomainlst
  }

  #############################################################################
  ### GET ESTIMATION DATA (& TREE DOMAIN DATA) FROM TREE TABLE AND
  ### AGGREGATE TO CONDITION (DENOMINATOR)
  #############################################################################

  if (ratiotype == "PERTREE") {

    ###########################################################################
    ### GETS ESTIMATION DATA (DENOMINATOR)
    ###########################################################################

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
    ############################################################################
    if (bytdom) {
      pivot <- ifelse(esttype == "RATIO", TRUE, FALSE)

      suppressWarnings(
      tdomdata <- datSumTreeDom(tree=treef, seed=seedf, cond=condf, plt=plt, 
           tuniqueid=tuniqueid, cuniqueid=cuniqueid, puniqueid=puniqueid, 
           bycond=bycond, condid=condid,
           tsumvar=estvarn, TPA=estvarn.TPA, tdomtot=esttotn, tdomtotnm=estvarn.name,
           tfilter=estvarn.filter, tdomvar=tdomvar, tdomvar2=tdomvar2, 
           adjtree=adjtree, adjvar=adjvar, adjTPA=adjTPA, 
           pivot=pivot, metric=metric,
           addseed=addseed, seedonly=seedonly, woodland=woodland))
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
           tfilter=estvard.filter, adjtree=adjtree, adjTPA=adjTPA, 
           metric=metric,
           addseed=addseed, seedonly=seedonly, woodland=woodland))
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
    treedat <- list(tdomdat = tdomdat, 
                    estvarn = estvarn, 
                    estvarn.name = tsumvarn,
		                estvarn.filter = estvarn.filter, 
		                tdomvarlstn = tdomvarlstn, 
		                estvard = estvard,
 		                estvard.name = tdomvard, 
		                estvard.filter = estvard.filter, 
		                tdomvarlstd = tdomvarlstd,
		                estunitsn = estunitsn, 
		                estunitsd = estunitsd,
		                tdomainlst = tdomainlst,
		                pcdomainlst = pcdomainlst)
  } else {
    treedat <- list(tdomdat = tdomdat, 
                    estvar = estvarn, 
                    estvar.name = tsumvarn,
		                estvar.filter = estvarn.filter, 
		                tdomvarlst = tdomvarlstn, 
		                estunits = estunitsn,
		                tdomainlst = tdomainlst,
		                pcdomainlst = pcdomainlst)
  }
  
  treedat$treeqry <- treeqry
  return(treedat)
}

