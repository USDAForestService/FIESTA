check.tree <- 
  function(gui, treex, seedx = NULL, estseed = "none", 
           condx = NULL, pltx = NULL, bycond = TRUE, tuniqueid = "PLT_CN",  
           cuniqueid = "PLT_CN", condid = "CONDID", puniqueid = NULL, 
           esttype = "TREE", ratiotype = "PERACRE",
	         estvarn = NULL, estvarn.TPA = TRUE, 
           estvarn.filter = NULL, estvarn.derive = NULL,
           estvarn.name = NULL, esttotn = TRUE, 
           estvard = NULL, 
           estvard.filter = NULL, estvard.derive = NULL,
	         estvard.name = NULL, esttotd = TRUE, 
           bytdom, tdomvar = NULL, tdomvar2 = NULL,
           bydomainlst = NULL,
	         adjtree = FALSE, adjvar = "tadjfac", 
           pltassgn = NULL, adjTPA = 1, metric = FALSE, 
	         ACI = FALSE, woodland = "Y", 
           domclassify = NULL, 
           dbconn = NULL, schema = NULL,
           pltidsWITHqry = NULL, pcwhereqry = NULL,
           pjoinid = NULL) {

  ###################################################################################
  ### GETS ESTIMATION DATA FROM TREE TABLE
  ###################################################################################

  ## Set global variables
  tdomvarlstn=estunitsd=variable=tsumvard <- NULL  

  if (estseed == "only") {
    seedonly <- TRUE
    addseed <- FALSE
  } else if (estseed == "add") {
    addseed <- TRUE
    seedonly <- FALSE
  } else {
    seedonly=addseed <- FALSE
  }
  
  ## Check estvarn.derive
  if (!is.null(estvarn.derive)) {
    if (!all(is.list(estvarn.derive), length(estvarn.derive) == 1, !is.null(names(estvarn.derive)))) {
      message("estvarn.derive must be a named list with one element")
      stop()
    }
    if (!is.null(estvarn)) {
      estvarn <- NULL
    }
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
                    pivot = pivot, 
                    metric = metric,
                    addseed = addseed, 
                    seedonly = seedonly, 
                    woodland = woodland,
                    ACI = ACI,
                    tderive = estvarn.derive,
                    domclassify = domclassify,
                    tround = 8,
                    dbconn = dbconn, schema = schema,
                    pltidsWITHqry = pltidsWITHqry,
                    pcwhereqry = pcwhereqry,
                    pjoinid = pjoinid)

    if (is.null(tdomdata)) return(NULL)
    tdomdat <- data.table(tdomdata$tdomdat)
    treeqryn <- tdomdata$treeqry
    domainlst <- tdomdata$domainlst
    tdomainlst <- tdomdata$tdomainlst
    pcdomainlst <- tdomdata$pcdomainlst
    
    tsumvarn <- tdomdata$tsumvarnm
    tdomvarlstn <- tdomdata$tdomlst
    estunitsn <- tdomdata$estunits
    tsumuniqueid <- tdomdata$tsumuniqueid
    classifynmlst <- tdomdata$classifynmlst
    tdomvarnm <- tdomdata$tdomvarnm
    tdomvar2nm <- tdomdata$tdomvar2nm
   
    if (pivot) {
      ## Transpose back to rows
#      tdomdat <- transpose2row(tdomdat, uniqueid = c(tsumuniqueid, pcdomainlst, tdomvar2),
#                             tvars = tdomvarlstn, na.rm = FALSE)
      tdomdat <- transpose2row(tdomdat, uniqueid = c(tsumuniqueid, pcdomainlst),
                               tvars = tdomvarlstn, na.rm = FALSE)
      if (!is.null(tdomvar2)) {
        if ("variable" %in% names(tdomdat)) {
          tdomdat <- data.table(tdomdat, tdomdat[, tstrsplit(variable, "#", fixed=TRUE)])
        } else {
          tdomdat <- data.table(tdomdat, tdomdat[, tstrsplit(get(tdomvar2), "#", fixed=TRUE)])
        }
        setnames(tdomdat, c("V1", "V2", "value"), c(tdomvarnm, tdomvar2nm, tsumvarn))
        tdomdat$variable <- NULL
      } else {
        setnames(tdomdat, c("variable", "value"), c(tdomvarnm, tsumvarn))
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
                 metric = metric, 
                 addseed = addseed, 
                 seedonly = seedonly, 
                 woodland = woodland,
                 ACI = ACI,
                 tderive = estvarn.derive,
                 domclassify = domclassify,
                 tround = 8,
                 dbconn = dbconn, schema = schema,
                 pltidsWITHqry = pltidsWITHqry,
                 pcwhereqry = pcwhereqry,
                 pjoinid = pjoinid)
    if (is.null(treedata)) return(NULL)
    tdomdat <- treedata$treedat
    tsumvarn <- treedata$sumvars
    estunitsn <- treedata$estunits
    treeqryn <- treedata$treeqry
    classifynmlst <- treedata$classifynmlst
    tdomainlst <- treedata$tdomainlst
    pcdomainlst <- treedata$pcdomainlst
  }

  #############################################################################
  ### GET ESTIMATION DATA (& TREE DOMAIN DATA) FROM TREE TABLE AND
  ### AGGREGATE TO CONDITION (DENOMINATOR)
  #############################################################################

  if (ratiotype == "PERTREE") {

    ## Check estvarn.derive
    if (!is.null(estvard.derive)) {
      if (!all(is.list(estvard.derive), length(estvard.derive) == 1, !is.null(names(estvard.derive)))) {
        message("estvarn.derive must be a named list with one element")
        stop()
      }
      if (!is.null(estvard)) {
        estvard <- NULL
      }
    }
    
    ###########################################################################
    ### GETS ESTIMATION DATA (DENOMINATOR)
    ###########################################################################

    ### GET TREE DATA (& TREE DOMAIN DATA) AGGREGATED TO CONDITION (DENOMINATOR)
    ############################################################################
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
                      tdomtot = esttotd, 
                      tdomtotnm = estvard.name, 
                      tfilter = estvard.filter,
                      tdomvar = tdomvar, tdomvar2 = tdomvar2, 
                      bydomainlst = bydomainlst,
                      adjtree = adjtree,
                      adjvar = adjvar, 
                      adjTPA = adjTPA, 
                      pivot = pivot, 
                      metric = metric,
                      addseed = addseed, 
                      seedonly = seedonly, 
                      woodland = woodland,
                      tderive = estvard.derive,
                      domclassify = domclassify,
                      tround = 8,
                      dbconn = dbconn, schema = schema,
                      pltidsWITHqry = pltidsWITHqry,
                      pcwhereqry = pcwhereqry,
                      pjoinid = pjoinid)
      
      if (is.null(tdomdata)) {
        message("invalid denominator... returning null")
        return(NULL)
      }
      tdomdatd <- data.table(tdomdata$tdomdat)
      treeqryd <- tdomdata$treeqry
      
      tsumvard <- tdomdata$tsumvarnm
      tdomvarlstd <- tdomdata$tdomlst
      estunitsd <- tdomdata$estunits

      if (!pivot) {
        tdomdatd <- tdomdatd[!is.na(tdomdatd[[tdomvar]]),]
      }
      tdombadlst <- tdomvarlstn[which(!tdomvarlstn %in% tdomvarlstd)]
      if (length(tdombadlst) > 0) {
        warning("there are more tree domains in the numerator than in the denominator")
      }
      
      ## Change names in denominator
      names(tdomdatd)[names(tdomdatd) %in% tdomvarlstd] <- paste(tdomvarlstd, "d", sep=".")
      names(tdomdatd)[names(tdomdatd) == tsumvard] <- paste(tsumvard, "d", sep=".")
      tsumvard <- paste(tsumvard, "d", sep=".")
      tdomvarlstd <- paste(tdomvarlstd, "d", sep=".")

    } else {

      ## Get summed tree data
      treedata <- 
        datSumTree(tree = treex, seed = seedx, 
                   cond = condx, plt = pltx, 
                   tuniqueid = tuniqueid, 
                   cuniqueid = cuniqueid, puniqueid = puniqueid, 
                   bycond = bycond, condid = condid,
                   tsumvarlst = estvard, 
                   tsumvarnmlst = estvard.name, 
                   TPA = estvarn.TPA,
                   tfilter = estvard.filter,
                   bydomainlst = bydomainlst,
                   adjtree = adjtree, 
                   adjvar = adjvar, 
                   adjTPA = adjTPA,
                   metric = metric, 
                   addseed = addseed, 
                   seedonly = seedonly, 
                   woodland = woodland,
                   tderive = estvard.derive,
                   domclassify = domclassify,
                   tround = 8,
                   dbconn = dbconn, schema = schema,
                   pltidsWITHqry = pltidsWITHqry,
                   pcwhereqry = pcwhereqry,
                   pjoinid = pjoinid)
      if (is.null(treedata)) {
        message("invalid denominator... returning null")
        return(NULL)
      }
      tdomdatd <- treedata$treedat
      tsumvard <- treedata$sumvars
      estunitsd <- treedata$estunits
      treeqryd <- treedata$treeqry
      tsumuniqueid <- treedata$tsumuniqueid
      
      names(tdomdatd)[names(tdomdatd) == tsumvard] <- paste(tsumvard, "d", sep=".")
      tsumvard <- paste(tsumvard, "d", sep=".")
      tdomvarlstd <- NULL
    }
    
    ## Merge table with denominator to table with numerator
    tdomdat <- merge(tdomdat, tdomdatd, by=tsumuniqueid)

  } else {
    tdomvard <- NULL
    tdomvarlstd <- NULL
  }

  if (esttype == "RATIO") {
    
    if (addseed && length(tsumvard) > 1) {
      tsumvard <- tsumvard[length(tsumvard)]
    }
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
    
    if (addseed && length(tsumvarn) > 1) {
      tsumvarn <- tsumvarn[length(tsumvarn)]
    }
    
    treedat <- list(tdomdat = tdomdat, 
                    estvar = estvarn, 
                    estvar.name = tsumvarn,
		                estvar.filter = estvarn.filter, 
		                tdomvarlst = tdomvarlstn, 
		                estunits = estunitsn,
		                tdomainlst = tdomainlst,
		                pcdomainlst = pcdomainlst)
  }
  
  if (!is.null(classifynmlst)) {
    treedat$classifynmlst <- classifynmlst
  }
  treedat$treeqryn <- treeqryn
  
  if (ratiotype == "PERTREE") {
    treedat$treeqryd <- treeqryd
  }
  return(treedat)
}

