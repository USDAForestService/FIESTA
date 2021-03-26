anGBpop_eval <- function(evalidlst=NULL, evalCur=FALSE, evalEndyrlst=NULL, 
	states=NULL, RS=NULL, evalType="VOL", datsource="datamart", data_dsn=NULL, 
	isseed=FALSE, ppsanm="pop_plot_stratum_assgn", byEndyr=FALSE,
	savedata=FALSE, outfolder=NULL, ...) {
  ## DESCRIPTION: estimates for each evalid in list
  
  ## Set global variables
  tree=seed=seed_layer=unitvar2 <- NULL
  gui <- FALSE
  istree=evalAll <- FALSE
  GBpop_evallst <- list()

  ## Check savedata 
  savedata <- FIESTA::pcheck.logical(savedata, varnm="savedata", 
		title="Save data extraction?", first="NO", gui=gui) 

  ## Check outfolder 
  if (savedata) {
    outfolder <- FIESTA::pcheck.outfolder(outfolder, gui=gui)
  } 

  ## Check byEndyr
  byEndyr <- FIESTA::pcheck.logical(byEndyr, varnm="byEndyr", 
		title="By Endyr?", first="NO", gui=gui) 
  if (byEndyr) {
    if (is.null(evalEndyrlst)) {
      stop("must include evalEndyrlst if byEndyr=TRUE")
    }
  }

  ## Get list of evalids from database
  ########################################################
  if (datsource == "sqlite") {
    if (is.null(data_dsn)) {
      stop("must include data_dsn")
    }

    dbconn <- DBtestSQLite(data_dsn, dbconnopen=TRUE, showlist=FALSE)
    tablst <- DBI::dbListTables(dbconn)
    stratalut <- chkdbtab(tablst, "pop_stratum")
    unitarea <- chkdbtab(tablst, "pop_estn_unit")
    pltassgn <- chkdbtab(tablst, ppsanm, stopifnull=TRUE)
    if (is.null(evalidlst) && !evalCur && is.null(evalEndyrlst)) {
      evalAll <- TRUE
    }
    evaliddat <- DBgetEvalid(datsource=datsource, data_dsn=data_dsn, 
		RS=RS, states=states, evalAll=evalAll,
		evalid=evalidlst, evalCur=evalAll, evalEndyr=evalEndyrlst, 
		evalType=evalType)
    evalidlst <- evaliddat$evalidlist

  } else {   ## datsource="datamart"
    evaliddat <- DBgetEvalid(evalid=evalidlst, evalEndyr=evalEndyrlst, 
		states=states, RS=RS, evalAll=FALSE, evalType=evalType)
    evalidlst <- evaliddat$evalidlist
  }
  if (byEndyr) {
    if (unique(unlist(lapply(evalidlst, length))) != length(evalEndyrlst)) {
      message("list of evalids does not match evalEndyrlst")
      message("evalidlst: ", toString(evalidlst), "\n", 
			"evalEndyrlst: ", toString(evalEndyrlst))
      stop()
    }
    evalidlst <- transpose(evalidlst)
    names(evalidlst) <- paste0("eval", evalEndyrlst)
  } else {
    evalidlst <- unlist(evalidlst)
    #evalidstcds <- substr(unlist(evalidlst), nchar(evalidlst)-5, nchar(evalidlst)-4)
    #evalidstabbr <- sapply(evalidstcds, pcheck.states, statereturn="ABBR")
    #names(evalidlst) <- paste0(evalidstabbr, evalidlst)
    names(evalidlst) <- paste0("eval", evalidlst)
  }  
  if (any(evalType %in% c("VOL", "CHNG"))) {
    istree <- TRUE
  }

  ## Get population from the sqlite database with pop tables
  #########################################################################
  if (datsource == "sqlite" && (!is.null(stratalut) && !is.null(unitarea))) {
    if (isseed) seed_layer <- "seed"

    if (byEndyr) {
      evalidloop <- names(evalidlst)
    } else {
      evalidloop <- unlist(evalidlst)
    }

    for (i in 1:length(evalidloop)) {
      evalid <- evalidloop[i]
      evalnm <- names(evalid)
      if (byEndyr) {
        evalnm <- evalid
        evalid <- evalidlst[[evalid]]
      }
      message("getting population data for ", toString(evalnm))
      message("evalid: ", toString(evalid))
 
      GBpopdat <- modGBpop(popType=evalType, cond="cond", plt="plot", 
		tree="tree", seed=seed_layer, pltassgn=pltassgn, pltassgnid="PLT_CN", 
		evalid=evalid, dsn=data_dsn, pjoinid="CN", strata=TRUE, unitvar="ESTN_UNIT", 
		unitvar2="STATECD", unitarea=unitarea, areavar="AREA_USED", 
		stratalut=stratalut, strvar="STRATUMCD", getwt=TRUE, getwtvar="P1POINTCNT",
 		stratcombine=TRUE, savedata=savedata, outfolder=outfolder, 
		outfn.pre=evalnm, ...)
      GBpop_evallst[[evalnm]] <- GBpopdat
    }
  } else {  
    ## Get population from datamart or sqlite database without pop tables
    #########################################################################
    if (datsource == "sqlite") {
      pltdat <- spGetPlots(evalid=evalidlst, datsource="sqlite", data_dsn=data_dsn,
		istree=istree, isseed=isseed, savePOP=TRUE, savedata=FALSE)
      tabs <- pltdat$tabs
      plt <- tabs$pltx
      cond <- tabs$condx
      tree <- tabs$treex
      seed <- tabs$seedx
      pop_plot_stratum_assgn <- tabs$pop_plot_stratum_assgnx
      
    } else {
      pltdat <- DBgetPlots(evalid=evalidlst, istree=istree, isseed=isseed,
		savePOP=TRUE, savedata=FALSE)
      plt <- pltdat$plt
      cond <- pltdat$cond
      tree <- pltdat$tree
      seed <- pltdat$seed
      pop_plot_stratum_assgn <- pltdat$pop_plot_stratum_assgn
    }

    ## Get strata information for evalidlst
    GBstratdat <- DBgetStrata(evalid=evalidlst, 
			POP_PLOT_STRATUM_ASSGN=pop_plot_stratum_assgn)

    if (byEndyr) {
      evalidloop <- names(evalidlst)
    } else {
      evalidloop <- unlist(evalidlst)
    }

    for (i in 1:length(evalidloop)) {
      evalid <- evalidloop[i]
      evalnm <- names(evalid)
      if (byEndyr) {
        evalid <- evalidlst[[evalid]]
      }
      message("getting population data for ", toString(evalnm))
      message("evalid: ", toString(evalid))

      GBpopdat <- modGBpop(popType=evalType, cond=cond, plt=plt, tree=tree, seed=seed,
		evalid=evalid, dsn=data_dsn, pltassgn=pltassgn, GBstratdat=GBstratdat, 
		savedata=savedata, outfolder=outfolder, outfn.pre=evalnm, ...)
      GBpop_evallst[[evalnm]] <- GBpopdat
    }
  }
  return(GBpop_evallst)
}
