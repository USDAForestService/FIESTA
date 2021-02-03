anGBpopDB_evalEndyrlst <- function(evalEndyrlst, states, evalType="VOL", 
	data_dsn=NULL, isseed=FALSE, ppsanm="pop_plot_stratum_assgn", 
	savedata=FALSE, out_dsn=NULL, out_fmt="sqlite", outfolder=NULL, 
	outfn.pre=NULL, outfn.date=FALSE, overwrite=FALSE) {
  ## DESCRIPTION: estimates for each evalid in list
  
  ## Set global variables
  tree=seed <- NULL
  gui <- FALSE
  istree=FALSE


  ## Check savedata 
  savedata <- FIESTA::pcheck.logical(savedata, varnm="savedata", 
		title="Save data extraction?", first="NO", gui=gui) 


  ## If savedata, check output file names
  ################################################################
  if (savedata) { 
    outlst <- pcheck.output(gui=gui, out_dsn=out_dsn, out_fmt=out_fmt, 
		outfolder=outfolder, outfn.pre=outfn.pre, outfn.date=outfn.date, 
		overwrite=overwrite)
    out_dsn <- outlst$out_dsn
    outfolder <- outlst$outfolder
    out_fmt <- outlst$out_fmt
#  } else {
#    out_dsn <- "tmpdat.sqlite"
#    out_fmt <- "sqlite"
#    outfolder <- tempdir()
#    savedata <- TRUE
  }


#  ## Check evalidlst
#  if (any(nchar(evalidlst) > 6)) {
#    stop("invalid evalidlst... must be integer values < 6 digits")
#  }
#  ## Generate list of evalids to generate estimates from
#  evalidlst <- data.table::transpose(lapply(stcds, function(x) 
#		paste0(x, substr(evalEndyrlst, 3, 4), "01")))
#  names(evalidlst) <- paste0("eval", evalEndyrlst)


  ## Get evalid list
  ########################################################
  evalidlst <- DBgetEvalid(states=states, RS=NULL, evalEndyr=evalEndyrlst, 
		evalType=evalType)$evalidlist
  evalidlst <- transpose(evalidlst)
  names(evalidlst) <- paste0("eval", evalEndyrlst)
 

  ## Get data from FIA Datamart and store in temporary directory
  #########################################################################
  if (!is.null(data_dsn)) {
    if (!is.null(outfolder)) {
      data_dsn <- file.path(outfolder, data_dsn)
    }
    plt <- "plot"
    cond <- "cond"
    pltassgn <- ppsanm
    dblayers <- c("plot", "cond", ppsanm)

    if (istree) {
      tree <- "tree"
      dblayers <- c(dblayers, "tree")
    }
    if (isseed) {
      seed <- "seed"
      dblayers <- c(dblayers, "seed")
    }

    ## Check table in database
    dtabs <- DBI::dbListTables(DBI::dbConnect(RSQLite::SQLite(), data_dsn))
    message("data tables: ", toString(dtabs))
    dtablst <- c("plot", "cond")
    if (!all(dtablst %in% dtabs)) {
      dtab.miss <- dtablst[!dtablst %in% dtabs]
      message("missing tables in database: ", toString(dtab.miss))
    }

  } else {
    if (any(evalType %in% c("VOL", "CHNG"))) {
      istree <- TRUE
    }
    pltdat <- DBgetPlots(evalid=evalidlst, istree=istree, isseed=isseed,
	savedata=savedata, out_fmt=out_fmt, outfolder=outfolder, out_dsn=out_dsn)
    plt <- pltdat$plt
    cond <- pltdat$cond
    tree <- pltdat$tree
    seed <- pltdat$seed
    pltassgn <- pltdat$POP_PLOT_STRATUM_ASSGN

  }


  ## Get strata information
  ########################################################
  stratdat <- DBgetStrata(evalid=evalidlst, getassgn=FALSE,
	savedata=savedata, out_dsn=out_dsn, out_fmt=out_fmt, outfolder=outfolder)
  names(stratdat)

  unitvar <- stratdat$unitvar
  strvar <- stratdat$strvar
  strwtvar <- stratdat$strwtvar
  getwt <- stratdat$getwt
  getwtvar <- stratdat$getwtvar
  areavar <- stratdat$areavar
  pltassgnid <- "PLT_CN"
  pjoinid <- "CN"

  

  ## Loop through Evaluation year list
  #########################################################################
  GBpop_evalEndyrlst <- list()

  for (evalnm in names(evalidlst)) {
    evalid <- evalidlst[[evalnm]]
    message("getting population data for ", toString(evalnm))
    message("evalid: ", toString(evalid))

    unitarea <- stratdat$unitarea[stratdat$unitarea$EVALID %in% evalid,] 
    stratalut <- stratdat$stratalut[stratdat$stratalut$EVALID %in% evalid,]


    GBpopdat <- modGBpop(cond=cond, plt=plt, tree=tree, seed=seed,
		pltassgn=pltassgn, pltassgnid=pltassgnid, evalid=evalid,
		dsn=data_dsn, pjoinid=pjoinid, 
		strata=TRUE, unitvar=unitvar, unitarea=unitarea, areavar=areavar, 
		stratalut=stratalut, strvar=strvar, getwt=TRUE, getwtvar=getwtvar,
 		stratcombine=TRUE, saveobj=savedata, outfolder=outfolder, 
		outfn.pre=evalnm, outfn.date=FALSE, overwrite=overwrite)

    GBpop_evalEndyrlst[[evalnm]] <- GBpopdat
  }

  return(GBpop_evalEndyrlst)
}
