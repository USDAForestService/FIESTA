anGBpop_evalidlst <- function(evalidlst, data_dsn=NULL, istree=FALSE, 
		isseed=FALSE, ppsanm="pop_plot_stratum_assgn", pjoinid="CN",
		savedata=FALSE, outfolder=NULL, out_fmt="sqlite", 
		out_dsn=NULL, overwrite=FALSE) {
  ## DESCRIPTION: estimates for each evalid in list
  
  ## Set global variables
  outfn.date <- FALSE
  outfn.pre <- NULL
  tname=sname <- NULL
  gui <- FALSE

  ## Check savedata 
  savedata <- FIESTA::pcheck.logical(savedata, varnm="savedata", 
		title="Save data extraction?", first="NO", gui=gui) 

  ## Get data from FIAdatamart and store in temporary directory
  ########################################################
  if (savedata) { 
    out_dsn <- tryCatch(DBtestSQLite(data_dsn, outfolder=outfolder, 
			overwrite=TRUE), 
			error=function(e) {
				message(e) 
			return(NULL) })
    if (is.null(out_dsn)) {
      out_dsn <- tryCatch(DBcreateSQLite(data_dsn, outfolder=outfolder, 
		overwrite=FALSE), error=function(e) return(NULL))
    }
  }


  ## Get plot data
  ########################################################
  pltdat <- DBgetPlots(evalid=evalidlst, istree=istree, isseed=isseed, 
	savedata=savedata, out_fmt=out_fmt, out_dsn=out_dsn, 
	overwrite=TRUE)
  plt <- pltdat$plt
  cond <- pltdat$cond
  tree <- pltdat$tree
  pltassgn <- pltdat$POP_PLOT_STRATUM_ASSGN


  ## Get strata information
  ########################################################
  stratdat <- DBgetStrata(evalid=evalidlst, getassgn=FALSE,
	savedata=savedata, out_fmt=out_fmt, out_dsn=out_dsn)
  names(stratdat)

  unitvar <- stratdat$unitvar
  strvar <- stratdat$strvar
  strwtvar <- stratdat$strwtvar
  getwt <- stratdat$getwt
  getwtvar <- stratdat$getwtvar
  areavar <- stratdat$areavar
  pltassgnid <- "PLT_CN"



  ## Loop through Evaluation list
  #########################################################################
  GBpop_evalidlst <- list()

  for (evalid in evalidlst) {
    message("getting estimates from ", evalid)
    evalnm <- paste0("eval_",evalid)

    unitarea <- stratdat$unitarea[stratdat$unitarea$EVALID == evalid,] 
    stratalut <- stratdat$stratalut[stratdat$stratalut$EVALID == evalid,]

    if (istree) tname <- "tree"
    if (isseed) sname <- "seed"

    if (!is.null(out_dsn)) {
      plt <- "plot"
      cond <- "cond"
      tree <- tname
      seed <- sname
      pltassgn <- ppsanm
    } else {
      plt <- pltdat$plt
      cond <- pltdat$cond
      tree <- pltdat$tree
      seed <- pltdat$seed
      pltassgn <- pltdat$POP_PLOT_STRATUM_ASSGN
    }

    GBpopdat <- modGBpop(cond=cond, plt=plt, tree=tree, seed=seed,
		pltassgn=pltassgn, pltassgnid=pltassgnid, evalid=evalid,
		dsn=out_dsn, pjoinid=pjoinid, strata=TRUE, unitvar=unitvar, 
		unitarea=unitarea, areavar=areavar, stratalut=stratalut, strvar=strvar, 
		getwt=TRUE, getwtvar=getwtvar, stratcombine=TRUE, saveobj=savedata, 
		outfolder=outfolder, outfn.pre=evalnm, outfn.date=FALSE, 
		overwrite=overwrite)

    GBpop_evalidlst[[evalnm]] <- GBpopdat
  }

  return(GBpop_evalidlst)
}
