anGBpop_evalEndyrlst <- function(evalEndyrlst, bnd_layer, bnd_dsn=NULL, 
	bnd.att=NULL, bnd.filter=NULL, evalType="VOL", data_dsn=NULL, 
	isseed=FALSE, xymeasCur=TRUE, pjoinid="PLOT_ID",  
	xy_dsn=NULL, xy_layer="xyCur_ACTUAL", xy.uniqueid="PLOT_ID", 
	xvar="LON_ACTUAL", yvar="LAT_ACTUAL", xy.crs=4269, strat_layer=NULL, 
	strat_lut=NULL, savedata=FALSE, out_dsn=NULL, out_fmt="sqlite", 
	outfolder=NULL, outfn.pre=NULL, outfn.date=FALSE, overwrite_dsn=FALSE,
	overwrite_layer=TRUE) {

  ## DESCRIPTION: get estimates for each evalid in list
  
  ## Set global variables
  tree=seed <- NULL
  gui <- FALSE
  istree <- FALSE

  #############################################################################
  ## Import boundary
  #############################################################################
  bndx <- pcheck.spatial(layer=bnd_layer, dsn=bnd_dsn, caption="boundary")
 
  ## bnd.filter
  bndx <- datFilter(bndx, xfilter=bnd.filter, stopifnull=TRUE)$xf


  ## Get intersecting states
  ########################################################
  states <- spGetStates(bndx, stbnd=FIESTA::stunitco, 
			stbnd.att="STATENM", RS=NULL, showsteps=TRUE)$statenames
  stcds <- pcheck.states(states, statereturn = "VALUE")

  ## Generate list of evalids to generate estimates from
  evalidlst <- data.table::transpose(lapply(stcds, function(x) 
		paste0(x, substr(evalEndyrlst, 3, 4), "01")))
  names(evalidlst) <- paste0("eval", evalEndyrlst)
  

  ## Get evalid list
  ########################################################
  evalidlst <- DBgetEvalid(states=states, RS=NULL, evalEndyr=evalEndyrlst, 
		evalType=evalType)$evalidlist
  evalidlst <- transpose(evalidlst)
  names(evalidlst) <- paste0("eval", evalEndyrlst)


  ## Check savedata 
  savedata <- FIESTA::pcheck.logical(savedata, varnm="savedata", 
		title="Save data extraction?", first="NO", gui=gui) 


  ## If savedata, check output file names
  ################################################################
  if (savedata) { 
    outlst <- pcheck.output(gui=gui, out_dsn=out_dsn, out_fmt=out_fmt, 
		outfolder=outfolder, outfn.pre=outfn.pre, outfn.date=outfn.date, 
		overwrite=overwrite_dsn)
    out_dsn <- outlst$out_dsn
    outfolder <- outlst$outfolder
    out_fmt <- outlst$out_fmt
#  } else {
#    out_dsn <- "tmpdat.sqlite"
#    out_fmt <- "sqlite"
#    outfolder <- tempdir()
#    savedata <- TRUE
  }


  ## Get data from FIA Datamart and store in temporary directory
  #########################################################################
  if (is.null(data_dsn)) {
    if (any(evalType %in% c("VOL", "CHNG"))) {
      istree <- TRUE
    }
    pltdat <- DBgetPlots(evalid=evalidlst, istree=istree, isseed=isseed,
		xymeasCur=xymeasCur, savedata=savedata, out_fmt=out_fmt, 
		outfolder=outfolder, out_dsn=out_dsn, overwrite_layer=overwrite_layer)
    plt <- pltdat$plt
    cond <- pltdat$cond
    tree <- pltdat$tree
    seed <- pltdat$seed

  } else {
    if (!is.null(outfolder)) {
      data_dsn <- file.path(outfolder, data_dsn)
    }
    plt <- "plot"
    cond <- "cond"
    dblayers <- c("plot", "cond")

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
    dtablst <- c("plot", "cond", "unitarea", "stratalut")
    if (!all(dtablst %in% dtabs)) {
      dtab.miss <- dtablst[!dtablst %in% dtabs]
      message("missing tables in database: ", toString(dtab.miss))
    }
  }


  ## Check xy data
  #########################################################################
  if (is.null(xy_dsn)) {
    xy_dsn <- data_dsn
  }
  if (!is.null(xy_dsn)) {
    xy_conn <- DBI::dbConnect(RSQLite::SQLite(), xy_dsn)
    xytablst <- DBI::dbListTables(xy_conn)
    xy_layer <- pcheck.varchar(xy_layer, varnm="xy_layer", 
			checklst=xytablst, caption="xy layer")

    xyfieldlst <- DBI::dbListFields(xy_conn, xy_layer)
    xy.uniqueid <- pcheck.varchar(xy.uniqueid, varnm="xy.uniqueid", 
		checklst=xyfieldlst, caption="xy uniqueid")
    xvar <- pcheck.varchar(xvar, varnm="xvar", 
		checklst=xyfieldlst, caption="x variable")
    yvar <- pcheck.varchar(yvar, varnm="yvar", 
		checklst=xyfieldlst, caption="y variable")

    ## put together query to extract xy for evaluation
    xyvars <- toString(paste0("xy.", c(xy.uniqueid, xvar, yvar)))


    ## Define query to extract data from SQLite database
    #########################################################################
    xy.qry <- paste("select distinct", xyvars, 
		"from", xy_layer, " xy",
		"join plot p on(p.PLOT_ID = xy.PLOT_ID)
		where p.STATECD in(", toString(stcds), ")")
    xy_conn <- DBI::dbConnect(RSQLite::SQLite(), xy_dsn)
    xy <- DBI::dbGetQuery(xy_conn, xy.qry)
    DBI::dbDisconnect(xy_conn)

  } else {
    xy <- "xyCur_PUBLIC"
    xvar <- "LON_PUBLIC"
    yvar <- "LAT_PUBLIC"
    xy.crs <- 4269

    if (xymeasCur) {
      xy <- pltdat$xyCur_PUBLIC
      xy.uniqueid <- "PLOT_ID"
      pjoinid <- "PLOT_ID"
    } else {
      xy <- pltdat$xy_PUBLIC
      xy.uniqueid <- "CN"
      pjoinid <- "CN"
    }
  }


  ## Get strata and/or estimation unit information
  #########################################################################
  if (!is.null(strat_layer)) {
    strata <- TRUE

    ## Get strata information for boundary
    stratdat <- FIESTA::spGetStrata(xyplt=xy, uniqueid=xy.uniqueid, 
		xvar=xvar, yvar=yvar, xy.crs=xy.crs, unit_layer=bndx, 
		unitvar=bnd.att, strat_layer=strat_layer, strat_lut=strat_lut, 
		rast.NODATA=0)
    pltassgn <- stratdat$pltassgn
    pltassgnid <- stratdat$pltassgnid
    unitarea <- stratdat$unitarea
    unitvar <- stratdat$unitvar
    areavar <- stratdat$areavar
    stratalut <- stratdat$stratalut
    strvar <- stratdat$strvar
  } else {
    strata <- FALSE
    stratalut <- NULL
    strvar <- NULL

    ## Get estimation unit information for boundary
    unitdat <- FIESTA::spGetEstUnit(xyplt=xy, uniqueid=xy.uniqueid, 
		xvar=xvar, yvar=yvar, 
          	unit_layer=bnd_layer, unit_dsn=bnd_dsn, 
		unitvar=bnd.att, unit.filter=bnd.filter)
    pltassgn <- unitdat$pltassgn
    pltassgnid <- unitdat$pltassgnid
    unitarea <- unitdat$unitarea
    unitvar <- unitdat$unitvar
    areavar <- unitdat$areavar
  }


  ## Add a temporary table to out_dsn
  #########################################################################
  if (!is.null(data_dsn)) {
    write2sqlite(pltassgn, data_dsn, out_name="ppsatmp", overwrite=TRUE,
		index.unique=pltassgnid)
    ppsanm <- "ppsatmp"
  } else {
    ppsanm <- pltassgn
  }


  ## Loop through Evaluation list
  #########################################################################
  GBpop_evalEndyrlst <- list()

  for (evalnm in names(evalidlst)) {
    message("getting population data for ", toString(evalnm))

    ## Get population data for boundary
    #########################################################################
    GBpopdat <- modGBpop(cond=cond, plt=plt, tree=tree, seed=seed,
		pltassgn=ppsanm, pltassgnid=pltassgnid,
		dsn=data_dsn, pjoinid=pjoinid, strata=strata, unitvar=unitvar, 
		unitarea=unitarea, areavar=areavar, stratalut=stratalut, strvar=strvar, 
		getwt=FALSE, stratcombine=TRUE, saveobj=FALSE, savedata=savedata, 
		outfolder=NULL, outfn.pre=evalnm, outfn.date=FALSE, 
		overwrite_layer=overwrite_layer)
 
    GBpop_evalEndyrlst[[evalnm]] <- GBpopdat
  }


  if (!is.null(out_dsn)) {
    ## Remove temporary table from data_dsn
    out_conn <- DBI::dbConnect(RSQLite::SQLite(), out_dsn)
    DBI::dbRemoveTable(out_conn, "ppsatmp")
    DBI::dbDisconnect(out_conn)
  }

  return(GBpop_evalEndyrlst)

}



