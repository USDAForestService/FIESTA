anGBestbnd_evalEndyrlst <- function(evalEndyrlst, data_dsn=NULL, 
		bnd_layer, bnd_dsn=NULL, bnd.att=NULL, bnd.filter=NULL,  
		xy_dsn=NULL, xy_layer="xyCur_ACTUAL", xy.uniqueid="PLOT_ID", 
		xvar="LON_ACTUAL", yvar="LAT_ACTUAL", xy.crs=4269,
		strat_layer=NULL, strat_lut=NULL, esttype="AREA", landarea="FOREST", 
		rowvar=NULL, colvar=NULL, estvar=NULL, estvar.filter=NULL, 
		savedata=FALSE, outfolder=NULL) {
  ## DESCRIPTION: estimates for each evalid in list
  
  ## Set global variables
  tree <- NULL

  ## Check esttype 
  ########################################################
  esttypelst <- c("AREA", "TREE", "RATIO")
  esttype <- FIESTA::pcheck.varchar(var2check=esttype, varnm="esttype", 
		checklst=esttypelst, caption="Estimation type", stopifnull=TRUE)

  FIAnamelst <- sort(unique(FIESTA::ref_codes[["VARIABLE"]]))
  rowvar <- FIESTA::pcheck.varchar(var2check=rowvar, varnm="rowvar", 
		checklst=FIAnamelst, caption="Estimation type")
  colvar <- FIESTA::pcheck.varchar(var2check=colvar, varnm="colvar", 
		checklst=FIAnamelst, caption="Estimation type")


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
  

  ## Get data from FIA Datamart and store in temporary directory
  #########################################################################
  if (is.null(data_dsn)) {
    if (!savedata) { 
      data_dsn <- file.path(tempdir(), "pltdat.sqlite")
    } else {
      outfolder <- pcheck.outfolder(outfolder)
      data_dsn <- file.path(outfolder, "pltdat.sqlite")
    }

    istree <- ifelse(esttype %in% c("TREE", "RATIO"), TRUE, FALSE) 
    pltdat <- DBgetPlots(evalid=evalidlst, istree=istree, savedata=TRUE, 
	out_fmt="sqlite", outfolder=outfolder, out_dsn="pltdat")
    DBI::dbListTables(DBI::dbConnect(RSQLite::SQLite(), data_dsn))
  }

  ## Check evalidlst
  #########################################################################
  evalidchk <- DBgetEvalid(evalid=unlist(evalidlst))


  ## Check xy data
  #########################################################################
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


  estlst <- list()
  estrawlst <- list()

  for (evalyr in names(evalidlst)) {
    message("getting estimates from ", toString(evalyr))

    xy.qry <- paste("select p.CN,", xyvars, "from", xy_layer, " xy",
	"join plot p on(p.PLOT_ID = xy.PLOT_ID)
	join pop_plot_stratum_assgn ppsa on(ppsa.PLT_CN = p.CN)  
	where ppsa.evalid in(", toString(evalidlst[[evalyr]]), ")")
    xy_conn <- DBI::dbConnect(RSQLite::SQLite(), xy_dsn)
    xy <- DBI::dbGetQuery(xy_conn, xy.qry)
    DBI::dbDisconnect(xy_conn)

    if (!is.null(strat_layer)) {
      strata <- TRUE

      ## Get strata information for boundary
      stratdat <- FIESTA::spGetStrata(xyplt=xy, uniqueid="CN", 
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
      unitdat <- FIESTA::spGetEstUnit(xyplt=xy, uniqueid="CN", 
		xvar=xvar, yvar=yvar, 
          	unit_layer=bnd_layer, unit_dsn=bnd_dsn, 
		unitvar=bnd.att, unit.filter=bnd.filter)
      pltassgn <- stratdat$pltassgn
      pltassgnid <- stratdat$pltassgnid
      unitarea <- stratdat$unitarea
      unitvar <- stratdat$unitvar
      areavar <- stratdat$areavar
    }

    xy_conn <- DBI::dbConnect(RSQLite::SQLite(), xy_dsn)
    DBI::dbWriteTable(xy_conn, "ppsatmp", pltassgn, overwrite=TRUE)
    DBI::dbDisconnect(xy_conn)


    ## Get population data for boundary
    #########################################################################
    GBpopdat <- modGBpop(cond="cond", plt="plot", tree="tree", 
		pltassgn="ppsatmp", pltassgnid=pltassgnid, 
		dsn=data_dsn, pjoinid="CN", strata=strata, unitvar=unitvar, 
		unitarea=unitarea, areavar=areavar, stratalut=stratalut, strvar=strvar, 
		getwt=FALSE, stratcombine=TRUE, saveobj=FALSE, savedata=savedata, 
		outfolder=NULL, outfn.pre=evalyr, outfn.date=FALSE, overwrite=TRUE)


    ## Generate estimates
    #########################################################################
    if (esttype == "AREA") {
      est <- modGBarea(GBpopdat=GBpopdat, landarea=landarea, sumunits=TRUE,
		rowvar=rowvar, row.FIAname=TRUE, 
		rawdata=TRUE, returntitle=TRUE, savedata=savedata)
    } else if (esttype == "TREE") {
      est <- modGBtree(GBpopdat=GBpopdat, landarea=landarea, sumunits=TRUE,
		estvar=estvar, estvar.filter=estvar.filter,
		rowvar=rowvar, row.FIAname=TRUE, 
		rawdata=TRUE, returntitle=TRUE, savedata=savedata)
    } else if (esttype == "RATIO") {
      est <- modGBratio(GBpopdat=GBpopdat, landarea=landarea, sumunits=TRUE,
		estvarn=estvar, estvarn.filter=estvar.filter,
		rowvar=rowvar, row.FIAname=TRUE, 
		rawdata=TRUE, returntitle=TRUE, savedata=savedata)
    }  
 
    estlst[[evalyr]] <- est$est
    estrawlst[[evalyr]] <- est$raw$unit.rowest
  }

  return(list(estlst=estlst, estrawlst=estrawlst))
}
