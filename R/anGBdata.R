anGBdata <- function(bnd_layer, bnd_dsn=NULL, bnd.att=NULL, bnd.filter=NULL, 
	RS=NULL, xy=NULL, xy_dsn=NULL, clipxy=TRUE, datsource="sqlite", 
	data_dsn=NULL, istree=TRUE, isseed=FALSE, 
	plot_layer="plot", cond_layer="cond", tree_layer="tree", 
	seed_layer="seed", puniqueid="CN", intensity1=TRUE, strata=TRUE, 
	strattype="RASTER", strat_layer=NULL, strat_dsn=NULL, strvar=NULL, 
	showsteps=FALSE, cex.plots=0.5, savedata=FALSE, savexy=TRUE, 
	savesteps=FALSE, saveobj=FALSE, outfolder=NULL, out_fmt="csv", 
	out_dsn=NULL, outfn.pre=NULL, outfn.date=FALSE, overwrite_dsn=FALSE, 
	overwrite_layer=TRUE, GBpltdat=NULL, ...) {


  ## Set global variables
  gui <- FALSE
  plt=strvar=stratalut=strwtvar <- NULL

  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- c(names(formals(anGBdata)), 
		names(formals(FIESTA::spGetPlots)))
  if (!all(input.params %in% formallst)) {
    miss <- input.params[!input.params %in% formallst]
    stop("invalid parameter: ", toString(miss))
  }


  ## Check savedata 
  savedata <- FIESTA::pcheck.logical(savedata, varnm="savedata", 
		title="Save data extraction?", first="NO", gui=gui) 

  ## Check savexy
  savexy <- FIESTA::pcheck.logical(savexy, varnm="savexy", 
		title="Save xy data?", first="NO", gui=gui)  

  ## Check savesteps
  savesteps <- FIESTA::pcheck.logical(savesteps, varnm="savesteps", 
		title="Save step data?", first="YES", gui=gui)  
 
  ## Check saveobj 
  saveobj <- FIESTA::pcheck.logical(saveobj, varnm="saveobj", 
		title="Save SApopdat object?", first="YES", gui=gui, stopifnull=TRUE)


  ## Check overwrite, outfolder, outfn 
  ########################################################
  if (savedata) {
    outlst <- pcheck.output(out_dsn=out_dsn, out_fmt=out_fmt, 
		outfolder=outfolder, outfn.pre=outfn.pre, outfn.date=outfn.date, 
		overwrite_dsn=overwrite_dsn, append_layer=append_layer, 
		createSQLite=FALSE, gui=gui)
    out_dsn <- outlst$out_dsn
    outfolder <- outlst$outfolder
    out_fmt <- outlst$out_fmt
    overwrite_layer <- outlst$overwrite_layer
    overwrite_dsn <- outlst$overwrite_dsn
    append_layer <- outlst$append_layer

  } else if (savesteps || saveobj) {
    outfolder <- pcheck.outfolder(outfolder)
  }
  

  ####################################################################
  ## Get FIA plot data from SQLite within boundary
  ####################################################################
  if (is.null(GBpltdat)) {
    GBpltdat <- spGetPlots(bnd=bnd_layer, bnd_dsn=bnd_dsn, bnd.filter=bnd.filter, 
		RS=RS, xy=xy, xy_dsn=xy_dsn, intensity1=intensity1, datsource=datsource, 
		data_dsn=data_dsn, istree=istree, isseed=isseed, plot_layer=plot_layer,
 		cond_layer=cond_layer, tree_layer=tree_layer, seed_layer=seed_layer, 
		savedata=FALSE, savexy=savexy, ...)
    if (is.null(GBpltdat)) return(NULL)
    if (saveobj) {
      message("saving GBpltdat object to: ", 
			file.path(outfolder, "GBpltdat.rda"), "...")
      save(GBpltdat, file=file.path(outfolder, "GBpltdat.rda"))
    }
  } else {
    GBpltdat.names <- c("xypltx", "bndx", "xy.uniqueid", "puniqueid",
		"pjoinid", "tabs")
    if (!all(GBpltdat.names %in% names(GBpltdat))) {
      stop("missing components in GBpltdat list: ", 
		toString(GBpltdat.names[!GBpltdat.names %in% names(GBpltdat)])) 
    }
  }

  ## Extract list objects
  xyplt <- GBpltdat$spxy
  xy.uniqueid <- GBpltdat$xy.uniqueid
  bnd <- GBpltdat$bndx
  puniqueid <- GBpltdat$puniqueid
  pjoinid <- GBpltdat$pjoinid
  pltx <- GBpltdat$tabs$pltx
  condx <- GBpltdat$tabs$condx
  treex <- GBpltdat$tabs$treex
  seedx <- GBpltdat$tabs$seedx

  if (showsteps) {
    ## Set plotting margins
    mar <-  par("mar")
    par(mar=c(1,1,1,1))

    plot(sf::st_geometry(bnd), border="dark grey")
    plot(sf::st_geometry(xyplt), add=TRUE, col="blue", cex=cex.plots)

    par(mar=mar)
  }

  if (savesteps) {
    ## Set plotting margins
    mar <-  par("mar")

    out_layer <- paste0("GAbnd_plots")
    jpgfn <- paste0(outfolder, "/", out_layer, ".jpg")
    jpeg(jpgfn, res=400, units="in", width=8, height=10)
      par(mar=c(1,1,1,1))

      plot(sf::st_geometry(bnd))
      plot(sf::st_geometry(xyplt), add=TRUE, col="blue", cex=.25)
    dev.off()
    message("Writing jpg to ", jpgfn, "\n")
    par(mar=mar)
  }

  ####################################################################
  ## Get strata data for bnd
  ####################################################################
  if (strata) {
    message("summarizing stratification data...")
    stratdat <- spGetStrata(xyplt, unittype="POLY", uniqueid=xy.uniqueid, 
		unit_layer=bnd, unitvar=bnd.att, strattype=strattype, 
		strat_layer=strat_layer, strat_dsn=strat_dsn, strvar=strvar, 
		rast.NODATA=0) 
    pltassgn <- setDT(stratdat$pltassgn)
    unitarea <- stratdat$unitarea
    unitvar <- stratdat$unitvar
    areavar <- stratdat$areavar
    stratalut <- stratdat$stratalut
    strvar <- stratdat$strvar
    strwtvar <- stratdat$strwtvar
    pltassgnid <- stratdat$pltassgnid

  } else {
    message("summarizing estimation unit data...")
    stratdat <- spGetEstUnit(xyplt, unittype="POLY", uniqueid=xy.uniqueid, 
		unit_layer=bnd, unitvar=bnd.att) 
    pltassgn <- stratdat$pltassgn
    unitarea <- stratdat$unitarea
    unitvar <- stratdat$unitvar
    areavar <- stratdat$areavar
    pltassgnid <- stratdat$pltassgnid
  } 

  ##########################################
  ## Create output list
  ##########################################
  GBdata <- list(bnd=bnd, plt=pltx, pltassgn=pltassgn, cond=condx,
			unitarea=unitarea, unitvar=unitvar, areavar=areavar, 
			stratalut=stratalut, strvar=strvar, strwtvar=strwtvar,
 			puniqueid=puniqueid, pjoinid=pjoinid, pltassgnid=pltassgnid)
  if (istree) {
    GBdata$tree <- treex
  }
  if (isseed) {
    GBdata$seed <- seedx
  }

  if (savexy) {
    GBdata$xyplt <- xyplt
    GBdata$xy.uniqueid <- xy.uniqueid
  }

  if (saveobj) {
    objfn <- getoutfn(outfn="GBpopdat.rda", outfolder=outfolder, 
		overwrite=overwrite_layer, outfn.date=TRUE)
    save(GBdata, file=objfn)
    message("saving object to: ", objfn)
  } 


  if (savedata) {
    if (!is.null(RS)) {
      datExportData(sf::st_drop_geometry(bnd), outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="bnd", 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer)
    }
    if (savexy) {
      datExportData(sf::st_drop_geometry(xyplt), outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="xyplt", 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer)
    }
    datExportData(pltassgn, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="pltassgn", 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer)
    datExportData(pltx, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="pltx", 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer)
    datExportData(condx, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="condx", 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer)
    if (istree) {
      datExportData(treex, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="treex", 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer)
    }
    if (isseed) {
      datExportData(seedx, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="seedx", 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer)
    }
    datExportData(unitarea, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="unitarea", 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer)  
    if (strata) {
      datExportData(stratalut, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="stratalut", 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer)
    }
  }

  return(GBdata)
}

