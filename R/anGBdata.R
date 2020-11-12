anGBdata <- function(bnd_layer, bnd_dsn=NULL, bnd.att=NULL, bnd.filter=NULL, 
	RS=NULL, clipxy=TRUE, datsource="sqlite", data_dsn=NULL, istree=TRUE, 
	plot_layer="plot", cond_layer="cond", tree_layer="tree", puniqueid="CN", 
	intensity1=TRUE, strata=TRUE, strattype="RASTER", strat_layer=NULL, 
	strat_dsn=NULL, strvar=NULL, showsteps=FALSE, cex.plots=0.5, 
	savedata=FALSE, savexy=TRUE, savesteps=FALSE, saveobj=FALSE, outfolder=NULL, 
	out_fmt="csv", out_dsn=NULL, outfn.pre=NULL, outfn.date=FALSE, 
	overwrite=TRUE, GBpltdat=NULL, ...) {


  ## Set global variables
  gui <- FALSE
  ref_titles <- FIESTA::ref_titles
  plt=strvar=stratalut <- NULL

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
  if (savedata || savexy || savesteps || saveobj) {
    outfolder <- FIESTA::pcheck.outfolder(outfolder, gui)
    overwrite <- FIESTA::pcheck.logical(overwrite, varnm="overwrite", 
		title="Overwrite?", first="NO", gui=gui)  
    outfn.date <- FIESTA::pcheck.logical(outfn.date , varnm="outfn.date", 
		title="Add date to outfiles?", first="NO", gui=gui) 

    ## If outfn.pre is not null, create a folder within the outfolder, named outfn.pre
    if (!is.null(outfn.pre)) {
      outfolder <- file.path(outfolder, outfn.pre)
      if (!dir.exists(outfolder)) dir.create(outfolder)
    }

    out_fmtlst <- c("sqlite", "gpkg", "csv", "gdb", "shp")
    out_fmt <- FIESTA::pcheck.varchar(var2check=out_fmt, varnm="out_fmt", 
		checklst=out_fmtlst, gui=gui, caption="Output format?") 
    if (out_fmt == "shp") out_fmt <- "csv"
    if (out_fmt != "csv" && is.null(out_dsn))
      out_dsn <- paste0("SAdata.", out_fmt)

    if (out_fmt == "gdb") {
      out_dsn <- DBtestESRIgdb(gdbfn=out_dsn, outfolder=outfolder, overwrite=overwrite, 
			showlist=FALSE, returnpath=FALSE)
    }	else if (out_fmt %in% c("sqlite", "gpkg")) {
      gpkg <- ifelse(out_fmt == "gpkg", TRUE, FALSE)
      out_dsn <- DBcreateSQLite(SQLitefn=out_dsn, gpkg=gpkg, outfolder=outfolder, 
			overwrite=overwrite, returnpath=FALSE)
    }	
  }


  ####################################################################
  ## Get FIA plot data from SQLite within boundary
  ####################################################################
  if (is.null(GBpltdat)) {
    GBpltdat <- spGetPlots(bnd_layer, bnd_dsn=bnd_dsn, bnd.filter=bnd.filter, 
		RS=RS, clipxy=clipxy, datsource=datsource, data_dsn=data_dsn, 
		istree=istree, plot_layer=plot_layer, cond_layer=cond_layer, 
		tree_layer=tree_layer, intensity1=intensity1, savedata=FALSE, 
		savexy=TRUE, ...)
    if (is.null(GBpltdat)) return(NULL)
    if (saveobj) {
      message("saving GBpltdat object to: ", file.path(outfolder, "GBpltdat.rda"), "...")
      save(GBpltdat, file=file.path(outfolder, "GBpltdat.rda"))
    }
  } else {
    GBpltdat.names <- c("clip_xyplt", "clip_polyv", "xy.uniqueid", "puniqueid",
		"pjoinid", "clip_tabs")
    if (!all(GBpltdat.names %in% names(GBpltdat))) 
      stop("missing components in GBpltdat list: ", 
		toString(GBpltdat.names[!GBpltdat.names %in% names(GBpltdat)])) 
  }

  ## Extract list objects
  xyplt <- GBpltdat$clip_xyplt
  xy.uniqueid <- GBpltdat$xy.uniqueid
  bnd <- GBpltdat$clip_poly
  puniqueid <- GBpltdat$puniqueid
  pjoinid <- GBpltdat$pjoinid
  plt <- GBpltdat$clip_tabs$clip_plt
  cond <- GBpltdat$clip_tabs$clip_cond
  tree <- GBpltdat$clip_tabs$clip_tree

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
  GBdata <- list(bnd=bnd, plt=plt, pltassgn=pltassgn, cond=cond, tree=tree, 
			unitarea=unitarea, unitvar=unitvar, areavar=areavar, 
			stratalut=stratalut, strvar=strvar, puniqueid=puniqueid,
			pjoinid=pjoinid, pltassgnid=pltassgnid)

  if (savexy) {
    GBdata$xyplt <- xyplt
    GBdata$xy.uniqueid <- xy.uniqueid
  }

  if (saveobj) {
    objfn <- getoutfn(outfn="GBpopdat.rda", outfolder=outfolder, 
		overwrite=overwrite, outfn.date=TRUE)
    save(GBdata, file=objfn)
    message("saving object to: ", objfn)
  } 


  if (savedata) {
   if (!is.null(RS))
     datExportData(sf::st_drop_geometry(bnd), outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="bnd", 
		outfn.date=outfn.date, overwrite_layer=overwrite)

    if (savexy)
      datExportData(sf::st_drop_geometry(xyplt), outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="xyplt", 
		outfn.date=outfn.date, overwrite_layer=overwrite)

    datExportData(pltassgn, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="pltassgn", 
		outfn.date=outfn.date, overwrite_layer=overwrite)
    datExportData(plt, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="plt", 
		outfn.date=outfn.date, overwrite_layer=overwrite)
    datExportData(cond, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="cond", 
		outfn.date=outfn.date, overwrite_layer=overwrite)
    datExportData(tree, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="tree", 
		outfn.date=outfn.date, overwrite_layer=overwrite)
    datExportData(unitarea, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="unitarea", 
		outfn.date=outfn.date, overwrite_layer=overwrite)  
    if (strata) {
      datExportData(stratalut, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="stratalut", 
		outfn.date=outfn.date, overwrite_layer=overwrite)
    }
  }

  return(GBdata)
}

