anMAdata <- function(bnd_layer, bnd_dsn=NULL, bnd.att=NULL, bnd.filter=NULL, 
	RS=NULL, clipxy=TRUE, datsource="sqlite", data_dsn=NULL, istree=TRUE, 
	plot_layer="plot", cond_layer="cond", tree_layer="tree", puniqueid="CN", 
	intensity1=TRUE, rastfolder=NULL, rastlst.cont=NULL, 
	rastlst.cont.name=NULL, rastlst.cat=NULL, rastlst.cat.name=NULL, 
	rastlst.cat.NODATA=NULL, showsteps=FALSE, cex.plots=0.5, savedata=FALSE, 
	savexy=FALSE, savesteps=FALSE, saveobj=FALSE, outfolder=NULL, out_fmt="csv", 
	out_dsn = NULL, outfn.pre=NULL, outfn.date=FALSE, overwrite=TRUE, 
	MApltdat=NULL, ...) {

  ## Set global variables
  gui <- FALSE
  ref_titles <- FIESTA::ref_titles
  plt <- NULL

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

    if (savesteps) {
      stepfolder <- file.path(outfolder, "SAdoms_steps")
      if (!dir.exists(stepfolder)) dir.create(stepfolder)
      if (out_fmt == "shp") {
        step_dsn <- NULL
      } else {
        step_dsn <- paste0("SAdoms_steps.", out_fmt)
      }
    }
  }


  ####################################################################
  ## Get FIA plot data from SQLite within boundary
  ####################################################################
  if (is.null(MApltdat)) {
    MApltdat <- spGetPlots(bnd_layer, bnd_dsn=bnd_dsn, bnd.filter=bnd.filter, 
		RS=RS, clipxy=clipxy, datsource=datsource, data_dsn=data_dsn, 
		istree=istree, plot_layer=plot_layer, cond_layer=cond_layer, 
		tree_layer=tree_layer, intensity1=intensity1, savedata=FALSE, 
		savexy=TRUE, ...)
    if (is.null(MApltdat)) return(NULL)
    if (saveobj) {
      message("saving MApltdat object to: ", file.path(outfolder, "MApltdat.rda"), "...")
      save(MApltdat, file=file.path(outfolder, "MApltdat.rda"))
    }
  } else {
    MApltdat.names <- c("clip_xyplt", "clip_polyv", "xy.uniqueid", "puniqueid",
		"pjoinid", "clip_tabs")
    if (!all(MApltdat.names %in% names(MApltdat))) 
      stop("missing components in MApltdat list: ", 
		toString(MApltdat.names[!MApltdat.names %in% names(MApltdat)])) 
  }

  ## Extract list objects
  xyplt <- MApltdat$clip_xyplt
  xy.uniqueid <- MApltdat$xy.uniqueid
  bnd <- MApltdat$clip_poly
  puniqueid <- MApltdat$puniqueid
  pjoinid <- MApltdat$pjoinid
  plt <- MApltdat$clip_tabs$clip_plt
  cond <- MApltdat$clip_tabs$clip_cond
  tree <- MApltdat$clip_tabs$clip_tree

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

    out_layer <- paste0("MAbnd_plots")
    jpgfn <- paste0(outfolder, "/", out_layer, ".jpg")
    jpeg(jpgfn, res=400, units="in", width=8, height=10)
      par(mar=c(1,1,1,1))

      plot(sf::st_geometry(bnd))
      plot(sf::st_geometry(xyplt), add=TRUE, col="blue", cex=.25)
    dev.off()
    message("Writing jpg to ", jpgfn, "\n")
    par(mar=mar)
  }

print("TEST")
print(rastlst.cont)
print(rastlst.cont.name)

  ####################################################################
  ## Get model data
  ####################################################################
  message("summarizing auxiliary model data...")
  MAmodeldat <- spGetModeldat(xyplt=xyplt, uniqueid=xy.uniqueid, 
		dom_layer=bnd, domvar=bnd.att, rastfolder=rastfolder, 
	  	rastlst.cont=rastlst.cont, rastlst.cont.name=rastlst.cont.name, 
		rastlst.cat=rastlst.cat, rastlst.cat.name=rastlst.cat.name, 
		rastlst.cat.NODATA=NULL, keepNA=FALSE, npixels=TRUE)
  pltassgn <- MAmodeldat$pltassgn
  unitlut <- MAmodeldat$domzonal
  unitvar <- MAmodeldat$domvar
  prednames <- MAmodeldat$prednames
  zonalnames <- MAmodeldat$zonalnames
  predfac <- MAmodeldat$predfac
  unitarea <- MAmodeldat$domarea
  areavar <- MAmodeldat$areavar
  pltassgnid <- MAmodeldat$pltassgnid
  npixelvar <- MAmodeldat$npixelvar

  ##########################################
  ## Create output list
  ##########################################
  MAdata <- list(bnd=bnd, plt=plt, pltassgn=pltassgn, cond=cond, tree=tree, 
			unitarea=unitarea, unitvar=unitvar, areavar=areavar, 
			unitlut=unitlut, prednames=prednames, predfac=predfac,
			zonalnames=zonalnames, puniqueid=puniqueid, pjoinid=pjoinid, 
			pltassgnid=pltassgnid, npixelvar=npixelvar)
  if (savexy) {
    MAdata$xyplt <- xyplt
    MAdata$xy.uniqueid <- xy.uniqueid
  }

  if (saveobj) {
    objfn <- getoutfn(outfn="MApopdat.rda", outfolder=outfolder, 
		overwrite=overwrite, outfn.date=TRUE)
    save(MAdata, file=objfn)
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
    datExportData(unitlut, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="unitlut", 
		outfn.date=outfn.date, overwrite_layer=overwrite)
  }
   	
  return(MAdata)
}
