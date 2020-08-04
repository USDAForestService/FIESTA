anMAest_data <- function(bnd, bnd_dsn=NULL, bnd.att, bnd.filter=NULL, 
	states=NULL, xy=NULL, xy_dsn=NULL, xy.uniqueid="PLT_CN", 
	xvar="LON_PUBLIC", yvar="LAT_PUBLIC", xy.crs=4269, clipxy=TRUE, 
	datsource="sqlite", SQLitefn=NULL, istree=TRUE, plot_layer="plot", 
	cond_layer="cond", tree_layer="tree", puniqueid="CN", intensity1=FALSE,
	rastlst.cont=NULL, rastlst.cont.name=NULL, rastlst.cat=NULL, 
	rastlst.cat.name=NULL, rastlst.cat.NODATA=NULL, 
	showsteps=FALSE, savedata=FALSE, savexy=FALSE, savesteps=FALSE, 
	outfolder=NULL, out_fmt="csv", out_dsn=NULL, outfn.pre=NULL, 
	outfn.date=FALSE, overwrite=TRUE) {


  ## Set global variables
  gui <- FALSE
  ref_titles <- FIESTA::ref_titles
  plt <- NULL

  ## Check savedata 
  savedata <- FIESTA::pcheck.logical(savedata, varnm="savedata", 
		title="Save data extraction?", first="NO", gui=gui)  

  ## Check overwrite, outfolder, outfn 
  ########################################################
  if (savedata || savexy || savesteps) {
    outfolder <- FIESTA::pcheck.outfolder(outfolder, gui)
    overwrite <- FIESTA::pcheck.logical(overwrite, varnm="overwrite", 
		title="Overwrite?", first="NO", gui=gui)  
    outfn.date <- FIESTA::pcheck.logical(outfn.date , varnm="outfn.date", 
		title="Add date to outfiles?", first="NO", gui=gui) 

    out_fmtlst <- c("sqlite", "gpkg", "csv", "gdb")
    out_fmt <- FIESTA::pcheck.varchar(var2check=out_fmt, varnm="out_fmt", 
		checklst=out_fmtlst, gui=gui, caption="Output format?") 

    out_fmtlst <- c("sqlite", "gpkg", "csv", "gdb")
    out_fmt <- FIESTA::pcheck.varchar(var2check=out_fmt, varnm="out_fmt", 
		checklst=out_fmtlst, gui=gui, caption="Output format?") 
    if (out_fmt != "shp" && is.null(out_dsn))
      out_dsn <- paste0("SAdata.", out_fmt)

    if (out_fmt == "gdb") {
      gdbfn <- DBtestESRIgdb(gdbfn=out_dsn, outfolder=outfolder, overwrite=FALSE, 
			showlist=FALSE, returnpath=FALSE)
    }	else if (out_fmt %in% c("sqlite", "gpkg")) {
      gpkg <- ifelse(out_fmt == "gpkg", TRUE, FALSE)
      SQLitefn <- DBcreateSQLite(SQLitefn=out_dsn, gpkg=gpkg, outfolder=outfolder, 
			overwrite=FALSE, returnpath=FALSE)
    }	

    if (savesteps) {
      stepfolder <- file.path(outfolder, "steps")
      if (!dir.exists(stepfolder)) dir.create(stepfolder)
      if (out_fmt == "shp") {
        step_dsn <- NULL
      } else {
        step_dsn <- paste0("steps.", out_fmt)
      }
    }
  }

  ####################################################################
  ## Get FIA plot data from SQLite within boundary
  ####################################################################
  MApltdat <- spGetPlots(bnd=bnd, bnd_dsn=bnd_dsn, bnd.filter=bnd.filter, 
		xy=xy, xy_dsn=xy_dsn, xy.uniqueid=xy.uniqueid, xvar=xvar, 
		yvar=yvar, xy.crs=xy.crs, clipxy=TRUE, datsource=datsource, 
		data_dsn=SQLitefn, istree=istree, plot_layer=plot_layer, 
		cond_layer=cond_layer, tree_layer=tree_layer, 
		intensity1=FALSE, savedata=FALSE)
  xyplt <- MApltdat$clip_xyplt
  xy.uniqueid <- MApltdat$xy.uniqueid
  bnd <- MApltdat$clip_poly
  puniqueid <- MApltdat$puniqueid
  plt <- MApltdat$clip_tabs$clip_plt
  cond <- MApltdat$clip_tabs$clip_cond
  tree <- MApltdat$clip_tabs$clip_tree

  if (showsteps) {
    plot(sf::st_geometry(bnd))
    plot(sf::st_geometry(xyplt), add=TRUE, col="blue", cex=.25)
  }

  if (savesteps) {
    out_layer <- paste0("MAbnd_plots")
    jpgfn <- paste0(stepfolder, "/", out_layer, ".jpg")
    jpeg(jpgfn, res=400, units="in", width=8, height=10)
      plot(sf::st_geometry(bnd))
      plot(sf::st_geometry(xyplt), add=TRUE, col="blue", cex=.25)
    dev.off()
    message("Writing jpg to ", jpgfn, "\n")
  }

  ####################################################################
  ## Get model data
  ####################################################################
  message("summarizing auxiliary model data...")
  MAmodeldat <- spGetModeldat(xyplt=xyplt, uniqueid=xy.uniqueid, 
		module="MA", dom_layer=bnd, domvar=bnd.att,
	  	rastlst.cont=rastlst.cont, rastlst.cont.name=rastlst.cont.name, 
		rastlst.cat=rastlst.cat, rastlst.cat.name=rastlst.cat.name, 
		rastlst.cat.NODATA=NULL, savedata=FALSE)

  pltassgn <- MAmodeldat$pltassgn
  unitlut <- MAmodeldat$domzonal
  unitvar <- MAmodeldat$domvar
  prednames <- MAmodeldat$prednames
  zonalnames <- MAmodeldat$zonalnames
  predfac <- MAmodeldat$predfac
  unitarea <- MAmodeldat$domarea
  areavar <- MAmodeldat$areavar
  pltassgnid <- MAmodeldat$pltassgnid


  ##########################################
  ## Create output list
  ##########################################
  MAdata <- list(bnd=bnd, plt=plt, pltassgn=pltassgn, cond=cond, tree=tree, 
			unitarea=unitarea, unitvar=unitvar, areavar=areavar, 
			unitlut=unitlut, prednames=prednames, predfac=predfac,
			zonalnames=zonalnames, puniqueid=puniqueid, pltassgnid=pltassgnid)

  if (savexy) {
    MAdata$xyplt <- xyplt
    MAdata$xy.uniqueid <- xy.uniqueid
  }

  if (savedata) {
    objfn <- file.path(outfolder, "MAdata.rda")
    save(MAdata, file=objfn)
    message("saving object to: ", objfn)

    if (savexy)
      datExportData(sf::st_drop_geometry(xyplt), outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="xyplt", 
		outfn.date=outfn.date, overwrite_layer=overwrite)

    datExportData(plt, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="plt", 
		outfn.date=outfn.date, overwrite_layer=overwrite)
    datExportData(pltassgn, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="pltassgn", 
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
