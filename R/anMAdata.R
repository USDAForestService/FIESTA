anMAdata <- function(bnd_layer, bnd_dsn=NULL, bnd.att=NULL, bnd.filter=NULL, 
	RS=NULL, clipxy=TRUE, datsource="sqlite", data_dsn=NULL, istree=TRUE, 
	isseed=FALSE, plot_layer="plot", cond_layer="cond", tree_layer="tree", 
	seed_layer="seed", puniqueid="CN", intensity1=TRUE, rastfolder=NULL, 
	rastlst.cont=NULL, rastlst.cont.name=NULL, rastlst.cat=NULL, rastlst.cat.name=NULL, 
	rastlst.cat.NODATA=NULL, showsteps=FALSE, cex.plots=0.5, savedata=FALSE, 
	savexy=FALSE, savesteps=FALSE, saveobj=FALSE, outfolder=NULL, out_fmt="csv", 
	out_dsn = NULL, outfn.pre=NULL, outfn.date=FALSE, overwrite_dsn=FALSE,
	overwrite_layer=TRUE, MApltdat=NULL, ...) {

  ## Set global variables
  gui <- FALSE
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
    outlst <- pcheck.output(out_dsn=out_dsn, out_fmt=out_fmt, 
		outfolder=outfolder, outfn.pre=outfn.pre, outfn.date=outfn.date, 
		overwrite_dsn=overwrite_dsn, gui=gui)
    out_dsn <- outlst$out_dsn
    outfolder <- outlst$outfolder
    out_fmt <- outlst$out_fmt
  }


  ####################################################################
  ## Get FIA plot data from SQLite within boundary
  ####################################################################
  if (is.null(MApltdat)) {
    MApltdat <- spGetPlots(bnd_layer, bnd_dsn=bnd_dsn, bnd.filter=bnd.filter, 
		RS=RS, clipxy=clipxy, datsource=datsource, data_dsn=data_dsn, 
		istree=istree, isseed=TRUE, plot_layer=plot_layer, cond_layer=cond_layer, 
		seed_layer=seed_layer, tree_layer=tree_layer, intensity1=intensity1, 
		savedata=FALSE, savexy=TRUE, ...)
    if (is.null(MApltdat)) return(NULL)
    if (saveobj) {
      message("saving MApltdat object to: ", file.path(outfolder, "MApltdat.rda"), "...")
      save(MApltdat, file=file.path(outfolder, "MApltdat.rda"))
    }
  } else {
    MApltdat.names <- c("xypltx", "bndx", "xy.uniqueid", "puniqueid",
		"pjoinid", "tabs")
    if (!all(MApltdat.names %in% names(MApltdat))) 
      stop("missing components in MApltdat list: ", 
		toString(MApltdat.names[!MApltdat.names %in% names(MApltdat)])) 
  }

  ## Extract list objects
  xyplt <- MApltdat$xypltx
  xy.uniqueid <- MApltdat$xy.uniqueid
  bnd <- MApltdat$bndx
  puniqueid <- MApltdat$puniqueid
  pjoinid <- MApltdat$pjoinid
  plt <- MApltdat$tabs$pltx
  cond <- MApltdat$tabs$condx
  tree <- MApltdat$tabs$treex

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

  ####################################################################
  ## Get model data
  ####################################################################
  message("summarizing auxiliary model data...")
  MAmodeldat <- spGetAuxiliary(xyplt=xyplt, uniqueid=xy.uniqueid, 
		dom_layer=bnd, domvar=bnd.att, rastfolder=rastfolder, 
	  	rastlst.cont=rastlst.cont, rastlst.cont.name=rastlst.cont.name, 
		rastlst.cat=rastlst.cat, rastlst.cat.name=rastlst.cat.name, 
		rastlst.cat.NODATA=NULL, keepNA=FALSE, npixels=TRUE)
  pltassgn <- MAmodeldat$pltassgn
  dunitlut <- MAmodeldat$dunitlut
  dunitvar <- MAmodeldat$dunitvar
  prednames <- MAmodeldat$prednames
  zonalnames <- MAmodeldat$zonalnames
  predfac <- MAmodeldat$predfac
  dunitarea <- MAmodeldat$dunitarea
  areavar <- MAmodeldat$areavar
  pltassgnid <- MAmodeldat$pltassgnid
  npixelvar <- MAmodeldat$npixelvar

  ##########################################
  ## Create output list
  ##########################################
  MAdata <- list(bnd=bnd, plt=plt, pltassgn=pltassgn, cond=cond, tree=tree, 
			dunitarea=dunitarea, dunitvar=dunitvar, areavar=areavar, 
			dunitlut=dunitlut, prednames=prednames, predfac=predfac,
			zonalnames=zonalnames, puniqueid=puniqueid, pjoinid=pjoinid, 
			pltassgnid=pltassgnid, npixelvar=npixelvar)
  if (savexy) {
    MAdata$xyplt <- xyplt
    MAdata$xy.uniqueid <- xy.uniqueid
  }

  if (saveobj) {
    objfn <- getoutfn(outfn="MApopdat.rda", outfolder=outfolder, 
		overwrite=overwrite_layer, outfn.date=TRUE)
    save(MAdata, file=objfn)
    message("saving object to: ", objfn)
  } 

  if (savedata) {
   if (!is.null(RS))
     datExportData(sf::st_drop_geometry(bnd), outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="bnd", 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer)

    if (savexy)
      datExportData(sf::st_drop_geometry(xyplt), outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="xyplt", 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer)

    datExportData(pltassgn, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="pltassgn", 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer)
    datExportData(plt, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="plt", 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer)
    datExportData(cond, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="cond", 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer)
    datExportData(tree, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="tree", 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer)
    datExportData(dunitarea, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="dunitarea", 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer)
    datExportData(dunitlut, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="dunitlut", 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer)
  }
   	
  return(MAdata)
}
