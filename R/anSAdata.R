anSAdata <- function(SAdoms, smallbnd=NULL, RS=NULL, xy=NULL, xy_dsn=NULL, 
	xy.joinid="PLOT_ID", clipxy=TRUE, datsource="sqlite", data_dsn=NULL, 
	istree=TRUE, isseed=FALSE, plot_layer="plot", cond_layer="cond", 
	tree_layer="tree", seed_layer="seed", puniqueid="CN", intensity1=FALSE, 
	rastfolder=NULL, rastlst.cont=NULL, rastlst.cont.name=NULL, 
	rastlst.cat=NULL, rastlst.cat.name=NULL, rastlst.cat.NODATA=NULL, 
	vars2keep="AOI", showsteps=FALSE, savedata=FALSE, savexy=FALSE, 
	savesteps=FALSE, saveobj=FALSE, outfolder=NULL, out_fmt="csv", 
	out_dsn = NULL, outfn.pre=NULL, outfn.date=FALSE, overwrite_dsn=FALSE, 
	overwrite_layer=TRUE, append_layer=FALSE, SApltdat=NULL, ...) {

  ## Set global variables
  gui <- FALSE
  plt=AOI <- NULL

## TESTING
#clipxy=TRUE
#plot_layer="plot"
#cond_layer="cond"
#tree_layer="tree"
#puniqueid="CN"
#measCur=TRUE
#bnd=SAdoms
#bnd.filter=measEndyr.filter=NULL
#bnd_dsn=NULL
#stbnd.att="COUNTYFIPS"
#states=NULL
#RS=NULL
#stbnd=NULL
#stbnd_dsn=NULL
#savebnd=FALSE
#xy=NULL
#istree=FALSE
#other_layers=NULL
#allyrs=FALSE
#measEndyr=NULL
#evalid=NULL
#evalEndyr=NULL
#evalCur=FALSE
#intensity1=FALSE
#isseed=FALSE
#savePOP=FALSE
#xy.joinid="PLOT_ID"
#xy.uniqueid="PLT_CN"
#xy_dsn=NULL


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

  ## Check overwrite, outfn.date, outfolder, outfn 
  ########################################################
  if (savedata) {
    outlst <- pcheck.output(out_dsn=out_dsn, out_fmt=out_fmt, 
		outfolder=outfolder, outfn.pre=outfn.pre, outfn.date=outfn.date, 
		overwrite_dsn=overwrite_dsn, overwrite_layer=overwrite_layer, 
		append_layer=append_layer, createSQLite=FALSE, gui=gui)
    out_dsn <- outlst$out_dsn
    outfolder <- outlst$outfolder
    out_fmt <- outlst$out_fmt
    overwrite_layer <- outlst$overwrite_layer
    overwrite_dsn <- outlst$overwrite_dsn
    append_layer <- outlst$append_layer

  } else if (savesteps || saveobj) {
    outfolder <- pcheck.outfolder(outfolder)
  }

  if (savesteps) {
    stepfolder <- file.path(outfolder, "SAdoms_steps")
    if (!dir.exists(stepfolder)) dir.create(stepfolder)
    step_dsn <- NULL
    step_fmt <- "shp"
  }

  ####################################################################
  ## Get FIA plot data from SQLite within boundary
  ####################################################################
  if (is.null(SApltdat)) {
    SApltdat <- spGetPlots(bnd=SAdoms, RS=RS, xy=xy, xy_dsn=xy_dsn, 
		xy.joinid=xy.joinid, clipxy=clipxy, datsource=datsource, 
		data_dsn=data_dsn, istree=istree, plot_layer=plot_layer, 
		cond_layer=cond_layer, tree_layer=tree_layer, seed_layer=seed_layer, 
 		intensity1=intensity1, savedata=FALSE, savexy=savexy, ...)
    if (is.null(SApltdat)) return(NULL)
    if (saveobj) {
      message("saving SApltdat object to: ", file.path(outfolder, "SApltdat.rda"), "...")
      save(SApltdat, file=file.path(outfolder, "SApltdat.rda"))
    }
  } else {
    SApltdat.names <- c("xypltx", "bndx", "xy.uniqueid", "puniqueid",
		"pjoinid", "tabs")
    if (!all(SApltdat.names %in% names(SApltdat))) {
      stop("missing components in SApltdat list: ", 
		toString(SApltdat.names[!SApltdat.names %in% names(SApltdat)])) 
    }
  }
 
  ## Extract list objects
  xyplt <- SApltdat$xypltx
  xy.uniqueid <- SApltdat$xy.uniqueid
  puniqueid <- SApltdat$puniqueid
  pjoinid <- SApltdat$pjoinid
  pltx <- SApltdat$tabs$pltx
  condx <- SApltdat$tabs$condx
  treex <- SApltdat$tabs$treex
  seedx <- SApltdat$tabs$seedx
  SAdoms <- SApltdat$bndx

  ## Check SAdoms
  #if (!all(c("DOMAIN", "AOI") %in% names(SAdoms))) {
  #  stop("invalid SAdoms...  need to include DOMAIN and AOI attributes")
  #}

  if (showsteps) {
    ## Set plotting margins
    mar <-  par("mar")
    par(mar=c(1,1,1,1))

    plot(sf::st_geometry(SAdoms), border="grey")
    #plot(sf::st_geometry(bnd), add=TRUE)
    plot(sf::st_geometry(xyplt), add=TRUE, col="blue", cex=.25)
    if (!is.null(smallbnd) && "sf" %in% class(smallbnd)) { 
      plot(sf::st_geometry(smallbnd), add=TRUE, border="red")
    }
    par(mar=mar)
  }

  if (savesteps) {
    ## Set plotting margins
    mar <-  par("mar")
    par(mar=c(1,1,1,1))

    out_layer <- paste0("SAdoms_plots")
    jpgfn <- paste0(stepfolder, "/", out_layer, ".jpg")
    jpeg(jpgfn, res=400, units="in", width=8, height=10)
    plot(sf::st_geometry(SAdoms), border="grey")
    #plot(sf::st_geometry(bnd), add=TRUE)
    plot(sf::st_geometry(xyplt), add=TRUE, col="blue", cex=.25)
    if (!is.null(smallbnd)) {
      plot(sf::st_geometry(smallbnd), add=TRUE, border="red")
    }
    dev.off()
    message("Writing jpg to ", jpgfn, "\n")
    par(mar=mar)
  }
 
  ## Check number of plots (Note: must be greater than 2 plots)
  extpoly <- spExtractPoly(xyplt=xyplt, polyvlst=SAdoms, 
		uniqueid=xy.uniqueid, polyvarlst=unique(c("DOMAIN", "AOI")), 
		keepNA=FALSE)
  test <- data.table(st_drop_geometry((extpoly$sppltext)))
  test <- test[AOI == 1, .N, by="DOMAIN"]

  message("checking number of plots in domain...")
  print(test)

  if (all(test$N <= 2)) {
    message("ALL AOIs have 2 plots or less... no estimates generated")
    return(NULL)
  }

  ####################################################################
  ## Get model data
  ####################################################################
  message("summarizing auxiliary model data...")
  SAmodeldat <- spGetAuxiliary(xyplt=xyplt, uniqueid=xy.uniqueid, 
		dom_layer=SAdoms, rastfolder=rastfolder,
	  	rastlst.cont=rastlst.cont, rastlst.cont.name=rastlst.cont.name, 
		rastlst.cat=rastlst.cat, rastlst.cat.name=rastlst.cat.name, 
		rastlst.cat.NODATA=NULL, keepNA=FALSE, npixels=TRUE, 
		vars2keep="AOI", savedata=FALSE)
  pltassgn <- SAmodeldat$pltassgn
  dunitlut <- SAmodeldat$dunitlut
  dunitvar <- SAmodeldat$dunitvar
  prednames <- SAmodeldat$prednames
  zonalnames <- SAmodeldat$zonalnames
  predfac <- SAmodeldat$predfac
  dunitarea <- SAmodeldat$dunitarea
  areavar <- SAmodeldat$areavar
  pltassgnid <- SAmodeldat$pltassgnid

  ##########################################
  ## Create output list
  ##########################################
  SAdata <- list(SAdoms=SAdoms, smallbnd=smallbnd, plt=pltx, 
	pltassgn=pltassgn, cond=condx, 
	dunitarea=dunitarea, dunitvar=dunitvar, areavar=areavar, 
	dunitlut=dunitlut, prednames=prednames, predfac=predfac,
	zonalnames=zonalnames, puniqueid=puniqueid, pjoinid=pjoinid, 
	pltassgnid=pltassgnid)
  if (istree) {
    SAdata$tree <- treex
  }
  if (isseed) {
    SAdata$seed <- seedx
  }

  if (savexy) {
    SAdata$xyplt <- xyplt
    SAdata$xy.uniqueid <- xy.uniqueid
  }

  if (saveobj) {
    objfn <- getoutfn(outfn="SApopdat.rda", outfolder=outfolder, 
		overwrite=overwrite_layer, outfn.date=TRUE)
    save(SAdata, file=objfn)
    message("saving object to: ", objfn)
  } 

  if (savedata) {
    if (!is.null(RS)) {
      datExportData(sf::st_drop_geometry(SAdoms), outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="SAdoms", 
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
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="plt", 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer)
    datExportData(condx, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="cond", 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer)
    datExportData(treex, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="tree", 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer)
    datExportData(dunitarea, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="dunitarea", 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer)
    datExportData(dunitlut, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="dunitlut", 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer)
  }
   	
  return(SAdata)
}
