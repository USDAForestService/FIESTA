anSAdata <- function(SAdoms, smallbnd=NULL, RS=NULL, 
	xy_datsource=NULL, xy=NULL, xy_dsn=NULL, xyjoinid="PLOT_ID", 
	clipxy=TRUE, datsource="sqlite", data_dsn=NULL, istree=TRUE, 
	isseed=FALSE, plot_layer="plot", cond_layer="cond", tree_layer="tree", 
	seed_layer="seed", puniqueid="CN", intensity1=TRUE, rastfolder=NULL, 
	rastlst.cont=NULL, rastlst.cont.name=NULL, rastlst.cont.NODATA=NULL, 
	rastlst.cat=NULL, rastlst.cat.name=NULL, rastlst.cat.NODATA=NULL, 
	vars2keep="AOI", showsteps=FALSE, savedata=FALSE, savexy=FALSE, 
	savesteps=FALSE, saveobj=FALSE, outfolder=NULL, out_fmt="csv", 
	out_dsn = NULL, outfn.pre=NULL, outfn.date=FALSE, overwrite_dsn=FALSE, 
	overwrite_layer=TRUE, append_layer=FALSE, pltdat=NULL, ...) {

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
#xyjoinid="PLOT_ID"
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
  if (savedata || savexy) {
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
  if (is.null(pltdat)) {
    pltdat <- spGetPlots(bnd=SAdoms, RS=RS, xy_datsource=xy_datsource, 
		xy=xy, xy_dsn=xy_dsn, xyjoinid=xyjoinid, clipxy=clipxy, 
		datsource=datsource, data_dsn=data_dsn, istree=istree, plot_layer=plot_layer, 
		cond_layer=cond_layer, tree_layer=tree_layer, seed_layer=seed_layer, 
 		isseed=isseed, intensity1=intensity1, savedata=FALSE, savexy=TRUE, ...)
    if (is.null(pltdat)) return(NULL)
    if (saveobj) {
      message("saving pltdat object to: ", file.path(outfolder, "pltdat.rda"), "...")
      save(pltdat, file=file.path(outfolder, "pltdat.rda"))
    }
  } else {
    pltdat.names <- c("xypltx", "bndx", "xy.uniqueid", "puniqueid",
		"pjoinid", "tabs")
    if (!all(pltdat.names %in% names(pltdat))) {
      stop("missing components in pltdat list: ", 
		toString(pltdat.names[!pltdat.names %in% names(pltdat)])) 
    }
  }
 
  ## Extract list objects
  spxy <- pltdat$spxy
  xyplt <- pltdat$xypltx
  xy.uniqueid <- pltdat$xy.uniqueid
  puniqueid <- pltdat$puniqueid
  pjoinid <- pltdat$pjoinid
  pltx <- pltdat$tabs$pltx
  condx <- pltdat$tabs$condx
  treex <- pltdat$tabs$treex
  seedx <- pltdat$tabs$seedx
  SAdoms <- pltdat$bndx

  ## Check SAdoms
  #if (!all(c("DOMAIN", "AOI") %in% names(SAdoms))) {
  #  stop("invalid SAdoms...  need to include DOMAIN and AOI attributes")
  #}

  if (showsteps && !is.null(spxy)) {
    ## Set plotting margins
    mar <-  par("mar")
    par(mar=c(1,1,1,1))

    plot(sf::st_geometry(SAdoms), border="grey")
    plot(sf::st_geometry(spxy), add=TRUE, col="blue", cex=.25)
    if (!is.null(smallbnd) && "sf" %in% class(smallbnd)) { 
      plot(sf::st_geometry(smallbnd), add=TRUE, border="red")
    }
    par(mar=mar)
  }

  if (savesteps && !is.null(spxy)) {
    ## Set plotting margins
    mar <-  par("mar")
    par(mar=c(1,1,1,1))

    out_layer <- paste0("SAdoms_plots")
    jpgfn <- paste0(stepfolder, "/", out_layer, ".jpg")
    jpeg(jpgfn, res=400, units="in", width=8, height=10)
    plot(sf::st_geometry(SAdoms), border="grey")
    #plot(sf::st_geometry(bnd), add=TRUE)
    plot(sf::st_geometry(spxy), add=TRUE, col="blue", cex=.25)
    if (!is.null(smallbnd)) {
      plot(sf::st_geometry(smallbnd), add=TRUE, border="red")
    }
    dev.off()
    message("Writing jpg to ", jpgfn, "\n")
    par(mar=mar)
  }

  ## Check number of plots (Note: must be greater than 2 plots)
  ## If all AOIs have less than 2 plots, return NULL
  polyvarlst <- unique(c("DOMAIN", "AOI")[!c("DOMAIN", "AOI") %in% names(spxy)])
  extpoly <- spExtractPoly(xyplt=spxy, polyvlst=SAdoms, 
		uniqueid=xy.uniqueid, polyvarlst=polyvarlst, keepNA=FALSE)
  test <- data.table(st_drop_geometry((extpoly$spxyext)))
  test <- test[AOI == 1, .N, by="DOMAIN"]

  message("checking number of plots in domain...")

  if (nrow(test) == 0) {
    message("No plots in AOI... no estimates generated")
    return(NULL)
  } else if (all(test$N <= 2)) {
    message("ALL AOIs have 2 plots or less... no estimates generated")
    print(test)
    return(NULL)
  }
  ####################################################################
  ## Get model data
  ####################################################################
  message("summarizing auxiliary model data...")
  auxdat <- spGetAuxiliary(xyplt=spxy, uniqueid=xy.uniqueid, 
		dunit_layer=SAdoms, rastfolder=rastfolder,
	  	rastlst.cont=rastlst.cont, rastlst.cont.name=rastlst.cont.name, 
		rastlst.cont.NODATA=rastlst.cont.NODATA, 
		rastlst.cat=rastlst.cat, rastlst.cat.name=rastlst.cat.name, 
		rastlst.cat.NODATA=rastlst.cat.NODATA, keepNA=FALSE, npixels=TRUE, 
		vars2keep="AOI", savedata=FALSE)
  pltassgn <- auxdat$pltassgn
  dunitzonal <- auxdat$dunitzonal
  dunitvar <- auxdat$dunitvar
  prednames <- auxdat$prednames
  zonalnames <- auxdat$zonalnames
  predfac <- auxdat$predfac
  dunitarea <- auxdat$dunitarea
  areavar <- auxdat$areavar
  pltassgnid <- auxdat$pltassgnid

  ##########################################
  ## Create output list
  ##########################################
  #pltdat <- pltdat
  #pltdat$spxy=pltdat$xypltx=xy.uniqueid <- NULL


  SAdata <- list(SAdoms=SAdoms, smallbnd=smallbnd, plt=pltx, 
	pltassgn=pltassgn, cond=condx, 
	dunitarea=dunitarea, dunitvar=dunitvar, areavar=areavar, 
	dunitzonal=dunitzonal, prednames=prednames, predfac=predfac,
	zonalnames=zonalnames, puniqueid=puniqueid, pjoinid=pjoinid, 
	pltassgnid=pltassgnid)
  if (istree) {
    SAdata$tree <- treex
  }
  if (isseed) {
    SAdata$seed <- seedx
  }

  if (savexy) {
    SAdata$spxy <- spxy
    SAdata$xy.uniqueid <- xy.uniqueid
  }

  if (saveobj) {
    objfn <- getoutfn(outfn="SApopdat.rds", outfolder=outfolder, 
		overwrite=overwrite_layer, outfn.date=TRUE)
    saveRDS(SAdata, file=objfn)
    message("saving object to: ", objfn)
  } 

  if (savedata) {
    if (!is.null(RS)) {
      datExportData(sf::st_drop_geometry(SAdoms), outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="SAdoms", 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer)
    }
    if (savexy) {
      datExportData(sf::st_drop_geometry(spxy), outfolder=outfolder, 
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
    if (istree) {
      datExportData(treex, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="tree", 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer)
    }
    if (isseed) {
      datExportData(seedx, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="tree", 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer)
    }
    datExportData(dunitarea, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="dunitarea", 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer)
    datExportData(dunitzonal, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="dunitzonal", 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer)
  }
   	
  return(SAdata)
}
