spGetModeldat <- function(xyplt, xyplt_dsn=NULL, uniqueid="PLT_CN",
 	domtype="POLY", dom_layer=NULL, dom_dsn=NULL, domvar="DOMAIN",
 	rastlst.cont=NULL, rastlst.cont.name=NULL, rastlst.cont.stat="mean", 
	rastlst.cont.NODATA=NULL, rastlst.cat=NULL, rastlst.cat.name=NULL, 
	rastlst.cat.NODATA=NULL, rastfolder=NULL, asptransform=FALSE, rast.asp=NULL, 
	rast.lut=NULL, rastlut=NULL, areacalc=TRUE, areaunits="ACRES", keepNA=TRUE, 
	NAto0=TRUE, npixels=TRUE, showext=FALSE, savedata=FALSE, exportsp=FALSE, 
	exportNA=FALSE, outfolder=NULL, out_fmt="csv", out_dsn=NULL, outfn.pre=NULL, 
	outfn.date=FALSE, overwrite=TRUE, vars2keep=NULL, ...){

  ##################################################################################
  ## DESCRIPTION: Get data extraction and zonal statistics for Model-assisted or
  ##		Model-based (Small Area) Estimation. The major steps are as follows:
  ## 1) Check parameters 
  ## 2) Extract point values from domlayer
  ## 3) Set up output data structures
  ## 4) Extract point values and get zonal statistics from continuous raster layers
  ## 5) Extract point values and get zonal statistics from categorical raster layers
  ## 6) Get total acres from domlayer (if areacalc=TRUE)
  ##################################################################################


  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- ifelse(nargs() == 0, TRUE, FALSE)

  if (gui) {uniqueid=savedata <- NULL}

  ## Set global variables
  value=count=ACRES=TOTPIXELCNT=rast.lutfn=predfac=aspfn=prednames.cat <- NULL

  ## Adds to file filters to Cran R Filters table.
  if (.Platform$OS.type=="windows") {
    Filters=rbind(Filters,shp=c("Shapefiles (*.shp)", "*.shp"))
    Filters=rbind(Filters,img=c("Erdas Imagine Images (*.img)", "*.img"))
    Filters=rbind(Filters,tif=c("Raster tif files (*.tif)", "*.tif"))
    Filters=rbind(Filters,csv=c("Comma-delimited files (*.csv)", "*.csv")) }

  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- c(names(formals(FIESTA::spGetModeldat)), 
		names(formals(FIESTA::spMakeSpatialPoints)))
  if (!all(input.params %in% formallst)) {
    miss <- input.params[!input.params %in% formallst]
    stop("invalid parameter: ", toString(miss))
  }


  ##################################################################################
  ## CHECK INPUT PARAMETERS
  ##################################################################################

  ## Spatial points for data extraction.. 
  ##################################################################################
  sppltx <- pcheck.table(tab=xyplt, tab_dsn=xyplt_dsn, tabnm="xyplt", 
			caption="XY coordinates?", stopifnull=TRUE)

  if (!"sf" %in% class(sppltx)) { 
    ## Create spatial object from xyplt coordinates
    sppltx <- spMakeSpatialPoints(xyplt=sppltx, xy.uniqueid=uniqueid, 
		exportsp=FALSE, ...)
  } else {
    ## GET uniqueid
    sppltnames <- names(sppltx)
    uniqueid <- FIESTA::pcheck.varchar(var2check=uniqueid, varnm="uniqueid", gui=gui, 
		checklst=sppltnames, caption="UniqueID of spplt", 
		warn=paste(uniqueid, "not in spplt"), stopifnull=TRUE)
  }

  ## Check domtype
  ###################################################################
  domtypelst <- c("POLY", "RASTER") 
  domtype <- FIESTA::pcheck.varchar(var2check=domtype, varnm="domtype", gui=gui,
	checklst=domtypelst, caption="Estimation unit type?", stopifnull=TRUE)

  ## Check domlayer and domvar
  ###################################################################
  if (domtype == "POLY") {
    ## Check domlayer
    domlayerx <- pcheck.spatial(layer=dom_layer, dsn=dom_dsn, gui=gui, 
		caption="Domain spatial polygons?", stopifnull=TRUE)

    ## Check domvar
    domvar <- FIESTA::pcheck.varchar(var2check=domvar, varnm="domvar", gui=gui, 
		checklst=names(domlayerx), caption="Domain variable", 
		warn=paste(domvar, "not in domlayer"))
    if (is.null(domvar)) {
      domvar <- "ONEUNIT"
      domlayerx[[domvar]] <- 1
    }

    varsmiss <- vars2keep[which(!vars2keep %in% names(domlayerx))]
    if (length(varsmiss) > 0) 
      stop("missing variables: ", paste(varsmiss, collapse=", "))
  
  } else {
    stop("under construction... please convert dom_layer to POLY")
  }

  ## Check continuous rasters
  ###################################################################
  rastlst.contfn <- suppressWarnings(getrastlst.rgdal(rastlst.cont, rastfolder, gui=gui))

  if (!is.null(rastlst.contfn)) {
    band.cont <- sapply(rastlst.contfn, function(x) rasterInfo(x)$nbands)
    nlayers.cont <- sum(band.cont)

    ## Check rastlst.cont.stat
    rastlst.cont.statlst <- c("mean", "sum") 
    rastlst.cont.stat <- FIESTA::pcheck.varchar(var2check=rastlst.cont.stat, 
		varnm="rastlst.cont.stat", gui=gui, checklst=rastlst.cont.statlst, 
		caption="Raster zonal stat?")
    if (is.null(rastlst.cont.stat)) rastlst.cont.stat <- "mean"

    ## Check if length of names equals either length of bands or length of rasters
    if (!is.null(rastlst.cont.name) && (!length(rastlst.cont.name) %in% 
			c(length(rastlst.cont), nlayers.cont))) {
      stop(paste0("number of rastlst.cont.name (", length(rastlst.cont.name), ") does not ", 
		"match number of rastlst.cont layers (", nlayers.cont, ")"))
    }

    ## Check rastlst.cont.NODATA
    if (!is.null(rastlst.cont.NODATA)) {
      if (!is.numeric(rast.cont.NODATA))
        stop("rast.cont.NODATA must be numeric")

      if (length(rastlst.cont.NODATA) == 1 && nlayers.cont > 1) {
        message("using same rastlst.cont.NODATA value for each raster in rastlst.cont")
        rastlst.cont.NODATA <- rep(rastlst.cont.NODATA, nlayers.cont)
      } else if (length(rastlst.cont.NODATA) > 1 && length(rastlst.cont.NODATA) != nlayers.cont) {
        stop("rastlst.cont.NODATA must be same length as rastlst.cont: ", nlayers.cont)
      }
    }

    ## Check asptransform    
    asptransform <- FIESTA::pcheck.logical(asptransform, varnm="asptransform", 
		title="Transform aspect layer?", first="YES", gui=gui)

    ## Transform aspect 
    if (asptransform) {
      ## Check aspect raster
      rast.aspfn <- getrastlst.rgdal(rast.asp, rastfolder, gui=gui)  

      if (is.null(rast.aspfn))
        stop("must identify aspect raster in rastlst.contfn using rast.asp")
      if (length(rast.aspfn) > 1) 
        stop("only one raster allowed for transforming aspect") 
      if (!rast.aspfn %in% rastlst.contfn)
        stop("rast.asp must be included in rastlst.contfn")
    }
  }

  ## Check categorical rasters
  ###################################################################
  rastlst.catfn <- suppressWarnings(getrastlst.rgdal(rastlst.cat, rastfolder, gui=gui))

  if (!is.null(rastlst.catfn)) {
    band.cat <- sapply(rastlst.catfn, function(x) rasterInfo(x)$nbands)
    nlayers.cat <- sum(band.cat)

    if (!is.null(rastlst.cat.name) && length(rastlst.cat.name) != length(rastlst.catfn))
      stop(paste0("number of rastlst.cat.name (", length(rastlst.cat.name), ") does not ", 
		"match number of rastlst.cat layers (", nlayers.cat, ")"))

    ## Check rastlst.cat.NODATA
    if (!is.null(rastlst.cat.NODATA)) {
      if (!is.numeric(rastlst.cat.NODATA))
        stop("rastlst.cat.NODATA must be numeric")

      if (length(rastlst.cat.NODATA) == 1 && nlayers.cat > 1) {
        message("using same rastlst.cat.NODATA value for each raster in rastlst.cat")
        rastlst.cat.NODATA <- rep(rastlst.cat.NODATA, nlayers.cat)
      } else if (length(rastlst.cat.NODATA) > 1 && length(rastlst.cat.NODATA) != nlayers.cat) {
        stop("rastlst.cat.NODATA must be same length as rastlst.cat: ", nlayers.cat)
      }
    }

    ## Check raster for lookup table
    rast.lutfn <- suppressWarnings(getrastlst.rgdal(rast.lut, rastfolder, gui=gui))
    
    if (!is.null(rast.lutfn)) {
      if (length(rast.lutfn) > 1) 
        stop("only one categorical raster allowed for grouping classes") 
      if (!rast.lutfn %in% rastlst.catfn)
        stop("rast.lut must be included in rastlst.catfn")

      ## Check rastlut
      rastlutx <- FIESTA::pcheck.table(rastlut, gui=gui, caption="Data table?", 
		returnDT=TRUE)
      if (is.null(rast.lut)) 
        stop("invalid lookup table for", rast.lut)
    } 
  }

  ## npixels    
  npixels <- FIESTA::pcheck.logical(npixels, varnm="npixels", 
		title="Number of pixels?", first="YES", gui=gui)

  ## Check showext    
  showext <- FIESTA::pcheck.logical(showext, varnm="showext", 
		title="Plot extents?", first="YES", gui=gui)

  ## Check keepNA    
  keepNA <- FIESTA::pcheck.logical(keepNA, varnm="keepNA", 
		title="Keep NA values?", first="YES", gui=gui)

  ## Check exportNA    
  exportNA <- FIESTA::pcheck.logical(exportNA, varnm="exportNA", 
		title="Export NA values?", first="YES", gui=gui)

  ## Check savedata 
  savedata <- FIESTA::pcheck.logical(savedata, varnm="savedata", 
		title="Save data extraction?", first="NO", gui=gui)  

  ## Check exportsp 
  exportsp <- FIESTA::pcheck.logical(exportsp, varnm="exportsp", 
		title="Export spatial?", first="NO", gui=gui)  


  ## Check overwrite, outfn.date, outfolder, outfn 
  ########################################################
  if (savedata || exportsp || exportNA) {
    outfolder <- FIESTA::pcheck.outfolder(outfolder, gui)
    overwrite <- FIESTA::pcheck.logical(overwrite, varnm="overwrite", 
		title="Overwrite?", first="NO", gui=gui)  
    outfn.date <- FIESTA::pcheck.logical(outfn.date , varnm="outfn.date", 
		title="Add date to outfiles?", first="YES", gui=gui) 

    out_fmtlst <- c("csv", "sqlite", "gpkg", "shp")
    out_fmt <- FIESTA::pcheck.varchar(var2check=out_fmt, varnm="out_fmt", 
		checklst=out_fmtlst, gui=gui, caption="Output format?") 

    if (out_fmt %in% c("sqlite", "gpkg")) {
      gpkg <- ifelse(out_dsn == "gpkg", TRUE, FALSE)        
      out_dsn <- DBcreateSQLite(out_dsn, outfolder=outfolder, outfn.date=outfn.date, 
				overwrite=overwrite, dbconnopen=FALSE)
    } else {
      if (!is.null(outfn.pre)) {
        outfolder <- file.path(outfolder, outfn.pre)
        if (!dir.exists(outfolder)) dir.create(outfolder)
      }
    } 
  }

  ##################################################################
  ## DO WORK
  ##################################################################

  #############################################################################
  ## 1) Extract values from domlayer
  #############################################################################
  domarea <- NULL
  if (!domvar %in% names(sppltx)) { 
      ## Extract values of polygon layer to points
    extpoly <- spExtractPoly(xyplt=sppltx, polyvlst=domlayerx, 
		uniqueid=uniqueid, polyvarlst=unique(c(domvar, vars2keep)), 
		keepNA=FALSE, exportNA=exportNA)
    sppltx <- unique(extpoly$spplt)
  } else {
    message(domvar, " already in spplt... not extracting from domlayer")
  }

  #############################################################################
  ## 2) Set up outputs - domlut, prednames, inputdf, zonalnames
  #############################################################################
  domlut <- data.table(unique(sf::st_drop_geometry(domlayerx[, c(domvar, vars2keep),
 		drop=FALSE])))
  setkeyv(domlut, domvar)
  prednames <- {}
  inputdf <- {}
  zonalnames <- {}

  ###############################################################################
  ## 3) Continuous raster layers - Extract values and get zonal statistics
  ###############################################################################
  if (!is.null(rastlst.cont)) {
    ## Extract values from continuous raster layers
    #############################################################################
    extdat.rast.cont <- spExtractRast(sppltx, uniqueid=uniqueid,
			rastlst=rastlst.contfn, interpolate=FALSE, showext=showext,
			var.name=rastlst.cont.name, rast.NODATA=rastlst.cont.NODATA, 
			keepNA=keepNA, exportNA=exportNA)
    sppltx <- unique(extdat.rast.cont$spplt)
    prednames.cont <- extdat.rast.cont$outnames
    inputdf.cont <- extdat.rast.cont$inputdf
    rm(extdat.rast.cont)
    gc() 

    if (NAto0) 
      for (col in prednames.cont) set(sppltx, which(is.na(sppltx[[col]])), col, 0)

    ## Transform aspect 
    if (asptransform) {
      aspnm <- inputdf.cont$var.name[inputdf.cont$rasterfile == rast.aspfn]     
      sppltx$cosAsp <- northness(sppltx[[aspnm]])
      sppltx$sinAsp <- eastness(sppltx[[aspnm]])
      prednames.cont <- c(prednames.cont[prednames.cont != aspnm], "cosAsp", "sinAsp")
    }
    prednames <- c(prednames, prednames.cont)
    inputdf <- rbind(inputdf, inputdf.cont)
    zonalnames <- c(zonalnames, prednames)
 
    ## Extract zonal means from continuous raster layers
    #############################################################################
    zonalDT.cont <- data.table(DOMAIN = unique(domlayerx[[domvar]]))
    setnames(zonalDT.cont, "DOMAIN", domvar)
    setkeyv(zonalDT.cont, domvar)
    #zonalDT.cont.names <- {}

    for (i in 1:length(rastlst.contfn)) {
      rastfn <- rastlst.contfn[i]
      rastnm <- inputdf.cont$var.name[inputdf.cont$rasterfile == rastfn]
      rast.cont.NODATA <- rastlst.cont.NODATA[i]
      zonalstat <- rastlst.cont.stat 
      message(rastfn, "...")

      if (asptransform && identical(rast.aspfn, rastfn)) {
        rastnm2 <- ifelse(is.null(rastnm), "cosAsp", paste0(rastnm, "_cos"))
        if (i == 1 && npixels) {
          zonalstat <- c("npixels", rastlst.cont.stat) 
          rastnm2 <- c("npixels", rastnm2)
        }  
        zonaldat.rast.cont <- spZonalRast(domlayerx, rast=rastfn, polyv.att=domvar, 
		zonalstat=zonalstat, pixelfun=northness, rast.NODATA=rast.cont.NODATA,
		na.rm=TRUE)
        zonalext <- setDT(zonaldat.rast.cont$zonalext)
        outname <- zonaldat.rast.cont$outname

        if (!is.null(rastnm)) 
          setnames(zonalext, outname, rastnm2)
        setkeyv(zonalext, domvar)
        zonalDT.cont <- zonalDT.cont[zonalext] 
  
        rastnm2 <- ifelse(is.null(rastnm), "sinAsp", paste0(rastnm, "_sin"))
        zonalstat <- c(rastlst.cont.stat) 
        zonaldat.rast.cont <- spZonalRast(domlayerx, rast=rastfn, rast.NODATA=rast.cont.NODATA,
 		polyv.att=domvar, zonalstat=rastlst.cont.stat, pixelfun=eastness, na.rm=TRUE)
        zonalext <- setDT(zonaldat.rast.cont$zonalext)
        outname <- zonaldat.rast.cont$outname
        if (!is.null(rastnm2)) 
          setnames(zonalext, outname, rastnm2)
        setkeyv(zonalext, domvar)
        zonalDT.cont <- zonalDT.cont[zonalext]
 
      } else {
        if (i == 1 && npixels) {
          zonalstat <- c("npixels", rastlst.cont.stat) 
          if (!is.null(rastnm)) 
            rastnm <- c("npixels", rastnm)
        }  
        zonaldat.rast.cont <- spZonalRast(domlayerx, rast=rastfn, rast.NODATA=rast.cont.NODATA, 
		polyv.att=domvar, zonalstat=zonalstat, showext=showext, na.rm=TRUE)
        zonalext <- setDT(zonaldat.rast.cont$zonalext)
        outname <- zonaldat.rast.cont$outname

        if (!is.null(rastnm)) 
          setnames(zonalext, outname, rastnm)
        setkeyv(zonalext, domvar)
        zonalDT.cont <- zonalDT.cont[zonalext] 
      }
      if (npixels) npixels <- FALSE
      rm(zonaldat.rast.cont)
      rm(zonalext)
      gc() 
    }
    domlut <- domlut[zonalDT.cont] 
  }

  ###############################################################################
  ## 4) Categorical raster layers - Extract values and get zonal probabilities
  ###############################################################################
  if (!is.null(rastlst.cat)) {

    ## Extract values from categorical raster layers
    ######################################################
    extdat.rast.cat <- spExtractRast(sppltx, uniqueid=uniqueid, rastlst=rastlst.catfn, 
			interpolate=FALSE, var.name=rastlst.cat.name, rast.NODATA=rastlst.cat.NODATA,
			keepNA=keepNA, exportNA=exportNA)
    sppltx <- extdat.rast.cat$sppltext
    prednames.cat <- extdat.rast.cat$outnames
    inputdf.cat <- extdat.rast.cat$inputdf
    prednames <- c(prednames, prednames.cat)
    predfac <- c(predfac, prednames.cat)
    inputdf <- rbind(inputdf, inputdf.cat)
    rm(extdat.rast.cat)
    gc() 

    if (NAto0) 
      for (col in prednames.cat) set(sppltx, which(is.na(sppltx[[col]])), col, 0)

    if (!is.null(rast.lut)) {
      rast.lutnm <- inputdf.cat$var.name[inputdf.cat$rasterfile == rast.lutfn]

      if (!rast.lutnm %in% names(rastlut)) 
        stop("must have variable named ", rast.lutnm, " in rastlut")

      ## Check that all values of sppltx are in rastlut
      FIESTA::check.matchval(sppltx, rastlut, rast.lutnm, tab1txt="sppltx", 
		tab2txt="rastlut")

      ## Check if class of rast.lutnm in rastlut matches class of rast.lutnm in sppltx
      tabs <- FIESTA::check.matchclass(sppltx, rastlut, uniqueid, rast.lutnm)
      sppltx <- tabs$tab1
      rastlut <- tabs$tab2

      sppltx <- merge(sppltx, rastlut, by=rast.lutnm, all.x=TRUE)
      sppltx <- sppltx[, c(names(sppltx)[!names(sppltx) %in% names(rastlut)],
				names(rastlut))]
    }
      
    ## Extract zonal proportions from categorical raster layers
    #############################################################################
    zonalDT.cat <- data.table(DOMAIN = unique(domlayerx[[domvar]]))
    setnames(zonalDT.cat, "DOMAIN", domvar)
    setkeyv(zonalDT.cat, domvar)
    for (i in 1:length(rastlst.catfn)) {
      rastfn <- rastlst.catfn[i]
      rastnm <- inputdf.cat[inputdf.cat$rasterfile == rastfn, "var.name"][[1]]
      message(rastfn, "...")
      rast.cat.NODATA <- rastlst.cat.NODATA[i]

      zonalstat <- "proportion"
      if (i == 1 && npixels)
        zonalstat <- c("npixels", zonalstat)        
      if (identical(rast.lutfn, rastfn)) {
        zonaldat.rast.cat <- spZonalRast(domlayerx, rast=rastfn, rast.NODATA=rast.cat.NODATA, 
 		polyv.att=domvar, zonalstat=zonalstat, rastlut=rastlut, outname=names(rastlut)[2],
		na.rm=TRUE)
      } else {
        zonaldat.rast.cat <- spZonalRast(domlayerx, rast=rastfn, rast.NODATA=rast.cat.NODATA, 
 		polyv.att=domvar, outname=rastnm, zonalstat=zonalstat, na.rm=TRUE)
      }

      zonalext <- setDT(zonaldat.rast.cat$zonalext)
      outname <- zonaldat.rast.cat$outname
      outname[grep("npixels", outname)] <- "npixels"
      setnames(zonalext, c(domvar, outname))
      setkeyv(zonalext, domvar)
      zonalDT.cat <- zonalDT.cat[zonalext] 
      zonalnames <- c(zonalnames, outname[outname != "npixels"])

      if (npixels) npixels <- FALSE
      rm(zonaldat.rast.cat)
      rm(zonalext)
      gc() 
    }
    domlut <- domlut[zonalDT.cat]  
  }
 
  ###################################################################################
  ## Get totacres from domain polygons (if areacalc = TRUE)
  ###################################################################################
  if (areacalc) {
    domlayerx <- areacalc.poly(domlayerx, unit=areaunits)
    areavar <- paste0(areaunits, "_GIS")

    domarea <- domlayerx[, c(domvar, areavar)]
    domarea <- aggregate(domarea[[areavar]], list(domarea[[domvar]]), sum)
    names(domarea) <- c(domvar, areavar)
  }

  ## Write data frames to CSV files
  #######################################
  pltassgn <- sf::st_drop_geometry(sppltx)
  if (savedata) {
    datExportData(pltassgn, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="pltassgn", 
		outfn.date=outfn.date, overwrite_layer=overwrite)
    datExportData(domlut, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="domlut", 
		outfn.date=outfn.date, overwrite_layer=overwrite)
  }


  returnlst <- list(pltassgn=pltassgn, domzonal=setDF(domlut), domvar=domvar,
		inputdf=inputdf, prednames=prednames, zonalnames=zonalnames, 
		predfac=predfac, npixelvar="npixels", pltassgnid=uniqueid)
  if (areacalc) {
    returnlst$domarea <- domarea
    returnlst$areavar <- areavar
  }
 
  return(returnlst)
}

