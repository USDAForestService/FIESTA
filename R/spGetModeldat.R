spGetModeldat <- function(spplt=NULL, spplt_dsn=NULL, xyplt=NULL, uniqueid="PLT_CN",
 	module="SA", domtype="POLY", dom_layer=NULL, dom_dsn=NULL, domvar="DOMAIN",
 	rastlst.cont=NULL, rastlst.cont.name=NULL, rastlst.cont.stat="mean", 
	rastlst.cat=NULL, rastlst.cat.name=NULL, rastfolder=NULL, rast.NODATA=NULL,
 	asptransform=FALSE, rast.asp=NULL, rast.lut=NULL, rastlut=NULL, areacalc=TRUE,
 	areaunits="ACRES", keepnull=FALSE, showext=FALSE, savedata=FALSE, 
	exportshp=FALSE, outfolder=NULL, outfn=NULL, outfn.date=TRUE, overwrite=FALSE,
 	vars2keep=NULL, ...){

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

  if (gui) {dat=xytable=uniqueid=unionshpnm=stratclip=savedata=parameters <- NULL}

  ## Set global variables
  value=count=ACRES=TOTPIXELCNT=rast.lutfn=predfac=aspfn <- NULL

  ## Adds to file filters to Cran R Filters table.
  if (.Platform$OS.type=="windows") {
    Filters=rbind(Filters,shp=c("Shapefiles (*.shp)", "*.shp"))
    Filters=rbind(Filters,img=c("Erdas Imagine Images (*.img)", "*.img"))
    Filters=rbind(Filters,tif=c("Raster tif files (*.tif)", "*.tif"))
    Filters=rbind(Filters,csv=c("Comma-delimited files (*.csv)", "*.csv")) }


  ##################################################################################
  ## CHECK INPUT PARAMETERS
  ##################################################################################

  ## Spatial Points: shp or xytable for data extraction.. 
  ##################################################################################
  if (is.null(spplt)) {

    ## Check parameters
    if (is.null(xyplt)) stop("either include spplt or xyplt for data extraction")
         
    ## Create spatial object from xyplt coordinates
    sppltx <- FIESTA::spMakeSpatialPoints(xyplt=xyplt, uniqueid=uniqueid, ...)
  } else {

    ## Check spplt
    sppltx <- pcheck.spatial(layer=spplt, dsn=spplt_dsn, gui=gui,
		caption="Spatial points with XY coords?")
    if (is.null(sp::proj4string(sppltx))) stop("spplt must have defined projection")

    ## GET uniqueid
    sppltnames <- names(sppltx@data)
    uniqueid <- FIESTA::pcheck.varchar(var2check=uniqueid, varnm="uniqueid", gui=gui, 
		checklst=sppltnames, caption="UniqueID of spplt", 
		warn=paste(uniqueid, "not in spplt"))

    ## Check for NA or duplicate values in uniqueid
    if (sum(is.na(sppltx@data[[uniqueid]])) > 0) stop("NA values in ", uniqueid)
    if (length(unique(sppltx@data[[uniqueid]])) < nrow(sppltx@data)) 
      stop("spplt records are not unique")
  }


  ## Verify spatial Layers
  ##################################################################################
  modulelst <- c("SA", "MA") 
  module <- FIESTA::pcheck.varchar(var2check=module, varnm="module", gui=gui,
	checklst=modulelst, caption="Module?", stopifnull=TRUE)

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

    ## Check attribute
    domvar <- FIESTA::pcheck.varchar(var2check=domvar, varnm="domvar", gui=gui, 
		checklst=names(domlayerx), caption="Domain variable", 
		warn=paste(domvar, "not in domlayer"))
    if (is.null(domvar)) {
      if (module == "SA") {
        domvar <- "ONEDOM"
      } else if (module == "MA") {
        domvar <- "ONEUNIT"
      }
      domlayerx@data[[domvar]] <- 1
    }

    varsmiss <- vars2keep[which(!vars2keep %in% names(domlayerx))]
    if (length(varsmiss) > 0) 
      stop("missing variables: ", paste(varsmiss, collapse=", "))
  
  } else {
    stop("under construction... please convert dom_layer to POLY")
  }

  ## Check continuous rasters
  ###################################################################
  rastlst.contfn <- FIESTA::getrastlst(rastlst.cont, rastfolder, filenames=TRUE, gui=gui)

  if (!is.null(rastlst.contfn)) {
    band.cont <- sapply(rastlst.contfn, function(x) raster::nbands(raster(x)))
    nlayers.cont <- sum(band.cont)

    ## Check rastlst.cont.stat
    rastlst.cont.statlst <- c("mean", "sum") 
    rastlst.cont.stat <- FIESTA::pcheck.varchar(var2check=rastlst.cont.stat, 
		varnm="rastlst.cont.stat", gui=gui, checklst=rastlst.cont.statlst, 
		caption="Raster zonal stat?")
    if (is.null(rastlst.cont.stat)) rastlst.cont.stat <- "mean"

    if (!is.null(rastlst.cont.name) && length(rastlst.cont.name) != nlayers.cont)
      stop(paste0("number of rastlst.cont.name (", length(rastlst.cont.name), ") does not ", 
		"match number of rastlst.cont layers (", nlayers.cont, ")"))

    ## Check asptransform    
    asptransform <- FIESTA::pcheck.logical(asptransform, varnm="asptransform", 
		title="Transform aspect layer?", first="YES", gui=gui)

    ## Transform aspect 
    if (asptransform) {
      ## Check aspect raster
      rast.aspfn <- FIESTA::getrastlst(rast.asp, rastfolder, filenames=TRUE, gui=gui)  

      if (is.null(rast.aspfn)) {
        stop("must identify aspect raster using rast.asp")
      } else if (sum(sapply(rastlst.contfn, function(x) identical(x, rast.aspfn))) == 0) {
        stop("rast.asp does not match any rasters in rastlst.cont")
      } 
    }
  }


  ## Check categorical rasters
  ###################################################################
  rastlst.catfn <- getrastlst(rastlst.cat, rastfolder, filenames=TRUE, gui=gui)

  if (!is.null(rastlst.catfn)) {
    band.cat <- sapply(rastlst.catfn, function(x) raster::nbands(raster(x)))
    nlayers.cat <- sum(band.cat)

    if (!is.null(rastlst.cat.name) && length(rastlst.cat.name) != length(rastlst.catfn))
      stop(paste0("number of rastlst.cat.name (", length(rastlst.cat.name), ") does not ", 
		"match number of rastlst.cat layers (", nlayers.cat, ")"))


    ## Check raster for lookup table
    rast.lutfn <- FIESTA::getrastlst(rast.lut, rastfolder, filenames=TRUE, gui=gui)  

    if (!is.null(rast.lutfn)) {
      ## Check rastlut
      rastlutx <- FIESTA::pcheck.table(rastlut, gui=gui, caption="Data table?", returnDT=TRUE)

      if (is.null(rast.lut)) {
        stop("must include a value lookup table for", rast.lut)
      } else if (sum(sapply(rastlst.catfn, function(x) identical(x, rast.lutfn))) == 0) {
          stop("rast.lut does not match any rasters in rastlst.cont")
      }
    } 
  }

  ## Check showext    
  showext <- FIESTA::pcheck.logical(showext, varnm="showext", 
		title="Plot extents?", first="YES", gui=gui)

  ## Check keepnull    
  keepnull <- FIESTA::pcheck.logical(keepnull, varnm="keepnull", 
		title="Keep NULL values?", first="YES", gui=gui)

  ## Check savedata 
  savedata <- FIESTA::pcheck.logical(savedata, varnm="savedata", 
		title="Save data extraction?", first="NO", gui=gui)  


  ## Check outfolder 
  ########################################################
  if (savedata) {
    ## Check overwrite 
    overwrite <- FIESTA::pcheck.logical(overwrite, varnm="overwrite", 
		title="Overwrite files?", first="NO", gui=gui)  

    outfolder <- FIESTA::pcheck.outfolder(outfolder, gui)

    if (is.null(outfn)) {
      outfn <- "dataext"
    } else if (!is.character(outfn)) {
      stop("outfn must be character")
    }      
    outfncsvbase <- paste0(outfn, "_", format(Sys.time(), "%Y%m%d"))
    if (!overwrite)
      outfncsvbase <- FIESTA::fileexistsnm(outfolder, outfncsvbase, "csv") 
 
    if (exportshp) {
      outfnshpbase <- paste0(outfn, "_", format(Sys.time(), "%Y%m%d"))
      if (!overwrite)
        outfnshpbase <- FIESTA::fileexistsnm(outfolder, outfnshpbase, "shp")  
  
      outfnnamesbase <- paste0(outfn, "_newnames_", format(Sys.time(), "%Y%m%d"))
      if (!overwrite)
        outfnnamesbase <- FIESTA::fileexistsnm(outfolder, outfnnamesbase, "csv") 
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
    extpoly <- spExtractPoly(spplt=sppltx, polylst=domlayerx, 
		uniqueid=uniqueid, polyvarlst=unique(c(domvar, vars2keep)), keepnull=FALSE)
    sppltx <- extpoly$spplt
  } else {
    message(domvar, " already in spplt... not extracting from domlayer")
  }

  #############################################################################
  ## 2) Set up outputs - domlut, prednames, inputdf, zonalnames
  #############################################################################
#  domlut <- data.table(DOMAIN = unique(domlayerx[[domvar]]))
  domlut <- data.table(unique(domlayerx@data[, c(domvar, vars2keep), drop=FALSE]))
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
			var.name=rastlst.cont.name)
    sppltx <- extdat.rast.cont$spplt
    prednames.cont <- extdat.rast.cont$outnames
    inputdf.cont <- extdat.rast.cont$inputdf

    ## Transform aspect 
    if (asptransform) {
      if (sum(sapply(rastlst.cont, function(x) identical(x, rast.asp))) > 0) {
        aspfn <- rastlst.contfn[sapply(rastlst.cont, function(x) identical(x, rast.asp))]
        aspnm <- inputdf.cont$var.name[inputdf.cont$rasterfile == aspfn]
      } else if (sum(grepl(rast.asp, inputdf.cont$rasterfile)) > 0) {
        aspfn <- inputdf.cont$rasterfile[grepl(rast.asp, inputdf.cont$rasterfile)]
        aspnm <- inputdf.cont$var.name[grepl(rast.asp, inputdf.cont$rasterfile)]
      } else {
        #stop("invalid rast.asp")
        asptransform <- FALSE 
      } 
      sppltx@data$cosAsp <- northness(sppltx@data[[aspnm]])
      sppltx@data$sinAsp <- eastness(sppltx@data[[aspnm]])
#      sppltx@data[[aspnm]] <- NULL
      prednames.cont <- c(prednames.cont[prednames.cont != aspnm], "cosAsp", "sinAsp")
    
#    if (!is.null(rastfolder)) aspfn <- paste(rastfolder, aspfn, sep="/")
#    rastfnlst.cont <- rastfnlst.cont[rastfnlst.cont != aspfn] 
#    rastfnlst.cont <- c(rastfnlst.cont, paste0(aspfn, " - derived"), paste0(aspfn, " - derived"))
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
      print(rastfn)
      print(rastnm)

      if (i == 1) {
        zonalstat <- c("npixels", rastlst.cont.stat) 
        if (!is.null(rastnm)) 
          rastnm <- c("npixels", rastnm)
      } else {
        zonalstat <- rastlst.cont.stat 
      }  

      if (identical(aspfn, rastfn)) {
        zonaldat.rast <- spZonalRast(domlayerx, rast=rastfn, attribute=domvar, 
		zonalstat=zonalstat, pixelfun=northness)
        zonalext <- setDT(zonaldat.rast$zonalext)
        outname <- zonaldat.rast$outname
        setnames(zonalext, outname, "cosAsp")
        setkeyv(zonalext, domvar)
        zonalDT.cont <- zonalDT.cont[zonalext] 
  
        zonaldat.rast <- spZonalRast(domlayerx, rast=rastfn, rast.NODATA=rast.NODATA,
 		attribute=domvar, zonalstat=rastlst.cont.stat, pixelfun=eastness)
        zonalext <- setDT(zonaldat.rast$zonalext)
        outname <- zonaldat.rast$outname
        setnames(zonalext, outname, "sinAsp")
        setkeyv(zonalext, domvar)
        zonalDT.cont <- zonalDT.cont[zonalext]
 
      } else {
        zonaldat.rast <- spZonalRast(domlayerx, rast=rastfn, rast.NODATA=rast.NODATA, 
		attribute=domvar, zonalstat=zonalstat, showext=FALSE)
        zonalext <- setDT(zonaldat.rast$zonalext)

        outname <- zonaldat.rast$outname
        if (!is.null(rastnm)) 
          setnames(zonalext, outname, rastnm)
        setkeyv(zonalext, domvar)
        zonalDT.cont <- zonalDT.cont[zonalext] 
      }
    }
    domlut <- domlut[zonalDT.cont] 

    rm(extdat.rast.cont)
    rm(zonaldat.rast)
    gc() 
  }

  ###############################################################################
  ## 4) Categorical raster layers - Extract values and get zonal probabilities
  ###############################################################################
  if (!is.null(rastlst.cat)) {

    ## Extract values from categorical raster layers
    ######################################################
    extdat.rast.cat <- spExtractRast(sppltx, uniqueid=uniqueid, rastlst=rastlst.catfn, 
			interpolate=FALSE, var.name=rastlst.cat.name)
    sppltx <- extdat.rast.cat$spplt
    prednames.cat <- extdat.rast.cat$outnames
    inputdf.cat <- extdat.rast.cat$inputdf
    prednames <- c(prednames, prednames.cat)
    predfac <- c(predfac, prednames.cat)
    inputdf <- rbind(inputdf, inputdf.cat)

    if (!is.null(rast.lut)) {
      rast.lutnm <- inputdf.cat$var.name[inputdf.cat$rasterfile == rast.lutfn]

      if (!rast.lutnm %in% names(rastlut)) 
        stop("must have variable named ", rast.lutnm, " in rastlut")

      ## Check that all values of sppltx are in rastlut
      FIESTA::check.matchval(sppltx@data, rastlut, rast.lutnm, tab1txt="sppltx", 
		tab2txt="rastlut")

      ## Check if class of rast.lutnm in rastlut matches class of rast.lutnm in sppltx
      tabs <- FIESTA::check.matchclass(sppltx@data, rastlut, uniqueid, rast.lutnm)
      sppltx@data <- tabs$tab1
      rastlut <- tabs$tab2

      sppltx <- sp::merge(sppltx, rastlut, by=rast.lutnm, all.x=TRUE)
      sppltx@data <- sppltx@data[, c(names(sppltx@data)[!names(sppltx@data) %in% names(rastlut)],
				names(rastlut))]
    }
      
    ## Extract zonal proportions from categorical raster layers
    #############################################################################
    zonalDT.cat <- data.table(DOMAIN = domlayerx[[domvar]])
    setnames(zonalDT.cat, "DOMAIN", domvar)
    setkeyv(zonalDT.cat, domvar)
    for (i in 1:length(rastlst.catfn)) {
      rastfn <- rastlst.catfn[i]
      rastnm <- inputdf.cat[inputdf.cat$rasterfile == rastfn, "var.name"][[1]]
      print(rastfn)
      print(rastnm)

      if (is.null(rastlst.cont) && i == 1) {
        zonalstat <- c("npixels", "proportion")
      } else {
        zonalstat <- "proportion"
      }

      if (identical(rast.lutfn, rastfn)) {
        zonaldat.rast.cat <- spZonalRast(domlayerx, rast=rastfn, rast.NODATA=rast.NODATA, 
 		attribute=domvar, zonalstat=zonalstat, rastlut=rastlut, outname=names(rastlut)[2])
      } else {
        zonaldat.rast.cat <- spZonalRast(domlayerx, rast=rastfn, rast.NODATA=rast.NODATA, 
 		attribute=domvar, zonalstat=zonalstat, outname=rastnm)
      }
      zonalext <- setDT(zonaldat.rast.cat$zonalext)
      outnames <- zonaldat.rast.cat$outname
      setkeyv(zonalext, domvar)
      zonalDT.cat <- zonalDT.cat[zonalext] 
      zonalnames <- c(zonalnames, outnames)
    }
    domlut <- domlut[zonalDT.cat]  

    rm(extdat.rast.cat)
    rm(zonaldat.rast.cat)
    gc() 
  }
 
  ###################################################################################
  ## Get totacres from domain polygons (if areacalc = TRUE)
  ###################################################################################
  if (areacalc) {
    domlayerx <- FIESTA::areacalc.poly(domlayerx, units=areaunits)
    areavar <- paste0(areaunits, "_GIS")

    domarea <- domlayerx@data[, c(domvar, areavar)]
    domarea <- aggregate(domarea[[areavar]], list(domarea[[domvar]]), sum)
    names(domarea) <- c(domvar, areavar)
  }


  returnlst <- list(sppltmodel=sppltx, pltmodel=sppltx@data, domzonal=setDF(domlut), 
		domvar=domvar, inputdf=inputdf, prednames=prednames, zonalnames=zonalnames, 
		predfac=predfac)
  if (areacalc) {
    returnlst$domarea <- domarea
    returnlst$areavar <- areavar
  }
 
  return(returnlst)
}

