spGetAuxiliary <- function(xyplt, xyplt_dsn=NULL, uniqueid="PLT_CN",
 	dunittype="POLY", dunit_layer=NULL, dunit_dsn=NULL, dunitvar="DOMAIN",
 	rastlst.cont=NULL, rastlst.cont.name=NULL, rastlst.cont.stat="mean", 
	rastlst.cont.NODATA=NULL, rastlst.cat=NULL, rastlst.cat.name=NULL, 
	rastlst.cat.NODATA=NULL, rastfolder=NULL, asptransform=FALSE, rast.asp=NULL, 
	rast.lut=NULL, rastlut=NULL, strata=FALSE, rast.strata=NULL, areacalc=TRUE, 
	areaunits="ACRES", keepNA=TRUE, NAto0=TRUE, npixels=TRUE, returnxy=FALSE, 	
	showext=FALSE, savedata=FALSE, exportsp=FALSE, exportNA=FALSE, 
	outfolder=NULL, out_fmt="csv", out_dsn=NULL, outfn.pre=NULL, outfn.date=FALSE,
 	overwrite_dsn=FALSE, overwrite_layer=TRUE, append_layer=FALSE, vars2keep=NULL, ...){

  ##################################################################################
  ## DESCRIPTION: Get data extraction and zonal statistics for Model-assisted or
  ##		Model-based (Small Area) Estimation. The major steps are as follows:
  ## 1) Check parameters 
  ## 2) Extract point values from dunit_layer
  ## 3) Set up output data structures
  ## 4) Extract point values and get zonal statistics from continuous raster layers
  ## 5) Extract point values and get zonal statistics from categorical raster layers
  ## 6) Get total acres from dunit_layer (if areacalc=TRUE)
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
  formallst <- c(names(formals(spGetAuxiliary)), 
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
  sppltx.names <- names(sf::st_drop_geometry(sppltx))

  ## Check dunittype
  ###################################################################
  dunittypelst <- c("POLY", "RASTER") 
  dunittype <- FIESTA::pcheck.varchar(var2check=dunittype, varnm="dunittype", gui=gui,
	checklst=dunittypelst, caption="Estimation unit type?", stopifnull=TRUE)

  ## Check dunit_layer and dunitvar
  ###################################################################
  if (dunittype == "POLY") {
    ## Check dunit_layer
    dunit_layerx <- pcheck.spatial(layer=dunit_layer, dsn=dunit_dsn, gui=gui, 
		caption="Domain spatial polygons?", stopifnull=TRUE)

    ## Check dunitvar
    dunitvar <- FIESTA::pcheck.varchar(var2check=dunitvar, varnm="dunitvar", gui=gui, 
		checklst=names(dunit_layerx), caption="Domain variable", 
		warn=paste(dunitvar, "not in dunit_layer"))
    if (is.null(dunitvar)) {
      dunitvar <- "ONEUNIT"
      dunit_layerx[[dunitvar]] <- 1
    }

    varsmiss <- vars2keep[which(!vars2keep %in% names(dunit_layerx))]
    if (length(varsmiss) > 0) {
      stop("missing variables: ", paste(varsmiss, collapse=", "))
    }
  } else {
    stop("under construction... please convert dunit_layer to POLY")
  }

  ## Check continuous rasters
  ###################################################################
  rastlst.contfn <- suppressWarnings(getrastlst.rgdal(rastlst.cont, 
	rastfolder, gui=gui, quiet=TRUE, stopifLonLat=TRUE))

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
      if (!is.numeric(rastlst.cont.NODATA))
        stop("rastlst.cont.NODATA must be numeric")

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

      if (is.null(rast.aspfn)) {
        stop("must identify aspect raster in rastlst.contfn using rast.asp")
      }
      if (length(rast.aspfn) > 1) {
        stop("only one raster allowed for transforming aspect") 
      }
      if (!rast.aspfn %in% rastlst.contfn) {
        stop("rast.asp must be included in rastlst.contfn")
      }
    }
  }
 
  ## Check categorical rasters
  ###################################################################
  rastlst.catfn <- suppressWarnings(getrastlst.rgdal(rastlst.cat, 
	rastfolder, quiet=TRUE, gui=gui))

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
      } else if (length(rastlst.cat.NODATA) > 1 && 
		length(rastlst.cat.NODATA) != nlayers.cat) {
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

    ## Check strata    
    strata <- FIESTA::pcheck.logical(strata, varnm="strata", 
		title="Strata?", first="NO", gui=gui)
    if (strata) {
      ## Check aspect raster
      rast.stratafn <- getrastlst.rgdal(rast.strata, rastfolder, gui=gui)  

      if (is.null(rast.stratafn)) {
        stop("must identify strata raster in rastlst.catfn using rast.strata")
      }
      if (length(rast.stratafn) > 1) {
        stop("only one raster allowed for strata") 
      }
      if (!rast.stratafn %in% rastlst.catfn) {
        stop("rast.strata must be included in rastlst.catfn")
      }
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

  ## Check returnxy 
  returnxy <- FIESTA::pcheck.logical(returnxy, varnm="returnxy", 
		title="Return XY spatial data?", first="NO", gui=gui)  

  ## Check savedata 
  savedata <- FIESTA::pcheck.logical(savedata, varnm="savedata", 
		title="Save data extraction?", first="NO", gui=gui)  

  ## Check exportsp 
  exportsp <- FIESTA::pcheck.logical(exportsp, varnm="exportsp", 
		title="Export spatial?", first="NO", gui=gui)  


  ## Check overwrite, outfn.date, outfolder, outfn 
  ########################################################
  if (savedata || exportsp || exportNA) {
    outlst <- pcheck.output(out_dsn=out_dsn, out_fmt=out_fmt, 
		outfolder=outfolder, outfn.pre=outfn.pre, outfn.date=outfn.date, 
		overwrite_dsn=overwrite_dsn, gui=gui)
    out_dsn <- outlst$out_dsn
    outfolder <- outlst$outfolder
    out_fmt <- outlst$out_fmt
  }

  ##################################################################
  ## DO WORK
  ##################################################################

  #############################################################################
  ## 1) Extract values from dunit_layer
  #############################################################################
  dunitarea <- NULL
  if (!dunitvar %in% names(sppltx)) { 
      ## Extract values of polygon layer to points
    extpoly <- spExtractPoly(xyplt=sppltx, polyvlst=dunit_layerx, 
		uniqueid=uniqueid, polyvarlst=unique(c(dunitvar, vars2keep)), 
		keepNA=FALSE, exportNA=exportNA)
    sppltx <- unique(extpoly$spxyext)
  } else {
    message(dunitvar, " already in spplt... not extracting from dunit_layer")
  }

  #############################################################################
  ## 2) Set up outputs - dunitlut, prednames, inputdf, zonalnames
  #############################################################################
  dunitlut <- data.table(unique(sf::st_drop_geometry(dunit_layerx[, c(dunitvar, vars2keep),
 		drop=FALSE])))
  setkeyv(dunitlut, dunitvar)
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
		keepNA=keepNA, exportNA=exportNA, outfolder=outfolder, 
		overwrite_layer=overwrite_layer)
    sppltx <- unique(extdat.rast.cont$spplt)
    prednames.cont <- extdat.rast.cont$outnames
    inputdf.cont <- extdat.rast.cont$inputdf
    rm(extdat.rast.cont)
    gc() 

    if (NAto0) {
      for (col in prednames.cont) set(sppltx, which(is.na(sppltx[[col]])), col, 0)
    }

    ## Transform aspect 
    if (asptransform) {
      aspnm <- inputdf.cont$var.name[inputdf.cont$rasterfile == rast.aspfn]     
      sppltx$asp_cos <- northness(sppltx[[aspnm]])
      sppltx$asp_sin <- eastness(sppltx[[aspnm]])
      prednames.cont <- c(prednames.cont[prednames.cont != aspnm], "asp_cos", "asp_sin")
    }
    prednames <- c(prednames, prednames.cont)
    inputdf <- rbind(inputdf, inputdf.cont)
    zonalnames <- c(zonalnames, prednames)
 
    ## Extract zonal means from continuous raster layers
    #############################################################################
    zonalDT.cont <- data.table(DOMAIN = unique(dunit_layerx[[dunitvar]]))
    setnames(zonalDT.cont, "DOMAIN", dunitvar)
    setkeyv(zonalDT.cont, dunitvar)
    #zonalDT.cont.names <- {}

    for (i in 1:length(rastlst.contfn)) {
      rastfn <- rastlst.contfn[i]
      rastnm <- inputdf.cont$var.name[inputdf.cont$rasterfile == rastfn]
      rast.cont.NODATA <- rastlst.cont.NODATA[i]
      zonalstat <- rastlst.cont.stat 
      #message(rastfn, "...")
 
      if (asptransform && identical(rast.aspfn, rastfn)) {
        rastnm2 <- ifelse(is.null(rastnm), "asp_cos", paste0(rastnm, "_cos"))
        if (i == 1 && npixels) {
          zonalstat <- c("npixels", rastlst.cont.stat) 
          rastnm2 <- c("npixels", rastnm2)
        }  
        zonaldat.rast.cont <- spZonalRast(dunit_layerx, rastfn=rastfn, polyv.att=dunitvar, 
		zonalstat=zonalstat, pixelfun=northness, rast.NODATA=rast.cont.NODATA,
		na.rm=TRUE)
        zonalext <- setDT(zonaldat.rast.cont$zonalext)
        outname <- zonaldat.rast.cont$outname
        class(zonalext[[dunitvar]]) <- class(dunitlut[[dunitvar]])        

        if (!is.null(rastnm)) 
          setnames(zonalext, outname, rastnm2)
        setkeyv(zonalext, dunitvar)
        zonalDT.cont <- zonalDT.cont[zonalext] 
  
        rastnm2 <- ifelse(is.null(rastnm), "asp_sin", paste0(rastnm, "_sin"))
        zonalstat <- c(rastlst.cont.stat) 
        zonaldat.rast.cont <- spZonalRast(dunit_layerx, rastfn=rastfn, 
		rast.NODATA=rast.cont.NODATA, polyv.att=dunitvar, zonalstat=rastlst.cont.stat, 
		pixelfun=eastness, na.rm=TRUE)
        zonalext <- setDT(zonaldat.rast.cont$zonalext)
        outname <- zonaldat.rast.cont$outname
        class(zonalext[[dunitvar]]) <- class(dunitlut[[dunitvar]])        
        if (!is.null(rastnm2)) {
          setnames(zonalext, outname, rastnm2)
        }
        setkeyv(zonalext, dunitvar)
        zonalDT.cont <- zonalDT.cont[zonalext]
 
      } else {
        if (i == 1 && npixels) {
          zonalstat <- c("npixels", rastlst.cont.stat) 
          if (!is.null(rastnm)) {
            rastnm <- c("npixels", rastnm)
          }
        } 
        zonaldat.rast.cont <- spZonalRast(dunit_layerx, rastfn=rastfn, 
		rast.NODATA=rast.cont.NODATA, polyv.att=dunitvar, zonalstat=zonalstat, 
		showext=showext, na.rm=TRUE)
        zonalext <- setDT(zonaldat.rast.cont$zonalext)
        outname <- zonaldat.rast.cont$outname
        class(zonalext[[dunitvar]]) <- class(dunitlut[[dunitvar]])        
        if (!is.null(rastnm)) {
          setnames(zonalext, outname, rastnm)
        }
        setkeyv(zonalext, dunitvar)
        zonalDT.cont <- zonalDT.cont[zonalext] 
      }
      if (npixels) npixels <- FALSE
      rm(zonaldat.rast.cont)
      rm(zonalext)
      gc() 
    }
    dunitlut <- dunitlut[zonalDT.cont] 
  }
  ###############################################################################
  ## 4) Categorical raster layers - Extract values and get zonal probabilities
  ###############################################################################
  if (!is.null(rastlst.cat)) {

    ## Extract values from categorical raster layers
    ######################################################
    extdat.rast.cat <- spExtractRast(sppltx, uniqueid=uniqueid, rastlst=rastlst.catfn, 
		interpolate=FALSE, var.name=rastlst.cat.name, rast.NODATA=rastlst.cat.NODATA,
		keepNA=keepNA, exportNA=exportNA, outfolder=outfolder, 
		overwrite_layer=overwrite_layer)
    sppltx <- extdat.rast.cat$sppltext
    prednames.cat <- extdat.rast.cat$outnames
    inputdf.cat <- extdat.rast.cat$inputdf
    prednames <- c(prednames, prednames.cat)
    predfac <- c(predfac, prednames.cat)
    inputdf <- rbind(inputdf, inputdf.cat)
    rm(extdat.rast.cat)
    gc() 

    if (NAto0) {
      for (col in prednames.cat) set(sppltx, which(is.na(sppltx[[col]])), col, 0)
    }

    if (!is.null(rast.lut)) {
      rast.lutnm <- inputdf.cat$var.name[inputdf.cat$rasterfile == rast.lutfn]

      if (!rast.lutnm %in% names(rastlut)) {
        stop("must have variable named ", rast.lutnm, " in rastlut")
      }
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
    zonalDT.cat <- data.table(DOMAIN = unique(dunit_layerx[[dunitvar]]))
    setnames(zonalDT.cat, "DOMAIN", dunitvar)
    setkeyv(zonalDT.cat, dunitvar)
    for (i in 1:length(rastlst.catfn)) {
      rastfn <- rastlst.catfn[i]
      rastnm <- inputdf.cat[inputdf.cat$rasterfile == rastfn, "var.name"][[1]]
      #message(rastfn, "...")
      rast.cat.NODATA <- rastlst.cat.NODATA[i]

      zonalstat <- "proportion"
      if (i == 1 && npixels)
        zonalstat <- c("npixels", zonalstat)        
      if (identical(rast.lutfn, rastfn)) {
        zonaldat.rast.cat <- spZonalRast(dunit_layerx, rastfn=rastfn, rast.NODATA=rast.cat.NODATA, 
 		polyv.att=dunitvar, zonalstat=zonalstat, rastlut=rastlut, outname=names(rastlut)[2],
		na.rm=TRUE)
      } else {
        zonaldat.rast.cat <- spZonalRast(dunit_layerx, rastfn=rastfn, rast.NODATA=rast.cat.NODATA, 
 		polyv.att=dunitvar, outname=rastnm, zonalstat=zonalstat, na.rm=TRUE)
      }

      zonalext <- setDT(zonaldat.rast.cat$zonalext)
      outname <- zonaldat.rast.cat$outname
      outname[grep("npixels", outname)] <- "npixels"
      setnames(zonalext, c(dunitvar, outname))
      class(zonalext[[dunitvar]]) <- class(dunitlut[[dunitvar]])        
      setkeyv(zonalext, dunitvar)
      zonalDT.cat <- zonalDT.cat[zonalext] 
      zonalnames <- c(zonalnames, outname[outname != "npixels"])

      ## Pivot zonalext table to generate stratalut
      stratalut <- strat.pivot(zonalext, rastnm, unitvars=dunitvar, strwtvar="strwt")

      if (npixels) npixels <- FALSE
      rm(zonaldat.rast.cat)
      rm(zonalext)
      gc() 
    }
    tabs <- check.matchclass(dunitlut, zonalDT.cat, dunitvar)
    dunitlut <- tabs$tab1
    zonalDT.cat <- tabs$tab2

    dunitlut <- dunitlut[zonalDT.cat]  
  }
 
  ###################################################################################
  ## Get totacres from domain polygons (if areacalc = TRUE)
  ###################################################################################
  if (areacalc) {
    dunit_layerx <- areacalc.poly(dunit_layerx, unit=areaunits)
    areavar <- paste0(areaunits, "_GIS")

    dunitarea <- dunit_layerx[, c(dunitvar, areavar)]
    dunitarea <- aggregate(dunitarea[[areavar]], list(dunitarea[[dunitvar]]), sum)
    names(dunitarea) <- c(dunitvar, areavar)
  }

  ## Write data frames to CSV files
  #######################################
  pltassgn <- sf::st_drop_geometry(sppltx)
  if (savedata) {
    datExportData(pltassgn, outfolder=outfolder, out_fmt=out_fmt, 
		out_dsn=out_dsn, out_layer="pltassgn", 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer,
		add_layer=TRUE, append_layer=append_layer)
    datExportData(dunitlut, outfolder=outfolder, out_fmt=out_fmt, 
		out_dsn=out_dsn, out_layer="dunitlut", 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer,
		add_layer=TRUE, append_layer=append_layer)
  }
    

  returnlst <- list(pltassgn=pltassgn, dunitlut=setDF(dunitlut), dunitvar=dunitvar,
		inputdf=inputdf, prednames=prednames, zonalnames=zonalnames, 
		predfac=predfac, npixelvar="npixels", pltassgnid=uniqueid)
  if (areacalc) {
    returnlst$dunitarea <- dunitarea
    returnlst$areavar <- areavar
  }
  if (strata) {
    stratalut <- merge(stratalut, dunitlut[, c(dunitvar, vars2keep, "npixels")], by=dunitvar)
    returnlst$stratalut <- stratalut
    returnlst$strwtvar <- "strwt"
    returnlst$strvar <- rastnm
  }

  ## Returnxy
  if (returnxy) {
    ## Add coordinate variables
    #xyplt <- data.frame(sf::st_coordinates(sppltx))
    #names(xy.coords) <- c(x,y)
    #sppltx <- sf::st_sf(data.frame(sppltx, xy.coords)) 
    returnlst$spxy <- sppltx[, sppltx.names]
    returnlst[["xy.uniqueid"]] <- uniqueid
  }

 
  return(returnlst)
}

