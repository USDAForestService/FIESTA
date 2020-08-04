spExtractRast <- function(xyplt, xyplt_dsn=NULL, uniqueid="PLT_CN", rastlst, 
 	rastfolder=NULL, rast.crs=NULL, bandlst=NULL, var.name=NULL, interpolate=FALSE, 
 	windowsize=1, windowstat=NULL, rast.NODATA=NULL, keepNA=TRUE, showext=FALSE, 
	savedata=FALSE, exportsp=FALSE, exportNA=FALSE, outfolder=NULL, out_fmt="shp", 
	out_dsn=NULL, out_layer="rastext", outfn.pre=NULL, outfn.date=TRUE, 
	overwrite=FALSE, ...){
  #####################################################################################
  ## DESCRIPTION: 
  ## Extracts values from one or more raster layers and appends to input spatial layer 
  ## or data frame. Points are reprojected on-the-fly to projection of rasters using
  ## PROJ.4 transformation parameters and rgdal spTransform function. Includes options
  ## to use bilinear interpolation or summarize over a window of n pixels using
  ## a specified statistic.
  #####################################################################################

  ## Check for necessary packages
  ###########################################################
  if (!"sf" %in% rownames(installed.packages()))
    stop("sf package is required for spExtractRast()")
  
  if (!"rgdal" %in% rownames(installed.packages()))
    stop("rgdal package is required for spExtractRast()")


  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- ifelse(nargs() == 0, TRUE, FALSE)
  if (gui) xyplt=bfun=focalrast=ffun=focalsave=extrtype <- NULL

  ##################################################################
  ## CHECK INPUT PARAMETERS
  ##################################################################

  ## Spatial points for data extraction.. 
  ##################################################################################
  sppltx <- pcheck.table(tab=xyplt, tab_dsn=xyplt_dsn, tabnm="xyplt", 
			caption="XY coordinates?", stopifnull=TRUE)
 
  if (!"sf" %in% class(sppltx)) { 
    ## Create spatial object from xyplt coordinates
    sppltx <- spMakeSpatialPoints(xyplt=sppltx, uniqueid=uniqueid, 
		exportsp=FALSE, ...)
  } else {
    ## GET uniqueid
    sppltnames <- names(sppltx)
    uniqueid <- FIESTA::pcheck.varchar(var2check=uniqueid, varnm="uniqueid", gui=gui, 
		checklst=sppltnames, caption="UniqueID of spplt", 
		warn=paste(uniqueid, "not in spplt"), stopifnull=TRUE)
  }
 
  ## Check showext    
  showext <- FIESTA::pcheck.logical(showext, varnm="showext", 
		title="Plot extents?", first="YES", gui=gui)
 
  ## Check savedata 
  savedata <- FIESTA::pcheck.logical(savedata, varnm="savedata", 
		title="Save data extraction?", first="NO", gui=gui)  

  ## Check exportsp 
  exportsp <- FIESTA::pcheck.logical(exportsp, varnm="exportsp", 
		title="Export spatial?", first="NO", gui=gui)  

  ## Check exportNA 
  exportNA <- FIESTA::pcheck.logical(exportNA, varnm="exportNA", 
		title="Export NA values?", first="NO", gui=gui)  


  ## Check overwrite, outfn.date, outfolder, outfn 
  ########################################################
  if (savedata || exportsp || exportNA) {
    overwrite <- FIESTA::pcheck.logical(overwrite, varnm="overwrite", 
		title="Overwrite files?", first="NO", gui=gui)  
    outfn.date <- FIESTA::pcheck.logical(outfn.date , varnm="outfn.date", 
		title="Add date to outfiles?", first="YES", gui=gui)  
    outfolder <- FIESTA::pcheck.outfolder(outfolder, gui)
  }
  
  ## Verify rasters
  ########################################################
  rastfnlst <- getrastlst.rgdal(rastlst, rastfolder, gui=gui)
  #if (any(rastfnlst == "")) stop("must write raster to file")
  nrasts <- length(rastfnlst)


  ## Get names of rasters
  rastnmlst <- lapply(rastfnlst, basename.NoExt)

  ## Get number of bands in each raster and set names
  nbandlist <- lapply(rastfnlst, function(x) rasterInfo(x)$nbands)
  names(nbandlist) <- rastnmlst
  nrastbands <- length(unlist(nbandlist))
  
  ## Check bands
  if (!is.null(bandlst)) {
    FIESTA::check.namedlist("bandlst", checknms=rastnmlst)

    # Check if all values are within range of actual bands 
    bandlist <- list()
    for(i in 1:length(rastnmlst)) {
      nm <- rastnmlst[[i]]
      if (nm %in% names(bandlst)) {
        if (any(bandlst[[nm]] > bandlst[[nm]])) {
          stop(paste("invalid bands: ", nm))
        } else {
          bandlist[nm] <- sapply(bandlst[nm], list)
        }
      } else {
        bandlist[[nm]] <- 1
      }      
    }
  } else {
    bandlist <- lapply(nbandlist, function(x) seq(1,x))
  }

  ## Number of layers
  band <- as.vector(unlist(bandlist)) 
  nlayers <- length(band)

  ## Get rasterfiles
  rasterfile <- rep(rastfnlst, unlist(lapply(bandlist, length)))

  ## Check rast.NODATA
  if (!is.null(rast.NODATA)) {
    if (!is.numeric(rast.NODATA))
      stop("rast.NODATA must be numeric")
    if (length(rast.NODATA) != nlayers) {
      if (length(rast.NODATA) == 1) {
        message(rast.NODATA, "used as NODATA value all raster layers")
        rast.NODATA <- rep(rast.NODATA, nlayers)
      } else if (length(rast.NODATA) == nrasts) {
        rast.NODATA <- rep(rast.NODATA, unlist(lapply(bandlist, length)))
        message("using same NODATA value for multiple bands")
      } else {
        stop("number of NODATA values does not match number of raster layers")
      }
    }
  } else {
    rast.NODATA  <- rep(NA, nlayers)
  }    

  ## Check var.name
  if (!is.null(var.name)) {
    if (!is.character(var.name)) 
      stop("var.name must be a character vector")
    if (length(var.name) != nlayers)
      stop("number of var.name must match ", nlayers, " layers")
  } else {
    var.name <- unlist(lapply(names(nbandlist), function(x, nbandlist) {
		if (nbandlist[[x]] == 1) { 
             return(x) 
		} else { 
		  return(paste(x, 1:nbandlist[[x]], sep="_")) 
		} }, nbandlist))
  }

  ## Check interpolate
  if (!is.null(interpolate)) {
    if (!is.logical(interpolate)) 
      stop("interpolate is invalid... must be logical, TRUE or FALSE")
    if (length(interpolate) != nlayers) {
      if (length(interpolate) == 1) {
        if (interpolate) {
          message("interpolation used for all raster layers")
        } else {
          message("no interpolation used for any raster layers")
        }
        interpolate <- rep(interpolate, nlayers)
      } else if (length(interpolate) == nrasts) {
        interpolate <- rep(interpolate, unlist(lapply(bandlist, length)))
        message("using same interpolation method for multiple bands")
      } else {
        stop("number of interpolation methods does not match number of raster layers")
      }
    }
  } else {
    interpolate <- rep(FALSE, nlayers)
    message("no interpolation used for data extraction")
  }    

  ## Check windowsize
  if (!is.null(windowsize)) {
    if (!is.numeric(windowsize)) stop("windowsize must be numeric")
    if (length(windowsize) != nlayers) {
      if (length(windowsize) == 1) {
        message(paste("pixel windowsize of", windowsize, "will be used for all raster layers"))
        windowsize <- rep(windowsize, nlayers)
      } else if (length(windowsize) == nrasts) {
        windowsize <- rep(windowsize, unlist(lapply(bandlist, length)))
        message("using same windowsize for multiple bands")
      } else {
        stop("number of windowsize values does not match number of raster layers")
      }
    }
  } else {
    message("using 1 pixel windowsize for data extraction")
    windowsize <- rep(1, nlayers)
  }
     
  statistic <- rep("none", nlayers)
  if (any(windowsize > 1)) {
    ngt1 <- sum(windowsize > 1)
    
    ## Check windowstat
    if (!is.null(windowstat)) {
      statlst <- c("mean", "min", "max", "median", "sum", "range", "var",
		"sd", "rsd", "mode", "value")
      if (!all(windowstat %in% statlst)) 
        stop("windowstat is invalid... must be in", paste(statlst, collapse=", "))
      if (length(windowstat) != nlayers) {
        if (length(windowstat) == 1 && ngt1 > 1) {
          message(paste(windowstat, "statistic will be used for each raster layer"))
          statistic[windowsize > 1] <- rep(windowstat, ngt1)
        } else if (length(windowstat) == ngt1) {
          statistic[windowsize > 1] <- windowstat
        } else {
          stop("number of statistics does not match number of windowsize greater than 1")
        }
      }
    }
  }

  inputs <- data.table(rasterfile, band, var.name, interpolate, windowsize, statistic,
			rast.NODATA)
  print(inputs)

  ########################################################################
  ### DO THE WORK
  ########################################################################  
  outnames <- {}  
  NAlst <- list() 
  for (i in 1:nrasts) {	## loop through rasters

    rastfn <- rastfnlst[[i]]
    rastnm <- FIESTA::basename.NoExt(rastfn)
    rast.prj <- rasterInfo(rastfn)$crs
    rast.bbox <- rasterInfo(rastfn)$bbox

    ## Check projection and reproject sppltx if different than rast
    sppltprj <- crsCompare(sppltx, rast.prj, crs.default=rast.crs)$x
    
    ## Subset Spatial data frame to just id, x, y
    sppltxy <- data.frame(sppltprj[[uniqueid]], sf::st_coordinates(sppltprj))
    names(sppltxy)[1] <- uniqueid

    ## Check extents
    names(rast.bbox) <- c("xmin", "ymin", "xmax", "ymax")
    bbox1 <- sf::st_bbox(rast.bbox, crs=rast.prj)
    bbox2 <- sf::st_bbox(sppltprj)
    check.extents(bbox1, bbox2, showext=showext, layer1nm=rastnm, layer2nm="xyplt",
			stopifnotin=TRUE)
           
    ## Extract values
    ########################################################          
    message(paste("extracting point values from", rastnm, "...")) 
    inputs.rast <- inputs[rasterfile == rastfn,]

    for (j in 1:nrow(inputs.rast)) {	## loop through raster bands

      band <- inputs.rast[j, band]
      if (nrow(inputs.rast) > 1) message(paste("band", band, "...")) 

      var.name <- inputs.rast[j, var.name]
      interpolate <- inputs.rast[j, interpolate]
      windowsize <- inputs.rast[j, windowsize]
      statistic <- inputs.rast[j, statistic] 
      if (statistic == "value") statistic <- NULL
      rast.NODATA <- inputs.rast[j, rast.NODATA]

      dat <- suppressWarnings(extractPtsFromRaster(ptdata=sppltxy, 
			rasterfile=rastfn, band=band, var.name=var.name, 
			interpolate=interpolate, windowsize=windowsize, statistic=statistic))
      cname <- names(dat)[2]
      outnames <- c(outnames, cname)

      if ("data.table" %in% class(sppltx)) stop("xyplt cannot be sf data.table")
      sppltx <- merge(sppltx, dat, by.x=uniqueid, by.y="pid")

      if (!is.na(rast.NODATA))
        sppltx[sppltx[[cname]] == rast.NODATA, cname] <- NA

      #print(table(sppltx[[cname]]))

      ## Print missing values to screen
      navals <- sum(is.na(sppltx[[cname]]))
      if (navals > 0) {
        NAlst[[var.name]] <- navals
        message(paste(navals, "missing values for", rastnm))
        if (exportNA) {
          sppltna <- sppltx[is.na(sppltx[[cname]]),]
          outfn.na <- paste(var.name, "na", sep="_")
          spExportSpatial(sppltna, out_layer=outfn.na, outfolder=outfolder, 
			out_dsn=out_dsn, outfn.pre=outfn.pre, outfn.date=outfn.date,
 			overwrite_layer=overwrite)
        }
      }

      if (!keepNA && sum(is.na(sppltx[[cname]])) > 0) {
        message("removing missing values from input dataset")
        sppltx <- sppltx[!is.na(sppltx[[cname]]),]
      }
    }
  }

  if (savedata)
    write2csv(sppltx, outfolder=outfolder, outfilenm=out_layer, outfn.pre=outfn.pre,
 		outfn.date=outfn.date, overwrite=overwrite)

  if (exportsp) 
    spExportSpatial(sppltx, out_layer=out_layer, outfolder=outfolder, 
		outfn.pre=outfn.pre, outfn.date=outfn.date, overwrite_layer=overwrite)

  returnlst <- list(sppltext=sppltx, outnames=outnames, rastfnlst=rastfnlst, 
				inputdf=inputs)

  if (length(NAlst) > 0) 
    returnlst$NAlst <- NAlst

  return(returnlst)
}
