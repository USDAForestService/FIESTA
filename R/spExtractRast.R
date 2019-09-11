spExtractRast <- function(spplt=NULL, spplt_dsn=NULL, xyplt=NULL, uniqueid="PLT_CN",
 	rastlst=NULL, rastfolder=NULL, rast.prjstr=NULL, bandlst=NULL, var.name=NULL,
 	interpolate=FALSE, windowsize=1, windowstat=NULL, keepnull=TRUE, showext=FALSE,
 	savedata=FALSE, exportshp=FALSE, exportna=FALSE, outfolder=NULL, outfn=NULL,
 	outfn.date=TRUE, overwrite=FALSE, ...){
  #####################################################################################
  ## DESCRIPTION: 
  ## Extracts values from one or more raster layers and appends to input spatial layer 
  ## or data frame. Points are reprojected on-the-fly to projection of rasters using
  ## PROJ.4 transformation parameters and rgdal spTransform function. Includes options
  ## to use bilinear interpolation or summarize over a window of n pixels using
  ## a specified statistic.
  #####################################################################################

  if (!"rgeos" %in% rownames(installed.packages()))
    stop("spExtractRast function requires package rgeos")

  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- ifelse(nargs() == 0, TRUE, FALSE)

  if (gui) shp=xytable=bfun=focalrast=ffun=focalsave=extrtype <- NULL

  ##################################################################
  ## CHECK INPUT PARAMETERS
  ##################################################################

  ### Spatial Points: shp or xytable for data extraction.. 
  ##################################################################################
  if (is.null(spplt)) {

    ## Check parameters
    if (is.null(xyplt)) stop("either include spplt or xyplt for data extraction")
         
    ## Create spatial object from xyplt coordinates
    sppltx <- FIESTA::spMakeSpatialPoints(xyplt=xyplt, uniqueid=uniqueid, ...)
  } else {
 
    ## Check spplt
    sppltx <- FIESTA::pcheck.spatial(layer=spplt, dsn=spplt_dsn, gui=gui,
		caption="Spatial points with XY coords?")
    if (is.null(sp::proj4string(sppltx))) stop("spplt must have defined projection")

    ## GET uniqueid
    sppltnames <- names(sppltx@data)
    uniqueid <- FIESTA::pcheck.varchar(var2check=uniqueid, varnm="uniqueid", gui=gui, 
		checklst=sppltnames, caption="UniqueID of spplt", 
		warn=paste(uniqueid, "not in spplt"), stopifnull=TRUE)

    ## Check for NA or duplicate values in uniqueid
    if (sum(is.na(sppltx@data[[uniqueid]])) > 0) stop("NA values in ", uniqueid)
    if (length(unique(sppltx@data[[uniqueid]])) < nrow(sppltx@data)) 
      stop("spplt records are not unique")
  }
 
   ## Check showext    
  showext <- FIESTA::pcheck.logical(showext, varnm="showext", 
		title="Plot extents?", first="YES", gui=gui)
 
  ## Check savedata 
  savedata <- FIESTA::pcheck.logical(savedata, varnm="savedata", 
		title="Save data extraction?", first="NO", gui=gui)  

  ## Check outfolder 
  ########################################################
  if (savedata || exportshp) {
    ## Check overwrite 
    overwrite <- FIESTA::pcheck.logical(overwrite, varnm="overwrite", 
		title="Overwrite files?", first="NO", gui=gui)  

    outfolder <- FIESTA::pcheck.outfolder(outfolder, gui)

    if (is.null(outfn)) {
      outfn <- "dataext"
    } else if (!is.character(outfn)) {
      stop("outfn must be character")
    }      
  }
  
  ## Verify rasters
  ########################################################
  rastfnlst <- getrastlst(rastlst, rastfolder, filenames=TRUE, gui=gui)
  if (any(rastfnlst == "")) stop("must write raster to file")
  nrasts <- length(rastfnlst)


  ## Get names of rasters
  rastnmlst <- lapply(rastfnlst, 
	function(x) sub(raster::extension(basename(x)), "", basename(x)))

  ## Import rasters
  rasterlst <- lapply(rastfnlst, raster::raster)

  ## Get number of bands in each raster and set names
  nbandlist <- lapply(rasterlst, nbands)
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
        interptxt <- ifelse(interpolate, "be used", "not be used")
        message(paste("interpolation will", interptxt, "for all raster layers"))
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

  inputs <- data.table(rasterfile, band, var.name, interpolate, windowsize, statistic)
  print(inputs)

  ## Check raster projections
  #raster.prj <- lapply(rasterlst, sp::proj4string)

  ########################################################################
  ### DO THE WORK
  ########################################################################  
  outnames <- {}      
  for (i in 1:nrasts) {

    rastfn <- rastfnlst[[i]]
    rast <- raster(rastfn)
    rastnm <- FIESTA::basename.NoExt(rastfn)

    ## Check projection
    prjdat <- CRScompare(rast, sppltx, prj4str=rast.prjstr)
    rast <- prjdat$layer1
    sppltprj <- prjdat$layer2
    
    ## Subset Spatial data frame to just id, x, y
    sppltxy <- data.frame(sppltprj[[uniqueid]], coordinates(sppltprj))
    names(sppltxy) <- c("id", "x", "y")

    ## Check extents
    msg <- FIESTA::check.extents(rast, sppltprj, showext, layer1nm=rastnm, 
			layer2nm="spplt")
    if (msg == "non-overlapping extents") stop("msg")

           
    ## Extract values
    ########################################################  
        
    message(paste("extracting point values from", rastnm, "...")) 
    inputs.rast <- inputs[rasterfile == rastfn,]

    for (j in 1:nrow(inputs.rast)) {

      band <- inputs.rast[j, band]
      if (nrow(inputs.rast) > 1) message(paste("band", band, "...")) 

      var.name <- inputs.rast[j, var.name]
      interpolate <- inputs.rast[j, interpolate]
      windowsize <- inputs.rast[j, windowsize]
      statistic <- inputs.rast[j, statistic] 
      if (statistic == "value") statistic <- NULL

      dat <- setDT(extractPtsFromRaster(ptdata=sppltxy, rasterfile=rastfn, band=band, 
			var.name=var.name, interpolate=interpolate, windowsize=windowsize, 
			statistic=statistic))
      cname <- names(dat)[2]
      outnames <- c(outnames, cname)
      sppltx <- sp::merge(sppltx, dat, by.x=uniqueid, by.y="pid", all.x=TRUE)

      ## Print missing values to screen
      navals <- sum(is.na(dat[[2]]))
      if (navals > 0) {
        message(paste(navals, "missing values for", rastnm))
        if (exportna) {
          sppltna <- sppltx[is.na(sppltx[[cname]]),]
          outfn.na <- paste(basename(rastfn), "na", sep="_")
          spExportShape(sppltna, outshpnm=outfn.na, outfolder=outfolder, 
			outfn.date=outfn.date, overwrite=overwrite, uniqueid=uniqueid)
        }
      }

      if (!keepnull && sum(is.na(sppltx[[cname]])) > 0) {
        message("removing missing values from input dataset")
        sppltx <- sppltx[!is.na(sppltx[[cname]]),]
      }
    }
  }

  if (savedata)
    write2csv(sppltx@data, outfolder=outfolder, outfilenm=outfn, outfn.date=outfn.date,
		overwrite=overwrite)

  if (exportshp) 
    spExportShape(sppltx, outshpnm=outfn, outfolder=outfolder, outfn.date=outfn.date,
		overwrite=overwrite, uniqueid=uniqueid)

  returnlst <- list(spplt=sppltx, pltdat=sppltx@data, outnames=outnames,
		rastfnlst=rastfnlst, inputdf=inputs)
  return(returnlst)
}
