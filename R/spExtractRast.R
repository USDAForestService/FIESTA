#' Spatial - Extracts point attribute values from raster layer(s).
#' 
#' Extracts values from one or more raster layers and appends to input
#' SpatialPoints layer or data frame. Points are reprojected on-the-fly to
#' projection of raster(s) using PROJ.4 transformation parameters and sf
#' spTransform function. Includes options to use bilinear interpolation or
#' summarize over a window of n pixels using a specified statistic.
#' 
#' *If variable = NULL, then it will prompt user for input.
#' 
#' @param xyplt Data frame object or String. Name of layer with xy coordinates
#' and unique identifier. Can be layer with xy_dsn, full pathname, including
#' extension, or file name (with extension) in xy_dsn folder.
#' @param xyplt_dsn String. Name of database where xyplt is. The dsn varies by
#' driver. See gdal OGR vector formats (https://www.gdal.org/ogr_formats.html).
#' @param xy.uniqueid String. Unique identifier of xyplt rows.
#' @param rastlst String vector or list or strings and/or rasters. File name(s)
#' with extensions, or raster object(s). Note: raster objects must be written
#' to file.
#' @param rastfolder String. Name of the folder with raster layers. Optional.
#' Useful if all raster layers are in same folder.
#' @param rast.crs EPSG code or PROJ.4 String. Name of coordinate reference
#' system for rasters with no projection defined. If more than one raster has
#' no projection defined, the same crs will be used.
#' @param bandlst Numeric named list. If rastfnlst includes a multi-layer
#' raster and only 1 or some layers are desired, specify layer numbers in a
#' named list format with names matching the base names in rastfnlst (e.g.,
#' list(rast1=5, rast3=1:3)). If NULL, all layers are extracted.
#' @param var.name String vector. Extracted variable name(s). If NULL, uses the
#' basename of raster layer, including band number for multi-band rasters.
#' @param interpolate Logical vector. If TRUE, uses bilinear interpolation of
#' pixel values, weighted average of 4 nearest pixels (i.e., continuous data).
#' @param windowsize Number vector. The size of window for summarizing data.
#' @param windowstat Character vector. If windowsize is greater than one, the
#' statistic to use for summarizing data ("mean", "min", "max", "median",
#' "sum", "range", "var", "sd", "rsd", "mode", "value"). If windowstat="value",
#' all pixel values are returned, otherwise 1 value per row in xyplt is
#' returned.
#' @param rast.NODATA Numeric vector. NODATA value(s) of raster if not
#' predefined (See notes below). This value will be converted to NA and removed
#' if keepNA=FALSE.  If rastfnlst includes more than one raster, the
#' rast.NODATA value should coincide with number of rasters in rastfnlst. If
#' only one rast.NODATA, the same NODATA value is used for all rasters.
#' @param keepNA Logical. If TRUE, keeps NA values after data extraction.
#' @param ncores Integer. Number of cores to use for extracting values. 
#' @param showext Logical. If TRUE, layer extents are displayed in plot window.
#' @param savedata Logical. If TRUE, the input data with extracted values are
#' saved to outfolder.
#' @param exportsp Logical. If TRUE, the extracted raster point data are
#' exported to outfolder.
#' @param exportNA Logical. If TRUE, NA values are exported to outfolder.
#' @param spMakeSpatial_opts List. See help(spMakeSpatial_options()) for a list
#' of options. Use to convert X/Y values to simple feature (sf) coordinates.
#' @param savedata_opts List. See help(savedata_options()) for a list
#' of options. Only used when savedata = TRUE. If out_layer = NULL,
#' default = 'rastext'.
#' @param gui Logical. If gui, user is prompted for parameters.
#'
#' @return \item{sppltext}{ sf object or data frame. Input xyplt data with
#' extracted raster values appended. } \item{outnames}{ String vector. Raster
#' output names. } \item{rastfnlst}{ String vector. Raster pathnames. }
#' \item{inputdf}{ Data frame. Raster information input to zonal summaries. }
#' \item{NAlst}{ sf List. If NA values exist after data extraction, the spatial
#' NA points are returned. }
#' 
#' If savedata=TRUE, pltassgn and unitarea are saved to outfolder.\cr If
#' exportsp=TRUE, the spatial sf points object is exported to outfolder.\cr.
#' If exportNA=TRUE and NA values exist after data extraction, the spatial NA
#' points are exported to outfolder.
#' @note
#' 
#' rast.NODATA\cr NODATA values are raster pixel values that have no data of
#' interest, including pixels within the extent of the layer, but outside the
#' area of interest. Sometimes these pixels have been defined previously. The
#' defined NODATA pixels are imported to R as NULL values. When not previously
#' defined, the pixels outside the area of interest will be the minimum or
#' maximum value depending on the data type (e.g., 16-bit signed: min=-32,768;
#' max=32,768) or byte size (1 byte: min=0; max=255).  These NODATA values will
#' be added to the zonal statistic calculations if not specified in
#' rast.NODATA.
#' 
#' The spTransform (sf) method is used for on-the-fly map projection
#' conversion and datum transformation using PROJ.4 arguments. Datum
#' transformation only occurs if the +datum tag is present in the both the from
#' and to PROJ.4 strings. The +towgs84 tag is used when no datum transformation
#' is needed. PROJ.4 transformations assume NAD83 and WGS84 are identical
#' unless other transformation parameters are specified.  Be aware, providing
#' inaccurate or incomplete CRS information may lead to erroneous data shifts
#' when reprojecting. See spTransform help documentation for more details.
#' @author Tracey S. Frescino
#' @keywords data
#' @examples
#' # Get point data from WYplt data in FIESTA
#' WYplt <- FIESTA::WYplt
#' 
#' # Get raster layers from FIESTA external data
#' fornffn <- system.file("extdata",
#'                        "sp_data/WYbighorn_forest_nonforest_250m.tif",
#'                        package = "FIESTA")
#' demfn <- system.file("extdata",
#'                      "sp_data/WYbighorn_dem_250m.img",
#'                      package = "FIESTA")
#' 
#' # Extract points from raster
#' xyext <- spExtractRast(xyplt = WYplt,
#'                        rastlst = c(fornffn, demfn),
#'                        var.name = c("fornf", "dem"),
#'                        xy.uniqueid = "CN",
#'                        spMakeSpatial_opts = list(xvar = "LON_PUBLIC",
#'                                                  yvar = "LAT_PUBLIC",
#'                                                  xy.crs = 4269))
#' names(xyext)
#' xyext$outnames
#' sppltext <- xyext$sppltext
#' head(sppltext)
#' xyext$inputdf
#' 
#' # Plot extracted values of forest/nonforest
#' plot(sppltext["fornf"])
#' 
#' # Plot extracted values of dem (i.e., elevation)
#' plot(sppltext["dem"])
#' @export spExtractRast
spExtractRast <- function(xyplt, 
                          xyplt_dsn = NULL, 
                          xy.uniqueid = "PLT_CN", 
                          rastlst, 
                          rastfolder = NULL, 
                          rast.crs = NULL, 
                          bandlst = NULL, 
                          var.name = NULL, 
                          interpolate = FALSE, 
                          windowsize = 1, 
                          windowstat = NULL, 
                          rast.NODATA = NULL, 
                          keepNA = TRUE, 
                          ncores = 1,
                          showext = FALSE, 
                          savedata = FALSE, 
                          exportsp = FALSE, 
                          exportNA = FALSE, 
                          spMakeSpatial_opts = NULL,
                          savedata_opts = NULL, 
                          gui = FALSE){
  #####################################################################################
  ## DESCRIPTION: 
  ## Extracts values from one or more raster layers and appends to input spatial layer 
  ## or data frame. Points are reprojected on-the-fly to projection of rasters using
  ## PROJ.4 transformation parameters and sf spTransform function. Includes options
  ## to use bilinear interpolation or summarize over a window of n pixels using
  ## a specified statistic.
  #####################################################################################

  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- ifelse(nargs() == 0, TRUE, FALSE)
  if (gui) xyplt=bfun=focalrast=ffun=focalsave=extrtype <- NULL

  
  ##################################################################
  ## CHECK PARAMETER NAMES
  ##################################################################
  
  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- names(formals(spExtractRast))
  if (!all(input.params %in% formallst)) {
    miss <- input.params[!input.params %in% formallst]
    stop("invalid parameter: ", toString(miss))
  }
  
  ## Check parameter lists
  pcheck.params(input.params, savedata_opts=savedata_opts)

  ## Check parameter lists
  pcheck.params(input.params, spMakeSpatial_opts=spMakeSpatial_opts, savedata_opts=savedata_opts)

  
  
  ## Set spMakeSpatial defaults
  spMakeSpatial_defaults_list <- formals(spMakeSpatial_options)[-length(formals(spMakeSpatial_options))]
  
  for (i in 1:length(spMakeSpatial_defaults_list)) {
    assign(names(spMakeSpatial_defaults_list)[[i]], spMakeSpatial_defaults_list[[i]])
  }
  
  ## Set user-supplied spMakeSpatial values
  if (length(spMakeSpatial_opts) > 0) {
    for (i in 1:length(spMakeSpatial_opts)) {
      if (names(spMakeSpatial_opts)[[i]] %in% names(spMakeSpatial_defaults_list)) {
        assign(names(spMakeSpatial_opts)[[i]], spMakeSpatial_opts[[i]])
      } else {
        stop(paste("Invalid parameter: ", names(spMakeSpatial_opts)[[i]]))
      }
    }
  }
  
  ## Set savedata defaults
  savedata_defaults_list <- formals(savedata_options)[-length(formals(savedata_options))]
  
  for (i in 1:length(savedata_defaults_list)) {
    assign(names(savedata_defaults_list)[[i]], savedata_defaults_list[[i]])
  }
  
  ## Set user-supplied savedata values
  if (length(savedata_opts) > 0) {
    if (!savedata) {
      message("savedata=FALSE with savedata parameters... no data are saved")
    }
    for (i in 1:length(savedata_opts)) {
      if (names(savedata_opts)[[i]] %in% names(savedata_defaults_list)) {
        assign(names(savedata_opts)[[i]], savedata_opts[[i]])
      } else {
        stop(paste("Invalid parameter: ", names(savedata_opts)[[i]]))
      }
    }
  }

  ##################################################################
  ## CHECK PARAMETER INPUTS
  ##################################################################  
  
  ## Spatial points for data extraction.. 
  ##################################################################################
  sppltx <- pcheck.table(tab=xyplt, tab_dsn=xyplt_dsn, tabnm="xyplt", 
			caption="XY coordinates?", stopifnull=TRUE)
 
  if (!"sf" %in% class(sppltx)) { 
    ## Create spatial object from xyplt coordinates
    sppltx <- spMakeSpatialPoints(xyplt=sppltx, 
                                  xy.uniqueid=xy.uniqueid, 
                                  xvar=xvar, 
                                  yvar=yvar,
                                  xy.crs=xy.crs,
                                  prj=prj,
                                  datum=datum,
                                  zone=zone,
                                  zoneS=zoneS,
                                  aea.param=aea.param)
  } else {
    ## GET uniqueid
    sppltnames <- names(sppltx)
    xy.uniqueid <- pcheck.varchar(var2check=xy.uniqueid, varnm="xy.uniqueid", gui=gui, 
		checklst=sppltnames, caption="UniqueID of spplt", 
		warn=paste(xy.uniqueid, "not in spplt"), stopifnull=TRUE)
  }
 
  ## Check showext    
  showext <- pcheck.logical(showext, varnm="showext", 
		title="Plot extents?", first="YES", gui=gui)
 
  ## Check savedata 
  savedata <- pcheck.logical(savedata, varnm="savedata", 
		title="Save data extraction?", first="NO", gui=gui)  

  ## Check exportsp 
  exportsp <- pcheck.logical(exportsp, varnm="exportsp", 
		title="Export spatial?", first="NO", gui=gui)  

  ## Check exportNA 
  exportNA <- pcheck.logical(exportNA, varnm="exportNA", 
		title="Export NA values?", first="NO", gui=gui)  


  ## Check overwrite, outfn.date, outfolder, outfn 
  ########################################################
  if (savedata || exportsp || exportNA) {
    outlst <- pcheck.output(outfolder=outfolder, out_dsn=out_dsn, 
            out_fmt=out_fmt, outfn.pre=outfn.pre, outfn.date=outfn.date, 
            overwrite_dsn=overwrite_dsn, overwrite_layer=overwrite_layer,
            add_layer=add_layer, append_layer=append_layer, gui=gui)
    outfolder <- outlst$outfolder
    out_dsn <- outlst$out_dsn
    out_fmt <- outlst$out_fmt
    overwrite_layer <- outlst$overwrite_layer
    append_layer <- outlst$append_layer
    outfn.date <- outlst$outfn.date
    outfn.pre <- outlst$outfn.pre
    if (is.null(out_layer)) {
      out_layer <- "rastext"
    }
  }
 
  ## Verify rasters
  ########################################################
  rastfnlst <- suppressWarnings(getrastlst.rgdal(rastlst, rastfolder, gui=gui))
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
    check.namedlist("bandlst", checknms=rastnmlst)

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
    if (!is.numeric(rast.NODATA)) {
      stop("rast.NODATA must be numeric")
    }
    if (!is.list(rast.NODATA)) {
      rast.NODATA <- list(rast.NODATA)
    }

    if (length(rast.NODATA) != nlayers) {
      if (length(rast.NODATA) == 1) {
        message(rast.NODATA, "used as NODATA value all raster layers")
        rast.NODATA <- rep(rast.NODATA, nlayers)
      } else if (length(rast.NODATA) == nrasts) {
        rast.NODATA <- rep(rast.NODATA, unlist(lapply(bandlist, length)))
        #message("using same NODATA value for multiple bands")
      } 
    }
  } else {
    rast.NODATA  <- rep(NA, nlayers)
  }    

  ## Check var.name
  if (!is.null(var.name)) {
    if (!is.character(var.name)) 
      stop("var.name must be a character vector")
    if (!length(var.name) %in% c(nlayers, length(rastfnlst)))
      stop("number of var.name must match ", nlayers, " layers")

    #if (length(var.name) != length(nlayers)) 
    names(nbandlist) <- var.name
  }   
  var.name <- unlist(lapply(names(nbandlist), function(x, nbandlist) {
		if (nbandlist[[x]] == 1) { 
             return(x) 
		} else { 
		  return(paste(x, 1:nbandlist[[x]], sep="_")) 
		} }, nbandlist))


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

  ## Check ncores
  if (!is.null(ncores)) {
    if (length(ncores) != 1) {
      stop("ncores must be integer vector of length = 1")
    } else if (!is.numeric(ncores)) {
      stop("ncores must be integer vector of length = 1")
    } else if (ncores > 1) {
      nbrcores <- parallel::detectCores()
      if (ncores > nbrcores) {
        message("ncores is greater than number of available cores")
        message("using ", nbrcores, " ncores")
        ncores <- nbrcores
      }
    }     
  } else {
    ncores <- 1
  }


  inputs <- data.table(rasterfile, band, var.name, interpolate, windowsize, statistic,
			rast.NODATA)
  message(paste0(capture.output(inputs), collapse = "\n"))

  ########################################################################
  ### DO THE WORK
  ########################################################################  
  outnames <- {}  
  NAlst <- list() 
  for (i in 1:nrasts) {	## loop through rasters

    rastfn <- rastfnlst[[i]]
    rastnm <- basename.NoExt(rastfn)
    rast.prj <- rasterInfo(rastfn)$crs
    rast.bbox <- rasterInfo(rastfn)$bbox      

    ## Check projection and reproject sppltx if different than rast
    sppltprj <- crsCompare(sppltx, rast.prj, crs.default=rast.crs)$x
    
    ## Subset Spatial data frame to just id, x, y
    sppltxy <- data.frame(sppltprj[[xy.uniqueid]], sf::st_coordinates(sppltprj))
    names(sppltxy)[1] <- xy.uniqueid

    ## Check extents
    if (showext) {
      names(rast.bbox) <- c("xmin", "ymin", "xmax", "ymax")
      bbox1 <- sf::st_bbox(rast.bbox, crs=rast.prj)
      bbox2 <- sf::st_bbox(sppltprj)
      check.extents(bbox1, bbox2, showext=showext, layer1nm=rastnm, 
		layer2nm="xyplt", stopifnotin=TRUE)
    }
           
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

      dat <- unique(suppressWarnings(extractPtsFromRaster(ptdata=sppltxy, 
			                rasterfile=rastfn, 
			                band=band, 
			                var.name=var.name, 
			                interpolate=interpolate, 
			                windowsize=windowsize, 
			                statistic=statistic, 
			                ncores=ncores)))
      cname <- names(dat)[2]
      outnames <- c(outnames, cname)
 
      if ("data.table" %in% class(sppltx)) {
        stop("xyplt cannot be sf data.table")
      }
      if (nrow(dat) == 0) {
        stop("no data in ", cname)
      }
      sppltx <- merge(sppltx, dat, by.x=xy.uniqueid, by.y="pid")
      rm(dat)
      gc()

      ## Set rast.NODATA values as NA
      #sppltx <- sppltx[!sppltx[[cname]] %in% rast.NODATA[[1]], ] 
      sppltx[sppltx[[cname]] %in% rast.NODATA[[1]], cname] <- NA 
      
      ## Print missing values to screen
      navals <- sum(is.na(sppltx[[cname]]))
      if (navals > 0) {
        NAlst[[var.name]] <- sppltx[is.na(sppltx[[cname]]),]
        message(paste(navals, "missing values for", rastnm))
        if (exportNA) {
          sppltna <- sppltx[is.na(sppltx[[cname]]),]
          outfn.na <- paste(var.name, "na", sep="_")
          
          spExportSpatial(sppltna, 
              savedata_opts=list(outfolder=outfolder, 
                                  out_fmt=out_fmt, 
                                  out_dsn=out_dsn, 
                                  out_layer=outfn.na,
                                  outfn.pre=outfn.pre, 
                                  outfn.date=outfn.date, 
                                  overwrite_layer=overwrite_layer,
                                  append_layer=append_layer, 
                                  add_layer=TRUE))
        }
      }

      if (!keepNA && sum(is.na(sppltx[[cname]])) > 0) {
        message("removing missing values from input dataset")
        sppltx <- sppltx[!is.na(sppltx[[cname]]),]
      }
    }
  }

  if (savedata) {
    datExportData(sppltx, 
      savedata_opts=list(outfolder=outfolder, 
                          out_fmt=out_fmt, 
                          out_dsn=out_dsn, 
                          out_layer=out_layer,
                          outfn.pre=outfn.pre, 
                          outfn.date=outfn.date, 
                          overwrite_layer=overwrite_layer,
                          append_layer=append_layer,
                          add_layer=TRUE)) 
  }

  if (exportsp) {
    spExportSpatial(sppltx, 
        savedata_opts=list(outfolder=outfolder, 
                            out_fmt=out_fmt, 
                            out_dsn=out_dsn, 
                            out_layer=out_layer,
                            outfn.pre=outfn.pre, 
                            outfn.date=outfn.date, 
                            overwrite_layer=overwrite_layer,
                            append_layer=append_layer, 
                            add_layer=TRUE))
  }

  returnlst <- list(sppltext=sppltx, outnames=outnames, rastfnlst=rastfnlst, 
				inputdf=inputs)

  if (length(NAlst) > 0) 
    returnlst$NAlst <- NAlst

  return(returnlst)
}
