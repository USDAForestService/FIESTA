#' Data - Reclass raster.
#'
#' Wrapper to reclass a raster using a vector of cut breaks.
#'
#'
#' @param rastfn String. Path name of raster to classify.
#' @param cutbreaks Integer vector. Breaks to use for classifying (e.g.,
#' c(0,50,75) uses function in calc: 'ifelse (A >= 0 & A < 50, 1, 
#' ifelse (A >= 50 & A < 75, 2, ifelse (A >= 75, 3, 255)))'
#' @param bnd sf R object or String. Boundary to clip raster (optional).
#' Can be a spatial sf object, full pathname to a shapefile, or name of
#' a layer within a database.
#' @param bnd_dsn String. Name of data source name with bnd_layer, if in a
#' database.
#' @param bnd.filter String. Optional filter of bnd_layer.
#' @param buffdist Number. The distance to buffer the polygon before clipping
#' raster, in units of raster.
#' @param nodataclass Integer. Class number to assign NODATA values to.
#' @param gethist Logical. If TRUE, returns a histogram of pixel values by class.
#' @param savedata_opts List. See help(savedata_options()) for a list
#' of options. Only used when savedata = TRUE.  
#' @return Data.
#' @author Tracey S. Frescino
#' @keywords data
#' @export
spClassifyRast <- function(rastfn,
                           cutbreaks,
                           bnd = NULL, 
                           bnd_dsn = NULL, 
                           bnd.filter = NULL, 
						   buffdist = NULL,
						   nodataclass = NULL,
						   gethist = FALSE,
                           savedata_opts = NULL) {


  ## Set global variables
  gui <- FALSE
  savedata <- TRUE
  savetmp <- TRUE

  ##################################################################
  ## CHECK PARAMETER NAMES
  ##################################################################
  input.params <- names(as.list(match.call()))[-1]
  formallst <- names(formals(spClassifyRast))
  if (!all(input.params %in% formallst)) {
    miss <- input.params[!input.params %in% formallst]
    stop("invalid parameter: ", toString(miss))
  }

  ## Check parameter lists
  pcheck.params(input.params, savedata_opts=savedata_opts)


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
      assign(names(savedata_opts)[[i]], savedata_opts[[i]])
    }
  }

  ## Check overwrite, outfn.date, outfolder, outfn
  ########################################################
  if (savedata) {
    outlst <- pcheck.output(out_dsn=out_dsn, out_fmt=out_fmt, 
                  outfolder=outfolder, outfn.pre=outfn.pre, 
                  outfn.date=outfn.date, overwrite_dsn=overwrite_dsn, 
                  overwrite_layer=overwrite_layer, 
                  append_layer=append_layer, createSQLite=FALSE, gui=gui)
    out_dsn <- outlst$out_dsn
    outfolder <- outlst$outfolder
    out_fmt <- outlst$out_fmt
    overwrite_layer <- outlst$overwrite_layer
    overwrite_dsn <- outlst$overwrite_dsn
    append_layer <- outlst$append_layer
  } 
  if (is.null(out_layer)) {
    out_layer <- "rastcl"
  }

  ## Verify ref raster
  ########################################################
  rast <- getrastlst.rgdal(rastfn)

  ## Get crs of reference raster
  rast.crs <- rasterInfo(rast)$crs
  rast.nodata <- rasterInfo(rast)$nodata_value
  rast.datatype <- rasterInfo(rast)$datatype
  

  ## Get nodata value of ref raster.
  ## If the nodata values was not assigned (NA), then use a default value.
  if (is.na(rast.nodata)) {
	nodata <- getDefaultNodata(rast.datatype)
  } else {
	nodata <- rast.nodata
  }


  ## Check cutbreaks
  if (!is.vector(cutbreaks) || !is.numeric(cutbreaks)) {
    stop("cutbreaks must be a numeric vector")
    if (!identical(sort(cutbreaks),cutbreaks)) {
	  stop("cutbreaks must be in sequential order")
	}
  }
 
  ####################################################################
  ## Get boundary info
  ## If a boundary is input, clip the ref raster to the boundary mask
  ####################################################################

  ## Import boundary
  bndx <- pcheck.spatial(layer=bnd, dsn=bnd_dsn, caption="boundary")
  if (!is.null(bndx)) {
    bndx <- datFilter(bndx, xfilter=bnd.filter, stopifnull=TRUE)$xf

    ## Compare crs of reference raster and reproject if different
    bndx <- crsCompare(bndx, rast.crs)$x	

    ## Check buffdist
    if (!is.null(buffdist)) {
      if (!is.numeric(buffdist)) stop("invalid buffdist... must be numeric")
    }
	
    if (!is.null(buffdist)) {
      ## This will buffer the polygon 1 pixel to include all pixels inside boundary
      bndx <- sf::st_buffer(bndx, dist=buffdist)
    }
 	
					  
	## Clip reference raster to polygon boundary 
    ###########################################################################
	if (savetmp) {
	  cliprastfn <- file.path(outfolder, "clip_rast.tif")
	} else {
	  cliprastfn <- tempfile("clip_rast", fileext="tif")
    }
	
	  
   ####################################################################
   ## Clip raster
   ####################################################################
   clipRaster(src = bndx, 
             srcfile = rast, 
			 src_band = 1, 
			 dstfile = cliprastfn,			 
             fmt = "GTiff", 
			 init = nodata,
			 dstnodata = nodata,
			 maskByPolygons = TRUE, 
             options = "COMPRESS=LZW")
  } else {
  
    cliprastfn <- rast
  }

  
  ## Create expression based on cut breaks
  expr <- ""
  for (i in 1:(length(cutbreaks))) {
    if (i == length(cutbreaks)) {
	  expr <- paste0(expr, "ifelse (A >= ", cutbreaks[i], ", ", i, ", ")
	} else {
      expr <- paste0(expr, "ifelse (A >= ", cutbreaks[i], 
					" & A < ", cutbreaks[i + 1], ", ", i, ", ") 
	}
  }
  expr <- paste0(expr, nodata, paste(rep(")", length(cutbreaks)), collapse="")) 

 
  lut <- data.frame(MIN = cutbreaks, 
                    MAX = c(cutbreaks[-1], paste0(cutbreaks[length(cutbreaks)], "+")), 
					CLASS = seq(1:length(cutbreaks))) 
  

  if (!is.null(nodataclass)) { 
    classvalues <- lut$CLASS
	if (!is.vector(nodataclass) || length(nodataclass) != 1) {
	  message("invalid nodataclass... must be a vector of length 1")
	  return(NULL)
	} else if (!nodataclass %in% lut$CLASS) {
	  message("invalid nodataclass... must be in: ", toString(lut$CLASS))
	  return(NULL)
	}
    expr <- paste0("ifelse (is.na(A), ", nodataclass, ", ", expr, ")")
  }             	
 
  message(expr)


  ####################################################################
  ## Reclass raster
  ####################################################################
  outfn <- paste0(file.path(outfolder, out_layer), ".tif")
  gdalraster::calc(expr, 
	               rasterfiles = cliprastfn,
				   var.names = "A",
				   dstfile = outfn,
				   dtName = "Byte", 
				   fmt = "GTiff",
				   options = c("COMPRESS=DEFLATE"),
                   nodata_value = nodata,
				   setRasterNodataValue = TRUE,
				   write_mode = "overwrite"
				   )
 

  ####################################################################
  ## Save lookup table to outfolder
  ####################################################################
  datExportData(lut, 
          savedata_opts = list(outfolder = outfolder, 
		                       out_fmt = "csv", 
							   out_layer = paste0(out_layer, "_lut")))

  returnlst <- list(outfn=outfn, lut=lut)
  
  
  if (gethist) {
  
    ## get min/max value
    ds <- new(gdalraster::GDALRaster, outfn, read_only=TRUE)
    minmax <- ds$getMinMax(band=1, approx_ok=FALSE)
    rast.min <- minmax[1]
    rast.max <- minmax[2]


    histdf <- data.frame(
	             value = seq(1:length(cutbreaks)), 
                 pixels = ds$getHistogram(band=1, 
				              min=rast.min, max=rast.max+1,
							  num_buckets=length(cutbreaks),
							  incl_out_of_range=FALSE, approx_ok=FALSE))
    returnlst$histdf <- histdf
  }

  return(returnlst)
}

