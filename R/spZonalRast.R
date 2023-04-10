#' Spatial - Extracts summary statistics by polygon (i.e., zone) for a raster.
#' 
#' Extracts summary statistics by polygon, or zone for a raster (single or
#' multi-band).
#' 
#' Use spZonalRast() to prompt for input.
#' 
#' If the projection of polyv is different than the projection of rast, the
#' polyv SpatialPolygons object is converted to the projection of rast (See
#' note about on-the-fly projection conversion).
#' 
#' @param polyv sf R object or String. Polygon data to identify zones. Can be a
#' spatial polygon object, full pathname to a shapefile, or name of a layer
#' within a database.
#' @param polyv_dsn String. Data source name (dsn; e.g., sqlite or shapefile
#' pathname) of zonal layer. The dsn varies by driver. See gdal OGR vector
#' formats (https://www.gdal.org/ogr_formats.html). Optional if polyv is sf
#' object.
#' @param polyv.att String. Name of attribute in polyv to identify zones for
#' summarizing raster statistics.
#' @param rastfn String or Raster. File name(s) with extensions, or raster
#' object(s).  Note: raster objects must be written to file.
#' @param rastfolder String. Name of the folder with raster layers. Optional.
#' Useful if all raster layers are in same folder.
#' @param bands Numeric vector. If rast is a multi-layer raster and only 1 or
#' some layers are desired, specify layer number(s) in a vector format. If
#' NULL, all layers are summed.
#' @param zonalstat String vector. Zonal statistic(s) to return for rasters
#' with continuous data ("mean", "sum", "majority", "minority", "variety",
#' "npixels") or rasters with discrete data ("count", "proportion").
#' @param pixelfun Function. A function to apply to the individual pixel values
#' before calculating sum and mean. The function should accept a single numeric
#' argument (pixel value) and return a single numeric argument.
#' @param outname String. Variable name for output. The output names will use
#' outname as a prefix to summary statistics (i.e., 'outname'.mean,
#' 'outname'.sum).
#' @param showext Logical. If TRUE, layer extents are displayed in plot window.
#' @param rastlut Data frame. A look up table to recode raster values. Must be
#' 2 columns: Column 1 with raster values and column 2 with recode values.
#' @param rast.NODATA Numeric. NODATA value or other values to ignore. These
#' values will be removed from output zonal table. See notes below.
#' @param na.rm Logical. If TRUE, Null values are removed before zonal
#' statistic calculations.
#' @param savedata Logical. If TRUE, the zonal data are saved to outfolder.
#' @param savedata_opts List. See help(savedata_options()) for a list
#' of options. Only used when savedata = TRUE. If out_layer = NULL,
#' default = 'zonalext'.
#'
#' @return \item{zonalext}{ Data frame. Zonal statistics by polygon attribute
#' (attribute). } \item{outname}{ String vector. Names of zonal statistic
#' variables generated in zonalext data frame. } \item{rasterfile}{ String
#' vector. Names of raster file(s) associated with zonal statistic. }
#' 
#' If savedata=TRUE, outdat data frame is saved to outfolder (Default name:
#' zonalext_'date'.csv).
#' @note rast.NODATA\cr NODATA values are raster pixel values that have no data
#' of interest, including pixels within the extent of the layer, but outside
#' the area of interest. Sometimes these pixels have been defined previously.
#' The defined NODATA pixels are imported to R as NULL values. When not
#' previously defined, the pixels outside the area of interest will be the
#' minimum or maximum value depending on the data type (e.g., 16-bit signed:
#' min=-32,768; max=32,768) or byte size (1 byte: min=0; max=255).  These
#' NODATA values will be added to the zonal statistic calculations if not
#' specified in rast.NODATA.
#' 
#' On-the-fly projection conversion\cr The spTransform (sf) method is used
#' for on-the-fly map projection conversion and datum transformation using
#' PROJ.4 arguments. Datum transformation only occurs if the +datum tag is
#' present in the both the from and to PROJ.4 strings. The +towgs84 tag is used
#' when no datum transformation is needed. PROJ.4 transformations assume NAD83
#' and WGS84 are identical unless other transformation parameters are
#' specified.  Be aware, providing inaccurate or incomplete CRS information may
#' lead to erroneous data shifts when reprojecting. See spTransform help
#' documentation for more details.
#' @author Tracey S. Frescino
#' @keywords data
#' @examples 
#' # Set up data from `FIESTA`
#' WYbhdistfn <- system.file("extdata",
#'                           "sp_data/WYbighorn_districtbnd.shp",
#'                           package = "FIESTA")
#' demfn <- system.file("extdata",
#'                      "sp_data/WYbighorn_dem_250m.img",
#'                      package = "FIESTA")
#'
#' # Import spatial data with `spImportSpatial`
#' WYbhdist <- spImportSpatial(WYbhdistfn)
#' 
#' # Extract mean and sum in `WYbhdist`
#' spZonalRast(polyv = WYbhdist, 
#'             polyv.att = "DISTRICTNA", 
#'             rastfn = demfn, 
#'             zonalstat = c("mean", "sum")) 
#' @export spZonalRast
spZonalRast <- function(polyv, 
                        polyv_dsn = NULL, 
                        polyv.att = NULL, 
                        rastfn, 
                        rastfolder = NULL, 
                        bands = NULL, 
                        zonalstat, 
                        pixelfun = NULL, 
                        outname = NULL, 
                        showext = FALSE, 
                        rastlut = NULL, 
                        rast.NODATA = NULL, 
                        na.rm = TRUE, 
                        savedata = FALSE, 
                        savedata_opts = NULL) { 
  ##################################################################################### 
  ## DESCRIPTION:  
  ## Extracts summary statistics by polygon (i.e., zone).  
  ##################################################################################### 

  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE 
  gui <- ifelse(nargs() == 0, TRUE, FALSE)  

  if (gui) showext=savedata=overwrite <- NULL 
 
  ## Set global variables
  count <- NULL
  
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
  ## CHECK INPUT PARAMETERS 
  ################################################################## 
  ## Check dsn, layer 
  spobj <- pcheck.spatial(layer=polyv, dsn=polyv_dsn, gui=gui, 
		caption="Polygon zones?") 
 
  ## Check polyv.att 
  polyv.att <- pcheck.varchar(var2check=polyv.att, varnm="polyv.att", 
		gui=gui, checklst=names(spobj), caption="Zonal attribute",  
		warn=paste(polyv.att, "not in polyv"), stopifnull=TRUE) 
    
  ## Get raster info 
  ########################################################  

  ## Verify rasters 
  rastfn <- suppressWarnings(getrastlst.rgdal(rastfn, rastfolder, 
		stopifLonLat=TRUE, gui=gui))

  ## Get names of raster 
  rastnm <- basename.NoExt(rastfn) 
 
  ## Get number of bands in each raster and set names 
  rast.nbands <- rasterInfo(rastfn)$nbands

  ## Get number of bands in each raster and set names 
  rast.prj <- rasterInfo(rastfn)$crs

  ## Check zonalstat     
  zonalstatlst <- c("mean", "sum", "majority", "minority", "variety", 
	"npixels", "count", "proportion") 
  zonalstat <- pcheck.varchar(var2check=zonalstat, varnm="zonalstat", 
	gui=gui, checklst=zonalstatlst, caption="Zonal statistic(s)", 
	stopifnull=TRUE, multiple=TRUE) 
    
  ## Check bands 
  if (!is.null(bands)) { 
    if (rast.nbands > 1) { 
      if (!is.numeric(bands)) stop("bands must be integer") 
      if (bands > rast.nbands) stop("invalid bands, outside of range") 
      message("returning same zonal statistics for all bands")  
    } 
  } else { 
    if (rast.nbands > 1) { 
      bands <- seq(1, rast.nbands) 
    } else { 
      bands <- 1 
    } 
  } 

  ## Check rast.NODATA
  if (!is.null(rast.NODATA)) {
    if (!is.numeric(rast.NODATA))
      stop("raster.NODATA must be numeric")
  }

  ## Check outnames 
  if (!is.null(outname)) { 
    if (!is.character(outname)) stop("out must be a character vector") 
    if (length(outname) != rast.nbands) 
      stop("number of outname must match ", rast.nbands, " bands") 
  } else { 
    outname <- rastnm 
  } 

  ## Check showext     
  showext <- pcheck.logical(showext, varnm="showext",  
		title="Plot extents?", first="YES", gui=gui) 
  
  ## Check savedata  
  savedata <- pcheck.logical(savedata, varnm="savedata",  
		title="Save data extraction?", first="NO", gui=gui)    


  ## Check overwrite, outfn.date, outfolder, outfn 
  ########################################################
  if (savedata) {
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
      out_layer <- "zonalext"
    }
  }
 
  ######################################################################## 
  ### DO THE WORK 
  ######################################################################## 
  #out <- setDT(unique(polyvx@data[,polyv.att, drop=FALSE])) 
  #setkeyv(out, polyv.att) 
  #if (nrow(out) > nrow(unique(out))) warning("there are polygons with duplicate ids") 

  ## Check projection and reproject spobj if different than rast
  spobjprj <- crsCompare(spobj, rast.prj)$x

  ## Check extents
  rast.bbox <- rasterInfo(rastfn)$bbox
  names(rast.bbox) <- c("xmin", "ymin", "xmax", "ymax")
  bbox1 <- sf::st_bbox(rast.bbox, crs=rast.prj)
  bbox2 <- sf::st_bbox(spobjprj)

  ## check if bbox2 is within bbox1
  check.extents(bbox1, bbox2, showext, layer1nm=rastnm, layer2nm="polv",
			stopifnotin=TRUE)
  dtype <- rasterInfo(rastfn)$datatype
  NODATAval <- rasterInfo(rastfn)$nodata_value
  if (is.null(rast.NODATA) || is.na(rast.NODATA)) {
    rast.NODATA <- NODATAval[1]
  }
  if (is.na(rast.NODATA)) {
    rast.NODATA <- NULL
  }

  zonalext <- data.table(unique(spobjprj[[polyv.att]])) 
  setnames(zonalext, polyv.att) 
  setkeyv(zonalext, polyv.att) 
  
  outnames <- {}   
  for (b in bands) { 
     
    prename <- outname 
    if (!is.null(prename) && rast.nbands > 1) {
      prename <- paste(prename, b, sep="_") 
    }

    if (any(zonalstat %in% c("mean", "sum", "npixels"))) { 
      atts <- zonalstat[which(zonalstat %in% c("mean", "min", "max", "sum", "npixels"))] 
#      atts[atts == "sum"] <- "sumvalues" 
      atts[atts == "count"] <- "npixels"  
      zstats <- setDT(zonalStats(src=spobjprj, attribute=polyv.att, 
                            rasterfile=rastfn, pixelfun=pixelfun, band=b, na.rm=TRUE, 
                            ignoreValue=rast.NODATA)) 
      zstats <- zstats[, c("zoneid", atts), with=FALSE] 
      var.name <- paste(prename, atts, sep=".") 
      setnames(zstats, names(zstats)[-1], var.name) 
      setkey(zstats, "zoneid") 

      ## Check if class of key(zstats) in zstats matches class of key(zonalext) in zonalext
      tabs <- check.matchclass(zonalext, zstats, key(zonalext), key(zstats))
      zonalext <- tabs$tab1
      zstats <- tabs$tab2

      zonalext <- zonalext[zstats]
      outnames <- c(outnames, var.name) 
      if (length(bands) > 1 && b == 1 && "npixels" %in% zonalstat) {
        zonalstat <- zonalstat[zonalstat != "npixels"]
      }
    }  

    if (any(zonalstat == "majority")) { 
      zstats <- setDT(zonalMajority(src=spobjprj, attribute=polyv.att, rasterfile=rastfn,
		band=b, na.rm=TRUE, ignoreValue=rast.NODATA)) 
      zstats <- zstats[, c("zoneid", "value")] 
      var.name <- paste(prename, "majority", sep=".") 
      setnames(zstats, names(zstats)[-1], var.name) 
      setkey(zstats, "zoneid") 

      ## Check if class of key(zstats) in zstats matches class of key(zonalext) in zonalext
      tabs <- check.matchclass(zonalext, zstats, key(zonalext), key(zstats))
      zonalext <- tabs$tab1
      zstats <- tabs$tab2

      zonalext <- zonalext[zstats] 
      outnames <- c(outnames, var.name) 
    }  

    if (any(zonalstat == "minority")) { 
      zstats <- setDT(zonalMinority(src=spobjprj, attribute=polyv.att, 
                            rasterfile=rastfn, band=b, na.rm=TRUE, 
                            ignoreValue=rast.NODATA)) 
      zstats <- zstats[, c("zoneid", "value")] 
      var.name <- paste(prename, "minority", sep=".") 
      setnames(zstats, names(zstats)[-1], var.name) 
      setkey(zstats, "zoneid") 

      ## Check if class of key(zstats) in zstats matches class of key(zonalext) in zonalext
      tabs <- check.matchclass(zonalext, zstats, key(zonalext), key(zstats))
      zonalext <- tabs$tab1
      zstats <- tabs$tab2

      zonalext <- zonalext[zstats] 
      outnames <- c(outnames, var.name) 
    }  

    if (any(zonalstat == "variety")) { 
      zstats <- setDT(zonalVariety(src=spobjprj, attribute=polyv.att, 
                            rasterfile=rastfn, band=b, na.rm=TRUE, 
                            ignoreValue=rast.NODATA)) 
      zstats <- zstats[, c("zoneid", "value")] 
      var.name <- paste(prename, "variety", sep=".") 
      setnames(zstats, names(zstats)[-1], var.name) 
      setkey(zstats, "zoneid") 

      ## Check if class of key(zstats) in zstats matches class of key(zonalext) in zonalext
      tabs <- check.matchclass(zonalext, zstats, key(zonalext), key(zstats))
      zonalext <- tabs$tab1
      zstats <- tabs$tab2

      zonalext <- zonalext[zstats] 
      outnames <- c(outnames, var.name) 
    }  
 
    if (any(zonalstat %in% c("count", "proportion"))) { 
      zstats <- setDT(zonalFreq(src=spobjprj, attribute=polyv.att,
                          rasterfile=rastfn, band=b, na.rm=na.rm, ignoreValue=rast.NODATA)) 
      newvar <- "value" 
      if (!is.null(rastlut)) { 
        LUTvar <- names(rastlut)[1] 
        newvar <- names(rastlut)[2]  

        ## Check that all values are in table and that class of merging variable matches 
        check.matchval(zstats, rastlut, "value", LUTvar, 
                                tab1txt="zstats", tab2txt="rastlut") 
        tabs <- check.matchclass(zstats, rastlut, "value", LUTvar,  
                                tab1txt="zstats", tab2txt="rastlut") 
        zstats <- tabs$tab1 
        rastlut <- tabs$tab2 
                  
        zstats <- merge(zstats, rastlut, by.x="value", by.y=LUTvar) 
      }
  
      if (any(zonalstat == "count")) { 
        zstats.cnt <- dcast(zstats, zoneid ~ get(newvar), fun.aggregate=sum, 
 				value.var="count") 
        if (any(names(zstats.cnt) == "NA")) { 
          warning("NA values in output") 
          print(zstats.cnt[zstats.cnt[["NA"]] > 0,]) 
          zstats.cnt[["NA"]] <- NULL 
        } 
        zstats.cnt[is.na(zstats.cnt)] <- 0 
        var.name <- paste(prename, names(zstats.cnt)[-1], sep=".") 
        setnames(zstats.cnt, names(zstats.cnt)[-1], var.name) 
        setkey(zstats.cnt, "zoneid") 

        ## Check if class of key(zstats.cnt) in zstats.cnt matches class of key(zonalext) in zonalext
        tabs <- check.matchclass(zonalext, zstats.cnt, key(zonalext), key(zstats.cnt))
        zonalext <- tabs$tab1
        zstats.cnt <- tabs$tab2

        zonalext <- zonalext[zstats.cnt] 
        outnames <- c(outnames, var.name) 
      } 
      if (any(zonalstat == "proportion")) { 
        zstats.prop <- dcast(zstats, zoneid ~ get(newvar), fun.aggregate=sum, 
  			value.var="zoneprop") 
 
        if (any(names(zstats.prop) == "NA")) { 
          warning("NA values in output") 
          print(zstats.prop[zstats.prop[["NA"]] > 0,]) 
          zstats.prop[["NA"]] <- NULL 
        } 
        zstats.prop[is.na(zstats.prop)] <- 0 
        var.name <- paste(prename, names(zstats.prop)[-1], sep=".") 
        setnames(zstats.prop, names(zstats.prop)[-1], var.name) 
        setkey(zstats.prop, "zoneid") 

        ## Check if class of key(zstats.prop) in zstats.prop matches class of key(zonalext) in zonalext
        tabs <- check.matchclass(zonalext, zstats.prop, key(zonalext), key(zstats.prop))
        zonalext <- tabs$tab1
        zstats.prop <- tabs$tab2

        zonalext <- zonalext[zstats.prop] 
        outnames <- c(outnames, var.name) 
      } 
    } 
    rm(zstats)
    gc() 
  }  

  if (savedata) {
    datExportData(zonalext, 
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
  
  
  returnlst <- list(zonalext=setDF(zonalext), outname=outnames,  
					rasterfile=rep(rastfn, length(outnames))) 
  return(returnlst) 
} 
