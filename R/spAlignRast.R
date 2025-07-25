#' Spatial - Aligns a list of raster layer(s) based on a reference raster.
#'
#' Rasters are pixel-aligned and reprojected using the gdal warp function
#' with help from the gdalraster package. The extent of the reference
#' raster is used or a given boundary extent.
#'
#' @param ref_rastfn String. Full path name of reference raster.
#' @param rastlst String. Full path names of one or more rasters to align.
#' @param resample_methodlst String. Resample method ('mode', 'near',
#' 'bilinear', 'cubic', 'cubicspline', 'max', 'min', 'med', 'average').
#' Suggested values: if raster type is categorical; 'mode' or 'near'.
#' if raster type is continuous; 'bilinear', 'cubic'.
#' @param clip Logical. If TRUE, subset raster to a boundary.
#' @param bnd R object or Full path name to a shapefile or layer in a database.
#' @param bnd_dsn String. Data source name of bnd, if bnd is a layer in a database.
#' @param buffdist Number. The distance to buffer the polygon before clipping
#' raster. Uses sf::st_buffer. The distance is based on units of the raster.
#' @param tile Logical. If TRUE, tile the output raster.
#' @param tile_blocksize Numeric. If tile = TRUE, define the size of tile block.
#' @param makestack Logical. If TRUE, makes a raster stack with format 'GTIFF'.
#' @param outrastnmlst String. Base name of output raster (e.g., 'elev').
#' @param stacknm String. Stack Name
#' @param outfolder String. Name of folder for writing output raster. If NULL,
#' outfolder = getwd().
#' @param overwrite Logical. If TRUE, overwrite output raster.
#'
#' @return String. List of output raster file names.
#'
#' @export spAlignRast
spAlignRast <- function(ref_rastfn,
                        rastlst,
                        resample_methodlst = NULL,
                        clip = FALSE,
                        bnd = NULL,
                        bnd_dsn = NULL,
                        buffdist = 30,
                        tile = TRUE,
                        tile_blocksize = 256,
                        makestack = FALSE,
                        outrastnmlst = NULL,
                        stacknm = "stack",
                        outfolder = NULL,
                        overwrite = TRUE) {
  ############################################################################
  ## DESCRIPTION:
  ## Rasters are pixel-aligned and reprojected using the gdal warp function
  ## with help from the gdalraster package. The extent of the reference
  ## raster is used or a given boundary extent.
  ############################################################################
  out_fmt <- "GTiff"
  gui <- FALSE
  validate <- TRUE


  ##################################################################
  ## CHECK PARAMETER NAMES
  ##################################################################

  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- names(formals(spAlignRast))
  if (!all(input.params %in% formallst)) {
    miss <- input.params[!input.params %in% formallst]
    stop("invalid parameter: ", toString(miss))
  }


  ## Verify rasters
  ########################################################
  rastlst <- suppressWarnings(getrastlst(rastlst, gui=gui))
  rastnmlst <- sapply(rastlst, basename.NoExt)
  #if (any(rastlst == "")) stop("must write raster to file")
  nrasts <- length(rastlst)
  out_fmt <- "GTiff"

  ## Check rasample_methodlst
  if (length(resample_methodlst) != nrasts) {
    if (length(resample_methodlst) != 1) {
      message("invalid resample_methodlst...")
    } else {
      methodlst <- c('mode', 'near', 'bilinear', 'cubic', 'cubicspline',
                     'max',  'min', 'med', 'average')
      if (!any(resample_methodlst %in% methodlst)) {
        message("invalid resample_methodlst... must be in following list: \n", toString(methodlst))
      }
      message("using ", resample_methodlst, " for all rasters...")
      resample_methodlst <- rep(resample_methodlst, nrasts)
    }
  }

  ## define output raster name
  if (is.null(outrastnmlst)) {
    if (clip) {
      outrastnmlst <- paste0(rastnmlst, "_clip")
    } else {
      outrastnmlst <- paste0(rastnmlst, "_align")
    }
  } else {
    if (length(outrastnmlst) != nrasts) {
      message("invalid outrastnmlst... must be a vector of length ", nrasts)
      stop()
    } else if (any(outrastnmlst %in% rastnmlst)) {
      message("invalid outrastnmlst... names must be different than input raster names")
      stop()
    }
  }

  ## Check tiled
  tile <- pcheck.logical(tile, varnm="tile",
                         title="Tile output", first="YES", gui=gui)
  ## Check clip
  clip <- pcheck.logical(clip, varnm="clip",
                         title="Clip output", first="NO", gui=gui)

  ## Check makestack
  makestack <- pcheck.logical(makestack, varnm="makestack",
                         title="Make raster stack", first="YES", gui=gui)


  ## check outfolder
  if (is.null(outfolder)) {
    outfolder <- getwd()
  } else {
    if (!dir.exists(outfolder)) {
      stop("invalid outfolder...")
    } else {
      outfolder <- normalizePath(outfolder, winslash = "/")
    }
  }

  ## check out_fmt
  out_fmtlst <- c("vrt",  "GTiff")
  if (!out_fmt %in% out_fmtlst) {
    stop("out_fmt must be if following list: ", toString(out_fmtlst))
  }


  ## get info for ref rastfn
  ref_info <- rasterInfo(ref_rastfn)
  ref_crs <- ref_info$crs               ## Coordinate Reference System
  ref_cellsize <- ref_info$cellsize     ## Cell size
  ref_bbox <- rasterInfo(ref_rastfn)$bbox ## Raster extent (xmin, ymin, xmax, ymax)
  #nbands <- rasterInfo$nbands


  if (clip) {
    ## Check bnd
    bndx <- pcheck.spatial(layer = bnd, dsn = bnd_dsn,
                           gui = gui, caption = "Poly to clip?")

    ## Validate polygon
    if (validate) {
      bndx <- polyfix.sf(bndx)
    }
    
    ## Check buffer distance
    if (!is.null(buffdist)) {
      if (any(!is.vector(buffdist), length(buffdist) > 1, !is.numeric(buffdist))) {
        message("invalid buffdist... must be a numeric vector of size 1")
      }
    }
    ## Check projection of bndx... make sure it matches ref rastfn
    bndx <- crsCompare(bndx, ref_crs)$x
  }


  ## Loop through rasters
  ##################################################################################
  outrastlst <- {}
  for (i in 1:length(rastlst)) {
    rastfn <- rastlst[i]
    outrastnm <- outrastnmlst[i]
    message("aligning ", rastfn, "...")

    ## add extension to outrastfn
    if (out_fmt == "vrt") {
      outrastfn <- file.path(outfolder, paste0(outrastnm, ".vrt"))
    } else if (out_fmt == "img") {
      outrastfn <- file.path(outfolder, paste0(outrastnm, ".img"))
    } else {
      outrastfn <- file.path(outfolder, paste0(outrastnm, ".tif"))
    }


    ## get info for raster
    rast_info <- rasterInfo(rastfn)
    rast_dtyp <- rast_info$datatype         ## data type
    rast_nodata <- rast_info$nodata_value   ## nodata value

  ## Define a nodata value to the default value for the data type (if nodata=NA)
  #############################################################

  ## data type    nodata
  ## -------------------
  ## Byte            255
  ## Int8           -128
  ## UInt16        65535
  ## Int16        -32767
  ## UInt32   4294967293
  ## Int32   -2147483647
  ## Float32    -99999.0
  ## Float64    -99999.0

    if (is.na(rast_nodata)) {
      nodata_value <- getDefaultNodata(rast_dtyp)
    } else {
      nodata_value <- rast_nodata
    }

    ## define target coordinate reference system (t_srs)
    #################################################################
    t_srs <- ref_crs


    ## define arguments for warp
    #################################################################
    args <- {}

    ## define target extent for raster if clipping to boundary (-te)
    #################################################################
    if (clip) {
      if (!is.null(buffdist)) {
        ## get extent of bnd and add to argument list
        bndext <- sf::st_bbox(sf::st_buffer(bndx, buffdist))
      } else {
        bndext <- sf::st_bbox(bndx)
      }
      #bndext <- sf::st_bbox(bndx)
      outrast_extent <- c("-te", as.character(bndext))
      args <- outrast_extent
    } else {
      outrast_extent <- NULL
    }

    ## define pixel resolution based on ref_rastfn (-tr)
    #################################################################
    pixel_res <- c("\n-tr", as.character(ref_cellsize))

    ## define target alignment
    ## Align coordinates of output extent to the values of -tr
    #target_align <- c("\n-tap")

    ## define resample_method
    ## -ovr = 'NONE' - uses base resolution for overview level (-wm)
    ##################################################################
    resample_method <- c("\n-r", resample_methodlst[i], "-ovr", "NONE")

    ## define amount of memory to use for caching
    #warp_memory <- c("\n-wm", "2000")

    ## define multithreaded warping implementation (-multi)
    #multi_thread <- c("\n-multi","-wo","NUM_THREADS=4")

    ## define datatype (-wt)
    datatype <- c("\n-wt", rast_info$datatype)


    ## create cl argument list with:
    ## output raster extent (te), pixel res (tr), and datatype (wt)
    args <- c(args, pixel_res, resample_method, datatype)

    if (clip) {
      args_vrt <- args
      dst_filename <- tempfile(tmpdir = outfolder, fileext = ".vrt")
    } else {
      dst_filename <- outrastfn
    }

    ## define format/compression and add to argument list
    if (out_fmt == "vrt") {
      #format <- c("\n-of", "VRT")
      #args <- c(args, format)
      
    } else if (out_fmt == "img") {

      ## only works if it is < 2GB
      compress <- c("\n-co", "COMPRESSED=YES")
      args <- c(args, compress)

    } else {
      #format <- c("\n-of", "GTiff")
      ## If not compressing, the default is to use if needed
      ## BIGTIFF=IF_SAFER
      BIGTIFF <- TRUE
      if (BIGTIFF && !clip) {
        compress <- c("\n-co", "COMPRESS=LZW", "-co", "BIGTIFF=YES")
      } else if (!clip) {
        compress <- c("\n-co", "COMPRESS=LZW")
      } else {
        compress <- NULL
      }

      #args <- c(args, format, compress)
      args <- c(args, compress)

      ## set tile blocksize
      if (tile && !clip) {
        blockxsize <- paste0("BLOCKXSIZE = ", tile_blocksize)
        blockysize <- paste0("BLOCKYSIZE = ", tile_blocksize)
        args <- c(args, "\n-co", "TILED=YES", "-co", blockxsize, "-co", blockysize)
      }
    }

    ## define nodata
    if (is.na(rast_nodata)) {
      ## define nodata of source (not already defined) to ignore before resampling
      args <- c(args, "\n-srcnodata", nodata_value, "-dstnodata", nodata_value)
      if (clip) {
        args_vrt <- c(args_vrt, "\n-srcnodata", nodata_value, "-dstnodata", nodata_value)
      }
    } else {
      ## define nodata of destination as defined nodata
      args <- c(args, "\n-dstnodata", nodata_value)
      if (clip) {
        args_vrt <- c(args_vrt, "\n-dstnodata", nodata_value)
      }
    }

    ## append overwrite to argument string
    if (overwrite && file.exists(outrastfn)) {
      args <- c(args, "\n-overwrite")
    }

    ## remove returns from argument list
    cl_arg <- sub("\n", "", args)
    
    if (clip) {
      cl_arg_vrt <- c(sub("\n", "", args_vrt), "-overwrite")
    }

    message("\nprocessing ", basename(rastfn), "...")
    message("warp arguments:\n", toString(cl_arg))

    ## Use warp to resample to same pixel size and extent
    gdalraster::warp(rastfn, dst_filename = dst_filename, t_srs = t_srs, cl_arg = cl_arg)

    
    ds <- new(gdalraster::GDALRaster, dst_filename, read_only = FALSE)
    ds$getDescription(band = 1)
    ds$close()
    
    
    if (clip) {
      ## Clip raster
      clipRaster(src = bndx, 
                 srcfile = dst_filename, 
                 src_band = 1, 
                 dstfile = outrastfn, 
                 fmt = out_fmt, 
                 maskByPolygons = TRUE, 
                 init = nodata_value, 
                 dstnodata = nodata_value, 
                 options = "COMPRESS=LZW")
      file.remove(dst_filename)
    }

    outrastlst <- c(outrastlst, outrastfn)
  }


  ## Stack rasters to a Virtual Raster
  if (makestack) {
    if (is.null(stacknm)) {
      stacknm <- "stack.tif"
    } else {
      if (is.na(getext(stacknm))) {
        stacknm <- paste0(stacknm, ".tif")
      }
    }

    ## check data types
    #print(sapply(outrastlst, function(x) rasterInfo(x)$datatype))

    stackvrtfn <- file.path(outfolder, "stack.vrt")
    gdalraster::buildVRT(stackvrtfn, outrastlst, cl_arg = "-separate")

    ## Create a geoTiff from a virtual raster
    stacktiffn <- file.path(outfolder, stacknm)
    gdalraster::translate(stackvrtfn, stacktiffn)
    #rasterInfo(stacktiffn)

    file.remove(stackvrtfn)
    sapply(outrastlst, file.remove)

    return(stacktiffn)
  } else {
    return(outrastlst)
  }
}


