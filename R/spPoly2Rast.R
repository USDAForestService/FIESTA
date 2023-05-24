#' Spatial - Converts SpatialPolygons layer to raster.
#' 
#' Converts SpatialPolygons layer to raster.
#' 
#' 
#' @param polyv sf R object or String. Polygon data to convert to raster.  Can
#' be a spatial polygon object, full pathname to a shapefile, or name of a
#' layer within a database.
#' @param polyv_dsn String. Data source name (dsn; e.g., sqlite or shapefile
#' pathname) of layer to convert. The dsn varies by driver. See gdal OGR vector
#' formats (https://www.gdal.org/ogr_formats.html). Optional if polyv is sf
#' object.
#' @param polyv.att String. Name of attribute in polyv to rasterize.
#' @param polyv.lut Data frame. Look up table of codes, if polyv.att is
#' character or want to group codes.
#' @param rastfn.template String. Full path name of raster to use as template
#' for new raster.
#' @param NODATA Number. The NODATA value for background values. If NODATA is
#' NULL, and a NODATA value is defined on the rastfn.template raster, the 
#' default is the defined NODATA value, else it is defined based on its datatype 
#' (see DEFAULT_NODATA for default data values).
#' @param outfolder String. If exportshp=TRUE, name of output folder. If NULL,
#' the working directory is used.
#' @param outfn String. Name of output raster. If NULL, default is 'polyrast'.
#' @param outfn.pre String. Add a prefix to output name (e.g., "01").
#' @param outfn.date Logical. If TRUE, add date to end of outfile (e.g.,
#' outfn_'date'.csv).
#' @param outext String. Name of raster extension (fmt).
#' @param overwrite Logical. If TRUE and exportshp=TRUE, overwrite files in
#' outfolder.
#' @return A list containing raster and raster information derived from the
#' original polygon.
#' @note
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
#' 
#' If exportshp=TRUE:\cr The st_write (sf) function is called. The ArcGIS
#' driver truncates variable names to 10 characters or less. Variable names are
#' changed before export using an internal function (trunc10shp). If Spatial
#' object has more than 1 record, it will be returned but not exported.
#' @author Tracey S. Frescino
#' @keywords data
#' @examples 
#' \donttest{
#' # Get polygon vector layer from FIESTA external data
#' WYbhdistfn <- system.file("extdata",
#'                           "sp_data/WYbighorn_districtbnd.shp", 
#'                           package = "FIESTA")
#' 
#' # Turn polygon into raster
#' # Note: raster values must be numeric, therefore names were changed to
#' # numeric codes based on lookup table produced from the following code.                      
#' new_rast <- spPoly2Rast(polyv = WYbhdistfn,
#'                         polyv.att = "DISTRICTNA",
#'                         outfolder = tempdir())
#' }
#' @export spPoly2Rast
spPoly2Rast <- function(polyv, 
                        polyv_dsn = NULL, 
                        polyv.att, 
                        polyv.lut = NULL, 
                        rastfn.template = NULL, 
                        NODATA = NULL, 
                        outfolder = NULL, 
                        outfn = "polyrast", 
                        outext = "img", 
                        outfn.pre = NULL, 
                        outfn.date = TRUE, 
                        overwrite = FALSE){

  #####################################################################################
  ## DESCRIPTION: 
  ## Clips, or intersects a polygon vector with another polygon vector with option 
  ## to export to an ArcGIS shapefile.
  #####################################################################################

  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- ifelse(nargs() == 0, TRUE, FALSE)

  ## If gui.. set variables to NULL
  if(gui){poly=clippoly=unionpoly=savedata <- NULL}

  drivers <- data.frame(
	fmt = c("raster", "ascii", "SAGA", "IDRISI", "CDF", "GTiff", "ENVI", 
		"EHdr", "HFA", "VRT"),
	DefaultExt = c("grd", "asc", "sdat", "rst", "nc", "tif", "envi", 
		"bil", "img", "vrt"),
	stringsAsFactors=FALSE
  )	

  ##################################################################
  ## CHECK INPUT PARAMETERS
  ##################################################################

  ## Get poly and clippoly layers
  polyvx <- pcheck.spatial(layer=polyv, dsn=polyv_dsn, gui=gui, caption="Poly to clip?")

  ## Check polyv.att
  polyv.att <- pcheck.varchar(var2check=polyv.att, varnm="polyv.att", gui=gui, 
		checklst=names(polyvx), caption="Polygon attribute to rasterize", 
		warn=paste(polyv.att, "not in polyvx"), stopifnull=TRUE)


  if (is.character(polyvx[[polyv.att]])) {
    if (is.null(polyv.lut)) {
      message("creating lookup table of codes")

      NAME <- sort(unique(polyvx[[polyv.att]]))
      CODE <- seq(1:length(NAME))
      polyv.lut <- data.frame(NAME, CODE, stringsAsFactors=FALSE)
      names(polyv.lut) <- c(polyv.att, "CODE")
    }
  }
  if (!is.null(polyv.lut)) {
    if (!polyv.att %in% names(polyv.lut)) 
      stop(polyv.att, " must be in polyv.lut") 
    polyvx <- merge(polyvx, polyv.lut, by.x=polyv.att)
    polyv.att <- names(polyv.lut)[names(polyv.lut) != polyv.att]
  }
    
  ## Check overwrite, outfn.date, outfolder, outfn 
  ########################################################
  overwrite <- pcheck.logical(overwrite, varnm="overwrite", 
		title="Overwrite files?", first="NO", gui=gui)  
  outfn.date <- pcheck.logical(outfn.date , varnm="outfn.date", 
		title="Add date to outfiles?", first="YES", gui=gui)  
  outfolder <- pcheck.outfolder(outfolder, gui)

  ## Check outext and outfilenm
  outext.tmp <- unlist(strsplit(outext, "\\."))
  if (length(outext.tmp) > 1) {
    outext <- outext.tmp[length(outext.tmp)]   
    if (!outext %in% drivers[["DefaultExt"]]) stop("outext is invalid") 
  }
  outfilenm <- getoutfn(outfn, outfolder=outfolder, outfn.pre=outfn.pre, 
		outfn.date=outfn.date, overwrite=overwrite, ext=outext)


  if (!is.null(rastfn.template)) {

    ## verify raster
    rastfn <- getrastlst(rastfn.template)

    ## get raster info
    rast_info <- rasterInfo(rastfn)
    rast.prj <- rast_info$crs
    rast.fmt <- rast_info$format
    nbands <- rast_info$nbands

    ## Check if projections match
    polyvx <- crsCompare(polyvx, rast.prj, nolonglat=TRUE)$x


    ## Create virtual raster by clipping raster template to extent of polyvx
    rastclip <- spClipRast(rast=rastfn, clippolyv=polyvx, 
                           maskByPolygons=FALSE, NODATA=NODATA, 
                           outfolder=outfolder, outfn="tmp", fmt="VRT")

    ## Create blank raster from clipped virtual raster
    rast.fmt <- drivers[drivers$DefaultExt == outext, "fmt"]
    rast <- gdalraster::rasterFromRaster(rastclip, fmt=rast.fmt, dstfile=outfilenm)

    ## Rasterize polygons
    polyrast <- rasterizePolygons(src=polyvx, burn_value=polyv.att, rasterfile=rast)

  } else {

    ## Project if longlat coordinate system
    polyvx <- checksf.longlat(polyvx)

    ## Create blank raster from polyvx
    rast <- rasterFromVectorExtent(polyvx, res=30, dstfile=outfilenm, fmt="HFA")

    ## Rasterize polygons
    polyrast <- rasterizePolygons(src=polyvx, burn_value=polyv.att, rasterfile=rast)

  }

  returnlst <- list(rastfn=outfilenm)
  if (!is.null(polyv.lut))
    returnlst$polyv.lut <- polyv.lut
  returnlst$polyv.att <- polyv.att

  return(returnlst)    
}

