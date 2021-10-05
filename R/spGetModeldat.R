#' Spatial wrapper - Extracts and compiles auxiliary data within a specified
#' boundary.
#' 
#' Wrapper to extract and compile auxiliary data by domain (i.e, estimation
#' unit or small area domain). The following information is compiled:\cr -
#' Attribute defining domain (i.e., estimation unit) from domain layer\cr -
#' Area by domain (i.e., estimation unit)\cr - Zonal statistics by domain
#' (i.e., estimation unit) - spZonalRast()\cr
#' 
#' *If variable = NULL, then it will prompt user for input.
#' 
#' If there is a raster and SpatialPolygon layer, and the projection of the
#' SpatialPolygons is different than the projection of the raster, the
#' SpatialPolygons object is reprojected to the projection of raster (See note
#' about on-the-fly projection conversion).
#' 
#' @param xyplt Data frame object or String. Name of layer with xy coordinates
#' and unique identifier. Can be layer with xy_dsn, full pathname, including
#' extension, or file name (with extension) in xy_dsn folder.
#' @param xyplt_dsn String. Name of database where xyplt is. The dsn varies by
#' driver. See gdal OGR vector formats (https://www.gdal.org/ogr_formats.html).
#' @param uniqueid String.* Unique identifier of xyplt records.
#' @param domtype String. Type of spatial layer dom_layer is ("POLY",
#' "RASTER").
#' @param dom_layer sf R object or String. Name of the domain spatial layer.
#' Can be a spatial polygon object, full pathname to a shapefile, name of a
#' polygon layer within a database, or a full pathname to raster file.
#' @param dom_dsn String. The data source name (dsn; i.e., folder or database
#' name) of dom_layer. The dsn varies by driver. See gdal OGR vector formats
#' (https://www.gdal.org/ogr_formats.html). Optional.
#' @param domvar String. Name of domain variable in domlayer. If NULL, assuming
#' one domain. An attribute names ONEUNIT is added to layer with value=1.
#' @param rastlst.cont String vector or list. A list of raster(s) with
#' continuous data values (e.g., DEM). The list may include file name of
#' raster(s) or raster objects that are not InMemory.
#' @param rastlst.cont.name String vector. Output names for continuous rasters.
#' Optional. If NULL, name of raster is used as default or name+'_'+layer
#' number for multi-band layers.
#' @param rastlst.cont.stat String. Zonal statistic for continuous rasters.
#' @param rastlst.cont.NODATA Numeric vector. NODATA value for continuous
#' rasters (See notes). These values will be converted to NA and removed from
#' output if keepNA=FALSE. If 1 number, the same value will be used for all
#' categorical rasters. If more than 1 number, the number of values must be
#' equal to the number of rasters in rastlst.cont.
#' @param rastlst.cat String vector or list. A list of raster(s) with thematic
#' (i.e., categorical) data values. The list may include file name of raster(s)
#' or raster objects that are not InMemory.
#' @param rastlst.cat.name String vector. Output names for categorical rasters.
#' If NULL, name of raster is used as default or name+'_'+layer number for
#' multi-band layers.
#' @param rastlst.cat.NODATA Numeric vector. NODATA value for categorical
#' rasters (See notes). These values will be converted to NA and removed from
#' output if keepNA=FALSE. If 1 number, the same value will be used for all
#' categorical rasters. If more than 1 number, the number of values must be
#' equal to the number of rasters in rastlst.cat.
#' @param rastfolder String. Name of the folder with raster layers. Optional.
#' Useful if all raster layers are in same folder.
#' @param asptransform Logical. If TRUE, transforms aspect to Northness and
#' Eastness indices using sin and cosine functions.
#' @param rast.asp String or raster object. The raster in rastlst.cont that is
#' the aspect raster (Note: aspect must have units in degrees).
#' @param rast.lut String. A raster in rastlst.cat to group class values. Only
#' one raster is allowed.
#' @param rastlut String or raster object. The raster look up table used for
#' collapsing rast.lut values.
#' @param areacalc Logical. If TRUE, returns area by domvar.
#' @param areaunits String. Output area units ("ACRES", "HECTARES",
#' "SQMETERS").
#' @param keepNA Logical. If TRUE, returns data frame of NA values.
#' @param NAto0 Logical. If TRUE, converts extracted NA values to 0.
#' @param npixels Logical. If TRUE, include number of pixels.
#' @param showext Logical. If TRUE, layer extents are displayed in plot window.
#' @param savedata Logical. If TRUE, the input data with extracted values are
#' saved to outfolder.
#' @param exportsp Logical. If TRUE, the extracted raster point data are
#' exported to outfolder.
#' @param exportNA Logical. If TRUE, NA values are exported to outfolder.
#' @param outfolder String. If savedata=TRUE or exportsp=TRUE, name of output
#' folder.  If NULL, the working directory is used.
#' @param out_fmt String. Format for output tables ('csv', 'sqlite', 'gpkg').
#' @param out_dsn String. Name of database if out_fmt = c('sqlite', 'gpkg').
#' @param outfn.pre String. Add a prefix to output name (e.g., "01").
#' @param outfn.date Logical. If TRUE, adds current date to outfile name.
#' @param overwrite_dsn Logical. If TRUE, overwrite dsn.
#' @param overwrite_layer Logical. If TRUE, overwrite layer(s) in dsn.
#' @param vars2keep String vector. Attributes in SAdoms, other than domvar to
#' include in domzonal output and extract to pltassgn points.
#' @param ...  Other parameters for spMakeSpatialPoints.
#' @return \item{pltassgn}{ sf object. xyplt data with extracted values from
#' rastlst*. } \item{domzonal}{ Data frame. Number of pixels and zonal
#' statistics from continuous rasters or zonal proportions from categorical
#' raster for each domain (i.e., estimation unit). } \item{domvar}{ Data frame.
#' Domain (i.e., estimation unit) name. } \item{inputdf}{ Data frame. Raster
#' information input to zonal summaries. } \item{prednames}{ String vector.
#' Name(s) of predictor variable(s). } \item{zonalnames}{ String vector.
#' Name(s) of zonal variable(s). } \item{predfac}{ String vector. Name(s) of
#' categorical (i.e. factor) variable(s). } \item{npixelvar}{ String. Name of
#' variable describing number of pixels. } \item{domarea}{ Data frame. Area by
#' domain (i.e., estimation unit). } \item{areavar}{ String. Name of variable
#' describing acres in domarea. } \item{pltassgnid}{ String. Unique identifier
#' of plot. }
#' 
#' If savedata=TRUE, datstrat and unitarea are saved to outfolder.  If
#' exportsp=TRUE, the sf object is exported to outfolder.
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
#' If exportsp=TRUE:\cr If out_fmt="shp", the writeOGR (rgdal) function is
#' called. The ArcGIS driver truncates variable names to 10 characters or less.
#' Variable names are changed before export using an internal function
#' (trunc10shp). If Spatial object has more than 1 record, it will be returned
#' but not exported.
#' 
#' On-the-fly projection conversion\cr The spTransform (rgdal) method is used
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
#' @export spGetModeldat
spGetModeldat <- function(xyplt, xyplt_dsn=NULL, uniqueid="PLT_CN",
 	domtype="POLY", dom_layer=NULL, dom_dsn=NULL, domvar="DOMAIN",
 	rastlst.cont=NULL, rastlst.cont.name=NULL, rastlst.cont.stat="mean", 
	rastlst.cont.NODATA=NULL, rastlst.cat=NULL, rastlst.cat.name=NULL, 
	rastlst.cat.NODATA=NULL, rastfolder=NULL, asptransform=FALSE, rast.asp=NULL, 
	rast.lut=NULL, rastlut=NULL, areacalc=TRUE, areaunits="ACRES", keepNA=TRUE, 
	NAto0=TRUE, npixels=TRUE, showext=FALSE, savedata=FALSE, exportsp=FALSE, 
	exportNA=FALSE, outfolder=NULL, out_fmt="csv", out_dsn=NULL, outfn.pre=NULL, 
	outfn.date=FALSE, overwrite_dsn=FALSE, overwrite_layer=TRUE, vars2keep=NULL, ...){

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
    if (length(varsmiss) > 0) {
      stop("missing variables: ", paste(varsmiss, collapse=", "))
    }
  } else {
    stop("under construction... please convert dom_layer to POLY")
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
  ## 1) Extract values from domlayer
  #############################################################################
  domarea <- NULL
  if (!domvar %in% names(sppltx)) { 
      ## Extract values of polygon layer to points
    extpoly <- spExtractPoly(xyplt=sppltx, polyvlst=domlayerx, 
		uniqueid=uniqueid, polyvarlst=unique(c(domvar, vars2keep)), 
		keepNA=FALSE, exportNA=exportNA)
    sppltx <- unique(extpoly$spxyext)
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
    zonalDT.cont <- data.table(DOMAIN = unique(domlayerx[[domvar]]))
    setnames(zonalDT.cont, "DOMAIN", domvar)
    setkeyv(zonalDT.cont, domvar)
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
        zonaldat.rast.cont <- spZonalRast(domlayerx, rastfn=rastfn, polyv.att=domvar, 
		zonalstat=zonalstat, pixelfun=northness, rast.NODATA=rast.cont.NODATA,
		na.rm=TRUE)
        zonalext <- setDT(zonaldat.rast.cont$zonalext)
        outname <- zonaldat.rast.cont$outname
        class(zonalext[[domvar]]) <- class(domlut[[domvar]])        

        if (!is.null(rastnm)) 
          setnames(zonalext, outname, rastnm2)
        setkeyv(zonalext, domvar)
        zonalDT.cont <- zonalDT.cont[zonalext] 
  
        rastnm2 <- ifelse(is.null(rastnm), "asp_sin", paste0(rastnm, "_sin"))
        zonalstat <- c(rastlst.cont.stat) 
        zonaldat.rast.cont <- spZonalRast(domlayerx, rastfn=rastfn, 
		rast.NODATA=rast.cont.NODATA, polyv.att=domvar, zonalstat=rastlst.cont.stat, 
		pixelfun=eastness, na.rm=TRUE)
        zonalext <- setDT(zonaldat.rast.cont$zonalext)
        outname <- zonaldat.rast.cont$outname
        class(zonalext[[domvar]]) <- class(domlut[[domvar]])        
        if (!is.null(rastnm2)) {
          setnames(zonalext, outname, rastnm2)
        }
        setkeyv(zonalext, domvar)
        zonalDT.cont <- zonalDT.cont[zonalext]
 
      } else {
        if (i == 1 && npixels) {
          zonalstat <- c("npixels", rastlst.cont.stat) 
          if (!is.null(rastnm)) {
            rastnm <- c("npixels", rastnm)
          }
        } 
        zonaldat.rast.cont <- spZonalRast(domlayerx, rastfn=rastfn, 
		rast.NODATA=rast.cont.NODATA, polyv.att=domvar, zonalstat=zonalstat, 
		showext=showext, na.rm=TRUE)
        zonalext <- setDT(zonaldat.rast.cont$zonalext)
        outname <- zonaldat.rast.cont$outname
        class(zonalext[[domvar]]) <- class(domlut[[domvar]])        
        if (!is.null(rastnm)) {
          setnames(zonalext, outname, rastnm)
        }
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
    zonalDT.cat <- data.table(DOMAIN = unique(domlayerx[[domvar]]))
    setnames(zonalDT.cat, "DOMAIN", domvar)
    setkeyv(zonalDT.cat, domvar)
    for (i in 1:length(rastlst.catfn)) {
      rastfn <- rastlst.catfn[i]
      rastnm <- inputdf.cat[inputdf.cat$rasterfile == rastfn, "var.name"][[1]]
      #message(rastfn, "...")
      rast.cat.NODATA <- rastlst.cat.NODATA[i]

      zonalstat <- "proportion"
      if (i == 1 && npixels)
        zonalstat <- c("npixels", zonalstat)        
      if (identical(rast.lutfn, rastfn)) {
        zonaldat.rast.cat <- spZonalRast(domlayerx, rastfn=rastfn, rast.NODATA=rast.cat.NODATA, 
 		polyv.att=domvar, zonalstat=zonalstat, rastlut=rastlut, outname=names(rastlut)[2],
		na.rm=TRUE)
      } else {
        zonaldat.rast.cat <- spZonalRast(domlayerx, rastfn=rastfn, rast.NODATA=rast.cat.NODATA, 
 		polyv.att=domvar, outname=rastnm, zonalstat=zonalstat, na.rm=TRUE)
      }

      zonalext <- setDT(zonaldat.rast.cat$zonalext)
      outname <- zonaldat.rast.cat$outname
      outname[grep("npixels", outname)] <- "npixels"
      setnames(zonalext, c(domvar, outname))
      class(zonalext[[domvar]]) <- class(domlut[[domvar]])        
      setkeyv(zonalext, domvar)

      zonalDT.cat <- zonalDT.cat[zonalext] 
      zonalnames <- c(zonalnames, outname[outname != "npixels"])

      if (npixels) npixels <- FALSE
      rm(zonaldat.rast.cat)
      rm(zonalext)
      gc() 
    }
    tabs <- check.matchclass(domlut, zonalDT.cat, domvar)
    domlut <- tabs$tab1
    zonalDT.cat <- tabs$tab2

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
		outfn.date=outfn.date, overwrite_layer=overwrite_layer)
    datExportData(domlut, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="domlut", 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer)
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

