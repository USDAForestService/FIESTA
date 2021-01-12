spGetStrata <- function(xyplt, xyplt_dsn=NULL, uniqueid="PLT_CN", 
	unittype="POLY", unit_layer=NULL, unit_dsn=NULL, unitvar=NULL, 
	unit.filter=NULL, strattype="RASTER", strat_layer=NULL, 
	strat_dsn=NULL, strvar=NULL, strat_lut=NULL, areaunits="ACRES", 
	rast.NODATA=NULL, keepNA=FALSE, keepxy=TRUE, showext=FALSE, 
	savedata=FALSE, exportsp=FALSE, exportNA=FALSE, outfolder=NULL, 
	out_fmt="shp", out_dsn=NULL, out_layer="strat_assgn", 
	outfn.date=TRUE, outfn.pre=NULL, overwrite=FALSE, ...){

  ## Check for necessary packages
  ###########################################################
  if (!"sf" %in% rownames(installed.packages()))
    stop("sf package is required for spGetStrata()")
  
  if (!"rgdal" %in% rownames(installed.packages()))
    stop("rgdal package is required for spGetStrata()")


  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- ifelse(nargs() == 0, TRUE, FALSE)

  if (gui) {uniqueid=stratclip=unitarea <- NULL}

  ## Set global variables
  value=count=strwt=polyv.lut=NAlst <- NULL

  ## Adds to file filters to Cran R Filters table.
  if (.Platform$OS.type=="windows") {
    Filters=rbind(Filters,shp=c("Shapefiles (*.shp)", "*.shp"))
    Filters=rbind(Filters,img=c("Erdas Imagine Images (*.img)", "*.img"))
    Filters=rbind(Filters,tif=c("Raster tif files (*.tif)", "*.tif"))
    Filters=rbind(Filters,csv=c("Comma-delimited files (*.csv)", "*.csv")) }



  ##################################################################
  ## CHECK INPUT PARAMETERS
  ##################################################################

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


  ## Spatial Layers: strattype and unittype
  ##################################################################################
  typelst <- c("POLY", "RASTER") 

  ## Check strattype
  ###################################################################
  strattype <- FIESTA::pcheck.varchar(var2check=strattype, varnm="strattype", 
	gui=gui, checklst=typelst, caption="Strata type?", stopifnull=TRUE)

  ## Check unittype
  ###################################################################
  unittype <- FIESTA::pcheck.varchar(var2check=unittype, varnm="unittype", 
	gui=gui, checklst=typelst, caption="Estimation unit type?")

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
    outfolder <- FIESTA::pcheck.outfolder(outfolder, gui)
    overwrite <- FIESTA::pcheck.logical(overwrite, varnm="overwrite", 
		title="Overwrite files?", first="NO", gui=gui)  
    outfn.date <- FIESTA::pcheck.logical(outfn.date , varnm="outfn.date", 
		title="Add date to outfiles?", first="NO", gui=gui) 

    out_fmtlst <- c("sqlite", "gpkg", "shp")
    out_fmt <- FIESTA::pcheck.varchar(var2check=out_fmt, varnm="out_fmt", 
		checklst=out_fmtlst, gui=gui, caption="Output format?") 

    if (out_fmt %in% c("sqlite", "gpkg")) {
      gpkg <- ifelse(out_dsn == "gpkg", TRUE, FALSE)        
      out_dsn <- DBcreateSQLite(out_dsn, outfolder=outfolder, outfn.date=outfn.date, 
				overwrite=overwrite, dbconnopen=FALSE)
    } else {
      if (!is.null(outfn.pre)) {
        outfolder <- file.path(outfolder, outfn.pre)
        if (!dir.exists(outfolder)) dir.create(outfolder)
      }
    } 
  }

  ##################################################################
  ## DO WORK
  ##################################################################
  unitarea=stratalut <- NULL

  ## Check unitlayer
  unitlayerx <- pcheck.spatial(layer=unit_layer, dsn=unit_dsn, gui=gui, 
	caption="Estimation unit layer?")
  nounit <- ifelse (is.null(unitlayerx), TRUE, FALSE)

  ## unit.filter
  if (!nounit)
    unitlayerx <- datFilter(unitlayerx, xfilter=unit.filter)$xf


  ##################################################################
  ## if unittype == "RASTER"
  ## Note: still working on combo... until then, covert to polygon
  ##################################################################
  if (unittype == "RASTER" && !nounit) {
    message("converting unit_layer to polygon...")

    unitlayerx <- polygonizeRaster(unitlayerx)
    unitvar <- "value"
    unittype == "POLY"
  }

  ##################################################################
  ## if unittype == "POLY"
  ##################################################################
  if (unittype == "POLY" || nounit) {
    if (strattype == "POLY") {
      message("converting strat_layer to raster...")

      polyrast <- spPoly2Rast(polyv=strat_layer, polyv_dsn=strat_dsn, 
		polyv.att=strvar, outfolder=outfolder)
      strat_layer <- polyrast$rastfn
      polyv.lut <- polyrast$polyv.lut
      strat_dsn <- NULL
      strvar <- polyrast$polyv.att

      rast.NODATA <- 0
    } else {
      strvar <- "value"
    }
 
    ##################################################################
    ## if strattype == "RASTER"
    ##################################################################
    ## Check strat_layer
    stratlayerfn <- suppressWarnings(getrastlst.rgdal(strat_layer, rastfolder=strat_dsn,
 		stopifLonLat=TRUE))

    ## Get raster info
    rast_info <- rasterInfo(stratlayerfn)
    stratlayer.res <- rast_info$cellsize
    nbands <- rast_info$nbands
    rast.prj <- rast_info$crs
    rast.bbox <- rast_info$bbox

      ## Check band
#      if (!is.null(band) && nbands > 1) {
#        if (!is.integer(band)) stop("band must be integer")
#        if (band > nbands) stop("invalid band, outside of range")
#      } 
    
    if (!nounit) {
      ## Check unitvar
      unitvar <- FIESTA::pcheck.varchar(var2check=unitvar, varnm="unitvar", gui=gui, 
		checklst=names(unitlayerx), caption="Estimation unit variable", 
		warn=paste(unitvar, "not in unitlayer"))
      if (is.null(unitvar)) {
        unitlayerx$ONEUNIT <- 1
        unitvar <- "ONEUNIT"
      }

      ## Check projection and reproject spobj if different than rast
      unitlayerprj <- crsCompare(unitlayerx, rast.prj)$x

      ## Check extents
      names(rast.bbox) <- c("xmin", "ymin", "xmax", "ymax")
      bbox1 <- sf::st_bbox(rast.bbox, crs=rast.prj)
      bbox2 <- sf::st_bbox(unitlayerprj)
      check.extents(bbox1, bbox2, showext=showext, layer1nm="rast", layer2nm="unitlayer",
			stopifnotin=TRUE)

      ## Extract values of polygon unitlayer to points
      ## Note: removing all NA values
      extpoly <- spExtractPoly(sppltx, polyvlst=unitlayerprj, uniqueid=uniqueid, 
		polyvarlst=unitvar, keepNA=FALSE, exportNA=exportNA)
      sppltx <- extpoly$sppltext
      unitNA <- extpoly$NAlst[[1]]
      outname <- extpoly$outname
      if (outname != unitvar) {
        message("unitvar changed from ", unitvar, " to ", outname, 
				" because of duplicate names in xyplt")
        names(unitlayerprj)[names(unitlayerprj) == unitvar] <- outname
        unitvar <- outname
      }

      ## Get pixel counts by estimation unit
      stratalut <- setDT(zonalFreq(src=unitlayerprj, attribute=unitvar, 
			rasterfile=stratlayerfn, band=1, na.rm=TRUE, ignoreValue=rast.NODATA))
      setnames(stratalut, c("zoneid", "value", "zoneprop"), c(unitvar, strvar, "strwt"))
      strataNA <- stratalut[is.na(get(strvar)), ]
      stratalut <- stratalut[!is.na(get(strvar)), ]

      ## Get unitarea 
      unitlayerprj <- areacalc.poly(unitlayerprj, unit=areaunits)
      areavar <- paste0(areaunits, "_GIS")  
      unitarea <- aggregate(unitlayerprj[[areavar]], list(unitlayerprj[[unitvar]]), sum)
      names(unitarea) <- c(unitvar, areavar)
        
    } else {  ## if nounit == TRUE
      stratalut <- areacalc.pixel(stratlayerfn, rast.NODATA=rast.NODATA)
      stratalut$strwt <- stratalut$count / sum(stratalut$count)
      strvar <- "value"

      unitarea <- sum(stratalut$area)
      unitvar <- NULL
      areavar <- NULL 
    }

    ## Extract values of raster layer to points
    extrast <- spExtractRast(sppltx, rastlst=stratlayerfn, var.name=strvar, 
			uniqueid=uniqueid, exportNA=exportsp, keepNA=keepNA, 
			outfolder=outfolder, overwrite=overwrite)
    sppltx <- extrast$spplt
    pltdat <- extrast$sppltext
    rastfnlst <- extrast$rastfnlst
    outname <- extrast$outnames
    NAlst <- extrast$NAlst[[1]]

    if (!is.null(NAlst)) {
      message("NA values shown in red... ")
      plot(sf::st_geometry(sppltx), pch=16, cex=.5)
      plot(sf::st_geometry(NAlst), add=TRUE, col="red", cex=1, pch=16)
    }
  }

  ## If lookup table, merge and aggregate
  #######################################
  if (!is.null(strat_lut)) {
    stratalut <- merge(stratalut, strat_lut, by=strvar)
    stratalut <- stratalut[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(unitvar, "strclass"), .SDcols=c("count", "strwt")]
    setorderv(stratalut, c(unitvar, "strclass"))
    setnames(stratalut, "strclass", "STRATUMCD")

    sppltx <- merge(sppltx, strat_lut, by=strvar)
    setnames(sppltx, "strclass", "STRATUMCD")
    strvar <- "STRATUMCD"
  } else {
    setnames(stratalut, strvar, "STRATUMCD")
    setnames(sppltx, strvar, "STRATUMCD")
    strvar <- "STRATUMCD"
  }  

  ## Write data frames to CSV files
  #######################################
  pltassgn <- sf::st_drop_geometry(sppltx)

  if (!keepxy) 
    pltassgn2 <- pltassgn[, c(uniqueid, unitvar, strvar)]

  if (!is.data.table(stratalut)) stratalut <- setDT(stratalut)
  setkeyv(stratalut, c(unitvar, strvar))  

  if (savedata) {
    datExportData(pltassgn, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="pltassgn", 
		outfn.date=outfn.date, overwrite_layer=overwrite)
    datExportData(unitarea, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="unitarea", 
		outfn.date=outfn.date, overwrite_layer=overwrite)
    datExportData(stratalut, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="stratalut", 
		outfn.date=outfn.date, overwrite_layer=overwrite)
  }

  ## Export to shapefile
  if (exportsp)
    spExportSpatial(sppltx, out_fmt=out_fmt, out_dsn=out_dsn, out_layer=out_layer,
 		outfolder=outfolder, outfn.pre=NULL, 
		outfn.date=outfn.date, overwrite_layer=overwrite)
    
  
  returnlst <- list(pltassgn=pltassgn, unitarea=unitarea, unitvar=unitvar, 
		areavar=areavar, stratalut=stratalut, strvar=strvar,
		pltassgnid=uniqueid, getwt=FALSE, strwtvar="strwt")
 
  return(returnlst)
}

