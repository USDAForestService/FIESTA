spGetEstUnit <- function(xyplt, xyplt_dsn=NULL, uniqueid="PLT_CN",
 	unittype="POLY", unit_layer, unit_dsn=NULL, unitvar=NULL, 
	unit.filter = NULL, areavar=NULL, areaunits="ACRES", rast.NODATA=NULL, 
	keepNA=FALSE, keepxy=FALSE, showext=FALSE, savedata=FALSE, 
	exportsp=FALSE, exportNA=FALSE, outfolder=NULL, out_fmt="shp", 
	out_dsn=NULL, out_layer="unit_assgn", outfn.date=FALSE, outfn.pre=NULL, 
	overwrite_dsn=FALSE, overwrite_layer=TRUE, append_layer=FALSE, ...){

  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- ifelse(nargs() == 0, TRUE, FALSE)

  if (gui) {dat=xytable=uniqueid=unionshpnm=savedata=parameters <- NULL}

  ## Set global variables
  value=count=ACRES_GIS <- NULL
  areacalc <- FALSE


  ## Adds to file filters to Cran R Filters table.
  if (.Platform$OS.type=="windows") {
    Filters=rbind(Filters,shp=c("Shapefiles (*.shp)", "*.shp"))
    Filters=rbind(Filters,img=c("Erdas Imagine Images (*.img)", "*.img"))
    Filters=rbind(Filters,tif=c("Raster tif files (*.tif)", "*.tif"))
    Filters=rbind(Filters,csv=c("Comma-delimited files (*.csv)", "*.csv")) }

  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- c(names(formals(FIESTA::spGetEstUnit)), 
		names(formals(FIESTA::spMakeSpatialPoints)))
  if (!all(input.params %in% formallst)) {
    miss <- input.params[!input.params %in% formallst]
    stop("invalid parameter: ", toString(miss))
  }


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


  ## Check unittype
  ###################################################################
  typelst <- c("POLY", "RASTER") 
  unittype <- FIESTA::pcheck.varchar(var2check=unittype, varnm="unittype", 
	gui=gui, checklst=typelst, caption="Estimation unit type?", stopifnull=TRUE)

  ###################################################################
  areaunitslst <- c("ACRES", "HECTARES", "SQMETERS") 
  areaunits <- FIESTA::pcheck.varchar(var2check=areaunits, varnm="areaunits", 
	gui=gui, checklst=areaunitslst, caption="Area units?", stopifnull=TRUE)


  ## Check showext    
  showext <- FIESTA::pcheck.logical(showext, varnm="showext", 
		title="Plot extents?", first="YES", gui=gui)

  ## Check keepNA    
  keepNA <- FIESTA::pcheck.logical(keepNA, varnm="keepNA", 
		title="Keep NULL values?", first="YES", gui=gui)

  ## Check savedata 
  savedata <- FIESTA::pcheck.logical(savedata, varnm="savedata", 
		title="Save data extraction?", first="NO", gui=gui)  

  ## Check exportNA 
  exportNA <- FIESTA::pcheck.logical(exportNA, varnm="exportNA", 
		title="Export NA values?", first="NO", gui=gui)  

  ## Check exportNA 
  exportNA <- FIESTA::pcheck.logical(exportNA, varnm="exportNA", 
		title="Export NA values?", first="NO", gui=gui)  

  ## Check overwrite, outfn.date, outfolder, outfn 
  ########################################################
  if (savedata || exportsp || exportNA) {
    outlst <- pcheck.output(out_dsn=out_dsn, out_fmt=out_fmt, 
		outfolder=outfolder, outfn.pre=outfn.pre, outfn.date=outfn.date, 
		overwrite_dsn=overwrite_dsn, append_layer=append_layer, 
		createSQLite=FALSE, gui=gui)
    out_dsn <- outlst$out_dsn
    outfolder <- outlst$outfolder
    out_fmt <- outlst$out_fmt
    overwrite_layer <- outlst$overwrite_layer
    append_layer <- outlst$append_layer
    if (out_fmt != "csv") {
      outfn.date <- FALSE
    }
  }



  ##################################################################
  ## DO WORK
  ##################################################################
  unitarea <- NULL
 
  ## Check unitlayer
  unitlayerx <- pcheck.spatial(layer=unit_layer, dsn=unit_dsn, gui=gui, 
	caption="Estimation unit layer?")
  
  if (unittype == "POLY") {

    ## Check unitvar
    unitvar <- FIESTA::pcheck.varchar(var2check=unitvar, varnm="unitvar", gui=gui, 
		checklst=names(unitlayerx), caption="Estimation unit variable", 
		warn=paste(unitvar, "not in unitlayer"))
    if (is.null(unitvar)) {
      unitlayerx$ONEUNIT <- 1
      unitvar <- "ONEUNIT"
    }

    ## unit.filter
    unitlayerx <- datFilter(unitlayerx, xfilter=unit.filter)$xf


    ## Check areavar
    areavar <- FIESTA::pcheck.varchar(var2check=areavar, varnm="areavar", gui=gui, 
		checklst=names(unitlayerx), caption="Area variable", 
		warn=paste(areavar, "not in arealayer"))
    if (is.null(areavar)) {
      message("calculating area")
      areacalc <- TRUE
    }

  
    ## Extract values of polygon unitlayer to points
    extpoly <- spExtractPoly(sppltx, polyvlst=unitlayerx, uniqueid=uniqueid, 
		polyvarlst=unitvar, keepNA=keepNA, exportNA=exportNA)
    sppltx <- extpoly$sppltext

    ## Calculate area
    if (areacalc) {
      areavar <- "ACRES_GIS"
      unitlayerx <- FIESTA::areacalc.poly(unitlayerx, unit=areaunits,
		areavar=areavar)
    } 

    ## Create unitarea with subset of spatial data frame
    unitarea <- unitlayerx[, c(unitvar, areavar)]
    unitarea <- aggregate(unitarea[[areavar]], list(unitarea[[unitvar]]), sum)
    names(unitarea) <- c(unitvar, areavar)

  } else { # unittype = "RASTER"
      
    ## Extract values of raster layer to points
    extrast <- spExtractRast(sppltx, rastlst=unitlayerx, var.name=unitvar, 
			uniqueid=uniqueid, keepNA=keepNA, exportNA=exportNA, 
			outfolder=outfolder, outfn.pre=outfn.pre, outfn.date=outfn.date, 
			overwrite_layer=overwrite_layer)
    sppltx <- extrast$spplt
         
    ## Calculate area
    unitarea <- FIESTA::areacalc.pixel(unitlayerx, units=areaunits) 

  }

  ## Write data frames to CSV files
  #######################################
  pltassgn <- sf::st_drop_geometry(sppltx)
  if (savedata) {
    datExportData(pltassgn, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="pltassgn", 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer)

    datExportData(unitarea, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="unitarea", 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer)
  }

  if (exportsp) {
    spExportSpatial(sppltx, out_fmt=out_fmt, out_dsn=out_dsn, out_layer=out_layer,
 		outfolder=outfolder, outfn.pre=NULL, 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer)
  }

  if (showext) {
    plot(sf::st_geometry(unitlayerx)) 
    plot(sf::st_geometry(sppltx), add=TRUE) 
  }
  
  returnlst <- list(pltassgn=pltassgn, unitarea=unitarea, unitvar=unitvar, areavar=areavar,
				pltassgnid=uniqueid)
  if (keepxy) {
    returnlst$spxyplt <- sppltx
  }

 
  return(returnlst)
}

