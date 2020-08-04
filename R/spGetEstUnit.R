spGetEstUnit <- function(xyplt, xyplt_dsn=NULL, uniqueid="PLT_CN",
 	unittype="POLY", unit_layer=NULL, unit_dsn=NULL, unitvar=NULL, 
	unit.filter = NULL, areavar=NULL, areaunits="ACRES", rast.NODATA=NULL, 
	keepNA=FALSE, showext=FALSE, savedata=FALSE, exportsp=FALSE, 
	exportNA=FALSE, outfolder=NULL, out_fmt="shp", out_dsn=NULL, 
	out_layer="unit_assgn", outfn.date=TRUE, outfn.pre=NULL, 
	overwrite=FALSE, ...){

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
    } 
    if (!is.null(outfn.pre))
      out_layer <- paste(outfn.pre, out_layer, sep="_")
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
			overwrite=overwrite)
    sppltx <- extrast$spplt
         
    ## Calculate area
    unitarea <- FIESTA::areacalc.pixel(unitlayerx, units=areaunits) 

  }

  ## Write data frames to CSV files
  #######################################
  pltassgn <- sf::st_drop_geometry(sppltx)
  if (savedata) {
    if (out_fmt == "csv") {  
      write2csv(pltassgn, outfolder=outfolder, outfilenm="pltassgn", outfn.pre=outfn.pre,
		outfn.date=outfn.date, overwrite=overwrite)
      write2csv(unitarea, outfolder=outfolder, outfilenm="unitarea", outfn.pre=outfn.pre,
		outfn.date=outfn.date, overwrite=overwrite)	
    } else {
      write2sqlite(pltassgn, SQLitefn=out_dsn, out_name="pltassgn", overwrite=overwrite)
      write2sqlite(unitarea, SQLitefn=out_dsn, out_name="unitarea", overwrite=overwrite)
    }     			
  }

  if (exportsp)
    spExportSpatial(sppltx, out_fmt=out_fmt, out_dsn=out_dsn, out_layer=out_layer,
 		outfolder=outfolder, outfn.pre=NULL, 
		outfn.date=outfn.date, overwrite_layer=overwrite)
  
  
  returnlst <- list(pltassgn=pltassgn, unitarea=unitarea, unitvar=unitvar, areavar=areavar,
				pltassgnid=uniqueid)
 
  return(returnlst)
}

