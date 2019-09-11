spGetEstUnit <- function(spplt=NULL, spplt_dsn=NULL, xyplt=NULL, uniqueid="PLT_CN",
 	unittype="POLY", unit_layer=NULL, unit_dsn=NULL, unitvar=NULL, areavar=NULL,
 	areaunits="ACRES", rast.NODATA=NULL, keepnull=FALSE, showext=FALSE, 
	savedata=FALSE, exportshp=FALSE, outfolder=NULL, outfn=NULL, overwrite=FALSE, ...){

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

  ## Spatial Points: shp or xytable for data extraction.. 
  ##################################################################################
  if (is.null(spplt)) {
    ## Check parameters
    if (is.null(xyplt)) stop("either include spplt or xyplt for data extraction")
         
    ## Create spatial object from xyplt coordinates
    sppltx <- FIESTA::spMakeSpatialPoints(xyplt=xyplt, uniqueid=uniqueid, ...)
  } else {
    ## Check spplt
    sppltx <- pcheck.spatial(layer=spplt, dsn=spplt_dsn, gui=gui,
		caption="Spatial points with XY coords?")
    if (is.null(sp::proj4string(sppltx))) stop("spplt must have defined projection")

    ## GET uniqueid
    sppltnames <- names(sppltx@data)
    uniqueid <- FIESTA::pcheck.varchar(var2check=uniqueid, varnm="uniqueid", gui=gui, 
		checklst=sppltnames, caption="UniqueID of spplt", 
		warn=paste(uniqueid, "not in spplt"), stopifnull=TRUE)

    if (sum(is.na(sppltx@data[[uniqueid]])) > 0) stop("NA values in ", uniqueid)
    if (length(unique(sppltx@data[[uniqueid]])) < nrow(sppltx@data)) 
      stop("spplt records are not unique")
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

  ## Check keepnull    
  keepnull <- FIESTA::pcheck.logical(keepnull, varnm="keepnull", 
		title="Keep NULL values?", first="YES", gui=gui)

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
      outfn <- "stratext"
    } else if (!is.character(outfn)) {
      stop("outfn must be character")
    }      
    outfncsvbase <- paste0(outfn, "_", format(Sys.time(), "%Y%m%d"))
    if (!overwrite)
      outfncsvbase <- FIESTA::fileexistsnm(outfolder, outfncsvbase, "csv") 
  }

  ##################################################################
  ## DO WORK
  ##################################################################
  unitarea <- NULL
 
  ## Check unitlayer
  unitlayerx <- FIESTA::pcheck.spatial(layer=unit_layer, dsn=unit_dsn, 
	gui=gui, caption="Estimation unit layer?", stopifnull=TRUE)
  
  if (unittype == "POLY") {

    ## Check unitvar
    unitvar <- FIESTA::pcheck.varchar(var2check=unitvar, varnm="unitvar", gui=gui, 
		checklst=names(unitlayerx), caption="Estimation unit variable", 
		warn=paste(unitvar, "not in unitlayer"))
    if (is.null(unitvar)) {
      unitlayerx@data$ONEUNIT <- 1
      unitvar <- "ONEUNIT"
    }

    ## Check areavar
    areavar <- FIESTA::pcheck.varchar(var2check=areavar, varnm="areavar", gui=gui, 
		checklst=names(unitlayerx), caption="Area variable", 
		warn=paste(areavar, "not in arealayer"))
    if (is.null(areavar)) {
      message("calculating area")
      areacalc <- TRUE
    }

  
    ## Extract values of polygon unitlayer to points
    extpoly <- spExtractPoly(sppltx, polylst=unitlayerx, uniqueid=uniqueid, 
		polyvarlst=unitvar, keepnull=keepnull)
    sppltx <- extpoly$spplt

    ## Calculate area
    if (areacalc) {
      areavar <- "ACRES_GIS"
      unitlayerx <- FIESTA::areacalc.poly(unitlayerx, units=areaunits,
		areavar=areavar)
    } 

    ## Create unitarea with subset of spatial data frame
    unitarea <- unitlayerx@data[, c(unitvar, areavar)]
    unitarea <- aggregate(unitarea[[areavar]], list(unitarea[[unitvar]]), sum)
    names(unitarea) <- c(unitvar, areavar)

  } else { # unittype = "RASTER"
      
    ## Extract values of raster layer to points
    extrast <- spExtractRast(sppltx, rastlst=unitlayerx, var.name=unitvar, 
			uniqueid=uniqueid, exportna=exportshp, outfolder=outfolder,
			overwrite=overwrite)
    sppltx <- extrast$spplt
         
    ## Calculate area
    unitarea <- FIESTA::areacalc.pixel(unitlayerx, units=areaunits) 

  }

  if (exportshp) 
    spExportShape(sppltx, outshpnm=outfn, outfolder=outfolder, overwrite=overwrite)

  
  returnlst <- list(pltunit=sppltx@data, sppltunit=sppltx, unitarea=unitarea, 
		unitvar=unitvar, areavar=areavar)
 
  return(returnlst)
}

