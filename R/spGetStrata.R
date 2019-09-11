spGetStrata <- function(spplt=NULL, spplt_dsn=NULL, xyplt=NULL, uniqueid="PLT_CN", 
	unittype="POLY", unit_layer=NULL, unit_dsn=NULL, unitvar=NULL, strattype="RASTER", 
	strat_layer=NULL, strat_dsn=NULL, strvar=NULL, areaunits="ACRES", rast.NODATA=NULL,
 	keepnull=FALSE, showext=FALSE, savedata=FALSE, exportshp=FALSE, outfolder=NULL,
 	outfn=NULL, overwrite=FALSE, ...){

  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- ifelse(nargs() == 0, TRUE, FALSE)

  if (gui) {dat=xytable=uniqueid=unionshpnm=stratclip=savedata=parameters=unitarea <- NULL}

  ## Set global variables
  value=count=ACRES_GIS=TOTPIXELCNT <- NULL


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

  ## Spatial Layers: strattype and unittype
  ##################################################################################
  typelst <- c("POLY", "RASTER") 

  ## Check strattype
  ###################################################################
  strattype <- FIESTA::pcheck.varchar(var2check=strattype, varnm="strattype", gui=gui,
	checklst=typelst, caption="Strata type?", stopifnull=TRUE)

  ## Check unittype
  ###################################################################
  unittype <- FIESTA::pcheck.varchar(var2check=unittype, varnm="unittype", gui=gui,
	checklst=typelst, caption="Estimation unit type?", stopifnull=TRUE)

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
  unitarea=stratalut <- NULL
 
  ## Check unitlayer
  unitlayerx <- FIESTA::pcheck.spatial(layer=unit_layer, dsn=unit_dsn, gui=gui, 
	caption="Estimation unit layer?")
  nounit <- ifelse (is.null(unitlayerx), TRUE, FALSE)
  
  if (unittype == "POLY" || nounit) {

    if (strattype == "RASTER") {
      ## Check strat_layer
      stratlayerx <- getrastlst(strat_layer, rastfolder=strat_dsn, stopifnofn=TRUE,
 		stopifLonLat=TRUE)[[1]]
      strvar <- "STRATUM"
      stratlayer.res <- raster::res(stratlayerx)
 
      ## Get number of bands in each raster and set names
      nbands <- raster::nbands(stratlayerx)

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
          unitlayerx@data$ONEUNIT <- 1
          unitvar <- "ONEUNIT"
        }
  
        ## Check projection
        prjdat <- CRScompare(stratlayerx, unitlayerx, nolonglat=TRUE)
        stratlayerx <- prjdat$layer1
        unitlayerprj <- prjdat$layer2

        ## Check extents of unitlayer and stratlayer
        msg <- FIESTA::check.extents(stratlayerx, unitlayerprj, showext, 
		layer1nm="stratlayer", layer2nm="unitlayer")
        if (msg == "non-overlapping extents") stop("msg")

        ## Extract values of polygon unitlayer to points
        extpoly <- spExtractPoly(sppltx, polylst=unitlayerprj, uniqueid=uniqueid, 
		polyvarlst=unitvar)
        sppltx <- extpoly$spplt

        if (!keepnull) 
          sppltx <- sppltx[!is.na(sppltx[[unitvar]]), ]
 
        ## Extract values of raster layer to points
        extrast <- spExtractRast(sppltx, rastlst=stratlayerx, var.name=strvar, 
			uniqueid=uniqueid, exportna=exportshp, outfolder=outfolder,
			overwrite=overwrite)
        sppltx <- extrast$spplt
        pltdat <- extrast$pltdat
        rastfnlst <- extrast$rastfnlst
 
        ## Get pixel counts by estimation unit
        stratalut <- setDT(zonalFreq(src=unitlayerprj, attribute=unitvar, 
			rasterfile=rastfnlst, band=1))
        stratalut.NA <- stratalut[is.na(value), ]
      
        if (!is.null(rast.NODATA)) {
          if (!is.numeric(rast.NODATA)) {
            message("rast.NODATA must be numeric")
          } else {
            stratalut <- stratalut[value != rast.NODATA,]
          }
        }
        stratalut <- stratalut[!is.na(value), ]
        setnames(stratalut, c("zoneid", "value", "zoneprop"), c(unitvar, strvar, "strwt"))

        ## Get unitarea 
        unitlayerprj <- FIESTA::areacalc.poly(unitlayerprj, units=areaunits)
        areavar <- paste0(areaunits, "_GIS")  
        unitarea <- aggregate(unitlayerprj[[areavar]], list(unitlayerprj[[unitvar]]), sum)
        names(unitarea) <- c(unitvar, areavar)
        
      } else {
        stop("under construction...  no unitlayer")

        ## Calculate area
        pixelarea <- areacalc.pixel(stratlayerx, units=areaunits) 

      }
  
    } else if (strattype == "POLY") {
      ## Check stratlayer
      stratlayerx <- FIESTA::pcheck.spatial(layer=strat_layer, dsn=strat_dsn,
		gui=gui, caption="Strata polygons?")

      ## Check strvar
      strvar <- FIESTA::pcheck.varchar(var2check=strvar, varnm="strvar", gui=gui, 
		checklst=names(stratlayerx), caption="Strata variable", 
		warn=paste(strvar, "not in stratlayer"), stopifnull=FALSE)
      if (is.null(strvar)) {
        print(names(stratlayerx))
        stop("must include strvar")
      }

      if (nounit) {
        message("no unitlayer... using stratlayer for estimation unit layer")
        stratlayerx@data$ONEUNIT <- 1
        unitvar <- "ONEUNIT"
        polyvarlst <- c(unitvar, strvar)
        ## Calculate area
        stratpoly <- FIESTA::areacalc.poly(stratlayerx)
      } else {
        polyvarlst <- strvar
        ## Calculate area
        stratpoly <- FIESTA::spUnionPoly(stratlayerx, unitlayerx, areacalc=TRUE)
      }
      stratalut <- aggregate(stratpoly@data$ACRES_GIS, 
				stratpoly@data[, polyvarlst, drop=FALSE], sum)
      names(stratalut) <- c(polyvarlst, "ACRES_GIS")

      areavar <- paste0(areaunits, "_GIS")
      unitarea <- aggregate(stratalut[[areavar]], stratalut[, unitvar, drop=FALSE], sum)
      names(unitarea) <- c(unitvar, areavar)

      ## Extract values of polygon layer to points
      extpoly <- spExtractPoly(sppltx, polylst=stratpoly, uniqueid=uniqueid, 
		polyvarlst=polyvarlst)
      sppltx <- extpoly$spplt
 
      if (!keepnull) 
        sppltx <- sppltx[!is.na(sppltx[[unitvar]]), ]
      pltdat <- sppltx@data
    }
  } else {
    stop("under construction")
  }

  if (exportshp) 
    spExportShape(sppltx, outshpnm=outfn, outfolder=outfolder, overwrite=overwrite)

  if (!is.data.table(stratalut)) stratalut <- setDT(stratalut)
  setkeyv(stratalut, c(unitvar, strvar))

  
  returnlst <- list(pltstrat=sppltx@data, sppltstrat=sppltx, unitarea=setDF(unitarea), 
		unitvar=unitvar, areavar=areavar, stratalut=setDF(stratalut), strvar=strvar)
  if (keepnull) returnlst$stratalut.NA <- stratalut.NA
 
  return(returnlst)
}

