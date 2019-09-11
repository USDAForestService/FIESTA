spClipPoint <- function(spplt=NULL, spplt_dsn=NULL, xyplt=NULL, uniqueid="PLT_CN", 
	clippolyv_layer, clippolyv_dsn=NULL, showext=FALSE, keepnull=FALSE, 
	savedata=FALSE, outfolder=NULL, outfn=NULL, outfn.pre=NULL, outfn.date=FALSE, 
	overwrite=FALSE, othertabnms=NULL, ...) {
  ###################################################################################
  ## DESCRIPTION: 
  ## Clip SpatialPoints or point file with polygon Spatial boundary. 
  ###################################################################################

  if (!"rgeos" %in% rownames(installed.packages()))
    stop("spClipPoint function requires package rgeos")


  ## Adds to file filters to Cran R Filters table.
  if (.Platform$OS.type=="windows") {
    Filters <- rbind(Filters, shp=c("Shapefiles (*.shp)", "*.shp"))
    Filters <- rbind(Filters, csv=c("Comma-delimited files (*.csv)", "*.csv")) }


  ##################################################################
  ## CHECK INPUT PARAMETERS
  ##################################################################
  intabs <- NULL

  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- ifelse(nargs() == 0, TRUE, FALSE)
  if (gui) shp=xytable=uniqueid=savedata <- NULL

  ## Spatial points: shp or xytable for data extraction.. 
  ##################################################################################
  if (is.null(spplt)) {
    ## Check parameters
    if (is.null(xyplt)) stop("either include spplt or xyplt for data extraction")
         
    ## Create spatial object from xyplt coordinates
    sppltx <- FIESTA::spMakeSpatialPoints(xyplt=xyplt, uniqueid=uniqueid, ...)
  } else {
    ## Check spplt
    sppltx <- FIESTA::pcheck.spatial(layer=spplt, dsn=spplt_dsn, gui=gui,
		caption="Spatial points with XY coords?")
    if (class(sppltx) != "SpatialPointsDataFrame") 
      stop("spplt must be a SpatialPointsDataFrame")
    if (is.null(sp::proj4string(sppltx))) stop("spplt must have defined projection")

    ## GET uniqueid
    sppltnames <- names(sppltx@data)
    uniqueid <- FIESTA::pcheck.varchar(var2check=uniqueid, varnm="uniqueid", gui=gui, 
		checklst=sppltnames, caption="UniqueID of spplt", 
		warn=paste(uniqueid, "not in spplt"))
  }
  
  ###################################################################################
  ##  STEP #2: GET INLAYER BY CLIPPING TO BOUNDARY IF NECESSARY   
  ###################################################################################

  ## Check polyvx
  clippolyvx <- FIESTA::pcheck.spatial(layer=clippolyv_layer, dsn=clippolyv_dsn, 
	gui=gui, caption="Clipping polygon?")


   ## Check showext    
  showext <- FIESTA::pcheck.logical(showext, varnm="showext", 
		title="Plot extents?", first="YES", gui=gui)

  ## Check savedata
  savedata <- FIESTA::pcheck.logical(savedata, "Save data?", "YES", gui=gui)

  ## Check exportshp
  #exportshp <- FIESTA::pcheck.logical(exportshp, "Export shapefile?", "YES", gui=gui)

  ## Check outfolder
  #if (savedata || exportshp) {
  if (savedata) {
    outfolder <- FIESTA::pcheck.outfolder(outfolder, gui)

    if (is.null(outfn)) outfn <- "xy"
    if (is.null(outfn.pre)) outfn.pre <- "clip"
    outfn <- paste(outfn.pre, outfn, sep="_")
  }

  ##################################################################
  ## DO WORK
  ##################################################################
  
  ## Check projections. Reproject points to clippoly projection.
  prjdat <- FIESTA::CRScompare(clippolyvx, sppltx, nolonglat=TRUE)
  clippolyvx <- prjdat$layer1
  sppltprj <- prjdat$layer2


  ## Check extents
  msg <- FIESTA::check.extents(clippolyvx, sppltprj, showext, layer1nm="clippolyv", 
			layer2nm="spplt")
  if (msg == "non-overlapping extents") stop("msg")


  ## Clip points that intersect polygon
  inpoly <- unlist(rgeos::gCovers(clippolyvx, sppltprj, byid=TRUE, returnDense=FALSE))

  if (length(inpoly) == 0) {
    return(NULL)
  } else {
    inpnts <- sppltprj[row.names(sppltprj) %in% inpoly,]

    if (keepnull) 
      outpnts <- sppltprj[!row.names(sppltprj) %in% inpoly,]
  }
  inpntids <- inpnts@data[[uniqueid]] 


  ## Clip othertables
  if (!is.null(othertabnms)) {
    if (!all(sapply(othertabnms, exists))) {
      miss <- othertabnms[which(!sapply(othertabnms, exists))]
      stop("invalid othertabnms: ", paste(miss, collapse=", "))
    }
    othertabs <- lapply(othertabnms, function(x) get(x, envir=environment()))
    intabs <- FIESTA::clip.othertables(inpntids, othertabnms=othertabnms,
		othertabs=othertabs, savedata=savedata, outfolder=outfolder, 
		overwrite=overwrite, outfn.pre=outfn.pre, outfn.date=outfn.date)
  } 
 
  ## Write data to outfolder
  if (savedata) {
    if (nrow(inpnts) > length(unique(inpnts@data[,uniqueid]))) {
      message("cannot export shapefile... more than 1 record per uniqueid")
    } else {
      FIESTA::spExportShape(inpnts, outfolder=outfolder, outshpnm=outfn,
			overwrite=overwrite, outfn.date=outfn.date)
    }
    FIESTA::write2csv(inpnts, outfolder=outfolder, outfilenm=outfn,
			outfn.date=outfn.date, overwrite=overwrite)
  }
  
  returnlst <- list(clip_spplt=inpnts, clip_xyplt=inpnts@data, clip_poly=clippolyvx)
  if (keepnull) returnlst$sppltnull <- outpnts 
  if (!is.null(intabs)) returnlst$clip_tabs <- intabs 
  
  return(returnlst)

}

