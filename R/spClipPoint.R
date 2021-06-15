spClipPoint <- function(xyplt, xyplt_dsn=NULL, xy.uniqueid="PLT_CN", 
	clippolyv, clippolyv_dsn=NULL, clippolyv.filter=NULL, showext=FALSE, 
	keepNA=FALSE, savedata=FALSE, returnsp=TRUE, exportsp=FALSE, 
	outfolder=NULL, out_fmt="shp", out_dsn=NULL, out_layer="pnt", 
	outfn.pre=NULL, outfn.date=FALSE, overwrite=FALSE, 
	othertabnms=NULL, stopifnotin=TRUE, ...) {

  ###################################################################################
  ## DESCRIPTION: 
  ## Clip (intersect) point vector layer with polygon vector layer. 
  ###################################################################################

  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- ifelse(nargs() == 0, TRUE, FALSE)
  if (gui) xyplt=xy.uniqueid=exportsp <- NULL

  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- c(names(formals(FIESTA::spClipPoint)), 
		names(formals(FIESTA::spMakeSpatialPoints)))
  if (!all(input.params %in% formallst)) {
    miss <- input.params[!input.params %in% formallst]
    stop("invalid parameter: ", toString(miss))
  }


  ##################################################################
  ## CHECK INPUT PARAMETERS
  ##################################################################
  intabs <- NULL

  ## Spatial points for clipping.. 
  ##################################################################################
  sppntx <- pcheck.spatial(xyplt, dsn=xyplt_dsn, tabnm="xyplt", 
			caption="XY coordinates?", stopifnull=TRUE)
 
  if (!"sf" %in% class(sppntx)) { 
    ## Create spatial object from xyplt coordinates
    sppntx <- spMakeSpatialPoints(sppntx, xy.uniqueid=xy.uniqueid, 
		exportsp=FALSE, ...)
  } else {
    ## GET xy.uniqueid
    sppntnames <- names(sppntx)
    xy.uniqueid <- FIESTA::pcheck.varchar(var2check=xy.uniqueid, 
		varnm="xy.uniqueid", gui=gui, 
		checklst=sppntnames, caption="UniqueID of xyplt", 
		warn=paste(xy.uniqueid, "not in xyplt"), stopifnull=TRUE)
  }

  ###################################################################################
  ##  STEP #2: GET INLAYER BY CLIPPING TO BOUNDARY IF NECESSARY   
  ###################################################################################

  ## Check polyvx
  clippolyvx <- pcheck.spatial(clippolyv, dsn=clippolyv_dsn, gui=gui, 
				tabnm="clippoly", caption="Clipping polygon?", stopifnull=TRUE)
    
  ## clippolyv.filter
  clippolyvx <- datFilter(clippolyvx, xfilter=clippolyv.filter, stopifnull=TRUE)$xf

   ## Check showext    
  showext <- FIESTA::pcheck.logical(showext, varnm="showext", 
		title="Plot extents?", first="YES", gui=gui)
  ## Check returnsp
  returnsp <- FIESTA::pcheck.logical(returnsp, "Return spatial object?", "YES", gui=gui)

  ## Check keepNA
  keepNA <- FIESTA::pcheck.logical(keepNA, "Keep null values?", "NO", gui=gui)

  ## Check exportsp
  exportsp <- FIESTA::pcheck.logical(exportsp, "Export spatial?", "YES", gui=gui)


  if (!is.null(othertabnms) && !is.character(othertabnms)) 
    stop("othertabnms must be a string vector of object or file names")


  ##################################################################
  ## DO WORK
  ##################################################################
  ## Check projections. Reproject points to clippolyv projection.
  prjdat <- crsCompare(sppntx, clippolyvx, nolonglat=TRUE)
  sppntx <- prjdat$x
  clippolyvx <- prjdat$ycrs

  ## Check extents
  bbox1 <- sf::st_bbox(clippolyvx)
  bbox2 <- sf::st_bbox(sppntx)
 
  ## Check if extents overlap... if not and stopifnotin=TRUE, return NULL
  chk <- check.extents(bbox1, bbox2, showext, layer1nm="polyv", layer2nm="sppntx",
			stopifnotin=stopifnotin, quiet=TRUE)
  if (is.null(chk)) return(NULL)
 

  ## Clip points that intersect polygon
  injoin <- sf::st_join(sppntx, clippolyvx, join=st_intersects, left=FALSE)
  inpnts <- sppntx[sppntx[[xy.uniqueid]] %in% injoin[[xy.uniqueid]],]

  if (showext) {
    plot(sf::st_geometry(clippolyvx))
    plot(sf::st_geometry(inpnts), add=TRUE, col="blue", cex=.25)
  }

  ## Get outside points
  if (keepNA)
    outpnt <- sppntx[!sppntx[[xy.uniqueid]] %in% injoin[[xy.uniqueid]],]

  ## Clip othertables
  if (!is.null(othertabnms)) {
    if (!all(sapply(othertabnms, exists))) {
      miss <- othertabnms[which(!sapply(othertabnms, exists))]
      stop("invalid othertabnms: ", paste(miss, collapse=", "))
    }
    othertabs <- lapply(othertabnms, function(x) get(x, envir=environment()))
    intabs <- clip.othertables(inpnts[[xy.uniqueid]], othertabnms=othertabnms,
		othertabs=othertabs, savedata=savedata, outfn.pre=outfn.pre, 
		outfolder=outfolder, out_dsn=out_dsn, outfn.date=outfn.date, 
		overwrite=overwrite)
  } 

  ## Write data to outfolder
  if (exportsp) {
    if (out_fmt == "shp" && nrow(inpnts) > length(unique(inpnts[[xy.uniqueid]])))
      message("cannot export shapefile... more than 1 record per xy.uniqueid")
    spExportSpatial(inpnts, out_layer=out_layer, out_dsn=out_dsn, 
		out_fmt=out_fmt, outfolder=outfolder, outfn.pre=outfn.pre, 
		outfn.date=outfn.date, overwrite_layer=overwrite, append_layer=TRUE) 
 
    spExportSpatial(clippolyvx, out_layer="bnd", out_dsn=out_dsn, 
		out_fmt=out_fmt, outfolder=outfolder, outfn.pre=outfn.pre, 
		outfn.date=outfn.date, overwrite_layer=overwrite, append_layer=TRUE)       
  } 

  if (savedata)
    write2csv(inpnts, outfolder=outfolder, outfilenm=out_layer,
		outfn.pre=outfn.pre, outfn.date=outfn.date, overwrite=overwrite)
   		

  if (!returnsp) inpnts <- sf::st_drop_geometry(inpnts)
  returnlst <- list(clip_xyplt=inpnts, xy.uniqueid=xy.uniqueid, clip_polyv=clippolyvx)
  if (!is.null(intabs)) returnlst$clip_tabs <- intabs
  
  return(returnlst)

}

