spExtractPoly <- function(xyplt, xyplt_dsn=NULL, uniqueid="PLT_CN", polyvlst, 
	polyv_dsn=NULL, polyvarlst=NULL, polyvarnmlst=NULL, keepNA=FALSE, 
	showext=FALSE, savedata=FALSE, exportsp=FALSE, exportNA=FALSE, out_fmt="shp", 
	out_dsn=NULL, out_layer="polyext", outfolder=NULL, outfn.pre=NULL, 
 	outfn.date=FALSE, overwrite=TRUE, ...){
  ######################################################################################
  ## DESCRIPTION: 
  ## Extracts values from one or more polygon layers and appends to input spatial layer 
  ## or data frame. Points are reprojected on-the-fly to projection of rasters using
  ## PROJ.4 transformation parameters and rgdal spTransform function. Includes options
  ## to use bilinear interpolation or summarize over a window of n pixels using
  ## a specified statistic.
  #########################################################################################

  ## Check for necessary packages
  ###########################################################
  if (!"sf" %in% rownames(installed.packages()))
    stop("sf package is required for spExtractPoly()")
  
  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- ifelse(nargs() == 0, TRUE, FALSE)
  if (gui) showext=savedata=exportsp <- NULL

  ##################################################################
  ## CHECK INPUT PARAMETERS
  ##################################################################

  ## Spatial points for data extraction.. 
  ##################################################################################
#  sppltx <- pcheck.table(xyplt, tab_dsn=xyplt_dsn, tabnm="xyplt", 
#			caption="XY coordinates?", stopifnull=TRUE)
  sppltx <- pcheck.spatial(xyplt, dsn=xyplt_dsn, tabnm="xyplt", 
			caption="XY coordinates?", stopifnull=TRUE)

  if (!"sf" %in% class(sppltx)) { 
    ## Create spatial object from xyplt coordinates
    sppltx <- spMakeSpatialPoints(xyplt=sppltx, uniqueid=uniqueid, 
		exportsp=FALSE, ...)
  } else {
    ## GET uniqueid
    sppltnames <- names(sppltx)
    uniqueid <- FIESTA::pcheck.varchar(var2check=uniqueid, varnm="uniqueid", gui=gui, 
		checklst=sppltnames, caption="UniqueID of spplt", 
		warn=paste(uniqueid, "not in spplt"), stopifnull=TRUE)
  }

  ## Verify polygons
  ########################################################
  if (!is.null(polyvlst) && class(polyvlst) != "list") {
    if ("sf" %in% class(polyvlst) || (methods::canCoerce(polyvlst, "sf"))) {
      polyvlst <- list(polyvlst)
    } else if (is.character(polyvlst)) {
      polyvlst <- as.list(polyvlst) 
    } else {
      stop("polyvlst must be a list object")
    }
  } else if (!"sf" %in% unlist(lapply(polyvlst, class))) {
    stop("invalid list object")
  }
  polyvlst <- lapply(polyvlst, 
		function(layer, polyv_dsn, gui) pcheck.spatial(layer, dsn=polyv_dsn, gui=gui),
 		polyv_dsn, gui)


  ## Check polyvarlst
  if (!is.null(polyvarlst)) {
    if (is.list(polyvarlst)) {
      if (length(polyvlst) != length(polyvarlst))
        stop("the length of polyvarlst must correspond with the length of polyvlst") 
    } else {
      if (length(polyvlst) > 1) {
        stop("polyvarlst must be a list corresponding to the length of polyvlst") 
      } else {  
        polyvarlst <- list(polyvarlst)
      }
    }
  } else {
    polyvarlst <- lapply(polyvlst, function(x) names(x)[!names(x) %in% attr(x, "sf_column")])
    names(polyvarlst) <- names(polyvlst)
  } 

  if (!is.null(polyvarnmlst)) {

    if (length(polyvarlst) != length(polyvarnmlst))
      stop("the length of polyvarnmlst must correspond with the length of polyvarlst") 
    if (!is.list(polyvarnmlst)) 
      polyvarnmlst <- as.list(polyvarnmlst)
    
  } else {
    polyvarnmlst <- polyvarlst
  }

   ## Check showext    
  showext <- FIESTA::pcheck.logical(showext, varnm="showext", 
		title="Plot extents?", first="YES", gui=gui)
 
  ## Check savedata 
  savedata <- FIESTA::pcheck.logical(savedata, varnm="savedata", 
		title="Save data extraction?", first="NO", gui=gui) 

  ## Check exportsp 
  exportsp <- FIESTA::pcheck.logical(exportsp, varnm="exportsp", 
		title="Export spatial layer?", first="NO", gui=gui)  

  ## Check keepNA
  keepNA <- FIESTA::pcheck.logical(keepNA, varnm="keepNA", 
		title="Keep NA values?", first="YES", gui=gui)

  ## Check exportNA
  exportNA <- FIESTA::pcheck.logical(exportNA, varnm="exportNA", 
		title="Export NA values?", first="YES", gui=gui)


  ## Check outfolder
  if (savedata || exportsp || exportNA) {
    overwrite <- FIESTA::pcheck.logical(overwrite, varnm="overwrite", 
		title="Overwrite files?", first="NO", gui=gui)  
    outfn.date <- FIESTA::pcheck.logical(outfn.date , varnm="outfn.date", 
		title="Add date to outfiles?", first="YES", gui=gui)  
    outfolder <- FIESTA::pcheck.outfolder(outfolder, gui)
  }

  ########################################################################
  ### DO THE WORK
  ########################################################################
  NAlst <- list()
  for (i in 1:length(polyvlst)) {
    polyv <- polyvlst[[i]]
    polyvnm <- names(polyvlst)[i]
    if (is.null(polyvnm)) 
      polyvnm <- paste0("poly", i)
 
    ## Check projections of inlayer point layer vs. polygon layer. 
    ## If different, reproject sppltx to polygon projection.
    prjdat <- crsCompare(sppltx, polyv, nolonglat=TRUE) 
    sppltx <- prjdat$x
    polyv <- prjdat$ycrs

    ## Check extents
    bbox1 <- sf::st_bbox(polyv)
    bbox2 <- sf::st_bbox(sppltx)
    check.extents(bbox1, bbox2, showext=showext, layer1nm="polyv", layer2nm="xyplt",
			stopifnotin=TRUE)

    ## Check polyvarlst
    ########################################################  
    polyvars <- polyvarlst[[i]]
    vars2remove <- names(polyv)[!names(polyv) %in% polyvars]
    if (length(vars2remove) == length(names(polyv))) {
       message("polyvarlst is invalid... extracting all variables")
       polyvars <- names(polyv)
    }

    ## Check polyvarnm
    ########################################################  
    polyvarnm <- polyvarnmlst[[i]]
    if (length(polyvarnm) != length(polyvars)) {
      message("number of names does not match number of attributes... using attribute names")
      polyvarnm <- polyvars
      polyvarnmlst[[i]] <- polyvarnm
    }

    ## Change names in polyvarnms that are the same as sppltx
    ########################################################  
    polyvarnm <- suppressWarnings(sapply(polyvarnm, checknm, names(sppltx)))
    polyvarnmlst[[i]] <- polyvarnm


    ## Subset polyv to polyvars
    ########################################################  
    polyv <- polyv[, polyvars]


    ## Change names in polyv that are the same as sppltx
    ########################################################  
    names(polyv)[names(polyv) %in% polyvars] <- polyvarnm

    ## Extract data from polygon
    ######################################################## 
    #sppltext <- sf::st_intersection(sppltx, polyv[, polyvars])
    sppltext <- unique(sf::st_join(sppltx, polyv))

    ## Set polyvarnm
    ########################################################  
    #names(sppltext)[names(sppltext) %in% polyvars] <- polyvarnm 

    ## Check points outside poly
    ########################################################  
    #sppltout <- sppltx[!sppltx[[uniqueid]] %in% sppltext[[uniqueid]],]

    ## Check null values
    ######################################################## 
    geocol <- attr(polyv, "sf_column")
    polyvcols <- names(polyv)[names(polyv) != geocol]
    sppltout <- sppltext[apply(st_drop_geometry(sppltext[, polyvcols]), 1, 
				function(x) all(is.na(x))),]
    nulln <- nrow(sppltout)

    if (nulln > 0) {
      warning(paste("there are", nulln, "null values for", polyvnm))
      NAlst[[polyvnm]] <- sppltout
    }

    if (exportNA)
      spExportSpatial(sppltout, out_dsn=out_dsn, 
		out_layer=paste0(out_layer, "_", polyvnm, "_NAvals"), 
		outfolder=outfolder, outfn.pre=outfn.pre, outfn.date=outfn.date, 
		overwrite_layer=overwrite)
 
    if (!keepNA) {
      ## Subset points inside boundary
      sppltext <- sppltext[!apply(st_drop_geometry(sppltext[, polyvcols]), 1, 
		function(x) all(is.na(x))),]
    }
  }

  if (savedata) 
    write2csv(sppltext, outfolder=outfolder, outfilenm=out_layer, outfn.pre=outfn.pre, 
		outfn.date=outfn.date, overwrite=overwrite)

  ## Export to shapefile
  if (exportsp) 
    spExportSpatial(sppltout, out_dsn=out_dsn, out_layer=out_layer, 
		outfolder=outfolder, outfn.pre=outfn.pre, outfn.date=outfn.date, 
		overwrite_layer=overwrite)
  
  returnlst <- list(sppltext=sppltext, outnames=unlist(polyvarnmlst))

  if (length(NAlst) > 0) 
    returnlst$NAlst <- NAlst

  return(returnlst)
}
