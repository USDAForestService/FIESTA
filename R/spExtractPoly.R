spExtractPoly <- function(spplt=NULL, spplt_dsn=NULL, xyplt=NULL, uniqueid="PLT_CN", 
	polylst, poly_dsn=NULL, polyvarlst=NULL, polyvarnmlst=NULL, keepnull=TRUE, 
	showext=FALSE, savedata=FALSE, exportshp=FALSE, outfolder=NULL, outfn=NULL, 
	outfn.date=TRUE, overwrite=FALSE, ...){
  ######################################################################################
  ## DESCRIPTION: 
  ## Extracts values from one or more polygon layers and appends to input spatial layer 
  ## or data frame. Points are reprojected on-the-fly to projection of rasters using
  ## PROJ.4 transformation parameters and rgdal spTransform function. Includes options
  ## to use bilinear interpolation or summarize over a window of n pixels using
  ## a specified statistic.
  #########################################################################################

  if (!"rgeos" %in% rownames(installed.packages()))
    stop("spExtractPoly function requires package rgeos")

  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- ifelse(nargs() == 0, TRUE, FALSE)

  if (gui) shp=xytable=bfun=focalrast=ffun=focalsave=extrtype=db <- NULL


  ## Adds to file filters to Cran R Filters table.
  if (.Platform$OS.type=="windows")
    Filters <- rbind(Filters, shp=c("Shapefiles (*.shp)", "*.shp"))



  ##################################################################
  ## CHECK INPUT PARAMETERS
  ##################################################################

  ### POINTS: shp or xytable for data extraction.. 
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

    ## Check for NA or duplicate values in uniqueid
    if (sum(is.na(sppltx@data[[uniqueid]])) > 0) stop("NA values in ", uniqueid)
    if (length(unique(sppltx@data[[uniqueid]])) < nrow(sppltx@data)) 
      stop("spplt records are not unique")
  }

  ## Verify polygons
  ########################################################
  if (is.null(polylst) && .Platform$OS.type=="windows") {
    db <- FIESTA::pcheck.logical(db, title = "Database?", first = "YES", gui=TRUE)
    if (db) {
      poly_dsn <- choose.files(default=getwd(), caption="Select database")
      layerlst <- rgdal::ogrListLayers(poly_dsn)
      layers <- select.list(layerlst, title="Polygon layers", multiple=TRUE)
      polylst <- lapply(layers, function(x, poly_dsn) pcheck.spatial(x, dsn=poly_dsn),
 			poly_dsn)
    } else {
      polys <- TRUE
      polyfnlst <- {}
      while (polys) {
        polyfnlst <- choose.files(default=poly_dsn, caption="Select polygon layer", 
                filters=Filters["shp",], multi=TRUE)
        if (length(polyfnlst) == 0) stop("")

        polysq <- select.list(c("YES", "NO"), title="More polygon shapefiles?")
        polys <- ifelse(polysq == "YES", TRUE, ifelse(polysq == "NO", FALSE, NULL)) 
        if (is.null(polys)) stop("")
      }
    }
  } else {
    if (class(polylst) != "list" && isS4(polylst)) 
      polylst <- list(polylst)
    polylst <- lapply(polylst, function(x, poly_dsn) pcheck.spatial(x, dsn=poly_dsn),
 		poly_dsn)
  }

  ## Check polyvarlst
  if (!is.null(polyvarlst)) {

    if (is.list(polyvarlst)) {
      if (length(polylst) != length(polyvarlst))
        stop("the length of polyvarlst must correspond with the length of polylst") 
    } else {
      if (length(polylst) > 1) {
        stop("polyvarlst must be a list corresponding to the length of polylst") 
      } else {  
        polyvarlst <- list(polyvarlst)
      }
    }
  } else {
    polyvarlst <- lapply(polylst, function(x) names(x@data))
    names(polyvarlst) <- names(polylst)
  } 

  if (!is.null(polyvarnmlst)) {

    if (length(polyvarlst) != length(polyvarnmlst))
      stop("the length of polyvarnmlst must correspond with the length of polyvarlst") 
    if (!is.list(polyvarnmlst)) 
      polyvarnmlst <- as.list(polyvarnmlst)
    
  } else {
    polyvarnmlst <- polyvarlst
  }

  ## Check for NULL elements in list
  polylst <- polylst[unlist(lapply(polylst, function(x) !is.null(x)))]
  polyvarlst <- polyvarlst[unlist(lapply(polylst, function(x) !is.null(x)))]
  polyvarnmlst <- polyvarnmlst[unlist(lapply(polylst, function(x) !is.null(x)))]

   ## Check showext    
  showext <- FIESTA::pcheck.logical(showext, varnm="showext", 
		title="Plot extents?", first="YES", gui=gui)

  ## Check keepnull
  keepnull <- FIESTA::pcheck.logical(keepnull, varnm="keepnull", 
		title="Keep NULL values?", first="YES", gui=gui)
 
  ### GET savedata 
  savedata <- FIESTA::pcheck.logical(savedata, varnm="savedata", 
		title="Save data extraction?", first="NO", gui=gui)  

  ## GET outfolder 
  ########################################################
  if (savedata || exportshp) {
    outfolder <- FIESTA::pcheck.outfolder(outfolder, gui)

    if (is.null(outfn)) {
      outfn <- "dataext"
    } else if (!is.character(outfn)) {
      stop("outfn must be character")
    } 
  }     

  ########################################################################
  ### DO THE WORK
  ########################################################################
  polyext <- sppltx@data[, uniqueid, drop=FALSE]
  for (i in 1:length(polylst)) {
    polyv <- polylst[[i]]
    sppltext <- sppltx
 
    ## Check projections of inlayer point layer vs. polygon layer. 
    ## If different, reproject inlayer to polygon projection.
    prj <- CRScompare(polyv, sppltext, nolonglat=TRUE) 
    polyv <- prj$layer1
    sppltext <- prj$layer2

    ## Check extents
    msg <- FIESTA::check.extents(polyv, sppltext, showext, layer1nm="poly", layer2nm="spplt")
    if (msg == "non-overlapping extents") stop("msg")

    ## Extract data from polygon
    vlst <- sp::over(sppltext, polyv, returnList=TRUE)
    names(vlst) <- sppltext[[uniqueid]]

    vals <- do.call(rbind, vlst)
    ids <- unlist(lapply(strsplit(row.names(vals), "\\."), '[[', 1))
    vals <- data.frame(ids, vals, stringsAsFactors=FALSE)
    names(vals)[names(vals) == "ids"] <- uniqueid

    ## Check for NULL values
    navals <- sppltext[!sppltext[[uniqueid]] %in% unique(ids),]

    if (is.vector(navals)) { 
      nulln <- length(navals) 
    } else { 
      nulln <- nrow(navals) 
    }
    if (nulln != 0)
      warning(paste("there are", nulln, "null values for poly", i))

    ## Check polyvarlst
    polyvars <- polyvarlst[[i]]
    if (all(polyvars %in% names(vals))) {
      vals <- vals[, c(uniqueid, polyvars)]
    } else {
      notin <- polyvars[!polyvars %in% names(vals)]
      message("polyvarlst names, ", paste(notin, collapse=", "), 
			", do not match data...  extracting all variables")
      polyvars <- names(vals)
    }

#    if (!keepnull && nulln > 0) {
#      polyext <-  merge(polyext, vals, by=uniqueid)
#    } else {
      polyext <-  merge(polyext, vals, by=uniqueid, all.x=TRUE)
#    }

    if (nrow(polyext) == 0) {
      pname <- ifelse(is.null(names(polylst[i])), paste("poly", i), names(polylst)[i])
      stop("check ", pname)
    } else {

      ## Set polyvarnm
      polyvarnm <- polyvarnmlst[[i]]
      if (length(polyvarnm) != length(polyvars)) {
        message("number of names does not match number of attributes... using attribute names")
        polyvarnm <- polyvars
        polyvarnmlst[[i]] <- polyvarnm
      }
      names(polyext)[names(polyext) %in% polyvars] <- polyvarnm
    }
  }
 
  ## Subset sppltx and merge sppltx to polyext
  sppltx <- sp::merge(sppltx, polyext, by=uniqueid)

  if (!keepnull) {
    if (!polyvarlst[[1]][1] %in% names(sppltx)) {
      print(names(sppltx))
      stop(polyvarlst[[1]][1], " not in spplt") 
    }
    sppltx <- sppltx[!is.na(sppltx[[polyvarlst[[1]][1]]]), ]
  }

  if (savedata)
    write2csv(polyext, outfolder=outfolder, outfilenm=outfn, outfn.date=outfn.date,
		overwrite=overwrite)

  if (exportshp)
    ## Output shapefile to outfolder
    FIESTA::spExportShape(sppltx, outshpnm=outfn, outfolder=outfolder, 
		outfn.date=outfn.date, overwrite=overwrite)

  returnlst <- list(spplt=sppltx, pltdat=sppltx@data, outnames=unlist(polyvarnmlst))
 
  #if (length(polyvarnmlst) == 1) {
  #  returnlst$outnames <- polyvarnmlst[[1]]
  #} else { 
  #  returnlst$outnames <- polyvarnmlst 
  #}
  #returnlst$outnames <- unlist(polyvarnmlst)

  return(returnlst)
}
