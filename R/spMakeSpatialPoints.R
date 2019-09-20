spMakeSpatialPoints <- function(xyplt=NULL, uniqueid=NULL, x=NULL, y=NULL, 
	EPSGCD=NULL, prj4str=NULL, prj=NULL, datum=NULL, zone=NULL, zoneS=FALSE, 
	aea.param="USGS", exportshp=FALSE, outfolder=NULL, outshpnm=NULL,
	outfn.date=TRUE, overwrite=FALSE){
  ##############################################################################
  ## DESCRIPTION:
  ## Generates an S4 SpatialPoints or SpatialPointsDataFrame object with defined 
  ## projection from a data table or matrix including X and Y coordinates, with 
  ## option to export as an ArcGIS shapefile (*.shp).
  ##############################################################################

  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- ifelse(nargs() == 0, TRUE, FALSE)


  ##################################################################
  ## CHECK INPUT PARAMETERS
  ##################################################################

  ## check xyplt
  xypltx <- FIESTA::pcheck.table(xyplt, tabnm="xyplt", "XY data table")
  if (is.null(xypltx))
    stop("must include data table with X and Y coordinates")
  if (!is.data.table(xypltx)) stop("xyplt is invalid")

  ## check uniqueid
  xypltnmlst <- names(xypltx)

  uniqueid <- FIESTA::pcheck.varchar(var2check=uniqueid, varnm="uniqueid", 
		checklst=xypltnmlst, caption="UniqueID variable - xyplt", 
		warn="uniqueid not in xyplt", gui=gui, stopifnull=TRUE)

  ## check for NA or duplicate values
  if (sum(is.na(xypltx[[uniqueid]])) > 0) stop("NA values in ", uniqueid)
  if (length(unique(xypltx[[uniqueid]])) < nrow(xypltx)) 
    warning("plt records are not unique")


  ## check xvar and yvar
  x <- FIESTA::pcheck.varchar(x, varnm="x", checklst=xypltnmlst, 
		caption="X variable", gui=gui)
  y <- FIESTA::pcheck.varchar(y, varnm="y", checklst=xypltnmlst, 
		caption="Y variable", gui=gui)

  ## check if x = y
  if (x == y) stop("x and y are the same value")


  ## set x and y variables to numeric
  if (!is.numeric(xypltx[[x]])) xypltx[[x]] <- as.numeric(xypltx[[x]])
  if (!is.numeric(xypltx[[y]])) xypltx[[y]] <- as.numeric(xypltx[[y]])

  ## check for NA values
  if (any(is.na(xypltx[[x]])) || any(is.na(xypltx[[y]]))) {
    missCN <- xypltx[list(NA,NA), on=c(x,y), "CN", nomatch=0]
    rowp <- ifelse(length(missCN) == 1, "row", "rows")
    message(paste("removing", length(missCN), rowp, "with NA values:", 
		paste(missCN, collapse=", ")))
    xypltx <- xypltx[!list(NA,NA), on=c(x,y)]
  }

  ## check EPSGCD
  if (!is.null(EPSGCD)) {
    EPSG <- rgdal::make_EPSG()
    if (!EPSGCD %in% EPSG[["code"]]) stop("EPSGCD is invalid")

    prj4str <- na.omit(EPSG[EPSG[["code"]] == EPSGCD, "prj4"])[[1]]
  }

  ## check prj4str   
  if (is.null(prj4str)) {
    prj4str <- FIESTA::build.prj4str(prj=prj, datum=datum, zone=zone, 
		zoneS=zoneS, aea.param=aea.param, gui=gui)
  } else {
    #if (!grepl("+proj", prj4str)) stop("prj4str missing +proj")

    if (!rgdal::checkCRSArgs(prj4str)[[1]]) stop("invalid prj4str")
    if (!grepl("+datum", prj4str)) warning("prj4str missing +datum")
  }
    
  ### GET savedata 
  exportshp <- FIESTA::pcheck.logical(exportshp, varnm="exportshp", 
		title="Export shapefile?", first="NO", gui=gui)


  ##################################################################
  ## DO WORK
  ##################################################################

  ## Make uniqueid a character
  xypltx[[uniqueid]] <- as.character(xypltx[[uniqueid]])

  ## Generate SpatialPoints object
  xypltx$x <- xypltx[[x]]
  xypltx$y <- xypltx[[y]]
  sp::coordinates(xypltx) <- c("x", "y")
  sp::proj4string(xypltx) <- sp::CRS(prj4str)


  if (exportshp) 
    FIESTA::spExportShape(xypltx, uniqueid=uniqueid, outfolder=outfolder, 
		outshpnm=outshpnm, outfn.date=outfn.date, overwrite=overwrite)
    
  return(xypltx)
}


