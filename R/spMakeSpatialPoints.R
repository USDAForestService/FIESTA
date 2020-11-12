spMakeSpatialPoints <- function(xyplt, xyplt_dsn=NULL, xy.uniqueid=NULL, 
	xvar=NULL, yvar=NULL, xy.crs=4269, prj=NULL, datum=NULL, zone=NULL, 
	zoneS=FALSE, aea.param="USGS", addxy=FALSE, exportsp=FALSE, ...){
  ##############################################################################
  ## DESCRIPTION:
  ## Generates sf points object with defined projection.
  ## Arguments
  ## crs - can be EPSG or PROJ.4 string
  ## ... - Arguments passed to spExportSpatial
  ##############################################################################
  if (!"sf" %in% rownames(installed.packages()))
    stop("spMakeSpatialPoints function requires package sf")

  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- ifelse(nargs() == 0, TRUE, FALSE)

  ##################################################################
  ## CHECK INPUT PARAMETERS
  ##################################################################
  ## check xyplt
  xypltx <- pcheck.table(xyplt, xyplt_dsn, tabnm="xyplt", 
		caption="XY data table", stopifnull=TRUE, returnDT=FALSE)

  ## check xy.uniqueid
  xypltnmlst <- names(xypltx)
  xy.uniqueid <- FIESTA::pcheck.varchar(var2check=xy.uniqueid, varnm="xy.uniqueid", 
		checklst=xypltnmlst, caption="UniqueID variable - xyplt", 
		warn="xy.uniqueid not in xyplt", gui=gui, stopifnull=TRUE)

  ## check for NA or duplicate values
  if (sum(is.na(xypltx[[xy.uniqueid]])) > 0) stop("NA values in ", xy.uniqueid)
  if (length(unique(xypltx[[xy.uniqueid]])) < nrow(xypltx)) 
    warning("plt records are not unique")

  ## check xvar and yvar
  x <- FIESTA::pcheck.varchar(xvar, varnm="xvar", checklst=xypltnmlst, 
		caption="X variable", gui=gui, stopifnull=TRUE)
  y <- FIESTA::pcheck.varchar(yvar, varnm="yvar", checklst=xypltnmlst, 
		caption="Y variable", gui=gui, stopifnull=TRUE)

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

  ## check xy.crs   
  if (is.null(xy.crs))
    xy.crs <- build.prj4str(prj=prj, datum=datum, zone=zone, 
		zoneS=zoneS, aea.param=aea.param, gui=gui) 
    
  ### check exportsp
  exportsp <- FIESTA::pcheck.logical(exportsp, varnm="exportsp", 
		title="Export spatial layer?", first="NO", gui=gui)

  ### check addxy
  addxy <- FIESTA::pcheck.logical(addxy, varnm="addxy", 
		title="Add xy variables?", first="NO", gui=gui)

  ##################################################################
  ## DO WORK
  ##################################################################

  ## Make xy.uniqueid a character
  xypltx[[xy.uniqueid]] <- as.character(xypltx[[xy.uniqueid]])
 

  ## Generate sf layer  
  spplt <- sf::st_as_sf(xypltx, coords=c(x,y), crs=xy.crs, 
		stringsAsFactors=FALSE, agr="identity")
  if (is.na(sf::st_crs(spplt))) stop("invalid crs: ", xy.crs) 
  

  ## Add coordinates
  if (addxy) {
    xy.coords <- data.frame(sf::st_coordinates(spplt))
    names(xy.coords) <- c(x,y)
    spplt <- sf::st_sf(data.frame(spplt, xy.coords)) 
  }

  if (exportsp) 
    spExportSpatial(spplt, ...)    
  return(spplt)
}


