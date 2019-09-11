spGetSAdoms <- function(smallbnd_layer, smallbnd_dsn=NULL, smallbnd.att, 
	smallbnd.name=NULL, smallbnd.filter=NULL, helperbnd_layer=NULL, 
	helperbnd_dsn=NULL, helperbnd.att=NULL, helperbnd.name=NULL, 
	helperbnd.filter=NULL, largebnd_layer=NULL, largebnd_dsn=NULL, 
	largebnd.att=NULL, largebnd.name=NULL, largebnd.filter=NULL, 
	exportshp=FALSE, outfolder=NULL, outfn=NULL, outfn.date=TRUE, 
	overwrite=FALSE) {

  ##############################################################################
  ## DESCRIPTION
  ## Generates small area domains for input to Small Area Module (modSA*).
  ## Small area boundaries are unioned with helper boundaries and aggregated to
  ## unique domains. The following steps are included in function.
  ## 1) Import boundaries
  ## 2) Check projections. If any boundary is projected, get the or most common projection 
  ## 3) Check attributes
  ## 4) Apply filters
  ## 5) Get intersection of helper and/or large boundaries
  ## 6) Append 2 new variables: 
  ## 	  DOMAIN - modeling polygons; AOI - Indicator of small area (1/0)
  ##	  If helperbnd, union helper and small area polygons.
  ##	     DOMAIN = union polygons; AOI=1 if small area polygon.
  ##	  If no helperbnd, 
  ##	     DOMAIN = small area polygons; AOI=1 for all polygons.
  ## 7) Calculate area of dissolved polygons
  ##
  ## VALUE
  ## Spatial Polygons of domain polygons, including DOMAIN and AOI attributes.
  ##############################################################################

  ## Set global variables
  gui <- FALSE
  prj4str <- NULL

  ##################################################################
  ## CHECK INPUT PARAMETERS
  ##################################################################

  ## smallbnd
  #############################################################################
  smallbndx <- pcheck.spatial(smallbnd_layer, smallbnd_dsn, caption="small boundary")
  smallbnd.att <- FIESTA::pcheck.varchar(var2check=smallbnd.att, varnm="smallbnd.att", 
		gui=gui, checklst=names(smallbndx), caption="Small area attribute", 
		warn=paste(smallbnd.att, "not in smallbnd"), stopifnull=TRUE)
  smallbnd.prj <- sp::proj4string(smallbndx)
  prjlst <- smallbnd.prj

  ## Apply filter
  if (!is.null(smallbnd.filter)) {
    FIESTA::check.logic(smallbndx@data, smallbnd.filter, "smallbnd filter")
    smallbndx <- subset(smallbndx, eval(parse(text = smallbnd.filter)))
  }

  ## helperbnd
  ####################################################################
  helperbndx <- pcheck.spatial(helperbnd_layer, helperbnd_dsn, caption="helper boundary")
  if (!is.null(helperbndx)) {
    helperbnd.att <- FIESTA::pcheck.varchar(var2check=helperbnd.att, varnm="helperbnd.att", 
		gui=gui, checklst=names(helperbndx), caption="Helper areas attribute", 
		warn=paste(helperbnd.att, "not in helperbnd"), stopifnull=TRUE)
    helperbnd.prj <- sp::proj4string(helperbndx)
    prjlst <- c(prjlst, helperbnd.prj)
  }
  ## Apply filter
  if (!is.null(helperbndx) && !is.null(helperbnd.filter)) {
    FIESTA::check.logic(helperbndx@data, helperbnd.filter, "helperbnd filter")
    helperbndx <- subset(helperbndx, eval(parse(text = helperbnd.filter)))
  }

  ## largebnd
  ####################################################################
  largebndx <- pcheck.spatial(largebnd_layer, largebnd_dsn, caption="large boundary")
  if (!is.null(largebndx)) {
    largebnd.att <- FIESTA::pcheck.varchar(var2check=largebnd.att, varnm="largebnd.att", 
		gui=gui, checklst=names(largebndx), caption="Large area attribute", 
		warn=paste(largebnd.att, "not in largebnd"), stopifnull=TRUE)
    largebnd.prj <- sp::proj4string(largebndx)
    prjlst <- c(prjlst, largebnd.prj)
  }
  ## Apply filter
  if (!is.null(largebndx) && !is.null(largebnd.filter)) {
    FIESTA::check.logic(largebndx@data, largebnd.filter, "largebnd filter")
    largebndx <- subset(largebndx, eval(parse(text = largebnd.filter)))
  }


  ## Check projections
  ## If any boundary is projected, get the or most common projection 
  #############################################################################
  bndLonLat <- sapply(prjlst, raster::isLonLat)
  notLonLat <- names(bndLonLat)[!bndLonLat] 

  if (length(notLonLat) == 1) {
    prj4str <- notLonLat
  } else if (length(notLonLat) > 1) {
    prj4strTF.12 <- compareCRS(notLonLat[1], notLonLat[2])
    if (prj4strTF.12 && length(notLonLat) > 1) {
      prj4str <- notLonLat[1]
    } else if (length(notLonLat) > 2) {
      prj4strTF.13 <- compareCRS(notLonLat[1], notLonLat[3])
        if (prj4strTF.13) prj4str <- notLonLat[1]
      prj4strTF.23 <- compareCRS(notLonLat[2], notLonLat[3])
        if (prj4strTF.23) prj4str <- notLonLat[2]
    }
  }


  ## Check exportshp
  #############################################################################
  exportshp <- FIESTA::pcheck.logical(exportshp, varnm="exportshp", 
		title="Export to shapefile?", first="NO", gui=gui)  

  if (exportshp) {
    ## Check overwrite 
    overwrite <- FIESTA::pcheck.logical(overwrite, varnm="overwrite", 
		title="Overwrite files?", first="NO", gui=gui)  

    outfolder <- FIESTA::pcheck.outfolder(outfolder, gui)

    if (is.null(outfn)) {
      outfn <- "SAares"
    } else if (!is.character(outfn)) {
      stop("outfn must be character")
    }      
  }


  #############################################################################
  ### DO THE WORK
  #############################################################################

  ## Get intersecting large and helper boundaries
  ####################################################################

  ## Check size of small boundaries
  #smallbnd.area = areacalc.poly(smallbndprj)@data
  #smallbnd.area <- aggregate(smallbnd.area$ACRES_NEW, list(smallbnd.area[[smallbnd.att]]), sum)
  #names(smallbnd.area) <- c(smallbnd.att, "ACRES")
  #smallbnd.area

  ## Get the helper boundary polygons within largebnd
  ####################################################################
  if (!is.null(largebndx)) {
    if (!is.null(helperbndx)) {
      prjdat <- CRScompare(largebndx, helperbndx, prj4str=prj4str, nolonglat=TRUE)
      largebndx <- prjdat$layer1
      helperbndprj <- prjdat$layer2

      ## Get the helper boundary polygons that intersect the large boundary
      helperbndx <- spClipPoly(helperbndprj, largebndx, outfolder="_tmp")
    }
  } else {
    largebndx <- helperbndx
  }

  ## Get the large boundary polygons within helperbnd
  ####################################################################
  if (!is.null(helperbndx)) {
    largebnd.rows <- 
        as.vector(na.omit(row.names(largebndx)[!is.na(sp::over(largebndx, helperbndprj))]))	
    if (length(largebnd.rows) == 0) 
        stop("helper boundary does not overlap large boundary")
    largebnd.ids <- 
        unique(largebndx@data[row.names(largebndx@data) %in% largebnd.rows, largebnd.att]) 
    largebndx <- largebndx[largebndx[[largebnd.att]] %in% largebnd.ids,]
  } 


  #######################################################################
  ## Append 2 new variables: 
  ## DOMAIN - modeling polygons; AOI - Indicator of small area (1/0)
  #######################################################################


  ## Union helper and small area polygons
  #######################################################################
  if (!is.null(helperbndx)) {
    ## Union helperbnd and smallbnd polygons 
system.time(
    SAdoms <- spUnionPoly(helperbndx, smallbndx)
)
#system.time(
#    SAdoms2 <- spUnionPoly(smallbndx, helperbndx)
#)

    ## Add a new column (DOMAIN) with unioned helperbnd.att and smallbnd.att
    SAdoms@data$DOMAIN <- SAdoms@data[[helperbnd.att]]
    SAdoms@data[!is.na(SAdoms@data[[smallbnd.att]]), "DOMAIN"] <- 
		SAdoms@data[!is.na(SAdoms@data[[smallbnd.att]]), smallbnd.att] 

    ## Add a new column (AOI)
    SAdoms@data$AOI <- 0
    SAdoms@data[!is.na(SAdoms@data[[smallbnd.att]]), "AOI"] <- 1

    ## Aggregate (dissolve) polygons (NOTE: Very slow for lots of polygons)
    SAdoms <- raster::aggregate(SAdoms, c("DOMAIN", "AOI"))

  } else {

    ## Add DOMAIN column to all rows
    SAdoms <- smallbndx
    SAdoms@data$DOMAIN <- SAdoms@data[[smallbnd.att]]

    ## Add a new column (AOI)
    SAdoms@data$AOI <- 1

    ## Aggregate (dissolve) polygons (Need to add, but too slow for veg poly)
    if (sum(duplicated(SAdoms$DOMAIN)) > 0) {
      message("aggregating polygons... ")
      ###########################################################################
      SAdoms <- raster::aggregate(SAdoms, c("DOMAIN", "AOI"))   
    }
  }

  ## Calculate area for dissolved polygons
  ###########################################################################
  SAdoms <- areacalc.poly(SAdoms)

  if (exportshp)
    spExportShape(SAdoms, outshpnm=outfn, outfolder=outfolder, 
		outfn.date=outfn.date, overwrite=overwrite)

  return(SAdoms)
}
