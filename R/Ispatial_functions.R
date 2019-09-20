## getrastlst - verifies a list of raster or raster files.
## pcheck.spatial - checks or gets Vector layer from file name or spatial object
## getRaster - checks or gets Raster layer from file name or spatial object
## build.prj4str	- builds PROJ.4 projections string from input parameters
## trunc10shp - truncates all variables in Spatial object to 10 characters of less
## getESPG - gets table of ESPG codes, filtering with input parameters
## check.extents - check extents of 2 spatial layers.
## getprjatt - gets the desired attribute from the proj4string.
## areacalc.poly - calculates area of polygons and appends to polygon attribute table.
## areacalc.pixel - calculates area of raster pixels and appends to polygon attribute table.
## aspect_transform - transforms aspect, in degrees, to easting and northing units.
## spPlotRastcl - plot raster discrete classes
## writeESRIprj - Adds *.prj file to folder with *.bil file. 
## CRScompare - compares projections, if different, projects layer2 layer1 projection.


getrastlst <- function(rastnmlst, rastfolder=NULL, filenames=FALSE, rast.prjstr=NULL, 
	stopifnofn=FALSE, stopifLonLat=FALSE, gui=TRUE, nlayers=FALSE){

  #########################################################################
  ## DESCRIPTION: 
  ## To verify rasters. Checks if rasters exist. If rastfolder is not NULL,
  ## checks for rasters in the rastfolder.
  ## 
  ## ARGUMENTS:
  ## rastnmlst   RasterLayer, RasterStack, RasterBrick, or String vector. 
  ##		List of raster names. Extensions included.
  ## rastfolder  String. The name of the folder where rasters are. Optional.
  ## filenames	Logical. If TRUE, the raster filenames are returned.
  ##
  ## VALUE: 
  ## rastlst  String vector. List of rasters.
  ##########################################################################


  ## Adds to file filters to Cran R Filters table.
  if (.Platform$OS.type == "windows") {
    Filters <- rbind(Filters,img=c("Erdas Imagine Images (*.img)", "*.img"))
    Filters <- rbind(Filters,img=c("GeoTiff (*.tif)", "*.tif"))
  }

  rastfnlst <- {}
  rastlst <- {}
#  nlayer <- 0
  if (is.null(rastnmlst)) {
    if (gui) {
      if (is.null(rastfolder)) {
        rastfolder <- getwd()
      } else {
        if (!file.exists(rastfolder)){
          warning("the raster folder does not exist - check path")
          rastfolder <- getwd()
        }
      }
    } else {
      if (stopifnofn) {
        stop("rastlst is NULL")
      } else {
        return(NULL)        
      }
    }
        
    rasts <- TRUE
    while (rasts) {
      rasttype <- select.list(c("Erdas Imagine Image", "GeoTIFF", "Arc/Info GRID"), 
		title="Raster Type")

      if (rasttype %in% c("Erdas Imagine Image", "GeoTIFF") && .Platform$OS.type=="windows") {
        rastnm <- choose.files(default=rastfolder, caption="Select raster image(s)", 
                filters=Filters[c("img", "tif"),], multi=TRUE)
        if (length(rastnm) == 0) {
          stop("")
        } else {
          if (raster::nbands(raster(rastnm)) > 1) { 
            rast <- raster::stack(rastnm)
          } else {
            rast <- raster(rastnm)
          }
          rastlst <- c(rastlst, rast)
          rastfnlst <- c(rastfnlst, rastnm) 
        }
      } else if (rasttype == "Arc/Info GRID" && .Platform$OS.type=="windows") {
        rastnm <- choose.dir(default=rastfolder, caption="Select raster grid")
        if (is.na(rastnm)) { 
          stop("")
        } else {
          if (nbands(raster(rastnm)) > 1) { 
            rast <- raster::stack(rastnm)
          } else {
            rast <- raster(rastnm)
          }
          rastlst <- c(rastlst, rast)
          rastfnlst <- c(rastfnlst, rastnm) 
        }
      } else {
        stop("")
      }
      rastfolder <- dirname(rastnm)
    }
  } else {  ## !is.null(rastnmlst)

    if (isS4(rastnmlst)) {
      if (!class(rastnmlst) %in% c("RasterLayer", "RasterStack", "RasterBrick")) {
        stop("rastnmlst must be RasterLayer, RasterStack, or RasterBrick")
      } else {
        if (filenames || stopifnofn) {
          rastfnlst <- rastnmlst[[1]]@file@name
          #nofn <- rastnmlst[which(rastfnlst == "")]
          if (any(rastfnlst == "")) {
            if (raster::inMemory(rastnm)) {
              msg <- "check raster... must be written to file, not in memory"
            } else {
              msg <- "no filename for raster"
            }
            if (stopifnofn) {
              stop(msg)
            } else {
              message(msg)
            }
          }
        } 
        rastlst <- c(rastlst, rastnmlst)
      }
      rastLonLat <- lapply(rastlst, raster::isLonLat)
      if (sum(unlist(rastLonLat)) > 0 && stopifLonLat) 
        stop("check for projected rasters... cannot be in longlat")
    } else { 
      if (class(rastnmlst) != "list") rastnmlst <- as.list(rastnmlst)
      for (i in 1:length(rastnmlst)) {
        rastnm <- rastnmlst[[i]]
        if (isS4(rastnm)) {
          if (!class(rastnm) %in% c("RasterLayer", "RasterStack", "RasterBrick")) {
            stop("rastnm must be RasterLayer, RasterStack, or RasterBrick")
          } else {
            if (filenames || stopifnofn) {
              rastfn <- rastnm[[1]]@file@name
              if (any(rastfn == "")) {
                if (raster::inMemory(rastnm)) {
                  msg <- "check raster... must be written to file, not in memory"
                } else {
                  msg <- "no filename for raster"
                }
                if (stopifnofn) {
                  stop(msg)
                } else {
                  message(msg)
                }
              } else {
                rastfnlst <- c(rastfnlst, rastfn)
              }
            } 
            rastlst <- c(rastlst, rastnm)
 #           nlayer <- nlayers + raster::nbands(rastnm)
          }
          rastLonLat <- lapply(rastlst, raster::isLonLat)
          if (sum(unlist(rastLonLat)) > 0 && stopifLonLat) stop("check for projected rasters")
        } else if (is.character(rastnm)) {
          if (!is.null(rastfolder))
            rastnm <- paste(rastfolder, rastnm, sep="/")          
          if (!file.exists(rastnm)) 
            stop("file does not exist: ", rastnm)
           
#          nlayer <- nlayer + raster::nbands(raster(rastnm))
          if (raster::nbands(raster(rastnm)) > 1) { 
            rast <- stack(rastnm)
          } else {
            rast <- raster(rastnm)
          }
          rastlst <- c(rastlst, rast)
          rastfnlst <- c(rastfnlst, rastnm)
        } else {
          stop("invalid raster name")
        }
      }
    }
  }
  proj.na <- is.na(lapply(rastlst, sp::proj4string))
  if (any(proj.na)) {
    if (!is.null(rast.prjstr)) {
      rastnames <- unlist(lapply(rastlst[proj.na], names))
      lapply(rastlst[proj.na], function(x, prjstr) {sp::proj4string(x) <- rast.prjstr}, rast.prjstr)
      message("setting projection of ", paste(rastnames, collapse=", "), " to: ", rast.prjstr)
    }
  }
  
  if (filenames) {
#    if (nlayers) return(list(rastfnlst=rastfnlst, nlayer=nlayer))
    return(rastfnlst)
  } else {
#    if (nlayers) return(list(rastfnlst=rastlst, nlayer=nlayer))
    return(rastlst)
  }
}


pcheck.spatial <- function(layer=NULL, dsn=NULL, caption=NULL, checkonly=FALSE,
		stopifnull=FALSE, gui=FALSE) {
  ## DESCRIPTION: checks or gets Vector layer from file name or spatial object.
  ## ARGUMENTS:
  ## layernm  	String. The name of the spatial layer file. 
  ## caption  	String. The text to add for gui window caption.
  ## checkonly	Logical. If TRUE, check layer only, do not return.

  ## Adds to file filters to Cran R Filters table.
  if (.Platform$OS.type == "windows") {
    Filters <- rbind(Filters,shp=c("Shapefiles (*.shp)", "*.shp"))
    Filters <- rbind(Filters,gpkg=c("GeoPackages (*.gpkg)", "*.gpkg"))
  }

  if (is.null(caption)) caption <- ""
 
  if (is.null(layer)) {
    if (gui && .Platform$OS.type=="windows") {
      ## Need to fix this part
      layertypelst <- c("ESRI Shapefile", "ESRI File Geodatabase", "GeoPackage") 
      stop("invalid layer")

      if (gui && .Platform$OS.type=="windows") {
        layernm <- choose.files(caption=paste("Select shapefile.", 
		caption), filters=Filters[c("shp", "gpkg")], multi=FALSE)
        if (length(layernm) == 0) stop("invalid shapefile") 
      } else if (stopifnull) {
        stop("invalid shapefile") 
      } else {
        return(NULL)
      } 
      layer <- rgdal::readOGR(dsn=dsn, layer=FIESTA::basename.NoExt(dsn),
		stringsAsFactors=FALSE, drop_unsupported_fields=TRUE)
    } else {
      return(NULL)
    }
  } else if (isS4(layer)) {
    if (!grepl("Spatial", class(layer))) {
      stop(paste(layer, "is not a Spatial layer")) 
    } else {
      splayer <- layer
    }
#  } else if (grepl("Raster", class(dsn))) {
#    stop("raster is an invalid dsn")
#    message("using extent of raster layer")
#    dsn <- as(extent(dsn), "SpatialPolygonsDataFrame")
  } else if (is.character(layer)) {
    if (is.null(dsn)) {
      if (file.exists(layer)) {
        dsn <- dirname(layer)
        layer <- FIESTA::basename.NoExt(layer)
      } else {
        stop(layer, " is invalid")
      }
    } else if (!file.exists(dsn)) {
      stop(dsn, " is invalid")
    }

    if (!checkonly) {
      layer <- FIESTA::basename.NoExt(layer)
      splayer <- rgdal::readOGR(dsn=dsn, layer=layer, stringsAsFactors=FALSE, 
			drop_unsupported_fields=TRUE)
    }
  }
        

  if (!checkonly) {
    if (is.na(sp::proj4string(splayer))) 
      warning("projection is not defined for: ", dsn)
    return(layer=splayer)

  } else {
    return(list(layer=layer, dsn=dsn))
  }
}

getRaster <- function(layernm=NULL, isgrid=FALSE, caption=NULL, checkonly=FALSE, gui=FALSE){
  ## DESCRIPTION: checks or gets Raster layer from file name or spatial object.
  ## ARGUMENTS:
  ## layernm  String. The name of the spatial layer file. 
  ## caption  String. The text to add for gui window caption.
  ## checkonly	Logical. If TRUE, check layer only, do not return.

  ## Adds to file filters to Cran R Filters table.
  if (.Platform$OS.type == "windows")
    Filters <- rbind(Filters,img=c("Erdas Imagine Images (*.img)", "*.img"))
 

  if (is.null(caption)) caption <- ""

  if (class(layernm) %in% c("RasterLayer", "RasterStack", "RasterBrick")) {
    layer <- layernm      
  } else if (isgrid) {
    if (is.null(layernm) && .Platform$OS.type=="windows") {
      layernm <- choose.dir(getwd(), paste("Select raster grid.", caption))
      if (is.na(layernm)) stop("invalid grid name")
    } else if (is.character(layernm) && .Platform$OS.type=="windows") {
      if (!file.exists(layernm)) {
        layernm <- choose.dir(getwd(), 
			paste("Raster grid does not exist. Select raster grid.", caption))
        if (is.na(layernm)) stop("invalid grid name")
      } else {
        stop("grid file does not exist")
      }
    } else {
      stop("invalid grid")
    }
    if (!checkonly) layer <- raster::raster(layernm)

  } else {
    if (is.null(layernm) && .Platform$OS.type=="windows") {
      layernm <- choose.files(default=getwd(), caption=paste("Select raster image", 
		caption), filters=Filters["img",], multi=FALSE)
      if (layernm == "") stop("invalid image name")
    } else if (.Platform$OS.type=="windows") { 
      if (is.character(layernm)) {
        if (!file.exists(layernm)) {
          if (file.exists(dirname(layernm))) { 
            warning("check layer name") 
            layerwd <- dirname(layernm)
          } else {
            warning("check path")
            layerwd <- getwd()
          }
          layernm <- choose.files(default=layerwd, caption=paste("Invalid layer name. Select", 
            caption, "image."), filters=Filters["img",], multi=FALSE)
          if (layernm == "") stop("")  
        }
      }
    }
    if (!checkonly) layer <- raster::raster(layernm)
  }

  if (!checkonly) {
    if (is.na(sp::proj4string(layer))) 
      stop("must define projection for", layernm)
    return(layer)

  } else {
    return(layernm)
  }
}

build.prj4str <- function(prj, datum, zone=NULL, zoneS=FALSE, aea.param="USGS",
	gui=FALSE) {

  #######################################################################
  ## DESCRIPTION: 
  ## Builds the proj4string from input parameters.
  ##
  ## ARGUMENTS:
  ## prj - String. Projection
  ## datum - String. Datum
  ## zone - String. If prj="utm", UTM zone
  ## zoneS - Logical. If prj="utm", if UTM zone is in Southern hemisphere
  ## aea.param - If prj="aea", parameters
  ####################################################################### 

  ## Set variable lists
  prjlst <- as.character(rgdal::projInfo(type="proj")$name)
  datumlst <- as.character(rgdal::projInfo(type="datum")$name)
  ellpslst <- as.character(rgdal::projInfo(type="ellps")$name)
  zonelst <- c(1:60)


  prj <- FIESTA::pcheck.varchar(var2check=prj, varnm="prj", checklst=prjlst, 
		caption="Projection?", gui=gui, stopifnull=TRUE)
  if (prj == "latlong") prj <- "longlat"

  datum <- FIESTA::pcheck.varchar(var2check=datum, varnm="datum", checklst=datumlst, 
		caption="Datum?", gui=gui, stopifnull=TRUE)

  if (prj == "utm") {
    zone <- FIESTA::pcheck.varchar(var2check=as.character(zone), varnm="zone", 
		checklst=zonelst, caption="UTM zone?", gui=gui)
    if (is.null(zone)) stop("must include zone number")

    zoneS <- FIESTA::pcheck.logical(zoneS, varn="zoneS", title="UTM South?", 
      	first="NO", gui=gui)
  }

  ###########################################
  prj4str <- paste0("+proj=", prj) 

  if (prj == "longlat") {
    prj4str = paste0(prj4str, " +datum=", datum, " +no_defs")
  } else if (prj == "utm") {
    prj4str <- paste0(prj4str, " +zone=", zone, " +datum=", datum)
    if (zoneS) prj4str <- paste(prj4str, "+south")
  } else if (prj == "aea") {
    if (aea.param == "USGS") {
      param <- " +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0" 
      prj4str <- paste0(prj4str, param, " +datum=", datum, " +units=m +no_defs")
    }
  }

  return(prj4str)
}


trunc10shp <- function(x) {

  #######################################################################
  ## DESCRIPTION: 
  ## Changes names of Spatial object to < 10 characters.
  ## Returns shp with new names and data table of old and new names.
  ##
  ## ARGUMENTS:
  ## x - Spatial object
  #######################################################################

  NAMES_gt10 <- names(x)[nchar(names(x)) > 10]
  othernms <- names(x)[nchar(names(x)) <= 10]

  if (length(NAMES_gt10) > 0) {
    newnms <- data.frame(NAMES_gt10, NEWNAME="0", stringsAsFactors = FALSE)
    for (nm in NAMES_gt10) {
      allnms <- c(newnms[["NAMES_gt10"]][newnms[["NAMES_gt10"]] != nm], 
			othernms, newnms[newnms$NEWNAME != 0, "NEWNAME"])
      newnm <- unique(getlt10char(dbname=nm))
      cnt <- 0
      while (newnm %in% allnms) {
        cnt <- cnt + 1
        substr(newnm, 10, 10) <- as.character(cnt)
        allnms <- c(newnms[["NAMES_gt10"]][!newnms[["NAMES_gt10"]] %in% newnm], 
			othernms)
      }
      newnms[newnms[["NAMES_gt10"]] == nm, "NEWNAME"] <- newnm
    }
  } else {
    newnms <- NULL
  }

  ## Change name in shp and print changes to screen (Note: NULL at end)
  if (length(newnms > 0)) {
    names(x)[names(x) %in% newnms[["NAMES_gt10"]]] <- newnms[["NEWNAME"]]
    apply(newnms, 1, function(x) cat("Changed name: ", x[1], "to", x[2], "\n"))
  }

  return(list(shp=x, newnms=newnms))
}


getEPSG <- function(prj=NULL, datum=NULL, zone=NULL) {

  #######################################################################
  ## DESCRIPTION: Return table of potential EPSG codes and corresponding prj4str.
  ## 
  ## ARGUMENTS:
  ## prj - String. Projection ("longlat", "utm", "aea" (albers), "lcc" (Lambert))
  ## datum - String. Datum ("WGS84", "NAD83", "NAD27")
  ## zone - Number/String. If prj="utm", the UTM zone.
  #######################################################################

  lut <- EPSG <- rgdal::make_EPSG()
  if (!is.null(prj)) 
    lut <- lut[grep(paste0("+proj=", prj), lut$prj4), c("code", "prj4")]
  if (!is.null(datum)) 
    lut <- lut[grep(datum, lut$prj4), c("code", "prj4")]
  if (!is.null(zone)) 
    lut <- lut[grep(paste0("+zone=", zone), lut$prj4), c("code", "prj4")]
  
  return(lut)
}



check.extents <- function(layer1, layer2, showext=FALSE, layer1nm=NULL, 
	layer2nm=NULL, layer2allin=FALSE, stopifnotin=FALSE) {

  ##########################################################################
  ## DESCRIPTION
  ## Check extents of layer1 and layer2

  ## Check extents
  ext.poly1 <- as(raster::extent(layer1), "SpatialPolygons")
  ext.poly2 <- as(raster::extent(layer2), "SpatialPolygons")

  if (is.null(layer1nm)) layer1nm <- "layer1"
  if (is.null(layer2nm)) layer2nm <- "layer2"

  if (layer2allin && !rgeos::gContains(ext.poly1, ext.poly2)) {
    msg <- paste(layer2nm, "is not completely contained within", layer1nm)
    if (stopifnotin) stop(msg)
  } else {
    msg <- ifelse(rgeos::gContainsProperly(ext.poly1, ext.poly2), 
					paste(layer2nm, "is fully within", layer1nm),
		 ifelse(rgeos::gContainsProperly(ext.poly2, ext.poly1), 
					paste(layer1nm, "is fully within", layer2nm),
			ifelse(rgeos::gIntersects(ext.poly1, ext.poly2), "overlapping extents",
			 		"non-overlapping extents")))
  }
  message(msg)

  if (showext) {
    ext.both <- raster::bind(ext.poly1, ext.poly2)
    sp::plot(ext.both)

    sp::plot(ext.poly1, add=TRUE)
    sp::plot(ext.poly2, border="red", add=TRUE)
  }
  return(msg)
}


getprjatt <- function(prj4str, prjatt, stopifnull=FALSE) {

  ## DESCRIPTION:
  ## Gets the desired attribute from the proj4string.

  if (prjatt == "units" && raster::isLonLat(prj4str)) 
    stop("must be a projected coordinate system")
  prjatt2 <- paste0("+", prjatt, "=")

  if (grepl(prjatt2, prj4str)) {
    ## prjatt ("datum", "units", "proj")

    att.split <- strsplit(prj4str, prjatt2)[[1]][2]
    att.val <- strsplit(att.split, " ")[[1]][1]
  } else {
    if ((prjatt == "datum" && !grepl('ellps', prj4str)) || 
		(prjatt == 'ellps' && !grepl('datum', prj4str))) {
      if (stopifnull) {
        stop(prjatt, " does not exist in prj4str")
      } else {
        warning(prjatt, " does not exist in prj4str")
      }
    }
    return(NULL)
  }
  return(att.val)  
}
  

areacalc.poly <- function(polyv, polyv_dsn=NULL, areaprj="aea", zone=NULL, 
	units="ACRES", areavar=NULL) {
 
  ## DESCRIPTION:
  ## Calculates area of polygons, appending the new variable (AREA_*), 
  ## to the attribute table. If polyv is longlat projection, it is projected
  ## to Albers Equal Area projection before calculating area, then 
  ## reprojected back to longlat to return.

  polyv <- FIESTA::pcheck.spatial(layer=polyv, dsn=polyv_dsn)

  unitslst <- c("ACRES", "HECTARES", "SQMETERS")
  units <- FIESTA::pcheck.varchar(var2check=units, varnm="units", 
		checklst=unitslst, caption="area units?", stopifnull=TRUE)
  if (is.null(areavar)) areavar <- paste0(units, "_GIS")

  ## Check if polyv is projected
  isll <- FALSE
  if (raster::isLonLat(polyv)) {
    if (is.null(areaprj)) stop("must project polyv")

    isll <- TRUE
    message("area can only be calculated with projected coordinate system")
      
    datum <- FIESTA::getprjatt(sp::proj4string(polyv), "datum")
    if (is.null(datum)) datum == "NAD83"
    message(paste("projecting to", areaprj, "with", datum, "datum to calculate area"))
      
    if (areaprj == "utm") {
      if (is.null(zone)) stop("must specify zone with utm projection")
      if (!zone %in% 1:22) stop("utm zone out of range for conterminous U.S.")
    }   
      
    polyv <- spReprojectShape(polyv, prj.new=areaprj, datum.new=datum, zone.new=zone) 
  }

  ## Calculate area
  polyv@data[[areavar]] <- sapply(polyv@polygons, function(x) methods::slot(x, "area"))

  ## Get polygon units
  polyv.units <- getprjatt(sp::proj4string(polyv), "units")
  if (is.null(polyv.units)) {
    message("no units defined in proj4string... assuming meters")
    units <- "m"
  }
  if (polyv.units %in% c("m", "meters")) {
    cfactor.ac <- 0.00024711
    cfactor.ha <- 0.0001
  } else if (polyv.units %in% c("ft", "us-ft")) {
    cfactor.ac <- 0.00002296
    cfactor.ha <- 0.0000092903
  } else {
    stop("no conversion factor defined")
  }

  ## Convert square meters to area unit
  if (units == "ACRES") {
    polyv@data$ACRES_GIS <- round(polyv@data[[areavar]] * cfactor.ac, 6)
    areavar <- "ACRES_GIS"
  } else if (units == "HECTARES") {
    polyv@data$HECTARES_GIS <- round(polyv@data[[areavar]] * cfactor.ha, 6)
    areavar <- "HECTARES_GIS"
  } else {
    areavar <- "SQMETERS_GIS"
  }

  if (isll) 
    polyv <- spReprojectShape(polyv, prj.new="longlat", datum.new=datum)

  return(polyv)     
}


areacalc.pixel <- function(rast, units="ACRES") {

  ## DESCRIPTION:
  ## Calculates and returns area of a raster pixel. If rast is longlat 
  ## projection, then stop to reproject.

  unitlst <- c("ACRES", "HECTARES", "SQMETERS")
  if (!units %in% unitlst) stop("must be 'ACRES', 'HECTARES', or 'SQMETERS'")

  ## Check if raster is projected
  if (raster::isLonLat(rast)) {
    prj4str = build.prj4str(prj="aea", datum="NAD83")
    #stop("area can only be calculated with projected coordinate system")
    message("the coordinate system is Long/Lat, projecting raster to albers for area calculation...")
    rast <- raster::projectRaster(rast, crs=sp::CRS(prj4str))
  }
  isll <- ifelse(raster::isLonLat(rast), TRUE, FALSE)
       
  ## Get pixel size 
  rast.res <- raster::res(rast)

  ## Get raster units
  rast.units <- FIESTA::getprjatt(sp::proj4string(rast), "units")
  if (is.null(rast.units)) {
    message("no units defined in proj4string... assuming meters")
    rast.units <- "m"
  }
  if (rast.units %in% c("m", "meters")) {
    cfactor.ac <- 0.00024711
    cfactor.ha <- 0.0001
  } else if (rast.units %in% c("ft", "us-ft")) {
    cfactor.ac <- 0.00002296
    cfactor.ha <- 0.0000092903
  } else {
    stop("no conversion factor defined")
  }

  ## Get datum
  datum <- FIESTA::getprjatt(sp::proj4string(rast), "datum")
  if (is.null(datum)) {
    ellps <- FIESTA::getprjatt(sp::proj4string(rast), "ellps")
    if (is.null(ellps)) {
      message("no datum defined in proj4string... assuming NAD83")
      datum <- "NAD83"
    }
  }

  if (units == "ACRES") {
    area.pixel <- rast.res[1] * rast.res[2] * cfactor.ac
  } else if (units == "HECTARES") {
    area.pixel <- rast.res[1] * rast.res[2] * cfactor.ha
  }

  return(area.pixel)     
}


aspect_transform <- function(df, asp) {
## DESCRIPTION: Transform aspect, in degrees to northing and easting units
  	#northness - set flat to east for neutral value:
	df$asp_n <- df[[asp]]
	df$asp_n[df$asp_n==-1] <- 90
	df$cosAsp <- cos(df$asp_n*pi/180)
	df$asp_n <- NULL
	#eastness - set flat to north for neutral value:
	df$asp_e <- df[[asp]]
	df$asp_e[df$asp_e==-1] <- 0
	df$sinAsp <- sin(df$asp_e*pi/180)
	df$asp_e <- NULL
     return(df)
} 


spPlotRastcl <- function(rastcl, bks=NULL, col=NULL, col.palette=NULL, ext=NULL, 
	labels=NULL, ...){
  ## DESCRIPTION: Plots a classified raster with legend for breaks. 

  if (class(rastcl) != "RasterLayer") stop("rastcl must be a Rasterlayer")
  
  ## Define class breaks of rastcl
  if (is.null(bks))
    bks <- sort(unique(na.omit(rastcl)))
  
  if (min(bks) == 0) {
    bks <- c(-1, bks)
  } else {    
    bks <- unique(c(0, sort(bks)))
  }
  nbrbks <- length(bks)

  ## Define labels for class breaks
  labpts <- bks[-1] - diff(bks)/2

  ## Check col.palette
  if (!is.null(col.palette) && !is.function(col.palette)) 
    stop("col.palette must be a function")

  ## Define colors for plotting rastcl
  if (is.null(col)) {
    if (!is.null(col.palette)) {
      if (!is.function(col.palette)) {
        stop("col.palette must be a function")
      } else {
        col <- col.palette(nbrbks)
      }
    } else {
      col <- terrain.colors(nbrbks)
    }
  } else {
    if (length(col) != nbrbks-1)
      stop(paste("you must have", nbrbks, "colors defined"))
  }
  if (is.null(labels))
    labels <- as.character(bks[-1])
  
  sp::plot(rastcl, ext=ext, col=col,
	axis.args=list(at=labpts, labels=labels), breaks=bks, ...)
}


writeESRIprj <- function(x) {
  ## Adds *.prj file to folder with *.bil file. 
  ## Note: when using raster getData(), the files are written the working 
  ## 		working directory as *.bil files. When read back into R, GDAL 
  ## 		thinks it is in ESRI format (not sure why), but missing a *.prj file. 
  ##		So, if you want to read from file, you must write a *.prj file 
  ##		to the same directory.
  
  p4s <- proj4string(x)
  xfn <- x@file@name
  
  rgdal::showWKT(p4s, morphToESRI = TRUE, 
      file=paste0(dirname(xfn), "/", basename.NoExt(xfn), ".prj"))
}  


CRScompare <- function(layer1prj, layer2, prj4str=NULL, nolonglat=TRUE){
  ##################################################################################
  ## DESCRIPTION: Compare Coordinate Reference System. If not the same, 
  ##		project layer2 to Coordinate System of layer1 and return both layers.
  ## ARGUMENTS:
  ##   layer1prj - the name of layer1 or layer1's proj4string
  ##   layer2 - the name of layer2 or layer2's proj4string  
  ##   prj4str - A PROJ.4 projection string. Include if nolonglat=TRUE and a 
  ##			projection other than default albers projection is desired.
  ##   nolonglat - If TRUE and both layers are in Geographic Coordinate System,
  ## 			project both layers to default projection (albers) or other 
  ##			projection if prjstr is NOT NULL.
  ## VALUE:
  ## 	  list	layer1 	layer1prj
  ##			layer2	layer2prj
  ###################################################################################
  reprj <- FALSE
 
  if (is.null(prj4str)) {
    ## Define default Coordinate System as USGS albers
    prj4.default <- paste("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0",
			"+ellps=GRS80 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs")
  } else {
    rgdal::checkCRSArgs(prj4str)
    prj4.default <- prj4str
    reprj <- TRUE
  }
  prj <- FIESTA::getprjatt(prj4.default, "proj")

  ################################################################################
  ## Check layers
  ################################################################################

  ## Get projection - layer1prj
  if (isS4(layer1prj)) {
    layer1prj.prj <- sp::proj4string(layer1prj)
  } else if (is.character(layer1prj)) {
    if (!rgdal::checkCRSArgs(layer1prj)) stop("invalid layer1prj")
    layer1prj.prj <- layer1prj
  } else {
    stop("invalid layer1prj")
  }
  ## If layer1prj has no defined projection and prjstr 
  if (is.na(layer1prj.prj)) stop("layer1prj does not have a defined projection")

  ## Get projection - layer2
  if (isS4(layer2)) {
    layer2.prj <- sp::proj4string(layer2)
  } else if (is.character(layer2)) {
    if (!rgdal::checkCRSArgs(layer2)) stop("invalid layer2")
    layer2.prj <- layer2
  } else {
    stop("invalid layer2")
  }
  if (is.na(layer2.prj)) stop("layer2 does not have defined projection")


  ################################################################################
  ## Check if layers are Geographic if nolonglat=TRUE
  ## If both layers are in Geographic Coordinate System and nolonglat=TRUE, 
  ## or if reprj=TRUE, project both layers to default projection (prj4.default)
  ################################################################################
  if (reprj || (nolonglat && raster::isLonLat(layer1prj) && raster::isLonLat(layer2))) {
    message("projecting both layers to ", prj)
    layer1prj.prj <- prj4.default
    layer1prj <- sp::spTransform(layer1prj, CRSobj = CRS(layer1prj.prj))
  }

  ## Checks if projection is same
  if (!raster::compareCRS(layer1prj.prj, layer2.prj)) {	
    if (is.raster(layer2)) {
      stop("cannot reproject a raster")
    } else if (is.raster(layer1prj)) {
      ## Check number of features
      if (is.raster(layer2) && is.raster(layer1prj)) {
        if (length(layer2@polygons) > length(layer1prj@polygons))
          if (nolonglat && raster::isLonLat(layer2))
            message("layer2 has more features than layer1... consider switching projection")
      }
    }

    ## if projection is not the same, reproject layer2 to layer1prj
    layer2prj <- sp::spTransform(layer2, CRSobj = CRS(layer1prj.prj))

    message(paste("reprojecting layer... \n", "from:", layer2.prj, 
		"\n to:", layer1prj.prj))

  } else {
#    ## if projection is the same, but not identical text, redefine projection 
#    ## of layer2 to match layer1prj
#    message(paste("layer1prj:", layer1prj.prj))
#    message(paste("layer2:", layer2.prj))
#
#    message(paste("projections are the same but not identical...",
#		"setting projection of layer2 to match layer1prj: \n", layer1prj.prj))
#    suppressWarnings(sp::proj4string(layer2) <- layer1prj.prj)
    layer2prj <- layer2
  }

  returnlst <- list(layer1=layer1prj, layer2=layer2prj)
  return(returnlst)
}  

