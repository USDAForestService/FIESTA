## getext - get extent of filename
## getrastlst.rgdal - verifies a list of raster or raster files.
## polyfix.sf
## pcheck.spatial - checks or gets Vector layer from file name or spatial object
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
## checksf.longlat
## checkrast.longlat
## crsCompare - compares projections, if different projects 
## sf_dissolve - dissolve vector polygons
## closest_poly - get polygon of y with closest polygon to x (slower than centroid)
## getIntersect - get intersection of layer1 with layer2
## clip.othertables 
## spGetSates - get intersecting states



getext <- function(x) { 
  xbasename <- basename(x)
  strsplit(xbasename, paste0(basename.NoExt(xbasename), "."))[[1]][2] 
}
  

getrastlst.rgdal <- function(rastnmlst, rastfolder=NULL, stopifLonLat=FALSE,
	stopifnull=FALSE, gui=TRUE){

  #########################################################################
  ## DESCRIPTION: 
  ## To verify rasters. Checks if rasters exist. If rastfolder is not NULL,
  ## checks for rasters in the rastfolder.
  ## 
  ## ARGUMENTS:
  ## rastnmlst   RasterLayer, RasterStack, RasterBrick, or String vector. 
  ##		List of raster names. Extensions included.
  ## rastfolder  String. The name of the folder where rasters are. Optional.
  ##
  ## VALUE: 
  ## rastfnlst  String vector. List of raster file names.
  ##########################################################################

  ## Adds to file filters to Cran R Filters table.
  if (.Platform$OS.type == "windows") {
    Filters <- rbind(Filters,img=c("Erdas Image (*.img)", "*.img"))
    Filters <- rbind(Filters,tif=c("GeoTIFF (*.tif)", "*.tif"))
    Filters <- rbind(Filters,bil=c("Binary (*.bil)", "*.bil"))
  }

  rastfnlst <- {}
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
        
      rasts <- TRUE
      while (rasts) {
        rasttype <- select.list(c("Erdas Image (*.img)", "GeoTIFF (*.tif)", "binary (*.bil)"),
		"Arc/Info GRID", title="Raster Type")
        rastext <- row.names(Filters)[Filters[,1] == rasttype]

        if (rasttype != "Arc/Info GRID" && .Platform$OS.type=="windows") {
          rastnm <- choose.files(default="", caption="Select raster image(s)", 
                filters=Filters[rastext,], multi=TRUE)
          if (length(rastnm) == 0) {
            stop("")
          } else {
            rastfnlst <- c(rastfnlst, rastnm) 
          }
        } else if (rasttype == "Arc/Info GRID" && .Platform$OS.type=="windows") {
          rastnm <- choose.dir(default=rastfolder, caption="Select raster grid")
          if (is.na(rastnm)) { 
            stop("")
          } else {
            rastfnlst <- c(rastfnlst, rastnm) 
          }
        } else {
          stop("")
        }
        rasts <- select.list(c("Yes", "No"), title="Another raster")
        rasts <- ifelse(rasts == "Yes", TRUE, FALSE)
      }
    } else {
      if (stopifnull) {
        stop("rastnmlst is NULL")
      } else {
        return(NULL)
      }
    }
  } else if (class(rastnmlst) %in% c("RasterLayer", "RasterStack", "RasterBrick")) {
    rastfn <- rastnmlst[[1]]@file@name
    if (file.exists(rastfn)) {
      rastfnlst <- c(rastfnlst, rastfn)
    } else {
      stop(rastfn, "is invalid... must be saved to file")
    }
  } else if (any(sapply(rastnmlst, isS4))) {
    for (rastnm in rastnmlst) {
      if (!class(rastnm) %in% c("RasterLayer", "RasterStack", "RasterBrick")) {
        if (file.exists(rastnm[[1]])) {
          rastfnlst <- c(rastfnlst, rastnm)
        } else {
          message(rastnm[[1]], "is invalid")
        }
      } else {
        rastfn <- rastnm[[1]]@file@name
        if (file.exists(rastfn)) {
         rastfnlst <- c(rastfnlst, rastfn)
        } else {
         stop(rastfn, "is invalid")
        }
      }
    }
  } else {  ## !is.null(rastnmlst)
    if (!is.null(rastfolder)) {
      if (file.exists(rastfolder) && rastfolder != "") {
        rastfnlst <- paste(rastfolder, rastnmlst, sep="/")
      } else {
        stop("rastfolder is not valid")
      }
    } else {
      rastfnlst <- rastnmlst
    }
    if (sum(sapply(rastfnlst, file.exists) == FALSE) > 0) {
       notexist <- rastfnlst[sapply(rastfnlst, file.exists) == FALSE]
       message("invalid rastnm in rastmlst:")
       print(paste(unlist(notexist), collapse=", "))
    }
  }

  ## Check each raster for projection information
  for (rastfn in rastfnlst) {
    message(rastfn)
    rast.info <- rasterInfo(rastfn)
    if (is.null(rast.info)) stop("invalid raster: ", rastfn)
    rast.prj <- rast.info$crs

    if (is.null(rast.prj) || rast.prj == "") {
      message(paste("raster has undefined projection:", rastfn))    

    } else if (sf::st_is_longlat(rast.prj)) {
      message(paste("rast is longlat:", rastfn))
      if (stopifLonLat) stop("")
    }
  }
  return(rastfnlst) 
}


polyfix.sf <- function(x) {
  # any bad polys?

  valid <- NULL
  message("checking polygons...")
  if (suppressWarnings(sum(sf::st_is_valid(x) == FALSE)) > 0) {
    message("poly invalid")

    if (sf::st_is_longlat(x))
      stop("polygons layer must be projected")

    # this is a well known R / GEOS hack (usually combined with the above) to 
    # deal with "bad" polygons
    message("buffering poly with 0 width...")
    x <- sf::st_buffer(x[!is.na(valid)], 0.0)

    message("checking polygons after buffer...")
    if (sum(sf::st_is_valid(x) == FALSE) > 0)
      stop("bad polys")
  }
  return(x)
}


pcheck.spatial <- function(layer=NULL, dsn=NULL, sql=NA, fmt=NULL, tabnm=NULL, 
	caption=NULL, stopifnull=FALSE, gui=FALSE, polyfix=FALSE, asSpatial=FALSE, 
	dropgeom=FALSE, stopifnoCRS=TRUE, checkonly=FALSE) {
  ## DESCRIPTION: checks or gets Vector layer from file name or spatial object.
  ## ARGUMENTS:
  ## dsn		String. The name of the database or path of shapefile (data source name)
  ##				or R Spatial Object.
  ## layer  	String. The name of layer in database or R Spatial Object.
  ## caption  	String. The text to add for gui window caption.
  ## checkonly	Logical. If TRUE, check layer only, do not return.

  ## Adds to file filters to Cran R Filters table.
  if (.Platform$OS.type == "windows") {
    Filters <- rbind(Filters,shp=c("Shapefiles (*.shp)", "*.shp"))
    Filters <- rbind(Filters,gpkg=c("GeoPackages (*.gpkg)", "*.gpkg"))
    Filters <- rbind(Filters,gdb=c("Esri file geodatabase (*.gdb)", "*.gdb"))
    Filters <- rbind(Filters,sqlite=c("SQLite/Spatialite (*.sqlite)", "*.sqlite"))
  }

  ## Check for installed packages
  if (!"sf" %in% rownames(installed.packages()))
    stop("importing spatial layers requires package sf")
  if (!is.null(fmt)) {
    if (fmt == "gdb") {
      if (!"arcgisbinding" %in% rownames(installed.packages()))
        stop("importing spatial layers from *gdb requires package arcgisbinding")
    }
  } 
  fmtlst <- c("shp", "sqlite", "gpkg", "gdb")
  stringsAsFactors <- FALSE 

  if (is.null(tabnm)) tabnm <- "layer" 
  if (is.null(caption)) caption <- ""

  ## Return NULL
  if (is.null(layer) && is.null(dsn)) {
    if (checkonly) {
      return(FALSE)
    } else {
      return(NULL)
    }
  }

  ## Check layer - if sf object
  if (!is.null(layer)) {
    if ("sf" %in% class(layer)) {
      return(layer)
    } else if (methods::canCoerce(layer, "sf")) {
      return(sf::st_as_sf(layer, stringsAsFactors=stringsAsFactors))
    } else if (is.character(layer) && file.exists(layer)) {
      dsn <- layer
    } else if (is.data.frame(layer)) {
      return(layer)
    }
  }

  if (!is.null(dsn)) {
    ext.dsn <- getext(dsn)
    if (!file.exists(dsn) && (is.na(ext.dsn) || ext.dsn == "NA")) {
      if (!is.null(fmt) && fmt %in% fmtlst) 
        dsn <- paste(dsn, fmt, sep=".")
      if (!file.exists(dsn)) dsn <- NULL
    }
    if (ext.dsn %in% c("shp", "csv")) {
      layer <- basename.NoExt(dsn)
    } else if (!gui && is.null(layer)) {
      stop("layer is NULL")
    }
  } else {
    ext.layer <- getext(layer)
    if (!is.na(ext.layer) && ext.layer != "NA" && file.exists(layer)) {
      if (ext.layer %in% c("shp", "csv")) {
        dsn <- layer
        layer <- basename.NoExt(dsn)
      } else {
        stop(ext.layer, " not supported")
      }
    }
  }    
 
  ######################################################
  ## Check dsn
  ######################################################
  if (gui && .Platform$OS.type=="windows") {
    if (is.null(dsn)) {
      fmt <- select.list(fmtlst, title="dsn format?", multiple=FALSE)
      if (fmt == "gdb") {
        dsn <- choose.dir(default=getwd(), caption="Select database")
      } else if (fmt %in% c("sqlite", "gpkg")) {
        dsn <- choose.files(default=getwd(), caption="Select database")
      } else if (fmt == "shp") {
        dsn <- choose.files(default=getwd(), caption="Select database")
      } else {
        stop("format not currently supported")
      }
    } else {
      fmt <- ext.dsn
    }
    if (fmt %in% c("shp", "csv")) {
      layer <- basename.NoExt(dsn)    
    } else {    
      layerlst <- sf::st_layers(dsn)$name
      layer <- select.list(layerlst, title="Select layer", multiple=FALSE)
    }    
  } 
  if (is.null(dsn))
    stop("dsn is NULL")
  
  ######################################################
  ## Check layer
  ######################################################
  layerlst <- sf::st_layers(dsn)

  ## Note: if dsn is a SpatiaLite database, only spatial layers are listed
  if (!layer %in% layerlst$name) {
    if (checkonly) {
      return(FALSE) 
    }
    if (ext.dsn == "sqlite") {
      return(pcheck.table(tab=layer, tab_dsn=dsn))
    } else {
      stop(layer, " is not in database")
    }
  } else {
    if (checkonly) {
      return(TRUE)
    }
  }

  geomtype <- layerlst$geomtype[layerlst$name == layer][[1]]
  if (is.na(geomtype)) {
    if (!checkonly) {
      return(pcheck.table(tab=layer, tab_dsn=dsn))
    } else {
      return(list(dsn=dsn, layer=layer))
    }
  } else {
    splayer <- sf::st_read(dsn=dsn, layer=layer, query=sql, 
				stringsAsFactors=stringsAsFactors, quiet=TRUE)
  }

  if ("sf" %in% class(splayer)) {

    if (nrow(splayer) == 0 && stopifnull) {
      msg <- "layer has 0 records"
      if (stopifnull) {
        stop(msg)
      } else {
        message(msg)
      }
    }
              
    ## Check if projection
    ############################################################
    if (is.na(sf::st_crs(splayer))) {
      msg <- paste("projection is not defined for:", dsn)
      if (stopifnoCRS) {
        stop(msg)
      } else {
        message(msg)
      }
    }

    ## If polyfix
    ############################################################
    if (polyfix)
      splayer <- polyfix.sf(splayer)

    ## Drop geometry in table
    ############################################################
    if (dropgeom)
      splayer <- sf::st_drop_geometry(splayer)
  }

  if (checkonly) {
    return(list(dsn=dsn, layer=layer))
  } else {
    return(splayer)
  }
}


build.prj4str <- function(prj, datum=NULL, ellps=NULL, zone=NULL, zoneS=FALSE, 
	aea.param="USGS", gui=FALSE) {

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
  #datumlst <- as.character(rgdal::projInfo(type="datum")$name)
  ellpslst <- as.character(rgdal::projInfo(type="ellps")$name)
  zonelst <- c(1:60)


  prj <- FIESTA::pcheck.varchar(var2check=prj, varnm="prj", checklst=prjlst, 
		caption="Projection?", gui=gui, stopifnull=TRUE)
  if (prj == "latlong") prj <- "longlat"

#  datum <- FIESTA::pcheck.varchar(var2check=datum, varnm="datum", checklst=datumlst, 
#		caption="Datum?", gui=gui)
  ellps.gui <- ifelse(is.null(datum), TRUE, FALSE)
  ellps <- FIESTA::pcheck.varchar(var2check=ellps, varnm="ellps", checklst=ellpslst, 
		caption="Ellipse?", gui=ellps.gui)
  if (is.null(ellps)) 
    stop("both datum and ellpse are NULL.. cannot reproject")

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
    if (!is.null(datum)) {
      prj4str = paste0(prj4str, " +datum=", datum, " +no_defs")
    } else {
      prj4str = paste0(prj4str, " +ellps=", ellps, " +no_defs")
    }
  } else if (prj == "utm") {
    prj4str <- paste0(prj4str, " +zone=", zone, " +datum=", datum)
    if (zoneS) prj4str <- paste(prj4str, "+south")
  } else if (prj == "aea") {
    if (aea.param == "USGS") {
      prj4str <- paste("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0",
			"+ellps=GRS80 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs")
    } else {
      param <- " +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0" 

      if (!is.null(datum)) {
        prj4str <- paste0(prj4str, param, " +datum=", datum, " +units=m +no_defs")
      } else {
        prj4str = paste0(prj4str, param, " +ellps=", ellps, " +no_defs")
      }
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



check.extents <- function(bbox1, bbox2, showext=FALSE, layer1nm=NULL, 
	layer2nm=NULL, stopifnotin=FALSE, quiet=FALSE) {

  ##########################################################################
  ## DESCRIPTION
  ## Check extents of layer1 and layer2
  ## stopifnotin	- stop if bbox2 is not all in bbox1

  if (is.null(layer1nm)) layer1nm <- "layer1"
  if (is.null(layer2nm)) layer2nm <- "layer2"

  if (sum(is.na(as.vector(bbox1))) == length(bbox1)) {
    warning(layer1nm, " bbox is invalid")
    return()
  }
  if (sum(is.na(as.vector(bbox2))) == length(bbox2)) {
    warning(layer2nm, " bbox is invalid")
    return()
  }
  
  ## Check extents
  bbox1sfc <- sf::st_as_sfc(bbox1)
  bbox2sfc <- sf::st_as_sfc(bbox2)

  bbox1sf <- sf::st_as_sf(bbox1sfc)
  bbox2sf <- sf::st_as_sf(bbox2sfc)

  bbox1sf$fld <- 1
  intpct <- suppressWarnings(tabulateIntersections(layer1=bbox1sf, 
			layer1fld="fld", layer2=bbox2sf))$int.pct

  if (showext) {
    bbox12sfc <- append(bbox1sfc, bbox2sfc)
    plot(sf::st_geometry(bbox12sfc))

    plot(sf::st_geometry(bbox2sfc), add=TRUE)
    plot(sf::st_geometry(bbox1sfc), add=TRUE, border="red")
  }

  if (is.na(intpct) || intpct == 0) {
    msg <- paste(layer1nm, "does not overlap", layer2nm)
    if (stopifnotin) {
      stop(msg)
    } else {
      return(NULL)
    }
  } else {
    if (intpct < 100 && !quiet) {
      message(layer1nm, " is not completely contained within ", layer2nm)
      message("...intersection of ", intpct, "%")
    }
    return(intpct)
  } 
}


getprjatt <- function(prj4str, prjatt, stopifnull=FALSE) {

  ## DESCRIPTION:
  ## Gets the desired attribute from the proj4string.

  if (prjatt == "units" && sf::st_is_longlat(prj4str)) 
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
	unit="ACRES", areavar=NULL) {
 
  ## DESCRIPTION:
  ## Calculates area of polygons, appending the new variable (AREA_*), 
  ## to the attribute table. If polyv is longlat projection, it is projected
  ## to Albers Equal Area projection before calculating area, then 
  ## reprojected back to longlat to return.

  ## Set global variables
  acre=hectare=km <- NULL

  polyv <- pcheck.spatial(layer=polyv, dsn=polyv_dsn)

  unitlst <- c("ACRES", "HECTARES", "SQMETERS", "SQKM")
  unit <- FIESTA::pcheck.varchar(var2check=unit, varnm="unit", 
		checklst=unitlst, caption="area units?", stopifnull=TRUE)
  if (is.null(areavar)) areavar <- paste0(unit, "_GIS")


  isll <- FALSE

  if (sf::st_is_longlat(polyv)) {
    isll <- TRUE
    message("area can only be calculated with projected coordinate system... projecting layer")

    #prj4str <- sf::st_crs(polyv)$proj4string

    ## Get longlat crs
    crs.longlat <- sf::st_crs(polyv)

    ## Check if polyv is projected
    polyv <- checksf.longlat(polyv)
  }

  ## Calculate area
  polyv[["AREA_GIS"]] <- sf::st_area(polyv)

  ## Get polygon units
#  polyv.units <- unique(units(polyv[["AREA_GIS"]])$numerator)
#  if (polyv.units %in% c("m", "meters")) {
#    cfactor.ac <- 0.00024711
#    cfactor.ha <- 0.0001
#  } else if (polyv.units %in% c("ft", "us-ft")) {
#    cfactor.ac <- 0.00002296
#    cfactor.ha <- 0.0000092903
#  } else {
#    stop("no conversion factor defined")
#  }

  ## Convert square meters to area unit
#  if (units == "ACRES") {
#    polyv[["ACRES_GIS"]] <- round(polyv[["AREA_GIS"]] * cfactor.ac, 6)
#    areavar <- "ACRES_GIS"
#  } else if (units == "HECTARES") {
#    polyv[["HECTARES_GIS"]] <- round(polyv[["AREA_GIS"]] * cfactor.ha, 6)
#    areavar <- "HECTARES_GIS"
#  } else {
#    areavar <- "SQMETERS_GIS"
#  }

  if (unit == "ACRES") {
    polyv[["ACRES_GIS"]] <- units::set_units(x=polyv[["AREA_GIS"]], value=acre)
  } else if (unit == "HECTARES") {
    polyv[["HECTARES_GIS"]] <- units::set_units(x=polyv[["AREA_GIS"]], value=hectare)
    areavar <- "HECTARES_GIS"
  } else if (unit == "SQKM") {
    polyv[["SQKM_GIS"]] <- units::set_units(x=polyv[["AREA_GIS"]], value=km^2)
    areavar <- "SQKM_GIS"
  } else {
    polyv[["SQMETERS_GIS"]] <- polyv[["AREA_GIS"]]
    areavar <- "SQMETERS_GIS"
  }

  if (isll) 
    polyv <- sf::st_transform(polyv, crs.longlat, quiet=TRUE)

  return(units::drop_units(polyv))     
}


areacalc.pixel <- function(rastfn, unit="ACRES", rast.NODATA=NULL, na.rm=TRUE) {

  ## DESCRIPTION:
  ## Calculates and returns area of a raster pixel. If rast is longlat 
  ## projection, reproject to default, Albers NAD83.

  unitlst <- c("ACRES", "HECTARES", "SQMETERS")
  if (!unit %in% unitlst) stop("must be 'ACRES', 'HECTARES', or 'SQMETERS'")


  ## Check if raster is not projected
  rastfn <- checkrast.longlat(rastfn)

  ## Check if rastfn is long/lat
  rast_info <- rasterInfo(rastfn)
  rast.prj <- rast_info$crs
  rast.res <- rast_info$cellsize

       
  ## Get raster units
  rast.units <- FIESTA::getprjatt(rast.prj, "units")
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
  cfactor <- ifelse(unit == "ACRES", cfactor.ac, ifelse(unit == "HECTARES", cfactor.ha, 1))


  ## Calculate pixel counts by raster value
  pixelarea <- pixelCount(rastfn)

  ## Calculate area based on number of pixels
  pixelarea$area <- pixelarea$count * rast.res[1] * rast.res[2] * cfactor


  ## Remove values that equal NA
  if (na.rm)
    pixelarea <- pixelarea[!is.na(pixelarea$value), ]

  ## Remove values that equal rast.NODATA
  if (!is.null(rast.NODATA))
    pixelarea <- pixelarea[pixelarea$value != rast.NODATA, ]

  return(pixelarea)     
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


spPlotRastcl <- function(rastcl, bks=NULL, col.bks=NULL, col.palette=NULL, ext=NULL, 
	labels=NULL, ...){
  ## DESCRIPTION: Plots a classified raster with legend for breaks. 
  ## ARGUMENTS:  
  ##  rastcl	- classified raster
  ##  bks		- raster breaks

  if (!"raster" %in% rownames(installed.packages()))
    stop("displaying raster class objects requires package raster")

  if (class(rastcl) != "RasterLayer") stop("rastcl must be a Rasterlayer")
  
  ## Define class breaks of rastcl
  if (is.null(bks))
    bks <- sort(raster::unique(rastcl, na.last=NA))
  
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
  if (is.null(col.bks)) {
    if (!is.null(col.palette)) {
      if (!is.function(col.palette)) {
        stop("col.palette must be a function")
      } else {
        col.bks <- col.palette(nbrbks)
      }
    } else {
      col.bks <- terrain.colors(nbrbks)
    }
  } else {
    if (length(col.bks) != nbrbks-1)
      stop(paste("you must have", nbrbks, "colors defined"))
  }
  if (is.null(labels))
    labels <- as.character(bks[-1])
  
  plot(rastcl, ext=ext, col=col.bks,
	axis.args=list(at=labpts, labels=labels), breaks=bks, ...)
}


#writeESRIprj <- function(x) {
  ## Adds *.prj file to folder with *.bil file. 
  ## Note: when using raster getData(), the files are written the working 
  ## 		working directory as *.bil files. When read back into R, GDAL 
  ## 		thinks it is in ESRI format (not sure why), but missing a *.prj file. 
  ##		So, if you want to read from file, you must write a *.prj file 
  ##		to the same directory.
  
#  p4s <- sp::proj4string(x)
#  xfn <- x@file@name
  
#  rgdal::showWKT(p4s, morphToESRI = TRUE, 
#      file=paste0(dirname(xfn), "/", basename.NoExt(xfn), ".prj"))
#}  


checksf.longlat <- function(x, nolonglat=TRUE, crs.default=NULL) {
  ##################################################################################
  ## DESCRIPTION: Check for longlat Geodetic coordinate system.
  ## 
  ## Default projection: NAD83 - Conus Albers
  ## +proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96,
  ##		+x_0=0 +y_0=0", "+ellps=GRS80 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs")
  ##################################################################################
  crs.albersUS <- 5070

  if (canCoerce(x, "sf"))
    x <- sf::st_as_sf(x, stringsAsFactors=FALSE)
  
  ## Define default coordinate System
  if (is.null(crs.default))
    crs.default <- crs.albersUS
  crs.default <- sf::st_crs(crs.default)
  prj4str.default <- crs.default$proj4string

  ## Check if projection defined
  if (is.na(sf::st_crs(x))) stop("no projection defined")


  ## Reproject coordinate system
  if (sf::st_is_longlat(x) && nolonglat)
    x <- sf::st_transform(x, crs.default, quiet=TRUE)

  return(x)
}


checkrast.longlat <- function(rastfn, dstfile=NULL, nolonglat=TRUE, crs.default=NULL) {
  ##################################################################################
  ## DESCRIPTION: Check for longlat Geodetic coordinate system.
  ## 
  ## Default projection: NAD83 - Conus Albers (EPSG:5070)
  ## +proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96,
  ##		+x_0=0 +y_0=0", "+ellps=GRS80 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs")
  ##################################################################################
  crs.albersUS <- "EPSG:5070"
  
  ## Define default coordinate System
  if (is.null(crs.default))
    crs.default <- crs.albersUS
  crs.default <- sf::st_crs(crs.default)

  rastfn <- getrastlst.rgdal(rastfn)
  rast.prj <- rasterInfo(rastfn)$crs
  rast.name <- basename.NoExt(rastfn)
  #rast.dirname <- dirname(nlcdfn)


  ## Reproject coordinate system
  if (sf::st_is_longlat(rast.prj) && nolonglat) {
    if (is.null(dstfile)) {
      dstfile <- paste0(getwd(), "/rastprj.img")
      message("saving projected raster to: ", dstfile)
    }
    rastprjfn <- reprojectRaster(rastfn, dstfile=dstfile, t_srs=crs.default, of="HFA")
    return(rastprjfn)
  } else {
    return(rastfn)
  }
}


  
crsCompare <- function(x, ycrs=NULL, x.crs=NULL, nolonglat=FALSE,
	checkonly=FALSE, crs.default=NULL){
  ##################################################################################
  ## DESCRIPTION: Compare Coordinate Reference System (CRS) of x to y CRS
  ##		string. If not the same, project x to y CRS.
  ## ARGUMENTS:
  ##   x - sf object or crs. CRS to check.
  ##   ycrs - sf object or crs. CRS to compare to x.
  ##   x.crs - CRS. If x has no defined projection info, use x.CRS to define it
  ##   nolonglat - Logical. If TRUE, stop if both layers are in Geographic 
  ##			Coordinate System.
  ## VALUE:
  ## 	 xprj - x object, in original format 
  ##	 crs	- crs object, in original format 
  ###################################################################################
  ## Default projection: NAD83 - Conus Albers
  ## +proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96,
  ##		+x_0=0 +y_0=0", "+ellps=GRS80 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs")
  ## EPSG:5070 NAD83/Conus Albers
  crs.albersUS <- paste("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5",
			"+x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +type=crs")

  if (canCoerce(x, "sf"))
    x <- sf::st_as_sf(x, stringsAsFactors=FALSE)
  if (canCoerce(ycrs, "sf"))
    ycrs <- sf::st_as_sf(ycrs, stringsAsFactors=FALSE)
 
  ## Define default Coordinate System as USGS albers
  if (is.null(crs.default) || is.na(crs.default) || crs.default=="")
    crs.default <- crs.albersUS

  ## Check x
  #############################
  crs.x <- sf::st_crs(x)

  if (is.na(crs.x)) {
    if (is.null(x.crs)) {
      stop("x has no projection defined... must include projection in x.crs")
    } else {
      crs.x <- x.crs
      if ("sf" %in% class(x))
        sf::st_crs(x) <- crs.x
    }
  }

  ## Check ycrs
  #############################
  if (is.null(ycrs)) {
    warning("ycrs is NULL...  using default projection:")
    message("EPSG:5070 - NAD83/Conus Albers")
    ycrs <- crs.default
  }
  crs.y <- sf::st_crs(ycrs)
  if (is.na(crs.y)) 
    stop("ycrs must have defined projection")
  if ("sf" %in% class(ycrs)) 
    ycrs <- sf::st_transform(ycrs, crs.y, quiet=TRUE)
    
  ## Check if longlat
  #############################
  if (nolonglat && sf::st_is_longlat(crs.y)) {
    if (!sf::st_is_longlat(crs.x)) {
      crs.y <- crs.x
    } else {
      message("crs.default must be in a projected CRS... using US albers")
      message(crs.albersUS)
      crs.y <- crs.albersUS
    }
    if ("sf" %in% class(ycrs)) 
      ycrs <- sf::st_transform(ycrs, crs.y, quiet=TRUE)
  } 

  prj4str.x <- sf::st_crs(x)$input
  prj4str.y <- sf::st_crs(ycrs)$input
 

  ## if projection is not the same, reproject layer1 to prj4crs
  if (!checkonly) {
    if (crs.x != crs.y) {
      xprj <- sf::st_transform(x, crs=crs.y, quiet=TRUE)
      message(paste("reprojecting layer... \n", "from:", prj4str.x, 
		"\n to:", prj4str.y))
      returnlst <- list(x=xprj)
    } else {
      returnlst <- list(x=x)
    }
    returnlst$ycrs <- ycrs

    return(returnlst)
  } else {
    return(NULL)
  }
}

sf_dissolve <- function(sflayer, col=NULL, areacalc=TRUE) {
  if (is.null(col)) {
    col <- "tmp"
    sflayer[[col]] <- 1
  }
  
  ## Dissolve polygons
  geocol <- attr(sflayer, "sf_column")
  sfd <- aggregate(sflayer[, geocol], by=sf::st_drop_geometry(sflayer[, col, drop=FALSE]), sum)
  names(sfd) <- c(col, "geometry")

  if (areacalc)
    sfd <- areacalc.poly(sfd)
  return(sf::st_cast(sfd))
}


closest_poly <- function(x.centroid, ypoly, ypoly.att=NULL, nbr=NULL, returnsf=TRUE) {
  ## DESCRIPTION: Get polygon(s) in y closest to x (centroid or polygon)

  a<- sf::st_distance(x.centroid, ypoly, by_element=TRUE) 


  ypoly$dist <- units::drop_units(sf::st_distance(x.centroid, ypoly, by_element=TRUE)) 
  ypoly <- ypoly[order(ypoly$dist, decreasing=FALSE),]
  if (is.null(nbr)) nbr <- nrow(ypoly)
  ypoly.near <- ypoly[1:nbr,]

  if (returnsf) {
    return(ypoly.near)
  } else {
    dist <- ypoly.near$dist
    if (is.null(ypoly.att)) {
      names(dist) <- row.names(ypoly.near)
    } else {
      names(dist) <- ypoly.near[[ypoly.att]]
    }
    return(dist)
  }
}


getIntersect <- function(layer1, layer2, layer1.unique, layer2fld, overlapThreshold=0) {

  ## get intersection of layer1 with layer2
  layer1.int <- sf::st_join(layer1, layer2, left=FALSE)
  layer1.intlst <- unique(layer1.int[[layer1.unique]]) 
  layer1.int <- layer1[layer1[[layer1.unique]] %in% layer1.intlst,]
  layer1.intd <- sf_dissolve(layer1.int, layer1.unique, areacalc=FALSE)

  layer1.pct <- suppressWarnings(tabulateIntersections(layer1=layer2,
 		layer1fld=layer2fld, layer2=layer1.intd, layer2fld=layer1.unique))
  layer1.intd <- layer1.intd[layer1.intd[[layer1.unique]] %in% 
		layer1.pct[layer1.pct$int.pct != 0, layer1.unique],]
  return (layer1.intd)
}


clip.othertables <- function(inids, othertabnms, othertabs=NULL, uniqueid="PLT_CN", 
	savedata=FALSE, outfn.pre=NULL, outfolder=NULL, out_fmt="csv",
	out_dsn=NULL, outfn.date=FALSE, overwrite=FALSE, gui=FALSE) {

  ## Adds to file filters to Cran R Filters table.
  if (.Platform$OS.type=="windows")
    Filters=rbind(Filters,csv=c("Comma-delimited files (*.csv)", "*.csv"))

 
  #if (is.null(outfn.pre)) outfn.pre <- "clip"

  ## Check savedata
  #############################################################################
  savedata <- FIESTA::pcheck.logical(savedata, varnm="savedata", 
		title="Save data?", first="NO", gui=gui) 
  if (savedata && out_fmt %in% c("sqlite", "gpkg")) {
    gpkg <- ifelse(out_fmt == "gpkg", TRUE, FALSE)
    out_dsn <- DBtestSQLite(out_dsn)
  }
   
  # GET A LIST AND CHECK THE OTHER TABLES
  if (is.null(othertabnms)) {
    if (gui) {
      othertabnms.resp <- select.list(c("NO", "YES"), title = "Other tables to subset?", 
		    multiple = FALSE)
      if (othertabnms.resp == "") stop("")
      while (othertabnms.resp == "YES") {
        tabresp <- select.list(c("RObject", "CSV"), title = "RObject or CSV?", 
          multiple = FALSE)
        if (tabresp == "RObject") { 
          otabnmlst <- c(ls(pos=1, all.names = TRUE), 
			ls(envir = as.environment("package:FIESTA"), pattern = "WY"))
          otabnm <- select.list(otabnmlst, title = "Other table", multiple = TRUE)
          if (length(otabnm) == 0) stop("")
          for (tabnm in otabnm)
            otablst[[tabnm]] <- get(tabnm) 
        } else if (tabresp == "CSV" && .Platform$OS.type == "windows") {
          otabnm <- choose.files(default = getwd(), caption = "Other table", 
                filters = Filters["csv",], multi = TRUE)
          if (length(otabnm) == 0) stop("")
          for (tabnm in otabnm) {
            nm <- unlist(strsplit(basename(tabnm), "\\.shp"))[1]
            otablst[[nm]] <- fread(tabnm)
          }
        }
        othertabnms.resp <- select.list(c("NO", "YES"), title = "Other tables to clip?", 
		      multiple = FALSE)
        if (othertabnms.resp == "") stop("")
      }
    }
  } 
  if (length(othertabnms) > 0 && length(othertabs) == 0) {
    othertabs <- list()
    for (othertabnm in othertabnms) {
      if (exists(othertabnm)) {
        othertabs[[othertabnm]] <- get(othertabnm)
      } else {
        stop(othertabnm, " is invalid")  
      }
    }
  }
 
  # Clips tables in othertabs to inids
  if (class(othertabs) != "list") stop("othertabs must be a list")
  if (length(othertabs) > 0) {
    intablst <- list()
    namesintablst <- {}

    for (i in 1:length(othertabs)) {
      otab <- othertabs[[i]]
      otabnm <- othertabnms[i]
      message(paste("Clipping", otabnm, ".."))

      # Set new name of return table
      returnnm <- paste("clip", otabnm, sep="_")
      outnm <- otabnm
      if (!is.null(outfn.pre)) outnm <- paste(outfn.pre, otabnm, sep="_") 
 
      if (substr(returnnm, nchar(returnnm) - 3, nchar(returnnm)) == ".csv")
        returnnm <- strsplit(returnnm, ".csv")[[1]]
      
      namesintablst <- c(namesintablst, returnnm)
      if (!is.null(otab)) {
        if (!"data.frame" %in% class(otab))
          stop(otabnm, " must be a data.frame")
        # Check uniqueid of other table.. change if PLT_CN/CN conflict
        otabvars <- names(otab)
        if (!uniqueid %in% otabvars) {
          if (uniqueid == "PLT_CN" && "CN" %in% otabvars) {
            otabid <- "CN"
          } else if (uniqueid == "CN" && "PLT_CN" %in% otabvars) {
            otabid <- "PLT_CN"
          } else {
            stop("uniqueid not in", otabnm)
          }
        } else {
          otabid <- uniqueid
        }
        if (otabnm == "cond" && "CONDID" %in% names(otab)) {
          idx.unique <- c(otabid, "CONDID")
        } else if (otabnm == "tree" && all(c("CONDID", "SUBP", "TREE") %in% names(otab))) {
          idx.unique <- c(otabid, "CONDID", "SUBP", "TREE")
        } else {
          idx.unique <- otabid
        }
        ## Subset data frame
        assign(returnnm, otab[otab[[otabid]] %in% inids, ])
        if (savedata) {
          datExportData(get(returnnm), out_fmt=out_fmt, outfolder=outfolder, 
			out_dsn=out_dsn, out_layer=outnm, overwrite_layer=overwrite,
			index.unique=idx.unique)
        }
        intablst[[returnnm]] <- get(returnnm)

      } else {
          intablst[returnnm] <- otab
      }
    }
  } else {
    intablst <- NULL
  }
  return(intablst)
}


spGetStates <- function(bnd_layer, bnd_dsn=NULL, bnd.filter=NULL, 
	stbnd=NULL, stbnd_dsn=NULL, stbnd.att=NULL, stname.att="STATENM",
	RS=NULL, states=NULL, overlap=2, showsteps=FALSE, savebnd=FALSE, 
	outfolder=NULL, ...) {

  ##############################################################################
  ## DESCRIPTION
  ## Get states that intersect a boundary. If RS != NULL, boundary is 
  ## clipped to RS states.
  ##############################################################################

 
  ##################################################################
  ## CHECK INPUT PARAMETERS
  ##################################################################
  gui <- FALSE

  #############################################################################
  ## Import boundary
  #############################################################################
  bndx <- pcheck.spatial(layer=bnd_layer, dsn=bnd_dsn, caption="boundary",
		stopifnull=TRUE)
 
  ## bnd.filter
  bndx <- datFilter(bndx, xfilter=bnd.filter, stopifnull=TRUE)$xf

 
  ########################################################################
  ### DO THE WORK
  ########################################################################
  RSlst <- unique(FIESTA::ref_statecd$RS)
  RS <- FIESTA::pcheck.varchar(var2check=RS, varnm="RS", 
		checklst=RSlst, gui=gui, caption="Research Station extent?") 

  ## Get intersecting states
  #############################################################################
  if (!is.null(stbnd) || !is.null(stbnd_dsn)) {

    stbnd <- pcheck.spatial(layer=stbnd, dsn=stbnd_dsn)
    stbnd.att <- FIESTA::pcheck.varchar(var2check=stbnd.att, varnm="stbnd.att", 
		gui=gui, checklst=names(stbnd), caption="State name attribute", 
		warn=paste(stbnd.att, "not in stbnd"))
    if (is.null(stbnd.att)) {
      if ("NAME" %in% names(stbnd)) {
        stbnd.att <- "NAME"
      } else if ("STATENM" %in% names(stbnd)) {
        stbnd.att <- "STATENM"
      } else {
        stop("include attribute in stbnd with state names - stbnd.att")
      }
    }
  } else if (exists("stunitco")) {
    stbnd <- FIESTA::stunitco			## longlat-NAD83 projection

    stbnd.att<- FIESTA::pcheck.varchar(var2check=stbnd.att, varnm="stbnd.att", 
		gui=gui, checklst=names(stbnd), caption="State name attribute", 
		warn=paste(stbnd.att, "not in stunitco"))
    if (is.null(stbnd.att)) stbnd.att <- "STATENM"
  } else if (is.null(states)) {
    stop("must include state names (states) or state boundary (stbnd) for bnd intersect")
  }

  if (!is.null(stbnd)) {
    ## Reproject stbnd to bnd projection
    prjdat <- crsCompare(stbnd, ycrs=bndx, nolonglat=TRUE)
    stbnd <- prjdat$x
    bndx <- prjdat$ycrs

    ## Check intersecting states
    stbnd <- suppressWarnings(sf::st_crop(stbnd, sf::st_bbox(bndx)))
    stated <- sf_dissolve(stbnd, stbnd.att)

    stateint <- suppressWarnings(selectByIntersects(stated, sf::st_make_valid(bndx), 
				overlapThreshold=overlap))
    states <- stateint[[stbnd.att]]

    ## Check name of attribute identifying state
    stname.att <- FIESTA::pcheck.varchar(var2check=stname.att, varnm="stname.att", 
		gui=gui, checklst=names(stbnd), caption="State name attribute", 
		warn=paste(stname.att, "not in stbnd"), stopifinvalid=FALSE)

    if (showsteps) {
      mar <-  par("mar")
      par(mar=c(1,1,1,1))

      plot(sf::st_geometry(stateint))
      plot(sf::st_geometry(bndx), add=TRUE, border="blue")
      par(mar=mar)
    }
  } else {
    stop("stbnd invalid... must include states")
  }
  
  statenames <- states
  if (!all(states %in% FIESTA::ref_statecd$MEANING)) {
    if (stbnd.att == "COUNTYFIPS")
      statenames <- FIESTA::ref_statecd[FIESTA::ref_statecd$VALUE %in% 
			unique(as.numeric(substr(states, 1,2))), "MEANING"]    
  }
  
  ## Check statenames
  if (is.null(RS)) {
    statenameslst <- FIESTA::ref_statecd$MEANING
    statenames <- pcheck.varchar(var2check=statenames, varnm="states", gui=gui, 
		checklst=statenameslst, caption="States", stopifnull=TRUE, multiple=TRUE)

  } else { 
    statenameslst <- FIESTA::ref_statecd[FIESTA::ref_statecd$RS == RS, "MEANING"]
    if (!all(statenames %in% statenameslst)) {
      statesout <- statenames[which(!statenames %in% statenameslst)] 
      statenames <- statenames[which(statenames %in% statenameslst)]  
 
      message(paste0("states are outside ", RS, " region: ", toString(statesout)))
      message("clipping boundary to ", RS, " states: ", toString(statenames))

      if (!is.null(stname.att)) {
        bndx <- spClipPoly(bndx, clippolyv=stbnd[stbnd[[stname.att]] %in% statenames, ])
        if (nrow(bndx) == 0) stop("invalid stname.att")
      }
      if (stbnd.att == "COUNTYFIPS") {
        stcds <- FIESTA::ref_statecd[FIESTA::ref_statecd$MEANING %in% statenames, "VALUE"]
        states <- states[as.numeric(substr(states, 1, 2)) %in% stcds]
      }
    }
  }

  ## Save boundary
  if (savebnd)
    spExportSpatial(bndx, outfolder=outfolder, out_layer="bnd", ...)

  return(list(states=states, bndx=bndx, stbnd.att=stbnd.att, statenames=statenames))

}


