spZonalRast <- function(polyv, polyv_dsn=NULL, polyv.att=NULL, rast, 
	rastfolder=NULL, bands=NULL, zonalstat, pixelfun=NULL, outname=NULL, 
	showext=FALSE, rastlut=NULL, rast.NODATA=NULL, na.rm=TRUE, savedata=FALSE, 
	outfolder=NULL, outfn="zonalext", outfn.pre=NULL, outfn.date=FALSE, 
	overwrite=FALSE) { 
  ##################################################################################### 
  ## DESCRIPTION:  
  ## Extracts summary statistics by polygon (i.e., zone).  
  ##################################################################################### 

  if (!"sf" %in% rownames(installed.packages()))
    stop("spZonalRast function requires package sf")

  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE 
  gui <- ifelse(nargs() == 0, TRUE, FALSE)  

  if (gui) showext=savedata=overwrite <- NULL 
 
  ## Set global variables
  count <- NULL

  ################################################################## 
  ## CHECK INPUT PARAMETERS 
  ################################################################## 
  ## Check dsn, layer 
  spobj <- pcheck.spatial(layer=polyv, dsn=polyv_dsn, gui=gui, 
		caption="Polygon zones?") 
 
  ## Check polyv.att 
  polyv.att <- FIESTA::pcheck.varchar(var2check=polyv.att, varnm="polyv.att", 
		gui=gui, checklst=names(spobj), caption="Zonal attribute",  
		warn=paste(polyv.att, "not in polyv"), stopifnull=TRUE) 
    
  ## Get raster info 
  ########################################################  

  ## Verify rasters 
  rastfn <- suppressWarnings(getrastlst.rgdal(rast, rastfolder, stopifLonLat=TRUE, gui=gui))

  ## Get names of raster 
  rastnm <- basename.NoExt(rastfn) 
 
  ## Import rasters 
  #rastx <- raster(rastfn)  

  ## Get number of bands in each raster and set names 
  rast.nbands <- rasterInfo(rastfn)$nbands

  ## Get number of bands in each raster and set names 
  rast.prj <- rasterInfo(rastfn)$crs

  ## Check zonalstat     
  zonalstatlst <- c("mean", "sum", "majority", "minority", "variety", 
	"npixels", "count", "proportion") 
  zonalstat <- FIESTA::pcheck.varchar(var2check=zonalstat, varnm="zonalstat", 
	gui=gui, checklst=zonalstatlst, caption="Zonal statistic(s)", 
	stopifnull=TRUE, multiple=TRUE) 
    
  ## Check bands 
  if (!is.null(bands)) { 
    if (rast.nbands > 1) { 
      if (!is.numeric(bands)) stop("bands must be integer") 
      if (bands > rast.nbands) stop("invalid bands, outside of range") 
      message("returning same zonal statistics for all bands")  
    } 
  } else { 
    if (rast.nbands > 1) { 
      bands <- seq(1, rast.nbands) 
    } else { 
      bands <- 1 
    } 
  } 

  ## Check rast.NODATA
  if (!is.null(rast.NODATA)) {
    if (!is.numeric(rast.NODATA))
      stop("raster.NODATA must be numeric")
  }

  ## Check outnames 
  if (!is.null(outname)) { 
    if (!is.character(outname)) stop("out must be a character vector") 
    if (length(outname) != rast.nbands) 
      stop("number of outname must match ", rast.nbands, " bands") 
  } else { 
    outname <- rastnm 
  } 

  ## Check showext     
  showext <- FIESTA::pcheck.logical(showext, varnm="showext",  
		title="Plot extents?", first="YES", gui=gui) 
  
  ## Check savedata  
  savedata <- FIESTA::pcheck.logical(savedata, varnm="savedata",  
		title="Save data extraction?", first="NO", gui=gui)    


  ## Check overwrite, outfn.date, outfolder, outfn 
  ########################################################
  if (savedata) {
    overwrite <- FIESTA::pcheck.logical(overwrite, varnm="overwrite", 
		title="Overwrite files?", first="NO", gui=gui)  
    outfn.date <- FIESTA::pcheck.logical(outfn.date , varnm="outfn.date", 
		title="Add date to outfiles?", first="YES", gui=gui)  
    outfolder <- FIESTA::pcheck.outfolder(outfolder, gui)
  }
 
  ######################################################################## 
  ### DO THE WORK 
  ######################################################################## 
  #out <- setDT(unique(polyvx@data[,polyv.att, drop=FALSE])) 
  #setkeyv(out, polyv.att) 
  #if (nrow(out) > nrow(unique(out))) warning("there are polygons with duplicate ids") 

  ## Check projection and reproject spobj if different than rast
  spobjprj <- crsCompare(spobj, rast.prj)$x

  ## Check extents
  rast.bbox <- rasterInfo(rastfn)$bbox
  names(rast.bbox) <- c("xmin", "ymin", "xmax", "ymax")
  bbox1 <- sf::st_bbox(rast.bbox, crs=rast.prj)
  bbox2 <- sf::st_bbox(spobjprj)
  check.extents(bbox1, bbox2, showext, layer1nm=rastnm, layer2nm="polv",
			stopifnotin=TRUE)
  dtype <- rasterInfo(rastfn)$datatype
  NODATAval <- rasterInfo(rastfn)$nodata_value

  zonalext <- data.table(unique(spobjprj[[polyv.att]])) 
  setnames(zonalext, polyv.att) 
  setkeyv(zonalext, polyv.att) 
  
  outnames <- {}   
  for (b in bands) { 
     
    prename <- outname 
    if (!is.null(prename) && rast.nbands > 1)  
      prename <- paste(prename, b, sep="_") 

    if (any(zonalstat %in% c("mean", "sum", "npixels"))) { 
      atts <- zonalstat[which(zonalstat %in% c("mean", "min", "max", "sum", "npixels"))] 
#      atts[atts == "sum"] <- "sumvalues" 
      atts[atts == "count"] <- "npixels"  
      zstats <- setDT(zonalStats(src=spobjprj, attribute=polyv.att, rasterfile=rastfn,  
 		pixelfun=pixelfun, band=b, na.rm=TRUE, ignoreValue=rast.NODATA)) 
      zstats <- zstats[, c("zoneid", atts), with=FALSE] 
      var.name <- paste(prename, atts, sep=".") 
      setnames(zstats, names(zstats)[-1], var.name) 
      setkey(zstats, "zoneid") 

      ## Check if class of key(zstats) in zstats matches class of key(zonalext) in zonalext
      tabs <- FIESTA::check.matchclass(zonalext, zstats, key(zonalext), key(zstats))
      zonalext <- tabs$tab1
      zstats <- tabs$tab2

      zonalext <- zonalext[zstats] 
      outnames <- c(outnames, var.name) 
      if (length(bands) > 1 && b == 1 && "npixels" %in% names(zonalstat)) 
        zonalstat <- zonalstat[zonalstat != "npixels"]
    }  

    if (any(zonalstat == "majority")) { 
      zstats <- setDT(zonalMajority(src=spobjprj, attribute=polyv.att, rasterfile=rastfn,
		band=b, na.rm=TRUE, ignoreValue=rast.NODATA)) 
      zstats <- zstats[, c("zoneid", "value")] 
      var.name <- paste(prename, "majority", sep=".") 
      setnames(zstats, names(zstats)[-1], var.name) 
      setkey(zstats, "zoneid") 

      ## Check if class of key(zstats) in zstats matches class of key(zonalext) in zonalext
      tabs <- FIESTA::check.matchclass(zonalext, zstats, key(zonalext), key(zstats))
      zonalext <- tabs$tab1
      zstats <- tabs$tab2

      zonalext <- zonalext[zstats] 
      outnames <- c(outnames, var.name) 
    }  

    if (any(zonalstat == "minority")) { 
      zstats <- setDT(zonalMinority(src=spobjprj, attribute=polyv.att, rasterfile=rastfn,
		band=b, na.rm=TRUE, ignoreValue=rast.NODATA)) 
      zstats <- zstats[, c("zoneid", "value")] 
      var.name <- paste(prename, "minority", sep=".") 
      setnames(zstats, names(zstats)[-1], var.name) 
      setkey(zstats, "zoneid") 

      ## Check if class of key(zstats) in zstats matches class of key(zonalext) in zonalext
      tabs <- FIESTA::check.matchclass(zonalext, zstats, key(zonalext), key(zstats))
      zonalext <- tabs$tab1
      zstats <- tabs$tab2

      zonalext <- zonalext[zstats] 
      outnames <- c(outnames, var.name) 
    }  

    if (any(zonalstat == "variety")) { 
      zstats <- setDT(zonalVariety(src=spobjprj, attribute=polyv.att, rasterfile=rastfn,
		band=b, na.rm=TRUE, ignoreValue=rast.NODATA)) 
      zstats <- zstats[, c("zoneid", "value")] 
      var.name <- paste(prename, "variety", sep=".") 
      setnames(zstats, names(zstats)[-1], var.name) 
      setkey(zstats, "zoneid") 

      ## Check if class of key(zstats) in zstats matches class of key(zonalext) in zonalext
      tabs <- FIESTA::check.matchclass(zonalext, zstats, key(zonalext), key(zstats))
      zonalext <- tabs$tab1
      zstats <- tabs$tab2

      zonalext <- zonalext[zstats] 
      outnames <- c(outnames, var.name) 
    }  
  
    if (any(zonalstat %in% c("count", "proportion"))) { 
      zstats <- setDT(zonalFreq(src=spobjprj, attribute=polyv.att, 
		rasterfile=rastfn, band=b, na.rm=na.rm, ignoreValue=rast.NODATA)) 
      newvar <- "value" 

      if (!is.null(rastlut)) { 
        LUTvar <- names(rastlut)[1] 
        newvar <- names(rastlut)[2]  

        ## Check that all values are in table and that class of merging variable matches 
        check.matchval(zstats, rastlut, "value", LUTvar, tab1txt="zstats", 
			tab2txt="rastlut") 
        tabs <- check.matchclass(zstats, rastlut, "value", LUTvar,  
 			tab1txt="zstats", tab2txt="rastlut") 
        zstats <- tabs$tab1 
        rastlut <- tabs$tab2 
                  
        zstats <- merge(zstats, rastlut, by.x="value", by.y=LUTvar) 
      }
  
      if (any(zonalstat == "count")) { 
        zstats.cnt <- dcast(zstats, zoneid ~ get(newvar), fun.aggregate=sum, 
 				value.var="count") 
        if (any(names(zstats.cnt) == "NA")) { 
          warning("NA values in output") 
          print(zstats.cnt[zstats.cnt[["NA"]] > 0,]) 
          zstats.cnt[["NA"]] <- NULL 
        } 
        zstats.cnt[is.na(zstats.cnt)] <- 0 
        var.name <- paste(prename, names(zstats.cnt)[-1], sep=".") 
        setnames(zstats.cnt, names(zstats.cnt)[-1], var.name) 
        setkey(zstats.cnt, "zoneid") 

        ## Check if class of key(zstats.cnt) in zstats.cnt matches class of key(zonalext) in zonalext
        tabs <- FIESTA::check.matchclass(zonalext, zstats.cnt, key(zonalext), key(zstats.cnt))
        zonalext <- tabs$tab1
        zstats.cnt <- tabs$tab2

        zonalext <- zonalext[zstats.cnt] 
        outnames <- c(outnames, var.name) 
      } 
      if (any(zonalstat == "proportion")) { 
        zstats.prop <- dcast(zstats, zoneid ~ get(newvar), fun.aggregate=sum, 
  			value.var="zoneprop") 
 
        if (any(names(zstats.prop) == "NA")) { 
          warning("NA values in output") 
          print(zstats.prop[zstats.prop[["NA"]] > 0,]) 
          zstats.prop[["NA"]] <- NULL 
        } 
        zstats.prop[is.na(zstats.prop)] <- 0 
        var.name <- paste(prename, names(zstats.prop)[-1], sep=".") 
        setnames(zstats.prop, names(zstats.prop)[-1], var.name) 
        setkey(zstats.prop, "zoneid") 

        ## Check if class of key(zstats.prop) in zstats.prop matches class of key(zonalext) in zonalext
        tabs <- FIESTA::check.matchclass(zonalext, zstats.prop, key(zonalext), key(zstats.prop))
        zonalext <- tabs$tab1
        zstats.prop <- tabs$tab2

        zonalext <- zonalext[zstats.prop] 
        outnames <- c(outnames, var.name) 
      } 
    } 
  }  

  if (savedata)
    write2csv(zonalext, outfolder=outfolder, outfilenm=outfn, outfn.pre=outfn.pre,
		outfn.date=outfn.date, overwrite=overwrite)
 

  returnlst <- list(zonalext=setDF(zonalext), outname=outnames,  
					rasterfile=rep(rastfn, length(outnames))) 
  return(returnlst) 
} 
