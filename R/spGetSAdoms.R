spGetSAdoms <- function(smallbnd, smallbnd_dsn=NULL, smallbnd.unique=NULL, 
	smallbnd.domain=NULL, smallbnd.filter=NULL, smallbnd.stfilter=NULL, 
	smallbnd.ecofilter=NULL, helperbnd=NULL, helperbnd_dsn=NULL, helperbnd.unique=NULL, 
	helperbnd.filter=NULL, largebnd=NULL, largebnd_dsn=NULL, largebnd.unique=NULL, 
	largebnd.filter=NULL, maxbnd=NULL, maxbnd_dsn=NULL, maxbnd.unique=NULL, 
	maxbnd.filter=NULL, helper_autoselect=TRUE, nbrdom.min=NULL, maxbnd.threshold=20, 
	largebnd.threshold=10, multiSAdoms=FALSE, showsteps=TRUE, savedata=FALSE, 
	savesteps=FALSE, maxbnd.addtext=TRUE, largebnd.addtext=FALSE, outfolder=NULL, 
	out_fmt="shp", out_dsn=NULL, outfn.pre=NULL, outfn.date=FALSE,  
	overwrite_dsn=FALSE, overwrite_layer=TRUE, addstate=FALSE, dissolve=FALSE) {
  ##############################################################################
  ## DESCRIPTION
  ## Generates small area domains for input to Small Area Module (modSA*).
  ## Small area boundaries are unioned with helper boundaries and aggregated to
  ## unique domains. The following steps are included in function.
  ##  1) Import small area boundary (smallbnd)
  ##  2) Apply filters to small area boundary
  ##	   - smallbnd.filter	 - filter using attribute in smallbnd
  ##	   - smallbnd.stfilter	 - filter boundary by state (e.g., c('Idaho', 'Montana'))
  ##	   - smallbnd.ecofilter	- filter boundary by eco (e.g. PROVINCE == 'M332')
  ##  3) Import helper boundary (modeling domains)
  ##  4) Apply filter for helper boundary
  ##	   - helperbnd.filter	- filter using attribute in helperbnd
  ##  5) Import large boundary (modeling extent)
  ##  6) Apply filter for large boundary
  ##	   - largebnd.filter	- filter using attribute in largebnd
  ##  7) Import max boundary (maximum modeling extent)
  ##  8) Apply filter for max boundary
  ##	   - maxbnd.filter	- filter using attribute in maxbnd
  ##  9) Check projections. If any boundary is projected, get most common projection 
  ## 10) Get intersection of helper and/or large boundaries
  ## 11) Autoselect modeling domains for smallbnd
  ## 12) Union modeling domains with smallbnd
  ## 13) Append 2 new variables: 
  ## 	  DOMAIN - modeling polygons; AOI - Indicator of small area (1/0)
  ##	  If helperbnd, union helper and small area polygons
  ##	     DOMAIN = union polygons; AOI=1 if small area polygon
  ##	  If no helperbnd, 
  ##	     DOMAIN = small area polygons; AOI=1 for all polygons
  ## 14) Dissolve unioned polygons by modeling domains
  ## 15) Calculate area of dissolved polygons
  ##
  ## VALUE
  ## Spatial Polygons of domain polygons, including DOMAIN and AOI attributes.
  ##############################################################################

  ## Check for necessary packages
  ###########################################################

  ## Set global variables
  gui <- FALSE  

#    smallbnd.domain = NULL
#    helperbnd.filter = NULL
#    largebnd.filter = NULL
#    savedata = FALSE
#    smallbnd.stfilter = NULL
#    maxbnd.filter = NULL
#    multiSAdoms = FALSE
#    nbrdom.min = NULL
#    maxbnd.threshold = 51
#    largebnd.threshold = 10

  ##################################################################
  ## CHECK INPUT PARAMETERS
  ##################################################################
  polyunion <- FALSE
  smallishelper <- FALSE
  largeishelper <- FALSE
  maxislarge <- FALSE
  #smallishelper <- FALSE


  ## Check savedata
  #############################################################################
  savedata <- FIESTA::pcheck.logical(savedata, varnm="savedata", 
		title="Save SAdoms?", first="YES", gui=gui) 

  ## Check showsteps
  #############################################################################
  showsteps <- FIESTA::pcheck.logical(showsteps, varnm="showsteps", 
		title="Show steps?", first="YES", gui=gui) 

  ## Check savesteps
  #############################################################################
  savesteps <- FIESTA::pcheck.logical(savesteps, varnm="savesteps", 
		title="Save step data?", first="YES", gui=gui)  

  ## Check overwrite, outfn.date, outfolder, outfn 
  ########################################################
  if (savedata || savesteps) {
    outlst <- pcheck.output(out_dsn=out_dsn, out_fmt=out_fmt, 
		outfolder=outfolder, outfn.pre=outfn.pre, outfn.date=outfn.date, 
		overwrite_dsn=overwrite_dsn, overwrite_layer=overwrite_layer,
		createSQLite=FALSE, gui=gui)
    out_dsn <- outlst$out_dsn
    outfolder <- outlst$outfolder
    out_fmt <- outlst$out_fmt
    overwrite_layer <- outlst$overwrite_layer

    if (savesteps) {
      stepfolder <- file.path(outfolder, "SAdoms_steps")
      if (!dir.exists(stepfolder)) dir.create(stepfolder)
      step_dsn <- NULL
      step_fmt <- "shp"
    }
  }

  #############################################################################
  ## smallbnd
  #############################################################################
  smallbndx <- pcheck.spatial(layer=smallbnd, dsn=smallbnd_dsn, caption="small boundary",
		stopifnull=TRUE)
  smallbndx.prj <- sf::st_crs(smallbndx)
  smallbndnmlst <- names(smallbndx)
  smallbnd.unique <- pcheck.varchar(var2check=smallbnd.unique, varnm="smallbnd.unique", 
		gui=gui, checklst=smallbndnmlst, caption="Small area attribute", 
		warn=paste(smallbnd.unique, "not in smallbnd"), stopifnull=FALSE)
  if (!is.null(smallbnd.unique)) {
    smallbndnmlst <- smallbndnmlst[smallbndnmlst != smallbnd.unique]
  }
  smallbnd.domain <- pcheck.varchar(var2check=smallbnd.domain, varnm="smallbnd.domain", 
		gui=gui, checklst=smallbndnmlst, caption="Small area domain", 
		stopifnull=FALSE, stopifinvalid=FALSE)
  if (all(is.null(smallbnd.unique), is.null(smallbnd.domain))) {
    smallbndx$SMALLAREA <- "SMALLAREA"
    smallbnd.unique <- "SMALLAREA"
  } else if (is.null(smallbnd.unique)) {
    smallbnd.unique <- smallbnd.domain
  } else {
    smallbnd.domain <- smallbnd.unique
  }    
  if (any(table(smallbndx[[smallbnd.unique]])) > 1) {
    message("smallbnd.unique is not unique")
  }
  ## Apply smallbnd.filter
  ####################################################################
  if (!is.null(smallbnd.filter))  {
    smallbndx <- subset(smallbndx, eval(parse(text = smallbnd.filter)))
  }
  smallbnd.cols <- names(smallbndx)
 
  ## Aggregate all fires to one polygon
  if (dissolve) {
    smallbndx <- sf_dissolve(smallbndx, areacalc=FALSE)
    smallbnd.unique <- "tmp"
    smallbnd.domain <- "tmp"
  }
 
  ## Apply smallbnd.stfilter (Just state)
  ####################################################################
  if (!is.null(smallbnd.stfilter)) {
    stunitco <- FIESTA::stunitco
    stlst <- sort(unique(stunitco$STATENM))
    if (!all(smallbnd.stfilter %in% sort(unique(stunitco$STATENM)))) 
      stop("smallbnd.stfilter is invalid")
    smallbnd.stfilter <- paste0("STATENM %in% c(", 
			addcommas(smallbnd.stfilter , quotes=TRUE), ")")
    stunitcof <- datFilter(stunitco, smallbnd.stfilter, stopifnull=TRUE)$xf

    ## Need to dissolve because small area could be in multiple counties
    stunitcof <- sf_dissolve(stunitcof, areacalc=FALSE)

    ## Check projections
    crsdat <- crsCompare(stunitcof, smallbndx, nolonglat=TRUE)
    stunitcof <- crsdat$x
    smallbndx <- crsdat$ycrs

    ## Intersect smallbnd with statebnd
    smallbndx2 <- selectByIntersects(smallbndx, stunitcof, 50)
    if (showsteps) {
      plot(sf::st_geometry(stunitcof))
      plot(sf::st_geometry(smallbndx2), add=TRUE, border="red")
    }

    if (is.null(smallbndx2) || nrow(smallbndx2) == 0) {
      message("the smallbnd has less than 50% overlap with the state boundary... returning NULL")
      return(NULL)
    } else {
      smallbndx <- smallbndx2
      rm(smallbndx2)
    }
  }
 
  ## Apply smallbnd.ecofilter
  ####################################################################
  if (!is.null(smallbnd.ecofilter)) {
    ecomap <- FIESTA::ecomap
    ecomapf <- datFilter(ecomap, smallbnd.ecofilter, stopifnull=TRUE)$xf

    ## Dissolve filtered ecomap layer
    ecomapf <- sf_dissolve(ecomapf, areacalc=FALSE)
    ecomap.cols <- names(ecomapf)

    ## Check projections
    crsdat <- crsCompare(ecomapf, smallbndx, nolonglat=TRUE)
    ecomapf <- crsdat$x
    smallbndx <- crsdat$ycrs

    ## Intersect smallbnd with ecomapf
    #smallbndx2 <- suppressWarnings(selectByIntersects(sf::st_make_valid(smallbndx), ecomapf, 49))
    smallbndx2 <- selectByIntersects(smallbndx, ecomapf, 50)

    if (is.null(smallbndx2) || nrow(smallbndx2) == 0) {
      message("the smallbnd has less than 50% overlap with the ecomap boundary... returning NULL")
      return(NULL)
    } else {
      smallbndx <- smallbndx2
      rm(smallbndx2)
    }
    if (showsteps) {
      plot(sf::st_geometry(ecomapf))
      plot(sf::st_geometry(smallbndx), add=TRUE, border="red")
    }
  }
  smallbndx.prj <- sf::st_crs(smallbndx)

  message("smallbnd...")
  #print(st_drop_geometry(smallbndx))


  ## Add AOI attribute to smallbndx
  smallbndx$AOI <- 1

  #############################################################################
  ## maxbnd
  #############################################################################
  maxbndx <- pcheck.spatial(layer=maxbnd, dsn=maxbnd_dsn, caption="max boundary")
 
  ## Check maxbndx
  if (!is.null(maxbndx)) {
    maxbnd.unique <- FIESTA::pcheck.varchar(var2check=maxbnd.unique, 
		varnm="maxbnd.unique", gui=gui, checklst=names(maxbndx), 
		caption="max areas attribute", 
		warn=paste(maxbnd.unique, "not in maxbnd"), stopifnull=TRUE)
  
    ## Apply maxbnd.filter
    if (!is.null(maxbndx) && !is.null(maxbnd.filter)) {
      FIESTA::check.logic(maxbndx, maxbnd.filter, "maxbndfilter")
      maxbndx <- subset(maxbndx, eval(parse(text = maxbnd.filter)))
      if (length(maxbndx) == 0) stop("maxbnd.filter removed all features")
    }

    ## Check projections (reproject smallbndx to projection of helperbndx
    maxbndx <- crsCompare(maxbndx, smallbndx, nolonglat=TRUE)$x

    ## Intersect smallbnd with ecomapf
    #smallbndx <- selectByIntersects(smallbndx, ecomapf, 49)
    #maxbndx2 <- selectByIntersects(maxbndx, smallbndx, 2)

    ## Check projections (reproject smallbndx to projection of helperbndx
    maxbndx <- crsCompare(maxbndx, smallbndx, nolonglat=TRUE)$x
  
    ## Change name of maxbnd.unique if equals largebnd.unique 
    #if (identical(maxbnd.unique, c(helperbnd.unique, largebnd.unique))) {
    #  tmp.unique <- checknm(maxbnd.unique, names(maxbndx))
    #  names(maxbndx)[names(maxbndx) == maxbnd.unique] <- tmp.unique
    #  maxbnd.unique <- tmp.unique
    #}
  }

  #############################################################################
  ## largebnd
  #############################################################################
  largebndx <- pcheck.spatial(layer=largebnd, dsn=largebnd_dsn, 
		caption="large boundary")

  ## Check largebndx
  if (!is.null(largebndx)) {
    if (is.null(maxbnd)) {
      maxislarge <- TRUE
    }
    largebnd.unique <- FIESTA::pcheck.varchar(var2check=largebnd.unique, 
		varnm="largebnd.unique", gui=gui, checklst=names(largebndx), 
		caption="max areas attribute", 
		warn=paste(largebnd.unique, "not in largebnd"), stopifnull=TRUE)
  
    ## Apply largebnd.filter
    if (!is.null(largebndx) && !is.null(largebnd.filter)) {
      FIESTA::check.logic(largebndx, largebnd.filter, "largebnd.filter")
      largebndx <- subset(largebndx, eval(parse(text = largebnd.filter)))
      if (length(largebndx) == 0) stop("largebnd.filter removed all features")
    }

    ## Check projections (reproject largebndx to projection of helperbndx
    prjdat <- crsCompare(largebndx, smallbndx, nolonglat=TRUE)
    largebndx <- prjdat$x
    smallbndx <- prjdat$ycrs

    ## Get intersection of smallbndx
    #smallbndx <- suppressWarnings(selectByIntersects(smallbndx, largebndx, 49))

    ## Change name of largebnd.unique if equals helperbnd.unique
    #if (identical(largebnd.unique, helperbnd.unique)) {
    #  tmp.unique <- checknm(largebnd.unique, names(largebndx))
    #  names(largebndx)[names(largebndx) == largebnd.unique] <- tmp.unique
    #  largebnd.unique <- tmp.unique
    #}
  }

   
  #############################################################################
  ## helperbnd
  #############################################################################
  helperbndx <- pcheck.spatial(layer=helperbnd, dsn=helperbnd_dsn, 
		caption="helper boundary")

  if (is.null(largebndx)) {
    largebnd.unique <- suppressWarnings(pcheck.varchar(var2check=largebnd.unique, 
		checklst=names(helperbndx), stopifinvalid=FALSE))
    if (!is.null(largebnd.unique)) {
      if (!is.null(largebnd.filter)) {
        FIESTA::check.logic(helperbndx, largebnd.filter, "largebndfilter")
        largebndx <- subset(helperbndx, eval(parse(text = largebnd.filter)))
        if (length(largebndx) == 0) stop("largebnd.filter removed all features")
      } else {
        largebndx <- helperbndx
      }
    } else {
      if (helper_autoselect) {
        message("no largebnd included... no autoselect")
        helper_autoselect <- FALSE
      }
    }
  }
 
  ## Check helperbnd.unique
  ## If helperbnd=NULL, check smallbnd for helperbnd.unique
  if (is.null(helperbndx)) {
    helperbndx <- smallbndx
    smallishelper <- TRUE
    helperbnd.unique <- pcheck.varchar(var2check=helperbnd.unique, 
		varnm="helperbnd.unique", gui=gui, checklst=names(helperbndx), 
		caption="Helper areas attribute")
    if (is.null(helperbnd.unique)) {
      helperbnd.unique <- smallbnd.unique
    }
  } else {
    helperbnd.unique <- FIESTA::pcheck.varchar(var2check=helperbnd.unique, 
		varnm="helperbnd.unique", gui=gui, checklst=names(helperbndx), 
		caption="Helper areas attribute", 
		warn=paste(helperbnd.unique, "not in helperbnd"), stopifnull=TRUE)

    if (any(table(helperbndx[[helperbnd.unique]])) > 1) {
      message("helperbnd.unique is not unique")
    }  
    ## Change name of helperbnd.unique if equals smallbnd.unique
    if (identical(helperbnd.unique, smallbnd.unique)) {
      tmp.unique <- checknm(helperbnd.unique, names(helperbndx))
      names(helperbndx)[names(helperbndx) == helperbnd.unique] <- tmp.unique     
      helperbnd.unique <- tmp.unique
    } else {
      polyunion <- TRUE
    }
  }

  if (!smallishelper) {
    ## Apply helperbnd.filter
    if (!is.null(helperbnd.filter)) {
      FIESTA::check.logic(helperbndx, helperbnd.filter, "helperbnd filter")
      helperbndx <- subset(helperbndx, eval(parse(text = helperbnd.filter)))
    }

    ## Check projections (reproject smallbndx to projection of helperbndx
    prjlst <- crsCompare(smallbndx, helperbndx)
    smallbndx <- prjlst$x
    helperbndx <- prjlst$ycrs
  }
  helperbndx.prj <- sf::st_crs(helperbndx)


  ## clip to US boundary
  #############################################################################

  ## Add AOI attribute to smallbndx
  #smallbndx$AOI <- 1

  ## Display smallbnd
#  if (showsteps) {
#    plot(sf::st_geometry(smallbndx)) 

#    if (!is.null(smallbnd.ecofilter)) {
#      plot(sf::st_geometry(ecomapf))
#      plot(sf::st_geometry(ecomapf), add=TRUE)
#    } else if (!is.null(smallbnd.stfilter)) {
#      plot(sf::st_geometry(stunitcof), border="transparent")
#      plot(sf::st_geometry(stunitcof), add=TRUE)
#    #} else {
#      #plot(sf::st_geometry(stunitco), border="dark grey", add=TRUE)
#    }
#    #plot(sf::st_geometry(smallbndx), add=TRUE) 
#  } 

  #############################################################################
  ### DO THE WORK
  #############################################################################
  if (helper_autoselect) { 
    autoselectlst <- helper.select(smallbndx, smallbnd.unique, smallbnd.domain=smallbnd.domain,
 		helperbndx=helperbndx, helperbnd.unique=helperbnd.unique, largebndx=largebndx, 
		largebnd.unique=largebnd.unique, maxbndx=maxbndx, maxbnd.unique=maxbnd.unique,
 		nbrdom.min=nbrdom.min, maxislarge=maxislarge, largeishelper=largeishelper, 
		polyunion=polyunion, showsteps=showsteps, savesteps=savesteps, 
		stepfolder=stepfolder, step_dsn=step_dsn, out_fmt=step_fmt, multiSAdoms=multiSAdoms, 
		maxbnd.threshold=maxbnd.threshold, largebnd.threshold=largebnd.threshold, 
		maxbnd.addtext=maxbnd.addtext, largebnd.addtext=largebnd.addtext, 
		overwrite=overwrite_layer)
    SAdomslst <- autoselectlst$SAdomslst
    helperbndxlst <- autoselectlst$helperbndxlst
    smallbndxlst <- autoselectlst$smallbndxlst
    largebndx <- autoselectlst$largebndx.int
    maxbndx <- autoselectlst$maxbndx.int
 
  } else {
    ## Add DOMAIN column to all rows
    helperbndx$DOMAIN <- helperbndx[[helperbnd.unique]]
    SAdomslst <- list(SAdoms=helperbndx)
    smallbndxlst <- list(smallbnd=smallbndx)
  }

  ###########################################################################
  ## Aggregate (dissolve) polygons on DOMAIN and calculate area on dissolved polygons
  ###########################################################################
  SAdomslst <- lapply(SAdomslst, sf_dissolve, c("DOMAIN", "AOI"))
  #SAdomslst <- lapply(SAdomslst, sf_dissolve, "DOMAIN")

  if (showsteps) {
    ## Set plotting margins
    mar <-  par("mar")
    par(mar=c(1,1,1,1))
  }

  for (i in 1:length(SAdomslst)) {   
    ## Check domain
    if (any(table(SAdomslst[[i]]$DOMAIN) > 1)) {
      stop("check smallbnd.domain.. may not be unique")
    } 

    ## Merge other attributes (smallbnd.domain) to SAdoms
    smallbndvars <- unique(c(smallbnd.domain, 
		names(smallbndxlst[[i]])[!names(smallbndxlst[[i]]) %in% names(SAdomslst[[i]])]))
    SAdomslst[[i]] <- merge(SAdomslst[[i]], 
		sf::st_drop_geometry(smallbndxlst[[i]][, smallbndvars]), 
		by.x="DOMAIN", by.y=smallbnd.domain, all.x=TRUE)

#    SAdomslst[[i]] <- merge(SAdomslst[[i]], 
#		sf::st_drop_geometry(smallbndx[, c(smallbnd.unique, smallbnd.domain)]), 
#		by.x="DOMAIN", by.y=smallbnd.unique, all.x=TRUE)

    ## Join maxbndx and largebndx attributes (using largest overlap)
    if (!maxislarge && !is.null(maxbndx) && !maxbnd.unique %in% names(SAdomslst[[i]])) {
      SAdomslst[[i]] <- suppressWarnings(sf::st_join(SAdomslst[[i]], 
					maxbndx[, maxbnd.unique], largest=TRUE))
    }
    if (!largeishelper && !is.null(largebndx) && !largebnd.unique %in% names(SAdomslst[[i]])) {
      SAdomslst[[i]] <- suppressWarnings(sf::st_join(SAdomslst[[i]], 
					largebndx[, largebnd.unique], largest=TRUE))
    }
    if (addstate) {
      SAdomslst[[i]] <- suppressWarnings(sf::st_join(SAdomslst[[i]], 
					stunitco[, "STATECD"], largest=TRUE))
    }
    if (showsteps) {
      plot(sf::st_geometry(SAdomslst[[i]]), border="dark grey")
      plot(sf::st_geometry(smallbndxlst[[i]]), add=TRUE, border="red", lwd=1, color="translucent")
    } 
    if (savedata) {
      SAdoms_layer <- "SAdoms"
      smallbnd_layer <- "smallbnd"
      if (length(SAdomslst) > 1) {
        SAdoms_layer <- paste0(SAdoms_layer, i)
        smallbnd_layer <- paste0(smallbnd_layer, i)
      }
      spExportSpatial(SAdomslst[[i]], outfolder=outfolder, out_fmt=out_fmt, 
			out_dsn=out_dsn, out_layer=SAdoms_layer, append_layer=TRUE,
			overwrite_layer=overwrite_layer)
      spExportSpatial(smallbndxlst[[i]], outfolder=outfolder, out_fmt=out_fmt, 
			out_dsn=out_dsn, out_layer=smallbnd_layer, append_layer=TRUE,
			overwrite_layer=overwrite_layer)
    }

    if (savesteps) {
      jpgfn <- paste0(stepfolder, "/SAdoms.jpg")
      jpeg(jpgfn, res=400, units="in", width=8, height=10)
          plot(sf::st_geometry(SAdomslst[[i]]), border="dark grey")
          plot(sf::st_geometry(smallbndxlst[[i]]), add=TRUE, border="red", lwd=1)
      dev.off()
      message("Writing jpg to ", jpgfn, "\n")
    }
  }
  if (showsteps) {
    par(mar=mar)
  }

  rm(smallbndx)
  rm(helperbndx)
  rm(largebndx)
  rm(maxbndx)
  if (helper_autoselect) rm(autoselectlst)
  gc()

  #if (!multiSAdoms) {
  #  SAdoms <- SAdomdat$SAdomlst[[1]]
  #  smallbnd <- SAdomdat$smallbndlst[[1]]
  #}  

  returnlst <- list(SAdomlst=SAdomslst, smallbndlst=smallbndxlst, 
		smallbnd.unique=smallbnd.unique)
  return(returnlst)
}
