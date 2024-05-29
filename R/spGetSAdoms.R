#' Spatial wrapper - Generate a set of model domain units for Small Area Estimation
#' (SAE) strategies.
#' 
#' Spatial process to generate a set of model domains (i.e., helper polygons)
#' for Small Area Estimation (SAE) strategies. If helper_autoselect=TRUE, an
#' automated process is used to select helper polygons within a large area
#' overlapping the small area. The helper polygons are unioned with the small
#' area polygons, resulting in a set of model domains that can be used for SAE.
#' 
#' \bold{optional boundaries}
#' 
#' The helperbnd, largebnd, and maxbnd are optional. If helperbnd=NULL, the
#' smallbnd polygons are used for model domain units. If largebnd=NULL, the
#' maxbnd is used to define the large area. If maxbnd=NULL, the largebnd is
#' used to restrain the model extent. If both, largebnd=NULL and maxbnd=NULL,
#' the extent of the smallbnd or helperbnd is used for defining and restraining
#' the model extent.
#' 
#' \bold{nbrdom.min}
#' 
#' The number of helper polygons selected are defined by nbrdom.min parameter.
#' If nbrdom.min=NULL, all helper polygons within the large area extent are
#' selected.
#' 
#' \bold{multiSAdoms}
#' 
#' Use multiSAdoms parameter when small area of interest has multiple polygon
#' features and the small area polygons overlap (within maxbnd.threshold) more
#' than one maxbnd polygon. If multiSAdoms=TRUE, more than one set of model
#' domain units are generated; one for each maxbnd where overlap is within
#' maxbnd.threshold. If multiSAdoms=FALSE, only one set of model domain units
#' are generated, using the maxbnd with the greatest overlap.
#' 
#' \bold{AOI attribute}
#' 
#' A variable named 'AOI' is appended to the SAdoms attribute table to
#' distinguish between the small area of interest polygons and the helper
#' domain units.
#' 
#' @param smallbnd sf R object or String. Small area of interest boundary.  Can
#' be a spatial polygon object, full pathname to a shapefile, or name of a
#' layer within a database.
#' @param smallbnd_dsn String. Data source name (dsn; e.g., sqlite or shapefile
#' pathname) of smallbnd. The dsn varies by driver. See gdal OGR vector formats
#' (https://www.gdal.org/ogr_formats.html). Optional if smallbnd is an R
#' object.
#' @param smallbnd.unique String. The attribute in smallbnd that defines unique
#' domain identifier in smallbnd that defines the unique small area(s). If
#' NULL, an attribute is appended to smallbnd attribute table and used as
#' smallbnd.unique, defining one polygon (SMALLAREA="SMALLAREA").
#' @param smallbnd.domain String. A different attribute to use as for grouped
#' modeling domains (optional). If NULL, smallbnd.domain=smallbnd.unique.
#' @param smallbnd.filter String. A filter for smallbnd. Must be R syntax.
#' @param smallbnd.stfilter String. A spatial filter for smallbnd to include
#' only smallbnd polygons that intersect (or overlap >= 30%) the filter
#' boundary. The filter is based on the stunitco internal R object, with
#' attributes: STATECD, STATENM, UNITCD, UNITNM, COUNTYCD, COUNTYNM. The filter
#' should include one of these attributes and must be R syntax.
#' @param helperbnd sf R object or String. Name of polygon spatial layer
#' delineating helper polygons for small area models. Can be a spatial polygon
#' object, full pathname to a shapefile, or name of a layer within a database.
#' @param helperbnd_dsn String. Data source name (dsn; e.g., sqlite or
#' shapefile pathname) of helperbnd. The dsn varies by driver. See gdal OGR
#' vector formats (https://www.gdal.org/ogr_formats.html). Optional if
#' helperbnd is an R object.
#' @param helperbnd.unique String. The attribute in helper polygon layer that
#' defines unique helper polygons.
#' @param helperbnd.filter String. A filter for helperbnd. Must be R syntax.
#' @param largebnd sf R object or String. Name of large area polygon spatial
#' layer, defining the model data extent for building small are models.  Can be
#' a spatial polygon object, full pathname to a shapefile, or name of a layer
#' within a database.
#' @param largebnd_dsn String. Data source name (dsn; e.g., sqlite or shapefile
#' pathname) of largebnd. The dsn varies by driver. See gdal OGR vector formats
#' (https://www.gdal.org/ogr_formats.html). Optional if largebnd is an R
#' object.
#' @param largebnd.unique String. The attribute in largebnd polygon layer that
#' defines unique large area polygon(s).
#' @param largebnd.filter String. A filter for largebnd. Must be R syntax.
#' @param maxbnd sf R object or String. Name of polygon spatial layer, defining
#' the maximum model data restraint for adding more helper polygons for
#' building small are models. Can be a spatial polygon object, full pathname to
#' a shapefile, or name of a layer within a database.
#' @param maxbnd_dsn String. Data source name (dsn; e.g., sqlite or shapefile
#' pathname) of maxbnd. The dsn varies by driver. See gdal OGR vector formats
#' (https://www.gdal.org/ogr_formats.html). Optional if maxbnd is an R object.
#' @param maxbnd.unique String. The attribute in maxbnd polygon layer that
#' defines unique max restraint area(s).
#' @param maxbnd.filter String. A filter for maxbnd. Must be R syntax.
#' @param helper_autoselect Logical. If TRUE, the helper boundaries are
#' automatically selected based on intersection with maxbnd and/or largebnd and
#' number of helperbnds defined by nbrdom.min.
#' @param nbrdom.min Integer. Set number for minimum domains for modeling. If
#' NULL, all domains within largebnd are selected.
#' @param maxbnd.threshold Integer. Percent for including additional maxbnds
#' for selecting helperbnds. If multiSAdoms=FALSE, the maxbnd with greatest
#' percentage over the maxbnd.threshold is selected.
#' @param largebnd.threshold Integer. Percent for including additional
#' largebnds for selecting helperbnds.
#' @param multiSAdoms Logical. If TRUE, and the percent intersect of smallbnd
#' with maxbnd is greater than maxbnd.threshold, more than 1 SAdoms will be
#' output in list.
#' @param bayes Logical. If TRUE, does not union smallbnd with largebnd.
#' If multiSAdoms = TRUE, returns intersecting maxbnd where larger than 
#' maxbnd threshold.
#' @param showsteps Logical. If TRUE, intermediate steps of selection process
#' are displayed.
#' @param savedata Logical. If TRUE, save SAdoms spatial layer to outfolder.
#' @param savesteps Logical. If TRUE, save steps spatial intermediate layers
#' and JPG images. All spatial layers are output as *.shp format in a separate
#' folder (SAdoms_steps).
#' @param saveobj Logical. If TRUE, save SAdomdat object to outfolder.
#' @param objnm String. Name of *.rds object.
#' @param maxbnd.addtext Logical. If TRUE, adds text to intermediate step plots
#' for maxbnd displays.
#' @param largebnd.addtext Logical. If TRUE, adds text to intermediate step
#' plots for largebnd displays.
#' @param savedata_opts List. See help(savedata_options()) for a list
#' of options. Only used when savedata = TRUE.  
#' @param addstate Logical. If TRUE, appends state attribute to SAdoms.
#' @param byeach Logical. If TRUE, creates an SAdom for each smallbnd polygon.
#' @param dissolve Logical. If TRUE, aggregates polygons to smallbnd.domain or
#' smallbnd.unique.
#' @return \item{SAdomslst}{ List object. Set(s) of model domain units. If
#' multiSAdoms=TRUE, the list may have more than one set of model domain units.
#' } \item{smallbndlst}{ List object. smallbnd(s). If multiSAdoms=TRUE, the
#' list may have more than one set of smallbnd. }
#' 
#' If exportsp=TRUE, the SAdoms spatial object(s) is exported to outfolder,
#' with format specified by out_fmt.
#' @note
#' 
#' If exportsp=TRUE and out_fmt="shp":\cr The st_write (sf) function is
#' called. The ArcGIS driver truncates variable names to 10 characters or less.
#' Variable names are changed before export using an internal function
#' (trunc10shp). If Spatial object has more than 1 record, it will be returned
#' but not exported.
#' 
#' The spTransform (sf) method is used for on-the-fly map projection
#' conversion and datum transformation using PROJ.4 arguments. Datum
#' transformation only occurs if the +datum tag is present in the both the from
#' and to PROJ.4 strings. The +towgs84 tag is used when no datum transformation
#' is needed. PROJ.4 transformations assume NAD83 and WGS84 are identical
#' unless other transformation parameters are specified.  Be aware, providing
#' inaccurate or incomplete CRS information may lead to erroneous data shifts
#' when reprojecting. See spTransform help documentation for more details.
#' @author Tracey S. Frescino
#' @keywords data
#' @export spGetSAdoms
spGetSAdoms <- function(smallbnd, 
                        smallbnd_dsn = NULL, 
                        smallbnd.unique = NULL, 
                        smallbnd.domain = NULL, 
                        smallbnd.filter = NULL, 
                        smallbnd.stfilter = NULL, 
                        helperbnd = NULL, 
                        helperbnd_dsn = NULL, 
                        helperbnd.unique = NULL, 
                        helperbnd.filter = NULL, 
                        largebnd = NULL, 
                        largebnd_dsn = NULL, 
                        largebnd.unique = NULL, 
                        largebnd.filter = NULL, 
                        maxbnd = NULL, 
                        maxbnd_dsn = NULL, 
                        maxbnd.unique = NULL, 
                        maxbnd.filter = NULL, 
                        helper_autoselect = TRUE, 
                        nbrdom.min = NULL, 
                        maxbnd.threshold = 10, 
                        largebnd.threshold = 5, 
                        multiSAdoms = FALSE,
                        bayes = FALSE,
                        showsteps = TRUE, 
                        savedata = FALSE, 
                        savesteps = FALSE, 
                        saveobj = FALSE,
                        objnm = "SAdomdat",
                        maxbnd.addtext = TRUE, 
                        largebnd.addtext = FALSE, 
                        savedata_opts = NULL, 
                        addstate = FALSE, 
                        dissolve = FALSE,
                        byeach = FALSE) {
  ##############################################################################
  ## DESCRIPTION
  ## Generates small area domains for input to Small Area Module (modSA*).
  ## Small area boundaries are unioned with helper boundaries and aggregated to
  ## unique domains. The following steps are included in function.
  ##  1) Import small area boundary (smallbnd)
  ##  2) Apply filters to small area boundary
  ##	   - smallbnd.filter	 - filter using attribute in smallbnd
  ##	   - smallbnd.stfilter	 - filter boundary by state (e.g., c('Idaho', 'Montana'))
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
  ## CHECK PARAMETER NAMES
  ##################################################################
  
  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- names(formals(spGetSAdoms))
  if (!all(input.params %in% formallst)) {
    miss <- input.params[!input.params %in% formallst]
    stop("invalid parameter: ", toString(miss))
  }

  ## Check parameter lists
  pcheck.params(input.params, savedata_opts=savedata_opts)
  
  ## Set savedata defaults
  savedata_defaults_list <- formals(savedata_options)[-length(formals(savedata_options))]
  
  for (i in 1:length(savedata_defaults_list)) {
    assign(names(savedata_defaults_list)[[i]], savedata_defaults_list[[i]])
  }
  
  ## Set user-supplied savedata values
  if (length(savedata_opts) > 0) {
    if (!savedata) {
      message("savedata=FALSE with savedata parameters... no data are saved")
    }
    for (i in 1:length(savedata_opts)) {
      if (names(savedata_opts)[[i]] %in% names(savedata_defaults_list)) {
        assign(names(savedata_opts)[[i]], savedata_opts[[i]])
      } else {
        stop(paste("Invalid parameter: ", names(savedata_opts)[[i]]))
      }
    }
  }
  

  ##################################################################
  ## CHECK INPUT PARAMETERS
  ##################################################################
  polyunion <- FALSE
  smallishelper <- FALSE
  largeishelper <- FALSE
  maxislarge <- FALSE
  #smallishelper <- FALSE

  ## Check multiSAdoms
  #############################################################################
  multiSAdoms <- pcheck.logical(multiSAdoms, varnm="multiSAdoms", 
		title="More than 1 SAdom?", first="YES", gui=gui) 

  ## Check byeach
  #############################################################################
  byeach <- pcheck.logical(byeach, varnm="byeach", 
		title="By each smallbnd poly?", first="YES", gui=gui) 
  if (byeach && !multiSAdoms) {
    multiSAdoms <- TRUE
  }

  ## Check bayes
  #############################################################################
  bayes <- pcheck.logical(bayes, varnm="bayes", 
                           title="Baysian?", first="NO", gui=gui) 
  
  ## Check savedata
  #############################################################################
  savedata <- pcheck.logical(savedata, varnm="savedata", 
		title="Save SAdoms?", first="YES", gui=gui) 

  ## Check showsteps
  #############################################################################
  showsteps <- pcheck.logical(showsteps, varnm="showsteps", 
		title="Show steps?", first="YES", gui=gui)   

  ## Check savesteps
  #############################################################################
  savesteps <- pcheck.logical(savesteps, varnm="savesteps", 
		title="Save step data?", first="YES", gui=gui)  

  ## Check saveobj
  saveobj <- pcheck.logical(saveobj, varnm="saveobj",
		title="Save SApopdat object?", first="YES", gui=gui, stopifnull=TRUE)


  ## Check overwrite, outfn.date, outfolder, outfn 
  ########################################################
  if (savedata) {
    outlst <- pcheck.output(outfolder=outfolder, out_dsn=out_dsn, 
            out_fmt=out_fmt, outfn.pre=outfn.pre, outfn.date=outfn.date, 
            overwrite_dsn=overwrite_dsn, overwrite_layer=overwrite_layer,
            add_layer=add_layer, append_layer=append_layer, gui=gui)
    outfolder <- outlst$outfolder
    out_dsn <- outlst$out_dsn
    out_fmt <- outlst$out_fmt
    overwrite_layer <- outlst$overwrite_layer
    append_layer <- outlst$append_layer
    outfn.date <- outlst$outfn.date
    outfn.pre <- outlst$outfn.pre

  } else if (savesteps || saveobj) {
    outfolder <- pcheck.outfolder(outfolder)
    if (is.null(out_layer)) {
      out_layer <- "SAdoms"
    }
    if (!is.null(outfn.pre)) {
      out_layer <- paste0(outfn.pre, "_", out_layer)
    }
  }
    
  if (savesteps) {
    stepfolder <- file.path(outfolder, "SAdoms_steps")
    if (!dir.exists(stepfolder)) dir.create(stepfolder)
    step_dsn <- NULL
    step_fmt <- "shp"
  }

  #############################################################################
  ## smallbnd
  #############################################################################
  smallbndx <- pcheck.spatial(layer=smallbnd, dsn=smallbnd_dsn, 
                        caption="small boundary", stopifnull=TRUE)
  smallbndx <- checksf.longlat(smallbndx)
  if (!all(sf::st_is_valid(smallbndx))) {
    smallbndx <- sf::st_make_valid(smallbndx, 
                                 geos_method = 'valid_structure', 
                                 geos_keep_collapsed = FALSE)
  }
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
    warning("both smallbnd.unique and smallbnd.domain is null...  adding DOMAIN=1")
    smallbndx$DOMAIN <- 1
    smallbnd.unique=smallbnd.domain <- "DOMAIN"
  } else if (is.null(smallbnd.unique)) {
    smallbnd.unique <- smallbnd.domain
  } else if (is.null(smallbnd.domain)) {
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
    smallbndx$ONEDOM <- 1
    smallbnd.unique <- "ONEDOM"
    smallbnd.domain <- "ONEDOM"
  } else {
    ## Aggregate if smallbnd.domain is different than smallbnd.unique
    if (smallbnd.domain != smallbnd.unique) {
      if (length(unique(smallbndx[[smallbnd.domain]])) < length(unique(smallbndx[[smallbnd.unique]]))) {
        smallbndx <- sf_dissolve(smallbndx, col=smallbnd.domain)
        smallbnd.unique <- smallbnd.domain
      } else {
        smallbndx <- sf_dissolve(smallbndx, unique(c(smallbnd.unique, smallbnd.domain)))
      }
    } else {
      smallbndx <- sf_dissolve(smallbndx, unique(c(smallbnd.unique, smallbnd.domain)))
    }
  }
 
  ## Apply smallbnd.stfilter (Just state)
  ####################################################################
  if (!is.null(smallbnd.stfilter)) {
    stunitco <- FIESTAutils::stunitco
    stlst <- sort(unique(stunitco$STATENM))
    if (!all(smallbnd.stfilter %in% sort(unique(stunitco$STATENM)))) 
      stop("smallbnd.stfilter is invalid")
    smallbnd.stfilter <- paste0("STATENM %in% c(", 
			          addcommas(smallbnd.stfilter , quotes=TRUE), ")")
    stunitcof <- datFilter(stunitco, smallbnd.stfilter, stopifnull=TRUE)$xf

    ## Need to dissolve because small area could be in multiple counties
    stunitcof <- sf_dissolve(stunitcof, areacalc=FALSE)

    ## Check projections... project stunitco to smallbnd projections
    crsdat <- crsCompare(stunitcof, smallbndx, nolonglat=TRUE)
    stunitcof <- crsdat$x
    smallbndx <- crsdat$ycrs

    ## Intersect smallbnd with statebnd
    smallbndx2 <- suppressWarnings(selectByIntersects(smallbndx, stunitcof, 50))
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
#  if (!is.null(smallbnd.ecofilter)) {
#    ecomap <- FIESTA::ecomap
#    ecomapf <- datFilter(ecomap, smallbnd.ecofilter, stopifnull=TRUE)$xf
#
#    ## Dissolve filtered ecomap layer
#    ecomapf <- sf_dissolve(ecomapf, areacalc=FALSE)
#    ecomap.cols <- names(ecomapf)
#
#    ## Check projections
#    crsdat <- crsCompare(ecomapf, smallbndx, nolonglat=TRUE)
#    ecomapf <- crsdat$x
#    smallbndx <- crsdat$ycrs
#
#    ## Intersect smallbnd with ecomapf
#    #smallbndx2 <- suppressWarnings(selectByIntersects(sf::st_make_valid(smallbndx), ecomapf, 49))
#    smallbndx2 <- selectByIntersects(smallbndx, ecomapf, 50)
#
#    if (is.null(smallbndx2) || nrow(smallbndx2) == 0) {
#      message("the smallbnd has less than 50% overlap with the ecomap boundary... returning NULL")
#      return(NULL)
#    } else {
#      smallbndx <- smallbndx2
#      rm(smallbndx2)
#    }
#    if (showsteps) {
#      plot(sf::st_geometry(ecomapf))
#      plot(sf::st_geometry(smallbndx), add=TRUE, border="red")
#    }
#  }

#  message("smallbnd...")
#  #print(st_drop_geometry(smallbndx))


  ## Add AOI attribute to smallbndx
  smallbndx$AOI <- 1

  #############################################################################
  ## maxbnd
  #############################################################################
  maxbndx <- pcheck.spatial(layer=maxbnd, dsn=maxbnd_dsn, caption="max boundary")
 
  ## Check maxbndx
  if (!is.null(maxbndx)) {
    maxbnd.unique <- pcheck.varchar(var2check=maxbnd.unique, 
		varnm="maxbnd.unique", gui=gui, checklst=names(maxbndx), 
		caption="max areas attribute", 
		warn=paste(maxbnd.unique, "not in maxbnd"), stopifnull=TRUE)
  
    ## Apply maxbnd.filter
    if (!is.null(maxbndx) && !is.null(maxbnd.filter)) {
      check.logic(maxbndx, maxbnd.filter, "maxbndfilter")
      maxbndx <- subset(maxbndx, eval(parse(text = maxbnd.filter)))
      if (length(maxbndx) == 0) stop("maxbnd.filter removed all features")
    }

    ## Check projections (reproject maxbndx to projection of smallbndx
    maxbndx <- crsCompare(maxbndx, smallbndx, nolonglat=TRUE)$x

    ## Intersect smallbnd with ecomapf
    #smallbndx <- selectByIntersects(smallbndx, ecomapf, 49)
    #maxbndx2 <- selectByIntersects(maxbndx, smallbndx, 2)
  
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
  if (!all(sf::st_is_valid(largebndx))) {
    largebndx <- sf::st_make_valid(largebndx)
  }
  
  ## Check largebndx
  if (!is.null(largebndx)) {
    largebnd.unique <- pcheck.varchar(var2check=largebnd.unique, 
		varnm="largebnd.unique", gui=gui, checklst=names(largebndx), 
		caption="max areas attribute", 
		warn=paste(largebnd.unique, "not in largebnd"), stopifnull=FALSE)
  
    ## Apply largebnd.filter
    if (!is.null(largebndx) && !is.null(largebnd.filter)) {
      check.logic(largebndx, largebnd.filter, "largebnd.filter")
      largebndx <- subset(largebndx, eval(parse(text = largebnd.filter)))
      if (length(largebndx) == 0) stop("largebnd.filter removed all features")
    }

    ## Check projections (reproject largebndx to projection of smallbndx
    largebndx <- crsCompare(largebndx, smallbndx, nolonglat=TRUE)$x

    if (is.null(maxbndx)) {
      maxislarge <- TRUE
    } else {
      if (identical(largebndx, maxbndx) && identical(largebnd.unique, maxbnd.unique)) {
        maxislarge <- TRUE
      }
    }

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
  if (!all(sf::st_is_valid(helperbndx))) {
    helperbndx <- sf::st_make_valid(helperbndx)
  }
  
  if (is.null(largebndx)) {
    largebnd.unique <- suppressWarnings(pcheck.varchar(var2check=largebnd.unique, 
		checklst=names(helperbndx), stopifinvalid=FALSE))
    if (!is.null(largebnd.unique)) {
      if (!is.null(largebnd.filter)) {
        check.logic(helperbndx, largebnd.filter, "largebndfilter")
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
  
  if (is.null(maxbndx)) {
    maxbndx <- largebndx
    maxbnd.unique <- largebnd.unique
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
    helperbnd.unique <- pcheck.varchar(var2check=helperbnd.unique, 
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
      check.logic(helperbndx, helperbnd.filter, "helperbnd filter")
      helperbndx <- subset(helperbndx, eval(parse(text = helperbnd.filter)))
    }

    ## Check projections (reproject helperbndx to projection of smallbndx
    helperbndx <- crsCompare(helperbndx, smallbndx, nolonglat=TRUE)$x
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
    if (is.null(helperbndx)) {
      stop("invalid helperbnd for autoselection")
    }
    autoselectlst <- helper.select(smallbndx, smallbnd.unique=smallbnd.unique,
 		      smallbnd.domain=smallbnd.domain,
 		      helperbndx=helperbndx, helperbnd.unique=helperbnd.unique, 
 		      largebndx=largebndx, largebnd.unique=largebnd.unique, 
 		      maxbndx=maxbndx, maxbnd.unique=maxbnd.unique,
 		      nbrdom.min=nbrdom.min, 
 		      maxislarge=maxislarge, largeishelper=largeishelper, 
		      polyunion=polyunion, 
		      showsteps=showsteps, savesteps=savesteps, 
		      stepfolder=stepfolder, step_dsn=step_dsn, 
		      out_fmt=step_fmt, multiSAdoms=multiSAdoms, byeach=byeach,
		      maxbnd.threshold=maxbnd.threshold, largebnd.threshold=largebnd.threshold, 
		      maxbnd.addtext=maxbnd.addtext, largebnd.addtext=largebnd.addtext, 
		      overwrite=overwrite_layer, bayes=bayes)
    SAdomslst <- autoselectlst$SAdomslst
    helperbndxlst <- autoselectlst$helperbndxlst
    smallbndxlst <- autoselectlst$smallbndxlst
    largebndxlst <- autoselectlst$largebndxlst
    maxbndxlst <- autoselectlst$maxbndxlst

  } else {

    ## Add DOMAIN column to all rows
    helperbndx$DOMAIN <- helperbndx[[helperbnd.unique]]
    SAdomslst <- list(SAdoms=helperbndx)
    smallbndxlst <- list(smallbnd=smallbndx)
  }

  ###########################################################################
  ## Aggregate (dissolve) polygons on DOMAIN and calculate area on dissolved polygons
  SAcols <- c("DOMAIN", "AOI")
  SAcols <- SAcols[SAcols %in% names(SAdomslst[[1]])]
  SAdomslst <- lapply(SAdomslst, sf_dissolve, SAcols)

  if (showsteps) {
    ## Retain par parameters
    mar <-  graphics::par("mar")
    on.exit(graphics::par(mar=mar))
    
    ## Set plotting margins
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
    if (length(smallbndvars) > 1) {
      SAdomslst[[i]] <- merge(SAdomslst[[i]], 
		         sf::st_drop_geometry(smallbndxlst[[i]][, smallbndvars]), 
		         by.x="DOMAIN", by.y=smallbnd.domain, all.x=TRUE)
    }

#    SAdomslst[[i]] <- merge(SAdomslst[[i]], 
#		sf::st_drop_geometry(smallbndx[, c(smallbnd.unique, smallbnd.domain)]), 
#		by.x="DOMAIN", by.y=smallbnd.unique, all.x=TRUE)
    if (!is.null(largebndx)) {
      SAdomslst[[i]] <- suppressWarnings(sf::st_join(SAdomslst[[i]], 
					largebndx[, largebnd.unique], largest=TRUE))
    }
    if (!maxislarge && !is.null(maxbndx)) {
      SAdomslst[[i]] <- suppressWarnings(sf::st_join(SAdomslst[[i]], 
					maxbndx[, maxbnd.unique], largest=TRUE))
    } 
    if (addstate) {
      ## Check projections (reproject largebndx to projection of helperbndx
      prjdat <- crsCompare(SAdomslst[[i]], stunitco, nolonglat=TRUE)
      SAdomslst[[i]] <- prjdat$x
      stunitco <- prjdat$ycrs
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

      spExportSpatial(SAdomslst[[i]], 
          savedata_opts=list(outfolder=outfolder, 
                              outsp_fmt=outsp_fmt, 
                              out_dsn=out_dsn, 
                              out_layer=SAdoms_layer,
                              outfn.pre=outfn.pre, 
                              outfn.date=outfn.date, 
                              overwrite_layer=overwrite_layer,
                              append_layer=append_layer, 
                              add_layer=TRUE))
      
      spExportSpatial(smallbndxlst[[i]], 
          savedata_opts=list(outfolder=outfolder, 
                              out_fmt=out_fmt, 
                              out_dsn=out_dsn, 
                              out_layer=smallbnd_layer,
                              outfn.pre=outfn.pre, 
                              outfn.date=outfn.date, 
                              overwrite_layer=overwrite_layer,
                              append_layer=append_layer, 
                              add_layer=TRUE))
    }

    if (savesteps) {
      SAdoms_layer <- "SAdoms"
      if (length(SAdomslst) > 1) {
        SAdoms_layer <- paste0(SAdoms_layer, i)
      }

      jpgfn <- paste0(stepfolder, "/", SAdoms_layer, ".jpg")
      jpeg(jpgfn, res=400, units="in", width=8, height=10)
          plot(sf::st_geometry(SAdomslst[[i]]), border="dark grey")
          plot(sf::st_geometry(smallbndxlst[[i]]), add=TRUE, border="red", lwd=1)
      dev.off()
      message("Writing jpg to ", jpgfn, "\n")
    }
  }
  #if (showsteps) {
  #  par(mar=mar)
  #}


  #if (!multiSAdoms) {
  #  SAdoms <- SAdomdat$SAdomlst[[1]]
  #  smallbnd <- SAdomdat$smallbndlst[[1]]
  #} 

  message("Number of model domains generated: ", length(SAdomslst), "\n") 

  returnlst <- list(SAdomlst=SAdomslst, smallbndlst=smallbndxlst, 
		smallbnd.unique=smallbnd.unique, smallbnd.domain=smallbnd.domain, 
           largebnd.unique=largebnd.unique)

  if (addstate) {
    returnlst$stcdlst <- unique(unlist(lapply(SAdomslst, 
				function(x) return(unique(x$STATECD)))))
  }
  if (!is.null(largebnd.unique)) {
    returnlst$largebndlst <- unique(unlist(lapply(SAdomslst, function(x, largebnd.unique)
 		unique(x[[largebnd.unique]]), largebnd.unique)))
    returnlst$largebndsf <- 
		largebndx[largebndx[[largebnd.unique]] %in% returnlst$largebndlst, ]
  }

  rm(smallbndx)
  rm(helperbndx)
  rm(largebndx)
  rm(maxbndx)
  if (helper_autoselect) rm(autoselectlst)
  # gc()


  if (saveobj) {
    objfn <- getoutfn(outfn=objnm, ext="rda", outfolder=outfolder, 
                      overwrite=overwrite_layer, outfn.pre=outfn.pre, outfn.date=TRUE)
    saveRDS(returnlst, file=objfn)
    message("saving object to: ", objfn)
  }

  return(returnlst)
}
