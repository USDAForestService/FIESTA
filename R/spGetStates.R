#' Spatial wrapper - Extracts states that intersect a boundary.
#' 
#' Wrapper to get state names that intersect a given boundary.
#' 
#' @param bnd_layer sf R object, Area of Interest (AOI) boundary. Can be a spatial 
#' sf object, full pathname to a shapefile, or name of a layer within a database.
#' @param bnd_dsn String. Data source name (dsn; e.g., SQLite database or shapefile
#' pathname) of bnd. The dsn varies by driver. See gdal OGR vector formats
#' (https://www.gdal.org/ogr_formats.html). 
#' @param bnd.filter String. Filter to subset bnd spatial layer.
#' @param stbnd sf R object, State boundary to use for intersection. If NULL, 
#' FIESTAutils::stunitco is used. Can be a spatial 
#' sf object, full pathname to a shapefile, or name of a layer within a database.
#' @param stbnd_dsn String. Data source name (dsn; e.g., SQLite database or shapefile
#' pathname) of stbnd. The dsn varies by driver. See gdal OGR vector formats
#' (https://www.gdal.org/ogr_formats.html).
#' @param stbnd.att String. Attribute in stbnd to output.
#' @param stname.att String. Attribute in stbnd to identify state.
#' @param RS String. Name of FIA research station to restrict states to
#' ('RMRS','SRS','NCRS','NERS','PNWRS'). If NULL, all research stations are
#' included.
#' @param states String. States to subset boundary to.
#' @param overlap Number. Percent overlap to include.
#' @param showsteps Logical. If yes, display intersecting boundaries.
#' @param savebnd Logical. If yes, save boundary to outfolder.
#' @param savedata_opts List. See help(savedata_options()) for a list
#' of options. Only used when savebnd = TRUE. 
#'
#' @author Tracey S. Frescino
#' @keywords data
#' @export spGetStates
spGetStates <- function(bnd_layer, 
                        bnd_dsn = NULL, 
                        bnd.filter = NULL, 
                        stbnd = NULL, 
                        stbnd_dsn = NULL, 
                        stbnd.att = "COUNTYFIPS", 
                        stname.att = "STATENM", 
                        RS = NULL, 
                        states = NULL, 
                        overlap = 1, 
                        showsteps = FALSE, 
                        savebnd = FALSE, 
                        savedata_opts = NULL) {

  ##############################################################################
  ## DESCRIPTION
  ## Get states that intersect a boundary. If RS != NULL, boundary is
  ## clipped to RS states.
  ##############################################################################

  ##################################################################
  ## CHECK PARAMETER NAMES
  ##################################################################
  
  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- names(formals(spGetStates))
  if (!all(input.params %in% formallst)) {
    miss <- input.params[!input.params %in% formallst]
    stop("invalid parameter: ", toString(miss))
  }
  
  ## Set par 
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))

  
  ## Check parameter lists
  pcheck.params(input.params, savedata_opts=savedata_opts)
  
  ## Set savedata defaults
  savedata_defaults_list <- formals(savedata_options)[-length(formals(savedata_options))]
  
  for (i in 1:length(savedata_defaults_list)) {
    assign(names(savedata_defaults_list)[[i]], savedata_defaults_list[[i]])
  }
  
  ## Set user-supplied savedata values
  if (length(savedata_opts) > 0) {
    for (i in 1:length(savedata_opts)) {
      assign(names(savedata_opts)[[i]], savedata_opts[[i]])
    }
  }
  
  
  ## Check savedata
  #############################################################################
  savebnd <- pcheck.logical(savebnd, varnm="savebnd", 
                             title="Save boundary?", first="NO", gui=gui) 
  
  ## Check overwrite, outfn.date, outfolder, outfn 
  ########################################################
  if (savebnd) {
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
  }
  
  
  ##################################################################
  ## CHECK INPUT PARAMETERS
  ##################################################################
  gui <- FALSE
  clipbnd <- FALSE

  #############################################################################
  ## Import boundary
  #############################################################################
  bndx <- pcheck.spatial(layer=bnd_layer, dsn=bnd_dsn, caption="boundary",
		stopifnull=TRUE)

  gtypes <- c("POLYGON", "GEOMETRYCOLLECTION", "MULTIPOLYGON")
  geotype <- unique(sf::st_geometry_type(bndx))

  ## Check geometry type
  if (!geotype %in% gtypes || geotype == "GEOMETRYCOLLECTION" &&
	length(sf::st_collection_extract(bndx, "POLYGON") == 0)) {
    stop("invalid geometry type")
  }

  ## bnd.filter
  bndx <- datFilter(bndx, xfilter=bnd.filter, stopifnull=TRUE)$xf


  ########################################################################
  ### DO THE WORK
  ########################################################################
  RSlst <- c("ALL", unique(FIESTAutils::ref_statecd$RS))
  RS <- pcheck.varchar(var2check=RS, varnm="RS", 
                    checklst=RSlst, gui=gui, 
                    caption="Research Station extent?")
  if (!is.null(RS) && RS == "ALL") {
    RS <- NULL
  }

  ## Get intersecting states
  #############################################################################
  if (!is.null(stbnd) || !is.null(stbnd_dsn)) {
    stbnd <- pcheck.spatial(layer=stbnd, dsn=stbnd_dsn)
    stbnd.att <- pcheck.varchar(var2check=stbnd.att, 
                      varnm="stbnd.att", gui=gui, 
                      checklst=names(stbnd), 
                      caption="State name attribute", 
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
    stbnd <- FIESTAutils::stunitco			## longlat-NAD83 projection

    stbnd.att<- pcheck.varchar(var2check=stbnd.att, 
                      varnm="stbnd.att", gui=gui, 
                      checklst=names(stbnd), 
                      caption="State name attribute", 
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

    ## Check name of attribute identifying state
    stname.att <- pcheck.varchar(var2check=stname.att, 
                        varnm="stname.att", gui=gui, 
                        checklst=names(stbnd), 
                        caption="State name attribute", 
                        warn=paste(stname.att, "not in stbnd"), 
                        stopifinvalid=FALSE)
    statenamesint <- stateint[[stbnd.att]][!is.na(stateint[[stbnd.att]])]

    if (!is.null(states)) {
      states <- pcheck.states(states)
      if (!all(states %in% statenamesint)) {
        statesmiss <- states[!states %in% statenamesint]
        if (length(statesmiss) == length(states)) {
          stop("no states intersect boundary")
        } else {
          message("not all states intersect boundary: ", statesmiss)
          states <- states[states %in% statenamesint]
          stateint <- stateint[stateint$STATENM %in% states, ]
          clipbnd <- TRUE
        }
      } else if (length(states) < length(statenamesint)) {
        clipbnd <- TRUE
      }
      statenames <- states
    } else {
      states <- statenamesint
      statenames <- states
    }
    if (showsteps) {
      mar <-  graphics::par("mar")
      graphics::par(mar=c(1,1,1,1))

      plot(sf::st_geometry(stateint))
      plot(sf::st_geometry(bndx), add=TRUE, border="blue")
      graphics::par(mar=mar)
    }
  } else {
    states <- pcheck.states(states)
    statenames <- states
  }
  if (!all(states %in% FIESTAutils::ref_statecd$MEANING)) {
    if (stbnd.att == "COUNTYFIPS") {
      states <- formatC(as.numeric(states), width=5, digits=5, flag="0")
      statenames <- FIESTAutils::ref_statecd[FIESTAutils::ref_statecd$VALUE %in%
			sort(unique(as.numeric(sapply(states, 
			substr, nchar(states)-5, nchar(states)-3)))), "MEANING"]
    }
  }
  ## Check statenames
  if (is.null(RS) && !clipbnd) {
    statenameslst <- FIESTAutils::ref_statecd$MEANING
    statenames <- pcheck.varchar(var2check=statenames, 
                        varnm="states", gui=gui, 
                        checklst=statenameslst, 
                        caption="States", 
                        stopifnull=TRUE, multiple=TRUE)

  } else {
    if (!is.null(RS)) {
      statenameslst <- FIESTAutils::ref_statecd[FIESTAutils::ref_statecd$RS == RS, "MEANING"]
      if (!all(statenames %in% statenameslst)) {
        statesout <- statenames[which(!statenames %in% statenameslst)]
        statenames <- statenames[which(statenames %in% statenameslst)]
        message(paste0("states are outside ", RS, " region: ", toString(statesout)))

        if (length(statenames) == 0) {
          stop("no states in RMRS")
        }
        message("clipping boundary to ", RS, " states: ", toString(statenames))
      }
    }
    if (!is.null(stname.att)) {
      bndx <- spClipPoly(bndx, 
                    clippolyv=stbnd[stbnd[[stname.att]] %in% statenames, ])
      if (nrow(bndx) == 0) stop("invalid stname.att")
    }
    if (stbnd.att == "COUNTYFIPS") {
      stcds <- FIESTAutils::ref_statecd[FIESTAutils::ref_statecd$MEANING %in% statenames, "VALUE"]
      states <- states[as.numeric(substr(states, 1, 2)) %in% stcds]
    }
  }
  message("boundary intersected the following states: ", toString(statenames))

  ## Save boundary
  if (savebnd) {
    spExportSpatial(bndx, 
          savedata_opts=list(outfolder=outfolder, 
                              out_fmt=out_fmt, 
                              out_dsn=out_dsn, 
                              out_layer="bnd",
                              outfn.pre=outfn.pre, 
                              outfn.date=outfn.date, 
                              overwrite_layer=overwrite_layer,
                              append_layer=append_layer, 
                              add_layer=TRUE))
  }
  
  return(list(states=states, bndx=bndx, stbnd.att=stbnd.att, statenames=statenames))
}


