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
#' @param stbnd.att String. Attribute in stunitco to output ('STATECD',
#' 'STATENM', 'COUNTYFIPS').
#' @param RS String. Name of FIA research station to restrict states to
#' ('RMRS','SRS','NCRS','NERS','PNWRS'). If NULL, all research stations are
#' included.
#' @param states String. States to subset boundary to.
#' @param overlap Number. Percent overlap to include.
#' @param showsteps Logical. If yes, display intersecting boundaries.
#' @param savebnd Logical. If yes, save boundary to outfolder.
#' @param savedata_opts List. See help(savedata_options()) for a list
#' of options. Only used when savebnd = TRUE. 
#' @return A list containing states and state names that the boundary crosses,
#' and boundary and attribute information for the intersecting boundary. 
#' @author Tracey S. Frescino
#' @keywords data
#' @examples
#' # Get polygon vector layer from FIESTA external data
#' WYbhdistfn <- system.file("extdata", 
#'                           "sp_data/WYbighorn_districtbnd.shp",
#'                           package = "FIESTA")
#' 
#' # Get intersecting statenames
#' spGetStates(WYbhdistfn)$statenames
#' 
#' # Get intersecting COUNTYFIP codes
#' spGetStates(WYbhdistfn,
#'             stbnd.att = "COUNTYFIPS")$states
#' @export spGetStates
spGetStates <- function(bnd_layer, 
                        bnd_dsn = NULL, 
                        bnd.filter = NULL, 
                        stbnd.att = "COUNTYFIPS", 
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
      if (names(savedata_opts)[[i]] %in% names(savedata_defaults_list)) {
        assign(names(savedata_opts)[[i]], savedata_opts[[i]])
      } else {
        stop(paste("Invalid parameter: ", names(savedata_opts)[[i]]))
      }
    }
  }
  
  ## Check showsteps
  #############################################################################
  showsteps <- pcheck.logical(showsteps, varnm="showsteps", 
                             title="Show steps?", first="NO", gui=gui) 
  
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
#  if (!geotype %in% gtypes || geotype == "GEOMETRYCOLLECTION" &&
#	length(sf::st_collection_extract(bndx, "POLYGON") == 0)) {
#    #stop("invalid geometry type")
#  }

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
  ## Define stbnd
  stbnd <- FIESTAutils::stunitco	

  ## Check stbnd.att
  stbndlst <- c("STATECD", "STATENM", "COUNTYFIPS")
  stbnd.att<- pcheck.varchar(var2check=stbnd.att, 
                      varnm="stbnd.att", gui=gui, 
                      checklst=stbndlst, 
                      caption="State name attribute", 
                      warn=paste(stbnd.att, "not in stunitco"))
  if (is.null(stbnd.att)) stbnd.att <- "COUNTYFIPS"

  ## Define stbnd
  stbnd <- FIESTAutils::stunitco	

  ## Reproject stbnd to bnd projection
  prjdat <- crsCompare(stbnd, ycrs=bndx, nolonglat=TRUE)
  stbnd <- prjdat$x
  bndx <- prjdat$ycrs

  ## Check intersecting states
  stbnd <- suppressWarnings(sf::st_crop(stbnd, sf::st_bbox(bndx)))
  stated <- sf_dissolve(stbnd, stbnd.att)

  stateint <- suppressWarnings(selectByIntersects(stated, sf::st_make_valid(bndx),
				overlapThreshold=overlap))

#  ## Check name of attribute identifying state
#  stname.att <- pcheck.varchar(var2check=stname.att, 
#                        varnm="stname.att", gui=gui, 
#                        checklst=names(stbnd), 
#                        caption="State name attribute", 
#                        warn=paste(stname.att, "not in stbnd"), 
#                        stopifinvalid=FALSE)

  statenamesint <- stateint[[stbnd.att]][!is.na(stateint[[stbnd.att]])]
  if (stbnd.att == "COUNTYFIPS") {
    statenamesint <- formatC(as.numeric(statenamesint), width=5, digits=5, flag="0")
    stcdchk <- sort(unique(as.numeric(sapply(statenamesint, 
			substr, nchar(statenamesint)-5, nchar(statenamesint)-3))))
  } else if (stbnd.att == "STATECD") {
    stcdchk <- statenamesint
  } else if (stbnd.att == "STATENM") {
    stcdchk <- FIESTAutils::ref_statecd[FIESTAutils::ref_statecd$MEANING %in%
			statenamesint, "VALUE"]
  }

  if (!is.null(states)) {
    states <- pcheck.states(states, statereturn="VALUE")
    if (is.null(states)) {
      stop("invalid states")
    }
    if (!all(states %in% stcdchk)) {
      statesmiss <- states[!states %in% stcdchk]
      statesmiss <- FIESTAutils::ref_statecd[FIESTAutils::ref_statecd$VALUE %in%
			statesmiss, "MEANING"]
      if (length(statesmiss) == length(states)) {
        stop("no states intersect boundary")
      } else {
        message("not all states intersect boundary: ", statesmiss)
        states <- states[states %in% stcdchk]
        stateint <- stateint[stateint$STATENM %in% states, ]
        clipbnd <- TRUE
      }
    } else if (length(states) < length(stcdchk)) {
      clipbnd <- TRUE
    }
  } 
  statenames <- FIESTAutils::ref_statecd[FIESTAutils::ref_statecd$VALUE %in%
			stcdchk, "MEANING"]
  if (showsteps) {
    mar <-  graphics::par("mar")
    on.exit(graphics::par(mar=mar))
    graphics::par(mar=c(1,1,1,1))

    plot(sf::st_geometry(stateint))
    plot(sf::st_geometry(bndx), add=TRUE, border="blue")
    #graphics::par(mar=mar)
  }

  ## IF RS is not null, check if states are outside RS
  if (!is.null(RS)) {
    RSstatelst <- FIESTAutils::ref_statecd[FIESTAutils::ref_statecd$RS == RS, "VALUE"]
    if (!all(stcds %in% RSstatelst)) {
      statesout <- stcds[which(!stcds %in% RSstatelst)]
      stcds <- stcds[which(stcds %in% RSstatelst)]

      statesoutnames <- FIESTAutils::ref_statecd[FIESTAutils::ref_statecd$VALUE %in%
			statesout, "MEANING"]
      message(paste0("states are outside ", RS, " region: ", toString(statesoutnames)))

      if (length(stcds) == 0) {
        stop("no states in RMRS")
      }
      statenames <- FIESTAutils::ref_statecd[FIESTAutils::ref_statecd$VALUE %in%
			stcds, "MEANING"]
      message("clipping boundary to ", RS, " states: ", toString(statenames))
      clipbbnd <- TRUE
    }
  }

  ## Clip bound to states
  if (clipbnd) {
    bndx <- spClipPoly(bndx, clippolyv=stbnd[stbnd[["STATENM"]] %in% statenames, ])
    #if (nrow(bndx) == 0) stop("invalid stname.att")
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
  
  return(list(states=statenamesint, bndx=bndx, stbnd.att=stbnd.att, statenames=statenames))
}


