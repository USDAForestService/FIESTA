spGetStates <- function(bnd_layer, bnd_dsn=NULL, bnd.filter=NULL,
	stbnd=NULL, stbnd_dsn=NULL, stbnd.att=NULL, stname.att="STATENM",
	RS=NULL, states=NULL, overlap=0, showsteps=FALSE, savebnd=FALSE,
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
  bndx <- FIESTA::datFilter(bndx, xfilter=bnd.filter, stopifnull=TRUE)$xf


  ########################################################################
  ### DO THE WORK
  ########################################################################
  RSlst <- c("ALL", unique(ref_statecd$RS))
  RS <- pcheck.varchar(var2check=RS, varnm="RS",
		checklst=RSlst, gui=gui, caption="Research Station extent?")
  if (!is.null(RS) && RS == "ALL") {
    RS <- NULL
  }

  ## Get intersecting states
  #############################################################################
  if (!is.null(stbnd) || !is.null(stbnd_dsn)) {
    stbnd <- pcheck.spatial(layer=stbnd, dsn=stbnd_dsn)
    stbnd.att <- pcheck.varchar(var2check=stbnd.att, varnm="stbnd.att",
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

    stbnd.att<- pcheck.varchar(var2check=stbnd.att, varnm="stbnd.att",
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

    ## Check name of attribute identifying state
    stname.att <- pcheck.varchar(var2check=stname.att, varnm="stname.att",
		gui=gui, checklst=names(stbnd), caption="State name attribute",
		warn=paste(stname.att, "not in stbnd"), stopifinvalid=FALSE)
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
  }
  if (!all(states %in% FIESTA::ref_statecd$MEANING)) {
    if (stbnd.att == "COUNTYFIPS") {
      statenames <- FIESTA::ref_statecd[FIESTA::ref_statecd$VALUE %in%
			unique(as.numeric(substr(states, 1,2))), "MEANING"]
    }
  }
  ## Check statenames
  if (is.null(RS) && !clipbnd) {
    statenameslst <- FIESTA::ref_statecd$MEANING
    statenames <- pcheck.varchar(var2check=statenames, varnm="states", gui=gui,
		checklst=statenameslst, caption="States", stopifnull=TRUE, multiple=TRUE)

  } else {
    if (!is.null(RS)) {
      statenameslst <- FIESTA::ref_statecd[FIESTA::ref_statecd$RS == RS, "MEANING"]
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
      bndx <- FIESTA::spClipPoly(bndx, clippolyv=stbnd[stbnd[[stname.att]] %in% statenames, ])
      if (nrow(bndx) == 0) stop("invalid stname.att")
    }
    if (stbnd.att == "COUNTYFIPS") {
      stcds <- FIESTA::ref_statecd[FIESTA::ref_statecd$MEANING %in% statenames, "VALUE"]
      states <- states[as.numeric(substr(states, 1, 2)) %in% stcds]
    }
  }

  ## Save boundary
  if (savebnd) {
    FIESTA::spExportSpatial(bndx, outfolder=outfolder, out_layer="bnd", ...)
  }
  return(list(states=states, bndx=bndx, stbnd.att=stbnd.att, statenames=statenames))

}


