spGetXY <- function(bnd, bnd_dsn=NULL, bnd.filter=NULL, states=NULL, RS=NULL, 
	xy=NULL, xy_dsn=NULL, xy.uniqueid="PLT_CN", xvar=NULL, yvar=NULL, xy.crs=4269, 
	xyjoinid=NULL, pjoinid="CN", xy_datsource=NULL, clipxy=TRUE, 
	evalid=NULL, evalCur=FALSE, evalEndyr=NULL, measCur=FALSE, measEndyr=NULL, 
	measEndyr.filter=NULL, invyrs=NULL, allyrs=FALSE, intensity1=FALSE, 
	showsteps=FALSE, savedata=FALSE, exportsp=FALSE, returnxy=TRUE, outfolder=NULL,
 	out_fmt="csv", out_dsn=NULL, out_layer="xyplt", outfn.pre=NULL, outfn.date=FALSE, 
	overwrite_dsn=FALSE, overwrite_layer=FALSE, append_layer=FALSE) {

  ##############################################################################
  ## DESCRIPTION
  ## Get FIA plots within the boundary population (area of interest)
  ## 1) Reproject state boundary to bnd projection (nolonglat=TRUE)
  ## 2) Intersect with state boundary 
  ## 5) Clip spatial coordinates and other tables to boundary
  ##
  ## ARGUMENTS
  ## xy - file name, R object, or layer in SQLitfn
  ##
  ## VALUE
  ## List of clipped data frames
  ##############################################################################

  ## Set global variables
  xydat=stateFilter=statecnty=stcds <- NULL
  returnlst <- {}

  ##################################################################
  ## CHECK INPUT PARAMETERS
  ##################################################################
  gui <- FALSE
  coordtype <- "public"

  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- names(formals(spGetXY))
  if (!all(input.params %in% formallst)) {
    miss <- input.params[!input.params %in% formallst]
    stop("invalid parameter: ", toString(miss))
  }


  #############################################################################
  ## Import boundary
  #############################################################################
  bndx <- pcheck.spatial(layer=bnd, dsn=bnd_dsn, caption="boundary")
 
  if (!is.null(bndx)) {
    ## bnd.filter
    bndx <- datFilter(bndx, xfilter=bnd.filter, stopifnull=TRUE)$xf
    clipxy <- TRUE
  } else {
    clipxy <- FALSE
  }

  #############################################################################
  ## Set xy_datsource
  ########################################################
  datsourcelst <- c("obj", "csv", "datamart", "sqlite")
  xy_datsource <- FIESTA::pcheck.varchar(var2check=xy_datsource, varnm="xy_datsource", 
		checklst=datsourcelst, gui=gui, caption="Data source?") 
  if (is.null(xy_datsource)) {
    if (!is.null(xy) && "sf" %in% class(xy)) {
      xy_datsource == "obj"
    } else if (!is.null(xy_dsn)) {
      dsn.ext <- getext(xy_dsn)
      if (!is.na(dsn.ext) && dsn.ext != "") {
        xy_datsource <- ifelse(dsn.ext == "gdb", "gdb", 
		ifelse(dsn.ext %in% c("db", "db3", "sqlite", "sqlite3"), "sqlite", 
             ifelse(dsn.ext == "csv", "csv",
			ifelse(dsn.ext == "shp", "shp", "datamart")))) 
      }
    } else if (!is.null(xy)) {
      xy.ext <- getext(xy)
      if (!is.na(xy.ext) && xy.ext != "") {
        xy_datsource <- ifelse(xy.ext == "shp", "shp", 
             ifelse(xy.ext == "csv", "csv", "datamart")) 
      }
    } else {
      stop("must include xy_datsource")
    }
  } 
  if (xy_datsource == "sqlite") {
    if (!all(c("RSQLite", "DBI") %in% rownames(installed.packages()))) {
	 message("RSQLite and DBI packages are required to run SQLite queries")
    }
  } 
 
  ## Check measEndyr.filter
  #############################################################################
  measEndyr.filter <- check.logic(bnd, measEndyr.filter)

  
  ## Check savedata
  #############################################################################
  savedata <- FIESTA::pcheck.logical(savedata, varnm="savedata", 
		title="Save data?", first="NO", gui=gui) 
 
  ## Check overwrite, outfn.date, outfolder, outfn 
  ########################################################
  if (savedata) {
    outlst <- pcheck.output(out_dsn=out_dsn, out_fmt=out_fmt, 
		outfolder=outfolder, outfn.pre=outfn.pre, outfn.date=outfn.date, 
		overwrite_dsn=overwrite_dsn, append_layer=append_layer, 
		createSQLite=FALSE, gui=gui)
    out_dsn <- outlst$out_dsn
    outfolder <- outlst$outfolder
    out_fmt <- outlst$out_fmt
    overwrite_layer <- outlst$overwrite_layer
    append_layer <- outlst$append_layer
    if (out_fmt != "csv") {
      outfn.date <- FALSE
    }
  }
 
  ########################################################################
  ### DO THE WORK
  ########################################################################
  if (!is.null(evalid)) {
    evalid <- unlist(evalid)
    stcds <- unique(as.numeric(substr(evalid, nchar(evalid)-6, nchar(evalid)-4))) 
    savePOP <- TRUE
  } else if (!is.null(states)) {
    if (!all(states %in% FIESTA::ref_statecd$MEANING))
      stop("states is invalid")
    statenames <- states
    stcds <- FIESTA::ref_statecd$VALUE[FIESTA::ref_statecd$MEANING %in% states]
  } else if (!is.null(bndx)) {
 
    ## Get intersecting states
    statedat <- spGetStates(bndx, stbnd=NULL, stbnd_dsn=NULL, 
			stbnd.att="COUNTYFIPS", RS=RS, states=states, showsteps=showsteps)
    bndx <- statedat$bndx
    stbnd.att <- statedat$stbnd.att
    statenames <- statedat$statenames
    if (!is.null(stbnd.att) && stbnd.att == "COUNTYFIPS") {
      statecnty <- statedat$states
      stcds <- unique(as.numeric(substr(statecnty, 1,2)))
    } else {
      stcds <- FIESTA::ref_statecd$VALUE[FIESTA::ref_statecd$MEANING %in% statedat$states]
    }
    message("boundary intersected states: ", toString(statenames))
  } else {
    stop("must include bndx or states")
  }

  #############################################################################
  ## If xy is separate file or database, and clipxy=TRUE, import first
  #############################################################################
  if (!is.null(xy) && "sf" %in% class(xy)) {
    xyplt <- xy

  } else if (xy_datsource == "gdb") {
    spxy <- pcheck.table(xy, xy_dsn)
    
  } else if (xy_datsource %in% c("obj", "csv")) {

    ####################################################################
    ## 1) Import file(s)
    ## 2) Clip xy (for all states) to boundary
    ####################################################################

    ## Check xy table
    xyplt <- pcheck.table(xy)

    ## Make spatial
    spxy <- spMakeSpatialPoints(xyplt=xyplt, xy.uniqueid=xy.uniqueid, 
		xvar=xvar, yvar=yvar, xy.crs=xy.crs)

  } else {			## xy_datsource in('datamart', 'sqlite')

    if (xy_datsource == "datamart") {
      spxy <- DBgetCoords(states=states, evalid=evalid, evalCur=evalCur,
		evalEndyr=evalEndyr, measCur=measCur, measEndyr=measEndyr,
		allyrs=allyrs, invyrs=invyrs, intensity1=intensity1, issp=TRUE)[[1]]

      xy.uniqueid <- "PLT_CN"
  
    } else if (xy_datsource == "sqlite") {

      ####################################################################
      ## 1) Check if data for all states is in database
      ## 1) Get most current plots from xy database that intersect state
      ## 2) Clip xy (for state) to boundary
      ## 3) Subset other data with clipped xy joinid
      ####################################################################
      xyindb <- FALSE
      plot_layer <- NULL
  
      ## Check for data tables in database
      ###########################################################
      dbconn <- suppressWarnings(DBtestSQLite(xy_dsn, dbconnopen=TRUE, showlist=FALSE))
      tablst <- DBI::dbListTables(dbconn)

      if (is.null(xy)) {
        xytabs <- tablst[grepl("xy", tablst)]
        if (length(xytabs) == 0) {
          stop("no xy in ", xy_dsn)
        }
        if (length(findnm("ACTUAL", xytabs, returnNULL=TRUE)) == 1) {
          xy <- xytabs[grepl("ACTUAL", xytabs)]
          message("xy is NULL...  using ", xy)
        } else if (length(findnm("PUBLIC", xytabs, returnNULL=TRUE)) == 1) {
          xy <- xytabs[grepl("PUBLIC", xytabs)]
          message("xy is NULL...  using ", xy)
        } else {
          plot_layer <- findnm("plot", xytabs, returnNULL=TRUE)
          if (!is.null(plot_layer) && length(plot_layer) == 1) {
            message("xy is NULL...  using ", plot_layer, " table")
            xy <- plot_layer
            pfields <- DBI::dbListFields(dbconn, xy)
            if ("LON_PUBLIC" %in% pfields) {
              xvar <- "LON_PUBLIC"
              if ("LAT_PUBLIC" %in% pfields) {
                yvar <- "LAT_PUBLIC"
              }
            }
          } else {
            stop(xy, " not in ", xy_dsn) 
          }
        } 
      } else {
        if (!xy %in% tablst) {
          stop(xy, " not in database")
        }
      }
      xyfields <- DBI::dbListFields(dbconn, xy)
      xystatenm <- findnm("STATECD", xyfields, returnNULL=TRUE)

      if (is.null(xvar)) {
        if (grepl("ACTUAL", xy)) {
          xvar <- "LON_ACTUAL"
          yvar <- "LAT_ACTUAL"
        } else if (grepl("PUBLIC", xy)) {
          xvar <- "LON_PUBLIC"
          yvar <- "LAT_PUBLIC"
        }
      }

      ## Check xvar
      if (!is.null(xvar) && !xvar %in% xyfields) {
        stop(xvar, " is not in table")
      }
      ## Check yvar
      if (!is.null(yvar) && !yvar %in% xyfields) {
        stop(yvar, " is not in table")
      }
 
      ## check xy.uniqueid (unique identifier of xy)
      xy.uniqueid <- pcheck.varchar(var2check=xy.uniqueid, varnm="xy.uniqueid", 
		gui=gui, checklst=xyfields, caption="xy uniqueid", stopifnull = FALSE)

      ## check xyjoinid (variable to join to plot table)
      xyjoinid <- pcheck.varchar(var2check=xyjoinid, varnm="xyjoinid", 
		gui=gui, checklst=xyfields, caption="xyjoinid", stopifnull = FALSE)
      if (is.null(xyjoinid)) {
        xyjoinid <- xy.uniqueid
      }

      if (!is.null(xystatenm)) {
        sql <- paste0("select * from ", xy, " where ", xystatenm, " IN(", 
			toString(stcds), ")")
        xyplt <- pcheck.table(xy, tab_dsn=xy_dsn, tabqry=sql)
 
        ## Make spatial
        spxy <- spMakeSpatialPoints(xyplt=xyplt, xy.uniqueid=xy.uniqueid, 
			xvar=xvar, yvar=yvar, xy.crs=xy.crs)            
      } else {
        plot_layer <- findnm("plot", tablst, returnNULL=TRUE)
        if (!is.null(plot_layer) && length(plot_layer) == 1) {
          pltfields <- DBI::dbListFields(dbconn, plot_layer)
          pjoinid <- pcheck.varchar(var2check=pjoinid, varnm="pjoinid", 
			gui=gui, checklst=pltfields, caption="plot joinid", stopifnull=TRUE)
          if (is.null(pjoinid)) {
            pjoinid <- xyjoinid
          }
          pstatenm <- findnm("STATECD", pltfields, returnNULL=TRUE)

          if (!is.null(pstatenm)) {
            sql <- paste0("select xy.* from ", xy, " xy join ", 
			plot_layer, " p ON(xy.", xyjoinid, " = p.", pjoinid, ") where p.", 
			pstatenm, " IN(", toString(stcds), ")")
            xyplt <- pcheck.table(xy, tab_dsn=xy_dsn, tabqry=sql)

            ## Make spatial
            spxy <- spMakeSpatialPoints(xyplt=xyplt, xy.uniqueid=xy.uniqueid, 
			xvar=xvar, yvar=yvar, xy.crs=xy.crs)            
          } else {
            stop("STATECD not in tables")
          }
        } else {
          message("no plot layer in database")
          spxy <- pcheck.spatial(xy, dsn=xy_dsn)
        }
      } 
    }
  }

  if (clipxy) {
    clipdat <- spClipPoint(spxy, clippolyv=bndx)
    spxy <- clipdat$clip_xyplt 
    if (length(spxy) == 0) stop("xy does not overlap bndx")

    if (showsteps) {
      plot(st_geometry(bndx), border="black")
      plot(st_geometry(spxy), add=TRUE, col="blue")
    }
  } 
 
  ## Add a STATECD variable to spxy if not already there
  stunitco.names <- c("STATECD", "UNITCD", "COUNTYCD", "COUNTYFIPS")
  statevars <- stunitco.names[!stunitco.names %in% names(spxy)]
  if (length(statevars) > 0) {
    spxy <- spExtractPoly(spxy, polyvlst=FIESTA::stunitco, 
			polyvarlst=statevars)$spxyext
  }

  ## Subset columns of spxy
  spxy <- spxy[, unique(c(xy.uniqueid, xyjoinid, stunitco.names))]

  #############################################################################
  ## measEndyr.filter
  #############################################################################
  if (!is.null(measEndyr.filter)) {
    filternames <- check.logic(bnd, measEndyr.filter, returnvar=TRUE)
    if (length(filternames) > 0) {
      spxy <- spExtractPoly(spxy, polyvlst=bndx, polyvarlst=filternames)$spxyext
    } else {
      spxy <- spExtractPoly(spxy, polyvlst=bndx)$spxyext
    }
  }

  #############################################################################
  ## Save tables
  #############################################################################
  if (savedata) {
    datExportData(sf::st_drop_geometry(spxy), outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="xyids", 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer, 
		add_layer=TRUE, append_layer=append_layer)
  } 
  if (exportsp) {
    spExportSpatial(spxy, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer=out_layer, 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer, 
		add_layer=TRUE, append_layer=append_layer)
  }    

  if (showsteps) {
    ## Set plotting margins
    mar <-  par("mar")
    par(mar=c(1,1,1,1))

    plot(sf::st_geometry(spxy), col="blue", cex=.5)
    if (!is.null(bndx)) {
      plot(st_geometry(bndx), add=TRUE, border="black", lwd=0.75)
    }
    par(mar=mar)
  }
 
  if (returnxy) {
    returnlst$spxy <- spxy
  }
  returnlst$xyids <- sf::st_drop_geometry(spxy)
  returnlst$bndx <- bndx
  returnlst$xy.uniqueid <- xy.uniqueid
  returnlst$xyjoinid <- xyjoinid
  returnlst$pjoinid <- pjoinid
  returnlst$states <- statenames
  returnlst$statecnty <- statecnty
  returnlst$stbnd.att <- stbnd.att
  return(returnlst)
}
