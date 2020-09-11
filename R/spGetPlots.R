spGetPlots <- function(bnd, bnd_dsn=NULL, bnd.filter=NULL, states=NULL, 
	stbnd=NULL, stbnd_dsn=NULL, stbnd.att="COUNTYFIPS", RS=NULL, xy=NULL, 
	xy_dsn=NULL, xy.uniqueid="PLT_CN", xvar="LON_PUBLIC", yvar="LAT_PUBLIC", 
	xy.crs=4269, xy.joinid="PLT_CN", clipxy=TRUE, datsource="datamart", 
	data_dsn=NULL, istree=TRUE, plot_layer="plot", cond_layer="cond", tree_layer="tree", 
	other_layers=NULL, puniqueid="CN", evalid=NULL, evalCur=FALSE, evalEndyr=NULL, 
	evalType="AREAVOL", measCur=FALSE, measEndyr=NULL, Endyr.filter=NULL, 
	invyrs=NULL, allyrs=FALSE, intensity1=FALSE, showsteps=FALSE, savedata=FALSE, 
	savebnd=FALSE, savexy=TRUE, outfolder=NULL, out_fmt="shp", out_dsn=NULL, 
	outfn.pre=NULL, outfn.date=FALSE, overwrite_dsn=FALSE, 
	overwrite_layer=FALSE, ...) {

  ##############################################################################
  ## DESCRIPTION
  ## Get FIA plots within the boundary population (area of interest)
  ## 1) Reproject state boundary to bnd projection (nolonglat=TRUE)
  ## 2) Intersect with state boundary 
  ## 3) Get FIA plots for intesected states (including tree, shp, INTENSITY=1)
  ## 4) Merge coordinate data if included separately (e.g., coordinates from SDS)
  ## 5) Clip spatial coordinates and other tables to boundary
  ##
  ## ARGUMENTS
  ## xy - file name, R object, or layer in SQLitfn
  ##
  ## VALUE
  ## List of clipped data frames
  ##############################################################################

  ## Set global variables
  xydat=stateFilter=statecnty=xypltx=tabs2save=ZSTUNCOPLOT=INVYR <- NULL
  cuniqueid=tuniqueid <- "PLT_CN"
  returnlst <- list()
  #clipdat <- list()

  ##################################################################
  ## CHECK INPUT PARAMETERS
  ##################################################################
  gui <- FALSE
  coordtype <- "public"

  #############################################################################
  ## Import boundary
  #############################################################################
  bndx <- pcheck.spatial(layer=bnd, dsn=bnd_dsn, caption="boundary")
 
  ## bnd.filter
  bndx <- datFilter(bndx, xfilter=bnd.filter, stopifnull=TRUE)$xf


  #############################################################################
  ## Set datsource
  ########################################################
  datsourcelst <- c("obj", "csv", "datamart", "sqlite")
  datsource <- FIESTA::pcheck.varchar(var2check=datsource, varnm="datsource", 
		checklst=datsourcelst, gui=gui, caption="Data source?") 
  if (datsource == "sqlite") {
    if (!all(c("RSQLite", "DBI") %in% rownames(installed.packages())))
	 stop("RSQLite and DBI packages are required to run SQLite queries")
  } 
  if (datsource %in% c("sqlite", "gdb")) {
    if (is.null(data_dsn)) stop("data_dsn is NULL")
    if (!file.exists(data_dsn)) stop(data_dsn, " is invalid")
  }
#  if (datsource %in% c("sqlite"))
#    data_dsn <- DBtestSQLite(data_dsn, dbconnopen=FALSE)
  
   
  ## Check savedata
  #############################################################################
  savedata <- FIESTA::pcheck.logical(savedata, varnm="savedata", 
		title="Save data?", first="NO", gui=gui) 

  ## Check savebnd
  #############################################################################
  savebnd <- FIESTA::pcheck.logical(savebnd, varnm="savebnd", 
		title="Save spatial bnd?", first="NO", gui=gui)  
 
 
  ## Check overwrite, outfn.date, outfolder, outfn 
  ########################################################
  if (savedata) {
    outfolder <- FIESTA::pcheck.outfolder(outfolder, gui)
    overwrite_dsn <- FIESTA::pcheck.logical(overwrite_dsn, varnm="overwrite_dsn", 
		title="Overwrite dsn?", first="NO", gui=gui)  
    overwrite_layer <- FIESTA::pcheck.logical(overwrite_layer, varnm="overwrite_layer", 
		title="Overwrite layers?", first="NO", gui=gui)  
    outfn.date <- FIESTA::pcheck.logical(outfn.date , varnm="outfn.date", 
		title="Add date to outfiles?", first="NO", gui=gui) 

    ## If outfn.pre is not null, create a folder within the outfolder, named outfn.pre
    if (!is.null(outfn.pre)) {
      outfolder <- file.path(outfolder, outfn.pre)
      if (!dir.exists(outfolder)) dir.create(outfolder)
    }

    out_fmtlst <- c("sqlite", "gpkg", "csv", "gdb")
    out_fmt <- FIESTA::pcheck.varchar(var2check=out_fmt, varnm="out_fmt", 
		checklst=out_fmtlst, gui=gui, caption="Output format?") 

    out_fmtlst <- c("sqlite", "gpkg", "csv", "gdb")
    out_fmt <- FIESTA::pcheck.varchar(var2check=out_fmt, varnm="out_fmt", 
		checklst=out_fmtlst, gui=gui, caption="Output format?") 
    if (out_fmt != "shp" && is.null(out_dsn))
      out_dsn <- paste0("GBdata.", out_fmt)

    if (out_fmt == "gdb") {
      gdbfn <- DBtestESRIgdb(gdbfn=out_dsn, outfolder=outfolder, 
			overwrite=overwrite_dsn, showlist=FALSE, returnpath=FALSE)
    }	else if (out_fmt %in% c("sqlite", "gpkg")) {
      gpkg <- ifelse(out_fmt == "gpkg", TRUE, FALSE)

      SQLitefn <- DBcreateSQLite(SQLitefn=out_dsn, gpkg=gpkg, outfolder=outfolder, 
			overwrite=overwrite_dsn, returnpath=FALSE)
    }	

#    if (savesteps) {
#      stepfolder <- file.path(outfolder, "steps")
#      if (!dir.exists(stepfolder)) dir.create(stepfolder)
#      if (out_fmt == "shp") {
#        step_dsn <- NULL
#      } else {
#        step_dsn <- paste0("steps.", out_fmt)
#      }
#    }
  }

  ########################################################################
  ### DO THE WORK
  ########################################################################
  if (!is.null(states)) {
    if (!all(states %in% FIESTA::ref_statecd$MEANING))
      stop("states is invalid")
  } else {
    ## Get stbnd.att
    if (is.null(stbnd.att) && exists("stunitco"))
      stbnd.att <- "COUNTYFIPS"

    ## Get intersecting states
    statedat <- spGetStates(bndx, stbnd=stbnd, stbnd_dsn=stbnd_dsn, 
			stbnd.att=stbnd.att, RS=RS, states=states, savebnd=savebnd, 
			outfolder=outfolder, ...)
    bndx <- statedat$bndx
    stbnd.att <- statedat$stbnd.att
    statenames <- statedat$statenames
    if (!is.null(stbnd.att) && stbnd.att == "COUNTYFIPS") {
      statecnty <- statedat$states
      stcds <- unique(as.numeric(substr(statecnty, 1,2)))
    } else {
      stcds <- FIESTA::ref_statecd$VALUE[FIESTA::ref_statecd$MEANING %in% statedat$states]
    }
  }
  message("boundary intersected states: ", toString(statenames))

  #############################################################################
  ## If xy is separate file or database, and clipxy=TRUE, import first
  #############################################################################
  ## Check xy table
  xyinfo <- pcheck.spatial(xy, xy_dsn, checkonly=TRUE)

  if (!is.null(xyinfo)) {
    if (!is.null(xyinfo) && getext(xy_dsn) %in% c("csv", "shp")) {
      xydat <- pcheck.spatial(xyinfo$xy, xyinfo$xy_dsn)
      xyfields <- names(xydat)
      xy.uniqueid <- pcheck.varchar(var2check=xy.uniqueid, varnm="xy.uniqueid", 
		gui=gui, checklst=xyfields, caption="xy uniqueid", stopifnull=TRUE)
      xy.joinid <- pcheck.varchar(var2check=xy.joinid, varnm="xy.joinid", 
		gui=gui, checklst=xyfields, caption="xy joinid")
      if (is.null(xy.joinid)) xy.joinid <- xy.uniqueid
      xvar <- pcheck.varchar(var2check=xvar, varnm="xvar", gui=gui, 
		checklst=xyfields, caption="x variable", stopifnull=TRUE)
      yvar <- pcheck.varchar(var2check=yvar, varnm="yvar", gui=gui, 
		checklst=xyfields, caption="y variable", stopifnull=TRUE)
    }
  }

  if (datsource %in% c("obj", "csv")) {

    ####################################################################
    ## 1) Import file(s)
    ## 2) Clip xy (for all states) to boundary
    ## 3) Subset other data with clipped xy joinid
    ####################################################################

    ## plot data
    obj <- ifelse (datsource == "obj", TRUE, FALSE)
    pltx <- pcheck.table(plot_layer, obj=obj, stopifnull=TRUE)

    ## condition data
    condx <- pcheck.table(cond_layer, obj=obj, stopifnull=TRUE)
    tabs2save <- c("pltx", "condx")

    ## tree data
    if (istree) {
      treex <- pcheck.table(tree_layer, obj=obj, stopifnull=TRUE)
      if (!is.null(treex)) 
        tabs2save <- c(tabs2save, "treex")
    }

    ## other data
    if (!is.null(other_layers)) {
      for (layer in other_layers) {
        assign(paste0(layer, "x"), pcheck.table(layer, obj=obj, stopifnull=TRUE))
        tabs2save <- c(tabs2save, layer)
      }
    }

    if (clipxy) {
      if (is.null(xydat))
        message("no xy data... using all plots in dataset")

      clipdat <- spClipPoint(xyplt=xy, xy.uniqueid=xy.joinid, 
			xvar=xvar, yvar=yvar, xy.crs=xy.crs, addxy=TRUE, clippolyv=bndx)
      xypltx <- clipdat$clip_xyplt 
      #bndx <- rbind(bndx, clipdat$clip_polyv)
      if (length(xyplt) == 0) stop("xy does not overlap bndx")

      if (showsteps) {
        plot(st_geometry(clipdat$clip_polyv))
        plot(st_geometry(xyplt), add=TRUE, col="blue")
      }

      ## Define pjoinid
      pltfields <- names(plt)
      if (xy.joinid %in% pltfields) {
        pjoinid  <- xy.joinid
      } else {
        if (xy.joinid == "PLT_CN" && "CN" %in% pltfields) {
          pjoinid <- "CN"
        } else {
          stop(xy.joinid, " not in plt")
        }
      }

      message("Clipping plt ..")
      pltx <- pltx[pltx[[pjoinid]] %in% xypltx[[xy.joinid]],]
      if (nrow(pltx) == 0) stop("xy.joinid invalid")
      tabs2save <- c(tabs2save, "pltx")

      clipids <- pltx[[puniqueid]]

      ## Subset cond data
      condx <- condx[condx[[cuniqueid]] %in% clipids,]

      ## Subset tree data
      if (istree) 
        treex <- treex[treex[[tuniqueid]] %in% clipids,]

      ## other data
      if (!is.null(other_layers)) {
        for (layer in other_layers) {
          assign(paste0(layer, "x"), get(layer)[get(layer)[["PLT_CN"]] %in% clipids, ])
        }
      }
    }

  } else {			## datsource in('datamart', 'sqlite')

    condx <- {}
    pltx <- {}
    tabs2save <- c("pltx", "condx")
    if (istree) {
      treex <- {} 
      tabs2save <- c(tabs2save, "treex")
    }
    if (!is.null(other_layers)) {
      for (layer in other_layers) {
        assign(paste0(layer, "x"), {})
        tabs2save <- c(tabs2save, paste0(layer, "x"))
      }
    }

    for (i in 1:length(stcds)) { 
      stcd <- stcds[i]
      state <- pcheck.states(stcd) 

      message(paste0("\ngetting data for ", state, "..."))

      msg <- "..."
      if (allyrs) {
        msg <- paste0(msg, "for all years")
      } else if (measCur) {
        msg <- paste0(msg, "for most currently measured plots")
        if (!is.null(measEndyr)) {
          msg <- paste0(msg, ", from year ", measEndyr, " or before")
          if (!is.null(Endyr.filter)) 
            msg <- paste0(msg, ", ", Endyr.filter)
        }
      } else if (!is.null(evalid)) {
        msg <- paste0(msg, "for evaluation:", evalid)
      } else if (evalCur) {
        msg <- paste0(msg, "for most evaluation")
        if (!is.null(evalEndyr))
          msg <- paste0("ending in ", evalEndyr)
          if (!is.null(Endyr.filter)) 
            msg <- paste0(msg, ", ", Endyr.filter)
      } else if (!is.null(invyrs)) {
        msg <- paste0(msg, "for inventory years", min(invyrs), "to", max(invyrs))
      } else {
        msg <- "using all plots in database"
        allyrs <- TRUE
      }
      message(paste(msg, "\n"))

      ## Check for counties
      if (!is.null(stbnd.att) && stbnd.att == "COUNTYFIPS" && !is.null(statecnty)) {
        stcnty <- statecnty[startsWith(statecnty, formatC(stcd, width=2, flag="0"))]
        countycds <- sort(as.numeric(unique(substr(stcnty, 3, 5))))
        stateFilter <- paste("p.countycd IN(", toString(countycds), ")")
      }

      if (datsource == "datamart") {
        stabbr <- pcheck.states(stcd, statereturn="ABBR") 

        #########################################
        ## Get CSV files
        #########################################

        ## PLOT table
        PLOT <- DBgetCSV("PLOT", stabbr, ZIP=TRUE, returnDT=TRUE, stopifnull=FALSE)

        ## COND table 
        cond <- DBgetCSV("COND", stabbr, ZIP=TRUE, returnDT=TRUE, stopifnull=FALSE)
 
        ## TREE table
        if (istree)
          tree <- DBgetCSV("TREE", stabbr, ZIP=TRUE, returnDT=TRUE, stopifnull=FALSE)

        ## other tables
        if (!is.null(other_layers)) {
          for (layer in other_layers)
            assign(layer, FIESTA::DBgetCSV(layer, stabbr, ZIP=TRUE, 
			returnDT=TRUE, stopifnull=FALSE))
        }

        ## Create state filter
        stfilter <- stateFilter

        ## Get most current evalid
        if (evalCur) {
          ## POP_PLOT_STRATUM_ASSGN table (ZIP FILE) - 
          pop_plot_stratum_assgn <- DBgetCSV("POP_PLOT_STRATUM_ASSGN", stabbr, 
			ZIP=TRUE, returnDT=TRUE, stopifnull=FALSE)    

          evalCurType <- ifelse(evalType == "ALL", "00", 
			ifelse(evalType == "AREAVOL", "01", "00"))
          evalid <- getEvalid.ppsa(ppsa=pop_plot_stratum_assgn, states=stcd, 
			evalEndyr=evalEndyr, evalCur=evalCur, evalType=evalCurType)
        }

        ## Get evalid filter
        if (!is.null(evalid)) {
          stfilter <- paste0("ppsa.evalid IN(", toString(evalid), ")")
        } else {
          if (intensity1)
            stfilter <- paste(stfilter, "p.intensity == 1", sep=" and ")
        }

        ## Print stfilter
        message(stfilter)

        ## get pfromqry
        pfromqry <- getpfromqry(evalid=evalid, plotCur=measCur, 
				Endyr=measEndyr, invyrs=invyrs, allyrs=allyrs, 
				intensity1=intensity1, syntax="R", plotnm="PLOT")
        if (is.null(pfromqry)) {
          message("no time frame specified... including all years")
          allyrs <- TRUE
          pfromqry <- getpfromqry(evalid=evalid, plotCur=measCur, 
				Endyr=measEndyr, invyrs=invyrs, allyrs=allyrs, 
				intensity1=intensity1, syntax="R", plotnm="PLOT")
        }
        ## Set up query for plots
        plt.qry <- paste0("select distinct p.* from ", pfromqry, " where ", stfilter) 
        puniqueid <- "CN"

        ## Query plt table
        plt <- setDT(sqldf::sqldf(plt.qry))
        plt[, ZSTUNCOPLOT := paste0("Z", 
			formatC(plt$STATECD, width=2, digits=2, flag=0), 
          		formatC(plt$UNITCD, width=2, digits=2, flag=0),
          		formatC(plt$COUNTYCD, width=3, digits=3, flag=0),
          		formatC(plt$PLOT, width=5, digits=5, flag=0))] 

        ## If duplicate plots, sort descending based on INVYR or CN and select 1st row
        if (nrow(plt) > length(unique(plt[[puniqueid]]))) {
          if ("INVYR" %in% names(plt)) {
            setorder(plt, -INVYR)
          } else {
            setorderv(plt, -puniqueid)
          }
          plt <- plt[, head(.SD, 1), by=pjoinid]
        }

        ## Check xy.joinid
        pltfields <- names(plt)
        if (xy.joinid %in% pltfields) {
          pjoinid  <- xy.joinid
        } else {
          if (xy.joinid == "PLT_CN" && "CN" %in% pltfields) {
            pjoinid <- "CN"
          } else {
            stop(xy.joinid, " not in plt")
          }
        }

        if (!is.null(Endyr.filter)) clipxy <- TRUE
        if (clipxy) {

          ## Generate xy table for all plots in state (xystate)
          #########################################################
          if (is.null(xydat)) {
            xvar <- "LON_PUBLIC"
            yvar <- "LAT_PUBLIC"
            measCur.xy <- FALSE
            if (allyrs || measCur) measCur.xy <- TRUE
            xyvars <- paste0("p.", c("STATECD", "UNITCD", "COUNTYCD", "PLOT", "LON", "LAT"))
            xyfromqry <- getpfromqry(evalid=evalid, plotCur=measCur, 
				Endyr=measEndyr, invyrs=invyrs, allyrs=allyrs, 
				intensity1=intensity1, syntax="R", plotnm="PLOT")
            xy.qry <- paste0("select distinct ", toString(xyvars), " from ", 
				xyfromqry, " where ", stfilter) 
            xystate <- setDT(sqldf::sqldf(xy.qry))
            setnames(xystate, c("LON", "LAT"), c(xvar, yvar))

            if (measCur.xy) {
              xystate[, ZSTUNCOPLOT := paste0("Z", 
				formatC(xystate$STATECD, width=2, digits=2, flag=0), 
          			formatC(xystate$UNITCD, width=2, digits=2, flag=0),
          			formatC(xystate$COUNTYCD, width=3, digits=3, flag=0),
          			formatC(xystate$PLOT, width=5, digits=5, flag=0))] 
              xy.joinid <- "ZSTUNCOPLOT"
            } else {
              xy.joinid <- puniqueid
            } 
            xystate <- xystate[, c(xy.joinid, xvar, yvar), with=FALSE]
            pjoinid <- xy.joinid
          }

          ## Get most current plots in database for !Endyr.filter
          #######################################################
          if (!is.null(Endyr.filter)) {
            bndxf1 <- datFilter(bndx, xfilter=Endyr.filter)$xf
            bndxf2 <- datFilter(bndx, xfilter=paste0("!", Endyr.filter))$xf

            ## Clip data using measEndyr for Endyr.filter
            clipdat <- spClipPoint(xyplt=xystate, 
				xy.uniqueid=xy.joinid, xvar=xvar, yvar=yvar, xy.crs=xy.crs, 
				clippolyv=bndxf1, stopifnotin=FALSE)
            xyplt1 <- clipdat$clip_xyplt
            plt1 <- plt[plt[[pjoinid]] %in% xyplt1[[xy.joinid]], ]
            pltids1 <- plt1[[puniqueid]]

            cond1 <- cond[cond[["PLT_CN"]] %in% pltids1, ]
            if (istree)
              tree1 <- tree[tree[["PLT_CN"]] %in% pltids1, ]
            if (!is.null(other_layers)) {
              for (layer in other_layers) {
                assign(paste0(layer, "2"), 
				get(layer)[get(layer)[["PLT_CN"]] %in% pltids1, ])
              }
            }

            ## Clip plots from all database
            ############################################
            ## Get most current evalid
            if (evalCur) {
              evalid <- getEvalid.ppsa(ppsa=pop_plot_stratum_assgn, states=stcd, 
				evalEndyr=evalEndyr, evalCur=evalCur, evalType=evalCurType)
            }
            ## Get evalid filter
            if (!is.null(evalid)) {
              stfilter <- paste0("ppsa.evalid IN(", toString(evalid), ")")
            } else {
              if (intensity1)
                stfilter <- paste(stfilter, "p.intensity == 1", sep=" and ")
            }
            ## get pfromqry
            pfromqry2 <- getpfromqry(evalid=evalid, plotCur=measCur, 
				invyrs=invyrs, allyrs=allyrs, 
				intensity1=intensity1, syntax="R", plotnm="PLOT")
            ## Set up query for plots
            plt2.qry <- paste0("select distinct p.* from ", pfromqry2, " where ", stfilter) 

            ## Query plt table
            plt2 <- setDT(sqldf::sqldf(plt2.qry))
            plt2[, ZSTUNCOPLOT := paste0("Z", 
			formatC(plt2$STATECD, width=2, digits=2, flag=0), 
          		formatC(plt2$UNITCD, width=2, digits=2, flag=0),
          		formatC(plt2$COUNTYCD, width=3, digits=3, flag=0),
          		formatC(plt2$PLOT, width=5, digits=5, flag=0))] 
            xystate2 <- plt2[, c(xy.joinid, "LON", "LAT"), with=FALSE]
            setnames(xystate2, c("LON", "LAT"), c(xvar, yvar))

            ## If duplicate plots, sort descending based on INVYR or CN and select 1st row
            if (nrow(plt2) > length(unique(plt2[[pjoinid]]))) {
              if ("INVYR" %in% names(plt)) {
                setorder(plt2, -INVYR)
              } else {
                setorderv(plt2, -puniqueid)
              }
              plt2 <- plt2[, head(.SD, 1), by=pjoinid]
            }

            ## Clip xystate and other tables for bndxf2
            ############################################
            clipdat <- spClipPoint(xyplt=xystate, 
				xy.uniqueid=xy.joinid, xvar=xvar, yvar=yvar, xy.crs=xy.crs, 
				clippolyv=bndxf2, stopifnotin=FALSE)
            xyplt2 <- clipdat$clip_xyplt

            plt2 <- plt2[plt2[[pjoinid]] %in% xyplt2[[xy.joinid]], ]
            pltids2 <- plt2[[puniqueid]]

            cond2 <- cond[cond[["PLT_CN"]] %in% pltids2, ]
            if (istree)
              tree2 <- tree[tree[["PLT_CN"]] %in% pltids2, ]
            if (!is.null(other_layers)) {
              for (layer in other_layers) {
                assign(paste0(layer, "2"), 
				get(layer)[get(layer)[["PLT_CN"]] %in% pltids2, ])
              }
            }

            plt <- rbind(plt1, plt2)
            cond <- rbind(cond1, cond2)
            if (istree)
              tree <- rbind(tree1, tree2)
            if (!is.null(other_layers)) {
              for (i in 1:length(other_layers)) {
                layer <- other_layers[i]
                assign(paste0(layer), rbind(paste0(layer, "1"), paste0(layer, "2")))
              }
            }
            if (savexy || showsteps)
              xyplt <- rbind(xyplt1, xyplt2)

          } else {    ## Endyr.filter = NULL

            ## Clip data
            clipdat <- spClipPoint(xyplt=xystate, 
				xy.uniqueid=xy.joinid, xvar=xvar, yvar=yvar, xy.crs=xy.crs, 
				clippolyv=bndx)
            xyplt <- clipdat$clip_xyplt

            pltx <- plt[plt[[pjoinid]] %in% xyplt[[xy.joinid]], ]
            pltids <- pltx[[puniqueid]]

            cond <- cond[cond[["PLT_CN"]] %in% pltids, ]
            if (istree)
              tree <- tree[tree[["PLT_CN"]] %in% pltids, ]
            if (!is.null(other_layers)) {
              for (layer in other_layers) {
                assign(layer, 
				get(layer)[get(layer)[["PLT_CN"]] %in% pltids, ])
              }
            }
          }  ## if Endyr.filter is not NULL

          pltx <- rbind(pltx, plt)
          condx <- rbind(condx, cond)
          if (istree)
            treex <- rbind(treex, tree)
          if (!is.null(other_layers)) {
            for (i in 1:length(other_layers)) {
              layer <- other_layers[i]
              assign(paste0(layer, "x"), rbind(paste0(layer, "x"), layer))
            }
          }
          if (savexy || showsteps)
            xypltx <- rbind(xypltx, xyplt)

        } else {      ## clipxy = FALSE

          if ((savexy || showsteps) && !is.null(xydat))
            xypltx <- xydat

          pltx <- rbind(pltx, plt)
          condx <- rbind(condx, cond)
          treex <- rbind(treex, tree)
        
        }   ## clipxy
        rm(plt)
        rm(cond)
        if (istree) rm(tree)
        gc()
          
      } else if (datsource == "sqlite") {

        ####################################################################
        ## 1) Check if data for all states is in database
        ## 1) Get most current plots from xy database that intersect state
        ## 2) Clip xy (for state) to boundary
        ## 3) Subset other data with clipped xy joinid
        ####################################################################

        if (i == 1) {
          conn <- DBtestSQLite(data_dsn, dbconnopen=TRUE, showlist=FALSE)
          tabs <- DBI::dbListTables(conn)
          layers <- c(plot_layer, cond_layer)
          if (istree) layers <- c(layers, tree_layer)
          if (!any(layers %in% tabs))
            stop("missing layers in database: ", toString(layers[!layers %in% tabs]))
          if (!is.null(other_layers) && !any(other_layers %in% tabs))
            stop("missing layers in database: ", toString(other_layers[!other_layers %in% tabs]))

          ## Check for state in database
          dbstcds <- DBI::dbGetQuery(conn, paste("select distinct statecd from", 
				plot_layer))[[1]]
          if (!all(stcds %in% dbstcds)) {
            statemiss <- stcds[!stcds %in% dbstcds]
            message("database does not include all states: ", toString(statemiss))
          
            if (length(stcds) == length(statemiss)) {
              message("database does not include states...")
              return(NULL)
            } else {
              message("database does not include all states: ", toString(statemiss))
            }  
          }
          if (clipxy && is.null(xydat)) {
            xyconn <- DBtestSQLite(xy_dsn, dbconnopen=TRUE, showlist=FALSE,
					createnew=FALSE)
            if (!is.null(xyconn)) {
              tabs <- DBI::dbListTables(xyconn)
            } else {
              xyconn <- conn
            }

            ## Check xy data
            ######################################################################
            if (is.null(xy)) {
              xytabs <- DBI::dbListTables(xyconn)
              xytabs <- xytabs[grepl("xy", xytabs)]
              if (length(xytabs) == 0)
                stop("no xy in ", xy_dsn)

              xy <- xytabs[grepl("ACTUAL", xytabs)]
              if (length(xy) == 1) {
                xvar <- "LON_ACTUAL"
                yvar <- "LAT_ACTUAL"
              } else {
                xy <- xytabs[grepl("PUBLIC", xytabs)]
                if (length(xy) == 1) {
                  xvar <- "LON_PUBLIC"
                  yvar <- "LAT_PUBLIC"
                } else {
                  if ("plot" %in% tabs) 
                    xy <- "plot"
                }
              }
            } 
            if (!xy %in% tabs)
              stop(xy, " not in ", xy_dsn) 

            xyfields <- DBI::dbListFields(xyconn, xy)
            if (is.null(xvar))
              xvar <- xyfields[grepl("LON", xyfields)]
            if (is.null(yvar))
              yvar <- xyfields[grepl("LAT", xyfields)]
            xy.uniqueid <- pcheck.varchar(var2check=xy.uniqueid, varnm="xy.uniqueid", 
			gui=gui, checklst=xyfields, caption="xy uniqueid")
            xy.joinid <- pcheck.varchar(var2check=xy.joinid, varnm="xy.joinid", 
			gui=gui, checklst=xyfields, caption="xy joinid")
            if (is.null(xy.joinid)) xy.joinid <- xy.uniqueid
            xvar <- pcheck.varchar(var2check=xvar, varnm="xvar", gui=gui, 
			checklst=xyfields, caption="x variable", stopifnull=TRUE)
            yvar <- pcheck.varchar(var2check=yvar, varnm="yvar", gui=gui, 
			checklst=xyfields, caption="y variable", stopifnull=TRUE)

            if (grepl("ACTUAL", xy) && grepl("PUBLIC", xvar))
              warning("check xy and xvar: ", toString(c(xy, xvar)))
            if (grepl("ACTUAL", xy) && grepl("PUBLIC", yvar))
              warning("check xy and yvar: ", toString(c(xy, yvar)))

            ## Check xy.joinid
            pltfields <- DBI::dbListFields(conn, "plot")

            if (grepl("xyCur", xy)) {
              if (xy.joinid %in% c("CN", "PLT_CN") && "ZSTUNCOPLOT" %in% xyfields && 
				"ZSTUNCOPLOT" %in% pltfields) {
                 message("changing xy.joinid from ", xy.joinid, "to ZSTUNCOPLOT")
                 xy.joinid <- "ZSTUNCOPLOT"
              }
            }
                 
            if (xy.joinid %in% pltfields) {
              pjoinid  <- xy.joinid
            } else {
              if (xy.joinid == "PLT_CN" && "CN" %in% pltfields) {
                pjoinid <- "CN"
              } else {
                stop(xy.joinid, " not in plt")
              }
            }
          }
        }    ## End if i=1

        ## Create state filter
        stfilter <- paste("p.statecd IN(", toString(stcd), ")")
        if (!is.null(stateFilter)) 
          stfilter <- paste(stfilter, "and", stateFilter)
    
        ## Get most current evalid
        if (evalCur) {
          evalCurType <- ifelse(evalType == "ALL", "00", 
			ifelse(evalType == "AREAVOL", "01", "00"))
          evalid <- getEvalid(dbconn=conn, states=stcd, evalEndyr=evalEndyr, 
			evalCur=evalCur, evalType=evalCurType)
        }
        ## Get evalid filter
        if (!is.null(evalid)) {
          stfilter <- paste0("ppsa.evalid IN(", toString(evalid), ")")
        } else {
          if (intensity1)
            stfilter <- paste(stfilter, "p.intensity == 1", sep=" and ")
        }

        ## Print stfilter
        message(stfilter)

        ## get pfromqry
        pfromqry <- getpfromqry(dsn=data_dsn, evalid=evalid, plotCur=measCur, 
				Endyr=measEndyr, invyrs=invyrs, allyrs=allyrs, 
				intensity1=intensity1, syntax="R")
        if (is.null(pfromqry)) {
          message("no time frame specified... including all years")
          allyrs <- TRUE
          pfromqry <- getpfromqry(dsn=data_dsn, evalid=evalid, plotCur=measCur, 
				Endyr=measEndyr, invyrs=invyrs, allyrs=allyrs, 
				intensity1=intensity1, syntax="R")
        }
        ## Set up query for plots
        plt.qry <- paste0("select distinct p.* from ", pfromqry, " where ", stfilter) 

        ## Query database for plots
        rs <- DBI::dbSendQuery(conn, plt.qry)
        plt <- setDT(DBI::dbFetch(rs))
        DBI::dbClearResult(rs)

        zids <- c("STATECD", "UNITCD", "COUNTYCD", "PLOT")
        if (!"ZSTUNCOPLOT" %in% names(plt)) {
          if (!all(zids %in% names(plt))) {
            message("cannot create unique identifier for plot")
            message(toString(zids[which(!zids %in% names(plt))]), " not in plt")
          } else {
            plt[, ZSTUNCOPLOT := paste0("Z", 
			formatC(plt$STATECD, width=2, digits=2, flag=0), 
          		formatC(plt$UNITCD, width=2, digits=2, flag=0),
          		formatC(plt$COUNTYCD, width=3, digits=3, flag=0),
          		formatC(plt$PLOT, width=5, digits=5, flag=0))] 
          }
        }
        ## If duplicate plots, sort descending based on INVYR or CN and select 1st row
        if (nrow(plt) > length(unique(plt[[pjoinid]]))) {
          if ("INVYR" %in% names(plt)) {
            setorder(plt, -INVYR)
          } else {
            setorderv(plt, -puniqueid)
          }
          plt <- plt[, head(.SD, 1), by=pjoinid]
        }

        if (!is.null(Endyr.filter)) clipxy <- TRUE
        if (clipxy) {
          ## Generate xy table for all plots in state (xystate)
          #########################################################
          if (!is.null(xydat)) {
            xyvars <- c(xy.joinid, xvar, yvar)
            xystate <- xydat[, xyvars, with=FALSE]
          } else {            
            xy.joinid <- pjoinid
            xyvars <- c(xy.joinid, xvar, yvar)
            if (xy == "plot") {
              xy.qry <- paste0("select distinct ", toString(xyvars), " from ", 
				pfromqry, " where ", stfilter) 
            } else {            
              if (allyrs || measCur) measCur.xy <- TRUE
              xyfromqry <- getpfromqry(evalid=evalid, plotCur=measCur.xy, 
				invyrs=invyrs, allyrs=allyrs, 
				intensity1=intensity1, syntax="R", plotnm="PLOT")

              xy.qry <- paste0("select distinct ", toString(paste0("xy.", xyvars)), 
				" from ", xyfromqry, " JOIN ", xy, " xy ON (p.", pjoinid, 
				" = xy.", xy.joinid, ")", " where ", stfilter) 
            }
            xystate <- DBI::dbGetQuery(xyconn, xy.qry) 
          }

          ## Get most current plots in database for Endyr.filter & !Endyr.filter
          #######################################################################
          if (!is.null(Endyr.filter)) {
            bndxf1 <- datFilter(bndx, xfilter=Endyr.filter)$xf
            bndxf2 <- datFilter(bndx, xfilter=paste0("!", Endyr.filter))$xf

            ## Clip xystate and other tables for bndxf1
            ############################################
            clipdat <- spClipPoint(xyplt=xystate, 
				xy.uniqueid=xy.joinid, xvar=xvar, yvar=yvar, xy.crs=xy.crs, 
				clippolyv=bndxf1, stopifnotin=FALSE)
            xyplt1 <- clipdat$clip_xyplt
            plt1 <- plt[plt[[pjoinid]] %in% xyplt1[[xy.joinid]], ]
            pltids1 <- plt1[[puniqueid]]

            cond1.qry <- paste0("select cond.* from ", pfromqry,
			" join cond on(cond.PLT_CN = p.CN) where ", stfilter, 
				" and p.", puniqueid, " in(", addcommas(pltids1, quotes=TRUE), ")")
            rs <- DBI::dbSendQuery(conn, cond1.qry)
            cond1 <- DBI::dbFetch(rs)
            DBI::dbClearResult(rs)

            if (istree) {
              tree1.qry <- paste0("select tree.* from ", pfromqry,
			" join tree on(tree.PLT_CN = p.CN) where ", stfilter, 
			" and p.", puniqueid, " in(", addcommas(pltids1, quotes=TRUE), ")")
              rs <- DBI::dbSendQuery(conn, tree1.qry)
              tree1 <- DBI::dbFetch(rs)
              DBI::dbClearResult(rs)
            }

            if (!is.null(other_layers)) {
              for (i in 1:length(other_layers)) {
                layer <- other_layers[i]
                ofromqry <- paste(pfromqry, "JOIN", layer, "o on(o.PLT_CN=p.CN)")
                other.qry <- paste("select o.* from", ofromqry, "where", stfilter,
				" and p.", puniqueid, " in(", addcommas(pltids1, quotes=TRUE), ")")
                rs <- DBI::dbSendQuery(conn, other.qry)
                assign(paste0(layer, "1"), DBI::dbFetch(rs))
                othertabnms <- c(othertabnms, layer)
                DBI::dbClearResult(rs)
              }
            } 

            ## Clip plots from all database
            ############################################
            ## Get most current evalid
            if (evalCur) {
              evalid <- getEvalid.ppsa(ppsa=pop_plot_stratum_assgn, states=stcd, 
				evalCur=evalCur, evalType=evalCurType)
            }
            ## Get evalid filter
            if (!is.null(evalid)) {
              stfilter <- paste0("ppsa.evalid IN(", toString(evalid), ")")
            } else {
              if (intensity1)
                stfilter <- paste(stfilter, "p.intensity == 1", sep=" and ")
            }
            ## get pfromqry
            pfromqry2 <- getpfromqry(evalid=evalid, plotCur=measCur, 
				invyrs=invyrs, allyrs=allyrs, 
				intensity1=intensity1, syntax="R", plotnm="PLOT")
            ## Set up query for plots
            plt2.qry <- paste0("select distinct p.* from ", pfromqry2, " where ", stfilter) 

            ## Query database for plots
            rs <- DBI::dbSendQuery(conn, plt2.qry)
            plt2 <- DBI::dbFetch(rs)

            if (!"ZSTUNCOPLOT" %in% names(plt2)) {
              if (all(zids %in% names(plt2))) {
                plt2[, ZSTUNCOPLOT := paste0("Z", 
				formatC(plt2$STATECD, width=2, digits=2, flag=0), 
          			formatC(plt2$UNITCD, width=2, digits=2, flag=0),
          			formatC(plt2$COUNTYCD, width=3, digits=3, flag=0),
          			formatC(plt2$PLOT, width=5, digits=5, flag=0))] 
              }
            }

            ## If duplicate plots, sort descending based on INVYR or CN and select 1st row
            if (nrow(plt2) > length(unique(plt2[[pjoinid]]))) {
              if ("INVYR" %in% names(plt)) {
                setorder(plt2, -INVYR)
              } else {
                setorderv(plt2, -puniqueid)
              }
              plt2 <- plt2[, head(.SD, 1), by=pjoinid]
            }

            ## Clip xystate and other tables for bndxf2
            ############################################
            clipdat <- spClipPoint(xyplt=xystate, 
				xy.uniqueid=xy.joinid, xvar=xvar, yvar=yvar, xy.crs=xy.crs, 
				clippolyv=bndxf2, stopifnotin=FALSE)
            xyplt2 <- clipdat$clip_xyplt

            plt2 <- plt2[plt2[[pjoinid]] %in% xyplt2[[xy.joinid]], ]
            pltids2 <- plt2[[puniqueid]]

            cond2.qry <- paste0("select cond.* from ", pfromqry,
			" join cond on(cond.PLT_CN = p.CN) where ", stfilter, 
				" and p.", puniqueid, " in(", addcommas(pltids2, quotes=TRUE), ")")
            rs <- DBI::dbSendQuery(conn, cond2.qry)
            cond2 <- DBI::dbFetch(rs)
            DBI::dbClearResult(rs)

            if (istree) {
              tree2.qry <- paste0("select tree.* from ", pfromqry,
			" join tree on(tree.PLT_CN = p.CN) where ", stfilter, 
			" and p.", puniqueid, " in(", addcommas(pltids2, quotes=TRUE), ")")
              rs <- DBI::dbSendQuery(conn, tree2.qry)
              tree2 <- DBI::dbFetch(rs)
              DBI::dbClearResult(rs)
            }

            if (!is.null(other_layers)) {
              for (i in 1:length(other_layers)) {
                layer <- other_layers[i]
                ofromqry <- paste(pfromqry, "JOIN", layer, "o on(o.PLT_CN=p.CN)")
                other.qry <- paste("select o.* from", ofromqry, "where", stfilter,
				" and p.", puniqueid, " in(", addcommas(pltids2, quotes=TRUE), ")")
                rs <- DBI::dbSendQuery(conn, other.qry)
                assign(paste0(layer, "2"), DBI::dbFetch(rs))
                othertabnms <- c(othertabnms, layer)
                DBI::dbClearResult(rs)
              }
            } 
          
            plt <- rbind(plt1, plt2)
            cond <- rbind(cond1, cond2)
            if (istree)
              tree <- rbind(tree1, tree2)
            if (!is.null(other_layers)) {
              for (i in 1:length(other_layers)) {
                layer <- other_layers[i]
                assign(paste0(layer), rbind(paste0(layer, "1"), paste0(layer, "2")))
              }
            }
            if (savexy || showsteps)
              xyplt <- rbind(xyplt1, xyplt2)

          } else {    ## Endyr.filter = NULL

            ## Clip data
            clipdat <- spClipPoint(xyplt=xystate, 
				xy.uniqueid=xy.joinid, xvar=xvar, yvar=yvar, xy.crs=xy.crs, 
				clippolyv=bndx)
            xyplt <- clipdat$clip_xyplt

            plt <- plt[plt[[pjoinid]] %in% xyplt[[xy.joinid]], ]
            pltids <- plt[[puniqueid]]

            cond.qry <- paste0("select cond.* from ", pfromqry,
			" join cond on(cond.PLT_CN = p.CN) where ", stfilter, 
				" and p.", puniqueid, " in(", addcommas(pltids, quotes=TRUE), ")")
            rs <- DBI::dbSendQuery(conn, cond.qry)
            cond <- DBI::dbFetch(rs)
            DBI::dbClearResult(rs)

            if (istree) {
              tree.qry <- paste0("select tree.* from ", pfromqry,
			" join tree on(tree.PLT_CN = p.CN) where ", stfilter, 
			" and p.", puniqueid, " in(", addcommas(pltids, quotes=TRUE), ")")
              rs <- DBI::dbSendQuery(conn, tree.qry)
              tree <- DBI::dbFetch(rs)
              DBI::dbClearResult(rs)
            }

            if (!is.null(other_layers)) {
              for (i in 1:length(other_layers)) {
                layer <- other_layers[i]
                ofromqry <- paste(pfromqry, "JOIN", layer, "o on(o.PLT_CN=p.CN)")
                other.qry <- paste("select o.* from", ofromqry, "where", stfilter,
				" and p.", puniqueid, " in(", addcommas(pltids, quotes=TRUE), ")")
                rs <- DBI::dbSendQuery(conn, other.qry)
                assign(paste0(layer), DBI::dbFetch(rs))
                othertabnms <- c(othertabnms, layer)
                DBI::dbClearResult(rs)
              }
            } 
          }  ## if Endyr.filter is not NULL

          pltx <- rbind(pltx, plt)
          condx <- rbind(condx, cond)

          if (istree)
            treex <- rbind(treex, tree)
          if (!is.null(other_layers)) {
            for (i in 1:length(other_layers)) {
              layer <- other_layers[i]
              assign(paste0(layer, "x"), rbind(paste0(layer, "x"), layer))
            }
          }
          if (savexy || showsteps)
            xypltx <- rbind(xypltx, xyplt)

        } else {      ## clipxy = FALSE

          if ((savexy || showsteps) && !is.null(xydat))
            xypltx <- xydat
        
          ## Query database for plots
          rs <- DBI::dbSendQuery(conn, plt.qry)
          plt <- DBI::dbFetch(rs)

          ## If duplicate plots, sort descending based on INVYR or CN and select 1st row
          if (nrow(plt) > length(unique(plt[[pjoinid]]))) {
            if ("INVYR" %in% names(plt)) {
              setorder(plt, -INVYR)
            } else {
              setorderv(plt, -puniqueid)
            }
            plt <- plt[, head(.SD, 1), by=pjoinid]
          }

          pltids <- plt[[puniqueid]]
          pltx <- rbind(pltx, plt)
          rm(plt)
          gc()

          cond.qry <- paste0("select cond.* from ", pfromqry,
			" join cond on(cond.PLT_CN = p.CN) where ", stfilter, 
				" and p.", puniqueid, " in(", addcommas(pltids, quotes=TRUE), ")")
          rs <- DBI::dbSendQuery(conn, cond.qry)
          condx <- rbind(condx, DBI::dbFetch(rs))
          DBI::dbClearResult(rs)

          if (istree) {
            tree.qry <- paste0("select tree.* from ", pfromqry,
			" join tree on(tree.PLT_CN = p.CN) where ", stfilter, 
			" and p.", puniqueid, " in(", addcommas(pltids, quotes=TRUE), ")")
            rs <- DBI::dbSendQuery(conn, tree.qry)
            treex <- rbind(treex, DBI::dbFetch(rs))
            DBI::dbClearResult(rs)
          }

          if (!is.null(other_layers)) {
            for (i in 1:length(other_layers)) {
              layer <- other_layers[i]
              ofromqry <- paste(pfromqry, "JOIN", layer, "o on(o.PLT_CN=p.CN)")
              other.qry <- paste("select o.* from", ofromqry, "where", stfilter,
			" and p.", puniqueid, " in(", addcommas(pltids, quotes=TRUE), ")")
              rs <- DBI::dbSendQuery(conn, other.qry)
              assign(paste0(layer, "x"), rbind(paste0(layer, "x"), DBI::dbFetch(rs)))
              othertabnms <- c(othertabnms, layer)
              DBI::dbClearResult(rs)
            }
          } 
        }   ## clipxy
      }  ## datsource
    }  ## End of looping thru states

    ## Disconnect database
    if (datsource == "sqlite")
      DBI::dbDisconnect(conn)
  }

  #############################################################################
  ## Save tables
  #############################################################################
  if (savedata) {
    if (savexy) 
      datExportData(sf::st_drop_geometry(xypltx), outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="xyplt", 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer)
    for (tab in tabs2save) {
      datExportData(get(tab), outfolder=outfolder, out_fmt=out_fmt, 
		out_dsn=out_dsn, out_layer=tab, outfn.date=outfn.date, 
		overwrite_layer=overwrite_layer)
    }
  } 

  if (showsteps) {
    ## Set plotting margins
    mar <-  par("mar")
    par(mar=c(1,1,1,1))

    plot(sf::st_geometry(bndx), border="grey")
    plot(sf::st_geometry(xypltx), add=TRUE, col="blue", cex=.25)
    par(mar=mar)
  }

  returnlst$clip_tabs <- lapply(tabs2save, get, envir=environment())
#  returnlst$clip_tabs <- lapply(tabs2save, get)

  names(returnlst$clip_tabs) <- paste0("clip_", tabs2save)

  if (savexy) 
    returnlst$clip_xyplt <- xypltx
  
  returnlst$clip_polyv <- bndx
  returnlst$puniqueid <- puniqueid
  returnlst$xy.uniqueid <- xy.joinid
  returnlst$pjoinid <- pjoinid
  returnlst$states <- states
  return(returnlst)
}
