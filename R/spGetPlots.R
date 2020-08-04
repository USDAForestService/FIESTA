spGetPlots <- function(bnd, bnd_dsn=NULL, bnd.filter=NULL, states=NULL, 
	stbnd=NULL, stbnd_dsn=NULL, stbnd.att=NULL, RS=NULL, xy=NULL, xy_dsn=NULL, 	
	xy.uniqueid="PLT_CN", xvar="LON_PUBLIC", yvar="LAT_PUBLIC", xy.crs=4269, 
	xy.joinid="PLT_CN", clipxy=TRUE, datsource="datamart", data_dsn=NULL, 
	istree=TRUE, plot_layer="plot", cond_layer="cond", tree_layer="tree", 
	other_layers=NULL, puniqueid="CN", evalid=NULL, evalCur=FALSE, evalEndyr=NULL, 
	evalType="AREAVOL", measCur=FALSE, measEndyr=NULL, invyrs=NULL, allyrs=FALSE, 
	intensity1=FALSE, showsteps=FALSE, savedata=FALSE, savebnd=FALSE, savexy=FALSE, 
	outfolder=NULL, out_fmt="shp", out_dsn=NULL, outfn.pre=NULL, outfn.date=FALSE,
 	overwrite_dsn=FALSE, overwrite_layer=FALSE, ...) {

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
  xyx=stateFilter=xyplt=tabs2save <- NULL
  cuniqueid=tuniqueid <- "PLT_CN"
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
  bndx <- datFilter(bndx, xfilter=bnd.filter)$xf

  #############################################################################
  ## Set datsource
  ########################################################
  datsourcelst <- c("obj", "csv", "sqlite", "datamart")
  datsource <- FIESTA::pcheck.varchar(var2check=datsource, varnm="datsource", 
		checklst=datsourcelst, gui=gui, caption="Data source?") 
  if (datsource == "datamart") {
    if (!all(c("httr", "sqldf") %in% rownames(installed.packages())))
	 stop("httr and sqldf packages are required to run CSV queries")
  } else if (datsource == "sqlite") {
    if (!all(c("RSQLite", "DBI") %in% rownames(installed.packages())))
	 stop("RSQLite and DBI packages are required to run SQLite queries")
  } 
  if (datsource == "sqlite") 
    data_dsn <- DBtestSQLite(data_dsn, dbconnopen=FALSE)
       
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
  if (savedata || savexy || savebnd) {
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
    ## Get intersecting states
#    statedat <- spGetStates(bndx, stbnd=stbnd, stbnd_dsn=stbnd_dsn, 
#			stbnd.att=stbnd.att, RS=RS, states=states, savebnd=savebnd, 
#			outfolder=outfolder)


    ## Get intersecting states
    statedat <- spGetStates(bndx, stbnd=stbnd, stbnd_dsn=stbnd_dsn, 
			stbnd.att=stbnd.att, RS=RS, states=states, savebnd=savebnd, 
			outfolder=outfolder, ...) 

    bndx <- statedat$bndx
    if (!is.null(stbnd.att) && stbnd.att == "COUNTYFIPS") {
      statecnty <- statedat$states
      stcds <- unique(as.numeric(substr(states, 1,2)))
    } else {
      stcds <- FIESTA::ref_statecd$VALUE[FIESTA::ref_statecd$MEANING %in% statedat$states]
    }
  }
  message("boundary intersected states: ", toString(stcds))

  #############################################################################
  ## NOTE: Maybe add percent overlap here
  #############################################################################
  ## Check xy table
  xy <- pcheck.spatial(xy, xy_dsn)

  ## Subset xyplt to boundary
  if (!is.null(xy) && clipxy) {

    xy.joinid <- FIESTA::pcheck.varchar(var2check=xy.joinid, 
		varnm="xy.joinid", gui=gui, 
		checklst=names(xy), caption="JoinID in xy", 
		warn=paste(xy.joinid, "not in xy"), stopifnull=TRUE)

    clipdat <- spClipPoint(xyplt=xy, xy.uniqueid=xy.joinid, 
			xvar=xvar, yvar=yvar, xy.crs=xy.crs, addxy=TRUE, clippolyv=bndx)
    xyplt <- clipdat$clip_xyplt 
    #bndx <- rbind(bndx, clipdat$clip_polyv)
    if (length(xyplt) == 0) stop("xy does not overlap bndx")

    plot(st_geometry(clipdat$clip_polyv))
    plot(st_geometry(xyplt), add=TRUE, col="blue")
  }


  ## Loop through states (because of memory issues)
  #stcds <- FIESTA::ref_statecd$VALUE[FIESTA::ref_statecd$MEANING %in% states]
  #xyx <- {}
  #bndx <- {}

  if (datsource %in% c("obj", "csv")) {
    if (is.null(xy) && clipxy) {
      message("no xy data... using all plots in dataset")
      clipxy <- FALSE
      savexy <- FALSE
    }

    ## plot data
    obj <- ifelse (datsource == "obj", TRUE, FALSE)
    pltx <- pcheck.table(plot_layer, obj=obj)

    ## Check xy.joinid
    if (xy.joinid %in% names(plt)) {
      pjoinid  <- xy.joinid
    } else {
      if (xy.joinid == "PLT_CN" && "CN" %in% names(pltx)) {
        pjoinid <- "CN"
      } else {
        stop(xy.joinid, " not in plt")
      }
    }

    message("Clipping plt ..")
    pltx <- plt[plt[[pjoinid]] %in% xyplt[[xy.joinid]],]
    if (nrow(pltx) == 0) stop("xy.joinid invalid")
    tabs2save <- c(tabs2save, "pltx")

    if (clipxy)
      clipids <- pltx[[puniqueid]]

    ## condition data
    condx <- pcheck.table(cond_layer, obj=obj, stopifnull=TRUE)
    if (clipxy) 
      condx <- condx[condx[[cuniqueid]] %in% clipids,]
    tabs2save <- c(tabs2save, "condx")
  
    ## tree data
    if (istree) {
      treex <- pcheck.table(tree_layer, obj=obj, stopifnull=TRUE)
      if (clipxy) 
        treex <- treex[treex[[tuniqueid]] %in% clipids,]
      if (!is.null(treex)) 
        tabs2save <- c(tabs2save, "treex")
    }

    ## other data
    if (!is.null(other_layers)) {
      for (layer in other_layers) {
        assign(paste0(layer, "x"), pcheck.table(layer, obj=obj, stopifnull=TRUE))
        if (clipxy) 
          assign(paste0(layer, "x"), get(layer)[get(layer)[["PLT_CN"]] %in% clipids, ])
        tabs2save <- c(tabs2save, layer)
      }
    }
  } else {
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
      message("getting data for ", state, "...")

      ## Check for counties
      if (!is.null(stbnd.att) && stbnd.att == "COUNTYFIPS") {
        stcnty <- states[startsWith(states, as.character(stcd))]
        countycds <- sort(as.numeric(unique(substr(stcnty, 3,5))))
        stateFilter <- paste("p.countycd IN(", toString(countycds), ")")
      }

      if (datsource == "datamart") {
        #############################################################################
        ## Get most current plots from database that intersect state
        #############################################################################
        spcoords <- "PUBLIC" 
        datPlots <- DBgetPlots(states=state, istree=istree, savedata=FALSE, 
				evalid=evalid, evalCur=evalCur, evalEndyr=evalEndyr, 
				evalType=evalType, measCur=measCur, measEndyr=measEndyr, 
				invyrs=invyrs, allyrs=allyrs, intensity1=intensity1, 
				stateFilter=stateFilter, othertables=other_layers, ...)
        plt <- datPlots$plt
        cond <- datPlots$cond
        if (clipxy)
          othertabnms <- c("plt", "cond")
        puniqueid <- "CN"
        if (istree) {
          tree <- datPlots$tree
          if (clipxy)
            othertabnms <- c(othertabnms, "tree")
        }
        if (!is.null(other_layers)) {
          for (layer in other_layers) {
            assign(layer, datPlots[[layer]])
            if (clipxy) 
              othertabnms <- c(othertabnms, layer)
          }
        }

        if (is.null(xy)) {
          if ("xy_PUBLIC" %in% names(datPlots)) {
            xystate <- datPlots$xy_PUBLIC
            xy.uniqueid <- "PLT_CN"
          } else if ("xyCur_PUBLIC" %in% names(datPlots)) {
            xystate <- datPlots$xyCur_PUBLIC
            xy.uniqueid <- "ZSTUNCOPLOT"
            xy.joinid <- "ZSTUNCOPLOT"
          } else {
            stop("invalid xy")
          }
          xvar <- "LON_PUBLIC"
          yvar <- "LAT_PUBLIC"

          ## Subset xyplt to boundary
          if (clipxy) {
            clipdat <- spClipPoint(xyplt=xystate, xy.uniqueid=xy.joinid, 
					xvar=xvar, yvar=yvar, xy.crs=xy.crs, addxy=TRUE, 
					clippolyv=bndx, othertabnms=othertabnms)
            clip_tabs <- clipdat$clip_tabs
            pltx <- rbind(pltx, clip_tabs$clip_plt)
            condx <- rbind(condx, clip_tabs$clip_cond)
            if (savexy) 
              xyplt <- rbind(xyplt, clipdat$clip_xyplt) 
            if (istree)
              treex <- rbind(treex, clip_tabs$clip_tree)
            if (!is.null(other_layers)) {
              for (layer in other_layers) {
                assign(paste0(layer, "x"), rbind(get(paste0(layer, "x")), 
				clip_tabs[[paste0("clip_", layer)]]))
              }
            }
          } else {
            pltx <- plt
            condx <- cond
            if (istree) 
              treex <- tree
            if (!is.null(other_layers)) {
              for (layer in other_layers) {
                assign(paste0(layer, "x"), layer)
              }
            }
          }
        }
      } else if (datsource == "sqlite") {

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

          if (is.null(xy)) {
            xy_dsn <- SQLitefn

            ## Check xy parameters
            xytabs <- tabs[grepl("xy", tabs)]
            if (length(xytabs) == 0)
              stop("no xy is SQLitefn")
            xy.uniqueid <- "PLT_CN"
            xy.joinid <- "ZSTUNCOPLOT"

            xynm <- xytabs[grepl("ACTUAL", xytabs)]
            if (length(xynm) == 1) {
              xvar <- "LON_ACTUAL"
              yvar <- "LAT_ACTUAL"
            } else {
              xynm <- xytabs[grepl("PUBLIC", xytabs)]
              if (length(xynm) == 1) {
                xvar <- "LON_PUBLIC"
                yvar <- "LAT_PUBLIC"
              }
            }
          }
        }

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
 
        if (is.null(xy)) {   
          xyfromqry <- paste(pfromqry, paste0("JOIN ", xynm, 
				" xy on(xy.", xy.joinid, "=p.", xy.joinid, ")"))
          xy.qry <- paste("select distinct xy.* from", xyfromqry, "where", stfilter) 
          message("extracting xy data...")
          xystate <- DBI::dbGetQuery(conn, xy.qry) 
      
          ## Subset xyplt to boundary
          if (clipxy) {
            xyplt <- spMakeSpatialPoints(xystate, xy_dsn, xy.uniqueid=xy.joinid, 
			xvar=xvar, yvar=yvar, xy.crs=xy.crs, addxy=TRUE)
            clipdat <- spClipPoint(xyplt=xyplt, xy.uniqueid=xy.joinid, clippolyv=bndx)
            xyplt <- clipdat$clip_xyplt
            if (savexy)
              xyx <- rbind(xyx, xyplt) 

            if (i == 1) plot(st_geometry(bndx))
            plot(st_geometry(xyplt), add=TRUE, col="blue", cex=.8)
          }
        }

        plt.qry <- paste0("select * from ", pfromqry, " where ", stfilter, 
				" and p.[", puniqueid, "] = ?")
        rs <- DBI::dbSendQuery(conn, plt.qry, params = list(xyx$PLT_CN))
        pltx <- rbind(pltx, DBI::dbFetch(rs))
        DBI::dbClearResult(rs)

        cond.qry <- paste0("select cond.* from ", pfromqry,
			" join cond on(cond.PLT_CN = p.CN) where ", stfilter, 
				" and p.[", puniqueid, "] = ?")
        rs <- DBI::dbSendQuery(conn, cond.qry, params = list(xyx$PLT_CN))
        condx <- rbind(condx, DBI::dbFetch(rs))
        DBI::dbClearResult(rs)

        if (istree) {
          tree.qry <- paste0("select tree.* from ", pfromqry,
			" join tree on(tree.PLT_CN = p.CN) where ", stfilter, 
			" and p.[", puniqueid, "] = ?")
          rs <- DBI::dbSendQuery(conn, tree.qry, params = list(xyx$PLT_CN))
          treex <- rbind(treex, DBI::dbFetch(rs))
          DBI::dbClearResult(rs)
        }

        if (!is.null(other_layers)) {
          for (i in 1:length(other_layers)) {
            layer <- other_layers[i]
            ofromqry <- paste(pfromqry, "JOIN", layer, "o on(o.PLT_CN=p.CN)")
             other.qry <- paste("select o.* from", ofromqry, "where", stfilter,
			" and p.[", puniqueid, "] = ?")
            rs <- DBI::dbSendQuery(conn, other.qry, params = list(xyx$PLT_CN))
            assign(paste0(layer, "x"), rbind(paste0(layer, "x"), DBI::dbFetch(rs)))
            othertabnms <- c(othertabnms, layer)
            DBI::dbClearResult(rs)
          }
        } 
      }
      #tabs2save <- c(tabs2save, "pltx", "condx") 
      #if (istree) tabs2save <- c(tabs2save, "treex")
      #if (!is.null(other_layers)) 
      #tabs2save <- c(tabs2save, paste0(other_layers, "x")) 
    } 

    ## Disconnect database
    if (datsource == "sqlite")
      DBI::dbDisconnect(conn)
  }

  #############################################################################
  ## Save tables
  #############################################################################
  if (savedata) {
    for (tab in tabs2save) {
      spExportSpatial(get(tab), out_fmt=out_fmt, outfolder=outfolder, out_dsn=out_dsn,
		out_layer=tab, overwrite_layer=overwrite_layer)
    }
  }  
  if (savexy)
    spExportSpatial(xyx, out_fmt=out_fmt, outfolder=outfolder, out_dsn=out_dsn,
		out_layer="xyplt", overwrite_layer=overwrite_layer)

  returnlst <- lapply(tabs2save, get)
  names(returnlst) <- tabs2save
  if (savexy) 
    returnlst$xyplt <- xyplt
  
  returnlst$puniqueid <- puniqueid
  returnlst$pjoinid <- pjoinid
  return(returnlst)
}
