spGetPlots <- function(bnd=NULL, bnd_dsn=NULL, bnd.filter=NULL, states=NULL, 
	stbnd=NULL, stbnd_dsn=NULL, stbnd.att="COUNTYFIPS", RS=NULL, xy=NULL, 
	xy_dsn=NULL, xy.uniqueid="PLT_CN", xvar="LON_PUBLIC", yvar="LAT_PUBLIC", 
	xy.crs=4269, xy.joinid="PLT_CN", clipxy=TRUE, datsource="datamart", 
	data_dsn=NULL, istree=FALSE, isseed=FALSE, plot_layer="plot", cond_layer="cond", 
	tree_layer="tree", seed_layer="seed", ppsa_layer="pop_plot_stratum_assgn", 	
	other_layers=NULL, puniqueid="CN", savePOP=FALSE, evalid=NULL, evalCur=FALSE, 
	evalEndyr=NULL, evalType="VOL", measCur=FALSE, measEndyr=NULL, 
	measEndyr.filter=NULL, invyrs=NULL, allyrs=FALSE, intensity1=FALSE, 
	showsteps=FALSE, savedata=FALSE, savebnd=FALSE, savexy=TRUE, 
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
  xydat=stateFilter=statecnty=xypltx=tabs2save=evalidst=PLOT_ID=INVYR=othertabnms <- NULL
  cuniqueid=tuniqueid <- "PLT_CN"
  returnlst <- list()
  #clipdat <- list()

  ##################################################################
  ## CHECK INPUT PARAMETERS
  ##################################################################
  gui <- FALSE
  coordtype <- "public"

  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- c(names(formals(spGetPlots)), 
		names(formals(FIESTA::spMakeSpatialPoints)))
  if (!all(input.params %in% formallst)) {
    miss <- input.params[!input.params %in% formallst]
    stop("invalid parameter: ", toString(miss))
  }

  ## Define list of pop_tables (without PLT_CN)
  pop_tables <- c("POP_ESTN_UNIT", "POP_EVAL", "POP_EVAL_ATTRIBUTE", "POP_EVAL_GRP", 
	"POP_EVAL_TYP", "POP_STRATUM", "SURVEY") 

  #############################################################################
  ## Import boundary
  #############################################################################
  bndx <- pcheck.spatial(layer=bnd, dsn=bnd_dsn, caption="boundary")
 
  if (!is.null(bndx)) {
    ## bnd.filter
    bndx <- datFilter(bndx, xfilter=bnd.filter, stopifnull=TRUE)$xf
  } else {
    clipxy <- FALSE
  }

  #############################################################################
  ## Set datsource
  ########################################################
  datsourcelst <- c("obj", "csv", "datamart", "sqlite")
  datsource <- FIESTA::pcheck.varchar(var2check=datsource, varnm="datsource", 
		checklst=datsourcelst, gui=gui, caption="Data source?") 
  if (datsource == "sqlite") {
    if (!all(c("RSQLite", "DBI") %in% rownames(installed.packages()))) {
	 stop("RSQLite and DBI packages are required to run SQLite queries")
    }
  } 
  if (datsource %in% c("sqlite", "gdb")) {
    if (is.null(data_dsn)) stop("data_dsn is NULL")
    if (!file.exists(data_dsn)) stop(data_dsn, " is invalid")
  }  
   
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
    outlst <- pcheck.output(out_dsn=out_dsn, out_fmt=out_fmt, 
		outfolder=outfolder, outfn.pre=outfn.pre, outfn.date=outfn.date, 
		overwrite_dsn=overwrite_dsn, gui=gui)
    out_dsn <- outlst$out_dsn
    outfolder <- outlst$outfolder
    out_fmt <- outlst$out_fmt
  }

  ########################################################################
  ### DO THE WORK
  ########################################################################
  ## Check xy table
  xychk <- pcheck.spatial(xy, xy_dsn, checkonly=TRUE)
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
    ## Get stbnd.att
    if (is.null(stbnd.att) && exists("stunitco")) {
      stbnd.att <- "COUNTYFIPS"
    }

    ## Get intersecting states
    statedat <- spGetStates(bndx, stbnd=stbnd, stbnd_dsn=stbnd_dsn, 
			stbnd.att=stbnd.att, RS=RS, states=states, showsteps=showsteps, 
			savebnd=savebnd, outfolder=outfolder, ...)
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
      if (!is.null(treex)) {
        tabs2save <- c(tabs2save, "treex")
      }
    }
    ## seed data
    if (isseed) {
      seedx <- pcheck.table(seed_layer, obj=obj, stopifnull=TRUE)
      if (!is.null(seedx)) {
        tabs2save <- c(tabs2save, "seedx")
      }
    }
    ## pop_plot_stratam_assgn data
    if (savePOP) {
      pop_plot_stratum_assgnx <- pcheck.table(ppsa_layer, obj=obj, stopifnull=TRUE)
      if (!is.null(pop_plot_stratum_assgnx)) {
        tabs2save <- c(tabs2save, "pop_plot_stratum_assgnx")
      }
    }
    ## other data
    if (!is.null(other_layers)) {
      for (layer in other_layers) {
        assign(paste0(layer, "x"), pcheck.table(layer, obj=obj, stopifnull=TRUE))
        tabs2save <- c(tabs2save, layer)
      }
    }

    if (clipxy) {
      if (!xychk) {
        stop("must include xy to clip plots")
      }

      clipdat <- spClipPoint(xyplt=xy, xy.uniqueid=xy.joinid, 
			xvar=xvar, yvar=yvar, xy.crs=xy.crs, addxy=TRUE, clippolyv=bndx)
      xypltx <- clipdat$clip_xyplt 
      #bndx <- rbind(bndx, clipdat$clip_polyv)
      if (length(xyplt) == 0) stop("xy does not overlap bndx")

      if (showsteps) {
        plot(st_geometry(clipdat$clip_polyv), border="black")
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
      if (istree) {
        treex <- treex[treex[[tuniqueid]] %in% clipids,]
      }
      ## other data
      if (!is.null(other_layers)) {
        for (layer in other_layers) {
          if (is.null(pcheck.varchar(layer, checklst=pop_tables, stopifinvalid=FALSE))) {
            assign(paste0(layer, "x"), get(layer)[get(layer)[["PLT_CN"]] %in% clipids, ])
          }
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
    if (isseed) {
      seedx <- {} 
      tabs2save <- c(tabs2save, "seedx")
    }
    if (!is.null(other_layers)) {
      for (layer in other_layers) {
        assign(paste0(layer, "x"), {})
        tabs2save <- c(tabs2save, paste0(layer, "x"))
      }
    }

    msg <- "getting data for..."
    if (!is.null(evalid)) {
      evalid <- unlist(evalid)
      evalidst <- evalid[unique(as.numeric(substr(evalid, nchar(evalid)-6, 
					nchar(evalid)-4))) == stcd]
      msg <- paste0(msg, "for evaluation: ", toString(evalidst))
      savePOP <- TRUE
    } else if (allyrs) {
      msg <- paste0(msg, "for all years")
    } else if (measCur) {
      msg <- paste0(msg, "for most currently measured plots")
      if (!is.null(measEndyr)) {
        msg <- paste0(msg, ", from year ", measEndyr, " or before")
        if (!is.null(measEndyr.filter)) {
          msg <- paste0(msg, ", ", measEndyr.filter)
        }
      }
    } else if (evalCur) {
      msg <- paste0(msg, "for most current evaluation")
      if (!is.null(evalEndyr)) {
        msg <- paste0("ending in ", evalEndyr)
      }
      savePOP <- TRUE
    } else if (!is.null(invyrs)) {
      msg <- paste0(msg, "for inventory years ", min(invyrs), " to ", max(invyrs))
    } else {
      msg <- "using all plots in database"
      allyrs <- TRUE
    }
    message(paste(msg, "\n"))
    if (savePOP) {
      pop_plot_stratum_assgnx <- {} 
      tabs2save <- c(tabs2save, "pop_plot_stratum_assgnx")
    }


    for (i in 1:length(stcds)) { 
      stcd <- stcds[i]
      state <- pcheck.states(stcd) 
      message(paste0("\n", state, "..."))

      ## Check for counties
      if (!is.null(stbnd.att) && stbnd.att == "COUNTYFIPS" && !is.null(statecnty)) {
        stcnty <- statecnty[startsWith(statecnty, formatC(stcd, width=2, flag="0"))]
        countycds <- sort(as.numeric(unique(substr(stcnty, 3, 5))))
        stateFilter <- paste("p.countycd IN(", toString(countycds), ")")
      }

      if (datsource == "datamart") {
        xystate <- NULL
        stabbr <- pcheck.states(stcd, statereturn="ABBR") 

        ## Get plot data
        ###############################
        if (measCur && !is.null(measEndyr) && !is.null(measEndyr.filter)) {
          if (is.null(bndx)) {
            stop("no boundary to apply measEndyr.filter")
          } else {
            clipxy <- TRUE
          }
          dat <- DBgetPlots(states=stcd, datsource="datamart", stateFilter=stateFilter, 
			allyrs=TRUE, istree=istree, isseed=isseed, othertables=other_layers, 
			intensity1=intensity1, savePOP=savePOP)
        } else {
          dat <- DBgetPlots(states=stcd, datsource="datamart", stateFilter=stateFilter, 
			allyrs=allyrs, evalid=evalid, evalCur=evalCur, evalEndyr=evalEndyr, 
			evalType=evalType, measCur=measCur, measEndyr=measEndyr, invyrs=invyrs, 
			istree=istree, isseed=isseed, othertables=other_layers, 
			intensity1=intensity1, savePOP=savePOP)
        }
        PLOT <- dat$plt
        cond <- dat$cond
        if (istree) 
          tree <- dat$tree
        if (isseed)
          seed <- dat$seed
        if (savePOP) {
          pop_plot_stratum_assgn <- dat[[chkdbtab(names(dat), "POP_PLOT_STRATUM_ASSGN")]]
        }
        puniqueid <- "CN"

        if (!is.null(other_layers)) {
          for (layer in other_layers) {
            assign(layer, dat[[chkdbtab(names(dat), layer)]])
          }
        }
        if (!xychk) { 
          if ("xyCur_PUBLIC" %in% names(dat)) {
            xy_PUBLIC <- setDT(dat$xyCur_PUBLIC)
            xy.joinid=pjoinid <- "PLOT_ID"
          } else {
            xy_PUBLIC <- setDT(dat$xy_PUBLIC)
            xy.joinid <- "PLT_CN"
            pjoinid <- puniqueid
          }
          xvar <- "LON_PUBLIC"
          yvar <- "LAT_PUBLIC"
          xystate <- xy_PUBLIC[, c(xy.joinid, xvar, yvar), with=FALSE]
        } else {
          xydat <- pcheck.spatial(xy, xy_dsn)
          pjoinid <- ifelse (xy.joinid %in% names(PLOT), xy.joinid, puniqueid)
          xystate <- xydat[xydat[[xy.joinid]] %in% PLOT[[pjoinid]],]
        }
 
        if (clipxy) {  ## datsource="datamart"

          ## Get most current plots in database for !measEndyr.filter
          #######################################################
          if (measCur && !is.null(measEndyr) && !is.null(measEndyr.filter)) {
            bndxf1 <- datFilter(bndx, xfilter=measEndyr.filter)$xf
            bndxf2 <- datFilter(bndx, xfilter=paste0("!", measEndyr.filter))$xf

            ####################################################
            ## Clip xystate and other tables for bndxf1
            ####################################################
            ## ## Query plots - measCur=TRUE and measEndyr
            pfromqry <- getpfromqry(plotCur=TRUE, Endyr=measEndyr, 
				syntax="R", plotnm="PLOT")
            plt.qry <- paste0("select distinct p.* from ", pfromqry) 
            PLOT1 <- setDT(sqldf::sqldf(plt.qry))

            ## If duplicate plots, sort descending based on INVYR or CN and select 1st row
            if (nrow(PLOT1) > length(unique(PLOT1[[puniqueid]]))) {
              if ("INVYR" %in% names(PLOT1)) {
                setorder(PLOT1, -INVYR)
              } else {
                setorderv(PLOT1, -puniqueid)
              }
              PLOT1 <- PLOT1[, head(.SD, 1), by=pjoinid]
            }

            clipdat <- spClipPoint(xyplt=xystate, 
				xy.uniqueid=xy.joinid, xvar=xvar, yvar=yvar, xy.crs=xy.crs, 
				clippolyv=bndxf1, stopifnotin=FALSE)
            xyplt1 <- clipdat$clip_xyplt
            plt1 <- PLOT1[PLOT1[[pjoinid]] %in% xyplt1[[xy.joinid]], ]
            pltids1 <- plt1[[puniqueid]]

            cond1 <- cond[cond[["PLT_CN"]] %in% pltids1, ]
            if (istree)
              tree1 <- tree[tree[["PLT_CN"]] %in% pltids1, ]
            if (isseed)
              seed1 <- seed[seed[["PLT_CN"]] %in% pltids1, ]
            if (savePOP) {
              pop_plot_stratum_assgn1 <- 
			pop_plot_stratum_assgn[pop_plot_stratum_assgn[["PLT_CN"]] %in% pltids1, ]
            }
            if (!is.null(other_layers)) {
              for (layer in other_layers) {
                if (is.null(pcheck.varchar(layer, checklst=pop_tables, stopifinvalid=FALSE))) {
                  assign(paste0(layer, "2"), 
				get(layer)[get(layer)[["PLT_CN"]] %in% pltids1, ])
                }
              }
            }

            ####################################################
            ## Clip xystate and other tables for bndxf2
            ####################################################

            ## ## Query plots - measCur=TRUE
            pfromqry <- getpfromqry(plotCur=TRUE, syntax="R", plotnm="PLOT")
            plt.qry <- paste0("select distinct p.* from ", pfromqry) 
            PLOT2 <- setDT(sqldf::sqldf(plt.qry))

            ## If duplicate plots, sort descending based on INVYR or CN and select 1st row
            if (nrow(PLOT2) > length(unique(PLOT2[[puniqueid]]))) {
              if ("INVYR" %in% names(PLOT2)) {
                setorder(PLOT2, -INVYR)
              } else {
                setorderv(PLOT2, -puniqueid)
              }
              PLOT2 <- PLOT2[, head(.SD, 1), by=pjoinid]
            }         
            clipdat <- spClipPoint(xyplt=xystate, 
				xy.uniqueid=xy.joinid, xvar=xvar, yvar=yvar, xy.crs=xy.crs, 
				clippolyv=bndxf2, stopifnotin=FALSE)
            xyplt2 <- clipdat$clip_xyplt

            plt2 <- PLOT2[PLOT2[[pjoinid]] %in% xyplt2[[xy.joinid]], ]
            pltids2 <- plt2[[puniqueid]]

            cond2 <- cond[cond[["PLT_CN"]] %in% pltids2, ]
            if (istree)
              tree2 <- tree[tree[["PLT_CN"]] %in% pltids2, ]
            if (isseed)
              seed2 <- seed[seed[["PLT_CN"]] %in% pltids2, ]
            if (savePOP) {
              pop_plot_stratum_assgn2 <- 
			pop_plot_stratum_assgn[pop_plot_stratum_assgn[["PLT_CN"]] %in% pltids2, ]
            }
            if (!is.null(other_layers)) {
              for (layer in other_layers) {
                if (is.null(pcheck.varchar(layer, checklst=pop_tables, stopifinvalid=FALSE))) {
                  assign(paste0(layer, "2"), 
				get(layer)[get(layer)[["PLT_CN"]] %in% pltids2, ])
                }
              }
            }

            plt <- rbind(plt1, plt2)
            cond <- rbind(cond1, cond2)
            if (istree)
              tree <- rbind(tree1, tree2)
            if (isseed)
              seed <- rbind(seed1, seed2)
            if (savePOP) {
              pop_plot_stratum_assgn <- rbind(pop_plot_stratum_assgn1, pop_plot_stratum_assgn2)
            }
            if (!is.null(other_layers)) {
              for (i in 1:length(other_layers)) {
                layer <- other_layers[i]
                if (is.null(pcheck.varchar(layer, checklst=pop_tables, stopifinvalid=FALSE))) {
                  assign(paste0(layer), rbind(paste0(layer, "1"), paste0(layer, "2")))
                }
              }
            }
            if (savexy || showsteps) {
              xyplt <- rbind(xyplt1, xyplt2)
            }
          } else {    ## measEndyr.filter = NULL

            ## Clip data
            clipdat <- spClipPoint(xyplt=xystate, 
				xy.uniqueid=xy.joinid, xvar=xvar, yvar=yvar, xy.crs=xy.crs, 
				clippolyv=bndx)
            xyplt <- clipdat$clip_xyplt

            plt <- PLOT[PLOT[[pjoinid]] %in% xyplt[[xy.joinid]], ]
            pltids <- plt[[puniqueid]]

            cond <- cond[cond[["PLT_CN"]] %in% pltids, ]
            if (istree)
              tree <- tree[tree[["PLT_CN"]] %in% pltids, ]
            if (isseed)
              seed <- seed[seed[["PLT_CN"]] %in% pltids, ]
            if (!is.null(other_layers)) {
              for (layer in other_layers) {
                if (is.null(pcheck.varchar(layer, checklst=pop_tables, stopifinvalid=FALSE))) {
                  assign(layer, 
				get(layer)[get(layer)[["PLT_CN"]] %in% pltids, ])
                }
              }
            }
          }  ## if measEndyr.filter is not NULL

          pltx <- rbind(pltx, plt)
          condx <- rbind(condx, cond)
          if (istree)
            treex <- rbind(treex, tree)
          if (isseed)
            seedx <- rbind(seedx, seed)
          if (savePOP) {
            pop_plot_stratum_assgnx <- rbind(pop_plot_stratum_assgnx, pop_plot_stratum_assgn)
          }
          if (!is.null(other_layers)) {
            for (i in 1:length(other_layers)) {
              layer <- other_layers[i]
              assign(paste0(layer, "x"), rbind(paste0(layer, "x"), layer))
            }
          }

          if (savexy || showsteps) {
            xypltx <- rbind(xypltx, xyplt)
          }

        } else {      ## clipxy = FALSE, datsource="datamart"

          if ((savexy || showsteps) && !is.null(xydat)) {
            xypltx <- xydat
          }

          pltx <- rbind(pltx, PLOT)
          condx <- rbind(condx, cond)
          if (istree) {
            treex <- rbind(treex, tree)
          }
          if (isseed) {
            seedx <- rbind(seedx, seed)
          }
          if (savePOP) {
            pop_plot_stratum_assgnx <- rbind(pop_plot_stratum_assgnx, pop_plot_stratum_assgn)
          }       
        }   ## clipxy
        rm(PLOT)
        rm(cond)
        if (istree) rm(tree)
        if (isseed) rm(seed)
        if (savePOP) rm(pop_plot_stratum_assgn)
        gc()
          
      } else if (datsource == "sqlite") {
        ####################################################################
        ## 1) Check if data for all states is in database
        ## 1) Get most current plots from xy database that intersect state
        ## 2) Clip xy (for state) to boundary
        ## 3) Subset other data with clipped xy joinid
        ####################################################################
        if (i == 1) {
          xyindb <- FALSE
          dbconn <- DBtestSQLite(data_dsn, dbconnopen=TRUE, showlist=FALSE)
          tablst <- DBI::dbListTables(dbconn)
          plot_layer <- chkdbtab(tablst, plot_layer, stopifnull=TRUE)
          cond_layer <- chkdbtab(tablst, cond_layer, stopifnull=TRUE)
          if (istree) {
            tree_layer <- chkdbtab(tablst, tree_layer, stopifnull=TRUE)
          }
          if (isseed) {
            seedchk <- chkdbtab(tablst, seed_layer)
            if (is.null(seedchk) && seed_layer == "seed") {
              seedchk <- chkdbtab(tablst, "seedling")
              if (is.null(seedchk) && seed_layer == "seedling") {
                seed_layer <- chkdbtab(tablst, "seedling", stopifnull=TRUE)
              } else {
                seed_layer <- seedchk
              }
            } else {
              seed_layer <- seedchk
            }
          }
          if (savePOP) {
            ppsa_layer <- chkdbtab(tablst, ppsa_layer, stopifnull=TRUE)
          }       
          if (!is.null(other_layers) && !any(other_layers %in% tablst)) {
            stop("missing layers in database: ", 
			toString(other_layers[!other_layers %in% tablst]))
          }

          ## Check list of pop_tables
          pop_tables <- unlist(sapply(pop_tables, pcheck.varchar, 
			checklst=tablst, stopifinvalid=FALSE))

          ## Check for state in database
          dbstcds <- DBI::dbGetQuery(dbconn, paste("select distinct statecd from", 
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
          #if (clipxy) {
            if ((xychk && xy_dsn == data_dsn) || !xychk) {
              xyconn <- dbconn
              xyindb <- TRUE
              xy_dsn <- data_dsn
            } else {
              xyconn <- DBtestSQLite(xy_dsn, dbconnopen=TRUE, showlist=FALSE,
					createnew=FALSE)
            }
            if (is.null(xyconn)) {
              xyconn <- dbconn
            }
            ## Check xy data
            ######################################################################
            if (is.null(xy)) {
              xytabs <- DBI::dbListTables(xyconn)
              xytabs <- xytabs[grepl("xy", xytabs)]
              if (length(xytabs) == 0) {
                stop("no xy in ", xy_dsn)
              }
              if (any(grepl("ACTUAL", xytabs))) {
                xy <- xytabs[grepl("ACTUAL", xytabs)]
                message("xy is NULL...  using ", xy)
              } else if (any(grepl("PUBLIC", xytabs))) {
                xy <- xytabs[grepl("PUBLIC", xytabs)]
                message("xy is NULL...  using ", xy)
              } else if (any(grepl("plot", xytabs, ignore.case=TRUE))) {
                message("xy is NULL...  using plot table")
                if (length(grepl("plot", xytabs, ignore.case=TRUE)) == 1) {
                  ptab <- xytabs[grepl("plot", xytabs, ignore.case=TRUE)]
                  pfields <- DBI::dbListFields(xyconn, ptab)
                  if ("LON_PUBLIC" %in% pfields) {
                    xvar <- "LON_PUBLIC"
                    if ("LAT_PUBLIC" %in% pfields) {
                      yvar <- "LAT_PUBLIC"
                    }
                  }
                }
              } else {
                stop(xy, " not in ", xy_dsn) 
              }
            } else {
              xyindb <- TRUE
            }
            xyfields <- DBI::dbListFields(xyconn, xy)
            if (grepl("ACTUAL", xy)) {
              xvar <- "LON_ACTUAL"
              yvar <- "LAT_ACTUAL"
            } else if (grepl("PUBLIC", xy)) {
              xvar <- "LON_PUBLIC"
              yvar <- "LAT_PUBLIC"
            }
            if (!xvar %in% xyfields) {
              message(xvar, " not in xy fields: ", toString(xyfields))
            }
            if (!yvar %in% xyfields) {
              message(yvar, " not in xy fields: ", toString(xyfields))
            } 
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
            pltfields <- DBI::dbListFields(dbconn, "plot")

            ## Not sure about following code.
            ## Checks for PLOT_ID in both xy and plot data. 
            if (grepl("xyCur", xy)) {
              if (xy.joinid %in% c("CN", "PLT_CN") && "PLOT_ID" %in% xyfields && 
				"PLOT_ID" %in% pltfields) {
                 message("changing xy.joinid from ", xy.joinid, " to PLOT_ID")
                 xy.joinid <- "PLOT_ID"
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
          #}
        }    ## End if i=1

        ## Create state filter
        stfilter <- paste("p.statecd IN(", toString(stcd), ")")
        if (!is.null(stateFilter)) { 
          stfilter <- paste(stfilter, "and", stateFilter)
        }
 
        ## Get most current evalid
        if (evalCur) {
          ppsa_layer<- chkdbtab(tablst, ppsa_layer, stopifnull=TRUE)
          evalidlst <- DBgetEvalid(states=stcd, datsource="sqlite", 
			data_dsn=data_dsn, evalid=evalid, evalEndyr=evalEndyr,
			evalCur=evalCur, evalType=evalType, ppsanm=ppsa_layer)
          evalidst <- unlist(evalidlst$evalidlist)
        }
 
        ## Get evalid filter
        if (!is.null(evalidst)) {
          stfilter <- paste0("ppsa.evalid IN(", toString(evalidst), ")")
        } else {
          if (intensity1) {
            stfilter <- paste(stfilter, "p.intensity == 1", sep=" and ")
          }
        }

        ## Print stfilter
        message(stfilter)

        ## get pfromqry
        pfromqry <- getpfromqry(dsn=data_dsn, evalid=evalidst, plotCur=measCur, 
				Endyr=measEndyr, invyrs=invyrs, allyrs=allyrs, 
				intensity1=intensity1, syntax="R", plotnm=plot_layer, 
				ppsanm=ppsa_layer)
        if (is.null(pfromqry)) {
          message("no time frame specified... including all years")
          allyrs <- TRUE
          pfromqry <- getpfromqry(dsn=data_dsn, evalid=evalidst, plotCur=measCur, 
				Endyr=measEndyr, invyrs=invyrs, allyrs=allyrs, 
				intensity1=intensity1, syntax="R", plotnm=plot_layer, 
				ppsanm=ppsa_layer, chk=TRUE)
        }

        ## Set up query for plots
        plt.qry <- paste0("select distinct p.* from ", pfromqry, " where ", stfilter) 

        ## Query database for plots
        rs <- DBI::dbSendQuery(dbconn, plt.qry)
        plt <- setDT(DBI::dbFetch(rs))
        DBI::dbClearResult(rs)

        zids <- c("STATECD", "UNITCD", "COUNTYCD", "PLOT")
        if (!"PLOT_ID" %in% names(plt)) {
          if (!all(zids %in% names(plt))) {
            message("cannot create unique identifier for plot")
            message(toString(zids[which(!zids %in% names(plt))]), " not in plt")
          } else {
            plt[, PLOT_ID := paste0("ID", 
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

        if (!is.null(measEndyr.filter)) {
          if (is.null(bndx)) {
            stop("no boundary to apply measEndyr.filter")
          } else {
            clipxy <- TRUE
          }
        }

        ## Generate xy table for all plots in state (xystate)
        #########################################################
        if (xyindb) { 
          #xy.joinid <- pjoinid
          xyvars <- c(xy.joinid, xvar, yvar)
          if (xy == "plot") {
            xy.qry <- paste0("select distinct ", toString(xyvars), " from ", 
				pfromqry, " where ", stfilter) 
          } else { 
            if (allyrs || measCur) measCur.xy <- TRUE
            xyfromqry <- getpfromqry(evalid=evalidst, plotCur=measCur.xy, 
				invyrs=invyrs, allyrs=allyrs, intensity1=intensity1, 
				syntax="R", plotnm=plot_layer, ppsanm=ppsa_layer)
            xy.qry <- paste0("select distinct ", toString(paste0("xy.", xyvars)), 
				" from ", xyfromqry, " JOIN ", xy, " xy ON (p.", pjoinid, 
				" = xy.", xy.joinid, ")", " where ", stfilter) 
          }
          xystate <- DBI::dbGetQuery(xyconn, xy.qry) 
          if (nrow(xystate) == 0) break
        } else if (xychk) {
          xydat <- pcheck.spatial(layer=xy, dsn=xy_dsn)
          xyvars <- c(xy.joinid, xvar, yvar)
          xystate <- xydat[, xyvars, with=FALSE]
        } else if (clipxy) { 
          stop("must include xy data")
        } 

        ## Convert xystate to sf class
        if (!"sf" %in% class(xystate)) { 
          xystate <- spMakeSpatialPoints(xystate, xy.uniqueid=xy.joinid, 
			xvar=xvar, yvar=yvar, xy.crs=xy.crs)
        }
 
        if (clipxy) {    ## datsource="sqlite"

          ## Get most current plots in database for measEndyr.filter & !measEndyr.filter
          #######################################################################
          #p2fromqry <- paste(plot_layer, "p")
          p2fromqry <- pfromqry
          if (!is.null(measEndyr.filter)) {
            bndxf1 <- datFilter(bndx, xfilter=measEndyr.filter)$xf
            bndxf2 <- datFilter(bndx, xfilter=paste0("!", measEndyr.filter))$xf
            ## Clip xystate and other tables for bndxf1
            ############################################
            clipdat <- spClipPoint(xyplt=xystate, clippolyv=bndxf1, stopifnotin=FALSE)
            xyplt1 <- clipdat$clip_xyplt
            plt1 <- plt[plt[[pjoinid]] %in% xyplt1[[xy.joinid]], ]
            pltids1 <- plt1[[puniqueid]]

            cond1.qry <- paste0("select cond.* from ", p2fromqry,
			" join cond on(cond.PLT_CN = p.CN) where ", 
				"p.", puniqueid, " in(", addcommas(pltids1, quotes=TRUE), ")")
            rs <- DBI::dbSendQuery(dbconn, cond1.qry)
            cond1 <- DBI::dbFetch(rs)
            DBI::dbClearResult(rs)

            if (istree) {
              tree1.qry <- paste0("select tree.* from ", p2fromqry,
			" join tree on(tree.PLT_CN = p.CN) where ", stfilter, 
			" and p.", puniqueid, " in(", addcommas(pltids1, quotes=TRUE), ")")
              rs <- DBI::dbSendQuery(dbconn, tree1.qry)
              tree1 <- DBI::dbFetch(rs)
              DBI::dbClearResult(rs)
            }
            if (isseed) {
              seed1.qry <- paste0("select seed.* from ", p2fromqry,
			" join ", seed_layer, " on(seed.PLT_CN = p.CN) where ", stfilter, 
			" and p.", puniqueid, " in(", addcommas(pltids1, quotes=TRUE), ")")
              rs <- DBI::dbSendQuery(dbconn, seed1.qry)
              seed1 <- DBI::dbFetch(rs)
              DBI::dbClearResult(rs)
            }
            if (savePOP) {
              ppsa1.qry <- paste0("select ppsa.* from ", p2fromqry,
			" where ", stfilter, 
			" and p.", puniqueid, " in(", addcommas(pltids1, quotes=TRUE), ")")
              rs <- DBI::dbSendQuery(dbconn, ppsa1.qry)
              pop_plot_stratum_assgn1 <- DBI::dbFetch(rs)
              DBI::dbClearResult(rs)
            }       
            if (!is.null(other_layers)) {
              for (i in 1:length(other_layers)) {
                layer <- other_layers[i]
                if (!is.null(pcheck.varchar(layer, checklst=pop_tables, stopifinvalid=FALSE))) {
                  if (!is.null(evalidst)) {
                    other.qry <- paste0("select * from ", layer, " ppsa where ", stfilter)
                  } else {
                    other.qry <- paste0("select * from ", layer, " p where ", stfilter)
                  }
                } else {
                  ofromqry <- paste(p2fromqry, "JOIN", layer, "o on(o.PLT_CN=p.CN)")
                  other.qry <- paste("select o.* from", ofromqry, "where", stfilter,
				" and p.", puniqueid, " in(", addcommas(pltids1, quotes=TRUE), ")")
                }
                rs <- DBI::dbSendQuery(dbconn, other.qry)
                assign(paste0(layer, "1"), DBI::dbFetch(rs))
                othertabnms <- c(othertabnms, layer)
                DBI::dbClearResult(rs)
              }
            } 
            ## Clip plots from all database
            ############################################
            ## Get most current evalid
            if (evalCur) {
              evalidst <- getEvalid.ppsa(ppsa=ppsa_layer, states=stcd, 
				evalCur=evalCur, evalType=evalType)
            }
            ## Get evalid filter
            if (!is.null(evalidst)) {
              stfilter <- paste0("ppsa.evalid IN(", toString(evalidst), ")")
            } else {
              if (intensity1)
                stfilter <- paste(stfilter, "p.intensity == 1", sep=" and ")
            }
            ## get pfromqry
            pfromqry2 <- getpfromqry(evalid=evalidst, plotCur=measCur, 
				invyrs=invyrs, allyrs=allyrs, intensity1=intensity1, 
				syntax="R", plotnm=plot_layer, ppsanm=ppsa_layer)
            ## Set up query for plots
            plt2.qry <- paste0("select distinct p.* from ", pfromqry2, " where ", stfilter) 

            ## Query database for plots
            rs <- DBI::dbSendQuery(dbconn, plt2.qry)
            plt2 <- setDT(DBI::dbFetch(rs))
            DBI::dbClearResult(rs)

            if (!"PLOT_ID" %in% names(plt2)) {
              if (all(zids %in% names(plt2))) {
                plt2[, PLOT_ID := paste0("ID", 
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
            clipdat <- spClipPoint(xyplt=xystate, clippolyv=bndxf2, xy.uniqueid=xy.uniqueid,
			stopifnotin=FALSE)
            xyplt2 <- clipdat$clip_xyplt

            plt2 <- plt2[plt2[[pjoinid]] %in% xyplt2[[xy.joinid]], ]
            pltids2 <- plt2[[puniqueid]]

            cond2.qry <- paste0("select cond.* from ", p2fromqry,
			" join cond on(cond.PLT_CN = p.CN) where ", stfilter, 
				" and p.", puniqueid, " in(", addcommas(pltids2, quotes=TRUE), ")")
            rs <- DBI::dbSendQuery(dbconn, cond2.qry)
            cond2 <- DBI::dbFetch(rs)
            DBI::dbClearResult(rs)

            if (istree) {
              tree2.qry <- paste0("select tree.* from ", p2fromqry,
			" join tree on(tree.PLT_CN = p.CN) where ", stfilter, 
			" and p.", puniqueid, " in(", addcommas(pltids2, quotes=TRUE), ")")
              rs <- DBI::dbSendQuery(dbconn, tree2.qry)
              tree2 <- DBI::dbFetch(rs)
              DBI::dbClearResult(rs)
            }
            if (isseed) {
              seed2.qry <- paste0("select seed.* from ", p2fromqry,
			" join ", seed_layer, " on(seed.PLT_CN = p.CN) where ", stfilter, 
			" and p.", puniqueid, " in(", addcommas(pltids2, quotes=TRUE), ")")
              rs <- DBI::dbSendQuery(dbconn, seed2.qry)
              seed2 <- DBI::dbFetch(rs)
              DBI::dbClearResult(rs)
            }
            if (savePOP) {
              ppsa2.qry <- paste0("select ppsa.* from ", p2fromqry,
			" where ", stfilter, 
			" and p.", puniqueid, " in(", addcommas(pltids2, quotes=TRUE), ")")
              rs <- DBI::dbSendQuery(dbconn, ppsa2.qry)
              pop_plot_stratum_assgn2 <- DBI::dbFetch(rs)
              DBI::dbClearResult(rs)
            }       

            if (!is.null(other_layers)) {
              for (i in 1:length(other_layers)) {
                layer <- other_layers[i]
                if (!is.null(pcheck.varchar(layer, checklst=pop_tables, stopifinvalid=FALSE))) {
                  if (!is.null(evalidst)) {
                    other.qry <- paste0("select * from ", layer, " ppsa where ", stfilter)
                  } else {
                    other.qry <- paste0("select * from ", layer, " p where ", stfilter)
                  }
                } else {
                  ofromqry <- paste(p2fromqry, "JOIN", layer, "o on(o.PLT_CN=p.CN)")
                  other.qry <- paste("select o.* from", ofromqry, "where", stfilter,
				" and p.", puniqueid, " in(", addcommas(pltids2, quotes=TRUE), ")")
                }
                rs <- DBI::dbSendQuery(dbconn, other.qry)
                assign(paste0(layer, "2"), DBI::dbFetch(rs))
                othertabnms <- c(othertabnms, layer)
                DBI::dbClearResult(rs)
              }
            } 
          
            plt <- rbind(plt1, plt2)
            cond <- rbind(cond1, cond2)
            if (istree)
              tree <- rbind(tree1, tree2)
            if (isseed)
              seed <- rbind(seed1, seed2)
            if (savePOP) {
              pop_plot_stratum_assgn <- rbind(pop_plot_stratum_assgn1, pop_plot_stratum_assgn2)
            }
            if (!is.null(other_layers)) {
              for (i in 1:length(other_layers)) {
                layer <- other_layers[i]
                assign(paste0(layer), rbind(paste0(layer, "1"), paste0(layer, "2")))
              }
            }
            if (savexy || showsteps) {
              xyplt <- rbind(xyplt1, xyplt2)
            }

          } else {    ## measEndyr.filter = NULL
            ## Clip data
            clipdat <- spClipPoint(xyplt=xystate, clippolyv=bndx, xy.uniqueid=xy.uniqueid)
            xyplt <- clipdat$clip_xyplt
            plt <- plt[plt[[pjoinid]] %in% xyplt[[xy.joinid]], ]
            pltids <- plt[[puniqueid]]

            cond.qry <- paste0("select cond.* from ", p2fromqry,
			" join cond on(cond.PLT_CN = p.CN) where ", stfilter, 
				" and p.", puniqueid, " in(", addcommas(pltids, quotes=TRUE), ")")
            rs <- DBI::dbSendQuery(dbconn, cond.qry)
            cond <- DBI::dbFetch(rs)
            DBI::dbClearResult(rs)

            if (istree) {
              tree.qry <- paste0("select tree.* from ", p2fromqry,
			" join tree on(tree.PLT_CN = p.CN) where ", stfilter, 
			" and p.", puniqueid, " in(", addcommas(pltids, quotes=TRUE), ")")
              rs <- DBI::dbSendQuery(dbconn, tree.qry)
              tree <- DBI::dbFetch(rs)
              DBI::dbClearResult(rs)
            }
            if (isseed) {
              seed.qry <- paste0("select seed.* from ", p2fromqry,
			" join ", seed_layer, " on(seed.PLT_CN = p.CN) where ", stfilter, 
			" and p.", puniqueid, " in(", addcommas(pltids, quotes=TRUE), ")")
              rs <- DBI::dbSendQuery(dbconn, seed.qry)
              seed <- DBI::dbFetch(rs)
              DBI::dbClearResult(rs)
            }
            if (savePOP) {
              ppsa.qry <- paste0("select ppsa.* from ", p2fromqry,
			" where ", stfilter, 
			" and p.", puniqueid, " in(", addcommas(pltids, quotes=TRUE), ")")
              rs <- DBI::dbSendQuery(dbconn, ppsa.qry)
              pop_plot_stratum_assgn <- DBI::dbFetch(rs)
              DBI::dbClearResult(rs)
            }       

            if (!is.null(other_layers)) {
              for (i in 1:length(other_layers)) {
                layer <- other_layers[i]
                if (!is.null(pcheck.varchar(layer, checklst=pop_tables, stopifinvalid=FALSE))) {
                  if (!is.null(evalidst)) {
                    other.qry <- paste0("select * from ", layer, " ppsa where ", stfilter)
                  } else {
                    other.qry <- paste0("select * from ", layer, " p where ", stfilter)
                  }
                } else {
                  ofromqry <- paste(p2fromqry, "JOIN", layer, "o on(o.PLT_CN=p.CN)")
                  other.qry <- paste("select o.* from", ofromqry, "where", stfilter,
				" and p.", puniqueid, " in(", addcommas(pltids, quotes=TRUE), ")")
                }
                rs <- DBI::dbSendQuery(dbconn, other.qry)
                assign(paste0(layer), DBI::dbFetch(rs))
                othertabnms <- c(othertabnms, layer)
                DBI::dbClearResult(rs)
              }
            } 
          }  ## if measEndyr.filter is not NULL

          pltx <- rbind(pltx, plt)
          condx <- rbind(condx, cond)

          if (istree) {
            treex <- rbind(treex, tree)
          }
          if (isseed) {
            seedx <- rbind(seedx, seed)
          }
          if (savePOP) {
            pop_plot_stratum_assgnx <- rbind(pop_plot_stratum_assgnx, pop_plot_stratum_assgn)
          }
          if (!is.null(other_layers)) {
            for (i in 1:length(other_layers)) {
              layer <- other_layers[i]
              assign(paste0(layer, "x"), rbind(paste0(layer, "x"), layer))
            }
          }
          if (savexy || showsteps) {
            xypltx <- rbind(xypltx, xyplt)
          }
        } else {      ## clipxy=FALSE, datsource="sqlite"

          if ((savexy || showsteps) && !is.null(xystate)) {
            xypltx <- rbind(xypltx, xystate)
          }

          ## Query database for plots
          rs <- DBI::dbSendQuery(dbconn, plt.qry)
          plt <- setDT(DBI::dbFetch(rs))
          DBI::dbClearResult(rs)

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
          rs <- DBI::dbSendQuery(dbconn, cond.qry)
          condx <- rbind(condx, DBI::dbFetch(rs))
          DBI::dbClearResult(rs)

          if (istree) {
            tree.qry <- paste0("select tree.* from ", pfromqry,
			" join tree on(tree.PLT_CN = p.CN) where ", stfilter, 
			" and p.", puniqueid, " in(", addcommas(pltids, quotes=TRUE), ")")
            rs <- DBI::dbSendQuery(dbconn, tree.qry)
            treex <- rbind(treex, DBI::dbFetch(rs))
            DBI::dbClearResult(rs)
          }
          if (isseed) {
            seed.qry <- paste0("select seed.* from ", pfromqry,
			" join ", seed_layer, " on(seed.PLT_CN = p.CN) where ", stfilter, 
			" and p.", puniqueid, " in(", addcommas(pltids, quotes=TRUE), ")")
            rs <- DBI::dbSendQuery(dbconn, seed.qry)
            seedx <- rbind(seedx, DBI::dbFetch(rs))
            DBI::dbClearResult(rs)
          }
          if (savePOP) {
            ppsa.qry <- paste0("select ppsa.* from ", pfromqry,
			" where ", stfilter, 
			" and p.", puniqueid, " in(", addcommas(pltids, quotes=TRUE), ")")
            rs <- DBI::dbSendQuery(dbconn, ppsa.qry)
            pop_plot_stratum_assgnx <- rbind(pop_plot_stratum_assgnx, DBI::dbFetch(rs))
            DBI::dbClearResult(rs)
          }       

          if (!is.null(other_layers)) {
            for (i in 1:length(other_layers)) {
              layer <- other_layers[i]

              if (!is.null(pcheck.varchar(layer, checklst=pop_tables, stopifinvalid=FALSE))) {
                if (!is.null(evalidst)) {
                  other.qry <- paste0("select * from ", layer, " ppsa where ", stfilter)
                } else {
                  other.qry <- paste0("select * from ", layer, " p where ", stfilter)
                }
              } else {
                ofromqry <- paste(pfromqry, "JOIN", layer, "o on(o.PLT_CN=p.CN)")
                other.qry <- paste("select o.* from", ofromqry, "where", stfilter,
				" and p.", puniqueid, " in(", addcommas(pltids, quotes=TRUE), ")")
              }
              rs <- DBI::dbSendQuery(dbconn, other.qry)
              #assign(paste0(layer, "x"), DBI::dbFetch(rs))
              assign(paste0(layer, "x"), rbind(get(paste0(layer, "x")), DBI::dbFetch(rs)))
              othertabnms <- c(othertabnms, layer)
              DBI::dbClearResult(rs)
            }
          } 
        }   ## clipxy
      }  ## datsource
    }  ## End of looping thru states

    ## Disconnect database
    if (datsource == "sqlite") {
      DBI::dbDisconnect(dbconn)
    }
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

    plot(sf::st_geometry(xypltx), col="blue", cex=.5)
    if (!is.null(bndx)) {
      plot(st_geometry(bndx), add=TRUE, border="black", lwd=0.75)
    }
    par(mar=mar)
  }

  if (clipxy) {
    returnlst$clip_tabs <- lapply(tabs2save, get, envir=environment())
    names(returnlst$clip_tabs) <- paste0("clip_", tabs2save)
  } else {
    returnlst$tabs <- lapply(tabs2save, get, envir=environment())
    names(returnlst$tabs) <- tabs2save
  } 

  if (savexy) {
    if (clipxy) {
      returnlst$clip_xyplt <- xypltx
    } else { 
      returnlst$xyplt <- xypltx
    }
  }

  returnlst$clip_polyv <- bndx
  returnlst$puniqueid <- puniqueid
  returnlst$xy.uniqueid <- xy.joinid
  returnlst$pjoinid <- pjoinid
  returnlst$states <- states
  return(returnlst)
}
