DBgetPlots <- function (states=NULL, RS=NULL, invtype="ANNUAL", evalid=NULL, 
	evalCur=FALSE, evalEndyr=NULL, evalType="ALL", measCur=FALSE, measEndyr=NULL, 
	allyrs=FALSE, invyrs=NULL, istree=FALSE, isseed=FALSE, isveg=FALSE, 
	othertables=NULL, issp=FALSE, spcond=FALSE, spcondid1=FALSE, defaultVars=TRUE, 
	regionVars=FALSE, ACI=FALSE, subcycle99=FALSE, intensity1=FALSE, stateFilter=NULL,
	allFilter=NULL, alltFilter=NULL, savedata=FALSE, parameters=FALSE, outfolder=NULL,
 	out_fmt="csv", out_dsn=NULL, append_layer=FALSE, outfn.pre=NULL, outfn.date=FALSE,
 	overwrite=FALSE, savePOP=FALSE) {

  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- ifelse(nargs() == 0, TRUE, FALSE)

  if (gui) 
    invtype=evalCur=allyrs=evalType=istree=isseed=isveg=issp=
	spcondid1=defaultVars=regionVars=ACI=subcycle99=intensity1=
	allFilter=savedata=saveqry=parameters=BIOJENK_kg=BIOJENK_lb=PREV_PLTCN <- NULL

  ## Set global variables  
  CN=CONDID=COND_STATUS_CD=PLT_CN=FORTYPCD=pltvarlst=condvarlst=treevarlst=tsumvarlst=
	seedvarlst=ssumvarlst=vspsppvarlst=vspstrvarlst=dwmlst=filtervarlst=
	SUBPPROP_UNADJ=MICRPROP_UNADJ=TPA_UNADJ=TPAMORT_UNADJ=TPAREMV_UNADJ=SEEDCNT6=
	TREECOUNT_CALC=SEEDSUBP6=LIVE_CANOPY_CVR_PCT=CONDPROP_UNADJ=PLOT_NONSAMPLE_REASN_CD=
	PLOT_STATUS_CD=BA=DIA=CRCOVPCT_RMRS=TIMBERCD=SITECLCD=RESERVCD=JENKINS_TOTAL_B1=
	JENKINS_TOTAL_B2=POP_PLOT_STRATUM_ASSGN=NF_SAMPLING_STATUS_CD=NF_COND_STATUS_CD=
	ACI_NFS=OWNCD=OWNGRPCD=FORNONSAMP=PLOT_ID=sppvarsnew=
	STATECD=UNITCD=COUNTYCD=INVYR <- NULL


  ## SET OPTIONS
  options.old <- options()
  options(scipen=8) # bias against scientific notation
  on.exit(options(options.old), add=TRUE) 

  ## Define functions
  ###########################################################
  getcoords <- function(coords){
    switch(coords,
      ACTUAL = c("LON_ACTUAL", "LAT_ACTUAL"),
      PUBLIC = c("LON_PUBLIC", "LAT_PUBLIC"))
  }  

  ## Define variables
  ZIP <- TRUE
  actual=getinvyr <- FALSE
  SCHEMA <- ""
  SCHEMA. <- ""
  isRMRS <- FALSE
  xycoords = c("LON_PUBLIC", "LAT_PUBLIC")
  coords <- "PUBLIC"
  isdwm <- FALSE
  saveqry <- FALSE


  ## Set maxstates 
  ###########################################################
  ##  The number of states to append together, while still small enough to return 
  ##  as objects (without memory issues). This includes all tables except tree table..  
  ##  If there is more than 1 state with more than 6 inventory years and no filters,  
  ##  the tree table will not be returned as an object.. only written to outfolder.
  maxstates.tree <- ifelse(allyrs && is.null(allFilter), 3, 
						ifelse(!is.null(allFilter), 10, 20))  
  biojenk <- FALSE 
  greenwt <- TRUE
  outSQLite <- FALSE     
  xymeasCur <- FALSE

  ########################################################################
  ### GET PARAMETERS 
  ########################################################################
  iseval <- FALSE
  subsetPOP <- FALSE
  if (!is.null(evalid) || length(evalType)) savePOP=subsetPOP <- TRUE

  ## Get states, Evalid and/or invyrs info
  evalInfo <- DBgetEvalid(states=states, RS=RS, invtype=invtype, 
	evalid=evalid, evalCur=evalCur, evalEndyr=evalEndyr, evalType=evalType, 
	isdwm=isdwm, gui=gui)
  if (is.null(evalInfo)) return(NULL)
  states <- evalInfo$states
  rslst <- evalInfo$rslst
  evalidlist <- evalInfo$evalidlist
  invtype <- evalInfo$invtype
  invyrtab <- evalInfo$invyrtab
  SURVEY <- evalInfo$SURVEY

  if (length(evalidlist) > 0) {
    invyrs <- evalInfo$invyrs
    iseval <- TRUE
    savePOP <- TRUE
  }

  ### GET RS & rscd
  ###########################################################
  isRMRS <- ifelse(length(rslst) == 1 && rslst == "RMRS", TRUE, FALSE) 
     
  ## Get state abbreviations and codes 
  ###########################################################
  stabbrlst <- FIESTA::pcheck.states(states, statereturn="ABBR")
  stcdlst <- FIESTA::pcheck.states(states, statereturn="VALUE")

  ## Get number of states 
  nbrstates <- length(states)

  ## If using EVALID, you don't need to get INVYRS, intensity, or subcycle
  if (!iseval) {   

    ### Check measCur
    ###########################################################
    measCur <- FIESTA::pcheck.logical(measCur, varnm="measCur", title="Current measyear?", 
		first="YES", gui=gui)

    ### Check measEndyr
    ###########################################################
    measEndyr.filter <- NULL
    if (!is.null(measEndyr)) {
      minyr <- min(invyrtab$INVYR)
      if (!is.numeric(measEndyr) || measEndyr < minyr)
        stop("measEndyr must be yyyy format and greater than minimum inventory year: ", minyr)
      measCur <- TRUE
      measEndyr.filter <- paste0(" and MEASYEAR < ", measEndyr)
    }

    if (measCur) {
      xymeasCur <- TRUE
      allyrs <- FALSE
    }

    ### Check allyrs
    ###########################################################
    allyrs <- FIESTA::pcheck.logical(allyrs, varnm="allyrs", title="All years?", 
		first="YES", gui=gui)
    if (allyrs) xymeasCur <- TRUE
 
    ## Check INVYR(S) 
    ###########################################################
    if (!measCur) {
      if ((is.null(invyrs) || length(invyrs) == 0)) {
        invyrs <- sapply(states, function(x) NULL)
        for (state in states) { 
          stabbr <- FIESTA::pcheck.states(state, "ABBR")
          stinvyrlst <- sort(invyrtab[invyrtab$STATENM == state, "INVYR"])

          if (allyrs) {
            invyr <- stinvyrlst
          } else {
            if (!gui) stop("need to specify a timeframe for plot data")

            ## GET INVENTORY YEAR(S) FROM USER
            invyr <- select.list(as.character(stinvyrlst), 
			title=paste("Inventory year(s) -", stabbr), multiple=TRUE)
            if (length(invyr) == 0) stop("")
          }
          invyrs[[state]] <- as.numeric(invyr)
        }
      } else if (!is.null(invyrs)) {
        if (class(invyrs) != "list") {
          if (is.vector(invyrs) && is.numeric(invyrs)) {
            invyrs <- list(invyrs)
            if (length(states) == 1) {
              names(invyrs) <- states
            } else {
              warning("using specified invyrs for all states")
              yrs <- invyrs
              invyrs <- sapply(states, function(x) NULL)
              for (st in states) invyrs[st] <- yrs
            } 
          }
        } else if (length(invyrs) != length(states)) {
          stop("check invyrs list.. does not match number of states")
        }
        ## Check inventory years
        for (state in states) {
          stinvyrlst <- invyrtab[invyrtab$STATENM == state, "INVYR"]
          if (!all(invyrs[[state]] %in% stinvyrlst))
            stop("inventory years do not match database")
        }
      }
    }

    ## Check subcycle99
    ###########################################################
    subcycle99 <- FIESTA::pcheck.logical(subcycle99, varnm="subcycle99", 
		title="Keep SUBCYCLE 99?", first="NO", gui=gui)

    ## Check intensity1
    ###########################################################
    ## For periodic data, the INTENSITY variable does not equal 1
    if (invtype == "ANNUAL") {
      intensity1 <- FIESTA::pcheck.logical(intensity1, varnm="intensity1", 
		title="Intensity = 1?", first="YES", gui=gui)
    } else {
      intensity1 <- FALSE
    }

    ## Check ACI
    ###########################################################
    ACI <- FIESTA::pcheck.logical(ACI, varnm="ACI", 
		title="ACI conditions?", first="NO", gui=gui)

  } else {
    if (!is.null(subcycle99) && subcycle99) 
      message("subcycle99 plots are not included in FIA evaluations")  
    subcycle99 <- FALSE
    
    if (!is.null(ACI) && ACI) 
      message("ACI plots are not included in FIA evaluations")  
    ACI <- FALSE
    intensity1 <- FALSE
  }

  ## Get maximum number of inventory years for states in query 
  ## (used to determine size of tree data)
  #nbrinvyrs <- length(unique(unlist(invyrs)))
  
  ## GETS DATA TABLES (OTHER THAN PLOT/CONDITION) IF NULL
  ###########################################################
  if (gui) {
    datatablst <- c("tree", "seed", "veg", "dwm")
    datatabs <- select.list(c("NONE", datatablst), title="Other tables??", preselect="NONE", 
		multiple=TRUE)
    if (length(datatabs)==0) datatabs <- "NONE"
    istree <- ifelse(any(datatabs == "tree"), TRUE, FALSE)
    isseed <- ifelse(any(datatabs == "seed"), TRUE, FALSE)
    isveg <- ifelse(any(datatabs == "veg"), TRUE, FALSE)
    isdwm <- ifelse(any(datatabs == "dwm"), TRUE, FALSE)
  } else {
    istree <- FIESTA::pcheck.logical(istree, varnm="istree", 
		title="Tree variables?", first="YES", gui=gui)
    isseed <- FIESTA::pcheck.logical(isseed, varnm="isseed", 
		title="Seedling variables?", first="YES", gui=gui)
    isveg <- FIESTA::pcheck.logical(isveg, varnm="isveg", 
		title="Understory veg variables?", first="YES", gui=gui)
    isdwm <- FIESTA::pcheck.logical(isdwm, varnm="isdwm", 
		title="DWM variables?", first="YES", gui=gui)
  }

  ## Data warnings
  ########################################
  ## Note: Periodic data in database includes forested plots >= 5% cover 
  ## Note: Annual data in database includes forested plots >=10% cover

  if (invtype == "PERIODIC") {
    message("note: periodic data includes forested plots >= 5% cover")
    if (isveg) {
      cat("understory vegetation data only available for annual data", "\n" )
      isveg <- FALSE
    }
  }
  if (all(!rslst %in% c("RMRS", "PNWRS"))) isveg <- FALSE


  ## Check defaultVars
  ###########################################################
  defaultVars <- FIESTA::pcheck.logical(defaultVars, varnm="defaultVars", 
		title="Default variables?", first="YES", gui=gui)

  ## Check regionalVars
  ###########################################################
  regionVars <- FIESTA::pcheck.logical(regionVars, varnm="regionVars", 
		title="Regional variables?", first="NO", gui=gui)

  ## Check stateFilter
  ###########################################################
  if (!is.null(stateFilter)) {
    if (nbrstates > 1) {
      if (!is.list(stateFilter) || is.null(names(stateFilter))) {
        stop("if more than 1 state, stFilter must be a named list")
      } else if (length(stateFilter) > nbrstates) {
        stop("too many states in stFilter")
      } else if (!all(names(stateFilter) %in% states)) {
        stop("invalid stFilter names")
      }
    }
    ## Check for alias
#    if (!grepl("p.", stateFilter) && !grepl("c.", stateFilter)) 
#      stop("must include plot or condition alias to stateFilter ('p.' or 'c.')")
    if (!grepl("p.", stateFilter)) 
      stop("must include plot alias to stateFilter ('p.')")

    ## change R syntax to sql syntax
    stateFilter <- gsub("==", "=", stateFilter)
    stateFilter <- gsub("!=", "<>", stateFilter)
    if (grepl("%in%", stateFilter))
      stop("stateFilter must be in sql syntax... change %in% to in(...)")
  } 

  ## Check issp
  ###########################################################
  issp <- FIESTA::pcheck.logical(issp, varnm="issp", 
		title="SpatialPoints of plot vars?", first="YES", gui=gui)

  if (spcond) 
    ## Check spcondid1
    ###########################################################
    spcondid1 <- FIESTA::pcheck.logical(spcondid1, varnm="spcondid1", 
		title="Use cond1 for spatial?", first="YES", gui=gui)

  ## Check savedata
  ###########################################################
  savedata <- FIESTA::pcheck.logical(savedata, varnm="savedata", 
		title="Save data to outfolder?", first="YES", gui=gui)

  ## Check saveqry
  ###########################################################
  saveqry <- FIESTA::pcheck.logical(saveqry, varnm="saveqry", 
		title="Save queries to outfolder?", first="YES", gui=gui)

  ## Check parameters
  ###########################################################
  parameters <- FIESTA::pcheck.logical(parameters, varnm="parameters", 
		title="Save parameters", first="YES", gui=gui)

  ## Check savePOP
  ###########################################################
  savePOP <- FIESTA::pcheck.logical(savePOP, varnm="savePOP", 
		title="Return POP table", first="NO", gui=gui)
 

  ##  Check whether to return tree data
  ###########################################################
  treeReturn <- TRUE
  if (istree && (nbrstates > maxstates.tree)) {
    warning("tree data object is too big.. writing to folder, no returned object")
    #savedata <- TRUE
    treeReturn <- FALSE
  }

  ## Check outfolder, outfn.date, overwrite
  ###########################################################
  if (savedata | saveqry | parameters | !treeReturn) {
    outfolder <- pcheck.outfolder(outfolder, gui=gui)

    ## check append_layer
    append_layer <- FIESTA::pcheck.logical(append_layer, varnm="append_layer", 
		title="append data", first="NO", gui=gui)

    ## check overwrite
    overwrite <- FIESTA::pcheck.logical(overwrite, varnm="overwrite", 
		title="overwrite data", first="NO", gui=gui)


    ## Check out_fmt
    ###########################################################
    out_fmtlst <- c('sqlite', 'gpkg', 'csv', 'gdb')
    out_fmt <- pcheck.varchar(out_fmt, varnm="out_fmt", checklst=out_fmtlst, 
		caption="Out format", gui=gui)

    ## Check for necessary packages
    ###########################################################
    if (out_fmt %in% c("sqlite", "gpkg")) {
      if (!"RSQLite" %in% rownames(installed.packages()))
        stop("RSQLite package is required for exporting to sqlite or gpkg formats")
    } else if (out_fmt %in% c("gdb")) {
      if (!"arcgisbinding" %in% rownames(installed.packages()))
        stop("arcgisbinding package is required for exporting to gdb format")
      arcgisbinding::arc.check_product()
    }

    if (out_fmt != "csv" && overwrite) {
      outfilenm <- getoutfn(out_dsn, outfolder=outfolder, outfn.pre=outfn.pre, 
		outfn.date=outfn.date, overwrite=TRUE, outfn.default = "data")
      overwrite <- FALSE
    }
  }

  ###########################################################################
  #########################      BUILD QUERIES     ##########################
  ###########################################################################
  if (defaultVars) {
    DBvars <- DBvars.default(istree=istree, isseed=isseed, isveg=isveg, 
		isdwm=isdwm, regionVars=regionVars)
    for (nm in names(DBvars)) assign(nm, DBvars[[nm]])
    for (nm in names(filtervarlst)) assign(nm, filtervarlst[[nm]])

    ## add commas
    vars <- toString(c(paste0("p.", pltvarlst), paste0("c.", condvarlst)))
    if (iseval)
      vars <- paste0(vars, ", ppsa.EVALID")
  } else {
    vars <- "p.*"
  }
  sppvars <- {}
  if (biojenk) sppvars <- c(sppvars, "JENKINS_TOTAL_B1", "JENKINS_TOTAL_B2")
  if (greenwt) sppvars <- c(sppvars, "DRYWT_TO_GREENWT_CONVERSION")


  ###########################################################################
  ############################      From query       ########################
  ###########################################################################

  ## PLOT from/join query
  ################################################
  if (iseval) {
    fromqry <- paste0(SCHEMA., "POP_PLOT_STRATUM_ASSGN ppsa")
    pfromqry <- paste0(fromqry, " JOIN ", SCHEMA., 
			"PLOT p ON (p.CN = ppsa.PLT_CN)")
  } else if (measCur) {
    pfromqry <- getpfromqry(Endyr=measEndyr, SCHEMA.=SCHEMA., 
				subcycle99=subcycle99, intensity1=intensity1, popSURVEY=TRUE)
    pfromqry <- gsub("plot", "PLOT", pfromqry)
    pfromqry <- gsub("survey", "SURVEY", pfromqry)
  } else {
    pfromqry <- paste0(SCHEMA., "PLOT p")
  }
  
  ## PLOT/COND from/join query
  ################################################
  pcfromqry <- paste0(pfromqry, " JOIN ", SCHEMA., 
				"COND c ON (c.PLT_CN = p.CN)")

  ## xymeasCur
  ################################################
  if (xymeasCur) 
    xyfromqry <- getpfromqry(Endyr=measEndyr, SCHEMA.=SCHEMA.)
  
  ## TREE query
  ################################################
  if (istree) 
    tfromqry <- paste0(pfromqry, " JOIN ", SCHEMA., 
				"TREE t ON (t.PLT_CN = p.CN)")
 
  ## SEED query
  ################################################
  if (isseed)
    sfromqry <- paste0(pfromqry, " JOIN ", SCHEMA., 
				"SEEDLING s ON (s.PLT_CN = p.CN)")
  
  ## VEG query
  ################################################
  if (isveg) {
    vfromqry <- paste0(pfromqry, " JOIN ", SCHEMA., 
				"P2VEG_SUBPLOT_SPP v ON v.PLT_CN = p.CN")
    vstrfromqry <- paste0(pfromqry, " JOIN ", SCHEMA., 
				"P2VEG_SUBP_STRUCTURE v ON v.PLT_CN = p.CN")
  }
  ## DWM query
  ################################################
  if (isdwm)
    dfromqry <- paste0(pfromqry, " JOIN ", SCHEMA., 
				"COND_DWM_CALC d ON (d.PLT_CN = p.CN)")

  ## Other tables
  ################################################
  if (!is.null(othertables)) {
    xfromqry <- paste0(pfromqry, " JOIN ", SCHEMA., 
				"SUBX x ON (x.PLT_CN = p.CN)")
    xfromqry2 <- paste0(pfromqry, " JOIN ", SCHEMA., 
				"SUBX x ON (x.STATECD = p.STATECD
						and x.UNITCD = p.UNITCD
						and x.COUNTYCD = p.COUNTYCD
						and x.PLOT = p.PLOT)")
  }
  
  ## PPSA query
  ################################################
  if (savePOP || iseval)
    ppsafromqry <- paste0(SCHEMA., "POP_PLOT_STRATUM_ASSGN")
  


##############################################################################
##############################################################################
##############################################################################
  nbrcnds <- {}
  stcds <- {}
  stabbrfn <- ""
  pltcnt <- {}
  
  plt=cond=pltcond=tree=seed=spconddat=xy <- {}
  if(isveg){ vspspp <- vspstr <- {} }
  if(isdwm){ dwm <- {} }
  if(savePOP || iseval) ppsa <- {}  
  stateFilters <- {}
  filtervarlst <- c(pltvarlst, condvarlst)
  spcoords <- "PUBLIC"
  spcoordslst <- "PUBLIC"

  if (!is.null(othertables)) {
    for (i in 1:length(othertables)) 
      assign(paste0("other", i), {})
  }    

  ## Create empty object for each spcoords
  for (coords in spcoordslst) {
    if (xymeasCur) {
      assign(paste0("xyCur_", coords), {})
    } else {
      assign(paste0("xy_", coords), {})
    }

    if (issp) {
      if (xymeasCur) {
        assign(paste0("spxyCur_", coords), {})
      } else {
        assign(paste0("spxy_", coords), {})
      }
    } 
  }

  ## REF_SPECIES table 
  if (istree  && !is.null(sppvars)) {
    REF_SPECIES <- FIESTA::DBgetCSV("REF_SPECIES", ZIP=TRUE, returnDT=TRUE, stopifnull=FALSE)
  }



  ###################################################################################
  ## Loop through states
  ###################################################################################
  for (i in 1:length(states)) {
    evalid <- NULL
    state <- states[i]
    message("getting data from ", state)
    stcd <- FIESTA::pcheck.states(state, "VALUE")
    stabbr <- FIESTA::pcheck.states(state, "ABBR")
    pltx=condx=treex=seedx=vspsppx=vspstrx=dwmx=ppsax=spconddatx <- NULL   

    if (!is.null(othertables)) {
      for (j in 1:length(othertables)) 
        assign(paste0("otherx", j), NULL)
    }    
   

############ CSV only

    ## Get CSV files
    #################################################

    ## PLOT table  
    PLOT <- FIESTA::DBgetCSV("PLOT", stabbr, ZIP=TRUE, returnDT=TRUE, stopifnull=FALSE)

    ## COND table 
    COND <- FIESTA::DBgetCSV("COND", stabbr, ZIP=TRUE, returnDT=TRUE, stopifnull=FALSE)
 
    if (iseval || savePOP) 
      ## POP_PLOT_STRATUM_ASSGN table (ZIP FILE) - 
      ## To get estimation unit & stratum assignment for each plot. 
      POP_PLOT_STRATUM_ASSGN <- FIESTA::DBgetCSV("POP_PLOT_STRATUM_ASSGN", stabbr, 
		ZIP=TRUE, returnDT=TRUE, stopifnull=FALSE)    

    ## TREE table
    if (istree)
      TREE <- FIESTA::DBgetCSV("TREE", stabbr, ZIP=TRUE, returnDT=TRUE, stopifnull=FALSE)

    ## Seedling table
    if (isseed)
      SEEDLING <- FIESTA::DBgetCSV("SEEDLING", stabbr, ZIP=TRUE, returnDT=TRUE, 
		stopifnull=FALSE)

    ## Understory vegetation
    if (isveg) {
      P2VEG_SUBPLOT_SPP <- 
		FIESTA::DBgetCSV("P2VEG_SUBPLOT_SPP", stabbr, ZIP=TRUE, returnDT=TRUE, 
			stopifnull=FALSE)

      P2VEG_SUBP_STRUCTURE <- 
		FIESTA::DBgetCSV("P2VEG_SUBP_STRUCTURE", stabbr, ZIP=TRUE, returnDT=TRUE,
			 stopifnull=FALSE)
    }

    ## Other tables
    if (!is.null(othertables)) {
      for (othertable in othertables) {
        assign(othertable, 
 		FIESTA::DBgetCSV(othertable, stabbr, ZIP=TRUE, returnDT=TRUE, stopifnull=FALSE))
      }
    }
     
############ End CSV only

    ## If FIA evaluation, get all plot from all evaluations.
    if (iseval) {
      evalid <- evalidlist[[state]]
      evalFilter <- paste0("ppsa.EVALID IN(", toString(evalid), ")")

      if (isdwm) {
        evalid.dwm <- evalid[endsWith(as.character(evalid), "07")]
        if (length(evalid.dwm) == 0) stop("must include evaluation ending in 07")
        evalFilter.dwm <- paste("EVALID =", evalid.dwm)
      } 
    } else {
      ## Create filter for state
      stFilter <- paste0("p.STATECD IN(", stcd, ")") 

      if (measCur) {
        evalFilter <- stFilter 
   
      } else {
        if (length(invyrs) > 1){
          invyr <- invyrs[[state]]
        } else {
          invyr <- invyrs[[1]]
        }
        invyrFilter <- paste0("p.INVYR IN(", toString(invyr), ")")
        evalFilter <- paste0(stFilter, " and p.INVYR IN(", toString(invyr), ")")
      }
 
      if (!subcycle99)
        evalFilter <- paste(evalFilter, "and p.SUBCYCLE <> 99")
      if (intensity1)
        evalFilter <- paste(evalFilter, "and p.INTENSITY = '1'")
    }         

    ####################################################################################
    #############################  ADDS FILTER (OPTIONAL)  #############################
    ####################################################################################

    ## Get stateFilter 
    if (is.null(stateFilter) && gui) {
      stateFilters <- ""
      addfilter <- "YES"
      filtervars <- {}
      #filterlstst <- c(pltvarlst, condvarlst)
      filterlstst <- c(pltvarlst)
      filterlst <- filterlstst

      while (addfilter == "YES") {
        filtervar <- select.list(c("NONE", sort(filterlst)), 
		title=paste("Filter variable -", stabbr), multiple=FALSE)
        if (filtervar == "") stop("")
        if (filtervar == "NONE") {
          break
        } else if (filtervar %in% pltvarlst) {
          filterALIAS <- "p"
        } else {
          filterALIAS <- "c"
        }
        filterfromqry <- fromqry
            
        filtervars <- c(filtervars, filtervar)
        filterlst <- filterlst[filterlst != filtervar]
        filtervarx <- paste0(filterALIAS, ".", filtervar)

        filterdbqry <- paste0("select distinct ", filtervarx, " from ", filterfromqry, 
			" where ", evalFilter)
        filterdb <- sort(na.omit(sqldf::sqldf(filterdbqry)[[1]]))
        
        if (filtervar %in% c("ELEV", "CRCOVPCT_RMRS", "CRCOVPCT_LIVEMISS_RMRS", 
			"CRCOVPCT_LIVE_RMRS", "LIVE_CANOPY_CVR_PCT", "LIVE_MISSING_CANOPY_CVR_PCT") ||
			length(filterdb) > 20) {
          ## MINIMUM VALUE
          filtercd_min <- select.list(as.character(filterdb), 
			title=paste("Select MIN", filtervar), multiple=FALSE)
          if (filtercd_min == "") stop("")
      
          filterdbmax <- filterdb[as.numeric(filterdb) >= as.numeric(filtercd_min)]
          ## MAXIMUM VALUE
          filtercd_max <- select.list(as.character(filterdbmax), 
			title=paste("Select MAX", filtervar), multiple=FALSE)
          if (filtercd_max == "") stop("")
          
          stateFilters <- paste(stateFilters, "and (", filtervarx, ">=", filtercd_min, 
			"and", filtervarx, "<=", filtercd_max, ")")
        } else {      
          filtercd <- select.list(as.character(filterdb), 
			title="Select filter code(s)", multiple=TRUE)
          if (length(filtercd) == 0) stop("")
          stateFilters <- paste0(stateFilters, " and ", filtervarx, " in(", toString(filtercd), ")")
        }

        addfilter <- select.list(c("NO", "YES"), 
			title=paste("Another filter? -", stabbr), multiple=FALSE)
        if (addfilter == "") stop("")
      }
      if (i == 1 && length(states) > 1) {
        resp <- select.list(c("YES", "NO"), title="Same for all states", multiple=FALSE)
        if (resp == "YES") stateFilter <- FALSE
      }
    } 
    if (!is.null(stateFilter)) {
      stateFilters <- paste(" and", stateFilter)
    } else {
      stateFilters <- ""
    }

    ## SET QUERY FILTER
    xfilter <- paste0(evalFilter, stateFilters)
    message(paste(stcd, "-", xfilter))

    #####################################################################################
    ###################################    RUN QUERIES   ################################
    #####################################################################################

    ## pltcond query
    #####################################################################################
    if (is.null(PLOT)) {
      pltcondx <- NULL
    } else {
      #if (iseval) 
      #  vars <- paste0(vars, ", ppsa.EVALID")

      pltcondqry <- paste("select distinct", vars, "from", pcfromqry, "where", xfilter)
      pltcondx <- setDT(sqldf::sqldf(pltcondqry, stringsAsFactors=FALSE))
   
      ## Write query to outfolder
      if (saveqry) {
        pltcondqryfnbase <- DBgetfn("pltcond", invtype, outfn.pre, stabbr, 
		evalid=evalid, qry=TRUE, outfn.date=outfn.date)
        pltcondqryfn <- FIESTA::fileexistsnm(outfolder, pltcondqryfnbase, "txt")
        outfile <- file(paste0(outfolder, "/", pltcondqryfn, ".txt"), "w")
          cat(  paste0(pltcondqry, xfilter), "\n", file=outfile)
        close(outfile)
      }
    }

    if (is.null(pltcondx) || nrow(pltcondx) == 0) {
      message("no plots in database for ", state)
    } else {
      pltvarlst2 <- pltvarlst
      #if (iseval) pltvarlst2 <- c(pltvarlst2, "EVALID")
      condvarlst2 <- condvarlst

      ## Filter pltcond with allFilter      
      ###########################################
      pltcondx <- FIESTA::datFilter(x=pltcondx, xfilter=allFilter)$xf

      ## Tag ACI plots
      ###########################################################
      if (ACI && all("NF_SAMPLING_STATUS_CD", "NF_COND_STATUS_CD") %in% names(pltcondx)) {
        pltcondx[, c("ACI", "ACI_NFS") := 0,]
        pltcondx[NF_SAMPLING_STATUS_CD == 1 &
			!is.na(NF_COND_STATUS_CD) & NF_COND_STATUS_CD == 2,
			ACI_NFS:= 1]
        pltcondx[NF_SAMPLING_STATUS_CD == 1 &
			!is.na(NF_COND_STATUS_CD) & NF_COND_STATUS_CD == 2 &
			OWNGRPCD == 10, ACI := 1]
        condvarlst2 <- c(condvarlst2, "ACI", "ACI_NFS")
      }

      ## Separate pltcondx into 2 tables (pltx, condx)
      ###########################################################
      if (!is.null(pltvarlst2)) {
        pltx <- unique(pltcondx[, pltvarlst2, with=FALSE])
        pltx[, CN := as.character(CN)]
        setkey(pltx, CN)
        if ("PREV_PLTCN" %in% names(pltx))
          pltx[, PREV_PLTCN := as.character(PREV_PLTCN)]             
      }
      if (!is.null(condvarlst) && "CONDID" %in% names(pltcondx)) {
        condx <- unique(pltcondx[, condvarlst2, with=FALSE])
        condx[, PLT_CN := as.character(PLT_CN)]        
        setkey(condx, PLT_CN, CONDID)
      } 
 
      ## Change names of LON and LAT to LON_PUBLIC and LAT_PUBLIC
      ###########################################################
      if ("LON" %in% names(pltx)) {
        setnames(pltx, "LON", "LON_PUBLIC")
        pltvarlst2[pltvarlst2 == "LON"] <- "LON_PUBLIC"
      }
      if ("LAT" %in% names(pltx)) {
        setnames(pltx, "LAT", "LAT_PUBLIC")
        pltvarlst2[pltvarlst2 == "LAT"] <- "LAT_PUBLIC"
      }
      if ("ELEV" %in% names(pltx)) {
        setnames(pltx, "ELEV", "ELEV_PUBLIC")
        pltvarlst2[pltvarlst2 == "ELEV"] <- "ELEV_PUBLIC"
      }


      ## Create plot-level, number of condtion variables
      ###########################################################
      if (defaultVars) {

        ## Number of conditions
        nbrcnd <- condx[, list(NBRCND = length(COND_STATUS_CD)), by="PLT_CN"]
        nbrcndsamp <- condx[COND_STATUS_CD != 5, 
			list(NBRCNDSAMP = length(COND_STATUS_CD)), by="PLT_CN"]
        nbrcndfor <- condx[COND_STATUS_CD == 1, 
			list(NBRCNDFOR = length(COND_STATUS_CD)), by="PLT_CN"]
        nbrcndftyp <- condx[COND_STATUS_CD == 1 & FORTYPCD > 0, 
			list(NBRCNDFTYP = length(FORTYPCD)), by="PLT_CN"]

        ## Merge new condition variables together
        nbrcnd <- nbrcndsamp[nbrcnd]
        nbrcnd <- nbrcndfor[nbrcnd]
        nbrcnd <- nbrcndftyp[nbrcnd]
        nbrcnd[is.na(nbrcnd)] <- 0
        setkeyv(nbrcnd, "PLT_CN")

        rm(nbrcndsamp)
        rm(nbrcndfor)
        rm(nbrcndftyp)

        ## Merge to plt table
        pltx <- nbrcnd[pltx]

        nbrcndlst <- c("NBRCND", "NBRCNDSAMP", "NBRCNDFOR", "NBRCNDFTYP")
        pltvarlst2 <- c(pltvarlst2, nbrcndlst)
      

        ## CCLIVEPLT:
        ## A plot level canopy cover variable based on LIVE_CANOPY_CVR_PCT
        if (all(c("LIVE_CANOPY_CVR_PCT", "CONDPROP_UNADJ") %in% names(condx))) {
          ccliveplt <- condx[, 
			round(sum(LIVE_CANOPY_CVR_PCT * CONDPROP_UNADJ, na.rm=TRUE),2), 
			by=PLT_CN]
          setnames(ccliveplt, c("PLT_CN", "CCLIVEPLT"))

          pltx <- ccliveplt[pltx]
          pltvarlst2 <- c(pltvarlst2, "CCLIVEPLT")
        }

        ## Regional variables 
        ######################################################################
        if (isRMRS && regionVars) {
          ## CCRMRSPLT: plot level canopy cover variable based on CRCOVPCT_RMRS
          if (all(c("CRCOVPCT_RMRS", "CONDPROP_UNADJ") %in% names(condx))) {
            ccRMRSplt <- condx[, list(round(sum(CRCOVPCT_RMRS * CONDPROP_UNADJ, 
			na.rm=TRUE), 2)), by="PLT_CN"]
            setnames(ccRMRSplt, c("PLT_CN", "CCRMRSPLT"))
            pltx <- ccRMRSplt[pltx]

            pltvarlst2 <- c(pltvarlst2, "CCRMRSPLT")
          }
          ## CCPLT: plot level canopy cover variable based on CRCOV
          if (all(c("CRCOV", "CONDPROP_UNADJ") %in% names(condx))) {
            ccplt <- condx[, list(round(sum(CRCOVPCT_RMRS * CONDPROP_UNADJ, 
			na.rm=TRUE), 2)), by="PLT_CN"]
            setnames(ccplt, c("PLT_CN", "CCPLT"))
            pltx <- ccplt[pltx]

            pltvarlst2 <- c(pltvarlst2, "CCRMRSPLT")
          }
        }  

        ## FORNONSAMP: 
        ## Plot-level variable based on PLOT_STATUS_CD and PLOT_NONSAMPLE_REASN_CD
        if ("PLOT_NONSAMPLE_REASN_CD" %in% names(pltx)) {
          pltx[, FORNONSAMP := 
		ifelse(!is.na(PLOT_NONSAMPLE_REASN_CD) & PLOT_NONSAMPLE_REASN_CD == 2, 
			"Nonsampled-Denied access",
		ifelse(!is.na(PLOT_NONSAMPLE_REASN_CD) & PLOT_NONSAMPLE_REASN_CD == 3, 
			"Nonsampled-Hazardous",
		ifelse(!is.na(PLOT_NONSAMPLE_REASN_CD) & PLOT_NONSAMPLE_REASN_CD %in% c(5,6),
		 	"Nonsampled-Lost data",
		ifelse(!is.na(PLOT_NONSAMPLE_REASN_CD) & PLOT_NONSAMPLE_REASN_CD == 7, 
			"Nonsampled-Wrong location",
		ifelse(!is.na(PLOT_NONSAMPLE_REASN_CD) & PLOT_NONSAMPLE_REASN_CD == 8, 
			"Nonsampled-Skipped visit",
		ifelse(!is.na(PLOT_NONSAMPLE_REASN_CD) & PLOT_NONSAMPLE_REASN_CD == 9, 
			"Nonsampled-Dropped plot",
		ifelse(!is.na(PLOT_NONSAMPLE_REASN_CD) & PLOT_NONSAMPLE_REASN_CD %in% c(10,11),
 			"Nonsampled-Other",
		ifelse(PLOT_STATUS_CD == "1", "Sampled-Forest",
		ifelse(PLOT_STATUS_CD == "2", "Sampled-Nonforest",
		as.character(pltx$PLOT_STATUS_CD))))))))))]

          pltvarlst2 <- c(pltvarlst2, "FORNONSAMP")
        }

        ## Generate PLOT_ID, with STATECD, UNIT, COUNTYCD, PLOT to define
        pltx[, PLOT_ID := paste0("ID", 
		formatC(pltx$STATECD, width=2, digits=2, flag=0), 
          	formatC(pltx$UNITCD, width=2, digits=2, flag=0),
          	formatC(pltx$COUNTYCD, width=3, digits=3, flag=0),
          	formatC(pltx$PLOT, width=5, digits=5, flag=0))] 
        pltvarlst2 <- c(pltvarlst2, "PLOT_ID")


        ## Additional condition variables
        ######################################################################
        ref_fortypgrp <- FIESTA::ref_codes[FIESTA::ref_codes$VARIABLE == "FORTYPCD",]

        ## FORTYPGRP: condition level variable grouping FORTYPCD
        cndnames <- names(condx)
        if ("FORTYPCD" %in% names(condx)) {
          condx <- merge(condx, ref_fortypgrp[,c("VALUE", "GROUPCD")],
        		by.x="FORTYPCD", by.y="VALUE", all.x=TRUE)
          setnames(condx, "GROUPCD", "FORTYPGRPCD")
          setcolorder(condx, c(cndnames, "FORTYPGRPCD"))
        
          condvarlst2 <- c(condvarlst2, "FORTYPGRPCD")
        }
        ## FLDTYPGRP: condition level variable grouping FLDTYPGRP
        if ("FLDTYPCD" %in% names(condx)) {
          condx <- merge(condx, ref_fortypgrp[,c("VALUE", "GROUPCD")], 
               by.x="FLDTYPCD", by.y="VALUE", all.x=TRUE)
          setnames(condx, "GROUPCD", "FLDTYPGRPCD")
          setcolorder(condx, c(cndnames, "FLDTYPGRPCD"))

          condvarlst2 <- c(condvarlst2, "FLDTYPGRPCD")
        }
        setkey(condx, PLT_CN, CONDID)

        ## TIMBERCD condition level variable defining TIMBERLAND conditions
        if ("SITECLCD" %in% names(condx)) {
          condx[COND_STATUS_CD == 1, TIMBERCD := 2]
          condx[SITECLCD %in% 1:6, TIMBERCD := 1]

          condvarlst2 <- c(condvarlst2, "TIMBERCD")
        }
      }   ##  End (defaultVars)
      
      setnames(pltx, "PLT_CN", "CN")
      setkeyv(pltx, "CN")


      ##  GET PLOT AND CONDITION COUNTS  
      ######################################################################
      plotcnt.vars <- names(pltx)[names(pltx) %in% c("CN", "STATECD", "INVYR", "FORNONSAMP")]
      pltcnt <- rbind(pltcnt, 
		 datPlotcnt(plt=unique(pltx[, plotcnt.vars, with=FALSE]), savedata=FALSE))


      ##############################################################
      ## spconddata
      ##############################################################
      if (spcond) {
        ## Get condition data for spatial plot 
        spconddatx <- getspconddat(cond=condx, condid1=spcondid1, ACI=ACI)

        ## Append data
        spconddat <- rbind(spconddat, spconddatx)
      }

 
      ##############################################################
      ## xydata
      ##############################################################
      #xyx <- pltx[, c("CN", getcoords(coords)), with=FALSE]
      xyx <- copy(pltx)
      setnames(xyx, "CN", "PLT_CN")
 
      ## Get xy for the most current sampled plot
      if (xymeasCur) {
        xvars <- c("p.PLOT_ID", "p.CN", paste0("p.", getcoords(coords)))
        xyx.qry <- paste("select distinct", toString(xvars), "from", xyfromqry)
        xyx.qry <- gsub("from plot", "from pltx ", xyx.qry)

        xyCurx <- sqldf::sqldf(xyx.qry)
        names(xyCurx)[names(xyCurx) == "CN"] <- "PLT_CN"
        assign(paste0("xyCurx_", coords), xyCurx) 
        assign(paste0("xyCur_", coords), 
				rbind(get(paste0("xyCur_", coords)), xyCurx)) 
      } else {
        assign(paste0("xyx_", coords), xyx)
        assign(paste0("xy_", coords), 
				rbind(get(paste0("xy_", coords)), xyx))
      } 
    }
 
    ##############################################################
    ## Tree data
    ##############################################################
    if (istree && !is.null(pltx)) {
      if (is.null(treevarlst) & is.null(tsumvarlst)) {
        treex <- NULL
        istree <- FALSE
      } else {
        ## add commas
        ttvars <- toString(paste0("t.", c(treevarlst, tsumvarlst)))
        treeqry <- paste("select distinct", ttvars, "from", tfromqry, "where", xfilter)
        treex <- sqldf::sqldf(treeqry, stringsAsFactors=FALSE)

        if (!is.null(sppvars)) {
          sppsql <- paste("select SPCD,", paste(sppvars, collapse=","), "from REF_SPECIES")
          ref_spp <- sqldf::sqldf(sppsql)
        }

        if (nrow(treex) != 0) {
          treex <- setDT(treex)
          treex[, PLT_CN := as.character(PLT_CN)]
          setkey(treex, PLT_CN, CONDID)

          ## Subset overall filters from pltx
          treex <- treex[treex$PLT_CN %in% unique(pltx$CN),]

          ## Filter treex with alltFilter      
          ###########################################
          if (!is.null(alltFilter)) {
            treexf <- FIESTA::datFilter(x=treex, xfilter=alltFilter)$xf
            if (!is.null(treexf)) treex <- treexf

            pltx <- pltx[pltx$CN %in% treex$PLT_CN, ]
            condx <- condx[condx$PLT_CN %in% treex$PLT_CN, ]
          }

          if (is.null(treex)) {
            istree <- FALSE
          } else { 

            ## Check ACI
            if (!ACI) {
              ACIpltID <- condx[COND_STATUS_CD == 1, paste(PLT_CN, CONDID)]
              treex <- treex[paste(treex$PLT_CN, treex$CONDID) %in% ACIpltID,]
            } 

            ## Write query to outfolder
            if (saveqry) {
              treeqryfnbase <- DBgetfn("tree", invtype, outfn.pre, stabbr, 
			evalid=evalid, qry=TRUE, outfn.date=outfn.date)
              treeqryfn <- FIESTA::fileexistsnm(outfolder, treeqryfnbase, "txt")
              outfile <- file(paste0(outfolder, "/", treeqryfn, ".txt"), "w")
                cat(  paste0(treeqry, xfilter), "\n", file=outfile)
              close(outfile)
            }

            ## Make sure these variables are numeric
            nbrvars <- c("DIA", "DRYBIO_BOLE", "DRYBIO_STUMP", "DRYBIO_TOP", 
			"DRYBIO_SAPLING", "DRYBIO_WDLD_SPP", "BHAGE")
            if (any(nbrvars %in% names(treex)))
              nbrvars <- nbrvars[which(nbrvars %in% names(treex))]
            treex[, (nbrvars) := lapply(.SD, FIESTA::check.numeric), .SDcols=nbrvars]

            ## Change NA values to 0 values
            #if (any(names(treex) %in% treenavars)) 
            #  treex <- DT_NAto0(treex, treenavars)

            if (defaultVars)
              ## Create new tree variables - basal area
              treex[, BA := DIA * DIA * 0.005454]

            ## Create new biomass variables
            if (!is.null(sppvars)) {
              treenames <- names(treex)
              treex <- merge(treex, ref_spp, by="SPCD")
              if (biojenk) {
                treex[, BIOJENK_kg := exp(JENKINS_TOTAL_B1 + JENKINS_TOTAL_B2 * log(DIA * 2.54))]
                treex[, BIOJENK_lb := BIOJENK_kg * 2.2046]		## Converts back to tons
                treex[, JENKINS_TOTAL_B1 := NULL][, JENKINS_TOTAL_B2 := NULL]
                sppvarsnew <- c(sppvars, "BIOJENK_kg", "BIOJENK_lb")
              }
              setcolorder(treex, c(treenames, sppvarsnew)) 
            }  

            ## Append data
            if (treeReturn)
              tree <- rbind(tree, treex)
          }
        }
      }
    }
 
    ##############################################################
    ## Seedling data
    ##############################################################
    if (isseed && !is.null(pltx)) {
      ## GENERATE AND RUN QUERY AND WRITE TO OUTFOLDER

      if (is.null(seedvarlst)) {
        seedx <- NULL
        isseed <- NULL
      } else {
        ## add commas
        ssvars <- toString(paste0("s.", c(seedvarlst, ssumvarlst)))
        seedqry <- paste("select distinct", ssvars, "from", sfromqry, "where", xfilter)
        seedx <- sqldf::sqldf(seedqry, stringsAsFactors=FALSE)

        if (nrow(seedx) != 0) {
          seedx <- setDT(seedx)
          seedx[, PLT_CN := as.character(PLT_CN)]
          setkey(seedx, PLT_CN, CONDID)

          ## Subset overall filters from pltx
          seedx <- seedx[seedx$PLT_CN %in% unique(pltx$CN),]

          ## Check ACI
          if (!ACI) {
            ACIplts <- condx[COND_STATUS_CD == 1, paste(PLT_CN, CONDID)]
            seedx <- seedx[paste(seedx$PLT_CN, seedx$CONDID) %in% ACIplts,]
          } 

          ## Write query to outfolder
          if (saveqry) {
            seedqryfnbase <- DBgetfn("seed", invtype, outfn.pre, stabbr, 
			evalid=evalid, qry=TRUE, outfn.date=outfn.date)
            seedqryfn <- FIESTA::fileexistsnm(outfolder, seedqryfnbase, "txt")
            outfile <- file(paste0(outfolder, "/", seedqryfn, ".txt"), "w")
              cat(  paste0(seedqry, xfilter), "\n", file=outfile)
            close(outfile)
          }

          ## Change NA values to 0 values
#          if (any(names(seedx) %in% seednavars)) 
#            seedx <- FIESTA::DT_NAto0(seedx, seednavars)
     
          if (defaultVars && "TREECOUNT_CALC" %in% names(seedx)) {
            ## Create variable, SEEDCNT6, where a value of 6 means 6 or more seeds (per SUBP) 
            seedx[, SEEDCNT6 := TREECOUNT_CALC][TREECOUNT_CALC >= 6, SEEDCNT6 := 6]

            ## Create variable, SEEDSUBP6, indicating a species has 6 or more seedlings on a SUBP
            seedx[, SEEDSUBP6 := 0][TREECOUNT_CALC >= 6, SEEDSUBP6 := 1]
          }              
        }
      }
      ## Append data
      seed <- rbind(seed, seedx)
    }

    ##############################################################
    ## Understory vegetation data
    ##############################################################
    if (isveg && !is.null(pltx)) {

      ## Get data for P2VEG_SUBPLOT_SPP
      vspsppvars <- toString(paste0("v.", vspsppvarlst))
      vspsppqry <- paste("select distinct", vspsppvars, "from", vfromqry, "where", xfilter)
      vspsppx <- sqldf::sqldf(vspsppqry, stringsAsFactors=FALSE)

      if (nrow(vspsppx) != 0) {
        vspsppx <- setDT(vspsppx)
        vspsppx[, PLT_CN := as.character(PLT_CN)]
        setkey(vspsppx, PLT_CN)

        ## Subset overall filters from pltx
        vspsppx <- vspsppx[vspsppx$PLT_CN %in% unique(pltx$CN),]

        ## Write query to outfolder
        if (saveqry) {
          vspsppqryfnbase <- DBgetfn("vspspp", invtype, outfn.pre, stabbr, 
			evalid=evalid, qry=TRUE, outfn.date=outfn.date)
          vspsppqryfn <- FIESTA::fileexistsnm(outfolder, vspsppqryfnbase, "txt")
          outfile <- file(paste0(outfolder, "/", vspsppqryfn, ".txt"), "w")
          cat(  paste0(vspsppqry, xfilter), "\n", file=outfile)
          close(outfile)
        }
      }

      ## Get data for P2VEG_SUBP_STRUCTURE
      vspstrvars <- toString(paste0("v.", vspstrvarlst))
      vspstrqry <- paste("select distinct", vspstrvars, "from", vstrfromqry, "where", xfilter)
      vspstrx <- sqldf::sqldf(vspstrqry, stringsAsFactors=FALSE)

      if(nrow(vspstrx) != 0){
        vspstrx <- setDT(vspstrx)
        vspstrx[, PLT_CN := as.character(PLT_CN)]
        setkey(vspstrx, PLT_CN)

        ## Subset overall filters from pltx
        vspstrx <- vspstrx[vspstrx$PLT_CN %in% unique(pltx$CN),]

        ## Write query to outfolder
        if (saveqry) {
          vspstrqryfnbase <- DBgetfn("vspstr", invtype, outfn.pre, stabbr, 
			evalid=evalid, qry=TRUE, outfn.date=outfn.date)
          vspstrqryfn <- FIESTA::fileexistsnm(outfolder, vspstrqryfnbase, "txt")
          outfile <- file(paste0(outfolder, "/", vspstrqryfn, ".txt"), "w")
          cat(  paste0(vspstrqry, xfilter), "\n", file=outfile)
          close(outfile)
        }
      }
      vspspp <- rbind(vspspp, vspsppx)
      vspstr <- rbind(vspstr, vspstrx)
    }

    ##############################################################
    ## Down woody data
    ##############################################################
    if (isdwm && !is.null(pltx)) {
      cat("\n",
      "## STATUS: GETTING DOWN WOODY DATA (", stabbr, ") ...", "\n")
    
      if (is.null(dwmlst)) {
        dwmx <- NULL
        isdwm <- NULL
      } else {

        dvars <- toString(paste0("d.", dwmlst))
        xfilter.dwm <- sub("ppsa.", "", xfilter)
        dwmqry <- paste("select distinct", dvars, "from", dfromqry, "where", xfilter)
        dwmx <- sqldf::sqldf(dwmqry, stringsAsFactors=FALSE)

        if (nrow(dwmx) != 0) {
          dwmx <- setDT(dwmx)
          dwmx[, PLT_CN := as.character(PLT_CN)]
          setkey(dwmx, PLT_CN, CONDID)

          ## Subset overall filters from pltx
          dwmx <- dwmx[dwmx$PLT_CN %in% unique(pltx$CN),]

          ## Write query to outfolder
          if (saveqry) {
            dwmqryfnbase <- DBgetfn("dwm", invtype, outfn.pre, stabbr, 
			evalid=evalid, qry=TRUE, outfn.date=outfn.date)
            dwmqryfn <- fileexistsnm(outfolder, dwmqryfnbase, "txt")
            outfile <- file(paste0(outfolder, "/", dwmqryfn, ".txt"), "w")
            cat(  paste0(dwmqry, xfilter), "\n", file=outfile)
            close(outfile)
          }
        }
      }
      dwm <- rbind(dwm, dwmx)
    }


    ##############################################################
    ## Other tables
    ##############################################################
    if (!is.null(othertables) && !is.null(pltx)) {
      for (j in 1:length(othertables)) {
        othertable <- othertables[j]

        cat("\n",
        "## STATUS: GETTING", othertable, "(", stabbr, ") ...", "\n")
    
        othertablexnm <- paste0("otherx", j)
        if (othertable == "PLOTGEOM") {
          joinid <- "CN"
          xfromqry <- sub("x.PLT_CN", "x.CN", xfromqry)
        } else {
          joinid <- "PLT_CN"
        }
        xqry <- paste("select distinct x.* from", sub("SUBX", othertable, xfromqry), 
			"where", xfilter)
        assign(othertablexnm, sqldf::sqldf(xqry, stringsAsFactors=FALSE))

        if (nrow(get(othertablexnm)) != 0) {
          assign(othertablexnm, setDT(get(othertablexnm)))

          get(othertablexnm)[, PLT_CN := as.character(PLT_CN)]
          setkey(get(othertablexnm), "PLT_CN")

          ## Subset overall filters from pltx
          assign(othertablexnm, 
			get(othertablexnm)[get(othertablexnm)[[joinid]] %in% unique(pltx$CN),])
          assign(paste0("other", j), rbind(get(paste0("other", j)), get(othertablexnm)))
        }
      }
    }


    ##############################################################
    ## If savePOP or more than one evalType
    ##############################################################
    if (savePOP && !is.null(pltx)) {
      cat("\n",
      "## STATUS: GETTING POP_PLOT_STRATUM_ASSGN DATA(", stabbr, ") ...", "\n")
    
      ppsavars <- toString(c("PLT_CN", "EVALID", "STATECD", "ESTN_UNIT", "STRATUMCD"))
      ppsaqry <- paste("select", ppsavars, "from", ppsafromqry, "where statecd =", stcd)
      if (iseval) {
        if (subsetPOP) {
          ppsaqry <- paste(ppsaqry, "and evalid in(", toString(evalid), ")")
        } else {
          evalstyr <- substr(evalid, 1, nchar(evalid)-2)
          ppsaqry <- paste(ppsaqry, "and evalid like", paste0("'", evalstyr, "%'"))
        }
      }
      ppsax <- sqldf::sqldf(ppsaqry, stringsAsFactors=FALSE)
      if(nrow(ppsax) != 0){
        ppsax <- setDT(ppsax)
        ppsax[, PLT_CN := as.character(PLT_CN)]
        setkey(ppsax, PLT_CN)

        ## Subset overall filters from pltx
        ppsax <- ppsax[ppsax$PLT_CN %in% unique(pltx$CN),]

        ## Write query to outfolder
        if (saveqry) {
          ppsaqryfnbase <- DBgetfn("ppsa", invtype, outfn.pre, stabbr, 
			evalid=evalid, qry=TRUE, outfn.date=outfn.date)
          ppsaqryfn <- FIESTA::fileexistsnm(outfolder, ppsaqryfnbase, "txt")
          outfile <- file(paste0(outfolder, "/", ppsaqryfn, ".txt"), "w")
          cat(  paste0(ppsaqry, xfilter), "\n", file=outfile)
          close(outfile)
        }
      }
      ppsa <- rbind(ppsa, ppsax)
    }

    ###############################################################################
    ###############################################################################
    ## SAVE data
    ###############################################################################
    ###############################################################################
    if ((savedata || !treeReturn) && !is.null(pltx)) {

      overwrite_layer <- overwrite
      append_layer2 <- append_layer
      col.names <- ifelse (i == 1, TRUE, FALSE)
      if (i == 1) { 
        if (overwrite && append_layer) append_layer2 <- FALSE
      } else {
        if (length(states) > 1 && !append_layer) {
          append_layer2 <- TRUE
          overwrite_layer <- FALSE
        }
      }

      if (savedata && issp) {
        message("saving spatial xy data...")
        xycoords <- getcoords(coords)

        if (xymeasCur) {
          spxynm <- paste0("spxyCur_", coords)
          xyplt <- get(paste0("xyCurx_", coords))
        } else {
          spxynm <- paste0("spxy_", coords)
          xyplt <- get(paste0("xyx_", coords))
        }
        if (!is.null(xyplt)) {
          if (!is.null(pltx) && length(unique(xyplt$PLT_CN)) != nrow(pltx))
            warning("number of plots in ", spxynm, " does not match plt table")            

          ## Generate shapefile
          out_fmt_sp <- ifelse(out_fmt == "csv", "shp", out_fmt)

          assign(spxynm, spMakeSpatialPoints(xyplt=xyplt, xvar=xycoords[1], 
			yvar=xycoords[2], xy.uniqueid="PLT_CN", xy.crs=4269, addxy=TRUE, 
			exportsp=savedata, out_dsn=out_dsn, out_fmt=out_fmt_sp, 
			outfolder=outfolder, out_layer=spxynm, outfn.date=outfn.date,
 			overwrite_layer=overwrite_layer, append_layer=append_layer2,
			outfn.pre=outfn.pre))
        }
      }       
 
      if (savedata && !issp) {
        xycoords <- getcoords(coords)

        if (xymeasCur) {
          xynm <- paste0("xyCur_", coords)
          xyplt <- get(paste0("xyCurx_", coords))
        } else {
          xynm <- paste0("xy_", coords)
          xyplt <- get(paste0("xyx_", coords))
        }
 
        if (!is.null(xyplt)) {
          index.unique.xyplt <- NULL
          if (i == 1) index.unique.xyplt <- "PLT_CN"
          datExportData(xyplt, outfolder=outfolder, 
			out_fmt=out_fmt, out_dsn=out_dsn, out_layer=xynm, 
			outfn.date=outfn.date, overwrite_layer=overwrite_layer,
			index.unique=index.unique.xyplt, append_layer=append_layer2,
			outfn.pre=outfn.pre)
        }
      }   

      if (savedata && !is.null(spconddatx)) {
        index.unique.spconddat <- NULL
        if (i == 1) index.unique.spconddat <- "PLT_CN"
        datExportData(spconddat, outfolder=outfolder, 
			out_fmt=out_fmt, out_dsn=out_dsn, out_layer="spconddat", 
			outfn.date=outfn.date, overwrite_layer=overwrite_layer,
			index.unique=index.unique.spconddat, append_layer=append_layer2,
			outfn.pre=outfn.pre)
      }

      if (savedata && !is.null(pltx)) {
        index.unique.pltx <- NULL
        if (i == 1) index.unique.pltx <- "CN"
        datExportData(pltx, outfolder=outfolder, 
			out_fmt=out_fmt, out_dsn=out_dsn, out_layer="plot", 
			outfn.date=outfn.date, overwrite_layer=overwrite_layer,
			index.unique=index.unique.pltx, append_layer=append_layer2,
			outfn.pre=outfn.pre)
      }

      if (savedata && !is.null(condx)) {
        index.unique.condx <- NULL
        if (i == 1) index.unique.condx <- c("PLT_CN", "CONDID")
        datExportData(condx, outfolder=outfolder, 
			out_fmt=out_fmt, out_dsn=out_dsn, out_layer="cond", 
			outfn.date=outfn.date, overwrite_layer=overwrite_layer,
			index.unique=index.unique.condx, append_layer=append_layer2,
			outfn.pre=outfn.pre)
      }

      if (!treeReturn && !is.null(treex)) {
        index.unique.treex <- NULL
        if (i == 1) index.unique.treex <- c("PLT_CN", "CONDID", "SUBP", "TREE")
        datExportData(treex, outfolder=outfolder, 
			out_fmt=out_fmt, out_dsn=out_dsn, out_layer="tree", 
			outfn.date=outfn.date, overwrite_layer=overwrite_layer,
			index.unique=index.unique.treex, append_layer=append_layer2,
			outfn.pre=outfn.pre)
      }

      if (savedata && !is.null(seedx)) {
        index.unique.seedx <- NULL
        if (i == 1) index.unique.seedx <- c("PLT_CN", "CONDID", "SUBP")
        datExportData(seedx, outfolder=outfolder, 
			out_fmt=out_fmt, out_dsn=out_dsn, out_layer="seed", 
			outfn.date=outfn.date, overwrite_layer=overwrite_layer,
			index.unique=index.unique.seedx, append_layer=append_layer2,
			outfn.pre=outfn.pre)
      } 
      if (savedata && !is.null(vspsppx)) {
        index.unique.vspsppx <- NULL
        if (i == 1) index.unique.vspsppx <- c("PLT_CN", "CONDID")
        datExportData(vspsppx, outfolder=outfolder, 
			out_fmt=out_fmt, out_dsn=out_dsn, out_layer="vspspp", 
			outfn.date=outfn.date, overwrite_layer=overwrite_layer,
			index.unique=index.unique.vspsppx, append_layer=append_layer2,
			outfn.pre=outfn.pre)

        index.unique.vspstrx <- NULL
        if (i == 1) index.unique.vspstrx <- c("PLT_CN", "CONDID")
        datExportData(vspstrx, outfolder=outfolder, 
			out_fmt=out_fmt, out_dsn=out_dsn, out_layer="vspstr", 
			outfn.date=outfn.date, overwrite_layer=overwrite_layer,
			index.unique=index.unique.vspstrx, append_layer=append_layer2,
			outfn.pre=outfn.pre)
      } 

      if (savedata && !is.null(dwmx)) {
        index.unique.dwmx <- NULL
        if (i == 1) index.unique.dwmx <- c("PLT_CN", "CONDID")
        datExportData(dwmx, outfolder=outfolder, 
			out_fmt=out_fmt, out_dsn=out_dsn, out_layer="dwm", 
			outfn.date=outfn.date, overwrite_layer=overwrite_layer,
			index.unique=index.unique.dwmx, append_layer=append_layer2,
			outfn.pre=outfn.pre)
      } 

      if (savedata && !is.null(othertables)) {
        for (j in 1:length(othertables)) {
          othertable <- othertables[j]
          othertablexnm <- paste0("otherx", j)
          othernm <- paste0("other", j)

          index.unique.other <- NULL
          if (othertable == "SUBPLOT") {
            index.unique.other <- c("PLT_CN", "SUBP")
          }

          datExportData(get(othertablexnm), outfolder=outfolder, 
			out_fmt=out_fmt, out_dsn=out_dsn, out_layer=tolower(othertable), 
			outfn.date=outfn.date, overwrite_layer=overwrite_layer, 
			index.unique=index.unique.other, append_layer=append_layer2, 
			outfn.pre=outfn.pre)
        }
      }  
 
      if (savedata && savePOP && !is.null(ppsax)) {
        index.unique.ppsax <- NULL
        if (i == 1) index.unique.ppsax <- "PLT_CN"
        datExportData(ppsax, outfolder=outfolder, 
			out_fmt=out_fmt, out_dsn=out_dsn, out_layer="ppsa", 
			outfn.date=outfn.date, overwrite_layer=overwrite_layer,
			index.unique=index.unique.ppsax, append_layer=append_layer2,
			outfn.pre=outfn.pre)

      }
    }
    plt <- rbind(plt, pltx)
    cond <- rbind(cond, condx)
    rm(nbrcnd)
    rm(pltcondx)
    rm(treex)
    gc()
  }

  if (parameters) {
    ## OUTPUTS A TEXTFILE OF INPUT PARAMETERS TO OUTFOLDER
    ###########################################################

    params <- formals(DBgetPlots)
    params <- mget(names(params),ifnotfound="NULL",envir=as.environment(-1))

    outparamfn <- paste0("DBgetPlots_parameters_", stabbrfn, "_", 
		format(Sys.time(), "%Y%m%d"))
    if (!overwrite)
      outparamfn <- FIESTA::fileexistsnm(outfolder, outparamfn, "txt")  
    statesout <- toString(paste0("'", params$states, "'"))
    rsout <- toString(paste0("'", params$RS, "'"))
    stateFilter <- ifelse(is.null(params$stateFilter), FALSE, TRUE)

    outfile <- file(paste0(outfolder, "/", outparamfn, ".txt"), "w")
    cat(  "states <- c(", statesout, ")", "\n", 
      "RS <- c(", rsout, ")", "\n", 
      "invtype <- \"", params$invtype, "\"", "\n",
      "evalid <- ", FIESTA::getlistparam(params$evalid), "\n",  
      "evalCur <- ", params$evalCur, "\n",
      "evalEndyr <- ", FIESTA::getlistparam(params$evalEndyr), "\n",  
      "evalType <- \"", FIESTA::getlistparam(params$evalType), "\"", "\n",
      "allyrs <- ", params$allyrs, "\n",
      "invyrs <- ", FIESTA::getlistparam(params$invyrs), "\n",  
      "istree <- ", params$istree, "\n",
      "isseed <- ", params$isseed, "\n",
      "isveg <- ", params$isveg, "\n",
      "isdwm <- ", params$isdwm, "\n",
      "issp <- ", params$issp, "\n",
      "spcondid1 <- ", params$spcondid1, "\n",
      "defaultVars <- ", params$defaultVars, "\n",
      "regionVars <- ", params$regionVars, "\n",
      "ACI <- ", params$ACI, "\n",
      "subcycle99 <- ", params$subcycle99, "\n",
      "intensity1 <- ", params$intensity1, "\n",
      "allFilter <- \"", params$xfilters, "\"", "\n",
      "savedata <- ", params$savedata, "\n",
      "saveqry <- ", params$saveqry, "\n",
      "parameters <- ", params$parameters, "\n",
      "outfolder <- \"", params$outfolder, "\"", "\n",
      "outSQLitefn <- \"", params$outSQLitefn, "\"", "\n",
      "gpkg <- ", params$gpkg, "\n",
      "outfn.pre <- \"", params$outfn.pre, "\"", "\n",
      "outfn.date <- ", params$outfn.date, "\n",
      "overwrite <- ", params$overwrite, "\n",
      "savePOP <- ", params$savePOP, "\n",
     "\n",
    file = outfile, sep="")

    cat(  "fiadat <- DBgetPlots(states=states, RS=RS, invtype=invtype, evalid=evalid, 
	evalCur=evalCur, evalEndyr=evalEndyr, evalType=evalType, allyrs=allyrs, 
	invyrs=invyrs, istree=istree, isseed=isseed, isveg=isveg, isdwm=isdwm, 
	issp=issp, spcondid1=spcondid1, defaultVars=defaultVars, regionVars=regionVars, 
	ACI=ACI, subcycle99=FALSE, intensity1=TRUE, allFilter=allFilter, 
	savedata=savedata, saveqry=saveqry, parameters=parameters, outfolder=outfolder, 
	outSQLitefn=outSQLitefn, gpkg=gkpg, outfn.pre=outfn.pre, outfn.date=outfn.date,
	overwrite=overwrite, savePOP=savePOP)",
    file = outfile, sep="")

    close(outfile)
  }
 
  ## Write out plot/condition counts to comma-delimited file.
  if (savedata) {
    append_layer2 <- ifelse(overwrite, FALSE, append_layer)
    datExportData(pltcnt, outfolder=outfolder, 
			out_fmt=out_fmt, out_dsn=out_dsn, out_layer="pltcnt", 
			outfn.date=outfn.date, overwrite_layer=overwrite,
			append_layer=append_layer2, outfn.pre=outfn.pre)
  }


  ## GENERATE RETURN LIST
  fiadatlst <- list(states=states)
  if (!is.null(plt)) {
    nbrplots <- length(unique(plt$CN))
    if (nrow(plt) != nbrplots) warning("plt records are not unique")
    fiadatlst$plt <- setDF(plt) 
  }
  if (!is.null(cond)) {
    if (!is.null(plt)) {
      if (length(unique(cond$PLT_CN)) != nbrplots)
        warning("number of plots in cond table does not match plt table")
    }
    fiadatlst$cond <- setDF(cond)
  }
  notsame <- FALSE
  if (istree & !is.null(tree)) fiadatlst$tree <- setDF(tree)
  if (isseed & !is.null(seed)) fiadatlst$seed <- setDF(seed)
 

  if (isveg) {
    if (!is.null(vspspp)) fiadatlst$vspspp <- setDF(vspspp)
    if (!is.null(vspstr)) fiadatlst$vspstr <- setDF(vspstr)
  }
  if (isdwm) 
    if (!is.null(dwm)) fiadatlst$dwm <- setDF(dwm)

  if (!is.null(othertables)) {
    for (i in 1:length(othertables))
      fiadatlst[[othertables[i]]] <- get(paste0("other", i))
  }

  if (issp) {
    xycoords <- getcoords(coords)
    if (xymeasCur) {
      spxyCurnm <- paste0("spxyCur_", coords)
      assign(spxyCurnm, 
		spMakeSpatialPoints(xyplt=get(paste0("xyCur_", coords)), 
		xvar=xycoords[1], yvar=xycoords[2], xy.uniqueid="PLT_CN", xy.crs=4269))
      fiadatlst[[spxyCurnm]] <- get(spxyCurnm)
    } else {  
      spxynm <- paste0("spxy_", coords)
      assign(spxynm, 
		spMakeSpatialPoints(xyplt=get(paste0("xy_", coords)), 
		xvar=xycoords[1], yvar=xycoords[2], xy.uniqueid="PLT_CN", xy.crs=4269))
      fiadatlst[[spxynm]] <- get(spxynm)
    }
  } else {
    xycoords <- getcoords(coords)
    if (xymeasCur) {
      xyCurnm <- paste0("xyCur_", coords)
      assign(xyCurnm, get(paste0("xyCur_", coords))) 
      fiadatlst[[xyCurnm]] <- get(xyCurnm)
    } else {
      xynm <- paste0("xy_", coords)
      assign(xynm, get(paste0("xy_", coords))) 
      fiadatlst[[xynm]] <- get(xynm)
    }
  }

  if (!is.null(spconddat)) fiadatlst$spconddat <- setDF(spconddat)
  if ((savePOP || iseval) && !is.null(ppsa)) fiadatlst$POP_PLOT_STRATUM_ASSGN <- setDF(ppsa)
    
  if (length(evalidlist) > 0) fiadatlst$evalid <- evalidlist
  fiadatlst$pltcnt <- pltcnt


  if (!is.null(evalidlist)) {
    evaliddf <- data.frame(do.call(rbind, evalidlist))
    stcds <- FIESTA::pcheck.states(row.names(evaliddf), "VALUE")
    evaliddf <- data.frame(stcds, row.names(evaliddf), evaliddf, row.names=NULL)
    names(evaliddf) <- c("STATECD", "STATE", "EVALID")
    evaliddf <- evaliddf[order(evaliddf$STATECD), ]
    fiadatlst$evalid <- evalidlist

    if (savedata) {
      append_layer2 <- ifelse(overwrite, FALSE, append_layer)
      datExportData(evaliddf, outfolder=outfolder, 
			out_fmt=out_fmt, out_dsn=out_dsn, out_layer="evalid", 
			outfn.date=outfn.date, overwrite_layer=overwrite,
			append_layer=append_layer2, outfn.pre=outfn.pre)
    }
  }
  
  if (saveqry) cat("\n", paste("Saved queries to:", outfolder), "\n") 

 

  ## Return data list
  return(fiadat=fiadatlst)
}

