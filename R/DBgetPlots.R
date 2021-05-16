DBgetPlots <- function (states=NULL, datsource="datamart", data_dsn=NULL, 
	RS=NULL, invtype="ANNUAL", evalid=NULL, evalCur=FALSE, evalEndyr=NULL, 
	evalAll=FALSE, evalType="VOL", measCur=FALSE, measEndyr=NULL, allyrs=FALSE, 
	invyrs=NULL, xymeasCur=FALSE, istree=FALSE, isseed=FALSE, isveg=FALSE, 
	issubp=FALSE, islulc=FALSE, isdwm=FALSE, plotgeom=FALSE, othertables=NULL, 
	issp=FALSE, spcond=FALSE, spcondid1=FALSE, defaultVars=TRUE, regionVars=FALSE, 
	ACI=FALSE, subcycle99=FALSE, intensity1=FALSE, stateFilter=NULL, allFilter=NULL, 
	alltFilter=NULL, savedata=FALSE, saveqry=FALSE, outfolder=NULL, 
	out_fmt="csv", out_dsn=NULL, append_layer=FALSE, outfn.pre=NULL, 
	outfn.date=FALSE, overwrite_dsn=FALSE, overwrite_layer=TRUE, savePOP=FALSE, 
	returndata=TRUE) {

  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- ifelse(nargs() == 0, TRUE, FALSE)

  other_tables <- c("BOUNDARY", "COND_DWM_CALC", "COUNTY", "DWM_COARSE_WOODY_DEBRIS", 
	"DWM_DUFF_LITTER_FUEL", "DWM_FINE_WOODY_DEBRIS", "DWM_MICROPLOT_FUEL", 
	"DWM_RESIDUAL_PILE", "DWM_TRANSECT_SEGMENT", "DWM_VISIT", "GRND_CVR", 
	"INVASIVE_SUBPLOT_SPP", "LICHEN_LAB", "LICHEN_PLOT_SUMMARY", "LICHEN_VISIT", 
	"PLOTSNAP", "PLOT_REGEN", "SEEDLING_REGEN", "SITETREE", 
	"SOILS_EROSION", "SOILS_LAB", "SOILS_SAMPLE_LOC", "SOILS_VISIT", 
	"SUBPLOT_REGEN", "TREE_GRM_BEGIN", "TREE_GRM_ESTN", "TREE_GRM_MIDPT",
 	"TREE_GRM_THRESHOLD", "TREE_REGIONAL_BIOMASS", "TREE_WOODLAND_STEMS")

  pop_tables <- c("POP_ESTN_UNIT", "POP_EVAL", "POP_EVAL_ATTRIBUTE", "POP_EVAL_GRP", 
	"POP_EVAL_TYP", "POP_STRATUM", "SURVEY") 


  if (gui) {
    invtype=evalCur=evalAll=evalType=measCur=allyrs=istree=isseed=issubp=
	isveg=isdwm=isgrm=issccm=issp=spcondid1=defaultVars=regionVars=ACI=
	subcycle99=intensity1=allFilter=savedata=saveqry=parameters=out_fmt=
	overwrite=BIOJENK_kg=BIOJENK_lb=PREV_PLTCN=savePOP=xymeasCur <- NULL
  }

  ## Set global variables  
  CN=CONDID=COND_STATUS_CD=PLT_CN=FORTYPCD=pltvarlst=condvarlst=pgeomvarlst=
	treevarlst=tsumvarlst=seedvarlst=ssumvarlst=vsubpsppvarlst=vsubpstrvarlst=
	subpvarlst=subpcvarlst=dwmvarlst=grmvarlst=sccmvarlst=filtervarlst=
	SUBPPROP_UNADJ=MICRPROP_UNADJ=TPA_UNADJ=TPAMORT_UNADJ=TPAREMV_UNADJ=
	SEEDCNT6=TREECOUNT_CALC=SEEDSUBP=LIVE_CANOPY_CVR_PCT=CONDPROP_UNADJ=
	PLOT_NONSAMPLE_REASN_CD=PLOT_STATUS_CD=BA=DIA=CRCOVPCT_RMRS=TIMBERCD=
	SITECLCD=RESERVCD=JENKINS_TOTAL_B1=JENKINS_TOTAL_B2=POP_PLOT_STRATUM_ASSGN=
	NF_SAMPLING_STATUS_CD=NF_COND_STATUS_CD=ACI_NFS=OWNCD=OWNGRPCD=INVYR=
	FORNONSAMP=PLOT_ID=sppvarsnew=STATECD=UNITCD=COUNTYCD=invyrs=SEEDSUBP6=
	PREV_PLT_CN <- NULL


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

  ## Check arguments
  ###########################################################
  input.params <- names(as.list(match.call()))[-1]
  if (!all(input.params %in% names(formals(DBgetPlots)))) {
    miss <- input.params[!input.params %in% formals(DBgetPlots)]
    stop("invalid parameter: ", toString(miss))
  }

  ## Define variables
  actual=getinvyr <- FALSE
  SCHEMA <- ""
  SCHEMA. <- ""
  isRMRS <- FALSE
  xycoords = c("LON_PUBLIC", "LAT_PUBLIC")
  coords <- "PUBLIC"
  parameters <- FALSE
  biojenk <- FALSE 
  greenwt <- TRUE
  isgrm <- FALSE
  issccm=FALSE

  ########################################################################
  ### GET PARAMETERS 
  ########################################################################
  iseval <- FALSE
  subsetPOP <- FALSE

  ## Check invtype
  invtypelst <- c('ANNUAL', 'PERIODIC')
  invtype <- pcheck.varchar(invtype, varnm="invtype", checklst=invtypelst, 
		caption="Inventory Type", gui=gui)

  #############################################################################
  ## Set datsource
  ########################################################
  datsourcelst <- c("datamart", "sqlite")
  datsource <- FIESTA::pcheck.varchar(var2check=datsource, varnm="datsource", 
		checklst=datsourcelst, gui=gui, caption="Data source?") 
  if (datsource == "sqlite") {
    if (!all(c("RSQLite", "DBI") %in% rownames(installed.packages()))) {
	 stop("RSQLite and DBI packages are required to run SQLite queries")
    }
  } 
  if (datsource %in% c("sqlite", "gdb")) {
    data_dsn <- DBtestSQLite(data_dsn)
  }
  if (!is.null(data_dsn)) {
    if (getext(data_dsn) %in% c("sqlite", "db", "db3")) {
      dbconn <- DBtestSQLite(data_dsn, dbconnopen=TRUE, showlist=FALSE)
      dbtablst <- DBI::dbListTables(dbconn)
    } else {
      stop("only sqlite databases available currently")
    }     
  }

  ## GETS DATA TABLES (OTHER THAN PLOT/CONDITION) IF NULL
  ###########################################################
  if (gui) {
    datatablst <- c("tree", "seed", "veg", "subp", "dwm")
    datatabs <- select.list(c("NONE", datatablst), title="Other tables??", 
		preselect="NONE", multiple=TRUE)
    if (length(datatabs)==0) datatabs <- "NONE"
    istree <- ifelse(any(datatabs == "tree"), TRUE, FALSE)
    isseed <- ifelse(any(datatabs == "seed"), TRUE, FALSE)
    isveg <- ifelse(any(datatabs == "veg"), TRUE, FALSE)
    if (isveg) {
      issubp <- TRUE
    } else {
      issubp <- ifelse(any(datatabs == "subp"), TRUE, FALSE)
    }
    isdwm <- ifelse(any(datatabs == "dwm"), TRUE, FALSE)
    isgrm <- ifelse(any(datatabs == "grm"), TRUE, FALSE)
    issccm <- ifelse(any(datatabs == "sccm"), TRUE, FALSE)
  } else {
    istree <- FIESTA::pcheck.logical(istree, varnm="istree", 
		title="Tree variables?", first="YES", gui=gui)
    isseed <- FIESTA::pcheck.logical(isseed, varnm="isseed", 
		title="Seedling variables?", first="YES", gui=gui)
    isveg <- FIESTA::pcheck.logical(isveg, varnm="isveg", 
		title="Understory veg variables?", first="YES", gui=gui)
    if (isveg && invtype == "PERIODIC") {
      message("understory vegetation data only available for annual data\n")
      isveg <- FALSE
    }
    #if (all(!rslst %in% c("RMRS", "PNWRS"))) isveg <- FALSE
    if (isveg) {
      issubp <- TRUE
    } else {
      issubp <- FIESTA::pcheck.logical(issubp, varnm="issubp", 
		title="Subplot tables?", first="YES", gui=gui)
    }
    isdwm <- FIESTA::pcheck.logical(isdwm, varnm="isdwm", 
		title="DWM variables?", first="YES", gui=gui)
    isgrm <- FIESTA::pcheck.logical(isgrm, varnm="isgrm", 
		title="GRM variables?", first="YES", gui=gui)
    issccm <- FIESTA::pcheck.logical(issccm, varnm="issccm", 
		title="Subplot Change variables?", first="YES", gui=gui)
  }


  ########################################################################
  ### DBgetEvalid()
  ########################################################################

  ## Data warnings
  ## Note: Periodic data in database includes forested plots >= 5% cover 
  ## Note: Annual data in database includes forested plots >=10% cover

  if (isdwm) {
    evalType <- c(evalType, "DWM")
  }
#  if (isveg) {
#    evalType <- c(evalType, "P2VEG")
#  }
  if (isgrm || issccm) {
    evalType <- c(evalType, "CHNG")
  }
 
  ## Get states, Evalid and/or invyrs info
  evalInfo <- DBgetEvalid(states=states, RS=RS, datsource="datamart", 
		data_dsn=data_dsn, invtype=invtype, evalid=evalid, evalCur=evalCur, 
		evalEndyr=evalEndyr, evalAll=evalAll, evalType=evalType, gui=gui)
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
    if (!savePOP && (any(lapply(evalInfo$evalTypelist, length) > 1) || 
		any(lapply(evalInfo$evalidlist, length) > 1))) {
      savePOP <- TRUE
    }
  }

  ### GET RS & rscd
  ###########################################################
  isRMRS <- ifelse(length(rslst) == 1 && rslst == "RMRS", TRUE, FALSE) 
     
  ## Get state abbreviations and codes 
  ###########################################################
  stabbrlst <- FIESTA::pcheck.states(states, statereturn="ABBR")
  stcdlst <- FIESTA::pcheck.states(states, statereturn="VALUE")

  ## Get number of states 
  nbrstates <- length(states)  ##  Check whether to return tree data

  ## If using EVALID, you don't need to get INVYRS, intensity, or subcycle
  if (!iseval) {  
    ### Check measCur
    ###########################################################
    measCur <- FIESTA::pcheck.logical(measCur, varnm="measCur", 
		title="Current measyear?", first="YES", gui=gui)

    ### Check measEndyr
    ###########################################################
    measEndyr.filter <- NULL
    if (!is.null(measEndyr)) {
      if (!is.null(invyrtab)) {
        minyr <- min(invyrtab$INVYR)
        if (!is.numeric(measEndyr) || measEndyr < minyr)
          stop("measEndyr must be yyyy format and greater than minimum inventory year: ", 
			minyr)
        measCur <- TRUE
        measEndyr.filter <- paste0(" and MEASYEAR < ", measEndyr)
      }
    }
    if (measCur) {
      xymeasCur <- TRUE
      allyrs <- FALSE
    }

    ## Check allyrs
    ###########################################################
    allyrs <- FIESTA::pcheck.logical(allyrs, varnm="allyrs", title="All years?", 
		first="YES", gui=gui)
    if (allyrs) {
      ## xymeasCur
      xymeasCur <- FIESTA::pcheck.logical(xymeasCur, varnm="xymeasCur", 
		title="Most current XY?", first="YES", gui=gui)
      measCur <- FALSE
      measEndyr=measEndyr.filter <- NULL
    }

    ## Check INVYR(S) 
    ###########################################################
    if (!measCur) {
      if ((is.null(invyrs) || length(invyrs) == 0)) {
        if (is.null(invyrtab)) {
          stop("must include INVYR in plot")
        } 
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
      message("note: periodic data includes forested plots >= 5% cover")
      #intensity1 <- FALSE
    }

    ## Check ACI
    ###########################################################
    ACI <- FIESTA::pcheck.logical(ACI, varnm="ACI", 
		title="ACI conditions?", first="NO", gui=gui)

  } else {
    subsetPOP <- TRUE

    if (!is.null(subcycle99) && subcycle99) 
      message("subcycle99 plots are not included in FIA evaluations")  
    subcycle99 <- FALSE
    
    if (!is.null(ACI) && ACI) 
      message("ACI plots are not included in FIA evaluations")  
    ACI <- FALSE
    allyrs <- FALSE
  }

  ## Set maxstates 
  ###########################################################
  ##  The number of states to append together, while still small enough to return 
  ##  as objects (without memory issues). This includes all tables except tree table..  
  ##  If there is more than 1 state with more than 6 inventory years and no filters,  
  ##  the tree table will not be returned as an object.. only written to outfolder.
  maxstates.tree <- ifelse(allyrs && is.null(allFilter), 3, 
						ifelse(!is.null(allFilter), 10, 20))  


  ## Get maximum number of inventory years for states in query 
  ## (used to determine size of tree data)
  #nbrinvyrs <- length(unique(unlist(invyrs)))
  

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
    if (!grepl("p.", stateFilter)) {
      stop("must include plot alias to stateFilter ('p.')")
    }

    ## change R syntax to sql syntax
    stateFilter <- gsub("==", "=", stateFilter)
    stateFilter <- gsub("!=", "<>", stateFilter)
    if (grepl("%in%", stateFilter)) {
      stop("stateFilter must be in sql syntax... change %in% to in(...)")
    }
  } 

  ## Check issp
  ###########################################################
  issp <- FIESTA::pcheck.logical(issp, varnm="issp", 
		title="SpatialPoints of plot vars?", first="NO", gui=gui)

  if (spcond) {
    ## Check spcondid1
    ###########################################################
    spcondid1 <- FIESTA::pcheck.logical(spcondid1, varnm="spcondid1", 
		title="Use cond1 for spatial?", first="YES", gui=gui)
  }


  ########################################################################
  ### Saving data
  ########################################################################

  ## Check savedata
  savedata <- FIESTA::pcheck.logical(savedata, varnm="savedata", 
		title="Save data to outfolder?", first="YES", gui=gui)

  ## Check saveqry
  saveqry <- FIESTA::pcheck.logical(saveqry, varnm="saveqry", 
		title="Save queries to outfolder?", first="YES", gui=gui)

  ## Check parameters
  parameters <- FIESTA::pcheck.logical(parameters, varnm="parameters", 
		title="Save parameters", first="NO", gui=gui)

  ## Check savePOP
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

  ## Check outfolder, outfn.date, overwrite_dsn
  ###########################################################
  if (savedata | saveqry | parameters | !treeReturn | !returndata) {
    outlst <- pcheck.output(out_dsn=out_dsn, out_fmt=out_fmt, 
		outfolder=outfolder, outfn.pre=outfn.pre, outfn.date=outfn.date, 
		overwrite_dsn=overwrite_dsn, append_layer=append_layer, gui=gui)
    out_dsn <- outlst$out_dsn
    outfolder <- outlst$outfolder
    out_fmt <- outlst$out_fmt
  }
 
  ###########################################################################
  #########################      BUILD QUERIES     ##########################
  ###########################################################################
  if (defaultVars) {
    istree2 <- ifelse(istree || !is.null(alltFilter), TRUE, FALSE)
    DBvars <- DBvars.default(istree=istree2, isseed=isseed, isveg=isveg, 
		issubp=issubp, isdwm=isdwm, plotgeom=plotgeom, regionVars=regionVars)
    for (nm in names(DBvars)) assign(nm, DBvars[[nm]])
    for (nm in names(filtervarlst)) assign(nm, filtervarlst[[nm]])

    if (datsource == "sqlite") {
      pltfldlst <- DBI::dbListFields(dbconn, plot_layer)
      if (is.null(chkdbtab(pltfldlst, "LON")) && !is.null(chkdbtab(pltfldlst, "LON_PUBLIC"))) {
        pltvarlst <- sub("LON", "LON_PUBLIC", pltvarlst)
        pltvarlst <- sub("LAT", "LAT_PUBLIC", pltvarlst)
      }
      if (is.null(chkdbtab(pltfldlst, "ELEV")) && !is.null(chkdbtab(pltfldlst, "ELEV_PUBLIC"))) {
        pltvarlst <- sub("ELEV", "ELEV_PUBLIC", pltvarlst)
      }
      pltvarlst <- pltvarlst[pltvarlst %in% pltfldlst]
    }
 
    ## add commas
    vars <- toString(c(paste0("p.", pltvarlst), paste0("c.", condvarlst)))
    pcgvars <- toString(c(paste0("p.", pltvarlst), paste0("pg.", pgeomvarlst), 
		paste0("c.", condvarlst)))
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
  plot_layer <- "PLOT"
  cond_layer <- "COND"
  if (datsource == "sqlite") {
    plot_layer <- chkdbtab(dbtablst, "PLOT", stopifnull=TRUE)
    cond_layer <- chkdbtab(dbtablst, "COND", stopifnull=TRUE)
  } 

  ## PPSA query
  ################################################
  if (savePOP || iseval) {
    ppsa_layer <- "POP_PLOT_STRATUM_ASSGN"
    if (datsource == "sqlite") {
      ppsa_layer <- chkdbtab(dbtablst, "POP_PLOT_STRATUM_ASSGN")
      if (is.null(ppsa_layer)) {
        ppsa_layer <- chkdbtab(dbtablst, "ppsa", stopifnull=TRUE)
      }
    }
    ppsafromqry <- paste0(SCHEMA., ppsa_layer, " ppsa")
  }

  ## PLOT from/join query
  ################################################
  if (iseval) {
    pfromqry <- paste0(ppsafromqry, " JOIN ", SCHEMA., 
			plot_layer, " p ON (p.CN = ppsa.PLT_CN)")
  } else if (measCur) {
    popSURVEY <- TRUE
    survey_layer <- "SURVEY"
    if (datsource == "sqlite") {
      survey_layer <- chkdbtab(dbtablst, "SURVEY")
      popSURVEY <- ifelse(is.null(survey_layer), FALSE, TRUE)
    }
    pfromqry <- getpfromqry(Endyr=measEndyr, SCHEMA.=SCHEMA., allyrs=allyrs,
			subcycle99=subcycle99, intensity1=intensity1, popSURVEY=popSURVEY,
			plotnm=plot_layer, surveynm=survey_layer)
    pfromqry <- gsub("survey", survey_layer, pfromqry)
  } else {
    pfromqry <- paste0(SCHEMA., "PLOT p")
  }

  ## PLOT/COND from/join query
  ################################################
  pcfromqry <- paste0(pfromqry, " JOIN ", SCHEMA., 
				cond_layer, " c ON (c.PLT_CN = p.CN)")
  if (plotgeom) {
    plotgeom_layer <- "PLOTGEOM"
    if (datsource == "sqlite") {
      plotgeom_layer <- chkdbtab(dbtablst, "PLOTGEOM")
      if (is.null(plotgeom_layer)) {
        plotgeom <- FALSE
      }
    }
    pcgeomfromqry <- paste0(pcfromqry, " JOIN ", SCHEMA., 
				plotgeom_layer, " pg ON (pg.CN = p.CN)")
  }

  ## TREE query
  ################################################
  if (istree || !is.null(alltFilter)) {
    tree_layer <- "TREE" 
    if (datsource == "sqlite") {
      tree_layer <- chkdbtab(dbtablst, "TREE")
      if (is.null(tree_layer)) {
        istree <- FALSE
      }
    }
    tfromqry <- paste0(pfromqry, " JOIN ", SCHEMA., 
				tree_layer, " t ON (t.PLT_CN = p.CN)")
  }
  ## SEED query
  ################################################
  if (isseed) {
    seed_layer <- "SEEDLING"
    if (datsource == "sqlite") {
      seed_layer <- chkdbtab(dbtablst, "SEEDLING")
      if (is.null(seed_layer)) {
        isseed <- FALSE
      }
    }
    sfromqry <- paste0(pfromqry, " JOIN ", SCHEMA., 
				seed_layer, " s ON (s.PLT_CN = p.CN)")
  } 
  ## VEG query
  ################################################
  if (isveg) {
    vsub_layer <- "P2VEG_SUBPLOT_SPP"
    vstr_layer <- "P2VEG_SUBP_STRUCTURE"
    if (datsource == "sqlite") {
      vsub_layer <- chkdbtab(dbtablst, "P2VEG_SUBPLOT_SPP")
      if (is.null(vsub_layer)) {
        isveg <- FALSE
      }
      vstr_layer <- chkdbtab(dbtablst, "P2VEG_SUBP_STRUCTURE")
      if (is.null(vstr_layer)) {
        isveg <- FALSE
      }
    }
    vfromqry <- paste0(pfromqry, " JOIN ", SCHEMA., 
				vsub_layer, " v ON v.PLT_CN = p.CN")
    vstrfromqry <- paste0(pfromqry, " JOIN ", SCHEMA., 
				vstr_layer, " v ON v.PLT_CN = p.CN")
  }
  ## SUBP query
  ################################################
  if (issubp) {
    subp_layer <- "SUBPLOT"
    subpcond_layer <- "SUBP_COND"
    if (datsource == "sqlite") {
      subp_layer <- chkdbtab(dbtablst, "SUBPLOT")
      if (is.null(subp_layer)) {
        issubp <- FALSE
      }
      subpcond_layer <- chkdbtab(dbtablst, "SUBP_COND")
      if (is.null(subpcond_layer)) {
        issubp <- FALSE
      }
    }
    subpfromqry <- paste0(pfromqry, " JOIN ", SCHEMA., 
				subp_layer, " subp ON subp.PLT_CN = p.CN")
    subpcfromqry <- paste0(pfromqry, " JOIN ", SCHEMA., 
				subpcond_layer, " subpc ON subpc.PLT_CN = p.CN")
  }
  ## DWM query
  ################################################
  if (isdwm) {
    dwm_layer <- "COND_DWM_CALC"
    if (datsource == "sqlite") {
      dwm_layer <- chkdbtab(dbtablst, "COND_DWM_CALC")
      if (is.null(dwm_layer)) {
        isdwm <- FALSE
      }
    }
    dfromqry <- paste0(pfromqry, " JOIN ", SCHEMA., 
				dwm_layer, " d ON (d.PLT_CN = p.CN)")
  }
  ## SCCM query
  ################################################
  if (issccm) {
    sccm_layer <- "SUBP_COND_CHNG_MTRX"
    if (datsource == "sqlite") {
      sccm_layer <- chkdbtab(dbtablst, "SUBP_COND_CHNG_MTRX")
      if (is.null(sccm_layer)) {
        issccm <- FALSE
      }
    }
    sccmfromqry <- paste0(pcfromqry, " JOIN ", SCHEMA.,
		"COND PCOND ON (PCOND.PLT_CN = p.PREV_PLT_CN) JOIN ", SCHEMA.,
		sccm_layer, " ON (SCCM.PLT_CN = c.PLT_CN AND 
			SCCM.PREV_PLT_CN = PCOND.PLT_CN AND SCCM.CONDID = c.CONDID 
			AND SCCM.PREVCOND = PCOND.CONDID)")
    sccmwhereqry <- "c.CONDPROP_UNADJ IS NOT NULL 
		AND ((SCCM.SUBPTYP = 3 AND c.PROP_BASIS = 'MACR') 
		OR (SCCM.SUBPTYP = 1 AND c.PROP_BASIS = 'SUBP')) 
		AND COALESCE(c.COND_NONSAMPLE_REASN_CD, 0) = 0 
		AND COALESCE(PCOND.COND_NONSAMPLE_REASN_CD, 0) = 0" 
#		AND (c.COND_STATUS_CD = 1 AND PCOND.COND_STATUS_CD = 1)" 
  }

  ## lulc query
  ################################################
  if (islulc) {
    lulcfromqry <- paste0(pcfromqry, " JOIN ", SCHEMA.,
		"COND PCOND ON (PCOND.PLT_CN = p.PREV_PLT_CN and PCOND.CONDID=c.CONDID)")
  }

  ## GRM query
  ################################################
  if (isgrm) {
    grm_layer <- "TREE_GRM_COMPONENT"
    if (datsource == "sqlite") {
      grm_layer <- chkdbtab(dbtablst, "TREE_GRM_COMPONENT")
      if (is.null(grm_layer)) {
        isgrm <- FALSE
      }
    }
    grmfromqry <- paste0(pfromqry, " JOIN ", SCHEMA., 
				grm_layer, " grm ON (grm.PLT_CN = p.CN)")
  }

  ## Other tables
  ################################################
  if (!is.null(othertables)) {
    if (datsource == "sqlite") {
      for (othertab in othertables) {
        othertab_layer <- chkdbtab(dbtablst, othertab)
        if (is.null(othertab_layer)) {
          othertables <- othertables[othertables != othertab]
        }
      }
    }
    xfromqry <- paste0(pfromqry, " JOIN ", SCHEMA., 
				"SUBX x ON (x.PLT_CN = p.CN)")
    xfromqry2 <- paste0(pfromqry, " JOIN ", SCHEMA., 
				"SUBX x ON (x.STATECD = p.STATECD
						and x.UNITCD = p.UNITCD
						and x.COUNTYCD = p.COUNTYCD
						and x.PLOT = p.PLOT)")
  }
  


##############################################################################
##############################################################################
##############################################################################
  nbrcnds <- {}
  stcds <- {}
  stabbrfn <- ""
  pltcnt <- {}
  stateFilters <- {}
  filtervarlst <- c(pltvarlst, condvarlst)
  spcoords <- "PUBLIC"
  spcoordslst <- "PUBLIC"

  if (returndata) {
    plt=cond=pltcond=tree=seed=spconddat <- {}
    if(isveg) { vsubpspp=vsubpstr <- {} }
    if(issubp) { subp=subpc <- {} }
    if(isdwm) { dwm <- {} }
    if(issccm) { sccm <- {} }
    if(isgrm) { grm <- {} }
    if (islulc) {lulc <- {} }
    if(savePOP || iseval) ppsa <- {}  

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
  }

  ## REF_SPECIES table 
  if (istree && !is.null(sppvars)) {
    REF_SPECIES <- FIESTA::DBgetCSV("REF_SPECIES", returnDT=TRUE, stopifnull=FALSE)
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
    pltx=condx=treex=seedx=vsubpsppx=vsubpstrx=subpx=subpcx=dwmx=sccmx=
		ppsax=spconddatx=lulcx <- NULL   

    if (!is.null(othertables)) {
      for (j in 1:length(othertables)) 
        assign(paste0("otherx", j), NULL)
    }    
   
    ## Create filter for state
    stFilter <- paste0("p.STATECD IN(", stcd, ")") 

    ## If FIA evaluation, get all plot from all evaluations.
    if (iseval) {
      evalid <- evalidlist[[state]]
      evalFilter <- paste0("ppsa.EVALID IN(", toString(evalid), ")")

      if (any(evalType == "P2VEG")) {
        evalid.veg <- evalid[endsWith(as.character(evalid), "10")]
        if (length(evalid.veg) == 0) stop("must include evaluation ending in 10")
        evalFilter.veg <- paste("ppsa.EVALID =", evalid.veg)
      } else {
        evalFilter.veg <- evalFilter
      }
      if (isdwm) {
        evalid.dwm <- evalid[endsWith(as.character(evalid), "07")]
        if (length(evalid.dwm) == 0) stop("must include evaluation ending in 07")
        evalFilter.dwm <- paste("ppsa.EVALID =", evalid.dwm)
      } 
      if (isgrm) {
        evalid.grm <- evalid[endsWith(as.character(evalid), "03")]
        if (length(evalid.grm) == 0) stop("must include evaluation ending in 03")
        evalFilter.grm <- paste("ppsa.EVALID =", evalid.grm)
      } 
    } else {
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
 
      if (!subcycle99) {
        evalFilter <- paste(evalFilter, "and p.SUBCYCLE <> 99")
      }
      if (isveg) {
        evalFilter.veg <- evalFilter
      }
      if (isdwm) {
        evalFilter.dwm <- evalFilter 
      }      
      if (isgrm) {
        evalFilter.grm <- evalFilter
      }       
    } 
    if (intensity1) {
      evalFilter <- paste(evalFilter, "and p.INTENSITY = '1'")
    }

    if (datsource == "datamart") {

      ## Get CSV files
      #################################################

      ## PLOT table  
      PLOT <- FIESTA::DBgetCSV("PLOT", stabbr, returnDT=TRUE, stopifnull=FALSE)
 
      ## PLOTGEOM table  
      if (plotgeom) {
        PLOTGEOM <- FIESTA::DBgetCSV("PLOTGEOM", stabbr, returnDT=TRUE, stopifnull=FALSE)
      }

      ## COND table 
      COND <- FIESTA::DBgetCSV("COND", stabbr, returnDT=TRUE, stopifnull=FALSE)
 
      if (iseval || savePOP) {
        ## POP_PLOT_STRATUM_ASSGN table (ZIP FILE) - 
        ## To get estimation unit & stratum assignment for each plot. 
        POP_PLOT_STRATUM_ASSGN <- FIESTA::DBgetCSV("POP_PLOT_STRATUM_ASSGN", stabbr, 
		returnDT=TRUE, stopifnull=FALSE) 
      }   
      ## Seedling table
      if (isseed) {
        SEEDLING <- FIESTA::DBgetCSV("SEEDLING", stabbr, returnDT=TRUE, 
		stopifnull=FALSE)
      }
      ## Understory vegetation
      if (isveg) {
        P2VEG_SUBPLOT_SPP <- 
		FIESTA::DBgetCSV("P2VEG_SUBPLOT_SPP", stabbr, returnDT=TRUE, 
		stopifnull=FALSE)
        P2VEG_SUBP_STRUCTURE <- 
		FIESTA::DBgetCSV("P2VEG_SUBP_STRUCTURE", stabbr, returnDT=TRUE, 
		stopifnull=FALSE)
      }
      ## Subplot data
      if (issubp) {
        SUBPLOT <- 
		FIESTA::DBgetCSV("SUBPLOT", stabbr, returnDT=TRUE, stopifnull=FALSE)
        SUBP_COND <- 
		FIESTA::DBgetCSV("SUBP_COND", stabbr, returnDT=TRUE, stopifnull=FALSE)
      }
      ## DWM calc table
      if (isdwm) {
        COND_DWM_CALC <- FIESTA::DBgetCSV("COND_DWM_CALC", stabbr, returnDT=TRUE, 
		stopifnull=FALSE)
      }
      ## Area change matrix table
      if (issccm) {
        SUBP_COND_CHNG_MTRX <- FIESTA::DBgetCSV("SUBP_COND_CHNG_MTRX", stabbr, 
		returnDT=TRUE, stopifnull=FALSE)
      }
      ## GRM calc table
      if (isgrm) {
        TREE_GRM_COMPONENT <- FIESTA::DBgetCSV("TREE_GRM_COMPONENT", stabbr, 
		returnDT=TRUE, stopifnull=FALSE)
      }

      ## Other tables
      if (!is.null(othertables)) {
        for (othertable in othertables) {
          assign(othertable, 
 		FIESTA::DBgetCSV(othertable, stabbr, returnDT=TRUE, stopifnull=FALSE))
        }
      }
    } 
############ End CSV only        

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
        filterfromqry <- pfromqry
            
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
        if (resp == "YES") gui <- FALSE
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

    ## Run pltcond query
    #####################################################################################
    if (datsource == "datamart" && is.null(PLOT)) {
      pltcondx <- NULL
    } else {
      #if (iseval) 
      #  vars <- paste0(vars, ", ppsa.EVALID")

      if (plotgeom) {
        pltcondqry <- paste("select distinct", pcgvars, "from", pcgeomfromqry, "where", xfilter)
      } else {      
        pltcondqry <- paste("select distinct", vars, "from", pcfromqry, "where", xfilter)
      }
      if (datsource == "sqlite") {
        pltcondx <- DBI::dbGetQuery(dbconn, pltcondqry)
      } else {
        pltcondx <- setDT(sqldf::sqldf(pltcondqry, stringsAsFactors=FALSE))
      }
   
      ## Write query to outfolder
      if (saveqry) {
        pltcondqryfn <- DBgetfn("pltcond", invtype, outfn.pre, stabbr, 
		evalid=evalid, qry=TRUE, outfolder=outfolder, overwrite=overwrite_layer, 
		outfn.date=outfn.date, ext="txt")
        outfile <- file(pltcondqryfn, "w")
        cat(  pltcondqry, "\n", file=outfile)
        close(outfile)
        message("saved pltcond query to:\n", pltcondqryfn)
      }
    }

    if (is.null(pltcondx) || nrow(pltcondx) == 0) {
      message("no plots in database for ", state)
    } else {
      pltvarlst2 <- pltvarlst
      if (plotgeom) {
        pltvarlst2 <- unique(c(pltvarlst2, pgeomvarlst))
      }
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

        ## LANDUSECD
        ## A combination of PRESNFCD and COND_STATUS_CD
        if (all(c("PRESNFCD", "COND_STATUS_CD") %in% names(condx))) {
          condx$LANDUSECD <- with(condx, ifelse(is.na(PRESNFCD), COND_STATUS_CD, PRESNFCD))
        }
      }   ##  End (defaultVars)
      
      setnames(pltx, "PLT_CN", "CN")
      setkeyv(pltx, "CN")
    }
 
    ## Create combined unique identifier to subset other tables
    pcondID <- condx[, paste(PLT_CN, CONDID)]


    ##############################################################
    ## SUBP_COND_CHNG_MTRX (SCCM) data
    ##############################################################
    if (issccm && !is.null(pltx)) {
      message("\n",
      	"## STATUS: Getting area change data from SUBP_COND_CHNG_MTRX (", stabbr, ") ...", "\n")
      sccmvars <- c("PLT_CN", "CONDID", "SUBP", "SUBPTYP", "PREV_PLT_CN", 
			"PREVCOND", "SUBPTYP_PROP_CHNG")
      sccmqry <- paste("select", toString(paste0("SCCM.", sccmvars)), 
			"from", sccmfromqry, "where", sccmwhereqry, "and", xfilter)
#      sccmqry <- paste("select SCCM.PLT_CN, 
#			sum(COALESCE(SCCM.SUBPTYP_PROP_CHNG / 4, 0)) PROP_CHNG", 
#			"from", sccmfromqry, "where", sccmwhereqry, "and", xfilter,
#			"group by SCCM.PLT_CN")

      if (datsource == "sqlite") {
        sccmx <- DBI::dbGetQuery(dbconn, sccmqry)
      } else {
        sccmx <- sqldf::sqldf(sccmqry, stringsAsFactors=FALSE)
      }
      if (nrow(sccmx) != 0) {
        sccmx <- setDT(sccmx)
        sccmx[, PLT_CN := as.character(PLT_CN)]
        setkey(sccmx, PLT_CN)

        ## Subset overall filters from pltx
        sccmx <- sccmx[sccmx$PLT_CN %in% pltx$CN,]

        ## Merge to pltx
        #pltx <- merge(pltx, sccmx, all.x=TRUE, by.x="CN", by.y="PLT_CN")
        
      }

      if (returndata) {
        ## Append data
        sccm <- rbind(sccm, sccmx)
      }
    }

    ##############################################################
    ## lulc data
    ##############################################################
    if (islulc && !is.null(pltx)) {
      message("\n",
      	"## STATUS: Getting Land Use/Land Cover data (", stabbr, ") ...", "\n")
      lulcqry <- paste("select c.PLT_CN, p.PREV_PLT_CN, p.STATECD, p.UNITCD, p.COUNTYCD, p.PLOT,
 		pcond.CONDID PREV_CONDID, c.CONDID, 
    		pcond.INVYR PREV_INVYR, c.INVYR,
		pcond.CONDPROP_UNADJ PREV_CONDPROP_UNADJ, c.CONDPROP_UNADJ, 
 		pcond.COND_STATUS_CD PREV_COND_STATUS_CD, c.COND_STATUS_CD,
 		pcond.LAND_COVER_CLASS_CD PREV_LAND_COVER_CLASS_CD, c.LAND_COVER_CLASS_CD, 
		pcond.PRESNFCD PREV_PRESNFCD, c.PRESNFCD,
		case when pcond.PRESNFCD is null then pcond.COND_STATUS_CD else pcond.PRESNFCD end as PREV_LANDUSECD,
		case when c.PRESNFCD is null then c.COND_STATUS_CD else c.PRESNFCD end as LANDUSECD",  
			"from", lulcfromqry, "where", xfilter)
      if (datsource == "sqlite") {
        lulcx <- DBI::dbGetQuery(dbconn, lulcqry)
      } else {
        lulcx <- sqldf::sqldf(lulcqry, stringsAsFactors=FALSE)
      }
      if (nrow(lulcx) != 0) {
        lulcx <- setDT(lulcx)
        lulcx[, PLT_CN := as.character(PLT_CN)]
        lulcx[, PREV_PLT_CN := as.character(PREV_PLT_CN)]
        setkey(lulcx, PLT_CN)

        ## Subset overall filters from pltx
        lulcx <- lulcx[lulcx$PLT_CN %in% pltx$CN,]

        ## Merge to pltx
        #pltx <- merge(pltx, lulcx, all.x=TRUE, by.x="CN", by.y="PLT_CN")
        
      }

      if (returndata) {
        ## Append data
        lulc <- rbind(lulc, lulcx)
      }
    }


    ##############################################################
    ## Tree data
    ##############################################################
    if ((istree || !is.null(alltFilter)) && !is.null(pltx)) {
      ## TREE table
      if (istree || !is.null(alltFilter)) {
        TREE <- FIESTA::DBgetCSV("TREE", stabbr, returnDT=TRUE, stopifnull=FALSE)
      }

      message("\n",
      	"## STATUS: Getting tree data from TREE (", stabbr, ") ...", "\n")
      if (is.null(treevarlst) & is.null(tsumvarlst)) {
        treex <- NULL
        istree <- FALSE
      } else {
        ttvars <- toString(paste0("t.", c(treevarlst, tsumvarlst)))
        treeqry <- paste("select distinct", ttvars, "from", tfromqry, "where", xfilter)

        if (datsource == "sqlite") {
          treex <- DBI::dbGetQuery(dbconn, treeqry)
        } else {
          treex <- sqldf::sqldf(treeqry, stringsAsFactors=FALSE)
        }
        if (nrow(treex) != 0) {
          treex <- setDT(treex)
          treex[, PLT_CN := as.character(PLT_CN)]
          setkey(treex, PLT_CN, CONDID)

          ## Subset overall filters from condx
          treex <- treex[paste(treex$PLT_CN, treex$CONDID) %in% pcondID,]

          ## Filter treex with alltFilter      
          ###########################################
          if (!is.null(alltFilter)) {
            treex <- FIESTA::datFilter(x=treex, xfilter=alltFilter)$xf
            if (is.null(treex)) {
              pltx=condx <- NULL
            } else {
              pltx <- pltx[pltx$CN %in% treex$PLT_CN, ]
              condx <- condx[condx$PLT_CN %in% treex$PLT_CN, ]
            }
          }

          if (istree && !is.null(treex)) {
            ## Check ACI
            if (!ACI) {
              ACIpltID <- condx[COND_STATUS_CD == 1, paste(PLT_CN, CONDID)]
              treex <- treex[paste(treex$PLT_CN, treex$CONDID) %in% ACIpltID,]
            } 

            ## Write query to outfolder
            if (saveqry) {
              treeqryfn <- DBgetfn("tree", invtype, outfn.pre, stabbr, 
			evalid=evalid, qry=TRUE, outfolder=outfolder, 
			overwrite=overwrite_layer, outfn.date=outfn.date, ext="txt")
              outfile <- file(treeqryfn, "w")
              cat(  treeqryfn, "\n", file=outfile)
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
              sppsql <- paste("select SPCD,", paste(sppvars, collapse=","), 
				"from REF_SPECIES")
              ref_spp <- sqldf::sqldf(sppsql)

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
            if (treeReturn && returndata) {
              tree <- rbind(tree, treex)
            }
          }
          rm(TREE)
          gc()
        }
      }
    }

    ##############################################################
    ## Plot counts and spatial data
    ##############################################################
    if (!is.null(pltx)) {
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
      #xyx <- pltx[, c("CN", getcoords(coords), "PLOT_ID"), with=FALSE]
      xyx <- copy(pltx)
      setnames(xyx, "CN", "PLT_CN")

      ## Get xy for the most current sampled plot
      if (xymeasCur) {
        xyfromqry <- getpfromqry(Endyr=measEndyr, SCHEMA.=SCHEMA.,
		subcycle99=subcycle99, intensity1=intensity1, plotnm="pltx")

        xvars <- c("p.CN", "p.STATECD", "p.UNITCD", "p.COUNTYCD", "p.PLOT", 
		"p.PLOT_ID", paste0("p.", getcoords(coords)))
        xyx.qry <- paste("select distinct", toString(xvars), "from", xyfromqry)
        xyCurx <- sqldf::sqldf(xyx.qry)
        names(xyCurx)[names(xyCurx) == "CN"] <- "PLT_CN"
        xyCurx$COUNTYFIPS <- paste0(formatC(xyCurx$STATECD, width=2, digits=2, flag=0), 
          	formatC(xyCurx$COUNTYCD, width=3, digits=3, flag=0))
        assign(paste0("xyCurx_", coords), xyCurx) 
        if (returndata) {
          assign(paste0("xyCur_", coords), 
				rbind(get(paste0("xyCur_", coords)), xyCurx)) 
        }
      } else {
        xyx <- xyx[, c("PLT_CN", "STATECD", "UNITCD", "COUNTYCD", "PLOT", 
		"LON_PUBLIC", "LAT_PUBLIC", "PLOT_ID"), with=FALSE]
        xyx$COUNTYFIPS <- paste0(formatC(xyx$STATECD, width=2, digits=2, flag=0), 
          	formatC(xyx$COUNTYCD, width=3, digits=3, flag=0))
        assign(paste0("xyx_", coords), xyx)
        if (returndata) {
          assign(paste0("xy_", coords), 
				rbind(get(paste0("xy_", coords)), xyx))
        }
      } 
    }
 
    ##############################################################
    ## Seedling data (SEEDLING)
    ##############################################################
    if (isseed && !is.null(pltx)) {

      if (is.null(seedvarlst)) {
        seedx <- NULL
        isseed <- NULL
      } else {
        message("\n",
      	"## STATUS: Getting seed data from SEEDLING (", stabbr, ") ...", "\n")

        ssvars <- toString(paste0("s.", c(seedvarlst, ssumvarlst)))
        seedqry <- paste("select distinct", ssvars, "from", sfromqry, "where", xfilter)

        if (datsource == "sqlite") {
          seedx <- DBI::dbGetQuery(dbconn, seedqry)
        } else {
          seedx <- sqldf::sqldf(seedqry, stringsAsFactors=FALSE)
        }
        if (nrow(seedx) != 0) {
          seedx <- setDT(seedx)
          seedx[, PLT_CN := as.character(PLT_CN)]
          setkey(seedx, PLT_CN, CONDID)

          ## Subset overall filters from pltx
          seedx <- seedx[seedx$PLT_CN %in% unique(pltx$CN),]

          ## Subset overall filters from condx
          seedx <- seedx[paste(seedx$PLT_CN, seedx$CONDID) %in% pcondID,]

          ## Check ACI
          if (!ACI) {
            ACIplts <- condx[COND_STATUS_CD == 1, paste(PLT_CN, CONDID)]
            seedx <- seedx[paste(seedx$PLT_CN, seedx$CONDID) %in% ACIplts,]
          } 

          ## Write query to outfolder
#          if (saveqry) {
#            seedqryfnbase <- DBgetfn("seed", invtype, outfn.pre, stabbr, 
#			evalid=evalid, qry=TRUE, outfn.date=outfn.date)
#            seedqryfn <- FIESTA::fileexistsnm(outfolder, seedqryfnbase, "txt")
#            outfile <- file(paste0(outfolder, "/", seedqryfn, ".txt"), "w")
#              cat(  paste0(seedqry, xfilter), "\n", file=outfile)
#            close(outfile)
#          }

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

      if (returndata) {
        ## Append data
        seed <- rbind(seed, seedx)
      }
    }

    ##############################################################
    ## Understory vegetation data (P2VEG_SUBPLOT_SPP/P2VEG_SUBP_STRUCTURE
    ##############################################################
    if (isveg && !is.null(pltx)) {
      message("\n",
      "## STATUS: Getting veg data from P2VEG_SUBPLOT_SPP/P2VEG_SUBP_STRUCTURE (", 
		stabbr, ") ...", "\n")

      ## Get data for P2VEG_SUBPLOT_SPP
      vsubpsppvars <- toString(paste0("v.", vsubpsppvarlst))
      vsubpsppqry <- paste("select distinct", vsubpsppvars, "from", vfromqry, 
		"where", paste0(evalFilter.veg, stateFilters))

      if (datsource == "sqlite") {
        vsubpsppx <- DBI::dbGetQuery(dbconn, vsubpsppqry)
      } else {
        vsubpsppx <- sqldf::sqldf(vsubpsppqry, stringsAsFactors=FALSE)
      }
      if (nrow(vsubpsppx) != 0) {
        vsubpsppx <- setDT(vsubpsppx)
        vsubpsppx[, PLT_CN := as.character(PLT_CN)]
        setkey(vsubpsppx, PLT_CN)

        ## Subset overall filters from condx
        vsubpsppx <- vsubpsppx[paste(vsubpsppx$PLT_CN, vsubpsppx$CONDID) %in% pcondID,]
      }

      ## Get data for P2VEG_SUBP_STRUCTURE
      vsubpstrvars <- toString(paste0("v.", vsubpstrvarlst))
      vsubpstrqry <- paste("select distinct", vsubpstrvars, "from", vstrfromqry, 
		"where", paste0(evalFilter.veg, stateFilters))
      vsubpstrx <- sqldf::sqldf(vsubpstrqry, stringsAsFactors=FALSE)

      if(nrow(vsubpstrx) != 0){
        vsubpstrx <- setDT(vsubpstrx)
        vsubpstrx[, PLT_CN := as.character(PLT_CN)]
        setkey(vsubpstrx, PLT_CN)

        ## Subset overall filters from condx
        vsubpstrx <- vsubpstrx[paste(vsubpstrx$PLT_CN, vsubpstrx$CONDID) %in% pcondID,]
      }
      if (returndata) {
        vsubpspp <- rbind(vsubpspp, vsubpsppx)
        vsubpstr <- rbind(vsubpstr, vsubpstrx)
      }
    }

    ##############################################################
    ## Subplot data (SUBPLOT/SUBP_COND)
    ##############################################################
    if (issubp && !is.null(pltx)) {
      message("\n",
      "## STATUS: Getting subplot data from SUBPLOT/SUBP_COND (", stabbr, ") ...", "\n")

      ## Get data for SUBPLOT
      subpvars <- toString(paste0("subp.", subpvarlst))
      subpqry <- paste("select distinct", subpvars, "from", subpfromqry, 
		"where", paste0(evalFilter, stateFilters))

      if (datsource == "sqlite") {
        subpx <- DBI::dbGetQuery(dbconn, subpqry)
      } else {
        subpx <- sqldf::sqldf(subpqry, stringsAsFactors=FALSE)
      }
      if (nrow(subpx) != 0) {
        subpx <- setDT(subpx)
        subpx[, PLT_CN := as.character(PLT_CN)]
        setkey(subpx, PLT_CN)

        ## Subset overall filters from condx
        subpx <- subpx[subpx$PLT_CN %in% pltx$CN,]
      }

      ## Get data for SUBP_COND
      subpcvars <- toString(paste0("subpc.", subpcvarlst))
      subpcqry <- paste("select distinct", subpcvars, "from", subpcfromqry, 
		"where", paste0(evalFilter, stateFilters))
      subpcx <- sqldf::sqldf(subpcqry, stringsAsFactors=FALSE)

      if(nrow(subpcx) != 0){
        subpcx <- setDT(subpcx)
        subpcx[, PLT_CN := as.character(PLT_CN)]
        setkey(subpcx, PLT_CN)

        ## Subset overall filters from condx
        subpcx <- subpcx[paste(subpcx$PLT_CN, subpcx$CONDID) %in% pcondID,]
      }
      if (returndata) {
        subp <- rbind(subp, subpx)
        subpc <- rbind(subpc, subpcx)
      }
    }

    ##############################################################
    ## Down woody data (COND_DWM_CALC)
    ##############################################################
    if (isdwm && !is.null(pltx)) {
      message("\n",
      "## STATUS: Getting DWM data from COND_DWM_CALC (", stabbr, ") ...", "\n")
    
      if (is.null(dwmvarlst)) {
        dwmx <- NULL
        isdwm <- FALSE
      } else {

        dvars <- toString(paste0("d.", dwmvarlst))
        dwmqry <- paste("select distinct", dvars, "from", dfromqry, 
		"where", paste0(evalFilter.dwm, stateFilters))

        if (datsource == "sqlite") {
          dwmx <- DBI::dbGetQuery(dbconn, dwmqry)
        } else {
          dwmx <- sqldf::sqldf(dwmqry, stringsAsFactors=FALSE)
        }
        if (nrow(dwmx) != 0) {
          dwmx <- setDT(dwmx)
          dwmx[, PLT_CN := as.character(PLT_CN)]
          setkey(dwmx, PLT_CN, CONDID)

          ## Subset overall filters from condx
          dwmx <- dwmx[paste(dwmx$PLT_CN, dwmx$CONDID) %in% pcondID,]
        }
      }
      if (returndata) {
        dwm <- rbind(dwm, dwmx)
      }
    }


    ##############################################################
    ## Tree Change, Growth, and Mortality (TREE_GRM_COMPONENT)
    ##############################################################
    if (isgrm && !is.null(pltx)) {
      message("\n",
      "## STATUS: Getting GRM data from TREE_GRM_COMPONENT (", stabbr, ") ...", "\n")
    
      if (is.null(grmvarlst)) {
        grmx <- NULL
        isgrm <- NULL
      } else {

        #grmvars <- toString(paste0("grm.", grmvarlst))
        grmqry <- paste("select grm.* from", dfromqry, 
		"where", paste0(evalFilter.dwm, stateFilters))

        if (datsource == "sqlite") {
          grmx <- DBI::dbGetQuery(dbconn, grmqry)
        } else {
          grmx <- sqldf::sqldf(grmqry, stringsAsFactors=FALSE)
        }
        if (nrow(grmx) != 0) {
          grmx <- setDT(grmx)
          grmx[, PLT_CN := as.character(PLT_CN)]
          setkey(grmx, PLT_CN, CONDID)

          ## Subset overall filters from condx
          #grmx <- grmx[paste(grmx$PLT_CN, grmx$CONDID) %in% pcondID,]
          grmx <- grmx[grmx$PLT_CN %in% pltx$CN,]
        }
      }
      if (returndata) {
        grm <- rbind(grm, grmx)
      }
    }
 
    ##############################################################
    ## Other tables
    ##############################################################
    if (!is.null(othertables) && !is.null(pltx)) {
      for (j in 1:length(othertables)) {
        othertable <- othertables[j]
        othertablexnm <- paste0("otherx", j)

        cat("\n",
        "## STATUS: GETTING", othertable, "(", stabbr, ") ...", "\n")
    
        if (!is.null(pcheck.varchar(othertable, checklst=pop_tables, stopifinvalid=FALSE))) {
          xfromqry <- paste0(SCHEMA., othertable, " x")
          if (!iseval) {
            xfilterpop <- stFilter
            xfilterpop <- sub("p.", "x.", xfilterpop)
          } else {
            xfilterpop <- paste0("x.EVALID IN(", toString(evalid), ")")
          }
          xqry <- paste("select distinct x.* from", sub("SUBX", othertable, xfromqry), 
			"where", xfilterpop)

        } else {
          joinid <- "PLT_CN"
          xqry <- paste("select distinct x.* from", sub("SUBX", othertable, xfromqry), 
			"where", xfilter)
        }
        if (datsource == "sqlite") {
          tab <- tryCatch( DBI::dbGetQuery(dbconn, xqry),
			error=function(e) return(NULL))
        } else {
          tab <- tryCatch( sqldf::sqldf(xqry, stringsAsFactors=FALSE), 
			error=function(e) return(NULL))
        }
        if (is.null(tab)) {
          xqry <- paste("select * from", othertable, "where", stFilter)

          if (datsource == "sqlite") {
            tab <- tryCatch( DBI::dbGetQuery(dbconn, xqry),
			error=function(e) return(NULL))
          } else {
            tab <- tryCatch( sqldf::sqldf(xqry, stringsAsFactors=FALSE), 
			error=function(e) return(NULL))
          }
        }
 
        if (is.null(pcheck.varchar(othertable, checklst=pop_tables, stopifinvalid=FALSE))) {
          ## Subset overall filters from condx
          if ("CONDID" %in% names(tab)) {
            tab <- tab[paste(tab$PLT_CN, tab$CONDID) %in% pcondID,]
          } else {
            tab <- tab[tab[[joinid]] %in% unique(pltx$CN),]
          }
        }
        if (nrow(tab) == 0) {
          message("othertable must include PLT_CN")
          tab <- NULL
        }

        if (!is.null(tab)) {
          assign(othertablexnm, setDT(tab))

          if ("PLT_CN" %in% names(get(othertablexnm))) {
            get(othertablexnm)[, PLT_CN := as.character(PLT_CN)]
            setkey(get(othertablexnm), "PLT_CN")

            ## Subset overall filters from pltx
            assign(othertablexnm, 
			get(othertablexnm)[get(othertablexnm)[[joinid]] %in% unique(pltx$CN),])
          }
          if (returndata) 
            assign(paste0("other", j), rbind(get(paste0("other", j)), get(othertablexnm)))
        }
      }
    }

    ##############################################################
    ## If savePOP or more than one evalType
    ##############################################################
    if ((iseval || savePOP) && !is.null(pltx)) {
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
#        if (saveqry) {
#          ppsaqryfnbase <- DBgetfn("ppsa", invtype, outfn.pre, stabbr, 
#			evalid=evalid, qry=TRUE, outfn.date=outfn.date)
#          ppsaqryfn <- FIESTA::fileexistsnm(outfolder, ppsaqryfnbase, "txt")
#          outfile <- file(paste0(outfolder, "/", ppsaqryfn, ".txt"), "w")
#          cat(  paste0(ppsaqry, xfilter), "\n", file=outfile)
#          close(outfile)
#        }
      }
      if (returndata) {
        ppsa <- rbind(ppsa, ppsax)
      }
    }
 
    ###############################################################################
    ###############################################################################
    ## SAVE data
    ###############################################################################
    ###############################################################################
    if ((savedata || !treeReturn) && !is.null(pltx)) {
      message("saving data...")
      col.names <- ifelse (i == 1, TRUE, FALSE)
      if (i > 1) { 
        append_layer <- TRUE
      }
      if (append_layer && overwrite_layer) {
        overwrite_layer <- FALSE
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
 			overwrite_layer=overwrite_layer, append_layer=TRUE,
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
          if (!append_layer) index.unique.xyplt <- "PLT_CN"
          datExportData(xyplt, outfolder=outfolder, 
			out_fmt=out_fmt, out_dsn=out_dsn, out_layer=xynm, 
			outfn.date=outfn.date, overwrite_layer=overwrite_layer,
			index.unique=index.unique.xyplt, append_layer=append_layer,
			outfn.pre=outfn.pre)
        }
      }  
 
      if (savedata && !is.null(spconddatx)) {
        index.unique.spconddat <- NULL
        if (!append_layer) index.unique.spconddat <- "PLT_CN"
        datExportData(spconddat, outfolder=outfolder, 
			out_fmt=out_fmt, out_dsn=out_dsn, out_layer="spconddat", 
			outfn.date=outfn.date, overwrite_layer=overwrite_layer,
			index.unique=index.unique.spconddat, append_layer=append_layer,
			outfn.pre=outfn.pre)
      }
      if (savedata && !is.null(pltx)) {
        index.unique.pltx <- NULL
        if (i == 1) index.unique.pltx <- "CN"
        datExportData(pltx, outfolder=outfolder, 
			out_fmt=out_fmt, out_dsn=out_dsn, out_layer="plot", 
			outfn.date=outfn.date, overwrite_layer=overwrite_layer,
			index.unique=index.unique.pltx, append_layer=append_layer,
			outfn.pre=outfn.pre)
      }
      if (savedata && !is.null(condx)) {
        index.unique.condx <- NULL
        if (!append_layer) index.unique.condx <- c("PLT_CN", "CONDID")
        datExportData(condx, outfolder=outfolder, 
			out_fmt=out_fmt, out_dsn=out_dsn, out_layer="cond", 
			outfn.date=outfn.date, overwrite_layer=overwrite_layer,
			index.unique=index.unique.condx, append_layer=append_layer,
			outfn.pre=outfn.pre)
      }
      if (savedata && !is.null(sccmx)) {
        index.unique.sccmx <- NULL
        if (!append_layer) index.unique.sccmx <- c("PLT_CN")
        datExportData(sccmx, outfolder=outfolder, 
			out_fmt=out_fmt, out_dsn=out_dsn, out_layer="sccm", 
			outfn.date=outfn.date, overwrite_layer=overwrite_layer,
			index.unique=index.unique.sccmx, append_layer=append_layer,
			outfn.pre=outfn.pre)
      } 
      if (savedata && !is.null(treex)) {
        index.unique.treex <- NULL
        if (!append_layer) index.unique.treex <- c("PLT_CN", "CONDID", "SUBP", "TREE")
        datExportData(treex, outfolder=outfolder, 
			out_fmt=out_fmt, out_dsn=out_dsn, out_layer="tree", 
			outfn.date=outfn.date, overwrite_layer=overwrite_layer,
			index.unique=index.unique.treex, append_layer=append_layer,
			outfn.pre=outfn.pre)
      }
      if (savedata && !is.null(seedx)) {
        index.unique.seedx <- NULL
        if (!append_layer) index.unique.seedx <- c("PLT_CN", "CONDID", "SUBP")
        datExportData(seedx, outfolder=outfolder, 
			out_fmt=out_fmt, out_dsn=out_dsn, out_layer="seed", 
			outfn.date=outfn.date, overwrite_layer=overwrite_layer,
			index.unique=index.unique.seedx, append_layer=append_layer,
			outfn.pre=outfn.pre)
      } 
      if (savedata && !is.null(vsubpsppx)) {
        index.unique.vsubpsppx <- NULL
        if (!append_layer) index.unique.vsubpsppx <- c("PLT_CN", "CONDID")
        datExportData(vsubpsppx, outfolder=outfolder, 
			out_fmt=out_fmt, out_dsn=out_dsn, out_layer="vsubpspp", 
			outfn.date=outfn.date, overwrite_layer=overwrite_layer,
			index.unique=index.unique.vsubpsppx, append_layer=append_layer,
			outfn.pre=outfn.pre)

        index.unique.vsubpstrx <- NULL
        if (!append_layer) index.unique.vsubpstrx <- c("PLT_CN", "CONDID")
        datExportData(vsubpstrx, outfolder=outfolder, 
			out_fmt=out_fmt, out_dsn=out_dsn, out_layer="vsubpstr", 
			outfn.date=outfn.date, overwrite_layer=overwrite_layer,
			index.unique=index.unique.vsubpstrx, append_layer=append_layer,
			outfn.pre=outfn.pre)
      }
      if (savedata && !is.null(subpx)) {
        index.unique.subpx <- NULL
        if (!append_layer) index.unique.subpx <- "PLT_CN"
        datExportData(subpx, outfolder=outfolder, 
			out_fmt=out_fmt, out_dsn=out_dsn, out_layer="subplot", 
			outfn.date=outfn.date, overwrite_layer=overwrite_layer,
			index.unique=index.unique.subpx, append_layer=append_layer,
			outfn.pre=outfn.pre)

        index.unique.subpcx <- NULL
        if (!append_layer) index.unique.subpcx <- c("PLT_CN", "CONDID")
        datExportData(subpcx, outfolder=outfolder, 
			out_fmt=out_fmt, out_dsn=out_dsn, out_layer="subp_cond", 
			outfn.date=outfn.date, overwrite_layer=overwrite_layer,
			index.unique=index.unique.subpcx, append_layer=append_layer,
			outfn.pre=outfn.pre)
      }  

      if (savedata && !is.null(lulcx)) {
        index.unique.lulcx <- NULL
        if (!append_layer) index.unique.lulcx <- c("PLT_CN", "CONDID")
        datExportData(lulcx, outfolder=outfolder, 
			out_fmt=out_fmt, out_dsn=out_dsn, out_layer="lulc", 
			outfn.date=outfn.date, overwrite_layer=overwrite_layer,
			append_layer=append_layer, outfn.pre=outfn.pre)
      } 

      if (savedata && !is.null(dwmx)) {
        index.unique.dwmx <- NULL
        if (!append_layer) index.unique.dwmx <- c("PLT_CN", "CONDID")
        datExportData(dwmx, outfolder=outfolder, 
			out_fmt=out_fmt, out_dsn=out_dsn, out_layer="dwm_calc", 
			outfn.date=outfn.date, overwrite_layer=overwrite_layer,
			append_layer=append_layer, outfn.pre=outfn.pre)
      } 

      if (savedata && !is.null(othertables)) {
        for (j in 1:length(othertables)) {
          othertable <- othertables[j]
          othertablexnm <- paste0("otherx", j)
          othernm <- paste0("other", j)

          if (!is.null(get(othertablexnm))) {
            index.unique.other <- NULL
            if (othertable == "SUBPLOT") {
              index.unique.other <- c("PLT_CN", "SUBP")
            }
            datExportData(get(othertablexnm), outfolder=outfolder, 
			out_fmt=out_fmt, out_dsn=out_dsn, out_layer=tolower(othertable), 
			outfn.date=outfn.date, overwrite_layer=overwrite_layer, 
			index.unique=index.unique.other, append_layer=append_layer, 
			outfn.pre=outfn.pre)
          }
        }
      }  
      if (savedata && savePOP && !is.null(ppsax)) {
        #index.unique.ppsax <- NULL
        #if (i == 1) index.unique.ppsax <- "PLT_CN"
        datExportData(ppsax, outfolder=outfolder, 
			out_fmt=out_fmt, out_dsn=out_dsn, out_layer="pop_plot_stratum_assgn", 
			outfn.date=outfn.date, overwrite_layer=overwrite_layer,
			append_layer=append_layer, outfn.pre=outfn.pre)

      }
    }
    if (returndata) {
      plt <- rbind(plt, pltx)
      cond <- rbind(cond, condx)
    }
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
      "evalAll <- ", params$evalAll, "\n",    
      "evalType <- \"", FIESTA::getlistparam(params$evalType), "\"", "\n",
      "measCur <- ", params$measCur, "\n",
      "measEndyr <- ", FIESTA::getlistparam(params$measEndyr), "\n",
      "allyrs <- ", params$allyrs, "\n",
      "invyrs <- ", FIESTA::getlistparam(params$invyrs), "\n",  
      "istree <- ", params$istree, "\n",
      "isseed <- ", params$isseed, "\n",
      "isveg <- ", params$isveg, "\n",
      "issubp <- ", params$issubp, "\n",
      "isdwm <- ", params$isdwm, "\n",
      "issp <- ", params$issp, "\n",
      "spcond <- ", params$spcond, "\n", 
      "spcondid1 <- ", params$spcondid1, "\n",
      "defaultVars <- ", params$defaultVars, "\n",
      "regionVars <- ", params$regionVars, "\n",
      "ACI <- ", params$ACI, "\n",
      "subcycle99 <- ", params$subcycle99, "\n",
      "intensity1 <- ", params$intensity1, "\n",
      "allFilter <- \"", params$xfilters, "\"", "\n",
      "savedata <- ", params$savedata, "\n",
      "saveqry <- ", params$saveqry, "\n",
      "outfolder <- \"", params$outfolder, "\"", "\n",
      "out_dsn <- \"", params$out_dsn, "\"", "\n",
      "gpkg <- ", params$gpkg, "\n",
      "outfn.pre <- \"", params$outfn.pre, "\"", "\n",
      "outfn.date <- ", params$outfn.date, "\n",
      "overwrite <- ", params$overwrite, "\n",
      "savePOP <- ", params$savePOP, "\n",
     "\n",
    file = outfile, sep="")

    cat(  "fiadat <- DBgetPlots(states=states, RS=RS, invtype=invtype, evalid=evalid, 
	evalCur=evalCur, evalEndyr=evalEndyr, evalAll=evalAll, evalType=evalType, 
	measCur=measCur, measEndyr=measEndyr, allyrs=allyrs, invyrs=invyrs, istree=istree, 
	isseed=isseed, isveg=isveg, issubp=issubp, isdwm=isdwm, 
	issp=issp, spcondid1=spcondid1, defaultVars=defaultVars, regionVars=regionVars, 
	ACI=ACI, subcycle99=FALSE, intensity1=TRUE, allFilter=allFilter, 
	savedata=savedata, saveqry=saveqry, parameters=parameters, outfolder=outfolder, 
	out_dsn=out_dsn, gpkg=gkpg, outfn.pre=outfn.pre, outfn.date=outfn.date,
	overwrite=overwrite, savePOP=savePOP)",
    file = outfile, sep="")

    close(outfile)
  }
 
  ## Write out plot/condition counts to comma-delimited file.
  if (savedata) {
    datExportData(pltcnt, outfolder=outfolder, 
			out_fmt=out_fmt, out_dsn=out_dsn, out_layer="pltcnt", 
			outfn.date=outfn.date, overwrite_layer=overwrite_layer,
			append_layer=append_layer, outfn.pre=outfn.pre)
  }


  ## GENERATE RETURN LIST
  fiadatlst <- list(states=states)

  if (returndata) {
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
    if (istree && !is.null(tree)) {
      fiadatlst$tree <- setDF(tree)
    }
    if (isseed && !is.null(seed)) {
      fiadatlst$seed <- setDF(seed)
    }
    if (issccm && !is.null(sccm)) {
      fiadatlst$sccm <- setDF(sccm)
    }
    if (isveg) {
      if (!is.null(vsubpspp)) fiadatlst$vsubpspp <- setDF(vsubpspp)
      if (!is.null(vsubpstr)) fiadatlst$vsubpstr <- setDF(vsubpstr)
    }
    if (issubp) {
      if (!is.null(subpx)) fiadatlst$subplot <- setDF(subpx)
      if (!is.null(subpcx)) fiadatlst$subp_cond <- setDF(subpcx)
    }
    if (islulc) {
      if (!is.null(lulc)) fiadatlst$lulc <- setDF(lulc)
    }
    if (isdwm) {
      if (!is.null(dwm)) fiadatlst$dwm <- setDF(dwm)
    }
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
        #assign(xyCurnm, get(paste0("xyCur_", coords))) 
        fiadatlst[[xyCurnm]] <- get(paste0("xyCur_", coords))
      } else {
        xynm <- paste0("xy_", coords)
        #assign(xynm, get(paste0("xy_", coords))) 
        fiadatlst[[xynm]] <- get(paste0("xy_", coords))
      }
    }

    if (!is.null(spconddat)) {
      fiadatlst$spconddat <- setDF(spconddat)
    }
    if (savePOP || (iseval && length(evalidlist) > 1) && !is.null(ppsa)) {
      fiadatlst$pop_plot_stratum_assgn <- setDF(ppsa)
    }
  }
 
  if (length(evalidlist) > 0) fiadatlst$evalid <- evalidlist
  fiadatlst$pltcnt <- pltcnt

  if (!is.null(evalidlist)) {
    evaliddf <- data.frame(do.call(rbind, evalidlist))
    stcds <- FIESTA::pcheck.states(row.names(evaliddf), "VALUE")
    evaliddf <- data.frame(stcds, row.names(evaliddf), evaliddf, row.names=NULL)
    names(evaliddf) <- c("STATECD", "STATE", "EVALID")
    evaliddf <- evaliddf[order(evaliddf$STATECD), ]
    fiadatlst$evalid <- evalidlist

#    if (savedata) {
#      append_layer2 <- ifelse(overwrite, FALSE, append_layer)
#      datExportData(evaliddf, outfolder=outfolder, 
#			out_fmt=out_fmt, out_dsn=out_dsn, out_layer="evalid", 
#			outfn.date=outfn.date, overwrite_layer=overwrite,
#			append_layer=append_layer2, outfn.pre=outfn.pre)
#    }
  }
  
  #if (saveqry) cat("\n", paste("Saved queries to:", outfolder), "\n") 

 

  ## Return data list
  return(fiadat=fiadatlst)
}

