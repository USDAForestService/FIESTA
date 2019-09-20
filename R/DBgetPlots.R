DBgetPlots <- function (datsource="ORACLE", ZIP=TRUE, FS_FIADB=TRUE, states=NULL, 
	rs=NULL, invtype="ANNUAL", evalid=NULL, evalCur=FALSE, evalEndyr=NULL, 
	evalType="ALL", allyrs=FALSE, invyrs=NULL, actual=FALSE, istree=FALSE, 
	isseed=FALSE, isveg=FALSE, isdwm=FALSE, issp=FALSE, spcoords=NULL, 
	spcond=TRUE, spcondid1=FALSE, defaultVars=TRUE, regionVars=FALSE, ACI=FALSE, 
	subcycle99=FALSE, intensity1=TRUE, allFilter=NULL, savedata=FALSE, 
	saveqry=FALSE, parameters=FALSE, outfolder=NULL, outfn.pre=NULL, outfn.date=FALSE, 
	overwrite=FALSE, dbconn=NULL, dbconnopen=FALSE, returnPOP=FALSE, gui=FALSE) {


  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  if (!gui)
    gui <- ifelse(nargs() == 0 || (nargs() == 1 & !is.null(dbconn)) ||
		(nargs() == 2 & !is.null(dbconn) & dbconnopen), TRUE, FALSE)
  if (gui) 
    ZIP=invtype=evalCur=allyrs=evalType=istree=isseed=isveg=issp=
	shpcondid=defaultVars=regionVars=ACI=actual=subcycle99=intensity1=
	allFilter=savedata=saveqry=parameters=BIOJENK_kg=BIOJENK_lb=PREV_PLTCN <- NULL

  ## Set global variables  
  CN=CONDID=COND_STATUS_CD=PLT_CN=FORTYPCD=cvars=pvars=tvars=treenavars=svars=seednavars=
	vspsppvars=vspstrvars=dvars=tsvars=vars=filtervarlst=filterpvarlst=
	filtercvarlst=filterpvarflst=filtercvarflst=filterpvar10lst=filtercvar10lst=
	SUBPPROP_UNADJ=MICRPROP_UNADJ=TPA_UNADJ=TPAMORT_UNADJ=TPAREMV_UNADJ=SEEDCNT6=
	TREECOUNT_CALC=SEEDSUBP6=LIVE_CANOPY_CVR_PCT=CONDPROP_UNADJ=PLOT_NONSAMPLE_REASN_CD=
	PLOT_STATUS_CD=BA=DIA=DRYBIO_AG=DRYBIO_BOLE=DRYBIO_STUMP=DRYBIO_TOP=DRYBIO_SAPLING=
	DRYBIO_WDLD_SPP=TREEAGE=BHAGE=TOTAGE=STATUSCD=PROP=CRCOVPCT_RMRS=TIMBERCD=SITECLCD=
	RESERVCD=tsumvars=plotvars=condvars=tsumvarlst=JENKINS_TOTAL_B1=
	JENKINS_TOTAL_B2=POP_PLOT_STRATUM_ASSGN=NF_SAMPLING_STATUS_CD=NF_COND_STATUS_CD=
	ACI_NFS=OWNCD <- NULL


  ## SET OPTIONS
  options.old <- options()
  options(scipen=8) # bias against scientific notation
  on.exit(options(options.old), add=TRUE) 

  ## Set maxstates 
  ###########################################################
  ##  The number of states to append together, while still small enough to return 
  ##  as objects (without memory issues). This includes all tables except tree table..  
  ##  If there is more than 1 state with more than 6 inventory years and no filters,  
  ##  the tree table will not be returned as an object.. only written to outfolder.
  maxstates <- ifelse(!is.null(allFilter), 20, 100)   
  maxstates.tree <- ifelse(!is.null(allFilter), 3, 10)  
  biojenk <- FALSE 
  greenwt <- TRUE
     

  ## Define functions
  ###########################################################
  getcoords <- function(coords){
    switch(coords,
      ACTUAL = c("LON_ACTUAL", "LAT_ACTUAL"),
      PUBLIC = c("LON_PUBLIC", "LAT_PUBLIC"),
      DIGITIZED = c("LON_DIGITIZED", "LAT_DIGITIZED"))
  }


  ########################################################################
  ### GET PARAMETERS 
  ########################################################################
  iseval <- FALSE

  ## Get states, Evalid and/or invyrs info
  evalInfo <- DBgetEvalid(datsource=datsource, ZIP=ZIP, FS_FIADB=FS_FIADB, 
	states=states, rs=rs, invtype=invtype, evalid=evalid, evalCur=evalCur, 
	evalEndyr=evalEndyr, evalType=evalType, dbconn=dbconn, dbconnopen=TRUE, 
	isdwm=isdwm, gui=gui)
  states <- evalInfo$states
  rslst <- evalInfo$rslst
  evalidlist <- evalInfo$evalidlist
  invtype <- evalInfo$invtype
  invyrtab <- evalInfo$invyrtab
  if (length(evalidlist) > 0) {
    invyrs <- evalInfo$invyrs
    iseval <- TRUE
  }
  datsource <- evalInfo$datsource
  if (datsource == "CSV") 
    ZIP <- evalInfo$ZIP  
  if (datsource == "ORACLE") {
    dbconn <- evalInfo$dbconn
    FS_FIADB <- evalInfo$FS_FIADB
  }

  ### GET rs & rscd
  ###########################################################
  isRMRS <- ifelse(length(rslst) == 1 && rslst == "RMRS", TRUE, FALSE) 
    
  if (datsource == "ORACLE") {

    ## Set table schemas and aliases
    SCHEMA <- "FS_FIADB"

    if (length(rslst) == 1) NIMS_UNIT <- rslst
    if (!FS_FIADB && length(rslst) == 1) {
      SCHEMA <- paste0("FS_NIMS_FIADB_", NIMS_UNIT)	## NIMS FIADB REGIONAL TABLES
    } else {
      FS_FIADB <- TRUE
    }
    SCHEMA. <- paste0(SCHEMA, ".")

    ## Check actual
    ###########################################################
    if (length(rslst) == 1) {
      actual <- FIESTA::pcheck.logical(actual, varnm="actual", 
		title="Actual coords & data?", first="YES", gui=gui)
    } else {
      actual <- FIESTA::pcheck.logical(actual, varnm="actual", 
		title="Actual coords & data?", first="YES")
      if (is.null(actual)) actual <- FALSE
      if (actual) {
        warning ("actual coordinates are not available for more than 1 FIA Unit")
        actual <- FALSE
      }
    }
    if (actual) {
      if (length(rslst) > 1) {
        message("cannot get coordinates for more than one state... setting actual=FALSE")
        actual <- FALSE
      } else {
        if (!isRMRS)
          message("must have select permission for regional NIMS SDS table")
               
        SDS <- paste0("FS_NIMS_FIADB_", NIMS_UNIT) 	## SDS FIADB REGIONAL TABLES      
        SDSptabnm <- paste0(SDS, ".SDS_PLOT")        	## REGIONAL ACTUAL COORDINATES
        SDSctabnm <- paste0(SDS, ".SDS_COND")        	## REGIONAL ACTUAL COND VARIABLES

        ## Table aliases
        SDSPa <- "SDSp"
        SDSCa <- "SDSc"
      }
    }
  } else {
    actual <- FALSE
    SCHEMA <- ""
    SCHEMA. <- ""
  }

  ## Get state abbreviations and codes 
  ###########################################################
  stabbrlst <- FIESTA::pcheck.states(states, statereturn="ABBR")
  stcdlst <- FIESTA::pcheck.states(states, statereturn="VALUE")

  ## Get number of states 
  nbrstates <- length(states)

  ## If using EVALID, you don't need to get INVYRS, intensity, or subcycle
  if (!iseval) {   

    ### GET allyrs
    ###########################################################
    allyrs <- FIESTA::pcheck.logical(allyrs, varnm="allyrs", title="All years?", 
		first="YES", gui=gui)
 
    ## GETS INVYR(S) 
    ###########################################################
    if (is.null(invyrs) || length(invyrs) == 0) {
      #if (!gui) stop("invalid invyrs")
      invyrs <- sapply(states, function(x) NULL)
      for (state in states) { 
        stabbr <- FIESTA::pcheck.states(state, "ABBR")
        stinvyrlst <- sort(invyrtab[invyrtab$STATENM == state, "INVYR"])

        if (allyrs) {
          invyr <- stinvyrlst
        } else {
          ## GET INVENTORY YEAR(S) FROM USER
          invyr <- select.list(as.character(stinvyrlst), 
			title=paste("Inventory year(s) -", stabbr), multiple=TRUE)
          if (length(invyr) == 0) stop("")
        }
        invyrs[[state]] <- as.numeric(invyr)
      }
    } else {
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
  nbrinvyrs <- length(unlist(invyrs))
  
  ## GETS DATA TABLES (OTHER THAN PLOT/CONDITION) IF NULL
  ###########################################################
  if (invtype == "PERIODIC") isveg <- FALSE 
  if (all(!rslst %in% c("RMRS", "PNWRS"))) isveg <- FALSE

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

  ## Check defaultVars
  ###########################################################
  defaultVars <- FIESTA::pcheck.logical(defaultVars, varnm="defaultVars", 
		title="Default variables?", first="YES", gui=gui)

  ## Check regionalVars
  ###########################################################
  regionVars <- FIESTA::pcheck.logical(regionVars, varnm="regionVars", 
		title="Regional variables?", first="NO", gui=gui)

  ## Check statefilter
  ###########################################################
  stateFilter <- ifelse ((gui && is.null(allFilter)), TRUE, FALSE)

  ## Check issp
  ###########################################################
  issp <- FIESTA::pcheck.logical(issp, varnm="issp", 
		title="Spatial file of plot vars?", first="YES", gui=gui)


  ########################################################################
  ## Get the type of coordinates to use to make Spatial object
  ########################################################################
  if (issp) {
    spcoordslst <- c("ACTUAL", "PUBLIC")
    if (is.null(spcoords)) {
      if (actual == TRUE) {
        if (length(rslst) > 1) {
          warning("actual coordinates are only available for RMRS.. ", 
			"a shapefile will be generated with PUBLIC coords.")
          spcoords <- "PUBLIC"
        } else {
          spcoords <- select.list(spcoordslst, title="Select coordinate type", 
              preselect="ACTUAL", multiple=TRUE)
          if (length(spcoords) == 0) spcoords <- "ACTUAL"
        }
      } else {
        spcoords <- "PUBLIC"
      }
    }
    if (any(spcoords %in% c("ACTUAL", "DIGITIZED")) && !actual) {
      if (datsource == "CSV") {
        warning("only public coordinates available for CSV datsource")
        spcoords <- "PUBLIC"
      } else {
        stop("you must set actual=TRUE to generate a shapefile")
      }
    } else if (!all(spcoords %in% spcoordslst)){ 
      not <- spcoords[which(!spcoords %in% spcoordslst)]
      stop(paste("check spcoords.. invalid value:", not))
    }
  }

  if (issp)
    ## Check savedata
    ###########################################################
    spcondid1 <- FIESTA::pcheck.logical(spcondid1, varnm="spcondid1", 
		title="Use cond1 for shp?", first="YES", gui=gui)

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

  ## Check dbconnopen
  if (datsource == "ORACLE")
    dbconnopen <- FIESTA::pcheck.logical(dbconnopen, varnm="dbconnopen", 
		title="Keep ODBC open?", first="NO", gui=gui)

  ## Check returnPOP
  if (iseval && datsource == "CSV")    
    returnPOP <- FIESTA::pcheck.logical(returnPOP, varnm="returnPOP", 
		title="Return POP table", first="FALSE", gui=gui)


  ## GET OUTFOLDER IF NULL
  ###########################################################
  treeReturn <- TRUE
  datReturn <- TRUE	
 # if (istree && nbrstates > 2 && is.null(allFilter) && nbrinvyrs > 6) {
  if (istree && nbrstates > maxstates.tree && is.null(allFilter) && nbrinvyrs > 6) {
    warning("tree data object is too big.. writing to folder, no returned object")
    savedata <- TRUE
    treeReturn <- FALSE
  }
 
  if (nbrstates >= maxstates) {
    warning("data objects too big.. writing to outfolder, no returned objects")
    datReturn <- FALSE
    savedata <- TRUE
    treeReturn <- FALSE
  }

  if (savedata | saveqry | parameters | !datReturn | !treeReturn) {
    outfolder <- FIESTA::pcheck.outfolder(outfolder, gui=gui)
  
    ## Check outfn.pre
#    if (is.null(outfn.pre) || gsub(" ", "", outfn.pre) == "") {
#      outfn.pre <- ""
#    } else {
#      outfn.pre <- paste0(outfn.pre, "_")
#    }

    outfn.date <- FIESTA::pcheck.logical(outfn.date, varnm="outfn.date", 
		title="Add date to outfile", first="FALSE", gui=gui)
  }
 
  #####################################################################################
  #########################      ACTUAL VARIABLE LISTS     ############################
  #####################################################################################

  if (actual) {
    ## VARIABLES
    SDSpvarlst <- c("PLT_CN", "LON", "LAT", "ACTUAL_LON", "ACTUAL_LAT", "PLOT_PERIODIC")
    SDScvarlst <- c("CONDID", "ACTUAL_OWNCD", "ACTUAL_FORINDCD")

    SDSpvars <- paste("SDSp", SDSpvarlst, sep=".", collapse=", ")
    SDScvars <- paste("SDSc", SDScvarlst, sep=".", collapse=", ")

    ACTUALvarlst <- c(SDSpvarlst, SDScvarlst)
    ACTUALvars <- paste(SDSpvars, SDScvars, sep=", ")
    ACTUALpvars <- SDSpvarlst
    filterACTUALlst <- c("PLOT_PERIODIC", "ACTUAL_OWNCD", "ACTUAL_FORINDCD")

  } else {
    ACTUALcond <- NULL
  }


  #####################################################################################
  #####################################################################################
  ##############################      GET VARIABLES     ###############################
  #####################################################################################
  #####################################################################################
  #extractlist <- function(lst) {
  #  for (nm in names(lst)) assign(nm, lst[[nm]], envir=.GlobalEnv) }
  
  DBvars <- DBgetvars(invtype, defaultVars, istree, isseed, isveg, isdwm,
	regionVars, isRMRS, FS_FIADB, NIMS_UNIT, datsource, dbconn)
  for (nm in names(DBvars)) assign(nm, DBvars[[nm]])
  for (nm in names(filtervarlst)) assign(nm, filtervarlst[[nm]])
 
  #####################################################################################
  ##############################      BUILD QUERIES     ###############################
  #####################################################################################

  ## PERIODIC DATA
  #############################################
  ## PERIODIC DATA ARE FOUND IN FIADB TABLES (INCLUDES FORESTED PLOTS (>= 5% cover)) 

  if (invtype == "PERIODIC") {
    if (isveg) {
      cat("Currently understory vegetation data only available for annual data.", "\n" )
      isveg <- FALSE
    }
  }

  ##########################################
  ## NO REGION VARIABLES; ANNUAL OR PERIODIC
  ##########################################
  ## ANNUAL DATA INCLUDES FORESTED PLOTS (>= 10% cover) 

  ## PLOT from/join query
  if (iseval) {
    ppsatab <- paste0(SCHEMA., "POP_PLOT_STRATUM_ASSGN ppsa")
    pfromqry <- paste0(ppsatab, " JOIN ", SCHEMA., "PLOT p ON (p.CN = ppsa.PLT_CN)")
  } else {
    pfromqry <- paste0(SCHEMA., "PLOT p")
  }

  ## Cond from/join query
  fromqry <- paste0(pfromqry, " JOIN ", SCHEMA., "COND c ON (c.PLT_CN = p.CN)")

  ## ACTUAL query
  ################################################
  if (actual) 
    ACTUALfromqry <- paste0(fromqry, " JOIN ", SDSptabnm, " SDSp ON (SDSp.PLT_CN = p.CN)",
		" JOIN ", SDSctabnm, " SDSc ON (SDSc.CND_CN = c.CN)")

  ## TREE query
  ################################################
  if (istree) {
    if (iseval) {
      tfromqry <- paste0(ppsatab, " JOIN ", SCHEMA., 
		"TREE t ON (t.PLT_CN = ppsa.PLT_CN)")
    } else {
      #tfromqry <- paste0(SCHEMA., "TREE t")
      tfromqry <- paste0(SCHEMA., "PLOT p JOIN ", SCHEMA., "TREE t ON (t.PLT_CN = p.CN)")
      if (datsource == "CSV") 
        tfromqry <- sub(SCHEMA., "", tfromqry)
   }
  }

  ## SEED query
  ################################################
  if (isseed) {
    if (iseval) {
      sfromqry <- paste0(ppsatab, " JOIN ", SCHEMA., 
		"SEEDLING s ON (s.PLT_CN = ppsa.PLT_CN)")
    } else {
      sfromqry <- paste0(SCHEMA., "PLOT p JOIN ", SCHEMA., "SEEDLING s ON (s.PLT_CN = p.CN)")
      if (datsource == "CSV") 
        sfromqry <- sub(SCHEMA., "", sfromqry)
    }
  }

  ## DWM query
  ################################################
  if (isdwm) {
    dfromqry <- paste0(SCHEMA., "PLOT p JOIN ", SCHEMA., "COND_DWM_CALC d ON (d.PLT_CN = p.CN)")
    if (datsource == "CSV") 
      dfromqry <- sub(SCHEMA., "", dfromqry)
  }

  ## VEG query
  ################################################
  if (isveg) {
    vfromqry <- paste0(fromqry, " JOIN ", SCHEMA., 
		"P2VEG_SUBPLOT_SPP v ON v.PLT_CN = p.CN")
    vstrfromqry <- paste0(fromqry, " JOIN ", SCHEMA., 
		"P2VEG_SUBP_STRUCTURE v ON v.PLT_CN = p.CN")

    if (datsource == "CSV") {
      vfromqry <- sub(SCHEMA., "", vfromqry)
      vstrfromqry <- sub(SCHEMA., "", vstrfromqry)
    }
  }


  #####################################################################################
  #############################      SET OUTFILE NAMES    #############################
  #####################################################################################
  if (savedata | nbrstates > maxstates) {

    ## PLOT data
    outpltfn <- DBgetfn("plt", invtype, outfn.pre, stabbrlst, evalid=evalidlist,
		outfn.date=outfn.date)
 
    if (!overwrite) 
      outpltfn <- FIESTA::fileexistsnm(outfolder, outpltfn, "csv")
    path.outpltfn <- paste0(outfolder, "/", outpltfn, ".csv")

    ## COND data
    outcondfn <- DBgetfn("cond", invtype, outfn.pre, stabbrlst, evalid=evalidlist,
		outfn.date=outfn.date)
    if (!overwrite)
      outcondfn <- FIESTA::fileexistsnm(outfolder, outcondfn, "csv")
    path.outcondfn <- paste0(outfolder, "/", outcondfn, ".csv")

    ## ACTUAL data
    if (actual) {  
      outactualcfn <- DBgetfn("actualc", invtype, outfn.pre, stabbrlst, 
		evalid=evalidlist, outfn.date=outfn.date)
      if (!overwrite)
        outactualcfn <- FIESTA::fileexistsnm(outfolder, outactualcfn, "csv")
      path.outactualcfn <- paste0(outfolder, "/", outactualcfn, ".csv")

      outactualpfn <- DBgetfn("actualp", invtype, outfn.pre, stabbrlst, 
		evalid=evalidlist, outfn.date=outfn.date)
      if (!overwrite) 
        outactualpfn <- FIESTA::fileexistsnm(outfolder, outactualpfn, "csv")
      path.outactualpfn <- paste0(outfolder, "/", outactualpfn, ".csv")
    }

    ## TREE data
    if (istree) {  ## SET TREE FILE NAME      
      ## The tree data will be written to files by state.
      outtreecfn <- DBgetfn("tree", invtype, outfn.pre, stabbrlst, 
		evalid=evalidlist, outfn.date=outfn.date)
      if (overwrite) 
        outtreecfn <- FIESTA::fileexistsnm(outfolder, outtreecfn, "csv")
      path.outtreecfn <- paste0(outfolder, "/", outtreecfn, ".csv")
    }

    ## SEED data
    if (isseed) {  ## SET SEED FILE NAME
      outseedfn <- DBgetfn("seed", invtype, outfn.pre, stabbrlst, 
		evalid=evalidlist, outfn.date=outfn.date)
      if (!overwrite)
        outseedfn <- FIESTA::fileexistsnm(outfolder, outseedfn, "csv")
      path.outseedfn <- paste0(outfolder, "/", outseedfn, ".csv")
    }

    ## Plot counts
    outpltcntfn <- DBgetfn("pltcnt", invtype, outfn.pre, stabbrlst, evalid=evalidlist,
		outfn.date=outfn.date)
    if (!overwrite)
      outpltcntfn <- FIESTA::fileexistsnm(outfolder, outpltcntfn, "csv")
    path.outpltcntfn <- paste0(outfolder, "/", outpltcntfn, ".csv")
    
    ## Shapefile data
#    if (issp) {    
#      outshpbase <- paste0("/", outfn.pre, "shp_", invtype, "_", 
#		paste(stabbrlst, collapse="")
#      outshpbase <- DBgetfn("shp", invtype, outfn.pre, stabbrlst, evalid=evalidlist)
#      outshpdatbase <- DBgetfn("shpdat", invtype, outfn.pre, stabbrlst, evalid=evalidlist)
#    }

    ## Understory vegetation data
    if (isveg) { 
      outvspsppfn <- DBgetfn("vspspp", invtype, outfn.pre, stabbrlst, 
		evalid=evalidlist, outfn.date=outfn.date)
      if (!overwrite)
        outvspsppfn <- FIESTA::fileexistsnm(outfolder, outvspsppfn, "csv")
      path.outvspsppfn <- paste0(outfolder, "/", outvspsppfn, ".csv")

      outvspstrfn <- DBgetfn("vspstr", invtype, outfn.pre, stabbrlst, 
		evalid=evalidlist, outfn.date=outfn.date)
      if (!overwrite)
        outvspstrfn <- FIESTA::fileexistsnm(outfolder, outvspstrfn, "csv")
      path.outvspstrfn <- paste0(outfolder, "/", outvspstrfn, ".csv")
    }

    ## DWM data
    if (isdwm) { 
      outdwmfn <- DBgetfn("dwm", invtype, outfn.pre, stabbrlst, 
		evalid=evalidlist, outfn.date=outfn.date)
      if (!overwrite)
        outcwdfn <- FIESTA::fileexistsnm(outfolder, outdwmfn, "csv")
      path.outdwmfn <- paste0(outfolder, "/", outdwmfn, ".csv")
    }
  }

##################################################################################
##################################################################################
##################################################################################
  nbrcnds <- {}
  stcds <- {}
  stabbrfn <- ""
  pltcnt <- {}
  
  ACTUALcond <- {}
  ACTUALplot <- {}
  plt <- {}
  cond <- {}
  pltx <- {}
  condx <- {}
  pltcond <- {}
  tree <- {}
  seed <- {}
  if(isveg){ vspspp <- vspstr <- {} }
  if(isdwm){ dwm <- {} }  
  stateFilters <- {}
  filtervarlst <- as.vector(do.call(c, filtervarlst))

  for(shpcoord in spcoords)
    assign(paste("shpdat", shpcoord, sep="_"), {})

  if (iseval) {
    vars <- paste0(vars, ",EVALID")
    plotvars <- c(plotvars, "EVALID")
  } 

  ## Get CSV files
  if (datsource == "CSV") {

    ## PLOT table  (ZIP FILE) 
    PLOT <- FIESTA::DBgetCSV("PLOT", stabbrlst, ZIP=TRUE, returnDT=TRUE)

    ## COND table (ZIP FILE) 
    COND <- FIESTA::DBgetCSV("COND", stabbrlst, ZIP=TRUE, returnDT=TRUE)

    if (iseval) 
      ## POP_PLOT_STRATUM_ASSGN table (ZIP FILE) - 
      ## To get estimation unit & stratum assignment for each plot. 
      POP_PLOT_STRATUM_ASSGN <- FIESTA::DBgetCSV("POP_PLOT_STRATUM_ASSGN", stabbrlst, 
		ZIP=TRUE, returnDT=TRUE)    

    if (istree)
      ## TREE table (ZIP FILE) 
      TREE <- FIESTA::DBgetCSV("TREE", stabbrlst, ZIP=TRUE, returnDT=TRUE)

    if (isseed)
      ## TREE table (ZIP FILE) 
      SEEDLING <- FIESTA::DBgetCSV("SEEDLING", stabbrlst, ZIP=TRUE, returnDT=TRUE)

    if (isveg) {
      P2VEG_SUBPLOT_SPP <- 
		FIESTA::DBgetCSV("P2VEG_SUBPLOT_SPP", stabbrlst, ZIP=TRUE, returnDT=TRUE)

      P2VEG_SUBP_STRUCTURE <- 
		FIESTA::DBgetCSV("P2VEG_SUBP_STRUCTURE", stabbrlst, ZIP=TRUE, returnDT=TRUE)
    }
  }

  for (i in 1:length(states)) {
    evalid <- NULL
    state <- states[i]
    stcd <- FIESTA::pcheck.states(state, "VALUE")
    stabbr <- FIESTA::pcheck.states(state, "ABBR")

    if (length(invyrs) > 1){
      invyr <- invyrs[[state]]
    } else {
      invyr <- invyrs[[1]]
    }
    stFilter <- paste0("p.STATECD IN(", stcd, ")") 
    stinvyrFilter <- paste0(stFilter, " and p.INVYR IN(", FIESTA::addcommas(invyr), ")")

    ## If FIA evaluation, get all plot from all evaluations.
    if (iseval) {
      evalid <- evalidlist[[state]]
      evalFilter <- paste0("ppsa.EVALID IN(", paste(evalid, collapse=","), ")")
    } else {
      invyrFilter <- paste0("p.INVYR IN(", FIESTA::addcommas(invyr), ")")
      evalFilter <- stinvyrFilter
      if (!subcycle99)
        evalFilter <- paste(evalFilter, "and p.SUBCYCLE <> 99")
      if (intensity1)
        evalFilter <- paste(evalFilter, "and p.INTENSITY = '1'")
    } 
    evalFilter.min <- ifelse(iseval, paste("ppsa.EVALID =", min(evalid)), evalFilter)
    if (isdwm) {
      if (iseval) {
        evalid.dwm <- evalid[endsWith(as.character(evalid), "07")]
        if (length(evalid.dwm) == 0) stop("must include evaluation ending in 07")
      } 
      evalFilter.dwm <- ifelse(iseval, paste("EVALID =", evalid.dwm), evalFilter)
    }         

    ####################################################################################
    #############################  ADDS FILTER (OPTIONAL)  #############################
    ####################################################################################

    ## GETS stateFilter 
    isnbrcnd <- FALSE
    if (stateFilter) {
      stateFilters <- ""
      addfilter <- "YES"
      filtervars <- {}
      filterlstst <- filtervarlst[!filtervarlst == "INVYR"]
      #nbrcndlst <- c("NBRCND", "NBRCNDSAMP", "NBRCNDFOR", "NBRCNDFTYP")
      if (actual) filterlstst <- c(filterlstst, filterACTUALlst)
      #if (!is.null(cvars)) filterlstst <- c(filterlstst, nbrcndlst)
      filterlst <- filterlstst

      while (addfilter == "YES") {
        filtervar <- select.list(c("NONE", sort(filterlst)), 
		title=paste("Filter variable -", stabbr), multiple=FALSE)
        if (filtervar == "") stop("")
        if (filtervar == "NONE") {
          break
        } else if (filtervar %in% filterpvarlst) {
          filterALIAS <- "p"
        } else if (filtervar %in% filtercvarlst) {
          filterALIAS <- "c"
        }
        filterfromqry <- fromqry
            
        if (!isnbrcnd) {
          filtervars <- c(filtervars, filtervar)
          filterlst <- filterlst[filterlst != filtervar]
          filtervarx <- paste0(filterALIAS, ".", filtervar)

          filterdbqry <- paste0("select ", filtervarx, " from ", filterfromqry, 
			" where ", evalFilter)

          if (datsource == "CSV") {
            filterdb <- unique(na.omit(sqldf::sqldf(filterdbqry)))
          } else {
            tryCatch( filterdb <- unique(na.omit(DBqryORACLE(filterdbqry, dbconn, dbconnopen=TRUE))), 
			error=function(e) stop("filter query is invalid"))
          }
          filterdb <- filterdb[!is.na(filterdb),]
          filterdb <- filterdb[order(filterdb)]
        }

        if (filtervar %in% c("ELEV", "CRCOVPCT_RMRS", "CRCOVPCT_LIVEMISS_RMRS", 
		"CRCOVPCT_LIVE_RMRS", "LIVE_CANOPY_CVR_PCT", "LIVE_MISSING_CANOPY_CVR_PCT")) {
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
        #} else if (isnbrcnd) {
        #  nbrcndlist <- unique(nbrcnd[nbrcnd[,filtervar] > 0,filtervar]) 
        #  nbrcndval <- select.list(as.character(nbrcndlist), 
		#	title="Select filter code(s)", multiple=TRUE)
        #  if (length(nbrcndval) == 0) stop("")
        #  nfiltervar <- filtervar  
        } else if (actual && filtervar %in% filterACTUALlst) {
          stop("not done yet")
          filterqry <- paste0(filtervarx, " in(", incd, ")")   
        } else {      
          filtercd <- select.list(as.character(filterdb), 
			title="Select filter code(s)", multiple=TRUE)
          if (length(filtercd) == 0) {
            stop("")
          } else {
            #filtercd <- sapply(filtercd, function(x) paste0("'", x, "'"))
            filtercd <- FIESTA::addcommas(filtercd)
            incd <- filtercd[1]
            if (length(filtercd)>1)
              for (j in 2:length(filtercd)) 
                incd <- paste(incd, filtercd[j], sep=",")
            stateFilters <- paste0(stateFilters, " and ", filtervarx, " in(", incd, ")")
          }
        }

        addfilter <- select.list(c("NO", "YES"), 
			title=paste("Another filter? -", stabbr), multiple=FALSE)
        if (addfilter == "") stop("")
      }
      if (i == 1 && length(states) > 1) {
        resp <- select.list(c("YES", "NO"), title="Same for all states", multiple=FALSE)
        if (resp == "YES") stateFilter <- FALSE
      }
    } else {
      if (is.null(stateFilters)) stateFilters <- ""
    }

    ## SET QUERY FILTER
    xfilter <- paste0(evalFilter, stateFilters)
    xfilter.min <- paste0(evalFilter.min, stateFilters)
    

    #####################################################################################
    ###################################    RUN QUERIES   ################################
    #####################################################################################
    #mem.size = sum(.ls.objects()$Size)

    #### GENERATE MAIN QUERIES
    #####################################################################################

    if (!is.null(vars)) {
 
      pltcondqry <- paste("select", vars, "from", fromqry, "where", evalFilter)
      if (datsource == "CSV") {
        pltcondx <- sqldf::sqldf(pltcondqry, stringsAsFactors=FALSE)
      } else {
        stat <- paste("## STATUS: GETTING PLOT/COND DATA (", stabbr, ") ...")
        cat("\n", stat, "\n")
        tryCatch( pltcondx <- DBqryORACLE(pltcondqry, dbconn, dbconnopen=TRUE), 
			error=function(e) stop("pltcond query is invalid"))
      }

      ## Write query to outfolder
      if (saveqry) {
        pltcondqryfnbase <- DBgetfn("pltcond", invtype, outfn.pre, stabbr, 
		evalid=evalid, qry=TRUE, outfn.date=outfn.date)
        pltcondqryfn <- FIESTA::fileexistsnm(outfolder, pltcondqryfnbase, "txt")
        outfile <- file(paste0(outfolder, "/", pltcondqryfn, ".txt"), "w")
          cat(  paste0(pltcondqry, xfilter), "\n", file=outfile)
        close(outfile)
      }

      ## Filter pltcond with allFilter      
      ###########################################
      if (!is.null(pltcondx) && nrow(pltcondx) > 0) {
        pltcondx <- FIESTA::datFilter(x=pltcondx, xfilter=allFilter)$xf
      } else {
        message("no plots in database for ", state)
      }
 
      if (!is.null(pltcondx) && nrow(pltcondx) > 0) {
        ## Tag ACI plots
        ###########################################
        if (ACI) {
          pltcondx[, c("ACI", "ACI_NFS") := 0,]

          pltcondx[NF_SAMPLING_STATUS_CD == 1 &
			!is.na(NF_COND_STATUS_CD) & NF_COND_STATUS_CD == 2,
			ACI_NFS:= 1]
          pltcondx[NF_SAMPLING_STATUS_CD == 1 &
			!is.na(NF_COND_STATUS_CD) & NF_COND_STATUS_CD == 2 &
			OWNCD == 11, ACI := 1]
        }

        ## Separate pltcondx into 2 tables (pltx, condx)
        ###########################################
        if (!is.null(plotvars)) {
          pltx <- unique(pltcondx[,plotvars, with=FALSE])
          pltx[, CN := as.character(CN)]
          setkey(pltx, CN)
          if ("PREV_PLTCN" %in% names(pltx))
            pltx[, PREV_PLTCN := as.character(PREV_PLTCN)]             
        }
        if (!is.null(condvars) && "CONDID" %in% names(pltcondx)) {
          condx <- unique(pltcondx[,condvars, with=FALSE])
          condx[, PLT_CN := as.character(PLT_CN)]        
          setkey(condx, PLT_CN, CONDID)
        } 
 

        ###  GET ADDITIONAL PLOT INFO  
        ######################################################################

        ## Change names of LON and LAT to LON_PUBLIC and LAT_PUBLIC
        if ("LON" %in% names(pltx))
          setnames(pltx, "LON", "LON_PUBLIC")
        if ("LAT" %in% names(pltx))
          setnames(pltx, "LAT", "LAT_PUBLIC")
        if ("ELEV" %in% names(pltx))
          setnames(pltx, "ELEV", "ELEV_PUBLIC")


        ## Create plot-level, number of condtion variables
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
          nbrcndvars <- c("NBRCND", "NBRCNDSAMP", "NBRCNDFOR", "NBRCNDFTYP")
          nbrcnd <- nbrcndsamp[nbrcnd]
          nbrcnd <- nbrcndfor[nbrcnd]
          nbrcnd <- nbrcndftyp[nbrcnd]
          nbrcnd[is.na(nbrcnd)] <- 0

          rm(nbrcndsamp)
          rm(nbrcndfor)
          rm(nbrcndftyp)

          ## Merge to plt table
          pltx <- merge(pltx, nbrcnd, by.x="CN", by.y="PLT_CN", all.x=TRUE)
          #if (isnbrcnd) pltx <- pltx[pltx[,nfiltervar] %in% nbrcndval,]
        }

        ### CHECK FOR OWL DATA
        ######################################################################
        if (regionVars) {
          ## CCRMRSPLT
          ## A plot level canopy cover variable based on CRCOVPCT_RMRS
          if (all(c("CRCOVPCT_RMRS", "CONDPROP_UNADJ") %in% names(condx))) {
            ccRMRSplt <- condx[, list(round(sum(CRCOVPCT_RMRS * CONDPROP_UNADJ, 
			na.rm=TRUE), 2)), by="PLT_CN"]
            setnames(ccRMRSplt, c("PLT_CN", "CCRMRSPLT"))
            pltx <- merge(pltx, ccRMRSplt, by.x="CN", by.y="PLT_CN", all.x=TRUE)
          }
          ## CCPLT
          ## A plot level canopy cover variable based on CRCOV
          if (all(c("CRCOV", "CONDPROP_UNADJ") %in% names(condx))) {
            ccplt <- condx[, list(round(sum(CRCOVPCT_RMRS * CONDPROP_UNADJ, 
			na.rm=TRUE), 2)), by="PLT_CN"]
            setnames(ccplt, c("PLT_CN", "CCPLT"))
            pltx <- merge(pltx, ccplt, by.x="CN", by.y="PLT_CN", all.x=TRUE)
          }
        }  

        ## Additional condition variables
        ######################################################################
        ref_fortypgrp <- FIESTA::ref_codes[FIESTA::ref_codes$VARIABLE == "FORTYPCD",]
        ## FORTYPGRP
        ## A condition level variable grouping FORTYPCD
        cndnames <- names(condx)
        if ("FORTYPCD" %in% names(condx)) {
          condx <- merge(condx, ref_fortypgrp[,c("VALUE", "GROUPCD")],
        		by.x="FORTYPCD", by.y="VALUE", all.x=TRUE)
          setnames(condx, "GROUPCD", "FORTYPGRPCD")
          setcolorder(condx, c(cndnames, "FORTYPGRPCD"))
        }
        ## FLDTYPGRP
        ## A condition level variable grouping FLDTYPGRP
        if ("FLDTYPCD" %in% names(condx)) {
          condx <- merge(condx, ref_fortypgrp[,c("VALUE", "GROUPCD")], 
               by.x="FLDTYPCD", by.y="VALUE", all.x=TRUE)
          setnames(condx, "GROUPCD", "FLDTYPGRPCD")
          setcolorder(condx, c(cndnames, "FLDTYPGRPCD"))
        }
        setkey(condx, PLT_CN, CONDID)

        ## TIMBERCD
        #if (all(c("SITECLCD", "RESERVCD") %in% names(condx))) {
        #  condx[COND_STATUS_CD == 1, TIMBERCD := 2]
        #  condx[SITECLCD %in% 1:6 & RESERVCD == 0, TIMBERCD := 1]
        #}
        if ("SITECLCD" %in% names(condx)) {
          condx[COND_STATUS_CD == 1, TIMBERCD := 2]
          condx[SITECLCD %in% 1:6, TIMBERCD := 1]
        }

        ## Additional plot variables
        ######################################################################

        ## CCLIVEPLT:
        ## A plot level canopy cover variable based on LIVE_CANOPY_CVR_PCT
        if (all(c("LIVE_CANOPY_CVR_PCT", "CONDPROP_UNADJ") %in% names(condx))) {
          ccliveplt <- condx[, 
			round(sum(LIVE_CANOPY_CVR_PCT * CONDPROP_UNADJ, na.rm=TRUE),2), 
			by=PLT_CN]
          setnames(ccliveplt, c("PLT_CN", "CCLIVEPLT"))
          pltx <- merge(pltx, ccliveplt, by.x="CN", by.y="PLT_CN", all.x=TRUE)
        }

  #      Had to remove because someone decided to get rid of subpanel from database.
  #      ## P2PANELSUB
  #      if(all(c("P2PANEL", "SUBPANEL") %in% names(pltx)))
  #        pltx$P2PANELSUB <- paste(pltx$P2PANEL, pltx$SUBPANEL, sep="_")


        ## FORNONSAMP: 
        ## Plot-level variable based on PLOT_STATUS_CD and PLOT_NONSAMPLE_REASN_CD
        if ("PLOT_NONSAMPLE_REASN_CD" %in% names(pltx)) {
          pltx$FORNONSAMP <- as.character(pltx$PLOT_STATUS_CD)
          pltx[!is.na(PLOT_NONSAMPLE_REASN_CD) & PLOT_NONSAMPLE_REASN_CD == 2,
             "FORNONSAMP"] <- "Nonsampled-Denied access"
          pltx[!is.na(PLOT_NONSAMPLE_REASN_CD) & PLOT_NONSAMPLE_REASN_CD == 3,
             "FORNONSAMP"] <- "Nonsampled-Hazardous"
          pltx[!is.na(PLOT_NONSAMPLE_REASN_CD) & PLOT_NONSAMPLE_REASN_CD %in% c(5,6),
             "FORNONSAMP"] <- "Nonsampled-Lost data"
          pltx[!is.na(PLOT_NONSAMPLE_REASN_CD) & PLOT_NONSAMPLE_REASN_CD == 7,
             "FORNONSAMP"] <- "Nonsampled-Wrong location"
          pltx[!is.na(PLOT_NONSAMPLE_REASN_CD) & PLOT_NONSAMPLE_REASN_CD == 8,
             "FORNONSAMP"] <- "Nonsampled-Skipped visit"
          pltx[!is.na(PLOT_NONSAMPLE_REASN_CD) & PLOT_NONSAMPLE_REASN_CD == 9,
             "FORNONSAMP"] <- "Nonsampled-Dropped plot"
          pltx[!is.na(PLOT_NONSAMPLE_REASN_CD) & PLOT_NONSAMPLE_REASN_CD %in% c(10,11),
             "FORNONSAMP"] <- "Nonsampled-Other"
          pltx[PLOT_STATUS_CD == "1", "FORNONSAMP"] <- "Sampled-Forest"
          pltx[PLOT_STATUS_CD == "2", "FORNONSAMP"] <- "Sampled-Nonforest"
        }

        ## Generate ZSTCOPLOT, with STATECD, UNITCD, COUNTYCD, PLOT to define
        ## the unique location of a plot (without regards to remeasurement)
        #pltx$ZSTUNITCOPLOT <- paste0("Z", formatC(pltx$STATECD, width=2, flag=0), 
        #  formatC(pltx$UNITCD, width=2, flag=0),
        #  formatC(pltx$COUNTYCD, width=3, flag=0),
        #  formatC(pltx$PLOT, width=5, flag=0))

        ## Generate ZSTCOPLOT, with STATECD, COUNTYCD, PLOT to define
        pltx$ZSTCOPLOT <- paste0("Z", formatC(pltx$STATECD, width=2, digits=2, flag=0), 
          formatC(pltx$COUNTYCD, width=3, digits=3, flag=0),
          formatC(pltx$PLOT, width=5, digits=5, flag=0)) 


        ######################################################################
        ###  GET PLOT AND CONDITION COUNTS  
        ######################################################################
        plotcnt.vars <- c("CN", "INVYR", "FORNONSAMP")
        pltcnt <- rbind(pltcnt, 
		FIESTA::datPlotcnt(plt=unique(pltx[,plotcnt.vars, with=FALSE]), savedata=FALSE))


        if (actual) {
          stat <- paste("## STATUS: GETTING ACTUAL DATA (", stabbr, ") ...")
          cat("\n", stat, "\n")
          ACTUALqry <- paste0("select distinct ", ACTUALvars, " from ", ACTUALfromqry,  
			" where ", xfilter.min)
          tryCatch( ACTUALcondx <- DBqryORACLE(ACTUALqry, dbconn, dbconnopen=TRUE), 
			error=function(e) stop("actual query is invalid"))
 
          ## Write query to outfolder
          if (saveqry) {
            actualqryfnbase <- DBgetfn("actual", invtype, outfn.pre, stabbr, 
			evalid=evalid, qry=TRUE, outfn.date=outfn.date)
            actualqryfn <- FIESTA::fileexistsnm(outfolder, actualqryfnbase, "txt")
            outfile <- file(paste0(outfolder, "/", actualqryfn, ".txt"), "w")
                cat(  paste0(ACTUALqry, xfilter), "\n", file=outfile)
            close(outfile)
          }

          ## PLT_CN to a character field
          #################################################################
 
          ## CHECK FOR OVERALL FILTERS
          ACTUALcondx <- ACTUALcondx[ACTUALcondx$PLT_CN %in% unique(pltx[["CN"]]),]

          if ("CONDID" %in% names(ACTUALcondx))
            ACTUALcondx$CONDID <- as.integer(ACTUALcondx$CONDID)
          if ("OWNCD" %in% names(ACTUALcondx))
            names(ACTUALcondx)[names(ACTUALcondx)== "OWNCD"] <- "ACTUAL_OWNCD"
          names(ACTUALcondx)[names(ACTUALcondx)== "LON"] <- "LON_PUBLIC"
          names(ACTUALcondx)[names(ACTUALcondx)== "LAT"] <- "LAT_PUBLIC"

          if ("PLOT" %in% names(ACTUALcondx))
            names(ACTUALcondx)[names(ACTUALcondx)== "PLOT"] <- "PLOT_PERIODIC"
          names(ACTUALcondx)[names(ACTUALcondx)== "ACTUAL_LON"] <- "LON_ACTUAL"
          names(ACTUALcondx)[names(ACTUALcondx)== "ACTUAL_LAT"] <- "LAT_ACTUAL"

          ## Define coordinate variables
          #coordvars <- c("LON_ACTUAL", "LAT_ACTUAL", "LON_PUBLIC", "LAT_PUBLIC", 
		#	"LON_DIGITIZED", "LAT_DIGITIZED")
          coordvars <- c("LON_ACTUAL", "LAT_ACTUAL", "LON_PUBLIC", "LAT_PUBLIC")
          othercvars <- names(ACTUALcondx)[which(!names(ACTUALcondx) %in% coordvars)]

          ## Plot-level table (actualp)
          ACTUALplotx <- unique(ACTUALcondx[,c("PLT_CN", "PLOT_PERIODIC", coordvars)]) 

          ## Cond-level table (actualc)
          ACTUALcondx <- ACTUALcondx[,othercvars]
               
          if (savedata) {
            if (!datReturn) {
              ## Condition-level
              actualcfnbase <- DBgetfn("actualc", invtype, outfn.pre, stabbr, 
			evalid=evalid, outfn.date=outfn.date)
              actualcfn <- FIESTA::fileexistsnm(outfolder, actualcfnbase, "csv")
              path.outactualcfn <- paste0(outfolder, "/", actualcfn, ".csv")
              write.csv(ACTUALcondx, file=path.outactualcfn, row.names=FALSE)

              ## Plot-level
              actualpfnbase <- DBgetfn("actualp", invtype, outfn.pre, stabbr, 
			evalid=evalid, outfn.date=outfn.date)
              actualpfn <- FIESTA::fileexistsnm(outfolder, actualpfnbase, "csv")
              path.outactualpfn <- paste0(outfolder, "/", actualpfn, ".csv")
              write.csv(ACTUALplotx, file=path.outactualpfn, row.names=FALSE)

              #ACTUALcondx <- NULL
            } else {
              if (i == 1) {
                write.table(ACTUALcondx, file=path.outactualcfn, sep=",", 
				row.names=FALSE)
                write.table(ACTUALplotx, file=path.outactualpfn, sep=",", 
				row.names=FALSE)

              } else {
                fwrite(ACTUALcondx, file=path.outactualcfn, sep=",", 
				row.names=FALSE, col.names=FALSE, append=TRUE)

                write.table(ACTUALplotx, file=path.outactualpfn, sep=",", 
				row.names=FALSE, col.names=FALSE, append=TRUE)
              }
            }
          }
          ## Append data
          if (datReturn) {
            ACTUALcond <- rbind(ACTUALcond, ACTUALcondx)
            ACTUALplot <- rbind(ACTUALplot, ACTUALplotx)
          } else {
            ACTUALplot <- NULL
          }
        } else { 
          ACTUALplotx <- NULL
          ACTUALcondx <- NULL
        }

        if (issp) {

          stat <- paste("## STATUS: GENERATING SHAPEFILE DATA (", stabbr, ") ...")
          cat("\n", stat, "\n")

          for (coords in spcoords) { 
            xycoords <- getcoords(coords)

            if (coords == "PUBLIC") {
              ACTUALplotxx <- NULL
              actualxx <- FALSE
            } else {
              ACTUALplotxx <- ACTUALplotx
              actualxx <- TRUE
            }
            shpplt <- unique(pltx[, names(pltx)[names(pltx) != "EVALID"], with=FALSE])
            shpdatx <- getShapedat(plt=setDF(shpplt), 
			cond=setDF(condx), addcond=spcond, condid1=spcondid1, actual=actualxx, 
			ACTUALplot=ACTUALplotxx, spcoords=coords, xycoords=xycoords, 
			ACI=ACI, fromqry=fromqry, datsource=datsource, dbconn=dbconn, 
			xfilter=xfilter)
            setDT(pltx)
            setDT(condx)
            setkey(pltx, CN)
            setkey(condx, PLT_CN, CONDID)

            ## CHANGE CN TO PLT_CN
            if (!"PLT_CN" %in% names(shpdatx)) { 
              if ("CN" %in% names(shpdatx)) {
                names(shpdatx)[names(shpdatx)== "CN"] <- "PLT_CN"
              } else {
                message("PLT_CN and CN does not exist in dataset")
              }
            } else {
              if("CN" %in% names(shpdatx)) shpdatx$CN <- NULL
            }

            makeshp <- FALSE
            if (savedata) {

              if (!datReturn) {
                shpfn <- DBgetfn("shp", invtype, outfn.pre, stabbr, evalid=evalid, 
				othertxt=coords, addslash=FALSE)
                shp <- FIESTA::spMakeSpatialPoints(xyplt=shpdatx, x=xycoords[1], y=xycoords[2], 
                    	uniqueid="PLT_CN", prj4str="+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs",  
                    	exportshp=TRUE, outfolder=outfolder, outshpnm=shpfn, outfn.date=outfn.date,
				overwrite=overwrite)

                shpdatfn <- DBgetfn("shp", invtype, outfn.pre, stabbr, evalid=evalid, 
				othertxt=coords, outfn.date=outfn.date, addslash=FALSE)
                if (!overwrite)
                  shpdatfn <- FIESTA::fileexistsnm(outfolder, shpdatfn, "csv")
                path.shpdatfn <- paste0(outfolder, "/", shpdatfn, ".csv")

                write.csv(shp@data, file=path.shpdatfn, row.names=FALSE)
                shpdatx <- NULL
              } else {
                makeshp <- TRUE
              }
              
            } else {
              makeshp <- TRUE
            }
            if (datReturn) {
                assign(paste("shpdat", coords, sep="_"), 
				rbind(get(paste("shpdat", coords, sep="_")), shpdatx))
            } else {
                assign(paste("shpdat", coords, sep="_"), NULL)
            }
            rm(shpdatx)
            gc()
          }
        }
      } 
    }

    ##############################################################
    ## Tree data
    ##############################################################
    if (istree && !is.null(pltx)) {
      if (is.null(tvars) & is.null(tsumvars)) {
        treex <- NULL
        istree <- FALSE
      } else {
        if (is.null(tsumvars)) {
          ttvars <- tvars
        } else if(is.null(tvars)) {
          ttvars <- tsumvars
        } else {  
          ttvars <- paste(tvars, tsumvars, sep=",")
        }

        sppvars <- NULL
        if (biojenk) sppvars <- c("JENKINS_TOTAL_B1", "JENKINS_TOTAL_B2")
        if (greenwt) sppvars <- c(sppvars, "DRYWT_TO_GREENWT_CONVERSION")
        
        treeqry <- paste("select", ttvars, "from", tfromqry, "where", evalFilter.min)
        if (datsource == "CSV") {
          treex <- sqldf::sqldf(treeqry, stringsAsFactors=FALSE)
          if (!is.null(sppvars)) {
            sppsql <- paste("select SPCD,", paste(sppvars, collapse=","), "from REF_SPECIES")
            ref_spp <- FIESTA::DBqryCSV(sppsql, sqltables="REF_SPECIES")
          }
        } else {
          cat("\n", "## STATUS: GETTING TREE DATA (", stabbr, ") ...", "\n")
          tryCatch( treex <- DBqryORACLE(treeqry, dbconn, dbconnopen=TRUE), 
			error=function(e) stop("tree query is invalid"))
          if (!is.null(sppvars)) {
            sppsql <- paste("select SPCD,", paste(sppvars, collapse=","), "from FS_FIADB.REF_SPECIES")
            ref_spp <- DBqryORACLE(sppsql, dbconn, dbconnopen=TRUE)
          }
        }

        if (nrow(treex) != 0) {
          treex <- setDT(treex)
          treex[, PLT_CN := as.character(PLT_CN)]
          setkey(treex, PLT_CN, CONDID)

          ## Subset overall filters from pltx
          treex <- treex[treex$PLT_CN %in% unique(pltx$CN),]

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

          ## Change NA values to 0 values
          if (any(names(treex) %in% treenavars)) 
            treex <- DT_NAto0(treex, treenavars)
    
          if (istree) {
            if (defaultVars) {
              ## Check for numeric values
              treex <- FIESTA::check.numeric(treex, c("DIA", "DRYBIO_BOLE", "DRYBIO_STUMP",
				"DRYBIO_STUMP", "DRYBIO_SAPLING", "DRYBIO_WDLD_SPP", "BHAGE"))
 
              ## Create new tree variables - basal area, aboveground biomass, tree age
              treex[, BA := DIA * DIA * 0.005454]
              #treex[, DRYBIO_AG := DRYBIO_BOLE + DRYBIO_STUMP + DRYBIO_TOP + 
		   #DRYBIO_SAPLING + DRYBIO_WDLD_SPP]
              #treex[, TREEAGE := BHAGE][TOTAGE > 0, TREEAGE := TOTAGE]
              tsumvarlsts2 <- c(tsumvars, "BA")

              if (!is.null(sppvars)) {
                treenames <- names(treex)
                treex <- merge(treex, ref_spp, by="SPCD")

                if (biojenk) {
                  treex[, BIOJENK_kg := exp(JENKINS_TOTAL_B1 + JENKINS_TOTAL_B2 * log(DIA * 2.54))]
                  treex[, BIOJENK_lb := BIOJENK_kg * 2.2046]		## Converts back to tons
                  treex[, JENKINS_TOTAL_B1 := NULL][, JENKINS_TOTAL_B2 := NULL]
                  sppvarsnew <- c("BIOJENK_kg", "BIOJENK_lb")
                }
                if (greenwt) {
                  sppvarsnew <- "DRYWT_TO_GREENWT_CONVERSION"
                }
                setcolorder(treex, c(treenames, sppvarsnew)) 
              } 
            }          

            if (savedata) {
              if (!datReturn) {
                treecfnbase <- DBgetfn("tree", invtype, outfn.pre, stabbr, 
				evalid=evalid, outfn.date=outfn.date)
                treecfn <- FIESTA::fileexistsnm(outfolder, treecfnbase, "csv")
                path.outtreecfn <- paste0(outfolder, "/", treecfn, ".csv")
                write.csv(treex, file=path.outtreecfn, row.names=FALSE)
                treex <- NULL
              } else {
                if (i == 1) {
                  write.table(treex, file=path.outtreecfn, sep=",", row.names=FALSE)
                }else {
                  write.table(treex, file=path.outtreecfn, sep=",", row.names=FALSE, 
				col.names=FALSE, append=TRUE)
                }
              }                           
            }
            ## Append data
            if (treeReturn) {
              tree <- rbind(tree, treex)
            } else {
              tree <- NULL
            }
          } else {
            treex <- NULL
          }
        } else {
          treex <- NULL
        }
        rm(treex)
        rm(ttvars)
        gc()
      }
    }

    ##############################################################
    ## Seedling data
    ##############################################################
    if (isseed && !is.null(pltx)) {
      ## GENERATE AND RUN QUERY AND WRITE TO OUTFOLDER

      if (is.null(svars)) {
        seedx <- NULL
        isseed <- NULL
      } else {
        seedqry <- paste("select", svars, "from", sfromqry, "where", evalFilter.min)
        if (datsource == "CSV") {
          seedx <- sqldf::sqldf(seedqry, stringsAsFactors=FALSE)
        } else {
          cat("\n", "## STATUS: GETTING SEEDLING DATA (", stabbr, ") ...", "\n")
          tryCatch( seedx <- DBqryORACLE(seedqry, dbconn, dbconnopen=TRUE), 
			error=function(e) stop("seedling query is invalid"))
        }

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
          if (any(names(seedx) %in% seednavars)) 
            seedx <- FIESTA::DT_NAto0(seedx, seednavars)
     
          if (defaultVars) {
            if ("TREECOUNT_CALC" %in% names(seedx)) {
              ## Create variable, SEEDCNT6, where a value of 6 means 6 or more seeds (per SUBP) 
              seedx[, SEEDCNT6 := TREECOUNT_CALC][TREECOUNT_CALC >= 6, SEEDCNT6 := 6]

              ## Create variable, SEEDSUBP6, indicating a species has 6 or more seedlings on a SUBP
              seedx[, SEEDSUBP6 := 0][TREECOUNT_CALC >= 6, SEEDSUBP6 := 1]
            }
          }

          if (savedata) {
            if (!datReturn) {
              seedfnbase <- DBgetfn("seed", invtype, outfn.pre, stabbr, 
				evalid=evalid, outfn.date=outfn.date)
              seedfn <- FIESTA::fileexistsnm(outfolder, seedfnbase, "csv")
              path.outseedfn <- paste0(outfolder, "/", seedfn, ".csv")
              write.csv(seedx, file=path.outseedfn, row.names=FALSE)
              seedx <- NULL
            } else {
              if (i == 1) {
                write.table(seedx, file=path.outseedfn, sep=",", row.names=FALSE)
              } else {
                write.table(seedx, file=path.outseedfn, sep=",", row.names=FALSE, 
				col.names=FALSE, append=TRUE)
              }
            }
          }              
        } else {
          seedx <- NULL
        }
      }

      ## Append data
      seed <- rbind(seed, seedx)
      rm(seedx)
      gc()
    }

    ##############################################################
    ## Understory vegetation data
    ##############################################################
    if (isveg && !is.null(pltx)) {
 
      ## Get data for P2VEG_SUBPLOT_SPP
      vspsppqry <- paste("select", vspsppvars, "from", vfromqry, "where", evalFilter.min)
      if (datsource == "CSV") {
        vspsppx <- sqldf::sqldf(vspsppqry, stringsAsFactors=FALSE)
      } else {
        cat("\n",

        "## STATUS: GETTING UNDERSTORY VEGETATION DATA (", stabbr, ") ...", "\n")
        tryCatch( vspsppx <- DBqryORACLE(vspsppqry, dbconn, dbconnopen=TRUE), 
			error=function(e) stop("veg_spp query is invalid"))
      }

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

      vspstrqry <- paste("select", vspstrvars, "from", vstrfromqry, "where", evalFilter)
      ## Get data for P2VEG_SUBP_STRUCTURE
      if (datsource == "CSV") {
        vspstrx <- sqldf::sqldf(vspstrqry, stringsAsFactors=FALSE)
      } else {
        tryCatch( vspstrx <- DBqryORACLE(vspstrqry, dbconn, dbconnopen=TRUE), 
			error=function(e) stop("veg_structure query is invalid"))
      }

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

      if (savedata) {

        if (!datReturn) {
          vspsppfnbase <- DBgetfn("vspspp", invtype, outfn.pre, stabbr, 
			evalid=evalid, outfn.date=outfn.date)
          vspsppfn <- FIESTA::fileexistsnm(outfolder, vspsppfnbase, "csv")
          path.outvspsppfn <- paste0(outfolder, "/", vspsppfn, ".csv")
          write.csv(vspsppx, file=path.outvspsppfn, row.names=FALSE)
          vspsppx <- NULL

          vspstrfnbase <- DBgetfn("vspstr", invtype, outfn.pre, stabbr, 
			evalid=evalid, outfn.date=outfn.date)
          vspstrfn <- fileexistsnm(outfolder, vspstrfnbase, "csv")
          path.outvspstrfn <- paste0(outfolder, "/", vspstrfn, ".csv")
          write.csv(vspstrx, file=path.outvspstrfn, row.names=FALSE)
          vspstrx <- NULL
        } else {
          if (i == 1) {
            write.table(vspsppx, path.outvspsppfn, sep=",", row.names=FALSE)
            write.table(vspstrx, path.outvspstrfn, sep=",", row.names=FALSE)
          } else {
            write.table(vspsppx, file=path.outvspsppfn, sep=",", row.names=FALSE,
			col.names=FALSE, append=TRUE)
            write.table(vspstrx, file=path.outvspstrfn, sep=",", row.names=FALSE,
			col.names=FALSE, append=TRUE)
          }
        }
      }

      if (nbrstates <= 4) {
        vspspp <- rbind(vspspp, vspsppx)
        vspstr <- rbind(vspstr, vspstrx)
      } else {
        vspspp <- NULL
        vspstr <- NULL

        if (savedata) {
          warning("veg data too big.. saving to file")
        } else {
          stop("veg data too big.. must set savedata=TRUE")
        }            
      }
      rm(vspsppx)
      rm(vspstrx)
    }

    ## DOWN WOODY DATA
    if (isdwm && !is.null(pltx)) {
      cat("\n",
      "## STATUS: GETTING DOWN WOODY DATA (", stabbr, ") ...", "\n")
    
      if (is.null(dvars)) {
        dwmx <- NULL
        isdwm <- NULL
      } else {
        xfilter.dwm <- sub("ppsa.", "", xfilter.min)
        dwmqry <- paste("select", dvars, "from", dfromqry, "where", xfilter.dwm)
 
        if (datsource == "CSV") {
          dwmx <- sqldf::sqldf(dwmqry, stringsAsFactors=FALSE)
        } else {
          cat("\n", "## STATUS: GETTING DWM DATA (", stabbr, ") ...", "\n")
          tryCatch( dwmx <- DBqryORACLE(dwmqry, dbconn, dbconnopen=TRUE), 
			error=function(e) stop("dwm query is invalid"))
        }

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

      if (savedata) {
        if (!datReturn) {
          dwmfnbase <- DBgetfn("dwm", invtype, outfn.pre, stabbr, 
			evalid=evalid, outfn.date=outfn.date)
          dwmfn <- FIESTA::fileexistsnm(outfolder, dwmfnbase, "csv")
          path.outdwmfn <- paste0(outfolder, "/", dwmfn, ".csv")
          write.csv(dwmx, file=path.outdwmfn, row.names=FALSE)
          dwmx <- NULL

        } else {
          if (i == 1) {
            write.table(dwmx, path.outdwmfn, sep=",", row.names=FALSE)
          } else {
            write.table(dwmx, file=path.outdwmfn, sep=",", row.names=FALSE, 
			col.names=FALSE, append=TRUE)
          }
        }
      }

      if (nbrstates <= 4) {
        dwm <- rbind(dwm, dwmx)
      } else {
        dwm <- NULL

        if (savedata) {
          warning("dwm data too big.. saving to file")
        } else {
          stop("dwm data too big.. must set savedata=TRUE")
        }            

      }
      rm(dwmx)
    }


    ## SAVE plt and cond tables
    if (savedata) {
      if (!datReturn){
        if (!is.null(pltx)) {
          pltfn <- paste0("plt_", invtype, "_", stabbr)
          if (iseval)

          pltfn <- DBgetfn("plt", invtype, outfn.pre, stabbr, 
			evalid=evalid, outfn.date=outfn.date)
          if (!overwrite)
            pltfn <- FIESTA::fileexistsnm(outfolder, pltfn, "csv")
          path.outpltfn <- paste0(outfolder, "/", pltfn, ".csv")
          write.csv(pltx, file=path.outpltfn, row.names=FALSE)
          pltx <- NULL
        }
  
        if (!is.null(condx)) {
          condfn <- DBgetfn("cond", invtype, outfn.pre, stabbr, 
			evalid=evalid, outfn.date=outfn.date)
          if (!overwrite)
            condfn <- FIESTA::fileexistsnm(outfolder, condfn, "csv")
          path.outcondfn <- paste0(outfolder, "/", condfn, ".csv")
          write.csv(condx, file=path.outcondfn, row.names=FALSE)
          condx <- NULL
        }
      } else {
        if (i == 1) {
          if (!is.null(pltx))
            write.table(pltx, path.outpltfn, sep=",", row.names=FALSE)
          if (!is.null(condx))
            write.table(condx, path.outcondfn, sep=",", row.names=FALSE)
        } else {
          if (!is.null(pltx))
            write.table(pltx, file=path.outpltfn, sep=",", row.names=FALSE, 
			col.names=FALSE, append=TRUE)
          if (!is.null(condx))
            write.table(condx, file=path.outcondfn, sep=",", row.names=FALSE, 
			col.names=FALSE, append=TRUE)
        }
      }
    }

    plt <- rbind(plt, pltx)
    cond <- rbind(cond, condx)
    rm(nbrcnd)
    rm(pltcondx)
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
  
    statesout <- FIESTA::addcommas(sapply(params$states, function(x){paste0("'", x, "'")}))
    rsout <- FIESTA::addcommas(sapply(params$rs, function(x){paste0("'", x, "'")}))
    stateFilter <- ifelse(is.null(params$stateFilter), FALSE, TRUE)

    outfile <- file(paste0(outfolder, "/", outparamfn, ".txt"), "w")
    cat(  "datsource <- ", params$datsource, "\"", "\n",
      "ZIP <- ", params$ZIP, "\n",
      "FS_FIADB <- ", params$FS_FIADB, "\n",
      "states <- c(", statesout, ")", "\n", 
      "rs <- c(", rsout, ")", "\n", 
      "invtype <- \"", params$invtype, "\"", "\n",
      "evalid <- ", FIESTA::getlistparam(params$evalid), "\n",  
      "evalCur <- ", params$evalCur, "\n",
      "evalEndyr <- ", FIESTA::getlistparam(params$evalEndyr), "\n",  
      "evalType <- \"", FIESTA::getlistparam(params$evalType), "\"", "\n",
      "allyrs <- ", params$allyrs, "\n",
      "invyrs <- ", FIESTA::getlistparam(params$invyrs), "\n",  
      "actual <- ", params$actual, "\n",
      "istree <- ", params$istree, "\n",
      "isseed <- ", params$isseed, "\n",
      "isveg <- ", params$isveg, "\n",
      "isdwm <- ", params$isdwm, "\n",
      "issp <- ", params$issp, "\n",
      "spcoords <- c(", spcoords, ")", "\n", 
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
      "outfn.pre <- \"", params$outfn.pre, "\"", "\n",
      "dbconn <- \"", params$dbconn, "\"", "\n",
      "dbconnopen <- ", params$dbconnopen, "\n",
      "overwrite <- ", params$overwrite, "\n",
     "\n",
    file = outfile, sep="")

    cat(  "fiadat <- DBgetPlots(datsource=datsource, ZIP=ZIP, FS_FIADB=FS_FIADB, 
	states=states, rs=rs, invtype=invtype, evalid=evalid, evalCur=evalCur, 
	evalEndyr=evalEndyr, evalType=evalType, allyrs=allyrs, invyrs=invyrs, 
	actual=actual, istree=istree, isseed=isseed, isveg=isveg, isdwm=isdwm, 
	issp=issp, spcoords=spcoords, spcondid1=spcondid1, defaultVars=defaultVars, 
	regionVars=regionVars, ACI=ACI, subcycle99=FALSE, intensity1=TRUE, 
	allFilter=allFilter, savedata=savedata, saveqry=saveqry, parameters=parameters, 
	outfolder=outfolder, outfn.pre=outfn.pre, dbconn=dbconn, dbconnopen=dbconnopen,
	overwrite=overwrite)",
    file = outfile, sep="")

    close(outfile)
  }

  ## Write out plot/condition counts to comma-delimited file.
  if (savedata)
    write.table(pltcnt, path.outpltcntfn, sep=",", row.names=FALSE)


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
  if (actual & !is.null(ACTUALcond)) {
    if (!is.null(plt)) {
      if (length(unique(ACTUALcond$PLT_CN)) != nbrplots) {
        notsame <- TRUE
        warning("number of plots in actualc table does not match plt table")
        if (nbrplots > length(unique(ACTUALcond$PLT_CN))){
          FORNONSAMPvals <- unique(plt[plt$CN %in% plt$CN[which(!plt$CN %in% ACTUALcond$PLT_CN)],
			"FORNONSAMP"])
          warning(paste("these plots have the following FORNONSAMP value: ", 
			FIESTA::addcommas(FORNONSAMPvals)))
        }
      }
    }
    if (!is.null(cond)) {
      if (!notsame & nrow(ACTUALcond) != nrow(cond)) 
        warning("number of conditions in actualc table does not match cond table")
    }
    fiadatlst$actualc <- ACTUALcond

    if (nrow(ACTUALplot) != nrow(plt))
      warning("number of rows in plt does not match ACTUALplot table")
    fiadatlst$actualp <- ACTUALplot
  }
  if (istree & !is.null(tree)) fiadatlst$tree <- setDF(tree)
  if (isseed & !is.null(seed)) fiadatlst$seed <- setDF(seed)

  if (issp) {
    cat("\n",
    "## STATUS: GETTING SHAPEFILE ...", "\n", "\n")

    for (coords in spcoords) {
      shpnm <- paste("shp", coords, sep="_")
      shpdatnm <- paste("shpdat", coords, sep="_")

      xycoords <- getcoords(coords)

      if (!is.null(get(shpdatnm)) && makeshp) {
        if (!is.null(pltx)) {
          nbrplots <- nrow(pltx)
          if (length(unique(get(paste("shpdat", shpcoord, sep="_"))$PLT_CN)) != nbrplots)
            warning("number of plots in ", shpnm, " does not match plt table")    
        }
        if (savedata) {
          shpfn <- DBgetfn("shp", invtype, outfn.pre, stabbrlst, evalid=evalid,
			othertxt=coords)
          shpdatfn <- DBgetfn("shpdat", invtype, outfn.pre, stabbrlst, evalid=evalid,
			othertxt=coords, outfn.date=outfn.date)
          if (!overwrite)
            shpdatfn <- FIESTA::fileexistsnm(outfolder, shpdatfn, "csv")
          write.csv(get(shpdatnm), paste0(outfolder, "/", shpdatfn, ".csv"), 
			row.names=FALSE)
        } else {
          shpfn <- NULL
        }

        ## Generate shapefile
        assign(shpnm, FIESTA::spMakeSpatialPoints(xyplt=get(shpdatnm), x=xycoords[1], 
		y=xycoords[2], uniqueid="PLT_CN", 
		prj4str="+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs",  
          	exportshp=savedata, outfolder=outfolder, outshpnm=shpfn, outfn.date=outfn.date,
			overwrite=overwrite))
        shpnm2 <- sub("shp", "spplt", shpnm)
        if (datReturn) fiadatlst[[shpnm2]] <- get(shpnm)
      }
    }
  }

  if (isveg) {
    if (!is.null(vspspp)) fiadatlst$vspspp <- setDF(vspspp)
    if (!is.null(vspstr)) fiadatlst$vspstr <- setDF(vspstr)
  }
  if (isdwm) 
    if (!is.null(dwm)) fiadatlst$dwm <- setDF(dwm)
    
  if (length(evalidlist) > 0) fiadatlst$evalid <- evalidlist
  fiadatlst$pltcnt <- pltcnt
  
  if (saveqry) cat("\n", paste("Saved queries to:", outfolder), "\n") 

  ## Close database connection
  if (datsource == "ORACLE") {
    if (!dbconnopen) {
      DBI::dbDisconnect(dbconn)
    } else {
      fiadatlst$dbconn <- dbconn
    }
  }
  
  if (returnPOP) 
    fiadatlst$POP_PLOT_STRATUM_ASSGN <- POP_PLOT_STRATUM_ASSGN


  ## Return data list
  return(fiadat=fiadatlst)
}

