DBgetStrata <- function(dat=NULL, datsource="ORACLE", ZIP=TRUE, FS_FIADB=TRUE, 
	uniqueid="CN", states=NULL, evalid=NULL, evalCur=FALSE, evalEndyr=NULL, 
	evalType="areavol", savedata=FALSE, outfolder=NULL, outfn.pre=NULL, 
	outfn.date = FALSE, overwrite=FALSE, dbconn=NULL, dbconnopen=FALSE, 
	POP_PLOT_STRATUM_ASSGN=NULL){
  ######################################################################################
  ## DESCRIPTION: This function gets the strata info and area by estimation unit from 
  ##		FIA Database, extracts and merges plot-level assignments to data file, and 
  ##		generates a lookup table of strata weights by estimation unit.
  ## You must have the following variables in your data set (pltcond): 
  ##    STATECD, INVYR, a uniqueid, and PLOT_STATUS_CD (if nonsampled plots in dataset).
  ##
  ## FIADB TABLES USED:
  ##   SURVEY       	## To get latest inventory year.
  ##   POP_EVAL      	## To get EVALID and EVALID years.
  ##   POP_ESTN_UNIT	## To get total area (AREATOT_EU-includes water) by estimation unit
  ##   POP_STRATUM 	## To get pixel counts by estimation unit and stratum.
  ##   POP_PLOT_STRATUM_ASSGN	## To get estimation unit & stratum assignment for each plot.  
  ####################################################################################


  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- ifelse(nargs() == 0 || (nargs() == 1 & !is.null(dbconn)) ||
		(nargs() == 2 & !is.null(dbconn) & dbconnopen), TRUE, FALSE)

  ## Set global variables  
  INVYR=PLOT_STATUS_CD=PLT_CN=STATECD=UNITCD=ESTUNIT=FIAPLOTS=P2POINTCNT=EVALID=invyrtab <- NULL

  parameters <- FALSE
  PLTdups <- FALSE

  ## SET OPTIONS
  options.old <- options()
  options(scipen=8) # bias against scientific notation
  on.exit(options(options.old), add=TRUE) 


  #######################################################3###################
  ## CHECK INPUT PARAMETERS
  ###########################################################################

  ## Check dat
  ########################################################
  datx <- pcheck.table(dat, gui=gui, caption="Data table?", returnDT=TRUE)

  if (!is.null(datx)) {

    ## If include datx, check uniqueid, and necessary variables (STATECD, UNITCD, INVYR)
    if (typeof(datx) == "S4") datx <- setDT(datx@data)

    ## Check uniqueid
    ########################################################
    uniqueid <- FIESTA::pcheck.varchar(var2check=uniqueid, varnm="uniqueid", 
		gui=gui, checklst=names(datx), caption="UniqueID variable", 
		warn=paste(uniqueid, "not in dat"))
    if (is.null(uniqueid)) stop("")
    setkeyv(datx, uniqueid)

    ## Check for necessary variables (STATECD, UNITCD, INVYR)
    vars2keep <- c("STATECD", "UNITCD", "INVYR")
    if (any(!vars2keep %in% names(datx))) {
      vars <- vars2keep[which(!vars2keep %in% names(datx))]
      stop(paste("must have following variables in dat:", paste(vars, collapse=", ")))
    }

    ## Check if user-defined state is in dataset
    datx.stcd <- sort(unique(datx[["STATECD"]]))
    datx.states <- FIESTA::pcheck.states(datx.stcd, "MEANING")

    if (!is.null(states)) {
      if (!all(states %in% datx.states)) {
        nostate <- states[which(!states %in% datx.states)]
        stop(paste("state not in dataset:", toString(nostate)))
      }
    } else {
      states <- datx.states
    }

    if (is.null(evalid) && "EVALID" %in% names(datx)) 
      evalid <- unique(datx$EVALID)

    invyrtab <- unique(datx[, c("STATECD", "INVYR")])
    setorder(invyrtab, "STATECD", "INVYR")
    allinvyrs <- sort(invyrtab[["INVYR"]])


    id <- NULL
    if ("ZSTCOPLOT" %in% names(datx)) {
      id <- "ZSTCOPLOT"
    } else if (all(c("STATE", "COUNTYCD", "PLOT") %in% names(datx))) {
      id <- c("STATE", "COUNTYCD", "PLOT")
    } else if (all(c("COUNTYCD", "PLOT") %in% names(datx))) {
      id <- c("COUNTYCD", "PLOT")
    } else if ("PLOT" %in% names(datx)) {
      id <- "PLOT"
    } else {
      message("no identifier in plot... assuming 1 Evaluation")
    }

    if (!is.null(id) && is.null(evalid)) {
      dups <- datx[, length(get(uniqueid)), by=id]
      if (any(dups$V1 > 1)) { 
        message("plot locations are duplicated... \n",
		"all plots are assigned an estimation unit and strata, \n",
		"but area and strata proportions are from evaluation ending in ", 
   		max(allinvyrs), "\n")
        PLTdups <- TRUE
      }
    }
  }

  ## Get Evalid
  evalInfo <- DBgetEvalid(datsource=datsource, ZIP=ZIP, FS_FIADB=FS_FIADB, 
		states=states, invyrtab=invyrtab, evalid=evalid, evalCur=evalCur, 
		evalEndyr=evalEndyr, evalType=evalType, dbconn=dbconn, 
		dbconnopen=TRUE, gui=gui)
  states <- evalInfo$states
  rslst <- evalInfo$rslst
  evalidlist <- evalInfo$evalidlist
  invtype <- evalInfo$invtype
  invyrs <- evalInfo$invyrs

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

  } else {
    SCHEMA <- ""
    SCHEMA. <- ""
  }

  
  ## Check savedata
  ###########################################################
  savedata <- FIESTA::pcheck.logical(savedata, varnm="savedata", 
		title="Save data to outfolder?", first="YES", gui=gui)

  ## Check parameters
  ###########################################################
  parameters <- FIESTA::pcheck.logical(parameters, varnm="parameters", 
		title="Save parameters", first="YES", gui=gui)

  ## Check outfolder/outfn
  if (savedata || parameters) {
    outfolder <- FIESTA::pcheck.outfolder(outfolder, gui=gui)
  
    ## GET outfn
#    if (is.null(outfn.pre) || gsub(" ", "", outfn.pre) == "") {
#      outfn.pre <- ""
#    } else {
#      outfn.pre <- paste0(outfn.pre, "_")
#    }

    outfn.date <- FIESTA::pcheck.logical(outfn.date, varnm="outfn.date", 
		title="Add date to outfile", first="FALSE", gui=gui)
  }

  ##################################################################
  ## DO WORK
  ##################################################################
 
  ## Get stcd
  ########################################################
  ## Get state abbreviations
  stabbrlst <- FIESTA::pcheck.states(states, statereturn="ABBR", gui=TRUE)

  if (datsource == "CSV") {

    ## POP_ESTN_UNIT table (ZIP FILE)- To get total area by estimation unit
    POP_ESTN_UNIT <- FIESTA::DBgetCSV("POP_ESTN_UNIT", stabbrlst, ZIP=ZIP, returnDT=TRUE)

    ## POP_STRATUM table (ZIP FILE) - To get pixel counts by estimation unit and stratum.
    POP_STRATUM <- FIESTA::DBgetCSV("POP_STRATUM", stabbrlst, ZIP=ZIP, returnDT=TRUE)

    ## POP_PLOT_STRATUM_ASSGN table (ZIP FILE) - 
    ## To get estimation unit & stratum assignment for each plot. 
    if (!is.null(POP_PLOT_STRATUM_ASSGN)) {
      if (!"STATECD" %in% names(POP_PLOT_STRATUM_ASSGN)) {
        message("STATECD not in POP_PLOT_STRATUM_ASSGN")
        POP_PLOT_STRATUM_ASSGN <- NULL
      }
      stcds <- FIESTA::pcheck.states(states, statereturn="VALUE")
      if (!all(stcds %in% unique(POP_PLOT_STRATUM_ASSGN[["STATECD"]]))) {
        message("POP_PLOT_STRATUM_ASSGN must include: ", paste(states, collapse=", "))
        POP_PLOT_STRATUM_ASSGN <- NULL
      }
    }
    if (is.null(POP_PLOT_STRATUM_ASSGN)) {
      POP_PLOT_STRATUM_ASSGN <- FIESTA::DBgetCSV("POP_PLOT_STRATUM_ASSGN", stabbrlst, 
		ZIP=ZIP, returnDT=TRUE)
    }
  }
 
  ## SET VARIABLE NAMES
  strvar <- "STRATUMCD"
  unitvar <- "ESTN_UNIT"
  areavar <- "ACRES"
  evalidlst <- {}

  ## TO GET STRATUM LEVEL INFORMATION BY ESTIMATION UNIT AND EVALID
  ############################################################################################
  ## ASSIGN GENERIC VARIABLES TO STRATA AND ESTIMATION UNIT NAMES
  statecd <- "STATECD"

  ## Define variables
  POP_ESTN_UNIT_VARS <- c("STATECD", "ESTN_UNIT", "ESTN_UNIT_DESCR", "AREA_USED", "EVALID")
  POP_STRATUM_VARS <- c("STATECD", "ESTN_UNIT", "STRATUMCD", "STRATUM_DESCR",
	"P2POINTCNT", "P1POINTCNT", "EVALID")
  POP_PLOT_STRATUM_ASSGN_VARS <- c("PLT_CN", "UNITCD", "STATECD", "INVYR", "ESTN_UNIT", 
 		"COUNTYCD", "STRATUMCD", "EVALID")

  ## Get unitarea - Area by estimation unit
  ######################################################################
  unitarea_qry <- paste0("select ", paste0(POP_ESTN_UNIT_VARS, collapse=", "), 
		" from ", SCHEMA., "POP_ESTN_UNIT where evalid in (", 
		toString(unlist(evalidlist)), ")")  
  if (datsource %in% "ORACLE") {
    tryCatch( unitarea <- DBqryORACLE(unitarea_qry, dbconn, dbconnopen=TRUE), 
			error=function(e) stop("unitarea query is invalid"))
  } else {
    unitarea <- sqldf::sqldf(unitarea_qry)
  }
  names(unitarea) <- toupper(names(unitarea))
  unitarea <- setDT(unitarea)
  setnames(unitarea, "AREA_USED", areavar)

  setkeyv(unitarea, c("STATECD", "ESTN_UNIT"))


  ## Get strata weights - pixel counts by estimation unit and stratum for EVALID
  ######################################################################
  popstratum_qry <- paste0("select ", paste0(POP_STRATUM_VARS, collapse=", "), 
		" from ", SCHEMA., "POP_STRATUM where evalid in (", 
		toString(unlist(evalidlist)), ")")
  if (datsource %in% "ORACLE") {
    tryCatch( stratalut <- DBqryORACLE(popstratum_qry, dbconn, dbconnopen=TRUE), 
			error=function(e) stop("pop stratum query is invalid"))
  } else {
    stratalut <- sqldf::sqldf(popstratum_qry)
  }
  names(stratalut) <- toupper(names(stratalut))

  ## Get strata assignment for each plot
  ######################################################################
  if (PLTdups) {
    stcdlst <- FIESTA::pcheck.states(states, "VALUE")
    strassgn_qry <- paste0("select ", paste0(POP_PLOT_STRATUM_ASSGN_VARS, collapse=", "),
  		" from ", SCHEMA., "POP_PLOT_STRATUM_ASSGN where STATECD in (", stcdlst, ")",
		" and EVALID like '%0'")
  } else {
    strassgn_qry <- paste0("select ", paste0(POP_PLOT_STRATUM_ASSGN_VARS, collapse=", "),
  		" from ", SCHEMA., "POP_PLOT_STRATUM_ASSGN where evalid in (", 
  		toString(unlist(evalidlist)), ")")
  }
  if (datsource %in% "ORACLE") {
    tryCatch( POP_PLOT_STRATUM_ASSGN <- DBqryORACLE(strassgn_qry, dbconn, dbconnopen=TRUE), 
			error=function(e) stop("pop assignment query is invalid"))

  } else {        
    POP_PLOT_STRATUM_ASSGN <- sqldf::sqldf(strassgn_qry)
  }  
  names(POP_PLOT_STRATUM_ASSGN) <- toupper(names(POP_PLOT_STRATUM_ASSGN))

  POP_PLOT_STRATUM_ASSGN <- setDT(POP_PLOT_STRATUM_ASSGN)
  setkey(POP_PLOT_STRATUM_ASSGN, PLT_CN)


  ## if datx != NULL, merge strata assignments to dat
  if (!is.null(datx)) {
    ## Check that the values of PLT_CN in POP_PLOT_STRATUM_ASSGN are all in datx
    FIESTA::check.matchval(datx, POP_PLOT_STRATUM_ASSGN, uniqueid, "PLT_CN",
		tab1txt="dat", tab2txt="POP_PLOT_STRATUM_ASSGN")

    ## Check if class of uniqueid in POP_PLOT_STRATUM_ASSGN matches class of cuniqueid in condx
    tabs <- FIESTA::check.matchclass(datx, POP_PLOT_STRATUM_ASSGN, uniqueid, "PLT_CN")
    datx <- tabs$tab1
    POP_PLOT_STRATUM_ASSGN <- tabs$tab2

    ## Attribute sampled plots outside of evaluation with the values from the 
    if (PLTdups) {
      datstrat <- merge(datx, unique(POP_PLOT_STRATUM_ASSGN[, c("PLT_CN", "ESTN_UNIT", "STRATUMCD")]),
 		by.x=uniqueid, by.y="PLT_CN")
    } else {
      if ("EVALID" %in% names(datx)) {
        datstrat <- merge(datx, unique(POP_PLOT_STRATUM_ASSGN[, 
		c("EVALID", "PLT_CN", "ESTN_UNIT", "STRATUMCD")]),
 		by.x=c("EVALID", uniqueid), by.y=c("EVALID", "PLT_CN"))
      } else {
          datstrat <- merge(datx, 
			unique(POP_PLOT_STRATUM_ASSGN[, c("PLT_CN", "ESTN_UNIT", "STRATUMCD")]),
 			by.x=uniqueid, by.y="PLT_CN")
      }
      datstratid <- uniqueid
    }

    if (nrow(datstrat) != nrow(datx)) {
      nostrata <- datx[!get(uniqueid) %in% datstrat[[uniqueid]], uniqueid, with=FALSE][[1]]
      if (!all(datx[nostrata, "PLOT_STATUS_CD"][[1]] < 3)) {
        warn1 <- paste("no strata assignment for", length(nostrata), "plots with PLOT_STATUS_CD = 3")
        if (length(nostrata) <= 20) {
          warning(paste0(warn1, ": ", paste(nostrata, collapse=", ")))
        } else {
          warning(warn1)
        }
      }
    }
    datstratid <- "CN"
  } else {
    datstrat <- POP_PLOT_STRATUM_ASSGN
    datstratid <- "PLT_CN"
  }

  ## Check if there are NULL strata values in datstrat and print to screen
  nullvals <- datstrat[is.na(get(strvar)) | get(strvar) == "NA",]
  nbrnull <- nrow(nullvals)
  if (nbrnull >= 1) {
    stop(paste("There are", nbrnull, "NULL values for strata. Do you want to fix them?"))
    print(nullvals)
  }

  ## Generate a table of plot counts by strata/estimation unit from datstrat
  #stratacnt <- datstrat[, length(get(datstratid)), by=c(statecd, "UNITCD", unitvar, strvar)]
  stratacnt <- datstrat[, length(get(datstratid)), by=c(statecd, unitvar, strvar)]
  setnames(stratacnt, "V1", "FIAPLOTS")
  #setkeyv(stratacnt, c(statecd, "UNITCD", unitvar, strvar))
  setkeyv(stratacnt, c(statecd, unitvar, strvar))

  ## Merge pixel counts and area by estimation unit
  stratacnt.cols <- c("STATECD", unitvar, strvar, "FIAPLOTS")
  stratalut <- merge(setDT(stratalut), stratacnt[, stratacnt.cols, with=FALSE], 
		by=c("STATECD", unitvar, strvar), all.x=TRUE)
  setkeyv(stratalut, c("STATECD", unitvar, strvar))

  stratalut[, FIAPLOTS := NULL]
 
  if (savedata) {
   
    datstratfn <- DBgetfn("pltstrat", invtype, outfn.pre, stabbrlst, evalid=evalidlist)
    unitareafn <- DBgetfn("unitarea", invtype, outfn.pre, stabbrlst, evalid=evalidlist)
    stratalutfn <- DBgetfn("stratalut", invtype, outfn.pre, stabbrlst, evalid=evalidlist)
    FIESTA::write2csv(datstrat, outfolder=outfolder, outfilenm=datstratfn, 
		outfn.date=outfn.date, overwrite=overwrite)
    FIESTA::write2csv(unitarea, outfolder=outfolder, outfilenm=unitareafn, 
		outfn.date=outfn.date, overwrite=overwrite)
    FIESTA::write2csv(stratalut, outfolder=outfolder, outfilenm=stratalutfn, 
		outfn.date=outfn.date, overwrite=overwrite)
  }

  ## Close database connection
  if (datsource == "ORACLE" && !dbconnopen) DBI::dbDisconnect(dbconn)


  if (parameters) {
    ## OUTPUTS A TEXTFILE OF INPUT PARAMETERS TO OUTFOLDER
    ###########################################################
    outparamfnbase <- paste0("DBgetStrata_parameters_", format(Sys.time(), "%Y%m%d"))
    outparamfn <- FIESTA::fileexistsnm(outfolder, outparamfnbase, "txt")

    outfile <- file(paste0(outfolder, "/", outparamfn, ".txt"), "w")
    cat(  "dat = ", as.character(bquote(dat)), "\n",
      "uniqueid = \"", uniqueid, "\"", "\n",
      "savedata = ", savedata, "\n",
      "outfolder = \"", outfolder, "\"", "\n",
    file = outfile, sep="")

    cat(  "strat = DBgetStrata(dat=dat, uniqueid=uniqueid, savedata=savedata, 
		outfolder=outfolder)",
    file = outfile, sep="")
    close(outfile)
  }
  

  ## GET VALUES TO RETURN
  FIAstrata <- list(pltstrat=setDF(datstrat), uniqueid=uniqueid, unitarea=setDF(unitarea), 
	unitvar=unitvar, areavar=areavar, strlut=setDF(stratalut), strvar=strvar, 
	strwtvar="P1POINTCNT", evalid=evalidlist)

  return(FIAstrata)
}

