#' Database - Gets stratification information from FIA database.
#' 
#' Gets strata information from FIA's Oracle database or FIA DataMart,
#' including: (1) strata and estimation unit assignment per plot; (2) total
#' area by estimation unit; (3) pixel counts and number plots by
#' strata/estimation unit. Include a data frame of plots, states, or evaluation
#' information.
#' 
#' Dependent packages: DBI, rgdal
#' 
#' The following variables must be present in dat: STATECD, UNITCD, INVYR, a
#' uniqueid (ex. "PLT_CN"), and PLOT_STATUS_CD (if nonsampled plots in
#' dataset).
#' 
#' FIADB TABLES USED: \tabular{lll}{ \tab FS_FIADB.SURVEY \tab To get latest
#' inventory year.\cr \tab FS_FIADB.POP_EVAL \tab To get EVALID and EVALID
#' years.\cr \tab FS_FIADB.POP_ESTN_UNIT \tab To get total area by estimation
#' unit (AREATOT_EU-includes water).\cr \tab FS_FIADB.POP_STRATUM \tab To get
#' pixel counts by stratum and estimation unit.\cr \tab
#' FS_FIADB.POP_PLOT_STRATUM_ASSGN \tab To get estimation unit & stratum
#' assignment for each plot.\cr }
#' 
#' Area by estimation unit includes total area for all plots (evaltype="all").
#' 
#' @param dat Data frame, comma-delimited file (*.csv), or shapefile (*.shp).
#' The strata value is merged to this table and returned as a data frame. See
#' details for necessary variables.
#' @param uniqueid String. The unique plot identifier of dat (e.g., 'CN').
#' @param datsource String. Source of data ('datamart', 'sqlite').
#' @param data_dsn String. If datsource='sqlite', the name of SQLite database
#' (*.sqlite).
#' @param states String or numeric vector. Name(s) (e.g., 'Arizona','New
#' Mexico') or code(s) (e.g., 4, 35) of states for strata if dat=NULL.
#' @param eval_opts List of evaluation options for 'FIA' or 'custom'
#' evaluations to determine the set of data returned. See help(eval_options)
#' for a list of options.
#' @param getassgn Logical. If TRUE, extracts plot assignments from
#' pop_plot_stratum_assgn table in database.
#' @param pop_plot_stratum_assgn Data frame. The pop_plot_stratum_assgn for
#' state(s).
#' @param savedata Logical. If TRUE, writes output to outfolder.
#' @param savedata_opts List. See help(savedata_options()) for a list
#' of options. Only used when savedata = TRUE.
#' @param dbconn Open database connection.
#' @param dbconnopen Logical. If TRUE, the dbconn connection is not closed. 
#' @param evalInfo List. List object output from DBgetEvalid or DBgetXY 
#' FIESTA functions.   
#' 
#' @return FIAstrata - a list of the following objects: \item{pltassgn}{ Data
#' frame. Plot-level strata/estimation unit assignment.  If dat is not NULL,
#' strata/estimation unit variables are appended to dat. } \item{pltassgnid}{
#' String. Name of unique identifier of plot in pltassgn. } \item{unitarea}{
#' Data frame. Total acres by estimation unit. } \item{unitvar}{ String. Name
#' of the estimation unit variable (ESTN_UNIT). } \item{areavar}{ String. Name
#' of the acre variable (ACRES). } \item{stratalut}{ Data frame. Strata look-up
#' table with summarized pixel counts (P1POINTCNT) by strata/estimation unit. }
#' \item{strvar}{ String. Name of the strata variable (STRATA). }
#' \item{strwtvar}{ String. Name of the strata weight variable (P1POINTCNT). }
#' \item{evalid}{ List. evalid by state. }
#' 
#' Outputs to outfolder (if savedata=TRUE): \tabular{ll}{ \tab - CSV file of
#' pltassgn (*'date'.csv).\cr \tab - CSV file of unitarea (*'date'.csv).\cr
#' \tab - CSV file of stratalut (*'date'.csv).\cr \tab - If collapsed, a CSV
#' file of original classes and new collapsed classes.\cr }
#' @note Steps used in data extraction:
#' 
#' \enumerate{ \item Get EVALID and EVALID years by state - DBgetEvalid().
#' \item unitarea: get total area by estimation unit for EVALID
#' (POP_ESTN_UNIT).  \item stratalut: get pixel counts by estimation unit and
#' stratum for EVALID (POP_STRATUM).  \item pltassgn: get estimation unit and
#' stratum assignment for each plot for EVALID.  (POP_PLOT_STRATUM_ASSGN).
#' \item If dat is not NULL, merge pltassgn assignment to dat.  \item Merge
#' number of plots to stratalut \item Check for only 1 MEASYEAR or 1 INVYR and
#' number of plots by strata/estimation unit.  If less than minimumnum plots
#' per strata/estimation unit collapse using the following algorithm.  }
#' 
#' Strata collapsing: \cr If there are less than minplotnum (10) plots in the
#' smallest strata of the estimation unit, these plots are grouped with the
#' larger strata in the same estimation unit and defined as the highest strata
#' value. If, after grouping, there are still less than minplotnum, all of
#' these plots are combined with the corresponding strata of the estimation
#' unit above.  If there are no records above, then they are combined with the
#' estimation unit below.  The process repeats, grouping the strata to the
#' highest strata value if necessary.  All grouping is restrained within survey
#' units (UNITCD).
#' 
#' More than one evaluation: \cr If attributing a table of plots and there are
#' plots that have been visited more than once, all plots are assigned an
#' estimation unit and strata value, but the area and strata proportions are
#' from the most current evaluation for the dataset. The plots outside the most
#' current evaluation are attributes with values from the next most current
#' evaluation occurring in the database.
#' @author Tracey S. Frescino
#' @keywords data
#' @examples
#' \dontrun{
#' # Get strata for the most current evaluation of a state (ex. Wyoming)
#' WYstrat1 <- DBgetStrata(states = "Wyoming",
#'                         eval_opts = list(Cur = TRUE))
#' names(WYstrat1)
#' 
#' head(WYstrat1$pltassgn)
#' WYstrat1$unitarea
#' WYstrat1$unitvar
#' WYstrat1$areavar
#' WYstrat1$strvar
#' WYstrat1$evalid
#' 
#' # Get strata information for a specific set of plots
#' WYstrat4 <- DBgetStrata(dat = WYplt)
#' names(WYstrat4)
#' 
#' head(WYstrat4$pltassgn)
#' WYstrat4$unitarea
#' WYstrat4$evalid
#' }
#' @export DBgetStrata
DBgetStrata <- function(dat = NULL, 
                        uniqueid = "CN", 
                        datsource = "datamart", 
                        data_dsn = NULL, 
                        states = NULL,
                        eval_opts = eval_options(Cur=TRUE),
                        savedata = FALSE, 
                        getassgn = TRUE, 
                        pop_plot_stratum_assgn = NULL,
                        savedata_opts = NULL,
                        dbconn = NULL,
                        dbconnopen = FALSE,
                        evalInfo = NULL
                        ){
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
  gui <- ifelse(nargs() == 0, TRUE, FALSE)

  ## Set global variables  
  INVYR=PLOT_STATUS_CD=PLT_CN=STATECD=UNITCD=ESTUNIT=FIAPLOTS=P2POINTCNT=EVALID=invyrtab <- NULL


  ## Define variables
  ZIP <- TRUE
  SCHEMA. <- ""
  parameters <- FALSE
  PLTdups <- FALSE
  eval <- "FIA"

  ##################################################################
  ## CHECK INPUT PARAMETERS
  ##################################################################
  
  ## Check arguments
  input.params <- names(as.list(match.call()))[-1]
  if (!all(input.params %in% names(formals(DBgetStrata)))) {
    miss <- input.params[!input.params %in% formals(DBgetStrata)]
    stop("invalid parameter: ", toString(miss))
  } 
 
  ## Check parameter lists
  pcheck.params(input.params, savedata_opts=savedata_opts)


  ## Set eval_options defaults
  eval_defaults_list <- formals(eval_options)[-length(formals(eval_options))] 
  for (i in 1:length(eval_defaults_list)) {
    assign(names(eval_defaults_list)[[i]], eval_defaults_list[[i]])
  } 
  ## Set user-supplied eval_opts values
  if (length(eval_opts) > 0) {
    for (i in 1:length(eval_opts)) {
      if (names(eval_opts)[[i]] %in% names(eval_defaults_list)) {
        assign(names(eval_opts)[[i]], eval_opts[[i]])
      } else {
        stop(paste("Invalid parameter: ", names(eval_opts)[[i]]))
      }
    }
  } else {
    message("no evaluation timeframe specified...")
    message("see eval and eval_opts parameters (e.g., eval='custom', eval_opts=eval_options(Cur=TRUE))\n")
    stop()
  }

  
  ## Set savedata defaults
  savedata_defaults_list <- formals(savedata_options)[-length(formals(savedata_options))]
  
  for (i in 1:length(savedata_defaults_list)) {
    assign(names(savedata_defaults_list)[[i]], savedata_defaults_list[[i]])
  }
  
  ## Set user-supplied savedata values
  if (length(savedata_opts) > 0) {
    if (!savedata) {
      message("savedata=FALSE with savedata parameters... no data are saved")
    }
    for (i in 1:length(savedata_opts)) {
      if (names(savedata_opts)[[i]] %in% names(savedata_defaults_list)) {
        assign(names(savedata_opts)[[i]], savedata_opts[[i]])
      } else {
        stop(paste("Invalid parameter: ", names(savedata_opts)[[i]]))
      }
    }
  }

  #############################################################################
  ## Set datsource
  ########################################################
  datsourcelst <- c("datamart", "sqlite")
  datsource <- pcheck.varchar(var2check=datsource, varnm="datsource", 
		checklst=datsourcelst, gui=gui, caption="Data source?") 
  if (!is.null(data_dsn)) {
    if (getext(data_dsn) %in% c("sqlite", "db", "db3")) {
      dbconn <- DBtestSQLite(data_dsn, dbconnopen=TRUE, showlist=FALSE)
      dsn_tables <- DBI::dbListTables(dbconn)
    } else {
      stop("only sqlite databases available currently")
    }     
  }
  
  
  ##################################################################
  ## CHECK PARAMETER INPUTS
  ##################################################################
  
  ## Check dat
  ########################################################
  datx <- pcheck.table(dat, gui=gui, caption="Data table?", returnDT=TRUE)

  ## Check pop_plot_stratum_assgn 
  ########################################################
  POP_PLOT_STRATUM_ASSGN <- pcheck.table(pop_plot_stratum_assgn, caption="ppsa?", returnDT=TRUE)

  if (!is.null(datx)) {

    ## Check uniqueid
    ########################################################
    uniqueid <- pcheck.varchar(var2check=uniqueid, varnm="uniqueid", 
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
    datx.states <- pcheck.states(datx.stcd, "MEANING")

    if (!is.null(states)) {
      if (!all(states %in% datx.states)) {
        nostate <- states[which(!states %in% datx.states)]
        stop(paste("state not in dataset:", toString(nostate)))
      }
    } else {
      states <- datx.states
    }

    if (is.null(evalid)) {
      if ("EVALID" %in% names(datx)) {
        evalid <- unique(datx$EVALID)
      } else {
        evalAll <- TRUE
        evalCur <- FALSE
      }
    }

    invyrtab <- unique(datx[, c("STATECD", "INVYR")])
    setorder(invyrtab, "STATECD", "INVYR")
    allinvyrs <- sort(invyrtab[["INVYR"]])


    id <- NULL
    if ("PLOT_ID" %in% names(datx)) {
      id <- "PLOT_ID"
    } else if (all(c("STATECD", "COUNTYCD", "PLOT") %in% names(datx))) {
      id <- c("STATECD", "COUNTYCD", "PLOT")
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

  if (!is.null(POP_PLOT_STRATUM_ASSGN) && is.null(evalid)) {
    evalidnm <- findnm("evalid", names(POP_PLOT_STRATUM_ASSGN))
    evalid <- sort(unique(POP_PLOT_STRATUM_ASSGN[[evalidnm]]))
  }

  ## Get DBgetEvalid parameters from eval_opts
  ################################################
  if (eval == "FIA") {
    evalCur <- ifelse (Cur, TRUE, FALSE) 
    evalAll <- ifelse (All, TRUE, FALSE) 
    evalEndyr <- Endyr
    measCur=allyrs <- FALSE
    measEndyr <- NULL
  } else {
    measCur <- ifelse (Cur, TRUE, FALSE) 
    allyrs <- ifelse (All, TRUE, FALSE) 
    if (length(Endyr) > 1) {
      stop("only one Endyr allowed for custom estimations")
    }
    measEndyr <- Endyr
    evalCur=evalAll <- FALSE
    evalEndyr <- NULL
  }

  if (allyrs) {
    saveSURVEY <- TRUE
  }


  ## Get Evalid
  ##########################################################
  if (is.null(evalInfo)) {

    list.items <- c("states", "evalidlist", "invtype", "invyrtab")
    evalInfo <- pcheck.object(evalInfo, "evalInfo", list.items=list.items)
    evalInfo <- tryCatch( DBgetEvalid(states = states, 
                          datsource = datsource, 
                          data_dsn = data_dsn, 
                          dbconn = dbconn,
                          dbconnopen = TRUE,
                          invyrtab = invyrtab, 
                          evalid = evalid, 
                          evalCur = evalCur, 
                          evalEndyr = evalEndyr, 
                          evalAll = evalAll, 
                          evalType = evalType, 
                          gui = gui),
			error = function(e) {
                  message(e,"\n")
                  return(NULL) })
    if (is.null(evalInfo)) {
      iseval <- FALSE
    }
  }
  if (is.null(evalInfo)) stop("no data to return")
  states <- evalInfo$states
  rslst <- evalInfo$rslst
  evalidlist <- evalInfo$evalidlist
  invtype <- evalInfo$invtype
  invyrs <- evalInfo$invyrs

  if (is.null(evalidlist)) {
    stop("must include evalCur, evalid, or evalEndyr")
  } else if (is.null(evalid)) {
    evalid <- evalidlist
  }
  
  ## Check savedata
  ###########################################################
  savedata <- pcheck.logical(savedata, varnm="savedata", 
		title="Save data to outfolder?", first="YES", gui=gui)

  ## Check parameters
  ###########################################################
  parameters <- pcheck.logical(parameters, varnm="parameters", 
		title="Save parameters", first="YES", gui=gui)

  ## Check outfolder/outfn
  if (savedata) {
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
  ## DO WORK
  ##################################################################
 
  ## Get stcd
  ########################################################
  ## Get state abbreviations
  stabbrlst <- pcheck.states(states, statereturn="ABBR", gui=TRUE)
  stcds <- pcheck.states(states, statereturn="VALUE", gui=TRUE)

  ## Define variables
  POP_ESTN_UNIT_VARS <- c("STATECD", "ESTN_UNIT", "ESTN_UNIT_DESCR", "AREA_USED", "EVALID")
  POP_STRATUM_VARS <- c("STATECD", "ESTN_UNIT", "STRATUMCD", "STRATUM_DESCR",
	"P2POINTCNT", "P1POINTCNT", "EVALID")


############ CSV only

  if (datsource == "datamart") {

    ## POP_ESTN_UNIT table (ZIP FILE)- To get total area by estimation unit
    POP_ESTN_UNIT <- DBgetCSV("POP_ESTN_UNIT", stabbrlst, returnDT=TRUE,
		stopifnull=FALSE)

    ## POP_STRATUM table (ZIP FILE) - To get pixel counts by estimation unit and stratum.
    POP_STRATUM <- DBgetCSV("POP_STRATUM", stabbrlst, returnDT=TRUE, 
		stopifnull=FALSE)

  } else if (datsource == "sqlite") {

    if (!"POP_STRATUM" %in% dsn_tables) {
      stop("POP_STRATUM not in database")
    }
    if (!"POP_ESTN_UNIT" %in% dsn_tables) {
      stop("POP_ESTN_UNIT not in database")
    }
    POP_STRATUM <- DBI::dbReadTable(dbconn, "POP_STRATUM")
    POP_STRATUM <- changeclass(POP_STRATUM)
    POP_ESTN_UNIT <- DBI::dbReadTable(dbconn, "POP_ESTN_UNIT")
    POP_ESTN_UNIT <- changeclass(POP_ESTN_UNIT)
  }
 
  if (getassgn) {
    POP_PLOT_STRATUM_ASSGN_VARS <- c("PLT_CN", "UNITCD", "STATECD", "INVYR", "ESTN_UNIT", 
 		"COUNTYCD", "STRATUMCD", "EVALID")

    ## POP_PLOT_STRATUM_ASSGN table (ZIP FILE) - 
    ## To get estimation unit & stratum assignment for each plot. 
    if (!is.null(POP_PLOT_STRATUM_ASSGN)) {
      if (!"STATECD" %in% names(POP_PLOT_STRATUM_ASSGN)) {
        message("STATECD not in POP_PLOT_STRATUM_ASSGN")
        POP_PLOT_STRATUM_ASSGN <- NULL
      }
      if (!all(stcds %in% unique(POP_PLOT_STRATUM_ASSGN[["STATECD"]]))) {
        message("POP_PLOT_STRATUM_ASSGN must include: ", paste(states, collapse=", "))
        POP_PLOT_STRATUM_ASSGN <- NULL
      }
      POP_PLOT_STRATUM_ASSGN_VARS <- POP_PLOT_STRATUM_ASSGN_VARS[POP_PLOT_STRATUM_ASSGN_VARS %in% 
		names(POP_PLOT_STRATUM_ASSGN)]
    }
    if (is.null(POP_PLOT_STRATUM_ASSGN)) {
      if (datsource == "datamart") {
        POP_PLOT_STRATUM_ASSGN <- DBgetCSV("POP_PLOT_STRATUM_ASSGN", stabbrlst, 
		returnDT=TRUE, stopifnull=FALSE)
      } else {
        if (!"POP_PLOT_STRATUM_ASSGN" %in% dsn_tables) {
          stop("POP_PLOT_STRATUM_ASSGN not in database")
        } 
        POP_PLOT_STRATUM_ASSGN <- DBI::dbReadTable(dbconn, "POP_PLOT_STRATUM_ASSGN")
        POP_PLOT_STRATUM_ASSGN <- changeclass(POP_PLOT_STRATUM_ASSGN) 
        DBI::dbDisconnect(dbconn)
      }      
    } else {
      if (datsource == "sqlite") {
        DBI::dbDisconnect(dbconn)
      }
    }
  }
############ End CSV only
 
  ## SET VARIABLE NAMES
  strvar <- "STRATUMCD"
  unitvar <- "ESTN_UNIT"
  areavar <- "ACRES"
 

  ## TO GET STRATUM LEVEL INFORMATION BY ESTIMATION UNIT AND EVALID
  ############################################################################################
  ## ASSIGN GENERIC VARIABLES TO STRATA AND ESTIMATION UNIT NAMES
  statecd <- "STATECD"

  ## unitarea query - area by estimation unit
  ######################################################################
  unitarea_qry <- paste0("select ", paste0(POP_ESTN_UNIT_VARS, collapse=", "), 
		" from ", SCHEMA., "POP_ESTN_UNIT where evalid in (", 
		toString(unlist(evalidlist)), ")") 

  ## stratalut query - pixel counts by stratum and estimation unit
  ######################################################################
  popstratum_qry <- paste0("select ", paste0(POP_STRATUM_VARS, collapse=", "), 
		" from ", SCHEMA., "POP_STRATUM where evalid in (", 
		toString(unlist(evalidlist)), ")")


  ## Run queries for unitarea and stratalut
  ################################################################
  unitarea <- sqldf::sqldf(unitarea_qry)
  stratalut <- sqldf::sqldf(popstratum_qry)


  ## Define table names, keys
  ################################################################  
  names(unitarea) <- toupper(names(unitarea))
  unitarea <- setDT(unitarea)
  setnames(unitarea, "AREA_USED", areavar)
  setkeyv(unitarea, c("STATECD", "ESTN_UNIT"))

  names(stratalut) <- toupper(names(stratalut))
  stratalut <- setDT(stratalut)
  setnames(stratalut, toupper(names(stratalut)))
  setkeyv(stratalut, c("STATECD", "ESTN_UNIT", "STRATUMCD"))

  if (getassgn) {

    ## strassgn query - strata assignments
    ######################################################################
    if (PLTdups) {
      stcdlst <- pcheck.states(states, "VALUE")
      strassgn_qry <- paste0("select ", paste0(POP_PLOT_STRATUM_ASSGN_VARS, collapse=", "),
  		" from ", SCHEMA., "POP_PLOT_STRATUM_ASSGN where STATECD in (", stcdlst, ")",
		" and EVALID like '%0'")
    } else {
      strassgn_qry <- paste0("select ", paste0(POP_PLOT_STRATUM_ASSGN_VARS, collapse=", "),
  		" from ", SCHEMA., "POP_PLOT_STRATUM_ASSGN where evalid in (", 
  		toString(unlist(evalidlist)), ")")
    }
    POP_PLOT_STRATUM_ASSGN <- sqldf::sqldf(strassgn_qry)
    if (nrow(POP_PLOT_STRATUM_ASSGN) == 0) {
      message(strassgn_qry)
      stop("invalid POP_PLOT_STRATUM_ASSGN query... now rows selected")
    }
    names(POP_PLOT_STRATUM_ASSGN) <- toupper(names(POP_PLOT_STRATUM_ASSGN))
    POP_PLOT_STRATUM_ASSGN <- setDT(POP_PLOT_STRATUM_ASSGN)
    setkey(POP_PLOT_STRATUM_ASSGN, PLT_CN)
 
    ## if datx != NULL, merge strata assignments to dat
    if (!is.null(datx)) {
      ## Check if class of uniqueid in POP_PLOT_STRATUM_ASSGN matches class of cuniqueid in condx
      tabs <- check.matchclass(datx, POP_PLOT_STRATUM_ASSGN, uniqueid, "PLT_CN")
      datx <- tabs$tab1
      POP_PLOT_STRATUM_ASSGN <- tabs$tab2

      ## Check that the values of PLT_CN in POP_PLOT_STRATUM_ASSGN are all in datx
      check.matchval(datx, POP_PLOT_STRATUM_ASSGN, uniqueid, "PLT_CN",
		tab1txt="dat", tab2txt="POP_PLOT_STRATUM_ASSGN")

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
          warn1 <- paste("no strata assignment for", length(nostrata), 
			"plots with PLOT_STATUS_CD = 3")
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
      uniqueid <- "PLT_CN"
    }

    ## Check if there are NULL strata values in datstrat and print to screen
    nullvals <- datstrat[is.na(get(strvar)) | get(strvar) == "NA",]
    nbrnull <- nrow(nullvals)
    if (nbrnull >= 1) {
      stop(paste("There are", nbrnull, "NULL values for strata. Do you want to fix them?"))
      print(nullvals)
    }

    ## Generate a table of plot counts by strata/estimation unit from datstrat
    stratacnt <- datstrat[, list(FIAPLOTS=length(get(datstratid))), 
		by=c(statecd, unitvar, strvar)]
    setkeyv(stratacnt, c(statecd, unitvar, strvar))

    ## Merge pixel counts and area by estimation unit
    stratacnt.cols <- c("STATECD", unitvar, strvar, "FIAPLOTS")
    stratalut <- merge(stratalut, stratacnt[, stratacnt.cols, with=FALSE], 
		by=key(stratalut), all.x=TRUE)
    stratalut[, FIAPLOTS := NULL]
  }

  if (savedata) {
    datExportData(unitarea,           
          savedata_opts=list(outfolder=outfolder, 
                              out_fmt=out_fmt, 
                              out_dsn=out_dsn, 
                              out_layer="unitarea",
                              outfn.pre=outfn.pre, 
                              outfn.date=outfn.date, 
                              overwrite_layer=overwrite_layer,
                              append_layer=append_layer,
                              add_layer=TRUE))
    
    datExportData(stratalut,           
          savedata_opts=list(outfolder=outfolder, 
                              out_fmt=out_fmt, 
                              out_dsn=out_dsn, 
                              out_layer="stratalut",
                              outfn.pre=outfn.pre, 
                              outfn.date=outfn.date, 
                              overwrite_layer=overwrite_layer,
                              append_layer=append_layer,
                              add_layer=TRUE))
    

    if (getassgn) {
      datExportData(datstrat,           
            savedata_opts=list(outfolder=outfolder, 
                                out_fmt=out_fmt, 
                                out_dsn=out_dsn, 
                                out_layer="pltassgn",
                                outfn.pre=outfn.pre, 
                                outfn.date=outfn.date, 
                                overwrite_layer=overwrite_layer,
                                append_layer=append_layer,
                                add_layer=TRUE))
    }
  }


  ## GET VALUES TO RETURN
  FIAstrata <- list(unitarea=setDF(unitarea), unitvar=unitvar, unitvar2="STATECD", 
		areavar=areavar, stratalut=setDF(stratalut), 
		strvar=strvar, getwt=TRUE, getwtvar="P1POINTCNT", evalid=evalidlist)
 
  if (getassgn) {
    FIAstrata$pltassgn <- setDF(datstrat)
    FIAstrata$pltassgnid <- uniqueid
  }

  return(FIAstrata)
}

