#' Database - Gets or checks FIA EVALIDs and/or gets inventory years from FIA's
#' online publicly-available DataMart
#' (https://apps.fs.usda.gov/fia/datamart/CSV/datamart_csv.html).
#' 
#' Extracts FIA EVALIDs for identifying an estimation group of plots. EVALIDs
#' may be extracted by most current evaluation (evalCur=TRUE) or by the end
#' year of an evaluation (evalEndyr) or all evaluations in the database for one
#' or more states. See details for more information.
#' 
#' 
#' FIA Evaluation\cr An Evaluation defines a group of plots in the FIA Database
#' used for state-level estimates, representing different spans of data and
#' different stratification and area adjustments. An Evaluation Type (evalType)
#' is used to identify a specific set of plots for a particular response to be
#' able to ensure a sample-based estimate for a population. See FIA's Database
#' documentation for current available Evaluation Types and descriptions
#' (https://www.fia.fs.fed.us/library/database-documentation/index.php).
#' 
#' EVALID\cr An EVALID is a unique code defining an Evaluation, generally in
#' the format of a 2-digit State code, a 2-digit year code, and a 2-digit
#' Evaluation Type code.
#' 
#' EVAL_TYP\cr \tabular{llll}{ \tab \bold{EVALIDCD} \tab \bold{EVAL_TYP} \tab
#' \bold{Description}\cr \tab 00 \tab EXPALL \tab All area\cr \tab 01 \tab
#' EXPVOL/EXPCURR \tab Area/Volume\cr \tab 03 \tab
#' EXPCHNG/EXPGROW/EXPMORT/EXPREMV \tab Area Change/GRM\cr \tab 07 \tab EXPDWM
#' \tab DWM\cr \tab 08 \tab EXPREGEN \tab Regeneration\cr \tab 09 \tab EXPINV
#' \tab Invasive\cr \tab 10 \tab EXPP2VEG \tab Veg profile\cr \tab 12 \tab
#' EXPCRWN \tab Crown\cr }
#' 
#' @param states String or numeric vector. Name (e.g., 'Arizona','New Mexico')
#' or code (e.g., 4, 35) of state(s) for evalid. If all states in one or more
#' FIA Research Station is desired, set states=NULL and use RS argument to
#' define RS.
#' @param RS String vector. Name of research station(s)
#' ('RMRS','SRS','NCRS','NERS','PNWRS').  Do not use if states is populated.
#' @param datsource Source of data ('datamart', 'sqlite').
#' @param data_dsn If datsource='sqlite', the file name (data source name) of
#' the sqlite database (*.sqlite).
#' @param invtype String. The type of FIA data to extract ('PERIODIC',
#' 'ANNUAL').  See further details below.
#' @param evalCur Logical. If TRUE, the most current evalidation is extracted
#' for state(s).
#' @param evalEndyr Number. The end year of the evaluation period of interest.
#' Selects only sampled plots and conditions for the evalidation period. If
#' more than one state, create a named list object with evalEndyr labeled for
#' each state (e.g., list(Utah=2014, Colorado=2013).
#' @param evalid Integer. One or more EVALID to check if exists.
#' @param evalAll Logical. If TRUE, gets all EVALIDs for invtype.
#' @param evalType String vector. The type(s) of evaluation of interest ('ALL',
#' 'CURR', VOL', 'GRM', 'P2VEG', 'DWM", 'INV', 'REGEN', 'CRWN').  The evalType
#' 'ALL' includes nonsampled plots; 'CURR' includes plots used for area
#' estimates; 'VOL' includes plots used for area and/or tree estimates; The
#' evalType 'GRM' includes plots used for growth, removals, mortality, and
#' change estimates (eval_typ %in% c(GROW, MORT, REMV, CHNG)).  Multiple types
#' are accepted. See details below and FIA database manual for regional
#' availability and/or differences.
#' @param invyrtab Data frame. A data frame including inventory years by state.
#' If NULL, it is generated from SURVEY table from FIA database based on states
#' and invtype.
#' @param dbTabs List of database tables the user would like returned.
#'  See help(dbTables) for a list of options.
#' @param dbconn Open database connection.
#' @param dbconnopen Logical. If TRUE, the dbconn connection is not closed.
#' @param gui Logical. If TRUE, gui windows pop up for parameter selection.
#' @return A list of the following objects: \item{states}{ String vector. State
#' names. } \item{rslst}{ String vector. FIA research station names included in
#' output. } \item{evalidlist}{ Named list. evalid by state. } \item{invtype}{
#' String. Inventory type for states(s) (ANNUAL/PERIODIC). } \item{invyrtab}{
#' Data frame. Inventory years by state for evalidlist. } \item{evalTypelist}{
#' Named list. Evaluation type(s) by state. } \item{invyrs}{ Named list.
#' Inventory years by state for evalidlist. } \item{SURVEY}{ Data frame. If
#' returnPOP=TRUE, the SURVEY table from FIADB. }
#' @note FIA database tables used:\cr 1. SURVEY - To get latest inventory year,
#' invyrtab = NULL\cr 2. POP_EVAL - To get EVALID and EVALID years
#' @author Tracey S. Frescino
#' @keywords data
#' @examples
#' \dontrun{
#' # Get evalid and inventory years for Wyoming
#' WYeval <- DBgetEvalid(states="Wyoming")
#' names(WYeval)
#'
#' WYeval$evalidlist
#' WYeval$invtype
#' WYeval$invyrtab
#' WYeval$evalType
#' WYeval$invyrs
#'
#'
#' # Get evalid for Utah and Wyoming
#' DBgetEvalid(states=c("Wyoming", "Utah"))
#'
#' # Get evalid for an FIA Research Station
#' RSevalid <- DBgetEvalid(RS="NERS")
#' names(RSevalid)
#' RSevalid$evalidlist
#' }
#' @export DBgetEvalid
DBgetEvalid <- function(states = NULL, 
                        RS = NULL, 
                        datsource = "datamart", 
                        data_dsn = NULL, 
                        invtype = "ANNUAL", 
                        evalCur = TRUE, 
                        evalEndyr = NULL, 
                        evalid = NULL, 
                        evalAll = FALSE, 
                        evalType = "VOL", 
                        invyrtab = NULL, 
                        dbTabs = dbTables(),
                        dbconn = NULL,
                        dbconnopen = FALSE,
                        gui = FALSE) {
  ###############################################################################
  ## DESCRIPTION: Get or check evalid from FIA database.
  ## You must have the following variables in dat: STATECD, INVYR, a uniqueid.
  ## Dependent packages: sqldf, httr (FIESTA::DBgetCSV)
  ##
  ## FIADB TABLES USED:
  ##   SURVEY        		## To get latest inventory year, invyrtab = NULL
  ##   POP_EVAL        	## To get EVALID and EVALID years
  ################################################################################

  if (!gui) {
    gui <- ifelse(nargs() == 0, TRUE, FALSE)
  }

  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  if (nargs() == 0) gui <- TRUE
  if (gui) {
    evalCur=evalAll=evalType <- NULL
  }
  
  
  ## Set global variables
  EVAL_GRP_Endyr=STATECD=START_INVYR=END_INVYR=POP_EVAL=POP_EVAL_GRP=
		POP_EVAL_TYP=SURVEY=POP_PLOT_STRATUM_ASSGN=evaltyp <- NULL
  EVALID=evalidlist=evalTypelist=invyrs <- NULL
  surveynm=popevalnm=popevalgrpnm=popevaltypnm=ppsanm=plotnm=PLOT <- NULL

  #evalresp <- TRUE
  evalresp <- FALSE
  isgrm=issccm <- FALSE 
  returnevalid <- FALSE
  nopoptables <- TRUE
  

  ## Define variables
  SCHEMA. <- ""

  ## Define evalType choices
  evalTypelst <- c("ALL", "CURR", "VOL", "GRM", "P2VEG", "INV", "DWM", "CHNG")
#  evalTypelst <- c("ALL", "CURR", "VOL", "CHNG", "DWM", "GROW", "MORT", "REMV", 
#		"CRWN", "INV", "P2VEG")


  
  ##################################################################
  ## CHECK INPUT PARAMETERS
  ##################################################################
  input.params <- names(as.list(match.call()))[-1]
  if (!all(input.params %in% names(formals(DBgetEvalid)))) {
    miss <- input.params[!input.params %in% formals(DBgetEvalid)]
    stop("invalid parameter: ", toString(miss))
  }

  ## Set dbTables defaults
  dbTables_defaults_list <- formals(dbTables)[-length(formals(dbTables))]
  for (i in 1:length(dbTables_defaults_list)) {
    assign(names(dbTables_defaults_list)[[i]], dbTables_defaults_list[[i]])
  } 
  ## Set user-supplied dbTables values
  if (length(dbTabs) > 0) {
    for (i in 1:length(dbTabs)) {
      if (names(dbTabs)[[i]] %in% names(dbTables_defaults_list)) {
        assign(names(dbTabs)[[i]], dbTabs[[i]])
      } else {
        stop(paste("Invalid parameter: ", names(dbTabs)[[i]]))
      }
    }
  }

  
  ##################################################################
  ## CHECK PARAMETER NAMES
  ##################################################################
  getlistfromdt <- function(dt, x, xnm="STATECD") {
     ## DESCRIPTION: generates a list of 1 or more values from a data table
     dtunique <- dt[, lapply(get(x), unique), by=xnm]
     xnames <- dtunique[[xnm]]
     dtlst <- as.list(data.frame(t(dtunique[, -1])))

     if (xnm == "STATECD") {
       names(dtlst) <- pcheck.states(xnames)
     } else {
       names(dtlst) <- xnames
     }    
     return(dtlst)
  }

  
  ##################################################################
  ## CHECK PARAMETER INPUTS
  ##################################################################
 
  ## Check invtyp
  #####################################################
  invtypelst <- c("ANNUAL", "PERIODIC")
  invtype <- pcheck.varchar(var2check=invtype, varnm="invtype", 
		gui=gui, checklst=invtypelst, caption="Inventory type?")
  ann_inv <- ifelse (invtype == "ANNUAL", "Y", "N")

  ## Check database connection
  ######################################################
  if (!is.null(dbconn) && DBI::dbIsValid(dbconn)) {
    datsource == "sqlite"
    dbtablst <- DBI::dbListTables(dbconn)
    if (length(dbtablst) == 0) {
      stop("no data in database")
    }
  } else {
    datsourcelst <- c("sqlite", "datamart", "csv", "obj")
    datsource <- pcheck.varchar(var2check=datsource, varnm="datsource", 
		gui=gui, checklst=datsourcelst, caption="Data source?",
           stopifnull=TRUE, stopifinvalid=TRUE)

    if (datsource == "sqlite" && !is.null(data_dsn)) {
      dbconn <- DBtestSQLite(data_dsn, dbconnopen=TRUE, showlist=FALSE)
      dbtablst <- DBI::dbListTables(dbconn)
      if (length(dbtablst) == 0) {
        stop("no data in database")
      }
    }
  }
 
  ## Check evalid, invyrtab, and state/RS parameters
  ######################################################
  rslst <- c("RMRS","SRS","NCRS","NERS","PNWRS")
  if (!is.null(evalid)) {
    evalid <- unique(unlist(evalid)) 
    if (any(nchar(evalid) > 6)) {
      stop("invalid evalid")
    }
    stcdlst <- unique(substr(evalid, 1, nchar(evalid)-4))
    states <- pcheck.states(stcdlst, "MEANING")
  } else if (!is.null(invyrtab)) {
    if (!all(class(invyrtab) %in% c("data.frame", "data.table"))) {
      stop("invyrtab must be a data frame or data table") 
    }
    statenm <- findnm("STATECD", names(invyrtab), returnNULL=FALSE) 
    if (!is.null(statenm)) {
      stop("STATECD must be in invyrtab")
    } else {
      stcdlst <- unique(invyrtab[[statenm]])
      states <- pcheck.states(stcdlst, "MEANING")
    }
  } else {
    ## Check RS states
    #####################################################
    RS <- pcheck.varchar(var2check=RS, varnm="RS", 
		checklst=rslst, caption="Research Unit?", gui=gui, multiple=TRUE)
    if (!is.null(RS) && !is.null(states)) {     
      RSstatelst <- FIESTAutils::ref_statecd[FIESTAutils::ref_statecd$RS %in% RS,"MEANING"]
      if (!all(states %in% RSstatelst)) {
        msg <- paste("RS and states are invalid...", 
			toString(states[!states %in% RSstatelst]))
        message(msg)
        states <- toString(states[states %in% RSstatelst])
        if (is.null(states) || states == "") {
          stop("")
        } else {
          message("getting coordinates for ", states)
        }
      }
    } else {
      states <- pcheck.states(states, RS=RS)
      if (is.null(states)) {
        states <- pcheck.states(states, RS=rslst)
      }
    }
    stcdlst <- pcheck.states(states, "VALUE")
  }
  rslst <- unique(FIESTAutils::ref_statecd[match(states, FIESTAutils::ref_statecd$MEANING), 
		"RS"])
  rslst[rslst %in% c("NERS", "NCRS")] <- "NRS"
  rslst <- unique(rslst)
 

  #########################################################################
  ## Get database tables - POP_EVAL, POP_EVAL_TYPE, SURVEY
  #########################################################################
  if (datsource == "sqlite") {
    ## Check states in database
    ###############################################
    plotnm <- chkdbtab(dbtablst, plot_layer)
    pltflds <- names(DBI::dbGetQuery(dbconn, 
				paste("select * from", plotnm, "where 1=2")))
    stcdlstdb <- DBI::dbGetQuery(dbconn, 
		paste("select distinct statecd from", plotnm))[[1]]
    if (!all(stcdlst %in% stcdlstdb)) {
      stcdmiss <- stcdlst[!stcdlst %in% stcdlstdb]
      message("statecds missing in database: ", toString(stcdmiss))
      stcdlsttmp <- stcdlst[stcdlst %in% stcdlstdb]
      if (length(stcdlsttmp) == 0) {
        stop("no data in database for: ", toString(stcdlst), "\n")
      } else {
        stcdlst <- stcdlsttmp
      }
      message("states in database: ", toString(stcdlst))
    }
    surveynm <- chkdbtab(dbtablst, "SURVEY")
    popevalnm <- chkdbtab(dbtablst, "POP_EVAL")
    popevalgrpnm <- chkdbtab(dbtablst, "POP_EVAL_GRP")
    popevaltypnm <- chkdbtab(dbtablst, "POP_EVAL_TYP")

  } else if (datsource == "datamart") {
    SURVEY <- DBgetCSV("SURVEY", stcdlst, 
                       returnDT=TRUE, stopifnull=FALSE)
    if (!is.null(SURVEY)) {
      surveynm <- "SURVEY"
    }
    POP_EVAL <- DBgetCSV("POP_EVAL", stcdlst, 
                          returnDT=TRUE, stopifnull=FALSE)
    if (!is.null(POP_EVAL)) {
      popevalnm <- "POP_EVAL"
    }
    POP_EVAL_GRP <- DBgetCSV("POP_EVAL_GRP", stcdlst, 
                              returnDT=TRUE, stopifnull=FALSE)
    if (!is.null(POP_EVAL_GRP)) {
      popevalgrpnm <- "POP_EVAL_GRP"
    }
    POP_EVAL_TYP <- DBgetCSV("POP_EVAL_TYP", stcdlst, 
                              returnDT=TRUE, stopifnull=FALSE)
    if (!is.null(POP_EVAL_TYP)) {
      popevaltypnm <- "POP_EVAL_TYP"
    }
    PLOT <- DBgetCSV("PLOT", stcdlst, 
                              returnDT=TRUE, stopifnull=FALSE)
    if (!is.null(PLOT)) {
      plotnm <- "PLOT"
      pltflds <- names(PLOT)
    } 
  } else {
    SURVEY <- pcheck.table(survey_layer, stopifnull=FALSE, stopifinvalid=FALSE)
    if (!is.null(SURVEY)) {
      surveynm <- "SURVEY"
    }
    POP_EVAL <- pcheck.table(popeval_layer, stopifnull=FALSE, stopifinvalid=FALSE)
    if (!is.null(POP_EVAL)) {
      popevalnm <- "POP_EVAL"
    }
    POP_EVAL_GRP <- pcheck.table(popevalgrp_layer, stopifnull=FALSE, stopifinvalid=FALSE)
    if (!is.null(POP_EVAL_GRP)) {
      popevalgrpnm <- "POP_EVAL_GRP"
    }
    POP_EVAL_TYP <- pcheck.table(popevaltyp_layer, stopifnull=FALSE, stopifinvalid=FALSE)
    if (!is.null(POP_EVAL_TYP)) {
      popevaltypnm <- "POP_EVAL_TYP"
    }
    PLOT <- pcheck.table(plot_layer, stopifnull=FALSE, stopifinvalid=FALSE)
    if (!is.null(PLOT)) {
      plotnm <- "PLOT"     
      pltflds <- names(PLOT)
    }
  }
 
  ## Get SURVEY table
  if (!is.null(surveynm)) {
    survey.qry <- paste0("select * from ", surveynm, 
      	" where ann_inventory = '", ann_inv, 
		"' and statecd in(", toString(stcdlst), ")")
    if (datsource == "sqlite") {
      SURVEY <- setDT(DBI::dbGetQuery(dbconn, survey.qry)) 
    } else {
      SURVEY <- setDT(sqldf::sqldf(survey.qry)) 
    }
    if (nrow(SURVEY) == 0) return(NULL)
  }
  if (!is.null(popevaltypnm) && !is.null(popevalgrpnm)) {
    pop_eval_typ_qry <- paste0("select ptyp.* 
           		FROM POP_EVAL_TYP ptyp
				JOIN POP_EVAL_GRP pgrp ON(pgrp.CN = ptyp.EVAL_GRP_CN)
				WHERE pgrp.statecd in(", toString(stcdlst), ")")
    if (datsource == "sqlite") {
      POP_EVAL_TYP <- setDT(DBI::dbGetQuery(dbconn, pop_eval_typ_qry)) 
    } else {
      POP_EVAL_TYP <- setDT(sqldf::sqldf(pop_eval_typ_qry)) 
    }
  }
  if (!is.null(popevalnm)) {
    if (!is.null(popevaltypnm)) {
      ## Define query POP_EVAL, POP_EVAL_TYP table
      popevalvars <- c("CN", "EVAL_GRP_CN", "RSCD", "EVALID", 
		"EVAL_DESCR", "STATECD", "START_INVYR", "END_INVYR", "LOCATION_NM")
      pop_eval_qry <- paste0("select ", toString(paste0("pev.", popevalvars)), ", 
		pet.eval_typ from ", popevaltypnm, " pet join ", SCHEMA., 
		popevalnm, " pev on (pev.cn = pet.eval_cn) ",
		"where pev.STATECD ", paste0("in(", toString(stcdlst), ")"))
    } else {
      pop_eval_qry <- paste0("select * from POP_EVAL
					WHERE statecd in(", toString(stcdlst), ")")
    }
    if (datsource == "sqlite") {
      POP_EVAL <- setDT(DBI::dbGetQuery(dbconn, pop_eval_qry)) 
    } else {
      POP_EVAL <- setDT(sqldf::sqldf(pop_eval_qry)) 
    }
  }
  if (!is.null(popevalgrpnm)) {
    pop_eval_grp_qry <- paste0("select * from POP_EVAL_GRP
					WHERE statecd in(", toString(stcdlst), ")")
    if (datsource == "sqlite") {
      POP_EVAL_GRP <- setDT(DBI::dbGetQuery(dbconn, pop_eval_grp_qry)) 
    } else {
      POP_EVAL_GRP <- setDT(sqldf::sqldf(pop_eval_grp_qry)) 
    }

    ## Add a parsed EVAL_GRP endyr to POP_EVAL_GRP
    eval_grpnm <- findnm("EVAL_GRP", names(POP_EVAL_GRP))
    POP_EVAL_GRP[, EVAL_GRP_Endyr := as.numeric(substr(POP_EVAL_GRP[[eval_grpnm]], 
		nchar(POP_EVAL_GRP[[eval_grpnm]]) - 3, nchar(POP_EVAL_GRP[[eval_grpnm]])))]
  }
 
  if (all(is.null(popevalnm) && is.null(popevaltypnm) && is.null(popevalgrpnm))) {
    nopoptables <- TRUE
 
    state.qry <- paste("select distinct statecd from", plotnm)
    if (datsource == "sqlite") {
      stcdlstdb <- tryCatch( DBI::dbGetQuery(dbconn, state.qry)[[1]],
				error = function(e) {
                  	return(NULL) })
    } else {
      stcdlstdb <- tryCatch( sqldf::sqldf(state.qry)[[1]],
				error = function(e) {
                  	return(NULL) })
    }  

    ## Check if given states are in the database
    if (!is.null(stcdlstdb)) {
      if (!all(stcdlst %in% stcdlstdb)) {
        stcdmiss <- stcdlst[!stcdlst %in% stcdlstdb]
        message("statecds missing in database: ", toString(stcdmiss))
      } 
    }
  } else {
    nopoptables <- FALSE
  }

  ## Create state filter
  stfilter <- getfilter("STATECD", stcdlst, syntax='sql')

  ## In POP_EVAL table, Texas has several evaluations based on East, West, Texas

  ## Check if evalid is valid. If valid, get invyrtab invyrs, evalidlist, and invtype
  if (!is.null(evalid) && !nopoptables) {
    evalidnm <- findnm("EVALID", names(POP_EVAL))

    ## Check if evalid is valid
    if (!all(evalid %in% POP_EVAL[[evalidnm]])) {
      notin <- evalid[!evalid %in% POP_EVAL[[evalidnm]]]
      stop("invalid EVALID: ", toString(notin))
    } else {
      ## Create invyrtab (if pop tables exist)
      if (!is.null(surveynm) && 
		all(!is.null(popevalnm) && !is.null(popevaltypnm) && !is.null(popevalgrpnm))) {
        invyrs <- list()
        evalidlist <- list()
        evalTypelist <- list()
        evalEndyrlist <- list()
        for (i in 1:length(evalid)) { 
          eval <- evalid[[i]]
          st <- substr(eval, 1, nchar(eval)-4)
          etypcd <- substr(eval, nchar(eval)-1, nchar(eval))
          state <- pcheck.states(st, "MEANING")
          pop_eval <- POP_EVAL[POP_EVAL[[evalidnm]] == eval,]
          startyr <- unique(min(pop_eval$START_INVYR))
          endyr <- unique(min(pop_eval$END_INVYR))
          ann_inventory <- SURVEY[SURVEY$STATECD == st & SURVEY$INVYR == endyr, 
		"ANN_INVENTORY"][[1]]
          invtype <- ifelse(ann_inventory == "Y", "ANNUAL", "PERIODIC")
          stinvyr <- startyr:endyr
          evalTypelist[[state]] <- unique(pop_eval$EVAL_TYP)[1]
          evalEndyrlist[[state]] <- endyr
          if (state %in% names(invyrs)) {
            invyrs[[state]] <- sort(unique(c(invyrs[[state]], stinvyr)))
            evalidlist[[state]] <- sort(unique(c(evalidlist[[state]], eval)))
          } else {
            invyrs[[state]] <- stinvyr
            evalidlist[[state]] <- eval
          }
          invyrtab <- invyrtab[invyrtab$ANN_INVENTORY == ann_inventory,]
        }
        returnevalid <- TRUE
      } else { 
        ## Create invyrtab (if pop tables do not exist but pop_plot_stratum_assgn table does)

        message("no SURVEY table in database... assuming ANNUAL inventory plots")
        invtype <- "ANNUAL"

        ## Check for pop_plot_stratum_assgn
        if (datsource == "sqlite") {
          ppsanm <- chkdbtab(dbtablst, ppsa_layer)
          if (!is.null(POP_PLOT_STRATUM_ASSGN)) {
            ppsanm <- "POP_PLOT_STRATUM_ASSGN"
            ppsaflds <- DBI::dbListFields(dbconn, ppsanm)
          } else {
            message("there is no pop_plot_stratum_assgn table in database")
          }

        } else if (datsource == "DATAMART") {
          POP_PLOT_STRATUM_ASSGN <- DBgetCSV("PLOT", stcdlst, 
                              returnDT=TRUE, stopifnull=FALSE)
          if (!is.null(POP_PLOT_STRATUM_ASSGN)) {
            ppsanm <- "POP_PLOT_STRATUM_ASSGN"
            ppsaflds <- names(POP_PLOT_STRATUM_ASSGN)
          } else {
            message("pop_plot_stratum_assgn is not in datamart")
          }

        } else {
          POP_PLOT_STRATUM_ASSGN <- pcheck.table(ppsa_layer, 
						stopifnull=FALSE, stopifinvalid=FALSE)
          ppsanm <- "POP_PLOT_STRATUM_ASSGN"
          if (!is.null(POP_PLOT_STRATUM_ASSGN)) {
            ppsanm <- "POP_PLOT_STRATUM_ASSGN"
            ppsaflds <- names(POP_PLOT_STRATUM_ASSGN)
          } 
        }

        if (!is.null(ppsanm)) {
          invyrnm <- findnm("INVYR", ppsaflds, returnNULL=TRUE) 
  
          ## Check evalids 
          evalid.qry <- paste("select distinct evalid from ", ppsanm) 
          if (datsource == "sqlite") {
            evalidindb <- DBI::dbGetQuery(dbconn, evalid.qry)
          } else {
            evalidindb <- sqldf::sqldf(evalid.qry)
          }
          if (!all(evalid %in% evalidindb)) {
            missevalid <- sort(!evalid[evalid %in% evalidindb])
            message(ppsa_layer, " is missing evalids: ", toString(missevalid))
            ppsanm <- NULL
          } else {
            if (!is.null(invyrnm)) {
              invqry <- paste("select statecd, invyr, count(*) NBRPLOTS from", ppsanm, 
			"where evalid in(", toString(evalid), ") group by statecd, invyr") 
              if (datsource == "sqlite") {
                invyrtab <- DBI::dbGetQuery(dbconn, invqry)
              } else {
                invyrtab <- sqldf::sqldf(invqry)
              } 
            } else {
              if (!is.null(plotnm)) {
                invyrnm <- findnm("INVYR", pltflds)  
                if (!is.null(invyrnm)) {     
                  invqry <- paste("select p.statecd, p.invyr, count(*) from", 
				ppsanm, "ppsa", "join", plotnm, 
				"p on(p.CN = ppsa.PLT_CN) where evalid in(", toString(evalid), 
				") group by p.statecd, p.invyr")
                    if (datsource == "sqlite") {
                    invyrtab <- DBI::dbGetQuery(dbconn, invqry)
                  } else {
                    invyrtab <- sqldf::sqldf(invqry)
                  }
                }
              }
            }
          }
        } else {
          ## Create invyrtab (if no pop tables or pop_plot_stratum_assgn)

          if (!is.null(plotnm)) {
            invyrnm <- findnm("INVYR", pltflds)  
            if (!is.null(invyrnm)) {     
              invqry <- paste("select statecd, invyr, count(*) from", 
				plotnm, "where statecd in(", toString(stcdlst), 
				") group by statecd, invyr")
              invyrtab <- DBI::dbGetQuery(dbconn, invqry)
            }
          }
        }
      } ## End create invyrtab
    }
  } ## End check evalid
 
  if (!returnevalid) {

    ## Check invyrtab. Data frame with inventory years by state
    if (is.null(invyrtab)) {
      if (!is.null(surveynm)) {
        ## GET invtype & ann_inventory
        ###########################################################
        invlst <- c("ANNUAL", "PERIODIC")
        if (is.null(invtype)) {
          invtype <- select.list(invlst, title="Inventory Type", multiple=FALSE)
          if (invtype == "") stop("")
        } else {
          if (!invtype %in% invlst) {
            warning("invalid invtype")
            invtype <- select.list(invlst, title="Select inventory type", multiple=FALSE)
            if (invtype == "") stop("")
          }
        }
        ## Create table of state, inventory year
        invyrqry <- paste0("select distinct STATECD, STATENM, STATEAB, ANN_INVENTORY, 
		INVYR from ", "SURVEY where STATENM IN(", 
		toString(paste0("'", states, "'")), 
		") and invyr <> 9999 and P3_OZONE_IND = 'N' order by STATECD, INVYR")

        if (datsource == "sqlite") {
          invyrtab <- DBI::dbGetQuery(dbconn, invyrqry)
        } else {
          invyrtab <- sqldf::sqldf(invyrqry, stringsAsFactors=FALSE)
        }
        cat("Inventory years by state...", "\n" )
        message(paste0(utils::capture.output(invyrtab), collapse = "\n"))


      } else {
        ## Create table of all inventory years in database
        invdbtab <- NULL
        if (!is.null(plotnm)) {
          invyrnm <- findnm("INVYR", pltflds, returnNULL=TRUE) 
        
          if (!is.null(invyrnm)) {
            invyrqry <- paste("select statecd, invyr, count(*) NBRPLOTS from", plotnm, 
			"where", stfilter, "group by statecd, invyr order by statecd, invyr")   
            if (datsource == "sqlite") {
              invyrtab <- DBI::dbGetQuery(dbconn, invyrqry)
            } else {
              invyrtab <- sqldf::sqldf(invyrqry, stringsAsFactors=FALSE)
            }
          }
        }
      }
    } else {
      if (!"INVYR" %in% names(invyrtab)) {
        stop("INVYR must be in invyrtab")
      }
      evalEndyr <- as.list(tapply(invyrtab$INVYR, invyrtab$STATECD, max))
      names(evalEndyr) <- pcheck.states(as.numeric(names(evalEndyr)), 
		statereturn="MEANING")

      if (!is.null(evalid)) {
        ## Check evalid
        if (length(evalid) != length(states)) {
          warning("evalid must be a named list matching states.. using evalEndyr")
        } else if (length(evalid) > 1 && !all(names(evalid) %in% states)) {
          warning("evalid names do not match states... using evalEndyr")
        } else if (sum(evalid == evalEndyr[names(evalid)]) < length(evalid)) {
          warning("invalid evalid... using evalEndyr")
        } 
      }  
    }
 
    if (!is.null(invyrtab)) {
      ## Get possible range of inventory years from invyrtab
      stinvyr.vals <- as.list(by(invyrtab$INVYR, invyrtab$STATECD, range))
      names(stinvyr.vals) <- pcheck.states(names(stinvyr.vals), "MEANING")
      stinvyr.min <- lapply(stinvyr.vals, '[[', 1)
      stinvyr.max <- lapply(stinvyr.vals, '[[', 2)
      invyr.min <- min(unlist(stinvyr.min))
      invyr.max <- max(unlist(stinvyr.max))

      if (!all(states %in% names(stinvyr.vals))) {
        missnames <- states[!states %in% names(stinvyr.vals)]
        misscodes <- pcheck.states(missnames, "VALUE")
        message("there is no data in the database for: ", toString(missnames))
        stcdlst <- stcdlst[!stcdlst %in% misscodes]
        states <- states[!states %in% missnames]
      }
    }
 
    if (!is.null(evalid)) {
      evalresp <- TRUE

    } else if (is.null(evalEndyr)) {
      ## Check evalAll
      ###########################################################
      evalAll <- pcheck.logical(evalAll, varnm="evalAll", 
		title="All evaluations?", first="YES", gui=gui)

      if (is.null(evalAll) || !evalAll) {
        ## Check evalCur
        evalCur <- pcheck.logical(evalCur, varnm="evalCur", 
		title="Most current evaluation?", first="YES", gui=gui)
        if (evalCur) evalresp <- TRUE
      } else {
        if (evalAll) {
          evalCur <- FALSE
          evalresp <- TRUE
        }
      }
 
      if ((is.null(evalCur) || !evalCur) && (is.null(evalAll) || !evalAll)) {
        if (gui) {
          evalresp <- select.list(c("NO", "YES"), title="Use an Evaluation?", 
		  	multiple=FALSE)
          if (evalresp == "") stop("")
          evalresp <- ifelse(evalresp == "YES", TRUE, FALSE)
        } else {
        #return(list(states=states, rslst=rslst, evalidlist=NULL, 
	#		invtype=invtype, invyrtab=invyrtab, SURVEY=SURVEY))


          returnlst <- list(states=states, rslst=rslst, 
                                 evalidlist=NULL, 
                                 invtype=invtype, 
                                 invyrtab=invyrtab, 
                                 evalType=evalTypelist, 
                                 SURVEY=SURVEY,
                                 PLOT=PLOT)
          if (datsource == "sqlite" && !dbconnopen) {
            DBI::dbDisconnect(dbconn)
          } else {
            returnlst$dbconn <- dbconn
          }
          return(returnlst)
        }
      }
    }

    ## Check evalEndyr
    if (!is.null(evalEndyr)) {
      evalresp <- TRUE
      if (class(evalEndyr)[1] != "list") {
        if (!is.vector(evalEndyr) || !is.numeric(as.numeric(evalEndyr))) {
          stop("invalid evalEndyr")
        }
        evalEndyr <- sapply(states, function(x) list(evalEndyr))
      } else {
        if (length(evalEndyr) > 1 && is.null(names(evalEndyr))) {
          stop("invalid evalEndyr... names do not match states")
        }
      }
      if (!is.null(invyrtab)) {
        for (st in names(evalEndyr)) {
          evalendyr <- evalEndyr[[st]]
          invendyr.min <- stinvyr.min[[st]]
          invendyr.max <- stinvyr.max[[st]]

          if (all(evalendyr < invendyr.min) || any(evalendyr > invendyr.max)) {
            message(paste("check evalEndyr.. outside of range in database:", st))    
            evalEndyr[[st]] <- invendyr.max
            #evalresp <- FALSE
          }
        }
      } 
    }
 
    ## Get last year of evaluation period and the evaluation type
    if (evalresp) {
      ## Get the evalidation type
      evalType <- pcheck.varchar(var2check=evalType, varnm="evalType", gui=gui, 
		checklst=evalTypelst, caption="Evaluation type", multiple=TRUE, 
		preselect="VOL")
      if (is.null(evalType)) {
        evalType <- "VOL"
      }
 
      if (datsource == "sqlite" && nopoptables) {
        ppsanm <- chkdbtab(dbtablst, ppsa_layer)
        if (is.null(ppsanm)) {
          message("need to include pop_plot_stratum_assgn table in database to extract an FIA evaluation\n")
          message("database tables: ", toString(dbtablst))
          stop()
        }

        ## Create lookup and get code for evalType
        evalCode <- c("00","01","01","03")
        names(evalCode) <- c("ALL", "CURR", "VOL", "CHNG")  
        evalTypecd <- unique(evalCode[which(names(evalCode) %in% evalType)])

        ppsaflds <- DBI::dbListFields(dbconn, ppsanm)
        ppsastnm <- findnm("STATECD", ppsaflds, returnNULL=TRUE)
        if (!is.null(ppsastnm)) {
          eval.qry <- paste("select distinct STATECD, EVALID from", ppsanm, 
			"where STATECD in(", toString(stcdlst), ")")
        } else {
          eval.qry <- paste("select distinct EVALID from", ppsanm) 
        }

        ## Get table of EVALID found in database
#        eval.qry <- paste("select distinct STATECD, EVALID 
#			from", ppsanm,  
#			"where", stfilter, "order by STATECD, EVALID")
        evaldt <- tryCatch( setDT(DBI::dbGetQuery(dbconn, eval.qry)),
			error=function(e) return(NULL))
        if (is.null(evaldt)) {
          return(NULL)
        }
 
        if (!"STATECD" %in% names(evaldt)) {
          evaldt[, STATECD := substr(EVALID, nchar(EVALID) - 5, nchar(EVALID)-4)]
          evaldt <- evaldt[evaldt$STATECD %in% as.character(stcdlst),]
        }

        ## Add endyr and evaltType columns to dataframe
        evaldt[, Endyr := substr(EVALID, nchar(EVALID) - 3, nchar(EVALID)-2)]
        evaldt[, evaltyp := substr(EVALID, nchar(EVALID)-1, nchar(EVALID))]

        if (!all(evalTypecd %in% unique(evaldt$evaltyp))) { 
          evaldttyp <- sort(unique(evaldt$evaltyp))
          notype <- evalTypecd[!evalTypecd %in% evaldttyp]
          if (length(notype) > 0) {
            stop(notype, " not in database")
          } else {
            stop("invalid evalType... must be in following list: ", toString(evaldttyp)) 
          }
        }

        ## Create list of evalTypes
        evalTypelist <- rep(list(evalType), length(states))
        names(evalTypelist) <- states
     
        ## Subset evaldt to evalType
        evaldt <- evaldt[evaltyp %in% evalTypecd,]
        evaldt$YEAR <- as.numeric(paste0("20", evaldt$Endyr))

        ## Generate evalidlist
        if (!is.null(evalid)) {
          evaldt <- evaldt[evaldt$EVALID %in% evalid,]
          if (nrow(evaldt) == 0) {
            stop("evalid not in database")
          }
          evalAll <- TRUE
        }
 
        if (evalAll) {
          evalidlist <- getlistfromdt(evaldt, x="EVALID")
          evalEndyrlist <- getlistfromdt(evaldt, x="YEAR")
        } else if (evalCur) {
          Endyr.max <- evaldt[, list(Endyr=max(Endyr)), by="STATECD"]
          evaldt <- merge(evaldt, Endyr.max, by=c("STATECD", "Endyr"))
          evalidlist <- getlistfromdt(evaldt, x="EVALID")
          evalEndyrlist <- getlistfromdt(evaldt, x="YEAR")
        } else if (!is.null(evalEndyr)) {
          #if (!is.numeric(evalEndyr))  stop("evalEndyr must be numeric yyyy")
          if (any(sapply(evalEndyr, function(x) nchar(x) != 4))) {
            stop("evalEndyr must be numeric yyyy")
          }
          yr <- substr(unlist(evalEndyr), 3, 4)
          evaldt <- evaldt[Endyr %in% yr, ]
          evalidlist <- getlistfromdt(evaldt, x="EVALID")
          evalEndyrlist <- getlistfromdt(evaldt, x="YEAR")
        } 

        ## Create table of inventory years
        if (!is.null(ppsanm)) {
          if ("INVYR" %in% ppsaflds) {
            invqry <- paste("select statecd, invyr, count(*) NBRPLOTS from", ppsanm, 
			"where evalid in(", toString(unlist(evalidlist)), 
			") group by statecd, invyr")  
          } else if (!is.null(plotnm) && "INVYR" %in% DBI::dbListFields(dbconn, plotnm)) {
             invqry <- paste("select p.statecd, p.invyr, count(*) NBRPLOTS from", 
			ppsanm, "ppsa", "join", plotnm, "p ON(p.CN = ppsa.PLT_CN)",
			"where evalid in(", toString(unlist(evalidlist)), 
			") group by p.statecd, p.invyr")  
          }  
          invyrtab <- DBI::dbGetQuery(dbconn, invqry)       
        } else if (!is.null(plotnm) && "INVYR" %in% DBI::dbListFields(dbconn, plotnm)) {
          invqry <- paste("select statecd, invyr, count(*) NBRPLOTS from", plotnm, 
			"where", stfilter, "group by statecd, invyr")   
          invyrtab <- DBI::dbGetQuery(dbconn, invqry)
        } else {
          invyrtab <- NULL
        }
      } else {    ## datsource="datamart" or datsource="csv" & poptables
        invyrs <- list()
        evalidlist <- sapply(states, function(x) NULL)
        evalEndyrlist <- sapply(states, function(x) NULL)

        ## check evalType
        if (invtype == "PERIODIC" && evalType == "ALL") {
          evalType <- "CURR"
        } else {
          if (length(grep("VOL", evalType, ignore.case=TRUE)) > 0) {
            evalType[grep("VOL", evalType, ignore.case=TRUE)] <- "VOL" 
          }
          if (length(grep("VOL", evalType, ignore.case=TRUE)) > 0 && 
			length(grep("CURR", evalType, ignore.case=TRUE)) > 0) {
            evalType <- evalType[-grep("CURR", evalType, ignore.case=TRUE)]
          }  
          if (length(grep("GRM", evalType, ignore.case=TRUE)) > 0) 
            evalType[grep("GRM", evalType, ignore.case=TRUE)] <- "GROW"  
        }

        evalTypelist <- sapply(states, function(x) list(unique(evalType)))
        evalTypelist <- lapply(evalTypelist, function(x) paste0("EXP", x))

      
        ## Loop thru states
        for (stcd in stcdlst) {
          state <- pcheck.states(stcd, "MEANING")
          stabbr <- pcheck.states(stcd, "ABBR")
          stinvyrs <- unique(stinvyr.vals[[state]])
          invtype.invyrs <- setDT(invyrtab)[invyrtab$STATECD == stcd][["INVYR"]]

          ## In POP_EVAL table, Texas has several evaluations based on East, West, Texas
          ## Remove East and West in LOCATION_NM and EVAL_DESCR
          if (stcd == 48) {
            POP_EVAL_GRPstcd <- POP_EVAL_GRP[STATECD == stcd & 
		            !grepl("EAST", POP_EVAL_GRP$EVAL_GRP_DESCR, ignore.case=TRUE) & 
		            !grepl("WEST", POP_EVAL_GRP$EVAL_GRP_DESCR, ignore.case=TRUE), ]
          } else {
            POP_EVAL_GRPstcd <- POP_EVAL_GRP[STATECD == stcd,]
          }
   
          ## Get evalid and inventory years from POP_EVAL table
          setkey(POP_EVAL, "EVAL_GRP_CN")
          setkey(POP_EVAL_GRPstcd, "CN")

          ## Subset POP_EVAL/POP_EVAL_GRP by state and inventory type
#          popevaltab <- POP_EVAL[POP_EVAL$EVAL_GRP_CN %in% POP_EVAL_GRPstcd$CN,]
          popevalgrptab <- POP_EVAL_GRPstcd[POP_EVAL_GRPstcd$EVAL_GRP_Endyr %in% invtype.invyrs,]
          popevaltab <- POP_EVAL[POP_EVAL$EVAL_GRP_CN %in% popevalgrptab$CN,]
#          popevaltab <- POP_EVAL[POP_EVAL$EVAL_GRP_CN %in% POP_EVAL_GRPstcd$CN &
#		  POP_EVAL$EVAL_TYP %in% evalTypelist[[state]],]
#          popevaltab <- POP_EVAL[POP_EVAL_GRPstcd[, c("CN", "EVAL_GRP_Endyr")]]
          #popevaltab <- popevaltab[popevaltab$END_INVYR %in% invtype.invyrs,]
          POP_EVAL_endyrs <- na.omit(unique(popevalgrptab[["EVAL_GRP_Endyr"]]))

          if (!is.null(evalEndyr)) {
            Endyr <- evalEndyr[[state]]
            if (!all(Endyr %in% POP_EVAL_endyrs)) {
              missEndyr <- Endyr[!Endyr %in% POP_EVAL_endyrs]
              stop(paste0(toString(missEndyr), " data are not in ", 
                        stabbr, "_", "POP_EVAL: ", toString(POP_EVAL_endyrs)))
            }  
          } else {   ## is.null(evalEndyr)
            if (evalCur) {
              Endyr <- max(POP_EVAL_endyrs)
            } else if (evalAll) {
              Endyr <- POP_EVAL_endyrs
            } else {
              if (length(POP_EVAL_endyrs) > 1 && gui) {
                Endyr <- select.list(as.character(POP_EVAL_endyrs), 
                                   title="Eval End Year?", multiple=FALSE)
                if (Endyr == "") stop("")
              } else {
                Endyr <- max(POP_EVAL_endyrs)
                message("No end year specified.. using most current year in database")
              }
            }
          }
          ## Populate evalEndyrlist
          evalEndyrlist[[state]] <- Endyr

          ## Subset popevaltab by Endyr
          #popevaltab <- popevaltab[END_INVYR %in% Endyr,]
          popevalgrptab <- POP_EVAL_GRPstcd[POP_EVAL_GRPstcd$EVAL_GRP_Endyr %in% Endyr,]
          popevaltab <- POP_EVAL[POP_EVAL$EVAL_GRP_CN %in% popevalgrptab$CN,]


          ## Check evalType with evalType in database for state
          evalType.chklst <- unique(popevaltab$EVAL_TYP)

          if (invtype == "ANNUAL") {
            if (!all(evalTypelist[[state]] %in% evalType.chklst)) {
              eType.invalid <- evalTypelist[[state]][!evalTypelist[[state]] %in% evalType.chklst]
              message("removing invalid evalType for ", state, ": ", 
                    toString(eType.invalid), "... \nmust be following list: ", 
                    toString(evalType.chklst))
              evalTypelist[[state]] <- evalTypelist[[state]][!evalTypelist[[state]] %in% eType.invalid]
            }
            evalidall <- unique(popevaltab$EVALID[!is.na(popevaltab$EVALID)])
            evalidlist[[state]] <- 
              sort(unique(popevaltab$EVALID[popevaltab$EVAL_TYP %in% evalTypelist[[state]]]))
            invyrs[[state]] <- 
              min(popevaltab$START_INVYR, na.rm=TRUE):max(popevaltab$END_INVYR, na.rm=TRUE)

          } else {
            if (!all(evalTypelist[[state]] %in% evalType.chklst)) { 
              evalid.min <- min(popevaltab$EVALID)
              evalTypelist[[state]] <- 
                popevaltab[popevaltab$EVALID == min(popevaltab$EVALID), "EVAL_TYP"][1]
              message(paste("invalid evalType for", state, "...using", evalTypelist[[state]]))
            }
            evalidlist[[state]] <- 
              sort(unique(popevaltab$EVALID[popevaltab$EVAL_TYP %in% evalTypelist[[state]]]))
            invyrs[[state]]  <- ifelse (any(is.na(popevaltab$END_INVYR)), 
                                      unique(as.numeric(popevaltab$REPORT_YEAR_NM)), 
                                      min(popevaltab$START_INVYR, na.rm=TRUE):max(popevaltab$END_INVYR, na.rm=TRUE))
          }  ## invtype
        }  ## for state loop
      }  ## datsource
    } else {
      if (datsource == "sqlite") {
        ## Create table of inventory years
        invdbtab <- NULL
        if (!is.null(plotnm) && "INVYR" %in% DBI::dbListFields(dbconn, plotnm)) {
          invqry <- paste("select statecd, invyr, count(*) NBRPLOTS from", plotnm, 
			"where", stfilter, "group by statecd, invyr order by statecd, invyr")   
          invyrtab <- DBI::dbGetQuery(dbconn, invqry)
        }
      }
    }
  }  ## returnevalid
 

  returnlst <- list(states=states, rslst=rslst, 
                    evalidlist=evalidlist, 
                    invtype=invtype, invyrtab=invyrtab, 
                    evalTypelist=evalTypelist, 
                    evalEndyrlist=evalEndyrlist)
  if (!is.null(invyrs)) returnlst$invyrs <- invyrs

  ## Return population information
  returnlst$SURVEY <- SURVEY
  returnlst$PLOT <- PLOT
  #if (is.null(POP_PLOT_STRATUM_ASSGN)) {
  #  returnlst$POP_PLOT_STRATUM_ASSGN <- POP_PLOT_STRATUM_ASSGN
  #}
  #returnlst$POP_EVAL <- POP_EVAL[EVALID %in% unlist(evalidlist),]

  if (datsource == "sqlite" && !dbconnopen) {
    DBI::dbDisconnect(dbconn)
  } else {
    returnlst$dbconn <- dbconn
  }

  return(returnlst)
}