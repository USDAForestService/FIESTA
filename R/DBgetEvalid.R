DBgetEvalid <- function (states=NULL, rs=NULL, datsource="CSV", ZIP=TRUE, 
	FS_FIADB=TRUE, invyrtab=NULL, invtype="ANNUAL", evalCur=TRUE, evalEndyr=NULL, 
	evalid=NULL, evalType="AREAVOL", dbconn=NULL, dbconnopen=FALSE, isdwm=FALSE,
	gui=FALSE) {

  ###############################################################################
  ## DESCRIPTION: Gets evalid or checks evalid from FIA database.
  ## You must have the following variables in dat: STATECD, INVYR, a uniqueid.
  ## Dependent packages: DBI
  ##
  ## FIADB TABLES USED:
  ##   SURVEY        		## To get latest inventory year, invyrtab = NULL
  ##   POP_EVAL        	## To get EVALID and EVALID years
  ################################################################################

  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  if (nargs() == 0) gui <- TRUE

  if (!gui)
    gui <- ifelse(nargs() == 0 || (nargs() == 1 & !is.null(dbconn)) ||
		(nargs() == 2 & !is.null(dbconn) & dbconnopen), TRUE, FALSE)
  

  ## SET OPTIONS
  options.old <- options()
  options(scipen=8) # bias against scientific notation
  on.exit(options(options.old), add=TRUE) 

  ## Set global variables
  EVALID=evalidlist=evalTypelist <- NULL

  if (gui) {
    if (!is.null(dbconn)) {
      if (DBI::dbIsValid(dbconn)) {
        datsource <- "ORACLE"
      } else {
        stop("dbconn is invalid")
      }
    } else {
      if (!is.null(dbconnopen) && !dbconnopen) dbconnopen <- NULL
      datsource <- NULL
    }
    ZIP=FS_FIADB=invtype=evalCur=evalType <- NULL
  }
  invyrs <- NULL
  evalresp <- FALSE

  ##################################################################
  ## CHECK INPUT PARAMETERS
  ##################################################################
  ## Check datsource
  datsourcelst <- c("ORACLE", "CSV")
  datsource <- FIESTA::pcheck.varchar(var2check=datsource, varnm="datsource", 
		checklst=datsourcelst, gui=gui, caption="Data source?", stopifnull=TRUE)  

  if (datsource == "CSV") {
    if (!"sqldf" %in% rownames(installed.packages()))
      stop("the sqldf package is required when datsource='ORACLE'")
  
    if (!"httr" %in% rownames(installed.packages()))
      stop("the httr package is required when datsource='ORACLE'")

    ## Check ZIP
    ZIP <- FIESTA::pcheck.logical(ZIP, varnm="ZIP", title="Zip files?", 
		gui=gui, first="YES")
  } else {
    if (!"DBI" %in% rownames(installed.packages()))
      stop("the DBI package is required when datsource='ORACLE'")
  }

  ## If evalid is not NULL, get state
  rslst <- c("RMRS","SRS","NCRS","NERS","PNWRS")
  if (!is.null(evalid)) {
    evalid <- unique(unlist(evalid)) 
    stcdlst <- unique(unlist(sapply(evalid, substr, nchar(evalid)-5, nchar(evalid)-4)))
    states <- FIESTA::pcheck.states(stcdlst, "MEANING")
  } else if (!is.null(invyrtab)) {
    if (!all(class(invyrtab) %in% c("data.frame", "data.table"))) 
      stop("invyrtab must be a data frame or data table") 
    if (!"STATECD" %in% names(invyrtab)) {
      stop("STATECD must be in invyrtab")
    } else {
      stcdlst <- unique(invyrtab[["STATECD"]])
      states <- FIESTA::pcheck.states(stcdlst, "MEANING")
    }
  } else {
    rs <- FIESTA::pcheck.varchar(var2check=rs, varnm="rs", 
		checklst=rslst, caption="Research Unit?", gui=gui, multiple=TRUE)
    if (is.null(rs)) rs <- rslst  

    states <- pcheck.states(states, gui=gui, rs=rs)
    if (is.null(states)) {
      states <- pcheck.states(states, gui=TRUE, rs=rs)
      if (is.null(states)) stop("must include states")
    }
    stcdlst <- pcheck.states(states, "VALUE")
  }
  rslst <- unique(FIESTA::ref_statecd[match(states, FIESTA::ref_statecd$MEANING), 
		"RS"])
  rslst[rslst %in% c("NERS", "NCRS")] <- "NRS"
  rslst <- unique(rslst)

  
  ## Get database connection, if datsource=ORACLE, and define SCHEMA for queries
  if (datsource == "ORACLE") {

    if (is.null(dbconn)) {
      ## CONNECT TO ORACLE DATABASE
      dbconn <- DBtestORACLE(dbconnopen=TRUE)
    } else {
      if (!DBI::dbIsValid(dbconn)) stop("the database connection is invalid")
    } 
    if (length(rslst) == 1) {
      FS_FIADB <- pcheck.logical(FS_FIADB, varnm="FS_FIADB", 
		title="FS_FIADB schema?", first="YES", gui=gui, stopifnull=TRUE)
    } else {
      FS_FIADB <- TRUE
    }
 
    ## check dbconnopen
    dbconnopen <- FIESTA::pcheck.logical(dbconnopen, varnm="dbconnopen", 
		title="Keep ODBC open?", first="NO", gui=gui)

    if (!FS_FIADB) {
      SCHEMA <- paste("FS_NIMS_FIADB", rslst, sep="_")
    } else {
      SCHEMA <- "FS_FIADB"
    }
    SCHEMA. <- paste0(SCHEMA, ".")
  } else {
    SCHEMA. <- ""
  }

  ## Define query POP_EVAL, POP_EVAL_TYP table
  popevaltypqry <- paste0("select pev.*, pet.eval_typ from ", SCHEMA., 
		"POP_EVAL_TYP pet join ", SCHEMA., "POP_EVAL pev on (pev.cn = pet.eval_cn) ",
		"where pev.STATECD in ", FIESTA::addcommas(stcdlst, paren=TRUE))

  if (datsource == "ORACLE") {
    ## Get POP_EVAL table with POP_EVAL_TYP
    tryCatch( POP_EVAL <- DBqryORACLE(popevaltypqry, dbconn, dbconnopen=TRUE), 
		error=function(e) stop("pop_eval query is invalid"))

    ## Get SURVEY table
    surveyqry <- paste0("select * from ", SCHEMA., "SURVEY where STATECD in ", 
		FIESTA::addcommas(stcdlst, paren=TRUE))
    tryCatch( SURVEY <- DBqryORACLE(surveyqry, dbconn, dbconnopen=TRUE), 
		error=function(e) stop("survey query is invalid"))

  } else if (datsource == "CSV") {
 
    POP_EVAL <- FIESTA::DBgetCSV("POP_EVAL", stcdlst, ZIP=ZIP)
    POP_EVAL_TYP <- FIESTA::DBgetCSV("POP_EVAL_TYP", stcdlst, ZIP=ZIP)
    SURVEY <- FIESTA::DBgetCSV("SURVEY", stcdlst, ZIP=ZIP, returnDT=TRUE)

    POP_EVAL <- sqldf::sqldf(popevaltypqry)
  }

  ## In POP_EVAL table, Texas has several evaluations based on East, West, Texas
  if ("Texas" %in% states) {
    Texascd <- FIESTA::pcheck.states("Texas", "VALUE")
    POP_EVAL <- POP_EVAL[!POP_EVAL$STATECD == Texascd | POP_EVAL$LOCATION_NM == "Texas", ]
    maxyr <- max(POP_EVAL[POP_EVAL$STATECD == Texascd, "END_INVYR"])
    SURVEY <- SURVEY[!SURVEY$STATECD == Texascd | SURVEY$INVYR <= maxyr,]
  }
 
  ## Check if evalid is valid. If valid, get invyrtab invyrs, evalidlist, and invtype
  if (!is.null(evalid)) {
    ## Check if evalid is valid
    if (!all(evalid %in% POP_EVAL$EVALID)) {
      stop("invalid EVALID")
    } else {
      ## Create table of state, inventory year, and cycle
      invyrqry <- paste0("select STATECD, STATENM, STATEAB, ANN_INVENTORY, INVYR, 
		CYCLE from ", SCHEMA., "SURVEY where STATENM IN(", 
		FIESTA::addcommas(states, quotes=TRUE), ") and invyr <> 9999")
      if (datsource == "CSV") {
        invyrtab <- sqldf::sqldf(invyrqry, stringsAsFactors=FALSE)
      } else {
        invyrtab <- tryCatch( SURVEY <- DBqryORACLE(invyrqry, dbconn, dbconnopen=TRUE), 
		error=function(e) stop("invyr query is invalid"))
      }

      invyrs <- list()
      evalidlist <- list()
      evalTypelist <- list()
      for (i in 1:length(evalid)) { 
        eval <- evalid[[i]]
        st <- substr(eval, nchar(evalid)-5, nchar(evalid)-4)
        etypcd <- substr(eval, nchar(evalid)-1, nchar(evalid))
        state <- FIESTA::pcheck.states(st, "MEANING")
        startyr <- unique(min(POP_EVAL[POP_EVAL$EVALID == eval, c("START_INVYR")]))
        endyr <- unique(max(POP_EVAL[POP_EVAL$EVALID == eval, c("END_INVYR")]))
 
        ann_inventory <- SURVEY[SURVEY$STATECD == st & SURVEY$INVYR == endyr, 
		"ANN_INVENTORY"][[1]]
        invtype <- ifelse(ann_inventory == "Y", "ANNUAL", "PERIODIC")
        stinvyr <- startyr:endyr
        evalTypecd <- unique(POP_EVAL$EVALID[endsWith(as.character(POP_EVAL$EVALID), 
			paste0(substr(endyr, nchar(endyr)-1, nchar(endyr)), etypcd))])
        evalTypelist[[state]] <- POP_EVAL[POP_EVAL$EVALID %in% evalTypecd, "EVAL_TYP"][[1]]

        if (state %in% names(invyrs)) {
          invyrs[[state]] <- sort(unique(c(invyrs[[state]], stinvyr)))
          evalidlist[[state]] <- sort(unique(c(evalidlist[[state]], eval)))
        } else {
          invyrs[[state]] <- stinvyr
          evalidlist[[state]] <- eval
        }
        invyrtab <- invyrtab[invyrtab$ANN_INVENTORY == ann_inventory,]
      }
    }
    return(returnlst <- list(states=states, evalidlist=evalidlist, invtype=invtype,
 		invyrtab=invyrtab, invyrs=invyrs, evalType=evalTypelist, datsource=datsource, 
		FS_FIADB=FS_FIADB, ZIP=ZIP, dbconn=dbconn))
  }
 
  ## Check invyrtab. Data frame with inventory years by state
  if (is.null(invyrtab)) {
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
    ann_inventory <- ifelse(invtype == "PERIODIC", 'N', 'Y')

    ## Create table of state, inventory year, and cycle
    invyrqry <- paste0("select STATECD, STATENM, STATEAB, ANN_INVENTORY, INVYR, CYCLE from ", 
		"SURVEY where STATENM IN(", FIESTA::addcommas(states, quotes=TRUE), 
		") and invyr <> 9999 and ANN_INVENTORY = '", ann_inventory, "'")

    invyrtab <- sqldf::sqldf(invyrqry, stringsAsFactors=FALSE)
    cat("INVENTORY CYCLE BY INVENTORY YEAR", "\n" )
    print(invyrtab)

  } else {
    if (!"INVYR" %in% names(invyrtab)) stop("INVYR must be in invyrtab")
    evalEndyr <- as.list(tapply(invyrtab$INVYR, invyrtab$STATECD, max))
    names(evalEndyr) <- FIESTA::pcheck.states(as.numeric(names(evalEndyr)), 
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

  ## Get possible range of inventory years from invyrtab
  stinvyr.vals <- as.list(by(invyrtab$INVYR, invyrtab$STATECD, range))
  names(stinvyr.vals) <- FIESTA::pcheck.states(names(stinvyr.vals), "MEANING")
  stinvyr.min <- lapply(stinvyr.vals, '[[', 1)
  stinvyr.max <- lapply(stinvyr.vals, '[[', 2)
  invyr.min <- min(unlist(stinvyr.min))
  invyr.max <- max(unlist(stinvyr.max))
 
  if (is.null(evalEndyr)) {
    ## Check evalCur
    ###########################################################
    evalCur <- FIESTA::pcheck.logical(evalCur, varnm="evalCur", 
		title="Most current evaluation?", first="YES", gui=gui)
    if (is.null(evalCur) || !evalCur) {
      if (gui) {
        evalresp <- select.list(c("NO", "YES"), title="Use an Evaluation?", 
		  multiple=FALSE)
        if (evalresp == "") stop("")
        evalresp <- ifelse(evalresp == "YES", TRUE, FALSE)
      } else {
        evalresp <- FALSE
      }
    } else {
      evalresp <- TRUE
    }
  }
  if (!is.null(evalEndyr)) {
    if (class(evalEndyr)[1] != "list") {
      if (length(states) > 1) {
        if (!is.numeric(evalEndyr)) stop("evalEndyr must be numeric")
        if (length(evalEndyr) == 1) {
          if (all(evalEndyr >= stinvyr.min) && all(evalEndyr <= stinvyr.max)) {
            warning("using same evalEndyr for each state: ", unique(evalEndyr))
            evalEndyr <- sapply(states, function(x) {list(evalEndyr)})
          } else {
            stop("check evalEndyr.. not in range in database")
            #evalEndyr <- NULL
          }
        } else if (is.vector(evalEndyr) && length(evalEndyr) == length(stinvyr.max) && 
			identical(names(evalEndyr), names(stinvyr.max))) {
          evalEndyr <- as.list(evalEndyr)
        }
      } else {
        if (class(evalEndyr)[1] %in% c("data.table", "data.frame"))
           evalEndyr <- evalEndyr[[1]]
        if (evalEndyr >= stinvyr.min && evalEndyr <= stinvyr.max) {
          evalEndyr <- list(evalEndyr); names(evalEndyr) <- states
        } else {
          stop("check evalEndyr.. not in range in database")
          #evalEndyr <- NULL
        }
      }
      evalresp <- TRUE
    } else {

      if (is.null(names(evalEndyr))) 
        stop("check evalEndyr.. must be named list")

      if (length(evalEndyr) != length(states)) {
        stop("check evalEndyr.. number of years does not match number of states")
      } else if (!identical(names(evalEndyr), names(stinvyr.vals))) {
        stop("evalEndyr names must match: ", toString(names(stinvyr.vals)))
      } else if (sum(unlist(lapply(evalEndyr, is.numeric))) != length(evalEndyr)) {
        stop("check evalEndyr.. must be numeric")
      }

      if (is.null(names(evalEndyr))) {
        if (length(states) > 1)
          message("evalEndyr is not a named list... using order of states")
        names(evalEndyr) <- states
      }

      for (st in names(evalEndyr)) {
        evalendyr <- evalEndyr[[st]]
        invendyr.min <- stinvyr.min[[st]]
        invendyr.max <- stinvyr.max[[st]]

        if (evalendyr < invendyr.min || evalendyr > invendyr.max) {
          message(paste("check evalEndyr.. outside of range in database:", st))    
          evalendyr <- invendyr.max
        } else {
          evalresp <- TRUE
        }
      }
    }
  }

  ## Get last year of evaluation period and the evaluation type
  if (evalresp) {
    invyrs <- list()
    evalidlist <- sapply(states, function(x) NULL)

    ## Get the evalidation type - areavol or grm)
    evalSelectlst <- c("AREAVOL", "GRM", "DWM")
    if (invtype == "ANNUAL") evalSelectlst <- c("ALL", evalSelectlst)
    evalType <- FIESTA::pcheck.varchar(var2check=evalType, varnm="evalType", gui=gui, 
		checklst=evalSelectlst, caption="Evaluation type", multiple=TRUE, 
		preselect="AREAVOL")
    if (is.null(evalType)) evalType <- "ALL"

    ## check evalType
    if (length(grep("AREAVOL", evalType, ignore.case=TRUE)) > 0) 
      evalType[grep("AREAVOL", evalType, ignore.case=TRUE)] <- "VOL"  
    if (length(grep("GRM", evalType, ignore.case=TRUE)) > 0) 
      evalType[grep("GRM", evalType, ignore.case=TRUE)] <- "GROW"  

    if (isdwm) {
      message("adding dwm to evalType")
      evalType <- c(evalType, "DWM")
    }
    evalTypelist <- sapply(states, function(x) list(evalType))
    evalTypelist <- lapply(evalTypelist, function(x) paste0("EXP", x))

    for (stcd in stcdlst) {
      state <- FIESTA::pcheck.states(stcd, "MEANING")
      stabbr <- FIESTA::pcheck.states(stcd, "ABBR")
      stinvyrs <- unique(stinvyr.vals[[state]])

      POP_EVAL_endyrs <- unique(POP_EVAL[POP_EVAL$STATECD == stcd, "END_INVYR"])
      if (any(is.na(POP_EVAL_endyrs))) 
        POP_EVAL_endyrs <- unique(c(POP_EVAL_endyrs, 
          POP_EVAL[POP_EVAL$STATECD == stcd & is.na(POP_EVAL$END_INVYR), "REPORT_YEAR_NM"]))
      POP_EVAL_endyrs <- sort(POP_EVAL_endyrs)
      POP_EVAL_endyrs <- POP_EVAL_endyrs[POP_EVAL_endyrs %in% invyrtab$INVYR]
      if (!is.null(evalEndyr)) {
        Endyr <- evalEndyr[[state]]

        if (!Endyr %in% POP_EVAL_endyrs) {
          popevalcols <- c("CN", "EVAL_GRP_CN", "EVALID", "EVAL_DESCR", 
			"REPORT_YEAR_NM", "START_INVYR", "END_INVYR", "LAND_ONLY", 
			"TIMBERLAND_ONLY", "GROWTH_ACCT", "ESTN_METHOD")
          message(POP_EVAL[POP_EVAL$STATECD == stcd, popevalcols])
          stop(paste0(Endyr, " is not in ", stabbr, "_", "POP_EVAL table"))

#          msg <- paste(Endyr, "is not in POP_EVAL...")
#          if (max(POP_EVAL_endyrs) < max(stinvyrs)) {
#            Endyr <- max(POP_EVAL_endyrs[POP_EVAL_endyrs > max(stinvyrs)])
#          } else {
#            Endyr <- min(POP_EVAL_endyrs[POP_EVAL_endyrs > max(stinvyrs)])
#          }
#          message(paste(msg, "using", Endyr)) 
        } 
      } else {   ## is.null(evalEndyr)
        if (evalCur) {
          Endyr <- max(POP_EVAL_endyrs)
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
 
      ## Get evalid and inventory years from POP_EVAL table
      pop_endyr <- POP_EVAL$STATECD == stcd & POP_EVAL$END_INVYR == Endyr
      if (all(is.na(pop_endyr[pop_endyr != FALSE])))
        stop("invalid invtype for ", state)
      popevaltab <- POP_EVAL[POP_EVAL$STATECD == stcd & !is.na(POP_EVAL$END_INVYR) & 
		POP_EVAL$END_INVYR == Endyr,]

      if (is.null(invtype)) {
        Startyr <- unique(POP_EVAL[POP_EVAL$STATECD == stcd & POP_EVAL$END_INVYR == Endyr, 
		"START_INVYR"])
        #invtype <- ifelse(is.na(Startyr) || Endyr == Startyr, "PERIODIC", "ANNUAL")
        invtype <- ifelse(Endyr == Startyr, "PERIODIC", "ANNUAL")
      }

      ## Check evalType with evalType in database for state
      evalType.chklst <- unique(popevaltab$EVAL_TYP)

      if (invtype == "ANNUAL") {
        if (!all(evalTypelist[[state]] %in% evalType.chklst)) 
          stop(paste("invalid evalType for", state))

        evalidall <- popevaltab$EVALID[!is.na(popevaltab$EVALID)]
        evalidlist[[state]] <- 
		popevaltab$EVALID[popevaltab$EVAL_TYP %in% evalTypelist[[state]]]
        invyrs[[state]]  <- 
		min(popevaltab$START_INVYR, na.rm=TRUE):max(popevaltab$END_INVYR, na.rm=TRUE)
     
      } else {
        if (!all(evalTypelist[[state]] %in% evalType.chklst)) { 
          evalid.min <- min(popevaltab$EVALID)
          evalTypelist[[state]] <- popevaltab[popevaltab$EVALID == min(popevaltab$EVALID),
			"EVAL_TYP"][1]
          message(paste("invalid evalType for", state, "...using", evalTypelist[[state]]))
        }
        evalidlist[[state]] <- 
		popevaltab$EVALID[popevaltab$EVAL_TYP %in% evalTypelist[[state]]]
        invyrs[[state]]  <- ifelse (any(is.na(popevaltab$END_INVYR)), 
		unique(as.numeric(popevaltab$REPORT_YEAR_NM)),
		min(popevaltab$START_INVYR, na.rm=TRUE):max(popevaltab$END_INVYR, na.rm=TRUE))
      }
    }
  } 
    
  returnlst <- list(states=states, rslst=rslst, evalidlist=evalidlist, 
		invtype=invtype, invyrtab=invyrtab, evalTypelist=evalTypelist)
  if (!is.null(invyrs)) returnlst$invyrs <- invyrs
  returnlst$datsource <- datsource
  
  ## Close database connection
  if (datsource == "ORACLE") {
    if (!dbconnopen) {
      DBI::dbDisconnect(dbconn)
    } else {
      returnlst$dbconn <- dbconn
    }
    returnlst$FS_FIADB <- FS_FIADB
  } else {
    returnlst$ZIP <- ZIP
  }

  return(returnlst)
}
