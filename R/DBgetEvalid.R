DBgetEvalid <- function (states=NULL, RS=NULL, invyrtab=NULL, invtype="ANNUAL", 
	evalCur=TRUE, evalEndyr=NULL, evalid=NULL, evalAll=FALSE, evalType="AREAVOL", 
	isveg=FALSE, isdwm=FALSE, gui=FALSE, returnPOP=TRUE) {

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
  if (gui) evalCur=evalAll=returnPOP=evalType <- NULL

  if (!gui)
    gui <- ifelse(nargs() == 0, TRUE, FALSE)
  
  ## Set global variables
  EVAL_GRP_Endyr=STATECD=START_INVYR=END_INVYR <- NULL


  ## SET OPTIONS
  options.old <- options()
  options(scipen=8) # bias against scientific notation
  on.exit(options(options.old), add=TRUE) 

  ## Define variables
  ZIP <- TRUE
  SCHEMA. <- ""

  ## Set global variables
  EVALID=evalidlist=evalTypelist <- NULL
  invyrs <- NULL
  evalresp <- TRUE


  ##################################################################
  ## CHECK INPUT PARAMETERS
  ##################################################################
  if (!"sqldf" %in% rownames(installed.packages(.Library)))
    stop("the sqldf package is required")
  
  ## Check ZIP
  ZIP <- FIESTA::pcheck.logical(ZIP, varnm="ZIP", title="Zip files?", 
		gui=gui, first="YES")

  ## If evalid is not NULL, get state
  rslst <- c("RMRS","SRS","NCRS","NERS","PNWRS")
  if (!is.null(evalid)) {
    evalid <- unique(unlist(evalid)) 
    stcdlst <- substr(evalid, 1, nchar(evalid)-4)
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
    RS <- FIESTA::pcheck.varchar(var2check=RS, varnm="RS", 
		checklst=rslst, caption="Research Unit?", gui=gui, multiple=TRUE)
    if (is.null(RS)) RS <- rslst  

    states <- pcheck.states(states, gui=gui, RS=RS)
    if (is.null(states)) {
      states <- pcheck.states(states, gui=TRUE, RS=RS)
      if (is.null(states)) stop("must include states")
    }
    stcdlst <- pcheck.states(states, "VALUE")
  }
  rslst <- unique(FIESTA::ref_statecd[match(states, FIESTA::ref_statecd$MEANING), 
		"RS"])
  rslst[rslst %in% c("NERS", "NCRS")] <- "NRS"
  rslst <- unique(rslst)
  

  ## Define query POP_EVAL, POP_EVAL_TYP table
  popevalvars <- c("CN", "EVAL_GRP_CN", "RSCD", "EVALID", "EVAL_DESCR", "STATECD", 
		"START_INVYR", "END_INVYR", "LOCATION_NM")
  popevaltypqry <- paste0("select ", toString(paste0("pev.", popevalvars)), ", 
		pet.eval_typ from ", SCHEMA., "POP_EVAL_TYP pet join ", SCHEMA., 
		"POP_EVAL pev on (pev.cn = pet.eval_cn) ",
		"where pev.STATECD in ", paste0("(", toString(stcdlst), ")"))

############ CSV only

  #########################################################################
  ## Get database tables - POP_EVAL, POP_EVAL_TYPE, SURVEY
  #########################################################################

  POP_EVAL_GRP <- DBgetCSV("POP_EVAL_GRP", stcdlst, ZIP=ZIP, 
		stopifnull=FALSE, returnDT=TRUE)
  POP_EVAL <- FIESTA::DBgetCSV("POP_EVAL", stcdlst, ZIP=ZIP, stopifnull=FALSE,
		returnDT=TRUE)
  if (nrow(POP_EVAL) == 0) {
    message("no data in database for ", toString(states))
    return(NULL)
  } else {
    if (!all(stcdlst %in% unique(POP_EVAL$STATECD))) {
      miss <- stcdlst[!stcdlst %in% unique(POP_EVAL$STATECD)]
      message("no data in database for ", toString(miss))
      stcdlst <- stcdlst[!stcdlst %in% miss]
      states <- pcheck.states(stcdlst, "MEANING")
    }
  }    

  POP_EVAL_TYP <- DBgetCSV("POP_EVAL_TYP", stcdlst, ZIP=ZIP, stopifnull=FALSE)
  if (nrow(POP_EVAL_TYP) == 0) return(NULL)

  SURVEY <- DBgetCSV("SURVEY", stcdlst, ZIP=ZIP, returnDT=TRUE, stopifnull=FALSE)
  if (nrow(SURVEY) == 0) return(NULL)

  POP_EVAL <- setDT(sqldf::sqldf(popevaltypqry))

  ## Add a parsed EVAL_GRP endyr to POP_EVAL_GRP
  POP_EVAL_GRP[, EVAL_GRP_Endyr := as.numeric(substr(POP_EVAL_GRP$EVAL_GRP, 
		nchar(POP_EVAL_GRP$EVAL_GRP) - 3, nchar(POP_EVAL_GRP$EVAL_GRP)))]


############ End CSV only

  ## In POP_EVAL table, Texas has several evaluations based on East, West, Texas

  ## Check if evalid is valid. If valid, get invyrtab invyrs, evalidlist, and invtype
  if (!is.null(evalid)) {
    ## Check if evalid is valid
    if (!all(evalid %in% POP_EVAL$EVALID)) {
      notin <- evalid[!evalid %in% POP_EVAL$EVALID]
      stop("invalid EVALID: ", toString(notin))
    } else {
      ## Create table of state, inventory year, and cycle
      invyrqry <- paste0("select STATECD, STATENM, STATEAB, ANN_INVENTORY, INVYR, 
		CYCLE from ", SCHEMA., "SURVEY where STATENM IN(", 
		toString(paste0("'", states, "'")), ") and invyr <> 9999")

############ CSV only
      invyrtab <- sqldf::sqldf(invyrqry, stringsAsFactors=FALSE)

############ End CSV only

      invyrs <- list()
      evalidlist <- list()
      evalTypelist <- list()
      for (i in 1:length(evalid)) { 
        eval <- evalid[[i]]
        st <- substr(eval, 1, nchar(eval)-4)
        etypcd <- substr(eval, nchar(eval)-1, nchar(eval))
        state <- FIESTA::pcheck.states(st, "MEANING")
        pop_eval <- POP_EVAL[POP_EVAL$EVALID == eval,]
        startyr <- unique(min(pop_eval$START_INVYR))
        endyr <- unique(min(pop_eval$END_INVYR))
        ann_inventory <- SURVEY[SURVEY$STATECD == st & SURVEY$INVYR == endyr, 
		"ANN_INVENTORY"][[1]]
        invtype <- ifelse(ann_inventory == "Y", "ANNUAL", "PERIODIC")
        stinvyr <- startyr:endyr
        evalTypelist[[state]] <- unique(pop_eval$EVAL_TYP)[1]
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
    return(returnlst <- list(states=states, rslst=rslst, evalidlist=evalidlist, 
		invtype=invtype, invyrtab=invyrtab, invyrs=invyrs, evalType=evalTypelist))
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

    ## Create table of state, inventory year
    invyrqry <- paste0("select distinct STATECD, STATENM, STATEAB, ANN_INVENTORY, 
		INVYR from ", "SURVEY where STATENM IN(", 
		toString(paste0("'", states, "'")), 
		") and invyr <> 9999 and P3_OZONE_IND = 'N' and ANN_INVENTORY = '", 
		ann_inventory, "'")
    invyrtab <- sqldf::sqldf(invyrqry, stringsAsFactors=FALSE)
    cat("Inventory years by state...", "\n" )
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

  if (!all(states %in% names(stinvyr.vals))) {
    missnames <- states[!states %in% names(stinvyr.vals)]
    misscodes <- FIESTA::pcheck.states(missnames, "VALUE")
    message("there is no data in the database for: ", toString(missnames))
    stcdlst <- stcdlst[!stcdlst %in% misscodes]
    states <- states[!states %in% missnames]
  }
 
  if (is.null(evalEndyr)) {
    ## Check evalAll
    ###########################################################
    evalAll <- FIESTA::pcheck.logical(evalAll, varnm="evalAll", 
		title="All evaluations?", first="YES", gui=gui)

    if (is.null(evalAll) || !evalAll) {
      ## Check evalCur
      evalCur <- FIESTA::pcheck.logical(evalCur, varnm="evalCur", 
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
        return(list(states=states, rslst=rslst, evalidlist=NULL, 
			invtype=invtype, invyrtab=invyrtab, SURVEY=SURVEY))

        return(returnlst <- list(states=states, rslst=rslst, evalidlist=NULL, 
		invtype=invtype, invyrtab=invyrtab, evalType=evalTypelist))
      }
    }
  }
  returnPOP <- FIESTA::pcheck.logical(returnPOP, varnm="returnPOP", 
		title="Return POP tables?", first="YES", gui=gui)

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
    evalSelectlst <- c("ALL", "AREAVOL", "GRM", "P2VEG", "DWM")
    evalType <- FIESTA::pcheck.varchar(var2check=evalType, varnm="evalType", gui=gui, 
		checklst=evalSelectlst, caption="Evaluation type", multiple=TRUE, 
		preselect="AREAVOL")
    if (is.null(evalType)) evalType <- "AREAVOL"

    ## check evalType
    if (invtype == "PERIODIC" && evalType == "ALL") {
      evalType <- "CURR"
    } else {
      if (length(grep("AREAVOL", evalType, ignore.case=TRUE)) > 0) 
        evalType[grep("AREAVOL", evalType, ignore.case=TRUE)] <- "VOL"  
      if (length(grep("GRM", evalType, ignore.case=TRUE)) > 0) 
        evalType[grep("GRM", evalType, ignore.case=TRUE)] <- "GROW"  
    }

    if (isdwm) {
      message("adding dwm to evalType")
      evalType <- c(evalType, "DWM")
    }
    if (isveg) {
      message("adding P2VEG to evalType")
      evalType <- c(evalType, "P2VEG")
    }

    evalTypelist <- sapply(states, function(x) list(unique(evalType)))
    evalTypelist <- lapply(evalTypelist, function(x) paste0("EXP", x))

    for (stcd in stcdlst) {
      state <- FIESTA::pcheck.states(stcd, "MEANING")
      stabbr <- FIESTA::pcheck.states(stcd, "ABBR")
      stinvyrs <- unique(stinvyr.vals[[state]])
      invtype.invyrs <- invyrtab[invyrtab$STATECD == stcd, "INVYR"]

      ## In POP_EVAL table, Texas has several evaluations based on East, West, Texas
      ## Remove East and West in LOCATION_NM and EVAL_DESCR
      if (stcd == 48) {
        POP_EVAL_GRPstcd <- POP_EVAL_GRP[STATECD == stcd & 
		!grepl("EAST", POP_EVAL_GRP$EVAL_GRP_DESCR, 
		ignore.case=TRUE) &
		!grepl("WEST", POP_EVAL_GRP$EVAL_GRP_DESCR, ignore.case=TRUE), ]
      } else {
        POP_EVAL_GRPstcd <- POP_EVAL_GRP[STATECD == stcd,]
      }

    
      ## Get evalid and inventory years from POP_EVAL table
      setkey(POP_EVAL, "EVAL_GRP_CN")
      setkey(POP_EVAL_GRPstcd, "CN")

      ## Subset POP_EVAL/POP_EVAL_GRP by state and inventory type
      popevaltab <- POP_EVAL[POP_EVAL_GRPstcd[, c("CN", "EVAL_GRP_Endyr")]]
      popevaltab <- popevaltab[popevaltab$END_INVYR %in% invtype.invyrs,]

      POP_EVAL_endyrs <- na.omit(unique(popevaltab[, "EVAL_GRP_Endyr"][[1]]))
      if (!is.null(evalEndyr)) {
        Endyr <- evalEndyr[[state]]

        if (!Endyr %in% POP_EVAL_endyrs) 
          stop(paste0(Endyr, " data are not in ", stabbr, "_", "POP_EVAL table"))
         
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

      ## Subset popevaltab by Endyr
      popevaltab <- popevaltab[EVAL_GRP_Endyr %in% Endyr,]

      ## Check evalType with evalType in database for state
      evalType.chklst <- unique(popevaltab$EVAL_TYP)

      if (invtype == "ANNUAL") {
        if (!all(evalTypelist[[state]] %in% evalType.chklst)) 
          stop(paste("invalid evalType for", state))
        evalidall <- popevaltab$EVALID[!is.na(popevaltab$EVALID)]
        evalidlist[[state]] <- 
		sort(popevaltab$EVALID[popevaltab$EVAL_TYP %in% evalTypelist[[state]]])
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

  if (returnPOP) {
    returnlst$SURVEY <- SURVEY
    returnlst$POP_EVAL <- POP_EVAL
  }

  return(returnlst)
}
