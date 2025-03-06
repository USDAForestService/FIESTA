check.estdata <- 
  function(esttype,
           popType, 
           popdatindb, popconn = NULL, pop_schema = pop_schema,
           pltcondx, totals,
           pltcondflds,
           pop_fmt = NULL, pop_dsn = NULL, 
           sumunits = FALSE, 
           landarea = NULL, landarea_both = TRUE, 
           ACI = NULL, pcfilter = NULL, 
           T1filter = NULL, T2filter = NULL,
	         allin1 = FALSE, divideby = NULL, 
           estround = 6, pseround = 3, 
	         returntitle = TRUE, 
           rawonly = FALSE, 
	         savedata = FALSE, 
           savedata_opts, 
           gui = FALSE){

  #############################################################################
  ## DESCRIPTION: Checks data inputs
  ## Apply plot filter
  ## - pcfilter (e.g., COUNTY == 3, FORTYPCD == 122)
  ## Check landarea ("FOREST", "ALL", "TIMBERLAND") and create landarea.filter
  ## - if landarea = FOREST, "COND_STATUS_CD == 1"
  ## - if landarea = TIMBERLAND, "SITECLCD %in% c(1:6) & RESERVCD == 0"
  ## Apply condition filters
  ## - landarea.filter
  ## - ACI.filter (e.g., if, ACI=FALSE, COND_STATUS_CD == 1)
  ## Check output parameters
  ## - divideby, divides final estimates by (hundred, thousand, million)
  ## - if sumunits=TRUE, aggregates all estimation units
  ## - If allin1=TRUE, puts estimate (% sample error) in each cell
  ## - If savedata=TRUE, saves tables to outfolder
  ## - If addtitle=TRUE, adds title(s) to saved tables
  ## - If returntitle, saves title(s) of tables
  ## - If rawdata=TRUE, generates and saves rawdata variables for estimates
  ## - If savedata, checks output folder
  ## - If rawdata, adds a folder named 'rawdata' to outfolder to add raw data
  ## - estround - round estimate values
  ## - pseround - round percent standard error values
  #############################################################################

  ## Set global variables
  rawfolder <- NULL
  rawdata <- TRUE
  SCHEMA. <- ""


  #############################################################################
  ## Check esttype
  #############################################################################
  esttypelst <- c("AREA", "TREE", "RATIO", "SEED", "LULC")
  esttype <- pcheck.varchar(var2check=esttype, varnm="esttype", gui=gui,
	checklst=esttypelst, caption="Esttype?")

  ## Check totals
  if (esttype %in% c("AREA", "TREE")) {
    totals <- pcheck.logical(totals, varnm="totals",
		         title="Totals?", first="NO", gui=gui, stopifnull=TRUE)
  }   
  
  ## Check pop_fmt and pop_dsn
  ###############################################
  if (popdatindb) {
    if (!is.null(popconn)) {
      if (!DBI::dbIsValid(popconn)) {
        message("database connection is invalid... ")
        message(popconn)
        stop()
      }
    } else if (!is.null(pop_dsn)) {
      if (is.null(pop_fmt) || pop_fmt == "") {
        #pop_fmtlst <- c('sqlite', 'sqlite3', 'db', 'db3', 'gpkg', 'csv', 'shp')
        message("pop_fmt is invalid... checking pop_dsn extension")
        pop_ext <- getext(pop_dsn)
        if (pop_ext %in% c("db", "db3", "sqlite")) {
          pop_fmt == "sqlite"
        } else { 
          stop("fmt not available")
        }
      }
      if (pop_fmt == "sqlite") {
        popconn <- DBtestSQLite(pop_dsn, dbconnopen = TRUE, 
                           createnew = FALSE, returnpath = FALSE)
      }
      if (is.null(popconn)) {
        stop("invalid database")
      } 
    }
    if (!is.null(pop_schema)) {
      SCHEMA. <- paste0(pop_schema, ".")
    }
  }
  
  # ## Get table fields - this will get any new fields added to pltcondx
  # ###########################################################################
  if (!popdatindb) {
    pltcondflds <- names(pltcondx)
  }

  ## Build where statement with plot/condition filters
  ###########################################################################
  where.qry <- {}
  

  ## Check pcfilter (plot and cond filters) and add to where.qry
  ###########################################################################
  pcfilter <- check.logic(pltcondflds, pcfilter, syntax="SQL", filternm="pcfilter")
  if (!is.null(pcfilter)) {
    
    ## Check to make sure variable in pcfilter are in pltcondflds
    pcfiltervars  <- unlist(lapply(pcfilter, 
        function(x) pltcondflds[sapply(pltcondflds, function(y) grepl(y, x))]))
    if (length(pcfiltervars) == 0) {
      stop("pcfilter is invalid: ", pcfilter)
    }
    pcfiltertmp <- pcfilter
    for (pcfiltervar in pcfiltervars) {
      pcfiltertmp <- gsub(pcfiltervar, paste0("pc.", pcfiltervar), pcfiltertmp)
    }
    
    pcfiltertmp <- RtoSQL(pcfiltertmp)
    where.qry <- paste0("\nWHERE ", pcfiltertmp)
  }
  
  
  ## Check T1filter (plot and cond filters) and add to where.qry
  ###########################################################################
  T1filter <- check.logic(pltcondflds, T1filter, syntax="SQL", filternm="T1filter")
  if (!is.null(T1filter)) {
    
    ## Check to make sure variable in pcfilter are in pltcondflds
    T1filtervars  <- unlist(lapply(T1filter, 
            function(x) pltcondflds[sapply(pltcondflds, function(y) grepl(y, x))]))
    if (length(T1filtervars) == 0) {
      stop("T1filter is invalid: ", T1filter)
    }
    T1filtertmp <- T1filter
    for (T1filtervar in T1filtervars) {
      T1filtertmp <- gsub(T1filtervar, paste0("pc.", T1filtervar), T1filtertmp)
    }
    
    T1filtertmp <- RtoSQL(T1filtertmp)
    if (is.null(where.qry)) {
      where.qry <- paste0("\nWHERE ", T1filtertmp)
    } else {
      where.qry <- paste0(where.qry, " AND ", T1filtertmp)
    }
  }
  
  
  ## Check T1filter (plot and cond filters) and add to where.qry
  ###########################################################################
  T2filter <- check.logic(pltcondflds, T2filter, syntax="SQL", filternm="T2filter")
  if (!is.null(T2filter)) {
    
    ## Check to make sure variable in pcfilter are in pltcondflds
    T2filtervars  <- unlist(lapply(T2filter, 
            function(x) pltcondflds[sapply(pltcondflds, function(y) grepl(y, x))]))
    if (length(T2filtervars) == 0) {
      stop("T2filter is invalid: ", T2filter)
    }
    T2filtertmp <- T2filter
    for (T2filtervar in T2filtervars) {
      T2filtertmp <- gsub(T2filtervar, paste0("ppc.", T2filtervar), T2filtertmp)
    }
    
    T2filtertmp <- RtoSQL(T2filtertmp)
    if (is.null(where.qry)) {
      where.qry <- paste0("\nWHERE ", T2filtertmp)
    } else {
      where.qry <- paste0(where.qry, " AND ", T2filtertmp)
    }
  }
  
  

  ## Check landarea and add to where.qry
  #############################################################################
  landarealst <- c("FOREST", "ALL", "TIMBERLAND")
  landarea <- pcheck.varchar(var2check=landarea, varnm="landarea", gui=gui,
	                  checklst=landarealst, caption="Sample land area?")

  ## Create landarea.filter
  landarea.filter <- NULL
  landcols <- {}
  if (landarea == "ALL") {
    if ("COND_STATUS_CD" %in% pltcondflds) {
      landarea.filter <- "pc.COND_STATUS_CD <> 5"
      # if (popType %in% c("CHNG", "GRM")) {
      #   landarea.filterCHNG <- gsub("pc.", "ppc.", landarea.filter)
      #   landarea.filter <- paste0(landarea.filter, " AND ", landarea.filterCHNG)
      # }
      landcols <- "COND_STATUS_CD"
    } else {
      message("COND_STATUS_CD not in pltcond")
      return(NULL)
    }
    
  } else if (landarea == "FOREST") {
    if ("COND_STATUS_CD" %in% pltcondflds) {
      landarea.filter <- "pc.COND_STATUS_CD = 1"
      # if (popType %in% c("CHNG", "GRM")) {
      #   landarea.filterCHNG <- gsub("pc.", "ppc.", landarea.filter)
      #   landarea.filter <- paste0(landarea.filter, " AND ", landarea.filterCHNG)
      # }
      landcols <- "COND_STATUS_CD"
    } else {
	    message("COND_STATUS_CD not in pltcond")
		  return(NULL)
	  }
  } else if (landarea == "TIMBERLAND") {
    landcols <- c("SITECLCD", "RESERVCD")
    if (any(!landcols %in% pltcondflds)) {
      landcols.miss <- landcols[which(!landcols %in% pltcondflds)]
      stop(paste("missing variables for TIMBERLAND landarea filter:",
		                paste(landcols.miss, collapse=", ")))
    }
    landarea.filter <- "pc.SITECLCD IN(1,2,3,4,5,6) AND pc.RESERVCD = 0"
    # if (popType %in% c("CHNG", "GRM")) {
    #   landarea.filterCHNG <- gsub("pc.", "ppc.", landarea.filter)
    #   landarea.filter <- paste0(landarea.filter, " AND ", landarea.filterCHNG)
    # }
  }
    
  ## Check for missing landcols
  if (length(landcols) > 0) {
    landcolsmiss <- landcols[which(!landcols %in% pltcondflds)]
    if (length(landcolsmiss) > 0) {
      message("missing variables: ", paste(landcolsmiss, collapse=", "))
		  return(NULL)
    }
  }

  ## Add landarea filter to where.qry
  if (!is.null(landarea.filter)) {
    if (popType == "CHNG" && landarea_both) {
      landarea.filterCHNG <- gsub("pc.", "pc.PREV_", landarea.filter)
      landarea.filter <- paste0("(", landarea.filter, 
                       "\n  AND ", landarea.filterCHNG, ")")
    }
    if (!is.null(where.qry)) {
      where.qry <- paste0(where.qry,
                        "\n  AND ", landarea.filter)
    } else {
      where.qry <- paste0("\nWHERE ", landarea.filter)
    }
  }

  ## Add ACI.filter to where.qry
  ###################################################################################
  if (esttype %in% c("TREE", "RATIO") && !ACI && landarea != "FOREST") {
    ACI.filter <- "pc.COND_STATUS_CD = 1"
    if (!is.null(where.qry)) {
      where.qry <- paste0(where.qry,
                          "\n  AND ", ACI.filter)
    } else {
      where.qry <- paste0("\nWHERE ", ACI.filter)
    }
  }
  

  ## Check sumunits
  ########################################################
  sumunits <- pcheck.logical(sumunits, varnm="sumunits",
		title="Sum estimation units?", first="YES", gui=gui, stopifnull=TRUE)

  ## Check rawonly
  rawonly <- pcheck.logical(rawonly, varnm="rawonly", title="Raw data only?",
		first="NO", gui=gui, stopifnull=TRUE)
  if (rawonly) rawdata <- TRUE


  ## Check divideby
  dividebylst <- c("hundred", "thousand", "million")
  if (!is.null(divideby) || gui) {
    divideby <- pcheck.varchar(var2check=divideby, varnm="divideby",
		gui=gui, checklst=dividebylst, caption="Divide estimates?")
  }

  ## Check allin1
  allin1 <- pcheck.logical(allin1, varnm="allin1",
		title="All 1 table - Est (%error)?", first="NO", gui=gui)

  ## Check returntitle
  returntitle <- pcheck.logical(returntitle, varnm="returntitle",
		title="Save output titles?", first="YES", gui=gui, stopifnull=TRUE)

  ## Check savedata
  savedata <- pcheck.logical(savedata, varnm="savedata",
		title="Save data tables?", first="YES", gui=gui, stopifnull=TRUE)

  ## Define objects
  for (i in 1:length(savedata_opts)) {
    assign(names(savedata_opts)[[i]], savedata_opts[[i]])
  }
  
  ## Check output info
  ########################################################
  if (savedata) {

    if (!rawonly) {
      outlst <- pcheck.output(savedata_opts = savedata_opts,
                              gui=gui)
      outfolder <- outlst$outfolder
      overwrite_layer <- outlst$overwrite_layer
      outfn.pre <- outfn.pre
    }
    if (rawdata) {
      ## Check raw_fmt
      raw_fmtlst <- c('sqlite', 'sqlite3', 'db', 'db3', 'gpkg', 'csv', 'gdb', 'shp')
      raw_fmt <- pcheck.varchar(raw_fmt, varnm="raw_fmt", checklst=raw_fmtlst,
                                  caption="Out raw format", gui=gui)

      if (!is.null(raw_fmt) && raw_fmt == "csv") {
        rawfolder <- paste(outfolder, "rawdata", sep="/")
        if (!file.exists(rawfolder)) dir.create(rawfolder)
      } else {
        if (is.null(raw_dsn)) {
          raw_dsn <- "rawdata"
        }
        outlst <- pcheck.output(savedata_opts = savedata_opts, 
                                gui=gui)
        rawfolder <- outlst$outfolder
        raw_fmt <- outlst$out_fmt
        raw_dsn <- outlst$out_dsn
        overwrite_layer <- outlst$overwrite_layer
      }
    }
  }
  
  ## Check rounding variables
  if (is.null(estround)) {
    estround <- ifelse(allin1, 0, 6)
  } else {
    if (!is.numeric(estround)) stop("estround must be a numeric")
    if (estround > 16) {
      estround <- ifelse(allin1, 0, 6)
      message("check estround... very high number, setting to ", estround)
    }
  }
  if (is.null(pseround)) {
    pseround <- ifelse(allin1, 0, 6)
  } else {
    if (!is.numeric(pseround)) stop("pseround must be a numeric")
    if (pseround > 16) {
      pseround <- ifelse(allin1, 0, 6)
      warning("check pseround... very high number, setting to ", pseround)
    }
  }

  ## Set up list of variables to return
  ######################################################################################
  returnlst <- list(esttype = esttype, 
                    sumunits = sumunits,
                    totals = totals,
                    pltcondflds = pltcondflds,
                    landarea = landarea, 
                    allin1 = allin1, 
                    divideby = divideby,
                    estround = estround, pseround = pseround, 
                    addtitle = addtitle, 
                    returntitle = returntitle,
                    rawonly = rawonly, 
                    savedata = savedata,
                    outfolder = outfolder, 
                    overwrite_layer = overwrite_layer, 
                    outfn.pre = outfn.pre,
                    outfn.date = outfn.date,
                    append_layer = append_layer,
                    rawfolder = rawfolder, 
                    raw_fmt = raw_fmt, 
                    raw_dsn = raw_dsn, 
                    landarea.filter = landarea.filter,
                    where.qry = where.qry)
  if (popdatindb) {
    returnlst$popconn <- popconn
    returnlst$SCHEMA. <- SCHEMA.
  }

  return(returnlst)
}
