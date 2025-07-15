check.estdata <- 
  function(esttype,
           popType, 
           popdatindb, popconn = NULL, pop_schema = pop_schema,
           pltcondx, totals,
           pltflds, condflds,
           dbqueriesWITH = NULL, dbqueries = NULL,
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
  if (popdatindb) {
    pltidsWITHqry <- dbqueriesWITH$pltidsWITH
    pltidsadjWITHqry <- dbqueriesWITH$pltidsadjWITH
    pltcondxqry <- dbqueries$pltcondx
    pltcondflds <- c(pltflds, condflds)
  } else {
    pltcondflds <- names(pltcondx)
    pltidsadjWITHqry=pltidsWITH <- NULL
  }

  ## Build where statement with plot/condition filters
  ###########################################################################
  pcwhereqry <- {}
  

  ## Check pcfilter (plot and cond filters) and add to pcwhereqry
  ###########################################################################
  if (!is.null(pcfilter)) {
    pcfilter <- check.pcfilter(pcfilter, pltflds = pltflds, condflds = condflds)
    pcwhereqry <- paste0("\nWHERE ", pcfilter)
  }

  
  ## Check T1filter (plot and cond filters) and add to pcwhereqry
  ###########################################################################
  if (!is.null(T1filter)) {
    T1filter <- check.pcfilter(T1filter, pltflds = pltflds, condflds = condflds)
    
    if (is.null(pcwhereqry)) {
      pcwhereqry <- paste0("\n WHERE ", T1filter)
    } else {
      pcwhereqry <- paste0(pcwhereqry, " AND ", T1filter)
    }
  }
  
  ## Check T1filter (plot and cond filters) and add to pcwhereqry
  ###########################################################################
  if (!is.null(T2filter)) {
    T2filter <- check.pcfilter(T2filter, pltflds = pltflds, condflds = condflds)
    
    if (is.null(pcwhereqry)) {
      pcwhereqry <- paste0("\n WHERE ", T2filter)
    } else {
      pcwhereqry <- paste0(pcwhereqry, " AND ", T2filter)
    }
  }
  

  ## Check landarea and add to pcwhereqry
  #############################################################################
  landarealst <- c("FOREST", "ALL", "TIMBERLAND")
  landarea <- pcheck.varchar(var2check=landarea, varnm="landarea", gui=gui,
	                  checklst=landarealst, caption="Sample land area?")

  ## Create landarea.filter
  landarea.filter <- NULL
  landcols <- {}
  if (landarea == "ALL") {
    if ("COND_STATUS_CD" %in% condflds) {
      landarea.filter <- "c.COND_STATUS_CD <> 5"
      landcols <- "COND_STATUS_CD"
    } else {
      message("COND_STATUS_CD not in pltcond")
      return(NULL)
    }
    
  } else if (landarea %in% "FOREST") {
    if ("COND_STATUS_CD" %in% condflds) {
      landarea.filter <- "c.COND_STATUS_CD = 1"
      landcols <- "COND_STATUS_CD"
    } else {
	     message("COND_STATUS_CD not in pltcond")
		  return(NULL)
    }
  } else if (landarea == "TIMBERLAND") {
    landcols <- c("SITECLCD", "RESERVCD")
    if (any(!landcols %in% condflds)) {
      landcols.miss <- landcols[which(!landcols %in% condflds)]
      stop(paste("missing variables for TIMBERLAND landarea filter:",
		                paste(landcols.miss, collapse=", ")))
    }
    landarea.filter <- paste0("c.SITECLCD IN(1,2,3,4,5,6) AND c.RESERVCD = 0")
  }
    
  ## Check for missing landcols
  if (length(landcols) > 0) {
    landcolsmiss <- landcols[which(!landcols %in% condflds)]
    if (length(landcolsmiss) > 0) {
      message("missing variables: ", paste(landcolsmiss, collapse=", "))
		  return(NULL)
    }
  }

  ## Add landarea filter to pcwhereqry
  if (!is.null(landarea.filter)) {
    if (popType == "CHNG" && landarea_both) {
      landarea.filterCHNG <- gsub("c.", "pcond.", landarea.filter)
      landarea.filter <- paste0("(", landarea.filter, 
                       "\n  AND ", landarea.filterCHNG, ")")
    }
    if (!is.null(pcwhereqry)) {
      pcwhereqry <- paste0(pcwhereqry,
                        "\n  AND ", landarea.filter)
    } else {
      pcwhereqry <- paste0("\n WHERE ", landarea.filter)
    }
  }

  ## Add ACI.filter to pcwhereqry
  ###################################################################################
  if (esttype %in% c("TREE", "RATIO") && !ACI && landarea != "FOREST") {
    ACI.filter <- "c.COND_STATUS_CD = 1"
    if (!is.null(pcwhereqry)) {
      pcwhereqry <- paste0(pcwhereqry,
                          "\n  AND ", ACI.filter)
    } else {
      pcwhereqry <- paste0("\n WHERE ", ACI.filter)
    }
  }
  
  
  ## Update pltcondx  or pltcondxqry with pcwhereqry
  ########################################################
  pltcondxadjWITHqry=pltcondxWITHqry <- NULL
  if (!is.null(pcwhereqry)) {
    if (!popdatindb && is.data.frame(pltcondx)) {
      pcwhereqry <- gsub("p.", "pc.", pcwhereqry)
      pcwhereqry <- gsub("c.", "pc.", pcwhereqry)
    
      pcqry <- paste0("SELECT * FROM pltcondx pc",
                      pcwhereqry)
      pcchk <- tryCatch(sqldf::sqldf(pcqry),
                        error=function(e) {
                          warning(e)
                          return(NULL)})
      if (is.null(pcchk) || nrow(pcchk) == 1) {
        stop("invalid filter: \n", pcwhereqry)
      } else {
        pltcondx <- pcchk
      }
    } else {
      pltcondx.qry <- paste0(pltcondxqry,
                            pcwhereqry)
      
      ## Build WITH query for pltcondx, including pltids WITH query
      pltcondxWITHqry <- paste0(pltidsWITHqry, ", ",
                                "\n----- pltcondx",
                                "\npltcondx AS",
                                "\n(", pltcondx.qry, ")")
      #dbqueriesWITH$pltcondxWITH <- pltcondxWITHqry

      ## Build WITH query for pltcondx, including pltidsadj WITH query
      pltcondxadjWITHqry <- paste0(pltidsadjWITHqry, ", ",
                                 "\n----- pltcondx",
                                 "\npltcondx AS",
                                 "\n(", pltcondx.qry, ")")
      #dbqueriesWITH$pltcondxadjWITH <- pltcondxadjWITHqry
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
                    pltcondxWITHqry = pltcondxWITHqry,
                    pltcondxadjWITHqry = pltcondxadjWITHqry,
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
                    landarea.filter = landarea.filter)
  if (popdatindb) {
    returnlst$popconn <- popconn
    returnlst$SCHEMA. <- SCHEMA.
  }

  return(returnlst)
}
