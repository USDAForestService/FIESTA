check.estdata <- function(esttype, datindb=FALSE, 
    pop_dsn=NULL, pop_fmt=NULL, pop_schema=pop_schema, totals=TRUE, 
    pltcondf=NULL, cuniqueid="PLT_CN", condid="CONDID", treex=NULL, seedx=NULL, 
	  vcondx=NULL, tuniqueid="PLT_CN", vuniqueid="PLT_CN", sumunits=FALSE, 
	  estseed="none", woodland="Y", landarea=NULL, ACI.filter=NULL, pcfilter=NULL, 
	  tfilter=NULL, TPA=TRUE, tpavar="TPA_UNADJ", allin1=FALSE, estround=6, pseround=3, 
	  divideby=NULL, addtitle=TRUE, returntitle=TRUE, rawdata=FALSE, rawonly=FALSE, 
	  savedata=FALSE, outfolder=NULL, overwrite_dsn=FALSE, overwrite_layer=TRUE, 
	  outfn.pre=NULL, outfn.date=TRUE, append_layer=FALSE, raw_fmt="csv", 
	  raw_dsn=NULL, gui=FALSE){

  ###################################################################################
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
  ###################################################################################

  ## Set global variables
  rawfolder <- NULL
  isdb <- FALSE
  whereqry <- NULL

  #############################################################################
  ## Check esttype
  #############################################################################
  esttypelst <- c("AREA", "TREE", "RATIO", "SEED", "LULC", "P2VEG", "DWM")
  esttype <- pcheck.varchar(var2check=esttype, varnm="esttype", gui=gui,
	checklst=esttypelst, caption="Esttype?")

  ## Check totals
  if (esttype %in% c("AREA", "TREE")) {
    totals <- pcheck.logical(totals, varnm="totals",
		title="Totals?", first="NO", gui=gui, stopifnull=TRUE)
  }      

  ## Check pop_fmt and pop_dsn
  ###############################################
  if (!is.null(pop_dsn)) {
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
    dbconn <- DBtestSQLite(pop_dsn, dbconnopen = TRUE, 
                         createnew = FALSE, returnpath = FALSE)
  }
	if (is.null(dbconn)) {
      stop("invalid database")
    } else {
      isdb <- TRUE
    }
    tablst <- DBI::dbListTables(dbconn)
  }

  ## Check pltcondf
  ###########################################################################
  pltcondf <- pcheck.table(pltcondf, conn = dbconn, stopifnull = TRUE, 
                       stopifinvalid = TRUE)
  setkeyv(pltcondf, c(cuniqueid, condid))
  
  ###########################################################################
  ## Apply pcfilter (plot and cond filters) to pltcondf table
  ###########################################################################
  if (!is.null(pcfilter)) {
    whereqry <- paste0("\nWHERE ", RtoSQL(pcfilter))
  }
  pltcondnmlst <- names(pltcondf)
  pltcondf <- datFilter(x = pltcondf, 
                        xfilter = pcfilter, 
						            title.filter = "plt filter?",
						            gui = gui, 
						            filternm = "pcfilter", 
						            xnm = "pltcondf")$xf
  if (is.null(pltcondf)) {
    message(paste(pcfilter, "removed all records"))
    return(NULL)
  }

  #############################################################################
  ## Check landarea
  #############################################################################
#  if (esttype == "RATIO") {
#    landarealst <- c("FOREST", "TIMBERLAND")
#  } else {
    landarealst <- c("FOREST", "ALL", "TIMBERLAND")
#  }
  if (esttype == "LULC") {
    landarealst <- c(landarealst, "CHANGE")
  }
  landarea <- pcheck.varchar(var2check=landarea, varnm="landarea", gui=gui,
	checklst=landarealst, caption="Sample land area?")


  ## Create landarea.filter
  #############################################################################
  landarea.filter <- NULL
  landcols <- {}
  if (landarea != "ALL") {
    if (landarea == "FOREST") {
      if ("COND_STATUS_CD" %in% names(pltcondf)) {
        landarea.filter <- "COND_STATUS_CD == 1"
        landcols <- "COND_STATUS_CD"
      }
    } else if (landarea == "TIMBERLAND") {
      landcols <- c("SITECLCD", "RESERVCD")
      if (any(!landcols %in% pltcondnmlst)) {
        landcols.miss <- landcols[which(!landcols %in% pltcondnmlst)]
        stop(paste("missing variables for TIMBERLAND landarea filter:",
		               paste(landcols.miss, collapse=", ")))
      }
      landarea.filter <- "SITECLCD %in% c(1:6) & RESERVCD == 0"
    }
    ## Check for missing landcols
    if (length(landcols) > 0) {
      landcolsmiss <- landcols[which(!landcols %in% pltcondnmlst)]
      if (length(landcolsmiss) > 0) {
        stop("missing variables: ", paste(landcolsmiss, collapse=", "))
      }
    }
  }
  if (!is.null(landarea.filter)) {
    if (!is.null(whereqry)) {
      whereqry <- paste0(whereqry, " AND ", RtoSQL(landarea.filter))
	  } else {
	    whereqry <- paste0("\nWHERE ", RtoSQL(landarea.filter))
    }	  
  }

  ###################################################################################
  ## Apply landarea filters
  ###################################################################################

  ## Apply landarea.filter to pltcondf
  pltcondf <- datFilter(x=pltcondf, xfilter=landarea.filter,
		  title.filter="landarea filter", gui=gui, stopifnull=FALSE)$xf
  if (is.null(pltcondf)) {
    message(paste(landarea.filter, "removed all records"))
    return(NULL)
  }

  ## Apply ACI.filter to condf
  if (landarea != "ALL") {
    pltcondf <- datFilter(x=pltcondf, xfilter=ACI.filter,
			title.filter="ACI.filter", gui=gui, stopifnull=FALSE)$xf
    if (is.null(pltcondf)) {
      message(paste(ACI.filter, "removed all records"))
      return(NULL)
    }
  }

  ## Check sumunits
  ########################################################
  sumunits <- pcheck.logical(sumunits, varnm="sumunits",
		title="Sum estimation units?", first="YES", gui=gui, stopifnull=TRUE)


  ## Check TPA
  ########################################################
  TPA <- pcheck.logical(TPA, varnm="TPA",
		title="TPA?", first="YES", gui=gui, stopifnull=TRUE)

  ## Check rawtable
  rawdata <- pcheck.logical(rawdata, varnm="rawdata", title="Output raw data?",
		first="NO", gui=gui, stopifnull=TRUE)

  ## Check rawonly
  rawonly <- pcheck.logical(rawonly, varnm="rawonly", title="Raw data only?",
		first="NO", gui=gui, stopifnull=TRUE)
  if (rawonly && !rawdata) rawdata <- TRUE


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


  ## Check addtitle
  addtitle <- pcheck.logical(addtitle, varnm="addtitle",
		title="Add title to output?", first="YES", gui=gui, stopifnull=TRUE)

  ## Check raw_fmt
  if (rawdata) {
    raw_fmtlst <- c('sqlite', 'sqlite3', 'db', 'db3', 'gpkg', 'csv', 'gdb', 'shp')
    raw_fmt <- pcheck.varchar(raw_fmt, varnm="raw_fmt", checklst=raw_fmtlst,
		caption="Out raw format", gui=gui)
  }


  ## Check output info
  ########################################################
  if (savedata) {
    if (!rawonly) {
      outlst <- pcheck.output(out_fmt="csv", outfolder=outfolder,
		           outfn.pre=outfn.pre, outfn.date=outfn.date,
		           overwrite_layer=overwrite_layer, append_layer=append_layer, gui=gui)
      outfolder <- outlst$outfolder
      overwrite_layer <- outlst$overwrite_layer
      outfn.pre <- outfn.pre
    }
    if (rawdata) {
      if (!is.null(raw_fmt) && raw_fmt == "csv") {
        rawfolder <- paste(outfolder, "rawdata", sep="/")
        if (!file.exists(rawfolder)) dir.create(rawfolder)
      } else {
        if (is.null(raw_dsn)) {
          raw_dsn <- "rawdata"
        }
        outlst <- pcheck.output(out_dsn=raw_dsn, out_fmt=raw_fmt,
		          outfolder=outfolder, outfn.pre=outfn.pre, outfn.date=outfn.date,
		          overwrite_dsn=overwrite_dsn, overwrite_layer=overwrite_layer,
		          append_layer=append_layer, gui=gui)
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
  returnlst <- list(pltcondf=setDT(pltcondf), cuniqueid=cuniqueid, sumunits=sumunits,
	    TPA=TPA, allin1=allin1, estround=estround, pseround=pseround, divideby=divideby,
	    addtitle=addtitle, returntitle=returntitle, estround=estround, pseround=pseround,
 	    landarea=landarea, rawdata=rawdata, rawonly=rawonly, savedata=savedata,
	    outfolder=outfolder, overwrite_layer=overwrite_layer, append_layer=append_layer,
	    rawfolder=rawfolder, raw_fmt=raw_fmt, raw_dsn=raw_dsn, pop_fmt=pop_fmt, 
	    pop_dsn=pop_dsn, whereqry=whereqry)


  if (esttype %in% c("TREE", "RATIO", "SEED")) {

    ## Check estseed
    ########################################################
    estseedlst <- c("none", "only", "add")
    estseed <- pcheck.varchar(var2check=estseed, varnm="estseed",
		checklst=estseedlst, caption="Seedlings", stopifnull=TRUE)
    if (estseed == "none") {
      seedx <- NULL
    } else {
      if (is.null(seedx)) {
        message("no seedling data in population data")
		    return(NULL)
      }
    }

 	## Check treex and seedx
    ###########################################################################
    if (estseed != "only") {
      treex <- pcheck.table(treex, conn = dbconn, stopifnull = TRUE, 
                       stopifinvalid = TRUE, checkonly = TRUE)				
      if (is.data.frame(treex)) {
        ## Check the values of tuniqueid in treex are all in cuniqueid in pltcondf
        treef <- check.matchval(treex, pltcondf, tuniqueid, cuniqueid, 
                      tab1txt="tree", tab2txt="cond", subsetrows=TRUE)
        returnlst$treef <- data.table(treef)
		    treenames <- names(treef)
		    
		    ## set key 			   
		    setkeyv(treef, c(tuniqueid, condid))	  
      } else {
	      returnlst$treef <- treex
		    treenames <- DBI::dbListFields(dbconn, treex)
	    }
    
      ## check tuniqueid in tree table
      tuniqueid <- pcheck.varchar(var2check=tuniqueid, varnm="tuniqueid", gui=gui,
	                   checklst=treenames, caption="tuniqueid")

	    ## Check tfilter
	    if (!is.null(tfilter)) {
		    tfilter <- RtoSQL(tfilter, x = treenames)
#	    if (is.null(whereqry)) {
#          whereqry <- paste0(whereqry, " AND ", tfilter)
#		} else {
#          whereqry <- paste0("\nWHERE ", tfilter)
#        }
        returnlst$tfilter <- tfilter		
      }
    }
	
    if (estseed %in% c("add", "only")) {
      seedx <- pcheck.table(seedx, conn = dbconn, stopifnull = TRUE, 
                       stopifinvalid = TRUE, checkonly = TRUE)
      if (is.data.frame(seedx)) {
        ## Check the values of tuniqueid in seedx are all in cuniqueid in pltcondf
        seedf <- check.matchval(seedx, pltcondf, tuniqueid, cuniqueid, 
                       tab1txt="seed", tab2txt="cond", subsetrows=TRUE)
        returnlst$seedf <- data.table(seedf)
		    seednames <- names(seedf)

		    ## set key 			   
		    setkeyv(seedf, c(tuniqueid, condid))
      } else {
	      returnlst$seedf <- seedx
		    seednames <- DBI::dbListFields(dbconn, seedx)
      }
 
      ## check tuniqueid in tree table
      tuniqueid <- pcheck.varchar(var2check=tuniqueid, varnm="tuniqueid", gui=gui,
	                   checklst=seednames, caption="tuniqueid")

	    ## check tuniqueid in seed table	
      if (!tuniqueid %in% seednames) {
	      message(tuniqueid, " not in seed table")
	      return(NULL)
	    }
	  
	    ## Check tfilter
	    if (estseed == "only" && !is.null(tfilter)) {
#		tfilter <- RtoSQL(tfilter, x = seednames)
#	    if (is.null(whereqry)) {
#          whereqry <- paste0(whereqry, " AND ", tfilter)
#		} else {
#          whereqry <- paste0("\nWHERE ", tfilter)
#        }		  
        returnlst$tfilter <- tfilter		
      }
	  }
	
    returnlst$tuniqueid <- tuniqueid
    returnlst$estseed <- estseed
	
    ## Check woodland
    woodlandlst <- c("Y", "N", "only")
    woodland <- pcheck.varchar(var2check=woodland, varnm="woodland", 
		checklst=woodlandlst, gui=gui, caption="Woodland?") 
	  returnlst$woodland <- woodland
  }

  if (esttype == "P2VEG") {
    vcondx <- pcheck.table(vcondx, conn = dbconn, stopifnull = TRUE, 
                       stopifinvalid = TRUE, checkonly = TRUE)
    if (is.data.frame(vcondx)) {
      ## Check the values of vuniqueid in vcondsppx are all in cuniqueid in pltcondf
      vcondf <- check.matchval(vcondx, pltcondf, c(vuniqueid, condid), 
				c(cuniqueid, condid), tab1txt="vcondx", tab2txt="cond", 
				subsetrows=TRUE)
	    returnlst$vcondf <- vcondf
	    vnames <- names(vcondf)
	  } else {
      returnlst$vcondf <- vcondx
      vnames <- DBI::dbListFields(dbconn, vcondx)
	  }
    returnlst$vuniqueid <- vuniqueid

    ## Check tfilter
	  if (!is.null(tfilter)) {
	    tfilter <- RtoSQL(tfilter, x = vnames)
#	  if (is.null(whereqry)) {
#        whereqry <- paste0(whereqry, " AND ", tfilter)
#      } else {
#        whereqry <- paste0("\nWHERE ", tfilter)
#      }
      returnlst$tfilter <- tfilter		
    }
  }

  if (esttype %in% c("AREA", "TREE")) {
    returnlst$totals <- totals
  }

  if (isdb) {
    returnlst$dbconn <- dbconn
  }

  return(returnlst)
}
