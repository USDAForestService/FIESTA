#' Data - Gets variable description or class for SPCD.
#' 
#' Merge the ref_species table to append new variables, names, or categories to x.
#' 
#' 
#' @param x Data frame or comma-delimited file (*.csv). The data table with
#' variable to classify.
#' @param uniquex String. Unique values of SPCD to match, if x is NULL.
#' @param NAclass String. NA values in xvar will be changed to NAclass.
#' @param group Logical. If TRUE, the group variable in ref_species
#' are merged to data table (E_SPGRPCD, W_SPGRPCD), depending on state(s) 
#' specified. If states overlap both E and W regions, the region with 
#' majority is used or E if equal. The group name is merged from 
#' ref_codes, SPGRPCD Variable.
#' @param states String. Name of state(s) the x table is from.
#' @param spcdname String. Name for species output type ('COMMON', 'SCIENTIFIC', 
#' 'SYMBOL', 'COMMON_SCIENTIFIC').
#' @param add0 Logical. IF TRUE, keep all codes in look up table. If FALSE,
#' only include codes that are in x.
#' @param stopifmiss Logical. IF TRUE, stops function if missing codes in LUTx.
#' @param xtxt String.* Name of x table for more useful information in
#' warnings.
#' @param dsn String. Data source name of database with x.
#' @param dbconn Open database connection.
#' @param dbconnopen Logica. If TRUE, keep database connection open.
#' @param dbwrite Logical. If TRUE, write class column to database table x.
#' @param dbreturn Logical. If TRUE, return table with class column.
#' @param overwrite Logical. If TRUE, and the class name already exists 
#' in x, overwrites class name.
#' @param savedata Logical. If TRUE, saves data to outfolder.
#' @param savedata_opts List. See help(savedata_options()) for a list
#' of options. Only used when savedata = TRUE. If out_layer = NULL,
#' default = 'datlut'. 
#' @param gui Logical. If gui, user is prompted for parameters.
#'
#' @return \item{xLUT}{ The input data table with look-up table variable(s). }
#' \item{xLUTnm}{ Name of the new variable(s). } \item{LUT}{ Look up table with
#' categories. }
#' 
#' If savedata = TRUE, a comma-delimited file is output to the outfolder as
#' outfn.  If outfn = NULL, the name of the file will be datlut_'date'.csv.
#' @note For available reference tables:
#' sort(unique(ref_codes$VARIABLE))
#' @author Tracey S. Frescino
#' @keywords data
#' @examples
#' WYtreelut <- datLUTspp(WYtree)
#' names(WYtreelut)
#' WYtree2 <- WYtreelut$xLUT
#' head(WYtree2)
#' @export datLUTspp
datLUTspp <- function(x = NULL, 
                      uniquex = NULL,
                      NAclass = "Other", 
                      group = FALSE, 
                      states = NULL,
                      spcdname = "COMMON",
                      add0 = FALSE, 
                      stopifmiss = FALSE, 
                      xtxt = NULL,
					  dsn = NULL,
                      dbconn = NULL,
                      dbconnopen = FALSE,
					  dbwrite = FALSE,
				      dbreturn = TRUE, 
                      overwrite = TRUE,
                      savedata = FALSE, 
                      savedata_opts = NULL, 
                      gui = FALSE){
  #################################################################################
  ## DESCRIPTION: Merge variable(s) from a reference table stored within FIESTA  
  ##      (ref_codes) or a comma-delimited file (*.csv).
  #################################################################################

  ## CHECK GUI - IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- ifelse(nargs() == 0, TRUE, FALSE)

  if (gui) x=group=sciname <- NULL 

  ## Adds to file filters to Cran R Filters table.
  if (.Platform$OS.type=="windows") 
    Filters <- rbind(Filters, csv=c("Comma-delimited files (*.csv)", "*.csv"))

  ## Set global variables
  VALUE=LUTnewvarnm=uniqueval <- NULL 
  returnlst <- list()

  ##################################################################
  ## CHECK PARAMETER NAMES
  ##################################################################
  
  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- names(formals(datLUTspp)) 
  if (!all(input.params %in% formallst)) {
    miss <- input.params[!input.params %in% formallst]
    stop("invalid parameter: ", toString(miss))
  }
  
  ## Check parameter lists
  pcheck.params(input.params, savedata_opts=savedata_opts)
  
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
  
  ##################################################################
  ## Check database connection (dbconn)
  ##################################################################
  isdb=issf <- FALSE 

  ## Check dbconn and dsn
  ####################################################################
  if (!is.null(dbconn)) {
    if (!DBI::dbIsValid(dbconn)) {
      message("invalid database dbconnection") 
	  return(NULL)
    }
    isdb <- TRUE
  } else if (!is.null(dsn)) {
    dbconn <- DBtestSQLite(dsn, dbconnopen = TRUE, 
                           createnew = FALSE, returnpath = FALSE)
    if (is.null(dbconn)) {
      stop("invalid database")
    } else {
      isdb <- TRUE
    }
  } 
  if (isdb) {
    tablst <- DBI::dbListTables(dbconn)
	if (length(tablst) == 0) {
	  message("no tables in database")
	  return(NULL)
	}
	
  	datnm <- chkdbtab(tablst, x)
	if (is.null(datnm)) { 
	  if ("data.frame" %in% class(x)) {
	    isdb <- FALSE
		datx <- x
	  } else {
	    return(NULL)
	  }
	}
  }
 
  ##################################################################
  ## CHECK PARAMETER INPUTS
  ##################################################################

  ## Check datx
  ########################################################
  if (isdb) {
    datnmlst <- DBI::dbListFields(dbconn, datnm)
	
	## Check dbwrite 
    dbwrite <- pcheck.logical(dbwrite, 
	                          varnm = "dbwrite", 
	                          title = "Write to database?", 
                              first = "NO", gui=gui)

	## Check dbreturn 
    dbreturn <- pcheck.logical(dbreturn, 
	                           varnm = "dbreturn", 
	                           title = "Return data from database?", 
                               first="NO", gui=gui)
	
	if (dbreturn) {
      datx <- pcheck.table(datnm, conn=dbconn, gui=gui, 
                       caption="Data table?", returnDT=TRUE)
	  if (is.null(datx)) {
        message("invalid x")
	    return(NULL)
      }

	  datidx <- checkidx(dbconn, datnm)
	  if (!is.null(datidx) && !is.na(datidx$cols)) {
	    datx.idx <- strsplit(datidx$cols, "\\,")[[1]]
		if (all(datx.idx %in% datnmlst)) {
		  datkey <- datx.idx
		} else {
		  datkey <- datx.idx[datx.idx %in% datnmlst]
		}
	  }
	}

  } else {
    datx <- pcheck.table(x, gui=gui, 
                       caption="Data table?", returnDT=TRUE)
    issf <- ifelse ("sf" %in% class(datx), TRUE, FALSE)
    if (issf) datx <- data.table(datx) 
    datnmlst <- names(datx) 
	datkey <- key(datx)
  }   
 
  ## Check xvar
  #############################################################
  xvar <- "SPCD"
  
  ## Check name
  #############################################################
  spcdnamelst <- c("COMMON", "SCIENTIFIC", "SYMBOL", "COMMON_SCIENTIFIC")
  spcdname <- pcheck.varchar(spcdname, "spcdname", spcdnamelst, gui=gui,
		caption="SPCD name type", stopifnull=TRUE, multiple = TRUE)
 
  ## Check if all species codes in datx are in ref table
  #############################################################
  ref_spp <- FIESTAutils::ref_species[, c("SPCD", "COMMON_NAME", "GENUS", 
	  "SPECIES", "SPECIES_SYMBOL", "E_SPGRPCD", "C_SPGRPCD", "P_SPGRPCD", 
	  "MAJOR_SPGRPCD", "SCIENTIFIC_NAME")]
	  
  if (any(spcdname == "COMMON_SCIENTIFIC")) {
    ref_spp$COMMON_SCIENTIFIC <- 
		paste0(ref_spp$COMMON_NAME, " (", ref_spp$SCIENTIFIC_NAME, ")")
  }
 

  ## Check group
  group <- pcheck.logical(group, varnm="group", title="Variable group?", 
		first="NO", gui=gui)

  ## Check add0 
  add0 <- pcheck.logical(add0, varnm="add0", title="Add 0 values to missing codes?", 
                             first="NO", gui=gui)
 
  ## Get unique values from data table
  if (isdb && !dbreturn) {
    uniqueval.qry <- paste("SELECT DISTINCT", xvar, "FROM", datnm,
                           "ORDER BY", xvar)
    uniqueval <- na.omit(DBI::dbGetQuery(dbconn, uniqueval.qry))[[1]]
  } else if (!is.null(datx)) {
    if (!is.numeric(datx[[xvar]])) {
	  #message("xvar must be a numeric vector in x")
	  uniqueval <- sort(unique(as.numeric(as.character(datx[[xvar]]))))
    } else {
      #uniquex <- sort(unique(na.omit(datx[[xvar]])))
      uniqueval <- sort(unique(datx[[xvar]]))
    }
  }
  if (length(uniqueval) == 0) {
    message("no values exist in data for ", xvar)
  }
  
  ## Check if all values in uniquex are in dataset
  if (!is.null(uniquex)) {
 	if (!all(uniquex %in% uniqueval)) {
	  missval <- uniquex[!uniquex %in% uniqueval]
      if (length(missval) > 0) {
		if (!all(is.na(missval))) {
		  message("uniquex values missing: ", toString(missval)) 
        } else {
          uniqueval <- c(uniqueval, NA)
        }			
      } else {
		message("no uniquex in x... returning NULL")
		return(NULL)
	  } 
	  uniquex <- uniquex[uniquex %in% uniqueval]
	  
	  if (add0 && (length(uniquex) < length(uniqueval))) {
        message("add0 = TRUE and uniquex less than uniqueval... using all values")
	  }
	}
  } else {
	uniquex <- uniqueval
  }
	  
  ## Check if all values in dataset are in ref_species
  if (!all(uniquex %in% unique(ref_spp$SPCD))) {
    missval <- uniquex[!uniquex %in% unique(ref_spp$SPCD)]
    if (!all(is.na(missval))) {
      warning("missing values in ref_species: ", toString(missval))
      return(NULL)
	}
  } 
  
  ## Subset ref_spp table 
  if (!add0) {
    ref_spp <- ref_spp[ref_spp$SPCD %in% uniquex, ]
  }
   
  ## Check savedata 
  savedata <- pcheck.logical(savedata, varnm="savedata", title="Save data table?", 
                             first="NO", gui=gui)
  
  ## Check output parameters
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
    if (is.null(out_layer)) {
      out_layer <- "datlut"
    }
  }
  
  ############################################################################
  ## DO THE WORK 
  ############################################################################
  LUTvar <- "SPCD"
  LUTnewvar <- {}
  for (name in spcdname) {
    LUTnewvar <- c(LUTnewvar, 
	        ifelse(name == "COMMON_SCIENTIFIC", "COMMON_SCIENTIFIC",
			ifelse(name == "COMMON", "COMMON_NAME", 
			ifelse(name == "SCIENTIFIC", "SCIENTIFIC_NAME", "SPECIES_SYMBOL"))))
  }
  lutvars <- c("SPCD", LUTnewvar)


  ## Get grpvar
  #############################################################
  if (group) {
    grpname <- "SPGRPNM"

    grpnames <- getSPGRPCD(states)
    grpcode <- grpnames[1]
    lutvars <- c(lutvars, grpcode, "MAJOR_SPGRPCD")

    ## Merge SPGRPCD names from ref_codes
    ref_spgrpcd <- ref_codes[ref_codes$VARIABLE == "SPGRPCD", c("VALUE", "MEANING")]

    ref_spp <- merge(ref_spp, ref_spgrpcd, by.x=grpcode, by.y="VALUE", all.x=TRUE)
    if (any(is.na(ref_spp$MEANING))) {
      if (length(grpnames) > 1) {
        ref_spp <- merge(ref_spp, ref_spgrpcd, by.x=grpnames[2], 
				by.y="VALUE", all.x=TRUE)
        ref_spp$MEANING <- ref_spp$MEANING.x
        ref_spp[is.na(ref_spp), "MEANING"] <- "MEANING.y"
      } else {
        missgrps <- sort(unique(ref_spp[is.na(ref_spp), "VALUE"]))
        message("missing group variables: ", toString(missgrps))
      }
    }
    setnames(ref_spp, "MEANING", grpname)
  } 

  ## Subset ref_spp table
  ###############################################################
  LUTx <- unique(ref_spp[, lutvars]) 

  for (newvar in LUTnewvar) {
    if (any(duplicated(LUTx[[newvar]]))) {
      dups <- unique(LUTx[[newvar]][duplicated(LUTx[[newvar]])])
      message("duplicated values exist in data:\n", toString(dups))

      for (dup in dups) {
        duprows <- which(LUTx[[newvar]] == dup)
        nbrdups <- length(duprows)
        for (i in 2:nbrdups) {
          row <- duprows[i]
          newnm <- paste0(dup, "_", i - 1)
          LUTx[duprows[i], ][[newvar]] <- paste0(dup, "_", i - 1)
          if (is.factor(LUTx[[newvar]])) {
            levels(LUTx[[newvar]]) <- c(levels(LUTx[[newvar]]), newnm)
          }
		}
      }
    }
  }

  ## Merge ref_spp to datx
  ###############################################################

  ## Classify xvar column
  if (isdb && dbwrite) {
    for (lutvar in lutvars) {
	  if (lutvar != xvar) {
        dbcl <- dbclass(dbconn, 
	             tabnm = datnm,
			     classcol = xvar, 
			     classvals = LUTx[[xvar]],
			     classlabels = LUTx[[lutvar]],
			     classnm = lutvar,
			     overwrite = overwrite
			     )
		if (lutvar %in% spcdname && is.null(dbcl)) {
		  return(NULL)
		}
	  }
	}	
  } 
   
  if (!isdb || dbreturn && !is.null(datx)) {
    ## Check if class of xvar in datx matches class of xvar in LUTx
    tabs <- check.matchclass(datx, LUTx, xvar, LUTvar, 
		tab1txt=xtxt, tab2txt=LUTvar)
    datx <- tabs$tab1
    LUTx <- tabs$tab2
 
    #all.x <- ifelse(add0, TRUE, FALSE)
    xLUT <- merge(datx, LUTx, by.x=xvar, by.y=LUTvar, all.x=TRUE)
 
    ## Get all values of LUTx newvars
    LUTnewvar.vals <- unique(unlist(lapply(LUTx[,LUTnewvar, with=FALSE], as.character)))

    ## If NA values and NAclass != NULL, add NA to LUT
    if (!is.null(NAclass) && sum(is.na(xLUT[[xvar]])) > 0 && 
							all(!is.na(LUTx[[LUTvar]]))) {
      NAclass <- checknm(NAclass, LUTnewvar.vals) 

      LUTxrow <- rep(NA, ncol(LUTx))
      for (v in LUTnewvar) {
        if (!is.numeric(LUTx[[v]])) {
           if (is.factor(LUTx[[v]]))
             levels(LUTx[[v]]) <- c(levels(LUTx[[v]]), NAclass)
           LUTxrow[which(names(LUTx) == v)] <- NAclass
        }
      }
      LUTx <- rbind(LUTx, as.list(LUTxrow))

      ## change NA values in xLUT
      DT_NAto0(xLUT, LUTnewvar, changeto=NAclass)
    }
  }

  ## Add records if no other values exist in xLUT
  if (!all(uniquex %in% LUTx[[xvar]])) {
    xvals <- unique(na.omit(uniquex))
    missvals <- xvals[which(!xvals %in% unique(LUTx[[LUTvar]]))]

    if (length(missvals) > 0) {
      otherx <- checknm("Other", LUTnewvar.vals) 
      #otherx <- "Other"
      message("adding unclassified values to class ", otherx)

      LUTxrow <- data.table(matrix(data=NA, nrow=length(missvals), ncol=ncol(LUTx)))
      names(LUTxrow) <- names(LUTx)
      LUTxrow[[LUTvar]] <- missvals

      for (v in names(LUTx)[which(names(LUTx) != LUTvar)]) {
        if (!is.numeric(LUTx[[v]])) {
          if (is.factor(LUTx[[v]]))
             levels(LUTx[[v]]) <- c(levels(LUTx[[v]]), otherx)
          LUTxrow[[v]] <- rep(otherx, nrow(LUTxrow))
        } else {
          LUTxrow[[v]] <- rep(9999, nrow(LUTxrow))
        }        
      }
      LUTx <- rbind(LUTx, as.list(LUTxrow))
    }
  }  
          
  ## Return list
  ########################################################
  if ((!isdb || dbreturn) && !is.null(datx)) {
	returnlst$xLUT <- xLUT
  } else {
    returnlst$xLUT <- datnm
  }

  if (!is.null(LUTnewvarnm) || !is.null(LUTnewvar)) {
    returnlst$xLUTnm <- ifelse (is.null(LUTnewvarnm), LUTnewvar, LUTnewvarnm)
  } else {
    returnlst$xLUTnm <- NULL
  }
  returnlst$LUT <- LUTx
  returnlst$ref_spp <- ref_spp

  if (group) {
    returnlst$grpcode <- grpcode
    returnlst$grpname <- grpname
  }  
 
  #### WRITE TO FILE 
  #############################################################
  if (savedata) {
    if ("sf" %in% class(datx)) {
      spExportSpatial(xLUT, 
              savedata_opts=list(outfolder = outfolder, 
                                  out_fmt = outsp_fmt, 
                                  out_dsn = out_dsn, 
                                  out_layer = out_layer,
                                  outfn.pre = outfn.pre, 
                                  outfn.date = outfn.date, 
                                  overwrite_layer = overwrite_layer,
                                  append_layer = append_layer, 
                                  add_layer = TRUE))
    } else {
      datExportData(xLUT, 
            savedata_opts=list(outfolder = outfolder, 
                                  out_fmt = out_fmt, 
                                  out_dsn = out_dsn, 
                                  out_layer = out_layer,
                                  outfn.pre = outfn.pre, 
                                  outfn.date = outfn.date, 
                                  overwrite_layer = overwrite_layer,
                                  append_layer = append_layer,
                                  add_layer = TRUE))
                    
    }
    datExportData(LUTx, 
            savedata_opts=list(outfolder = outfolder, 
                                  out_fmt = out_fmt, 
                                  out_dsn = out_dsn, 
                                  out_layer = "LUTx",
                                  outfn.pre = outfn.pre, 
                                  outfn.date = outfn.date, 
                                  overwrite_layer = overwrite_layer,
                                  append_layer = append_layer,
                                  add_layer = TRUE))
  } 
  
  return(returnlst)
}
