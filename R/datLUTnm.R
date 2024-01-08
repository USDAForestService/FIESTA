#' Data - Gets variable description or class.
#' 
#' Merge a look-up table to append new variables, names, or categories to x.
#' 
#' 
#' @param xvar String. Name of variable in the data table to join to.
#' @param x Data frame or comma-delimited file (*.csv). The data table with
#' variable to classify.
#' @param uniquex String. Unique values to match, if x is NULL.
#' @param LUT Data frame or comma-delimited file (*.csv). Name of the file with
#' collapsed classes (If FIAname=FALSE).
#' @param LUTvar String. Name of variable in LUT with values matching that
#' xvar.  If LUTvar=NULL, LUTvar=xvar.
#' @param LUTnewvar String. Name(s) of other variable(s) in the look-up table
#' to include in join. If NULL, all other variables in table will be included.
#' @param LUTnewvarnm String. Different name(s) for LUTnewvar. If NULL, names
#' will default to LUTnewvar. The length of LUTnewvarnm must equal the length
#' for LUTnewvar.
#' @param FIAname Logical. If TRUE, get FIA reference name based on (ref_codes)
#' within FIESTA.
#' @param group Logical. If TRUE and FIA=TRUE, the group variables in reference
#' table (ref_codes) are merged to data table (GROUPCD, GROUPNM).
#' @param NAclass String. NA values in xvar will be changed to NAclass.
#' @param add0 Logical. IF TRUE, keep all codes in look up table. If FALSE,
#' only include codes that are in x.
#' @param spcdname String. Name for species output type ('COMMON', 'SCIENTIFIC', 
#' 'SYMBOL', 'COMMON_SCIENTIFIC').
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
#' # Append forest type names using the reference table above.
#' ref_fortypcd <- ref_codes[ref_codes$VARIABLE == "FORTYPCD",]
#' WYcondlut <- datLUTnm(WYcond,
#'                       xvar = "FORTYPCD",
#'                       LUT = ref_fortypcd,
#'                       LUTvar = "VALUE",
#'                       LUTnewvar = "MEANING",
#'                       LUTnewvarnm = "FORTYPNM")
#' names(WYcondlut)
#' WYcond2 <- WYcondlut$xLUT
#' head(WYcond2[WYcond2$FORTYPCD > 0, ])
#' 
#' # Append forest type names the FIAname parameter. If the xvar is in the stored
#' # reference table, the name and values will automatically be appended.
#' WYcondlut2 <- datLUTnm(WYcond,
#'                        xvar = "FORTYPCD",
#'                        FIAname = TRUE)
#' names(WYcondlut2)
#' WYcond3 <- WYcondlut2$xLUT
#' head(WYcond3[WYcond3$FORTYPCD > 0, ])
#' @export datLUTnm
datLUTnm <- function(xvar, 
                     x = NULL, 
                     uniquex = NULL,
                     LUT = NULL, 
                     LUTvar = NULL, 
                     LUTnewvar = NULL, 
                     LUTnewvarnm = NULL, 
                     FIAname = FALSE,
					 group = FALSE, 
                     NAclass = "Other", 
                     add0 = FALSE, 
					 spcdname = "COMMON_SCIENTIFIC",
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

  if (gui) x=xvar=FIAname=LUT=LUTvar=LUTnewvar=datnmlst <- NULL 

  ## Adds to file filters to Cran R Filters table.
  if (.Platform$OS.type=="windows") 
    Filters <- rbind(Filters, csv=c("Comma-delimited files (*.csv)", "*.csv"))

  ## Set global variables
  VALUE=uniqueval=datnm <- NULL 
  returnlst <- list()
  
  ##################################################################
  ## CHECK PARAMETER NAMES
  ##################################################################
  
  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- names(formals(datLUTnm)) 
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
	  if (!is.null(datidx) && any(!is.na(datidx$cols))) {
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
	if (is.null(datx)) {
      if (is.character(x) && length(x) > 1) {
        datnmlst <- x
	  }
    } else {  
	  
      issf <- ifelse ("sf" %in% class(datx), TRUE, FALSE)
      if (issf) datx <- data.table(datx) 
      datnmlst <- names(datx) 
	  datkey <- key(datx)
	}
  }

  ## Check LUT
  ########################################################
  if (is.vector(LUT) && length(LUT) > 1) {
    LUTx <- data.table(LUT)
    setnames(LUTx, xvar)
  } else {
    LUTx <- pcheck.table(LUT, gui=gui, tabnm="LUT", caption="Look up table?")
  }

  ## Check FIAname
  ########################################################
  FIAname <- pcheck.logical(FIAname, varnm="FIAname", 
		title="FIA reference table?", first="YES", gui=gui)
  if (is.null(LUTx) && !FIAname) {
    message("LUT is null and FIAname=FALSE")
    return(NULL)
  }

 ## Check group
  if (FIAname) {
    group <- pcheck.logical(group, varnm="group", title="Variable group?", 
		  first="NO", gui=gui)
  } else {
     group <- FALSE
  }

  ## Check uniquex
  #############################################################
  if (is.null(uniquex) && is.null(x)) {
    message("both uniquex and x are NULL")
	return(NULL)
  } else if (!is.null(uniquex) && is.null(x)) {
	if (FIAname) {
	  datnmlst <- sort(unique(ref_codes$VARIABLE))
	} else if (!is.null(LUTx)) {
      datnmlst <- names(LUTx)
    }		
  } 
  
  ## Check xvar
  ##########################################
  xvar <- pcheck.varchar(xvar, "xvar", datnmlst, gui=gui,
		caption="Join variable in dat", stopifnull=TRUE)
 
  ## Check add0 
  add0 <- pcheck.logical(add0, varnm="add0", title="Add 0 values to missing codes?", 
                             first="NO", gui=gui)

  
  ## Get unique values
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
  if (!is.null(uniquex)) {
    if (!is.null(uniqueval)) {
	  if (!all(uniquex %in% uniqueval)) {
	    missval <- uniquex[!uniquex %in% uniqueval]
		if (length(missval) > 0) {
		  message("uniquex values missing: ", toString(missval)) 
		} else {
		  message("no uniquex in x... returning NULL")
		  return(NULL)
		}
	  } 
	  uniquex <- uniquex[uniquex %in% uniqueval]
	  
	  if (add0 && length(uniquex) < length(uniqueval)) {
        message("add0 = TRUE and uniquex less than uniqueval... using all values")
	  }
	} 
  }
  	   
  if (xvar == "SPCD") {
	return(datLUTspp(x = datx, 
		             uniquex = uniquex, 
				     spcdname = spcdname,
                     add0 = add0, 
				     stopifmiss = stopifmiss, 
                     xtxt = xtxt, 
				     dbconn = dbconn,
					 dbconnopen = dbconnopen,
					 dbwrite = dbwrite,
					 dbreturn = dbreturn,
					 overwrite = overwrite,
					 savedata = savedata,
                     savedata_opts = savedata_opts))
  }

  #######################################################################
  ## Check LUTvar
  #######################################################################
  LUTnmlst <- names(LUTx)

  if (!FIAname) {
    LUTvar <- pcheck.varchar(LUTvar, "LUTvar", LUTnmlst, gui=gui,
		caption="Join variable in LUT")
    if (is.null(LUTvar)) {
      if (xvar %in% LUTnmlst) {
        LUTvar <- xvar
      } else {
        stop("invalid LUTvar")
      }
    } else if (LUTvar != xvar && checknm(xvar, LUTnmlst) == xvar) {
	  setnames(LUTx, LUTvar, xvar)
	  LUTvar <- xvar
	}
	
	## Check LUTnewvar
    LUTnmlst <- LUTnmlst[!LUTnmlst %in% LUTvar]
    if (length(LUTnmlst) == 0) {
      LUTnewvar <- NULL
    } else {
      LUTnewvar <- pcheck.varchar(LUTnewvar, "LUTnewvar", LUTnmlst, gui=gui,
		caption="New variable(s)", multiple=TRUE)
      if (length(LUTnewvar) == 0) {
	    if (gui) {
          LUTnewvar <- select.list(LUTnewvarlst, title=paste("LUT", xvar, "NEW"), 
							multiple=TRUE)
          if (length(LUTnewvar) == 0) stop("")
        } else {
          if (length(LUTnmlst) == 1) {
            LUTnewvar <- LUTnmlst
          } else {
            LUTnewvar <- NULL
          }
        }
      }
	}
	if (!is.null(LUTnewvar)) {
	  if (!all(LUTnewvar %in% LUTnmlst)) { 
        notin <- LUTnewvar[which(!LUTnewvar %in% LUTnmlst)]
        warning("check LUTnewvar. Invalid name: ", toString(notin))
        LUTnewvar <- select.list(LUTnmlst, title=paste("LUT", xvar, "NEW"), 
			multiple=TRUE)
        if (length(LUTnewvar) == 0) stop("")
	  }
	}  
  
    if (!is.null(LUTnewvarnm)) {
      if (length(LUTnewvarnm) != length(LUTnewvar)) {
        message("LUTnewvarnm must be same number as LUTnewvar")
		LUTnewvarnm <- LUTnewvar
	  }
	  for (i in 1:length(LUTnewvar)) {
	    newvar <- LUTnewvar[i]
		newvarnm <- LUTnewvarnm[i]
	    if (checknm(newvarnm, LUTnmlst) == newvarnm) {
	      setnames(LUTx, newvar, newvarnm)
		  LUTnewvar[LUTnewvar %in% newvar] <- newvarnm
	    }
	  }
    } else {
      LUTnewvarnm <- LUTnewvar
    }
		
	#LUTnewvars <- LUTnewvar
    if (group) {
      if ("GROUPCD" %in% LUTnmlst) LUTnewvar <- unique(c(LUTnewvar, "GROUPCD"))
      if ("GROUPNM" %in% LUTnmlst) LUTnewvar <- unique(c(LUTnewvar, "GROUPNM"))
	  LUTnewvarnm <- c(LUTnewvarnm, "GROUPCD", "GROUPNM")
    }
	
	## Subset columns of LUTx
	LUTx <- LUTx[, c(LUTvar, LUTnewvar), with=FALSE]
 
 ## Subset rows of LUTx
	if (!add0 && !is.null(uniquex)) {
	  LUTx <- LUTx[LUTx[[LUTvar]] %in% uniquex, ]
	}

  } else if (FIAname) {
    xvar.ref <- getRefobject(toupper(xvar))
	
    ## Check if DSTRBCD
    #################################################
    LUTvar <- ifelse (FIAname && length(grep("DSTRBCD", xvar)) == 1, "DSTRBCD", xvar)
    if (FIAname && group) {
      if (!LUTvar %in% unique(FIESTAutils::ref_codes[!is.na(FIESTAutils::ref_codes[["GROUPNM"]]) & 
		FIESTAutils::ref_codes[["GROUPNM"]] != "", "VARIABLE"])) {
        message (paste("row group not available for", xvar))   
        group <- FALSE
      }
    }
     
	## Get FIA reference table for xvar
    #################################################
    lutvars <- c("VALUE", "MEANING")
    grpvars <- {}
    if (group) grpvars <- c("GROUPCD", "GROUPNM")  
    ref <- setDT(FIESTAutils::ref_codes[FIESTAutils::ref_codes[["VARIABLE"]] == xvar.ref,
 		c(lutvars, grpvars)])
 
    ## Check LUTx - xvar in LUTx
    #################################################
    if (!is.null(LUTx)) {
      if (!xvar %in% names(LUTx)) stop(paste(xvar, "not in LUT"))

      if (is.factor(LUTx[[xvar]])) LUTx[[xvar]] <- as.character(LUTx[[xvar]])
      #LUTx <- ref[is.na(VALUE) | VALUE %in% LUTx[[xvar]],]
      LUTx <- ref[VALUE %in% LUTx[[xvar]],]
      LUTnewvarlst <- names(LUTx)

      ## To get a name other than MEANING
      nameslst <- getnm(xvar, group=group)
      LUTnewvar <- nameslst$xvarnm
      LUTnewvar <- checknm(LUTnewvar, names(datx))         
      setnames(LUTx, lutvars, c(xvar, LUTnewvar))

      ## set new names
      if (group) {
        grpcode <- nameslst$grpcode
        grpname <- nameslst$grpname
        if (!is.null(datnmlst)) {
          grpnames <- sapply(c(grpcode, grpname), checknm, datnmlst)
        }     
        setnames(LUTx, grpvars, grpnames)
        LUTnewvar <- c(LUTnewvar, grpnames)
      }        
    } else {    
      LUTx <- ref
      LUTnewvarlst <- names(LUTx)

      if ("VALUE" %in% names(LUTx)) {
        ## To get a name other than MEANING
        nameslst <- getnm(LUTvar, group=group)
        LUTnewvar <- nameslst$xvarnm
        if (!is.null(datnmlst)) {
          LUTnewvar <- checknm(LUTnewvar, datnmlst) 
        } 
        setnames(LUTx, lutvars, c(xvar, LUTnewvar))
        LUTvar <- xvar

        ## set new names
        if (group) {
          grpcode <- nameslst$grpcode
          grpname <- nameslst$grpname
          if (!is.null(datnmlst)) {
            grpnames <- sapply(c(grpcode, grpname), checknm, datnmlst)
          }     
          setnames(LUTx, grpvars, grpnames)
          LUTnewvar <- c(LUTnewvar, grpnames)
        } 
      } else {
        stop("LUTvar not in LUTx")
      }

      if (!is.null(datx) && !is.null(uniquex)) {
        ## Subset LUT values to only those in datx
        if (sum(LUTx[[LUTvar]] %in% uniquex, na.rm=TRUE) == 0) {
          message(paste("no rows exist for", LUTvar))
          return(NULL)
        }
      }
    }
  } 

  ### Check NAclass
  if (!is.character(NAclass) && length(NAclass) != 1) {
    stop("NAclass must be a character string of length 1")
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
  ### SELECT RELEVANT COLUMNS FROM LUT & MERGE TO x.
  if ((!isdb || dbreturn) && !is.null(datx)) {
    if (!is.null(LUTnewvar) && length(LUTnewvar) != 0) {
      if (is.null(xtxt)) xtxt <- xvar
      ## Check that the all values of LUTvar in datx are all in xvar in LUTx
      check.matchval(datx, LUTx, xvar, LUTvar, tab1txt=xtxt, 
			tab2txt=paste(xtxt, "lut"), stopifmiss=stopifmiss)

      ## Check if class of xvar in datx matches class of xvar in LUTx
      tabs <- check.matchclass(datx, LUTx, xvar, LUTvar, 
		tab1txt=xtxt, tab2txt=LUTvar)
      datx <- tabs$tab1
      LUTx <- tabs$tab2

      LUTnewvar2 <- sapply(LUTnewvar, checknm, names(datx))	  
      if (all(LUTnewvar2 == LUTnewvar)) {
        setnames(LUTx, LUTnewvar, LUTnewvar2)
        LUTnewvar <- LUTnewvar2
      }
      datx.names <- names(datx)
      xLUT <- merge(datx, LUTx[,c(LUTvar, LUTnewvar), with=FALSE], 
			by.x=xvar, by.y=LUTvar, all.x=TRUE)

      xLUTvals <- unique(as.character(xLUT[[LUTnewvar[1]]][!is.na(xLUT[[xvar]])]))
      if (any(is.na(xLUTvals))) {
        xLUTmiss <- unique(xLUT[!is.na(get(xvar)) & is.na(get(LUTnewvar[1])), xvar, 
		with=FALSE])
        warning("missing codes: ", paste(xLUTmiss, collapse=", "))
      }

      setcolorder(xLUT, c(datx.names, LUTnewvar))
      if (!is.null(LUTnewvarnm)) 
        setnames(xLUT, LUTnewvar, LUTnewvarnm)    
    } else {
      xLUT <- datx
    }
     
    ## Only include xvar values that exist in x
    if (!add0) {
      LUTx <-LUTx[LUTx[[LUTvar]] %in% unique(xLUT[[xvar]]), ]
    }
  }
  
  ## Get all values of LUTx newvars
  LUTnewvar.vals <- unique(unlist(lapply(LUTx[,LUTvar, with=FALSE], as.character)))


  ## Add records if no values exist in xLUT
  if (!is.null(datx)) {
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
  if (!is.null(uniquex)) {
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
    } else if (!add0) {
	  LUTx <- LUTx[LUTx[[LUTvar]] %in% uniquex, ]
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
  returnlst$LUT <- LUTx[, c(LUTvar, LUTnewvar), with=FALSE]

  if (group) {
    returnlst$grpcode <- grpcode
    returnlst$grpname <- grpname
  }  
  
  #### WRITE TO FILE 
  #############################################################
  if (savedata) {
    if ("sf" %in% class(datx)) {
      spExportSpatial(xLUT, 
            savedata_opts=list(outfolder=outfolder, 
                                 out_fmt=outsp_fmt, 
                                 out_dsn=out_dsn, 
                                 out_layer=out_layer,
                                 outfn.pre=outfn.pre, 
                                 outfn.date=outfn.date, 
                                 overwrite_layer=overwrite_layer,
                                 append_layer=append_layer, 
                                 add_layer=TRUE))
    } else {
      datExportData(xLUT, 
            savedata_opts=list(outfolder=outfolder, 
                                 out_fmt=out_fmt, 
                                 out_dsn=out_dsn, 
                                 out_layer=out_layer,
                                 outfn.pre=outfn.pre, 
                                 outfn.date=outfn.date, 
                                 overwrite_layer=overwrite_layer,
                                 append_layer=append_layer,
                                 add_layer=TRUE))
                    
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
