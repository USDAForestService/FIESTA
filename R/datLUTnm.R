#' Data - Gets variable description or class.
#' 
#' Merge a look-up table to append new variables, names, or categories to x.
#' 
#' 
#' @param x Data frame or comma-delimited file (*.csv). The data table with
#' variable to classify.
#' @param xvar String. Name of variable in the data table to join to.
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
#' @param NAclass String. NA values in xvar will be changed to NAclass.
#' @param group Logical. If TRUE, the group variables in reference table
#' (ref_codes) are merged to data table (GROUPCD, GROUPNM).
#' @param add0 Logical. IF TRUE, keep all codes in look up table. If FALSE,
#' only include codes that are in x.
#' @param stopifmiss Logical. IF TRUE, stops function if missing codes in LUTx.
#' @param xtxt String.* Name of x table for more useful information in
#' warnings.
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
datLUTnm <- function(x, 
                     xvar = NULL, 
                     LUT = NULL, 
                     LUTvar = NULL, 
                     LUTnewvar = NULL, 
                     LUTnewvarnm = NULL, 
                     FIAname = FALSE, 
                     NAclass = "Other", 
                     group = FALSE, 
                     add0 = FALSE, 
                     stopifmiss = FALSE, 
                     xtxt = NULL,
                     savedata = FALSE, 
                     savedata_opts = NULL, 
                     gui = FALSE){
  #################################################################################
  ## DESCRIPTION: Merge variable(s) from a reference table stored within FIESTA  
  ##      (ref_codes) or a comma-delimited file (*.csv).
  #################################################################################

  ## CHECK GUI - IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- ifelse(nargs() == 0, TRUE, FALSE)

  if (gui) x=xvar=FIAname=LUT=LUTvar=LUTnewvar <- NULL 

  ## Adds to file filters to Cran R Filters table.
  if (.Platform$OS.type=="windows") 
    Filters <- rbind(Filters, csv=c("Comma-delimited files (*.csv)", "*.csv"))

  ## Set global variables
  VALUE <- NULL 
  
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
  ## CHECK PARAMETER INPUTS
  ##################################################################

  ## Check datx
  ########################################################
  isdatatable <- FALSE
  datx <- pcheck.table(x, gui=gui, caption="Data table?")
  if ("data.table" %in% class(datx)) {
    isdatatable <- TRUE
    datkey <- key(datx)
#    datx <- setDF(datx)
  }

  ## Check xvar
  ##########################################
  datnmlst <- names(datx)
  xvar <- pcheck.varchar(xvar, "xvar", datnmlst, gui=gui,
		caption="Join variable in dat", stopifnull=TRUE)

 
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
 
  if (FIAname) {
    ## Get FIA reference table for xvar
    xvar.ref <- getRefobject(toupper(xvar))
    if (is.null(xvar.ref)) 
      stop(paste("no reference name for", xvar))
  }

  ## Check group
  group <- pcheck.logical(group, varnm="group", title="Variable group?", 
		first="NO", gui=gui)

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
    }
    if (group) {
      if ("GROUPCD" %in% LUTnmlst) LUTnewvars <- unique(c(LUTnewvars, "GROUPCD"))
      if ("GROUPNM" %in% LUTnmlst) LUTnewvars <- unique(c(LUTnewvars, "GROUPNM"))
    }

    LUTnmlst <- LUTnmlst[!LUTnmlst %in% LUTvar]
    if (length(LUTnmlst) == 0) {
      LUTnewvar <- NULL
    } else {
      LUTnewvar <- pcheck.varchar(LUTnewvar, "LUTnewvar", LUTnmlst, gui=gui,
		caption="New variable(s)", multiple=TRUE)
      if (length(LUTnewvar) == 0) LUTnewvar <- LUTnmlst
    }

    if (!is.null(LUTnewvarnm)) {
      if (length(LUTnewvarnm) != length(LUTnewvar)) 
        stop("LUTnewvarnm must be same number as LUTnewvar")

    } else {
      LUTnewvarnm <- LUTnewvar
    }
#    } else {
#      
#      ## To get a name other than MEANING
#      nameslst <- getnm(xvar, group=group)
#      if ("MEANING" %in% names(LUTx)) {
#        LUTnewvarnm <- nameslst$LUTnewvarnm 
#        LUTnewvarnm <- checknm(LUTnewvarnm, names(datx))              
#        setnames(LUTx, "MEANING", LUTnewvarnm)
#        LUTnewvar <- c(LUTnewvar, LUTnewvarnm)
#      }
#      ## set new names
#      if (group) {
#        grpcode <- nameslst$grpcode
#        grpname <- nameslst$grpname
#        grpnames <- sapply(c(grpcode, grpname), checknm, names(datx))     
#        setnames(LUTx, c("GROUPCD", "GROUPNM"), grpnames) 
#        LUTnewvar <- c(LUTnewvar, grpnames)
#      }
#    } 
    LUTnmlst <- names(LUTx)
    LUTnewvarlst <- LUTnmlst[which(LUTnmlst != LUTvar)]
 
  } else if (FIAname) {

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
   # xvar.ref <- getRefobject(toupper(LUTvar))
    #if (is.null(xvar.ref)) message(paste("no reference name for", xvar))

    lutvars <- c("VALUE", "MEANING")
    grpvars <- {}
    if (group) grpvars <- c("GROUPCD", "GROUPNM")  
    ref <- setDT(FIESTAutils::ref_codes[FIESTAutils::ref_codes[["VARIABLE"]] == xvar.ref, c(lutvars, grpvars)])
 
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
        grpnames <- sapply(c(grpcode, grpname), checknm, names(datx))     
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
        LUTnewvar <- checknm(LUTnewvar, names(datx))  
        setnames(LUTx, lutvars, c(xvar, LUTnewvar))
        LUTvar <- xvar

        ## set new names
        if (group) {
          grpcode <- nameslst$grpcode
          grpname <- nameslst$grpname
          grpnames <- sapply(c(grpcode, grpname), checknm, names(datx))     
          setnames(LUTx, grpvars, grpnames)
          LUTnewvar <- c(LUTnewvar, grpnames)
        } 
      } else {
        stop("LUTvar not in LUTx")
      }

      ## Subset LUT values to only those in datx
      if (sum(LUTx[[LUTvar]] %in% datx[[LUTvar]], na.rm=TRUE) == 0) {
        message(paste("no rows exist for", LUTvar))
        return(NULL)
      }
    }

    LUTnmlst <- names(LUTx)
    LUTnewvarlst <- LUTnmlst[which(LUTnmlst != LUTvar)]
  } 

  ### Check NAclass
  if (!is.character(NAclass) && length(NAclass) != 1) {
    stop("NAclass must be a character string of length 1")
  }

  ### GET LUTnewvar
  ###########################################
  if (length(LUTnewvarlst) > 0) {
    if (is.null(LUTnewvar)) {
      if (gui) {
        LUTnewvar <- select.list(LUTnewvarlst, title=paste("LUT", xvar, "NEW"), 
			multiple=TRUE)
        if (length(LUTnewvar) == 0) stop("")
      } else {
        if (length(LUTnewvarlst) == 1) {
          LUTnewvar <- LUTnewvarlst
        } else {
          stop("must include LUTnewvar")
        }
      }
    } else if (!all(LUTnewvar %in% LUTnewvarlst)) { 
      notin <- LUTnewvar[which(!LUTnewvar %in% LUTnewvarlst)]
      warning("check LUTnewvar. Invalid name: ", toString(notin))
      LUTnewvar <- select.list(LUTnewvarlst, title=paste("LUT", xvar, "NEW"), 
			multiple=TRUE)
      if (length(LUTnewvar) == 0) stop("")
    }
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
  if (!is.null(LUTnewvar) && length(LUTnewvar) != 0) {
    if (is.null(xtxt)) xtxt <- xvar
    ## Check that the all values of LUTvar in datx are all in xvar in LUTx
    check.matchval(datx, LUTx, xvar, LUTvar, tab1txt=xtxt, tab2txt=paste(xtxt, "lut"),
		stopifmiss=stopifmiss)

    ## Check if class of xvar in datx matches class of xvar in LUTx
    tabs <- check.matchclass(datx, LUTx, xvar, LUTvar, 
		tab1txt=xtxt, tab2txt=LUTvar)
    datx <- tabs$tab1
    LUTx <- tabs$tab2

    LUTnewvar2 <- sapply(LUTnewvar, checknm, names(datx))
    if (!identical(LUTnewvar2, LUTnewvar)) {
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
  
  ## Get all values of LUTx newvars
  LUTnewvar.vals <- unique(unlist(lapply(LUTx[,LUTnewvar, with=FALSE], as.character)))

  ## If NA values and NAclass != NULL, add NA to LUT
  if (!is.null(NAclass) && sum(is.na(xLUT[[xvar]])) > 0 && all(!is.na(LUTx[[LUTvar]]))) {
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

  ## Add records if not other values exist in xLUT
  if (!all(unique(xLUT[[xvar]]) %in% LUTx[[xvar]])) {
    xvals <- unique(na.omit(xLUT[[xvar]]))
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
  if (isdatatable) {
    xLUT <- data.table(xLUT)
    setkeyv(xLUT, datkey)
  }
  xLUTlst <- list(xLUT=xLUT)

  if (!is.null(LUTnewvarnm) || !is.null(LUTnewvar)) {
    xLUTlst$xLUTnm <- ifelse (is.null(LUTnewvarnm), LUTnewvar, LUTnewvarnm)
  } else {
    xLUTlst$xLUTnm <- NULL
  }
  xLUTlst$LUT <- LUTx

  if (group) {
    xLUTlst$grpcode <- grpcode
    xLUTlst$grpname <- grpname
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
  }
  
  
  return(xLUTlst)
}
