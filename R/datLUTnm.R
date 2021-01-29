datLUTnm <- function(x, xvar=NULL, LUT=NULL, LUTvar=NULL, LUTnewvar=NULL, 
	LUTnewvarnm=NULL, FIAname=FALSE, NAclass="OtherNA", group=FALSE, add0=FALSE, 
	stopifmiss=FALSE, savedata=FALSE, outfolder=NULL, outfn="datlut", xtxt=NULL){
  #################################################################################
  ## DESCRIPTION: Merge variable(s) from a reference table stored within FIESTA  
  ##      (ref_codes) or a comma-delimited file (*.csv).
  #################################################################################

  ## CHECK GUI - IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- ifelse(nargs() == 0, TRUE, FALSE)

  if (gui) x=xvar=FIAname=LUT=LUTvar=LUTnewvar=varclass=minvar=maxvar=VALUE <- NULL 

  ## Adds to file filters to Cran R Filters table.
  if (.Platform$OS.type=="windows") 
    Filters <- rbind(Filters, csv=c("Comma-delimited files (*.csv)", "*.csv"))

  ##################################################################
  ## CHECK INPUT PARAMETERS
  ##################################################################
  ref_codes <- FIESTA::ref_codes

  ## Check datx
  ########################################################
  isdatatable <- FALSE
  datx <- FIESTA::pcheck.table(x, gui=gui, caption="Data table?")
  if ("data.table" %in% class(datx)) {
    isdatatable <- TRUE
    datkey <- key(datx)
#    datx <- setDF(datx)
  }

  ## Check xvar
  ##########################################
  datnmlst <- names(datx)
  xvar <- FIESTA::pcheck.varchar(xvar, "xvar", datnmlst, gui=gui,
		caption="Join variable in dat", stopifnull=TRUE)

 
  ## Check LUT
  ########################################################
  if (is.vector(LUT) && length(LUT) > 1) {
    LUTx <- data.table(LUT)
    setnames(LUTx, xvar)
  } else {
    LUTx <- FIESTA::pcheck.table(LUT, gui=gui, tabnm="LUT", caption="Look up table?")
  }

  ## Check FIAname
  ########################################################
  FIAname <- FIESTA::pcheck.logical(FIAname, varnm="FIAname", 
		title="FIA reference table?", first="YES", gui=gui)
  if (is.null(LUTx) && !FIAname) {
    message("LUT is null and FIAname=FALSE")
    return(NULL)
  }

  if (FIAname) {
    ## Get FIA reference table for xvar
    xvar.ref <- FIESTA::getRefobject(toupper(xvar))
    if (is.null(xvar.ref)) 
      stop(paste("no reference name for", xvar))
  }

  ## Check group
  group <- FIESTA::pcheck.logical(group, varnm="group", title="Variable group?", 
		first="NO", gui=gui)

  #######################################################################
  ## Check LUTvar
  #######################################################################
  LUTnmlst <- names(LUTx)

  if (!FIAname) {
    LUTvar <- FIESTA::pcheck.varchar(LUTvar, "LUTvar", LUTnmlst, gui=gui,
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
      LUTnewvar <- FIESTA::pcheck.varchar(LUTnewvar, "LUTnewvar", LUTnmlst, gui=gui,
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
#      nameslst <- FIESTA::getnm(xvar, group=group)
#      if ("MEANING" %in% names(LUTx)) {
#        LUTnewvarnm <- nameslst$LUTnewvarnm 
#        LUTnewvarnm <- FIESTA::checknm(LUTnewvarnm, names(datx))              
#        setnames(LUTx, "MEANING", LUTnewvarnm)
#        LUTnewvar <- c(LUTnewvar, LUTnewvarnm)
#      }
#      ## set new names
#      if (group) {
#        grpcode <- nameslst$grpcode
#        grpname <- nameslst$grpname
#        grpnames <- sapply(c(grpcode, grpname), FIESTA::checknm, names(datx))     
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
      if (!LUTvar %in% unique(ref_codes[!is.na(ref_codes[["GROUPNM"]]) & 
		ref_codes[["GROUPNM"]] != "", "VARIABLE"])) {
        message (paste("row group not available for", xvar))   
        group <- FALSE
      }
    }

    ## Get FIA reference table for xvar
    #################################################
   # xvar.ref <- FIESTA::getRefobject(toupper(LUTvar))
    #if (is.null(xvar.ref)) message(paste("no reference name for", xvar))

    lutvars <- c("VALUE", "MEANING")
    grpvars <- {}
    if (group) grpvars <- c("GROUPCD", "GROUPNM")  
    ref <- setDT(ref_codes[ref_codes[["VARIABLE"]] == xvar.ref, c(lutvars, grpvars)])

    ## Check LUTx - xvar in LUTx
    #################################################
    if (!is.null(LUTx)) {
      if (!xvar %in% names(LUTx)) stop(paste(xvar, "not in LUT"))

      if (is.factor(LUTx[[xvar]])) LUTx[[xvar]] <- as.character(LUTx[[xvar]])
      LUTx <- ref[is.na(VALUE) | VALUE %in% LUTx[[xvar]],]
      LUTnewvarlst <- names(LUTx)

      ## To get a name other than MEANING
      nameslst <- FIESTA::getnm(xvar, group=group)
      LUTnewvar <- nameslst$xvarnm
      LUTnewvar <- FIESTA::checknm(LUTnewvar, names(datx))         
      setnames(LUTx, lutvars, c(xvar, LUTnewvar))

      ## set new names
      if (group) {
        grpcode <- nameslst$grpcode
        grpname <- nameslst$grpname
        grpnames <- sapply(c(grpcode, grpname), FIESTA::checknm, names(datx))     
        setnames(LUTx, grpvars, grpnames)
        LUTnewvar <- c(LUTnewvar, grpnames)
      }        

    } else {    
      LUTx <- ref
      LUTnewvarlst <- names(LUTx)

      if ("VALUE" %in% names(LUTx)) {
        ## To get a name other than MEANING
        nameslst <- FIESTA::getnm(LUTvar, group=group)
        LUTnewvar <- nameslst$xvarnm
        LUTnewvar <- FIESTA::checknm(LUTnewvar, names(datx))   
        setnames(LUTx, lutvars, c(xvar, LUTnewvar))
        LUTvar <- xvar

        ## set new names
        if (group) {
          grpcode <- nameslst$grpcode
          grpname <- nameslst$grpname
          grpnames <- sapply(c(grpcode, grpname), FIESTA::checknm, names(datx))     
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
  if (!is.character(NAclass) && length(NAclass) != 1) 
    stop("NAclass must be a character string of length 1")

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
 
  ### GET savedata 
  savedata <- FIESTA::pcheck.logical(savedata, varnm="savedata", title="Save data tables?", 
		first="NO", gui=gui)

  ## GET OUTFOLDER IF NULL
  if (savedata)
    outfolder <- FIESTA::pcheck.outfolder(outfolder, gui)


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
    tabs <- FIESTA::check.matchclass(datx, LUTx, xvar, LUTvar, 
		tab1txt=xtxt, tab2txt=LUTvar)
    datx <- tabs$tab1
    LUTx <- tabs$tab2

    LUTnewvar2 <- sapply(LUTnewvar, FIESTA::checknm, names(datx))
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
  if (!add0) 
    LUTx <-LUTx[LUTx[[LUTvar]] %in% unique(xLUT[[xvar]]), ]

  ## Get all values of LUTx newvars
  LUTnewvar.vals <- unique(unlist(lapply(LUTx[,LUTnewvar, with=FALSE], as.character)))

  ## If NA values and NAclass != NULL, add NA to LUT
  if (!is.null(NAclass) && sum(is.na(datx[[xvar]])) > 0 && all(!is.na(LUTx[[LUTvar]]))) {
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
    xLUT <- setDT(xLUT)
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
  
  if (savedata) {
    if ("sf" %in% class(xLUT)) {
      spExportSpatial(xLUT, outfolder=outfolder, out_layer=outfn)
    } else {
      ## WRITE DATA TO OUTFOLDER
      write2csv(xLUT, outfilenm=outfn, outfolder=outfolder)
    }
  }
  return(xLUTlst)
}
