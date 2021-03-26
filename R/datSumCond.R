datSumCond <- function(cond=NULL, plt=NULL, plt_dsn=NULL, cuniqueid="PLT_CN", 
	puniqueid="CN", csumvar=NULL, csumvarnm=NULL, cfilter=NULL, getadjplot=FALSE, 
	adjcond=FALSE, NAto0=FALSE, savedata=FALSE, outfolder=NULL, out_fmt="csv",
	out_dsn=NULL, out_layer=NULL, outfn.pre=NULL, layer.pre=NULL, outfn.date=TRUE, 
	overwrite_dsn=FALSE, overwrite_layer=FALSE, append_layer=FALSE, returnDT=TRUE){
  #####################################################################################
  ## DESCRIPTION: Aggregates CONDPROP_UNADJ variable or other continuous condition 
  ##	variables to plot level with option to apply condition filters. If condition 
  ##	variable is not CONDPROP_UNADJ the variable is multiplied by CONDPROP_UNADJ  
  ##	for weighted sum.
  #####################################################################################

  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- ifelse(nargs() == 0, TRUE, FALSE)

  ## If gui.. set variables to NULL
  if(gui){ puniqueid=cuniqueid=csumvarnm=savedata <- NULL }

  ## SET OPTIONS
  options(scipen = 6) # bias against scientific notation
  options(stringsAsFactors=FALSE)

  ## SET glopbal variables
  CONDPROP_ADJ=CONDPROP_UNADJ <- NULL


  ##################################################################
  ## CHECK INPUT PARAMETERS
  ##################################################################

  ## Check cond table
  condx <- FIESTA::pcheck.table(cond, caption="Condition table?", stopifnull=TRUE)

  ## Check cuniqueid
  condnmlst <- names(condx)
  cuniqueid <- FIESTA::pcheck.varchar(var2check=cuniqueid, varnm="cuniqueid", 
	checklst=condnmlst, caption="UniqueID variable - cond", 
	warn="cuniqueid not in cond table", stopifnull=TRUE)

  ## Check for CONDPROP_UNADJ
  if (!"CONDPROP_UNADJ" %in% condnmlst) stop("CONDPROP_UNADJ not in cond")


  ## Check plt table
  noplt <- TRUE
  pltsp <- FALSE
  pltx <- pcheck.table(plt, gui=gui, tabnm="plt", caption="Plot table?")
  if (!is.null(pltx)) {
    noplt <- FALSE

    ## Remove totally nonsampled plots
    if ("PLOT_STATUS_CD" %in% names(pltx)) {
      if (3 %in% unique(pltx[["PLOT_STATUS_CD"]]))
        warning(paste("There are", sum(pltx[["PLOT_STATUS_CD"]] == 3), 
		"nonsampled plots"))
      pltx <- pltx[pltx$PLOT_STATUS_CD != 3,]
    } 

    if ("sf" %in% class(pltx))
      pltsp <- TRUE
      
    ## Check puniqueid
    pltnmlst <- names(pltx)
    nmlst <- names(pltx)
    puniqueid <- FIESTA::pcheck.varchar(var2check=puniqueid, varnm="puniqueid", 
		checklst=pltnmlst, caption="UniqueID variable - plt", 
		warn="puniqueid not in plot table", stopifnull=TRUE)

    ## Check that the values of cuniqueid in condx are all in puniqueid in pltx
    check.matchval(condx, pltx, cuniqueid, puniqueid)

    ## Check if class of cuniqueid matches class of puniqueid
    tabs <- check.matchclass(condx, pltx, cuniqueid, puniqueid)
    condx <- tabs$tab1
    pltx <- tabs$tab2  
  }

  ## Check csumvar
  csumvar <- FIESTA::pcheck.varchar(var2check=csumvar, varnm="csumvar", 
		checklst=condnmlst, caption="csumvar(s)", multiple=TRUE,
		stopifnull=TRUE, gui=gui)

  ## Check csumvarnm
  if (is.null(csumvarnm)) csumvarnm <- paste(csumvar, "PLT", sep="_")
  condnmlst <- sapply(csumvarnm, checknm, condnmlst)

  ## Check getadjplot
  getadjplot <- FIESTA::pcheck.logical(getadjplot, varnm="getadjplot", 
		title="Get plot adjustment?", first="NO", gui=gui)
  if (getadjplot && is.null(condx)) 
    stop("must include condx to adjust to plot")

  ## Check adjcond
  adjcond <- FIESTA::pcheck.logical(adjcond, varnm="adjcond", 
		title="Adjust conditions?", first="NO", gui=gui)


  ## Check savedata 
  savedata <- FIESTA::pcheck.logical(savedata, varnm="savedata", 
		title="Save data tables?", first="NO", gui=gui)

  ## If savedata, check output file names
  ################################################################
  if (savedata) { 
    outlst <- pcheck.output(gui=gui, out_dsn=out_dsn, out_fmt=out_fmt, 
		outfolder=outfolder, outfn.pre=outfn.pre, outfn.date=outfn.date, 
		overwrite_dsn=overwrite_dsn, overwrite_layer=overwrite_layer)
    out_dsn <- outlst$out_dsn
    outfolder <- outlst$outfolder
    out_fmt <- outlst$out_fmt

    ## outfn
    if (is.null(out_layer) || gsub(" ", "", out_layer) == "") {
      out_layer <- "condsum"
      if (!is.null(layer.pre)) {
        out_layer <- paste(layer.pre, out_layer, sep="_")
      }
    }
  }



  ################################################################################  
  ### DO WORK
  ################################################################################  

  if (getadjplot) {
    adjfacdata <- getadjfactorPLOT(condx=condx, cuniqueid=cuniqueid)
    condx <- adjfacdata$condadj
  }

  ## Filter cond
  cdat <- FIESTA::datFilter(x=condx, xfilter=cfilter, title.filter="tfilter",
			 stopifnull=TRUE, gui=gui)
  condf <- cdat$xf
  cfilter <- cdat$xfilter

  if (adjcond) {
    if ("cadjcnd" %in% names(condf))
      stop("cadjcnd not in cond... must get adjustment factor")
    csumvarnm <- paste0(csumvarnm, "_ADJ")
    condf.sum <- condf[, lapply(.SD, function(x) sum(x * CONDPROP_ADJ, na.rm=TRUE)),
 		by=cuniqueid, .SDcols=csumvar]
  } else {
    csumvar2 <- csumvar[csumvar != "CONDPROP_UNADJ"] 
    condf.sum <- condf[, lapply(.SD, function(x) sum(x * CONDPROP_UNADJ, na.rm=TRUE)),
 		by=cuniqueid, .SDcols=csumvar]
  }
  names(condf.sum) <- c(cuniqueid, csumvarnm)

  ## Merge to plt
  if (!noplt) {
    if (is.data.table(pltx)) {
      setkeyv(condf.sum, cuniqueid)
      setkeyv(pltx, puniqueid)
    }
    condf.sum <- merge(pltx, condf.sum, by.x=puniqueid, by.y=cuniqueid)
    if (NAto0)
      for (col in csumvarnm) set(condf.sum, which(is.na(condf.sum[[col]])), col, 0)
  }


  #### WRITE TO FILE 
  #############################################################
  if (savedata) {
    if (pltsp) {
      spExportSpatial(condf.sum, out_dsn=plt_dsn, out_layer=out_layer,
			outfolder=outfolder, outfn.date=outfn.date, 
			overwrite_layer=overwrite_layer, append_layer=append_layer)
    }
    datExportData(condf.sum, outfolder=outfolder, out_fmt=out_fmt, 
		out_dsn=out_dsn, out_layer=out_layer, outfn.date=outfn.date, 
		overwrite_layer=overwrite_layer, append_layer=append_layer)
    
  }  

  if (!returnDT) {     
    condf.sum <- setDF(condf.sum)
  }
  sumcondlst <- list(condsum=condf.sum, csumvarnm=csumvarnm)
  if (!is.null(cfilter))
    sumcondlst$cfilter <- cfilter

  return(sumcondlst)
} 
