datSumCond <- function(cond=NULL, plt=NULL, plt_dsn=NULL, cuniqueid="PLT_CN", 
	puniqueid="CN", csumvar=NULL, csumvarnm=NULL, cfilter=NULL, savedata=FALSE, 
	outfolder=NULL, outfn=NULL, outfn.date=TRUE, overwrite=FALSE){
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
  setkeyv(condx, cuniqueid) 

  ## Check for CONDPROP_UNADJ
  if (!"CONDPROP_UNADJ" %in% condnmlst) stop("CONDPROP_UNADJ not in cond")


  ## Check plt table
  noplt <- TRUE
  pltshp <- FALSE
  plt <- FIESTA::pcheck.table(plt, gui=gui, caption="Plot table?")
  if (!is.null(plt)) {
    noplt <- FALSE

    ## Remove totally nonsampled plots
    if ("PLOT_STATUS_CD" %in% names(plt)) {
      if (3 %in% unique(plt[["PLOT_STATUS_CD"]]))
        warning(paste("There are", sum(plt[["PLOT_STATUS_CD"]] == 3), 
		"nonsampled plots"))
      plt <- plt[plt$PLOT_STATUS_CD != 3,]
    } 

    if (typeof(plt) == "S4") {
      pltshp <- TRUE
      pltx <- data.table(plt@data)
    } else {
      pltx <- plt
    }

    ## Check puniqueid
    pltnmlst <- names(pltx)
    nmlst <- names(pltx)
    puniqueid <- FIESTA::pcheck.varchar(var2check=puniqueid, varnm="puniqueid", 
		checklst=pltnmlst, caption="UniqueID variable - plt", 
		warn="puniqueid not in plot table", stopifnull=TRUE)
    setkeyv(pltx, puniqueid)

    ## Check that the values of tuniqueid in treex are all in puniqueid in pltx
    FIESTA::check.matchval(condx, pltx, cuniqueid, puniqueid)

    ## Check if class of cuniqueid matches class of puniqueid
    tabs <- FIESTA::check.matchclass(condx, pltx, cuniqueid, puniqueid)
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

  ### Check savedata 
  savedata <- FIESTA::pcheck.logical(savedata, "Save data tables?", "NO")

  ### Check overwrite
  overwrite <- FIESTA::pcheck.logical(overwrite, "Save data tables?", "NO")

  ## GET outfolder
  if (savedata) {
    outfolder <- FIESTA::pcheck.outfolder(outfolder, gui)

    ## outfn
    if (is.null(outfn) || gsub(" ", "", outfn) == "") 
      outfn <- paste("condsum", sep="_")
    #outfn.param <- paste(outfn, "param", sep="_")
  }


  ################################################################################  
  ### DO WORK
  ################################################################################  

  ## Filter cond
  cdat <- FIESTA::datFilter(x=condx, xfilter=cfilter, title.filter="tfilter",
			 stopifnull=TRUE, gui=gui)
  condf <- cdat$xf
  cfilter <- cdat$xfilter


  ## Multiply variables by CONDPROP_UNADJ
  csumvar2 <- csumvar[csumvar != "CONDPROP_UNADJ"] 
  #indx <- which(names(condf) %in% csumvar2)
  #for (i in indx) {set(condf, i=NULL, j=i, value=condf[[i]]*condf[["CONDPROP_UNADJ"]]) }
  newcols <- paste(csumvar2, "new", sep="_")
  condf[, (newcols) := lapply(.SD, function(x) x * condf[["CONDPROP_UNADJ"]]), .SDcols=csumvar2] 

  ## Sum variables to plot
  csumvar[csumvar != "CONDPROP_UNADJ"] <- newcols
  condf.sum <- condf[, lapply(.SD, sum, na.rm=TRUE), by=cuniqueid, .SDcols=csumvar] 
  names(condf.sum) <- c(cuniqueid, csumvarnm)

  ## MERGE to plt
  if (!noplt) {
    condf.sum <- condf.sum[pltx]
    setcolorder(condf.sum, c(cuniqueid, pltnmlst[pltnmlst != puniqueid], csumvarnm))
    condf.sum <- FIESTA::DT_NAto0(condf.sum, csumvarnm)
  }
 
  #### WRITE TO FILE 
  #############################################################
  if (savedata) {
    if (pltshp) {
      spcondf.sum <- sp::merge(plt[, puniqueid], condf.sum, by=puniqueid, all.x=TRUE)
      FIESTA::spExportShape(spcondf.sum, outshpnm=outfn, outfolder=outfolder, 
			outfn.date=outfn.date, overwrite=overwrite)
    } else {
      FIESTA::write2csv(condf.sum, outfolder=outfolder, outfilenm=outfn, 
		outfn.date=outfn.date, overwrite=overwrite)
    }
  }   
 
  sumcondlst <- list(condsum=condf.sum)
  if (pltshp) 
    sumcondlst$spcondsum <- methods::as(spcondf.sum, "SpatialPointsDataFrame")
  sumcondlst$cfilter <- cfilter

  return(sumcondlst)
} 
