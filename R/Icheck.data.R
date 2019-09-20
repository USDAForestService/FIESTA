check.data <- function(gui, esttype, module="GB", method="GREG", SApackage=NULL,
	tree=NULL, cond=NULL, plt=NULL, tuniqueid=NULL, cuniqueid=NULL, 
	condid="CONDID", puniqueid=NULL, unitvar=NULL, unitvar2=NULL, autocombine=NULL, 
	prednames=NULL, predfac=NULL, sumunits=FALSE, adjplot=FALSE, adjsamp=FALSE, 
	strata=FALSE, strvar=NULL, nonresp=FALSE, substrvar=NULL, landarea=NULL, ACI=FALSE, 
	nonsamp.filter=NULL, plt.filter=NULL, cond.filter=NULL, nullcheck=FALSE, 
	allin1=FALSE, estround=6, pseround=3, divideby=NULL, savedata=FALSE, 
	addtitle=TRUE, returntitle=TRUE, rawdata=FALSE, outfolder=NULL, pvars2keep=NULL, 
	cvars2keep=NULL){

  ###################################################################################
  ## DESCRIPTION: Checks data inputs 
  ## Define necessary plot and condition-level variables: 
  ## - plt domains to add to cond (pdoms2keep) - STATECD, UNITCD, COUNTYCD, 
  ##		INVYR, MEASYEAR, PLOT_STATUS_CD, RDDISTCD, WATERCD, ELEV, ELEV_PUBLIC, 
  ##		ECOSUBCD, CONGCD
  ## - plt (pvars2keep) - unitvars, auxvars
  ## - cond (cvars2keep) - CONDPROP_UNADJ, COND_STATUS_CD 
  ## Check esttype, module, method, adj
  ## - esttype in("TREE", "RATIO")
  ## - module in("GB", "MA", "SA")
  ## - if (module="MA") method in("HT", "PS", "GREG")
  ## - if (module="SA") SApackage <- c("JoSAE", "sae"); method <- c("unit", "area")
  ## Check logical parameters: sumunits, autocombine, strata, ACI, adjsamp, adjplot
  ## - If sumunits=TRUE, estimation units are summed to 1 estimate
  ## - If sumunits=TRUE and autocombine=TRUE, 
  ##		if < 2 plots, combine strata; if < 10 plots, combine estimation units
  ## - If strata, only 1 auxvar allowed
  ## - If ACI, add NF_PLOT_STATUS_CD to pvars2keep and NF_COND_STATUS_CD to cvars2keep
  ## - If adjsamp, adj="samp", nonsample adjustment factors calculated at strata level
  ## - If adjplot, adj="plot", nonsample adjustment factors calculated at plot level
  ## Check landarea ("FOREST", "ALL", "TIMBERLAND")
  ## Import and check cond and plt tables and check unique identifiers
  ## Check plt table (if NULL, plt=cond[, pvars2keep])
  ## - Check for NA values
  ## - Apply plt filter, if plt exists.
  ## - Check missing pdoms2keep variables
  ## - Check predfac variable(s) and strvar - for factor status
  ## Subset pltx with puniqueid and pvars2keep only 
  ## Create condx with cuniqueid, pdoms2keep and cvars2keep 
  ## - Check for missing NA values in pvars2keep variables in pltx
  ## - if unitvar=NULL, add ONEUNIT=1 to pltx
  ## - Check for missing pdoms2keep in condx 
  ## - If STATECD, PLOT_STATUS_CD, NF_PLOT_STATUS_CD (if ACI), INVYR, or DESIGNCD is missing, 
  ##		print messsages
  ## - If adjsamp and DESIGNCD != 1, stop
  ## - if unitvar=NULL, add ONEUNIT=1
  ## - Check inventory year for cycle length
  ## Check cond table
  ## - Check for condid in cond... if no condid, add CONDID=1
  ## - Check if 1 plot-condition per record in cond
  ## - If no CONDPROP_UNADJ, add CONDPROP_UNADJ=1 (100%)
  ## - If no COND_STATUS_CD and 1 record per plot use PLOT_STATUS_CD if in table.
  ## - IF ACI=FALSE, create ACI.filter="COND_STATUS_CD == 1"
  ## - Create landarea.filter
  ## Generate table of sampled/nonsampled plots and conditions
  ## Generate and apply nonsample filter for condx ("COND_STATUS_CD != 5")
  ## - If ACI, add "(is.na(NF_COND_STATUS_CD) | NF_COND_STATUS_CD != 5)" 
  ## Make sure plots in pltx to match condx plots
  ## Copy condx to condf and use condf for filtering
  ## Apply cond filters to condf: landarea.filter, ACI.filter, cond.filter
  ## Check tree table (if esttype = TREE/RATIO):
  ## - Define necessary tree-level variables (tvars2keep)
  ## - Import tree table and check unique identifier (tuniqueid)
  ## - Check for condid in tree... if no condid, add CONDID=1
  ## - Check if class of tuniqueid matches class of cuniqueid in cond
  ## - Check if all values of tree are in cond and subset rows to match cond
  ## - Check for missing tvars2keep and NA values in tvars2keep
  ## - Add necessary variables to cvars2keep depending on data in tree
  ##   If trees in subplot (TPA_UNADJ > 5 & < 10), add SUBPPROP_UNADJ to cvars2keep
  ##     If no SUBPPROP_UNADJ in cond, add a variable SUBPROP_UNADJ=1 (100%)
  ##   If trees in microplot (TPA_UNADJ > 50), add MICRPROP_UNADJ to cvars2keep 
  ##     If no MICRPROP_UNADJ in cond, add a variable MICRPROP_UNADJ=1 (100%)
  ##   If trees in macroplot (TPA_UNADJ > 0 & < 5), add MACRPROP_UNADJ to cvars2keep 
  ##     If no MACRPROP_UNADJ in cond, add a variable MACRPROP_UNADJ=1 (100%)
  ## - Check for missing cvars2keep and NA values in cvars2keep
  ## Subset columns of condx to include unique key and cvars2keep
  ## Check other table parameters: allin1, estround, pseround, divideby, 
  ##		savedata, addtitle, returntitle, rawdata, outfolder
  ## - If allin1=TRUE, puts estimate (% sample error) in each cell
  ## - If savedata=TRUE, saves tables to outfolder
  ## - If addtitle=TRUE, adds title(s) to saved tables
  ## - If returntitle, saves title(s) of tables
  ## - If rawdata=TRUE, generates and saves rawdata variables for estimates
  ## Return data and nosamp.filter
  ###################################################################################

  ## Set global variables
  CONDPROP_UNADJ=COND_STATUS_CD=CONDID=SUBPPROP_UNADJ=MICRPROP_UNADJ=MACRPROP_UNADJ=
	STATECD=PLOT_STATUS_CD=PSTATUSCD=cndnmlst=pltdomainlst=invyrs=PROP_BASIS=
	ACI.filter=landarea.filter=V1=ONEUNIT=plotsampcnt=nfplotsampcnt=condsampcnt=
	nonsamp.cfilter=nonsamp.pfilter=nonsamp.cfilter.ACI=nonsamp.pfilter.ACI=INVYR=
	NF_PLOT_STATUS_CD=NF_COND_STATUS_CD=TPA_UNADJ=methodlst <- NULL


  ###################################################################################
  ## Define necessary plot and condition level variables
  ###################################################################################
  pdoms2keep <- unique(c(pvars2keep, "STATECD", "UNITCD", "COUNTYCD", "INVYR", 
	"MEASYEAR", "PLOT_STATUS_CD", "PSTATUSCD", "RDDISTCD", "WATERCD", "ELEV", 
	"ELEV_PUBLIC", "ECOSUBCD", "CONGCD"))
  pvars2keep <- unique(c(unitvar, unitvar2))
  cvars2keep <- unique(c(cvars2keep, "CONDPROP_UNADJ", "COND_STATUS_CD"))


  ###################################################################################
  ## Check esttype, module, method, adj
  ###################################################################################

  ## Check esttype 
  ########################################################
  esttypelst <- c("AREA", "TREE", "RATIO")
  esttype <- FIESTA::pcheck.varchar(var2check=esttype, varnm="esttype", gui=gui, 
		checklst=esttypelst, caption="Estimation type", stopifnull=TRUE)

  ## Check estimator module 
  ########################################################
  modulelst <- c("GB", "MA", "SA")
  module <- FIESTA::pcheck.varchar(var2check=module, varnm="module", gui=gui, 
		checklst=modulelst, caption="FIESTA module", stopifnull=TRUE)

  ## Check method 
  ########################################################
  if (module %in% c("MA", "SA")) {
    if (module == "MA") {
      methodlst <- c("HT", "PS", "GREG")
    } else if (module == "SA") {
      SApackagelst <- c("JoSAE", "sae")
      SApackage <- FIESTA::pcheck.varchar(var2check=SApackage, varnm="SApackage", gui=gui, 
		checklst=SApackagelst, caption="SA package", multiple=FALSE, stopifnull=TRUE)
      methodlst <- c("unit", "area")
    } 
    method <- FIESTA::pcheck.varchar(var2check=method, varnm="method", gui=gui, 
		checklst=methodlst, caption="method", multiple=FALSE, stopifnull=TRUE)
    if (module == "MA" && method == "PS") strata <- TRUE

    if (module == "SA" && method == "unit" && is.null(plt)) 
      stop("must include pltmodel for unit-level estimates")
  }

  ## Check adjsamp
  ########################################################
  adjplot <- FIESTA::pcheck.logical(adjplot, varnm = "adjplot", 
        title = "Adjust plot for nonsampled?", first = "YES", 
        gui = gui, stopifnull = TRUE)
  adjsamp <- FIESTA::pcheck.logical(adjsamp, varnm = "adjsamp", 
        title = "Adjust area for nonsampled?", first = "YES", 
        gui = gui, stopifnull = TRUE)

  adj <- ifelse((!is.null(adjplot) && adjplot), "plot", 
			ifelse((!is.null(adjsamp) && adjsamp), "samp", "none"))


  ###################################################################################
  ## Check logical parameters: sumunits, strata, ACI, adjsamp, adjplot
  ###################################################################################
  
  ## Check sumunits 
  ########################################################
  sumunits <- FIESTA::pcheck.logical(sumunits, varnm="sumunits", 
		title="Sum estimation units?", first="YES", gui=gui, stopifnull=TRUE)

  ## Check autocombine
  ########################################################
  if (sumunits) {
    autocombine <- FIESTA::pcheck.logical(autocombine, varnm="autocombine", 
		title="Combine estimation units?", first="YES", gui=gui, stopifnull=TRUE)
  } else {
    autocombine <- FALSE
  }   

  ## Check adj
  ########################################################
  if (adj == "samp" && module == "SA")  {
    message("adj='samp' is currently invalid for SA module... adjusting for plot")
    adj <- "plot"
  }
  if (adj == "plot" && module == "GB") 
    message("adj='plot' is not typical for GA modules")
      

  ## Check strata, strvars
  ###################################################################################
  if (module == "GB" || (module == "MA" && method == "PS")) {
    strata <- FIESTA::pcheck.logical(strata, varnm="strata", 
		title="Post stratify?", first="YES", gui=gui, stopifnull=TRUE)
    if (strata) {
      if (is.null(strvar)) stop("must include strvar for post-strat estimates")
      if (length(strvar) > 1) stop("invalid strvar... only 1 variable allowed")
      pvars2keep <- c(pvars2keep, strvar)

      if (module == "MA" && method == "PS") {
        if (!strvar %in% predfac) predfac <- strvar
        prednames <- NULL 
      }

      ## Check nonresp
      nonresp <- FIESTA::pcheck.logical(nonresp, varnm="nonresp", 
		title="Post stratify?", first="YES", gui=gui)
      if (nonresp) pvars2keep <- c(pvars2keep, substrvar)
    } else {
      strvar <- NULL
    }
  } else if (module == "MA" && method == "HT") {
    if (!is.null(prednames)) prednames <- NULL
    if (!is.null(predfac)) predfac <- NULL
  } else {
    strvar <- NULL
    pvars2keep <- c(pvars2keep, prednames)
  }

  ## Check predfac
  ###################################################################################
  if (module == "SA" || (module == "MA" && method == "GREG"))  {
    if (!is.null(predfac) && !any(predfac %in% prednames)) {
      warning("invalid predfac... setting predfac to NULL")
      predfac <- NULL
    }
  } 
 
  ## Check ACI (if ACI=FALSE, need to filter COND_STATUS_CD == 1)
  ###################################################################################
  ACI <- FIESTA::pcheck.logical(ACI, varnm="ACI", title="ACI?", first="NO", gui=gui)
  if (ACI) {
    pdoms2keep <- unique(c(pdoms2keep, "NF_PLOT_STATUS_CD"))
    #cvars2keep <- unique(c(cvars2keep, "NF_COND_STATUS_CD"))
  }

  #############################################################################
  ## Check landarea 
  #############################################################################
  if (esttype == "RATIO") {
    landarealst <- c("FOREST", "TIMBERLAND")
  } else {
    landarealst <- c("FOREST", "ALL", "TIMBERLAND")
  }
  landarea <- FIESTA::pcheck.varchar(var2check=landarea, varnm="landarea", gui=gui,
	checklst=landarealst, caption="Sample land area?")

 
  ###################################################################################
  ## Import plt and cond table and check unique identifiers
  ###################################################################################
  condx <- FIESTA::pcheck.table(cond, gui=gui, tabnm="cond", caption="Cond table?", 
		nullcheck=nullcheck)
  pltx <- FIESTA::pcheck.table(plt, gui=gui, tabnm="plt", caption="plt table?", 
		nullcheck=nullcheck, returnshp=FALSE)
  if (is.null(condx) && is.null(pltx)) stop("must include plt or cond table")

  if (!is.null(condx)) {
    if (isS4(condx)) condx <- data.table(condx@data)
    cuniqueid <- pcheck.varchar(var2check=cuniqueid, varnm="cuniqueid", gui=gui, 
		checklst=names(condx), caption="UniqueID variable - cond", 
		warn=paste(cuniqueid, "not in cond table"), stopifnull=TRUE)
    setkeyv(condx, cuniqueid)

    ## Check for NA values in necessary variables in cond table
    condx.na <- sum(is.na(condx[, cuniqueid, with=FALSE]))
    if (condx.na > 0) stop("NA values in ", cuniqueid)
  }
 
  if (!is.null(pltx)) {
    pltnmlst <- names(plt)
    if (isS4(pltx)) pltx <- data.table(pltx@data)
    puniqueid <- pcheck.varchar(var2check=puniqueid, varnm="puniqueid", gui=gui, 
		checklst=names(pltx), caption="UniqueID variable - plt", 
		warn=paste(puniqueid, "not in plt table"), stopifnull=FALSE)
    if (is.null(puniqueid))
      if ("PLT_CN" %in% names(pltx)) puniqueid <- "PLT_CN"
    if (length(unique(pltx[[puniqueid]])) < nrow(pltx)) 
      warning("plt records are not unique")

    ## Check for NA values in necessary variables in plt table
    pltx.na <- sum(is.na(pltx[, puniqueid, with=FALSE]))
    if (pltx.na > 0) stop("NA values in ", puniqueid)

    ## Check for unique uniqueids
    if (length(unique(pltx[[puniqueid]])) != nrow(pltx))
      stop("uniqueid in plt table is not unique")
    setkeyv(pltx, puniqueid)

    ## Apply plt.filter to plt table
    ###########################################################################
    pltf <- datFilter(x=pltx, xfilter=plt.filter, title.filter="plt filter?",
		vardelete=names(pltx)[!names(pltx) %in% pltdomainlst], gui=gui,
		filternm="plt.filter", stopifnull=TRUE)
    pltx <- pltf$xf
    if (is.null(pltx)) stop("plt.filter is invalid")
    plt.filter <- pltx$xfilter
 
    ## Create pltx with puniqueid and pvars2keep only 
    ## Create condx with cuniqueid, pdoms2keep and cvars2keep 
    ###########################################################################
    if (is.null(condx)) {
      cvars <- pltnmlst[which(!pltnmlst %in% pvars2keep)]
      condx <- pltx[, unique(c(puniqueid, cvars)), with=FALSE]
      pltx <- pltx[, unique(c(puniqueid, pvars2keep)), with=FALSE]
      cuniqueid <- puniqueid
      condnmlst <- names(condx)
    } else {
      condnmlst <- names(condx)

      ## Check if class of puniqueid in pltx matches class of puniqueid in condx
      tabs <- FIESTA::check.matchclass(condx, pltx, cuniqueid, puniqueid)
      condx <- tabs$tab1
      pltx <- tabs$tab2

      ## Check for matching unique identifiers of condx and pltx
      condx <- check.matchval(condx, pltx, cuniqueid, puniqueid, tab1txt="cond",
			tab2txt="plt", subsetrows=TRUE)

      if (any(pvars2keep %in% names(condx)) || any(c(pdoms2keep, cvars2keep) %in% names(pltx))) {
        ## Merge cond and plt tables
        pltnmlst <- names(pltx)[!names(pltx) %in% names(condx)] 
        pltnmlst <- unique(c(key(pltx), pltnmlst))
        pltcondx <- merge(pltx[, pltnmlst, with=FALSE], condx, by.x=key(pltx),
 			by.y=key(condx))
        pltcols <- pvars2keep[pvars2keep %in% names(pltcondx)]
        condcols <- unique(c(cvars2keep, condnmlst))
        condcols <- condcols[condcols %in% names(pltcondx)]
        pltx <- unique(pltcondx[, unique(c(puniqueid, pltcols)), with=FALSE])
        pdoms2keep <- pdoms2keep[pdoms2keep %in% names(pltcondx)]
        condx <- pltcondx[, unique(c(puniqueid, pdoms2keep, condcols)), with=FALSE]
        setnames(condx, puniqueid, cuniqueid)
        condnmlst <- names(condx)
      } 
    }
  } else {  ## pltx is NULL
    condnmlst <- names(condx)
    pvarsmiss <- pvars2keep[which(!pvars2keep %in% condnmlst)]
    if (length(pvarsmiss) > 0) 
      stop("missing variables: ", paste(pvarsmiss, collapse=", "))
    pvars <- condnmlst[which(condnmlst %in% pvars2keep)]
    if (length(pvars) > 0) {
      pltx <- unique(condx[, unique(c(cuniqueid, pvars)), with=FALSE])
      condx[, (pvars) := NULL]
    } else {
      pltx <- unique(condx[, unique(c(cuniqueid, pvars)), with=FALSE])
    }
    puniqueid <- cuniqueid

    message("assuming all conditions are located within estimation unit boundary")
  }

  ## Check for NA values in pvars2keep variables in pltx
  pltx.na <- sapply(pvars2keep, function(x, pltx){ sum(is.na(pltx[,x, with=FALSE])) }, pltx)
  if (any(pltx.na > 0))
    stop(paste(pltx.na[pltx.na > 0], "NA values in variable:", 
		paste(names(pltx.na[pltx.na > 0]), collapse=", ")))

  ## Add unitvar to plt table if NULL
  if (is.null(unitvar)) {
    pltx[, ONEUNIT := 1] 
    unitvar <- "ONEUNIT"
  }

  ## Check missing pdoms2keep variables in condx
  ###########################################################################
  pltnmlst <- names(pltx) 
  pmissvars <- pdoms2keep[which(!pdoms2keep %in% condnmlst)]
  if (length(pmissvars) > 0) {
    if ("STATECD" %in% pmissvars) 
      message("STATECD not in dataset.. assuming 1 state in dataset")
    if ("PLOT_STATUS_CD" %in% pmissvars) {
      if ("PSTATUSCD" %in% condnmlst) {
        pvars2keep[pvars2keep == "PLOT_STATUS_CD"] <- "PSTATUSCD"
      } else {
        if (nonresp) stop("PLOT_STATUS_CD must be in dataset if nonresp=TRUE")
        message("PLOT_STATUS_CD not in dataset.. assuming all plots are at least ", 
			"partially sampled")
      }
    }
    if ("NF_PLOT_STATUS_CD" %in% pmissvars) {
      if ("PSTATUSNF" %in% condnmlst) {
        pvars2keep[pvars2keep == "NF_PLOT_STATUS_CD"] <- "PSTATUSNF"
      } else {
        message("NF_PLOT_STATUS_CD not in dataset.. assuming all nonforest plots ", 
			"are at least partially sampled")
      }
    }
    if ("INVYR" %in% pmissvars) 
      message("INVYR not in dataset.. assuming inventory years do not span more ",
		"than 1 cycle of measurements")
    if (!"DESIGNCD" %in% names(condx)) {
      message("DESIGNCD not in dataset.. assuming DESIGNCD = 1 (annual inventory)")
      designcd <- 1
    } else {
      ##################################################################################
      ## NOTE: If adj='samp', make sure dataset is annual inventory only (DESIGNCD=1). 
      ## Only adjust plots when DESIGNCD=1. Cannot have more than 1 DESIGNCD.
      ##################################################################################
      designcd <- unique(condx[["DESIGNCD"]])
      if (length(designcd) != 1) {
        stop("more than 1 plot design, calculate separate estimates by design")
      } else if (adj == "samp" && designcd != 1) {
        stop("samp adjustment for trees is only for designcd = 1 (annual inventory)") 
      }
    }

    pmissvars <- pmissvars[which(!pmissvars %in% pltnmlst)]
    if (any(prednames %in% pmissvars)) {
      prednames[which(!prednames %in% pmissvars)]
      stop("predname not in tables: ", paste(prednames, collapse=", "))
    }
    unitvars <- c(unitvar, unitvar2)
    if (any(unitvars %in% pmissvars)) {
      unitvars[which(!unitvars %in% pmissvars)]
      stop("unitvar not in tables: ", paste(unitvars, collapse=", "))
    }
  }
  pdoms2keep <- unique(pdoms2keep[which(!pdoms2keep %in% pmissvars)])
  

  ## Check predfac variable(s) and strvar - for factor status
  ###########################################################################
  if (!is.null(predfac) && !is.character(predfac)) stop("invalid predfac... must be character string")
  if (!is.null(c(predfac, strvar))) {
    notfac <- predfac[!pltx[, lapply(.SD, is.factor), .SDcols=c(predfac, strvar)][[1]]]
    if (length(notfac) > 0) {
      #pltx[, (predfac) := lapply(.SD, as.factor), .SDcols=notfac]

      for (fac in notfac) {
        fac.levels <- sort(unique(pltx[[fac]]))
        pltx[[fac]] <- factor(pltx[[fac]], levels=fac.levels) 
      }
    }
  }
    

  ## Check inventory year for cycle length
  ############################################################
  states <- NULL
  invyrs <- NULL
  if ("STATECD" %in% names(condx)) {
    stcds <- unique(condx[["STATECD"]])
    states <- FIESTA::pcheck.states(stcds)
    if ("INVYR" %in% names(condx)) {
      invyrtab <- unique(condx[, c("STATECD", "INVYR")])
      invyrs <- as.list(by(invyrtab$INVYR, invyrtab$STATECD, I))
      names(invyrs) <- FIESTA::pcheck.states(names(invyrs))
    }
    invyrlength <- lapply(invyrs, length)  
    for (stcd in stcds) {
      state <- FIESTA::pcheck.states(stcd)
      rs <- unique(FIESTA::ref_statecd[FIESTA::ref_statecd$VALUE %in% stcd, "RS"])
      cycleyrs <- ifelse(rs == "SRS", 7, ifelse(rs == "NCRS", 6, 10)) 
      stinvyrlength <- invyrlength[[state]]
      if (stinvyrlength > cycleyrs) {
        message("check dataset, overlapping plots (cycles)") 
        nbr <- condx[, list(NBRPLOT=uniqueN(get(cuniqueid))), by=INVYR]
        nbr <- nbr[order(nbr[["INVYR"]]),]
        print(nbr)
      }
    }
  } 

  ## Generate table of sampled/nonsampled plots (if ACI, nonforest status included)
  ######################################################################################
  if (any(c("PLOT_STATUS_CD", "PSTATUSCD") %in% condnmlst)) {
    if ("PSTATUSCD" %in% condnmlst) 
      names(condx)[names(condx) == "PSTATUSCD"] <- "PLOT_STATUS_CD"
    ref_plot_status_cd <- FIESTA::ref_codes[FIESTA::ref_codes$VARIABLE == "PLOT_STATUS_CD", ]
    plotsampcnt <- condx[, list(NBRPLOT=uniqueN(get(cuniqueid))), by=PLOT_STATUS_CD]
    plotsampcnt <- 
	cbind(PLOT_STATUS_NM=ref_plot_status_cd[match(plotsampcnt$PLOT_STATUS_CD, 
	ref_plot_status_cd$VALUE), "MEANING"], plotsampcnt)
    setkey(plotsampcnt, PLOT_STATUS_CD)
  }
  if (ACI) {
    if (any(c("NF_PLOT_STATUS_CD", "PSTATUSNF") %in% condnmlst)) {
      if ("PSTATUSNF" %in% pltnmlst) 
        names(condx)[names(condx) == "PSTATUSNF"] <- "NF_PLOT_STATUS_CD"
      ref_nf_plot_status_cd <- FIESTA::ref_codes[FIESTA::ref_codes$VARIABLE == "NF_PLOT_STATUS_CD", ]
      nfplotsampcnt <- condx[, list(NBRPLOT=uniqueN(get(cuniqueid))), by=NF_PLOT_STATUS_CD]
      nfplotsampcnt <- 
	 	cbind(NF_PLOT_STATUS_NM=ref_nf_plot_status_cd[match(nfplotsampcnt$NF_PLOT_STATUS_CD, 
		ref_nf_plot_status_cd$VALUE), "MEANING"], nfplotsampcnt)
      setkey(nfplotsampcnt, NF_PLOT_STATUS_CD)
      nfplotsampcnt <- nfplotsampcnt[!is.na(NF_PLOT_STATUS_CD), ]
    }
    if (!is.null(plotsampcnt)) {
      plotsampcnt <- rbindlist(list(plotsampcnt, nfplotsampcnt), use.names=FALSE)
    } else {
      plotsampcnt <- nfplotsampcnt
    }
  }

  ###################################################################################
  ## Check cond table
  ###################################################################################
  condnmlst <- names(condx)
  if (is.null(condid) || (!is.null(condid) && !condid %in% names(condx))) {
    ## If condid = NULL, add a variable CONDID=1 to cond
    if (nrow(condx) == length(unique(condx[[cuniqueid]]))) {
      message("assuming one condition, adding CONDID=1 for 1 condition per plot")
      condx[, CONDID := 1]
      condid <- "CONDID"
    } else { 
      stop("only 1 record for each cuniqueid allowed")
    }
  } else {
    ## Check for NA values in condid
    condx.na <- sum(is.na(condx[, cuniqueid, with=FALSE]))
    if (condx.na > 0) stop("NA values in ", cuniqueid)
  }
  setkeyv(condx, c(cuniqueid, condid))

  ## Check if 1 plot-condition per record in cond
  ######################################################
  condx.dupid <- condx[duplicated(condx, by=c(cuniqueid, condid))][[cuniqueid]]
  if (length(condx.dupid) > 0) {
    msg <- paste("check cuniqueid/condid... duplicate records")
    if (length(condx.dupid) < 20) print(condx.dupid)
    stop(msg)
  }

  #############################################################################
  ## Check for necessary cond variables in cond table 
  #############################################################################

  ## If CONDPROP_UNADJ not in cond table and only 1 condition per plot, 
  ## 	add CONDPROP_UNADJ and set = 1 (100 percent)
  if (!"CONDPROP_UNADJ" %in% condnmlst) {
    ## If only 1 condition, check CONDPROP_UNADJ and COND_STATUS_CD
    if (nrow(condx) == length(unique(condx[[cuniqueid]]))) {
      message("CONDPROP_UNADJ not in dataset.. assuming CONDPROP_UNADJ = 1") 
      condx[, CONDPROP_UNADJ := 1]  
    }
  }

  ## Check for COND_STATUS_CD and create ACI filter
  #############################################################################
  if (!"COND_STATUS_CD" %in% condnmlst) {
    if (length(unique(condx[[cuniqueid]])) == nrow(condx) && "PLOT_STATUS_CD" %in% condnmlst) {
      message("COND_STATUS_CD not in dataset.. using PLOT_STATUS_CD for COND_STATUS_CD")
      condx[, COND_STATUS_CD := PLOT_STATUS_CD]
      condx[COND_STATUS_CD == 3, COND_STATUS_CD := 5]
    } else {
      if (landarea == "FOREST") 
        stop("COND_STATUS_CD not in dataset.. must include for landarea=FOREST")      
      message("COND_STATUS_CD not in dataset.. assuming all sampled conditions")
      cvars2keep <- cvars2keep[cvars2keep != "COND_STATUS_CD"]
    }
  } else {
    if (!ACI) ACI.filter <- "COND_STATUS_CD == 1"
  }

  if (ACI && !"NF_COND_STATUS_CD" %in% condnmlst)
    message("NF_COND_STATUS_CD not in dataset.. assuming all sampled nonforest conditions")


  ## Create landarea.filter 
  #############################################################################
  if (landarea == "FOREST") {
    landarea.filter <- "COND_STATUS_CD == 1"
  } else if (landarea == "TIMBERLAND") {
    landcols <- c("SITECLCD", "RESERVCD")
    if (any(!landcols %in% condnmlst)) {
      landcols.miss <- landcols[which(!landcols %in% condnmlst)]
      stop(paste("missing variables for TIMBERLAND landarea filter:", 
		paste(landcols.miss, collapse=", ")))
    } 
    landarea.filter <- "SITECLCD %in% c(1:6) & RESERVCD == 0"
  }    

  #############################################################################
  ## Generate table of sampled/nonsampled conditions from condx
  #############################################################################
  if ("COND_STATUS_CD" %in% names(condx)) {
    condsampcnt <- condx[, list(NBRCOND=.N), by=COND_STATUS_CD]
    ref_cond_status_cd <- FIESTA::ref_codes[FIESTA::ref_codes$VARIABLE == "COND_STATUS_CD", ]
 
    condsampcnt <- 
	cbind(COND_STATUS_NM=ref_cond_status_cd[match(condsampcnt$COND_STATUS_CD, 
	ref_cond_status_cd$VALUE), "MEANING"], condsampcnt)
    setkey(condsampcnt, COND_STATUS_CD)
  } 
  if (ACI && "NF_COND_STATUS_CD" %in% condnmlst) {
    ref_nf_cond_status_cd <- FIESTA::ref_codes[FIESTA::ref_codes$VARIABLE == "NF_COND_STATUS_CD", ]
    nfcondsampcnt <- condx[, list(NBRCOND=.N), by=NF_COND_STATUS_CD]
    nfcondsampcnt <- 
	 	cbind(NF_COND_STATUS_NM=ref_nf_cond_status_cd[match(nfcondsampcnt$NF_COND_STATUS_CD, 
		ref_nf_cond_status_cd$VALUE), "MEANING"], nfcondsampcnt)
    setkey(nfcondsampcnt, NF_COND_STATUS_CD)
    nfcondsampcnt <- nfcondsampcnt[!is.na(NF_COND_STATUS_CD), ]
    condsampcnt <- rbindlist(list(condsampcnt, nfcondsampcnt), use.names=FALSE)
  }

  #############################################################################
  ## Generate and apply nonsamp.filter
  #############################################################################
  if (is.null(nonsamp.filter) || nonsamp.filter == "") {
    if ("COND_STATUS_CD" %in% names(condx)) 
      nonsamp.filter <- "COND_STATUS_CD != 5"
    if (ACI) {
      if ("NF_COND_STATUS_CD" %in% names(condx)) {
        nonsamp.filter.ACI <- "(is.na(NF_COND_STATUS_CD) | NF_COND_STATUS_CD != 5)"
        if (!is.null(nonsamp.filter)) 
          nonsamp.filter <- paste(nonsamp.filter, "&", nonsamp.filter.ACI)
      }
    }
  } 
  if (!is.null(nonsamp.filter) && nonsamp.filter != "NONE") {
    ## Apply nonsamp.filter
    condx <- datFilter(x=condx, xfilter=nonsamp.filter, 
		title.filter="nonsamp.filter", gui=gui)$xf
    if (is.null(condx)) {
      message(paste(nonsamp.filter, "removed all records"))
      return(NULL)
    }
  }

  ## Make sure plots in pltx to match condx plots
  #############################################################################
  pltx <- check.matchval(pltx, condx, puniqueid, cuniqueid, 
			tab1txt="plt", tab2txt="cond", subsetrows=TRUE)

 

  ###################################################################################
  ## Apply cond filters
  ###################################################################################
  ## Make a copy of condx for filtering (condf)
  condf <- condx		## SETS cond filtered to cond
  condf$TOTAL <- 1	## ADDS a column to cond filtered for the total estimate

  ## Apply landarea.filter to condf
  condf <- FIESTA::datFilter(x=condf, xfilter=landarea.filter, 
		title.filter="landarea filter", gui=gui, stopifnull=FALSE)$xf
  if (is.null(condf)) {
    message(paste(landarea.filter, "removed all records"))
    return(NULL)
  }
  ## Apply cond.filter to condf
  condf <- datFilter(x=condf, xfilter=cond.filter, 
		title.filter="cond filter", gui=gui, stopifnull=FALSE)$xf
  if (is.null(condf)) {
    message(paste(cond.filter, "removed all records"))
    return(NULL)
  }
  ## Apply ACI.filter to condf
  if (landarea != "ALL") {
    condf <- FIESTA::datFilter(x=condf, xfilter=ACI.filter, 
			title.filter="ACI.filter", gui=gui, stopifnull=FALSE)$xf
    if (is.null(condf)) {
      message(paste(ACI.filter, "removed all records"))
      return(NULL)
    }
  }
  
  ###################################################################################
  ## Import and check tree data table
  ###################################################################################
  if (esttype %in% c("TREE", "RATIO")) {
    ## Define necessary variable for tree table
    tvars2keep <- "TPA_UNADJ"
    treex <- FIESTA::pcheck.table(tree, gui=gui, tabnm="tree", caption="Tree table?", 
		nullcheck=nullcheck, stopifnull=TRUE)
    treenmlst <- names(treex)

    ## Check unique identifiers
    if (sumunits & "PLT_CN" %in% treenmlst) tuniqueid <- "PLT_CN" 
    tuniqueid <- FIESTA::pcheck.varchar(var2check=tuniqueid, varnm="tuniqueid", gui=gui, 
		checklst=treenmlst, caption="UniqueID variable - tree", 
		warn=paste(tuniqueid, "not in tree table"), stopifnull=TRUE)

    ## Check for NA values in necessary variables in tree table
    treex.na <- sum(is.na(treex[, tuniqueid, with=FALSE]))
    if (treex.na > 0) stop("NA values in ", tuniqueid)

    if (tuniqueid %in% condnmlst) {
      idplace <- which(condnmlst %in% tuniqueid)
      if (idplace != 1) { 
	  condnmlst <- c(tuniqueid, condnmlst) 
	  condnmlst <- condnmlst[-(idplace + 1)] 
      }
    } 

    ## Check for condid in tree
    if (!condid %in% names(treex)) {
      if (nrow(treex) == length(unique(treex[[tuniqueid]]))) {
        treex[, CONDID := 1]
      } else { 
        stop("only 1 record for each tuniqueid allowed")
      }
    } else {
      ## Check for NA values in condid
      treex.na <- sum(is.na(treex[, tuniqueid, with=FALSE]))
      if (treex.na > 0) stop("NA values in ", tuniqueid)
    }
    setkeyv(treex, c(tuniqueid, condid))

    ## Check if class of tuniqueid in treex matches class of cuniqueid in condx
    tabs <- FIESTA::check.matchclass(condf, treex, cuniqueid, tuniqueid)
    condf <- tabs$tab1
    treex <- tabs$tab2

    ## Check that the values of tuniqueid in treex are all in cuniqueid in condf
    treex <- check.matchval(treex, condf, tuniqueid, cuniqueid, tab1txt="tree", 
		tab2txt="cond", subsetrows=TRUE)

    ## Check for missing tvars2keep 
    tmissvars <- tvars2keep[which(!tvars2keep %in% treenmlst)]
    if (length(tmissvars) > 0)
      stop("missing necessary variables from tree: ", paste(tmissvars, collapse=", "))

    ## Check for NA values in tvars2keep variables
    treex.na <- sapply(c(tuniqueid, condid, tvars2keep), 
		function(x, treex){ sum(is.na(treex[,x, with=FALSE])) }, treex)
    if (any(treex.na) > 0) 
      stop(treex.na[treex.na > 0], " NA values in variable: ", 
		paste(names(treex.na[treex.na > 0]), collapse=", "))

    ## Add necessary variables to cvars2keep depending on data in tree
    ###################################################################
    if (adj != "NONE") 
      ## Check for condition proportion variables
      cvars2keep <- unique(c(cvars2keep, check.PROP(treex, condx, checkNA=FALSE)))
  }  
 
  #############################################################################
  ## Check for necessary cond variables in condx and subset condx
  #############################################################################

  ## Check for missing cvars2keep and NA values in cvars2keep
  cmissvars <- cvars2keep[which(!cvars2keep %in% names(condx))]
  if (length(cmissvars) > 0)
    stop("missing necessary variables in cond: ", paste(cmissvars, collapse=", "))

  ## Check for NA values in necessary variables in cond table
  condx.na <- sapply(cvars2keep, function(x, condx){ sum(is.na(condx[,x, with=FALSE])) }, condx)
  if (any(condx.na) > 0) 
    warning(condx.na[condx.na > 0], " NA values in variable: ", 
		paste(names(condx.na[condx.na > 0]), collapse=", "))

  ## Subset columns of condx to include unique key and cvars2keep
  condx <- condx[, c(key(condx), cvars2keep), with=FALSE]

  #####################################################################################
  ### Check other table parameters
  #####################################################################################

  ## Check allin1
  ########################################################
  allin1 <- FIESTA::pcheck.logical(allin1, varnm="allin1", 
		title="All 1 table - Est (%error)?", first="NO", gui=gui)

  ## Check divideby
  ########################################################
  dividebylst <- c("hundred", "thousand", "million")
  if (!is.null(divideby) || gui)
    divideby <- FIESTA::pcheck.varchar(var2check=divideby, varnm="divideby", 
		gui=gui, checklst=dividebylst, caption="Divide estimates?")

  ### Check savedata 
  savedata <- FIESTA::pcheck.logical(savedata, varnm="savedata", 
		title="Save data tables?", first="YES", gui=gui, stopifnull=TRUE)

  ### Check addtitle 
  addtitle <- FIESTA::pcheck.logical(addtitle, varnm="addtitle", 
		title="Add title to output?", first="YES", gui=gui, stopifnull=TRUE)

  ### Check returntitle 
  returntitle <- FIESTA::pcheck.logical(returntitle, varnm="returntitle", 
		title="Save output titles?", first="YES", gui=gui, stopifnull=TRUE)

  ## Check rawtable
  ########################################################
  rawdata <- FIESTA::pcheck.logical(rawdata, varnm="rawdata", title="Output raw data?", 
		first="NO", gui=gui, stopifnull=TRUE)

  ## Check outfolder 
  ########################################################
  if (savedata) {
    outfolder <- FIESTA::pcheck.outfolder(outfolder, gui)
    if (rawdata && !file.exists(paste(outfolder, "rawdata", sep="/"))) 
      dir.create(paste(outfolder, "rawdata", sep="/"))
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
  returnlst <- list(condx=condx, condf=condf, pltx=pltx, cuniqueid=cuniqueid, 
	condid=condid, puniqueid=puniqueid, sumunits=sumunits, unitvar=unitvar, 
	unitvar2=unitvar2, autocombine=autocombine, prednames=prednames, predfac=predfac,
	adjplot=adjplot, adj=adj, strata=strata, strvar=strvar, nonresp=nonresp, 
	substrvar=substrvar, plotsampcnt=plotsampcnt, condsampcnt=condsampcnt, 
	nonsamp.filter=nonsamp.filter, states=states, invyrs=unlist(invyrs), allin1=allin1, 
	estround=estround, pseround=pseround, divideby=divideby, savedata=savedata, 
	addtitle=addtitle, returntitle=returntitle, rawdata=rawdata, outfolder=outfolder,
	estround=estround, pseround=pseround, landarea=landarea)

  if (ACI) returnlst$nfplotsampcnt <- nfplotsampcnt
  if (esttype %in% c("TREE", "RATIO")) {
    treelst <- list(treef=treex, tuniqueid=tuniqueid)
    returnlst <- append(treelst, returnlst) }
  if (module == "SA") returnlst$SApackage <- SApackage
  if (module %in% c("MA", "SA")) returnlst$method <- method
  
  return(returnlst)
}
