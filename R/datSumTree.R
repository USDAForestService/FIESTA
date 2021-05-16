datSumTree <- function(tree=NULL, seed=NULL, cond=NULL, plt=NULL, plt_dsn=NULL, 
	tuniqueid="PLT_CN", cuniqueid="PLT_CN", puniqueid="CN", bycond=FALSE, 
	condid="CONDID", bysubp=FALSE, subpid="SUBP", tsumvarlst=NULL, 
	tsumvarnmlst=NULL, TPA=TRUE, tfun=sum, ACI=FALSE, tfilter=NULL, 
	addseed=FALSE, lbs2tons=TRUE, metric=FALSE, getadjplot=FALSE, adjtree=FALSE, 
	adjTPA=1, NAto0=FALSE, savedata=FALSE, outfolder=NULL, out_fmt="csv", 
	out_dsn=NULL, out_layer=NULL, outfn.pre=NULL, layer.pre=NULL, outfn.date=TRUE, 
	overwrite_dsn=FALSE, overwrite_layer=FALSE, append_layer=FALSE, tround=16, 
	checkNA=FALSE, returnDT=TRUE){
  ####################################################################################
  ## DESCRIPTION: Aggregates tree variable(s) to plot(/cond)-level, 
  ##        using specified tree filters (ex. live trees only)
  ####################################################################################
 
  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- ifelse(nargs() == 0, TRUE, FALSE)

  ## Set global variables  
  COND_STATUS_CD=tadjfac=PLOT_STATUS_CD=COUNT=plts=SUBP=NF_COND_STATUS_CD=
	seedf=TREECOUNT_CALC=estunits <- NULL


  ## If gui.. set variables to NULL
  if (gui) ACI=bycond=tuniqueid=puniqueid=cuniqueid=TPA=tfun=adjtree=adjsamp=
	savedata=outfolder <- NULL
  checkNApvars <- {}
  checkNAcvars <- {}
  checkNAtvars <- {}
  seedonly=parameters <- FALSE
  ref_estvar <- FIESTA::ref_estvar

  ## SET OPTIONS
  options.old <- options()
  options(scipen=8) # bias against scientific notation
  on.exit(options(options.old), add=TRUE) 

  ## SET VARIABLE LISTS
  biovars <- c("DRYBIO_AG", "DRYBIO_BG", "DRYBIO_WDLD_SPP", "DRYBIO_SAPLING",
 	"DRYBIO_STUMP", "DRYBIO_TOP", "DRYBIO_BOLE", "DRYBIOT", "DRYBIOM", "DRYBIOTB",
 	"JBIOTOT")
  carbvars <- c("CARBON_BG", "CARBON_AG")

  ## SET VARIABLES TO CONVERT (from pounds to short tons.. * 0.0005)
  vars2convert <- c(biovars, carbvars, paste(biovars, "TPA", sep="_"), 
	paste(carbvars, "TPA", sep="_"))

  growvars <- c("TPAGROW_UNADJ", "GROWCFGS", "GROWBFSL", "GROWCFAL", "FGROWCFGS", 
	"FGROWBFSL", "FGROWCFAL")
  mortvars <- c("TPAMORT_UNADJ", "MORTCFGS", "MORTBFSL", "MORTCFAL", "FMORTCFGS", 
	"FMORTBFSL", "FMORTCFAL")
  remvars <- c("TPAREMV_UNADJ", "REMVCFGS", "REMVBFSL", "REMVCFAL", "FREMVCFGS", 
	"FREMVBFSL", "FREMVCFAL")
  tpavars <- c("TPA_UNADJ", "TPAMORT_UNADJ", "TPAGROW_UNADJ", "TPAREMV_UNADJ")
  propvar <- "CONDPROP_UNADJ"


  ##################################################################
  ## CHECK INPUT PARAMETERS
  ##################################################################
  noplt=nocond <- TRUE
  pltsp <- FALSE


  ## Check tree
  treex <- FIESTA::pcheck.table(tree, gui=gui, tabnm="tree", caption="Tree table?")

  ## Check seed 
  seedx <- FIESTA::pcheck.table(seed, gui=gui, tabnm="seed", caption="Seed table?")

  ## Check cond
  condx <- FIESTA::pcheck.table(cond, tabnm="cond", gui=gui, caption="Condition table?")

  ## Check addseed
  addseed <- FIESTA::pcheck.logical(addseed, varnm="addseed", title="Add seeds?", 
		first="NO", gui=gui)

  if (is.null(treex) && is.null(seedx)) {
    stop("must include tree and/or seed table")
  }
  if (addseed && is.null(seedx)) {
    stop("if addseed=TRUE, must include seed table")
  }
  if (addseed && is.null(treex)) {
    addseed <- FALSE
  }
  if (is.null(treex) && !is.null(seedx)) {
    addseed <- FALSE
    seedonly <- TRUE
    treex <- seedx
  }

  ## Check bycond
  ###################################################################################
  bycond <- FIESTA::pcheck.logical(bycond, varnm="bycond", title="By condition?", 
		first="YES", gui=gui, stopifnull=TRUE)

  ## Check bysubp
  ###################################################################################
  bysubp <- FIESTA::pcheck.logical(bysubp, varnm="bysubp", title="By subplot?", 
		first="YES", gui=gui, stopifnull=TRUE)

  ## Check checkNA
  ###################################################################################
  NAto0 <- FIESTA::pcheck.logical(NAto0, varnm="NAto0", title="Convert NA to 0?", 
		first="YES", gui=gui)
  if (is.null(NAto0)) NAto0 <- FALSE

  ## Check checkNA
  ###################################################################################
  checkNA <- FIESTA::pcheck.logical(checkNA, varnm="checkNA", title="Check NA values?", 
		first="YES", gui=gui)
  if (is.null(checkNA)) checkNA <- FALSE


  ## Check unique identifiers, set keys, and matching values/classes
  ###################################################################################

  ## Check tuniqueid
  tuniqueid <- FIESTA::pcheck.varchar(var2check=tuniqueid, varnm="tuniqueid", 	
		checklst=names(treex), caption="UniqueID variable - tree", 
		warn=paste(tuniqueid, "not in tree table"))
  setkeyv(treex, tuniqueid)
  checkNAtvars <- c(checkNAtvars, tuniqueid)
  setkeyv(treex, tuniqueid)

  if (addseed) {
    if (!tuniqueid %in% names(seedx)) {
      stop("must included tree uniqueid in seed table: ", tuniqueid)
    }
    setkeyv(seedx, tuniqueid)
  }

  ## Check unique ids and set keys
  if (bycond) {
    pltsp <- FALSE
    noplt <- TRUE

    ## Check condid in tree table and setkey to tuniqueid, condid
    condid <- FIESTA::pcheck.varchar(var2check=condid, varnm="condid", 
		checklst=names(treex), caption="cond ID - tree", 
		warn=paste(condid, "not in tree table"))
    if (is.null(condid)) {
      message("assuming all 1 condition plots")
      treex$CONDID <- 1
      condid <- "CONDID"

      if (addseed) {
        if (!condid %in% names(seedx)) {
          seedx$CONDID <- 1
        } else {
          stop(condid, "in seed but not in cond")
        }
      }  
    } else {
      if (addseed && !condid %in% names(seedx)) {
        stop(condid, "not in seed") 
      }
    }
    tsumuniqueid <- c(tuniqueid, condid)
    setkeyv(treex, tsumuniqueid)
    
    if (addseed) {
      setkeyv(seedx, tsumuniqueid)
    }
       
    if (!is.null(condx)) {
      nocond <- FALSE

      ## Check cuniqueid and condid in cond table
      condnmlst <- names(condx)
      cuniqueid <- FIESTA::pcheck.varchar(var2check=cuniqueid, varnm="cuniqueid", 
		checklst=condnmlst, caption="UniqueID variable - cond", 
		warn=paste(cuniqueid, "not in cond table"))
      if (is.null(cuniqueid)) {
        if (tuniqueid %in% condnmlst) {
          cuniqueid <- tuniqueid
        } else {
          stop("cuniqueid is invalid")
        }
      }

      ## Check if class of tuniqueid matches class of cuniqueid
      tabs <- FIESTA::check.matchclass(treex, condx, tuniqueid, cuniqueid)
      treex <- tabs$tab1
      condx <- tabs$tab2

      if (!condid %in% names(condx)) {
        stop("condid must be in condx")
      }
      csumuniqueid <- c(cuniqueid, condid)
      setkeyv(condx, csumuniqueid)
      checkNAcvars <- c(checkNAcvars, csumuniqueid)

      ## Check that values of tuniqueid in treex are all in puniqueid in pltx
      treex <- FIESTA::check.matchval(treex, condx, tsumuniqueid, csumuniqueid,
		tab1txt="tree", tab2txt="cond")
    
      if (addseed) {
        ## Check if class of tuniqueid matches class of cuniqueid
        tabs <- FIESTA::check.matchclass(seedx, condx, tuniqueid, cuniqueid)
        seedx <- tabs$tab1
        condx <- tabs$tab2

        ## Check that values of tuniqueid in treex are all in puniqueid in pltx
        seedx <- FIESTA::check.matchval(seedx, condx, tsumuniqueid, csumuniqueid)
      }
    } 
  } else {
    tsumuniqueid <- tuniqueid
  }

  if (bysubp) {
    pltsp <- FALSE
    noplt <- TRUE
    nocond <- TRUE

    ## Check condid in tree table and setkey to tuniqueid, condid
    subpid <- FIESTA::pcheck.varchar(var2check=subpid, varnm="subpid", 
		checklst=names(treex), caption="subplot ID - tree", 
		warn=paste(subpid, "not in tree table"))
    if (is.null(subpid)) {
      warning("assuming only 1 subplot")
      treex$SUBPID <- 1
      subpid <- "SUBPID"

      if (addseed) {
        if (!subpid %in% names(seedx)) {
          seedx$SUBPID <- 1
        } else {
          stop(subpid, "in seed but not in cond")
        }
      }  
    } else {
      if (!subpid %in% names(seedx)) {
        stop(subpid, "not in seed") 
      }
    }

    tsumuniqueid <- c(tsumuniqueid, subpid)
    setkeyv(treex, tsumuniqueid)
    checkNAtvars <- c(checkNAtvars, subpid)
    if (addseed) {
      setkeyv(seedx, tsumuniqueid)
    }
  }

  pltx <- pcheck.table(plt, gui=gui, tabnm="plt", caption="Plot table?")

  if (!is.null(pltx)) {
    noplt <- FALSE

    ## Remove totally nonsampled plots
    if ("PLOT_STATUS_CD" %in% names(pltx)) {
      if (3 %in% unique(pltx[["PLOT_STATUS_CD"]]))
          warning(paste("There are", sum(pltx[["PLOT_STATUS_CD"]] == 3), "nonsampled plots"))
      pltx <- pltx[plt$PLOT_STATUS_CD != 3,]
    }

    if ("sf" %in% class(pltx))
      pltsp <- TRUE

    ## Check puniqueid
    pltnmlst <- names(pltx)
    puniqueid <- FIESTA::pcheck.varchar(var2check=puniqueid, varnm="puniqueid", 
		checklst=pltnmlst, caption="UniqueID variable - plt", 
		warn=paste(puniqueid, "not in plot table"), stopifnull=TRUE)

    ## Check for unique plot records
    if (nrow(pltx) > length(unique(pltx[[puniqueid]]))) {
      message("plt table has > 1 record per uniqueid... will not be merged to plt.")
      noplt <- TRUE
    }


    ## Check if class of tuniqueid matches class of puniqueid
    tabs <- FIESTA::check.matchclass(treex, pltx, tuniqueid, puniqueid)
    treex <- tabs$tab1
    pltx <- tabs$tab2

    ## Check that the values of tuniqueid in treex are all in puniqueid in pltx
    treex <- check.matchval(treex, pltx, tuniqueid, puniqueid)

    if (addseed) {
      ## Check if class of tuniqueid matches class of puniqueid
      tabs <- FIESTA::check.matchclass(seedx, pltx, tuniqueid, puniqueid)
      seedx <- tabs$tab1
      pltx <- tabs$tab2

      ## Check that the values of tuniqueid in seedx are all in puniqueid in pltx
      check.matchval(seedx, pltx, tuniqueid, puniqueid)
    }

    ## Check that the values of cuniqueid in condx are all in puniqueid in pltx
    if (!is.null(condx)) 
      ## Check that the values of tuniqueid in treex are all in puniqueid in pltx
      FIESTA::check.matchval(condx, pltx, cuniqueid, puniqueid)

    ## Change uniqueid in plt table to match tree uniqueid
#    if (puniqueid == "CN" && tuniqueid == "PLT_CN") {
#      names(pltx)[names(pltx) == puniqueid] <- tuniqueid
#      puniqueid <- tuniqueid
#    }
    setkeyv(pltx, puniqueid)
    checkNApvars <- c(checkNApvars, puniqueid)
  } 


  ## Check ACI. If TRUE, include all trees, If FALSE, filter for forested plots only 
  ## (COND_STATUS_CD = 1)
  ######################################################################################
  ACI <- FIESTA::pcheck.logical(ACI, varnm="ACI", title="Include ACI tree data?", 
		first="NO", gui=gui)
  if (!ACI) {
    if (is.null(condx) || (!"COND_STATUS_CD" %in% names(condx))) {
      warning("COND_STATUS_CD not in table, assuming forested plots with no ACI plots")
    } else {
      cond.ids <- na.omit(condx[COND_STATUS_CD == 1, 
		do.call(paste, .SD), .SDcols=c(cuniqueid, condid)])
      treex <- treex[paste(get(eval(tuniqueid)), get(eval(condid))) %in% cond.ids]
    }
  }

  ## Check for NA values in necessary variables in all tables
  if (checkNA) {
    treex.na <- sapply(checkNAtvars, 
		function(x, treex){ sum(is.na(treex[,x, with=FALSE])) }, treex)
    if (any(treex.na) > 0) {
      stop(treex.na[treex.na > 0], " NA values in tree variable: ", 
		paste(names(treex.na[treex.na > 0]), collapse=", "))
    }
    condx.na <- sapply(checkNAcvars, 
		function(x, condx){ sum(is.na(condx[,x, with=FALSE])) }, condx)
    if (any(condx.na) > 0) {
      stop(condx.na[condx.na > 0], " NA values in cond variable: ", 
		paste(names(condx.na[condx.na > 0]), collapse=", "))
    }
    pltx.na <- sapply(checkNApvars, 
		function(x, pltx){ sum(is.na(pltx[,x, with=FALSE])) }, pltx)
    if (any(pltx.na) > 0) {
      stop(pltx.na[pltx.na > 0], " NA values in plt variable: ", 
		paste(names(pltx.na[condx.na > 0]), collapse=", "))
    }
  }

  ## Check getadjplot
  getadjplot <- FIESTA::pcheck.logical(getadjplot, varnm="getadjplot", 
		title="Get plot adjustment?", first="NO", gui=gui)
  if (getadjplot && is.null(condx)) 
    stop("must include condx to adjust to plot")

  ## Check adjtree
  adjtree <- FIESTA::pcheck.logical(adjtree, varnm="adjtree", title="Adjust trees", 
		first="NO", gui=gui)
  if (is.null(adjtree)) adjtree <- FALSE

      
  ###########################################################  
  ### Check tsumvarlst
  ###########################################################  
  tsumvarlst <- FIESTA::pcheck.varchar(var2check=tsumvarlst, 
	varnm="tsumvarlst", checklst=names(treex), caption="Aggregate variable(s)", 
	multiple=TRUE, stopifnull=TRUE, gui=gui)
  if (any(tsumvarlst == tuniqueid)) {
    tsumvarlst[tsumvarlst == tuniqueid] <- "TPA_UNADJ"
  }
  
  ### Convert variables from pound to tons if lbs2tons=TRUE
  if (lbs2tons && any(tsumvarlst %in% vars2convert)) {
    vars2convert <- tsumvarlst[which(tsumvarlst %in% vars2convert)]
    message("converting from pounds to tons: ", paste(vars2convert, collapse=", "))
    for (j in vars2convert) set(treex, i=NULL, j=j, value=treex[[j]] * 0.0005)
  }

  ## Check metric and convert
  metric <- FIESTA::pcheck.logical(metric, varnm="metric", title="Metric converstion?", 
	first="NO", stopifnull=TRUE, gui=gui)

  ## Check TPA and if the TPA variable is in treex
  TPA <- FIESTA::pcheck.logical(TPA, varnm="TPA", title="Calculate TPA?", first="NO", 
		stopifnull=TRUE, gui=gui)
 
  if (TPA) {
    if (any(tsumvarlst %in% mortvars)) {
      if (!"TPAMORT_UNADJ" %in% names(treex)) {
        stop("you must have TPAMORT_UNADJ in tree table to calculate trees per acre")
      }
    } else if (any(tsumvarlst %in% growvars)) {
      if (!"TPAGROW_UNADJ" %in% names(treex)) {
        stop("you must have TPAGROW_UNADJ in tree table to calculate trees per acre")
      }
    } else if (any(tsumvarlst %in% remvars)){
      if (!"TPAREMV_UNADJ" %in% names(treex)) {
        stop("you must have TPAREMV_UNADJ in tree table to calculate trees per acre")
      }
    } else {  
      if (!"TPA_UNADJ" %in% names(treex)) {
        stop("you must have TPA_UNADJ in tree table to calculate trees per acre")
      }
    }
     
    ## Check adjTPA and adjust TPA (default = 1)
    ## (e.g., if adjTPA=4 (only 1 subplot measured), multiply TPA* by 4)
    if (is.null(adjTPA)) {
      message("adjTPA is invalid, assuming no adjustments")
      adjTPA <- 1
    } else if (!is.numeric(adjTPA)) {
      stop("adjTPA must be a numeric number from 1 to 4")
    } else if (!adjTPA %in% 1:4) {
      stop("adjTPA must be between 1 and 4")
    } else if (adjTPA > 1) {
      if ("SUBP" %in% names(treex)) {
        if (adjTPA == 2 && any(treex[, unique(SUBP), by=tuniqueid][[2]] > 3)) {
          stop("more than 3 SUBP in dataset")
        } else if (adjTPA == 3 && any(treex[, unique(SUBP), by=tuniqueid][[2]] > 2)) {
          stop("more than 2 SUBP in dataset")
        } else if (adjTPA == 4 && any(treex[, unique(SUBP), by=tuniqueid][[2]] > 1)) {
          stop("more than 1 SUBP in dataset")
        }
      } else {
        message("assuming less than 3 SUBP in dataset")
      }
    }
  }
 
  ### GET tfun USED FOR AGGREGATION
  tfunlst <- c("sum", "mean", "max", "min", "length", "median")

  if (is.null(tfun)) {
    if (gui) {
      tfunstr <- select.list(tfunlst, title="Aggregate function", multiple=FALSE)
      if (tfunstr == "") stop("")
      tfun <- get(tfunstr)
    } else {
      tfun <- sum
    }
  }

  if (!is.function(tfun)) {
    stop("tfun is not a function")
  } else {
    if(tuniqueid %in% tsumvarlst & !identical(tfun, sum))
      stop("use sum with PLT_CN for getting number of trees.")

    if (length(grep("mean", deparse(tfun))) > 0) {
      tfunstr <- "mean"
    } else {
      tfunnm <- noquote(strsplit(deparse(tfun), ".Primitive")[[1]][2])
      if (is.na(tfunnm))
        tfunnm <- noquote(strsplit(deparse(tfun), "UseMethod"))[[2]][2]
      if (is.na(tfunnm)) {
        warning("unknown function")
        tfunstr <- "fun"
      } else {  
        tfunstr <- substr(tfunnm, 3, nchar(tfunnm)-2)
      }
    }
  }

  ## Get name for summed tree variable(s)
  getnm <- FALSE
  if (is.null(tsumvarnmlst)) {
    getnm <- TRUE
  } else {
    if (length(tsumvarnmlst) != length(tsumvarlst)) {
      message(paste("number of names in tsumvarnmlst does not match number of tsumvars.",
 		"using default names."))
      getnm <- TRUE
    }
  } 

  ### Filter tree data 
  ###########################################################  

  ## FILTER TREES FOR DIA >= 1
#  if ("DIA" %in% names(treex)) 
#    treef <- datFilter(x=treex, xfilter="DIA >= 1.0", title.filter="DIA < 1.0in")$xf 
  
  ## Tree filter
  tdat <- datFilter(x=treex, xfilter=tfilter, title.filter="tfilter", 
		stopifnull=TRUE, gui=gui)
  treef <- tdat$xf
  tfilter <- tdat$xfilter
 
  if (addseed || seedonly) {
    xfilter <- tryCatch( FIESTA::check.logic(seedx, tfilter),
		error=function(e) return(NULL))
    if (!is.null(xfilter)) {
      ## Seed filter
      sdat <- datFilter(x=seedx, xfilter=tfilter, title.filter="tfilter", xnm="seed")
      seedf <- sdat$xf
    } else {
      seedf <- seedx
    }
  }

  ## CHECK tround
  if (is.null(tround) | !is.numeric(tround)) {
    warning("tround is invalid.. rounding to 6 digits")
    tround <- 6
  }

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
      out_layer <- "tsum"
    }
    if (!is.null(layer.pre)) {
      out_layer <- paste(layer.pre, out_layer, sep="_")
    }
  }

  ################################################################################  
  ################################################################################  
  ### DO WORK
  ################################################################################ 
  ################################################################################  

  if (getadjplot) {
    ## Remove nonsampled plots 
    if ("COND_STATUS_CD" %in% names(condx)) {
      cond.nonsamp.filter <- "COND_STATUS_CD != 5"
      nonsampn <- sum(condx$COND_STATUS_CD == 5, na.rm=TRUE)
      if (length(nonsampn) > 0) {
        message("removing ", nonsampn, " nonsampled forest conditions")
      } else {
        message("assuming all sampled conditions in cond")
      }
    } else {
      message("assuming all sampled conditions in cond")
    }
    if (ACI && "NF_COND_STATUS_CD" %in% names(condx)) {
      cond.nonsamp.filter.ACI <- "(is.na(NF_COND_STATUS_CD) | NF_COND_STATUS_CD != 5)"
      message("removing ", sum(is.na(NF_COND_STATUS_CD) & NF_COND_STATUS_CD == 5, na.rm=TRUE), 
		" nonsampled nonforest conditions")
      if (!is.null(cond.nonsamp.filter)) 
        cond.nonsamp.filter <- paste(cond.nonsamp.filter, "&", cond.nonsamp.filter.ACI)
    }
    condx <- datFilter(x=condx, xfilter=cond.nonsamp.filter, 
		title.filter="cond.nonsamp.filter")$xf

    adjfacdata <- getadjfactorPLOT(treex=treef, seedx=seedf, condx=condx, 
		tuniqueid=tuniqueid, cuniqueid=cuniqueid)
    condx <- adjfacdata$condadj
    treef <- adjfacdata$treeadj
    if (addseed) {
      seedf <- adjfacdata$seedx
    }
   
    adjtree <- TRUE
  }

  if (adjtree && !"tadjfac" %in% names(treef)) {
    stop("you must have tadjfac variable in tree table to adjust trees")
  }
  tsumvarlst2 <- {}
  tsumvarnmlst2 <- {} 
  seedcountvar <- NULL

  ## If any variable in tsumvarlst is a TPA variable, add a count variable to treex
  if (any(tsumvarlst %in% tpavars)) {
    treef[, COUNT := 1]
    if (addseed || seedonly) {
      seedf[, COUNT := 1]
    }
  }   

  ## ADDS '_TPA' TO VARIABLE NAME, MULTIPLIES BY TPA_UNADJ, AND DIVIDES BY adjfac
  for (tvar in tsumvarlst) {
    if (tvar %in% c(tuniqueid, tpavars)) {
      tvar <- "COUNT"
    }
    if (tvar %in% ref_estvar$ESTVAR) { 
      estunits <- unique(ref_estvar$ESTUNITS[ref_estvar$ESTVAR == tvar])
    } else {
      if (metric) {
        message(tvar, " not in ref_estvar... no metric conversion")
        metric <- FALSE
      } else {
        message(tvar, " not in ref_estvar... no units found")
      }
    }
    if (metric) {
      metricunits <- unique(ref_estvar$METRICUNITS[ref_estvar$ESTVAR == tvar])
      if (estunits != metricunits) {
        cfactor <- FIESTA::ref_conversion$CONVERSION[FIESTA::ref_conversion$METRIC == 
			metricunits]
        tvarm <- paste0(tvar, "_m")
        treef[, (tvarm) := get(eval(tvar)) * cfactor]
        estunits <- metricunits
        tvar <- tvarm
      }
    } 
    ## MULTIPLY tvar BY TPA VARIABLE IF DESIRED
    if (TPA) {
      if (tvar %in% mortvars) {
        tpavar <- "TPAMORT_UNADJ"
      } else if (tvar %in% growvars) {
        tpavar <- "TPAGROW_UNADJ"
      } else if (tvar %in% remvars) {
        tpavar <- "TPAREMV_UNADJ"
      } else{
        #tpavar <- "TPAGROW_UNADJ"
        tpavar <- "TPA_UNADJ"
      } 
      newname <- paste0(tvar, "_TPA")
      ## Adjust by adjTPA variable (Default is 1)
      if (adjTPA > 1) {
        treef[, (tpavar) := get(eval(tpavar)) * adjTPA]
      }
      ## If metric, convert tpavar to trees per hectare
      if (metric) {
        tpa.m <- paste0(tpavar, "_m")
        treef[, (tpa.m) := 1 / ((1/ get(eval(tpavar)) * 0.4046860))]
        tpavar <- tpa.m
      }
      treef[, (newname) := get(eval(tvar)) * get(eval(tpavar))]

      if ((addseed || seedonly) && tvar=="COUNT" && tpavar %in% names(seedf)) {
        #seedf[, COUNT := TREECOUNT_CALC]
        if (adjTPA > 1) {
          seedf[, (tpavar) := get(eval(tpavar)) * adjTPA]
        }
        seedf[, (newname) := get(eval(tvar)) * get(eval(tpavar))]
        seedcountvar=treecountvar <- newname
      }
    } else {
      newname <- tvar
    }

    ## ADJUSTMENT FACTORS
    if (adjtree) {
      ## Create new name for adjusted variable
      if (length(grep("UNADJ", tvar)) == 1) {
        newname2 <- sub("UNADJ", "ADJ", tvar)
      } else {
        newname2 <- paste0(newname, "_ADJ")
      }

      ## Apply adjustments
      treef[, (newname2) := get(eval(newname)) * tadjfac]
   
      if ((addseed || seedonly) && tvar=="COUNT" && "tadjfac" %in% names(seedf)) {
        seedf[, (newname2) := get(eval(newname)) * tadjfac]
        seedcountvar=treecountvar <- newname2
      }       
    } else {
      newname2 <- newname
    }

    tsumvarlst2 <- c(tsumvarlst2, newname2)
    if (getnm) {
      if (toupper(tfunstr) != "SUM") {
        tsumvarnmlst <- c(tsumvarnmlst, paste0(newname2, "_", toupper(tfunstr)))  
        tsumvarnmlst2 <- sapply(tsumvarnmlst, FIESTA::checknm, names(treef))
      } else {
        tsumvarnmlst2 <- c(tsumvarnmlst2, newname2)  
      }
    } else {
      tsumvarnmlst2 <- tsumvarnmlst
    } 
  }

  ######################################################################## 
  ## Aggregate tree variables
  ######################################################################## 

  if (seedonly) {
    datvars <- seedf[, lapply(.SD, function(x) round(tfun(x, na.rm=TRUE), tround) ), 
		by=key(seedf), .SDcols=tsumvarlst2]
    setnames(datvars, c(tsumuniqueid, tsumvarnmlst2))
  } else {
    datvars <- treef[, lapply(.SD, function(x) round(tfun(x, na.rm=TRUE), tround) ), 
		by=key(treef), .SDcols=tsumvarlst2]
    setnames(datvars, c(tsumuniqueid, tsumvarnmlst2))

    if (addseed && !is.null(seedcountvar)) {
      sdatvars <- seedf[, lapply(.SD, function(x) round(tfun(x, na.rm=TRUE), tround) ), 
		by=key(seedf), .SDcols=seedcountvar]
      setnames(sdatvars, c(tsumuniqueid, paste0("SEED_", seedcountvar)))

      ## Merge using all.x and all.y in case there are plots with seedlings, no trees
      datvars <- merge(datvars, sdatvars, all.x=TRUE, all.y=TRUE)
      datvars[, (paste0("TREE_", treecountvar)) := get(treecountvar)]
      datvars[, (treecountvar) := sum(.SD, na.rm=TRUE), by=key(datvars), 
			.SDcols=c(paste0("TREE_", treecountvar), paste0("SEED_", treecountvar))]
      tsumvarnmlst2 <- treecountvar	
    } 
  }    
 
  ######################################################################## 
  ######################################################################## 

  ## Merge to cond or plot
  ###################################
  if (bycond && !nocond) {
    ## Merge to cond
    sumdat <- merge(condx, datvars, by.x=c(cuniqueid, condid),
		by.y=c(tuniqueid, condid), all.x=TRUE)
  } else if (!noplt) {
    if (is.data.table(pltx)) {
      setkeyv(datvars, tuniqueid)
      setkeyv(pltx, puniqueid)
    }
    sumdat <- merge(pltx, datvars, by.x=puniqueid, by.y=tuniqueid, all.x=TRUE)
  } else {
    sumdat <- datvars
  }

  if (NAto0) 
    ## Change NA values TO 0
    for (col in tsumvarnmlst2) set(sumdat, which(is.na(sumdat[[col]])), col, 0) 

  if (savedata && parameters) {
    ## OUTPUTS A TEXTFILE OF INPUT PARAMETERS TO OUTFOLDER
    ###########################################################
    outfn.param <- paste(out_layer, "parameters", sep="_")
    outparamfnbase <- paste(outfn.param, format(Sys.time(), "%Y%m%d"), sep="_")
    outparamfn <- fileexistsnm(outfolder, outparamfnbase, "txt")

    tsumvarlstout <- addcommas(sapply(tsumvarlst, function(x) {paste0("'", x, "'")}))
    tsumvarnmlstout <- addcommas(sapply(tsumvarnmlst, function(x) {paste0("'", x, "'")}))
    strunitvars <- addcommas(sapply(strunitvars, function(x) {paste0("'", x, "'")}))

    outfile <- file(paste0(outfolder, "/", outparamfn, ".txt"), "w")
    cat(  "tree = ", as.character(bquote(tree)), "\n",
      "cond = ", as.character(bquote(cond)), "\n",
      "plt = ", as.character(bquote(plt)), "\n",
      "plt_dsn = \"", plt_dsn, "\"", "\n",
      "tuniqueid = \"", tuniqueid, "\"", "\n",
      "cuniqueid = \"", cuniqueid, "\"", "\n",
      "puniqueid = \"", puniqueid, "\"", "\n",
      "bycond = ", bycond, "\n",
      "condid = \"", condid, "\"", "\n",
      "bysubp = ", bysubp, "\n",
      "subpid = \"", subpid, "\"", "\n",
      "tsumvarlst = c(", tsumvarlstout, ")", "\n",
      "tsumvarnmlst = c(", tsumvarnmlstout, ")", "\n",  
      "TPA = ", TPA, "\n",
      "tfun = ", noquote(tfunstr), "\n",
      "ACI = ", ACI, "\n",
      "tfilter = \"", tfilter, "\"", "\n",
      "lbs2tons = ", lbs2tons, "\n",
      "getadjplot = ", getadjplot, "\n",
      "adjtree = ", adjtree, "\n",
      "adjTPA = ", adjTPA, "\n",
      "NAto0 = ", NAto0, "\n",
      "savedata = ", savedata, "\n",
      "outfolder = \"", outfolder, "\"", "\n",
      "out_layer = ", out_layer, "\n",
      "outfn.date = ", outfn.date, "\n",
      "overwrite_dsn = ", overwrite_dsn, "\n",
      "tround = \"", tround, "\"", "\n", "\n",
    file = outfile, sep="")

    cat(  "sumdat <- datSumTree(tree=tree, cond=cond, plt=plt, plt_dsn=plt_dsn,
	tuniqueid=tuniqueid, cuniqueid=cuniqueid, puniqueid=puniqueid, bycond=bycond, 
	condid=condid, bysubp=bysubp, subpid=subpid, tsumvarlst=tsumvarlst, 
	tsumvarnmlst=tsumvarnmlst, TPA=TPA, tfun=tfun, ACI=ACI, tfilter=tfilter, 
	lbs2tons=lbs2tons, getadjplot=getadjplot, adjtree=adjtree, adjTPA=adjTPA, 
	NAto0=NAto0, savedata=savedata, outfolder=outfolder, out_layer=out_layer, 
	outfn.date=outfn.date, overwrite_dsn=overwrite_dsn, tround=tround)",
    file = outfile, sep="")
    close(outfile)

    #### WRITE TO FILE 
    #############################################################
    if (pltsp) {
      spExportSpatial(sumdat, out_dsn=plt_dsn, out_layer=out_layer, 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer, 
		append_layer=append_layer)
    }
    datExportData(sumdat, outfolder=outfolder, out_fmt=out_fmt, out_dsn=out_dsn,
		out_layer=out_layer, outfn.date=outfn.date, 
		overwrite_layer=overwrite_layer, append_layer=append_layer)
  } 

  if (!returnDT) {     
    sumdat <- setDF(sumdat)
  }
  sumtreelst <- list(treedat=sumdat, sumvars=tsumvarnmlst2)
  sumtreelst$estunits <- estunits
  if (!is.null(tfilter)) {
    sumtreelst$tfilter <- tfilter
  }
  return(sumtreelst)
} 
