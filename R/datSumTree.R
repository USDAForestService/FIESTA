datSumTree <- function(tree=NULL, cond=NULL, plt=NULL, plt_dsn=NULL, 
	tuniqueid="PLT_CN", cuniqueid="PLT_CN", puniqueid="CN", bycond=FALSE, 
	condid="CONDID", tsumvarlst=NULL, tsumvarnmlst=NULL, TPA=TRUE, 
	tfun=sum, ACI=FALSE, tfilter=NULL, lbs2tons=TRUE, getadjplot=FALSE, 
	adjtree=FALSE, adjTPA=1, savedata=FALSE, outfolder=NULL, 
	outfn=NULL, outfn.date=TRUE, overwrite=FALSE, tround=16, checkNA=FALSE){
  ####################################################################################
  ## DESCRIPTION: Aggregates tree variable(s) to plot(/cond)-level, 
  ##        using specified tree filters (ex. live trees only)
  ####################################################################################
      

  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- ifelse(nargs() == 0, TRUE, FALSE)

  ## Set global variables  
  COND_STATUS_CD=tadjfac=PLOT_STATUS_CD=COUNT=plts=SUBP <- NULL


  ## If gui.. set variables to NULL
  if (gui) ACI=bycond=tuniqueid=puniqueid=cuniqueid=TPA=tfun=adjtree=adjsamp=
	savedata=outfolder <- NULL
  checkNApvars <- {}
  checkNAcvars <- {}
  checkNAtvars <- {}

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

  ## Check tree
  treex <- FIESTA::pcheck.table(tree, gui=gui, tabnm="tree", caption="Tree table?")
  if (is.null(treex)) stop("you must include a tree table for tree estimates")

  ## Check cond
  condx <- FIESTA::pcheck.table(cond, tabnm="cond", gui=gui, caption="Condition table?")

  ## Check plt
  noplt <- TRUE
  pltshp <- FALSE
  plt <- FIESTA::pcheck.table(plt, gui=gui, tabnm="plt", caption="Plot table?")
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
      #plt.prj <- proj4string(plt)
      #plt.coords <- coordinates(plt)
    } else {
      pltx <- plt
    }
  } else {
    pltx <- plt
  }


  ## Check bycond
  ###################################################################################
  bycond <- FIESTA::pcheck.logical(bycond, varnm="bycond", title="By condition?", 
		first="YES", gui=gui, stopifnull=TRUE)
  if (bycond) noplt <- TRUE

  ## Check checkNA
  ###################################################################################
  checkNA <- FIESTA::pcheck.logical(checkNA, varnm="checkNA", title="Check NA values?", 
		first="YES", gui=gui)
  if (is.null(checkNA)) checkNA <- FALSE


  ## Check unique identifiers, set keys, and matching values/classes
  ###################################################################################

  ## Check tuniqueid
  treenmlst <- names(treex)
  tuniqueid <- FIESTA::pcheck.varchar(var2check=tuniqueid, varnm="tuniqueid", 	
		checklst=treenmlst, caption="UniqueID variable - tree", 
		warn=paste(tuniqueid, "not in tree table"))
  uniqueid <- tuniqueid
  setkeyv(treex, tuniqueid)
  checkNAtvars <- c(checkNAtvars, uniqueid)

  ## If bycond, check condid in tree table and setkey to tuniqueid, condid
  if (bycond) {
    ## Check condid in tree table
    condid <- FIESTA::pcheck.varchar(var2check=condid, varnm="condid", 
		checklst=treenmlst, caption="Cond ID - tree", 
		warn=paste(condid, "not in tree table"))
    if (is.null(condid)) {
      warning("assuming all 1 condition plots")
      treex$CONDID <- 1
      condid <- "CONDID"
    }
    setkeyv(treex, c(tuniqueid, condid))
    checkNAtvars <- c(checkNAtvars, condid)
  }

  if (!is.null(condx)) {
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
    if (!condid %in% names(condx)) stop("condid must be in condx")
    setkeyv(condx, c(cuniqueid, condid))
    checkNAcvars <- c(checkNAcvars, cuniqueid, condid)


    ## Check that values of tuniqueid in treex are all in puniqueid in pltx
    FIESTA::check.matchval(treex, condx, c(tuniqueid, condid), c(cuniqueid, condid))

    ## Check if class of tuniqueid matches class of cuniqueid
    tabs <- FIESTA::check.matchclass(treex, condx, tuniqueid, cuniqueid)
    treex <- tabs$tab1
    condx <- tabs$tab2
  }

  if (!noplt) {
    ## Check puniqueid
    pltnmlst <- names(pltx)
    puniqueid <- FIESTA::pcheck.varchar(var2check=puniqueid, varnm="puniqueid", 
		checklst=pltnmlst, caption="UniqueID variable - plt", 
		warn=paste(puniqueid, "not in plot table"), stopifnull=TRUE)

    ## Remove totally nonsampled plots
    #if ("PLOT_STATUS_CD" %in% pltnmlst)
    #  pltx <- pltx[PLOT_STATUS_CD != 3,]

    ## Check that the values of tuniqueid in treex are all in puniqueid in pltx
    FIESTA::check.matchval(treex, pltx, tuniqueid, puniqueid)

    ## Check if class of tuniqueid matches class of puniqueid
    tabs <- FIESTA::check.matchclass(treex, pltx, tuniqueid, puniqueid)
    treex <- tabs$tab1
    pltx <- tabs$tab2

    ## Check that the values of cuniqueid in condx are all in puniqueid in pltx
    if (!is.null(condx)) 
      ## Check that the values of tuniqueid in treex are all in puniqueid in pltx
      FIESTA::check.matchval(condx, pltx, cuniqueid, puniqueid)

    ## Change uniqueid in plt table to match tree uniqueid
    if (puniqueid == "CN" && tuniqueid == "PLT_CN") {
      names(pltx)[names(pltx) == puniqueid] <- tuniqueid
      puniqueid <- tuniqueid
    }
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
      message("COND_STATUS_CD not in table, assuming forested plots with no ACI plots")
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
    if (any(treex.na) > 0) 
      stop(treex.na[treex.na > 0], " NA values in tree variable: ", 
		paste(names(treex.na[treex.na > 0]), collapse=", "))

    condx.na <- sapply(checkNAcvars, 
		function(x, condx){ sum(is.na(condx[,x, with=FALSE])) }, condx)
    if (any(condx.na) > 0) 
      stop(condx.na[condx.na > 0], " NA values in cond variable: ", 
		paste(names(condx.na[condx.na > 0]), collapse=", "))

    pltx.na <- sapply(checkNApvars, 
		function(x, pltx){ sum(is.na(pltx[,x, with=FALSE])) }, pltx)
    if (any(pltx.na) > 0) 
      stop(pltx.na[pltx.na > 0], " NA values in plt variable: ", 
		paste(names(pltx.na[condx.na > 0]), collapse=", "))
  }

  ## Check getadjplot
  getadjplot <- FIESTA::pcheck.logical(getadjplot, varnm="getadjplot", 
		title="Get plot adjustment?", first="NO", gui=gui)

  ## Check adjtree
  adjtree <- FIESTA::pcheck.logical(adjtree, varnm="adjtree", title="Adjust trees", 
		first="NO", gui=gui)
  if (is.null(adjtree)) adjtree <- FALSE

  ## By plot / By cond
  ######################################################################################
  if (bycond) {
    pltshp <- FALSE
    noplt <- TRUE

    if (is.null(condx)) {
      ## Check if CONDID is in the tree table.
      if (!condid %in% names(treex)) 
        stop(paste(condid, "must be in tree table")) 
    } else {
      ## Check if CONDID is in the cond table.
      if (!condid %in% names(condx) | (!condid %in% names(treex)))
        stop(paste(condid, "must be in COND and TREE table")) 
    }
    treeid <- c(uniqueid, condid)
  } else {   	# by plot
    treeid <- uniqueid
  }      
      
  ### Variables to aggregate
  if (is.null(tsumvarlst)) {
    tsumvarlst <- select.list(treenmlst, title="Aggregate variable(s)", multiple=TRUE)
      if(length(tsumvarlst)==0){stop("")}
  } else if (sum(sapply(tsumvarlst, function(x){x %in% treenmlst})) < length(tsumvarlst)){
    stop("check tsumvarlst: tree variable(s) not in tree table")
  }
  tsumvarlst <- FIESTA::pcheck.varchar(var2check=tsumvarlst, varnm="tsumvarlst", gui=gui, 
		checklst=treenmlst, caption="Aggregate variable(s)", multiple=TRUE,
		stopifnull=TRUE)
  if (any(tsumvarlst == tuniqueid)) tsumvarlst[tsumvarlst == tuniqueid] <- "TPA_UNADJ"

  
  ### Convert variables from pound to tons if lbs2tons=TRUE
  if (lbs2tons && any(tsumvarlst %in% vars2convert)) {
    vars2convert <- tsumvarlst[which(tsumvarlst %in% vars2convert)]
    message("converting from pounds to tons: ", paste(vars2convert, collapse=", "))
    for (j in vars2convert) set(treex, i=NULL, j=j, value=treex[[j]] * 0.0005)
  }

  ## Check TPA and if the TPA variable is in treex
  TPA <- FIESTA::pcheck.logical(TPA, varnm="TPA", title="Calculate TPA?", first="NO", 
		stopifnull=TRUE, gui=gui)
 
  if (TPA) {
    if (any(tsumvarlst %in% mortvars)) {
      if (!"TPAMORT_UNADJ" %in% names(treex))
        stop("you must have TPAMORT_UNADJ in tree table to calculate trees per acre")
    } else if (any(tsumvarlst %in% growvars)) {
      if (!"TPAGROW_UNADJ" %in% names(treex))
        stop("you must have TPAGROW_UNADJ in tree table to calculate trees per acre")
    } else if (any(tsumvarlst %in% remvars)){
      if (!"TPAREMV_UNADJ" %in% names(treex))
        stop("you must have TPAREMV_UNADJ in tree table to calculate trees per acre")
    } else {  
      if (!"TPA_UNADJ" %in% names(treex))
        stop("you must have TPA_UNADJ in tree table to calculate trees per acre")}
     
    ## Check adjTPA and adjust TPA (default = 1)
    ## (e.g., if adjTPA=4 (only 1 subplot measured), multiply TPA* by 4)
    if (is.null(adjTPA)) {
      message("adjTPA is invalid, assuming no adjustments")
      adjTPA <- 1
    } else if (!is.numeric(adjTPA)) {
      stop("adjTPA must be a numeric number from 1 to 4")
    } else if (!adjTPA %in% 1:4){
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

  ## FILTER TREES FOR DIA >= 1
#  if ("DIA" %in% names(treex)) 
#    treef <- datFilter(x=treex, xfilter="DIA >= 1.0", title.filter="DIA < 1.0in")$xf 

  
  ## Tree filter
  tdat <- FIESTA::datFilter(x=treex, xfilter=tfilter, title.filter="tfilter", 
		stopifnull=TRUE, gui=gui)
  treef <- tdat$xf
  tfilter <- tdat$xfilter

  ## CHECK tround
  if (is.null(tround) | !is.numeric(tround)) {
    warning("tround is invalid.. rounding to 6 digits")
    tround <- 6
  }

  ### GET savedata 
  savedata <- FIESTA::pcheck.logical(savedata, varnm="savedata", title="Save data tables?", 
		first="NO", gui=gui)

  ## GET outfolder
  if (savedata) {
    outfolder <- pcheck.outfolder(outfolder, gui)

    ## outfn
    if (is.null(outfn) || gsub(" ", "", outfn) == "") 
      outfn <- paste("tsum", sep="_")
    outfn.param <- paste(outfn, "param", sep="_")
  }


  ################################################################################  
  ### DO WORK
  ################################################################################ 

  if (getadjplot) {
    adjfacdata <- getadjfactorPLOT(esttype="TREE", treex=treef, condx=condx, 
		tuniqueid=tuniqueid, cuniqueid=cuniqueid)
    condx <- adjfacdata$condadj
    treef <- adjfacdata$treeadj
  }

  if (adjtree && !"tadjfac" %in% names(treef))
    stop("you must have tadjfac variable in tree table to adjust trees")
  
  tsumvarlst2 <- {}
  tsumvarnmlst2 <- {}

  ## If any variable in tsumvarlst is a TPA variable, add a count variable to treex
  if (any(tsumvarlst %in% tpavars)) treef[, COUNT := 1]

  ## ADDS '_TPA' TO VARIABLE NAME, MULTIPLIES BY TPA_UNADJ, AND DIVIDES BY adjfac
  for (tvar in tsumvarlst) {
    if (tvar %in% c(tuniqueid, tpavars)) tvar <- "COUNT"
    
    ## MULTIPLY tvar BY TPA VARIABLE IF DESIRED
    if (TPA) {
      if (tvar %in% mortvars) {
        tpavar <- "TPAMORT_UNADJ"
      } else if (tvar %in% growvars) {
        tpavar <- "TPAGROW_UNADJ"
      } else if (tvar %in% remvars) {
        tpavar <- "TPAREMV_UNADJ"
      } else{
        tpavar <- "TPA_UNADJ"
      } 
      newname <- paste0(tvar, "_TPA")

      ## Adjust by adjTPA variable (Default is 1)
      treef[, (tpavar) := get(eval(tpavar)) * adjTPA]
      treef[, (newname) := get(eval(tvar)) * get(eval(tpavar))]
    } else {
      newname <- tvar
    }

    ## ADJUSTMENT FACTORS
    if (adjtree) {
      ## GET NEW NAMES FOR ADJUSTMENTS
      if (length(grep("UNADJ", tvar)) == 1) {
        newname2 <- sub("UNADJ", "ADJ", tvar)
      } else {
        newname2 <- paste0(newname, "_ADJ")
      }

      ## APPLY ADJUSTMENTS
      treef[, (newname2) := get(eval(newname)) * tadjfac]
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
  
  if (bycond) {
 #   tsumvarnmlst2 <- sapply(tsumvarnmlst, FIESTA::checknm, names(treef))
    datvars <- treef[, lapply(.SD, function(x) round(tfun(x), tround) ), by=key(treef), 
		.SDcols=tsumvarlst2]
    setnames(datvars, c(uniqueid, condid, tsumvarnmlst2))

    ## MERGE to cond
    if (!is.null(condx)) {
      sumdat <- merge(condx, datvars, all.x=TRUE)
    } else {
      sumdat <- datvars
    }

    ## CHANGE NA VALUES TO 0
    for (col in tsumvarnmlst2) set(sumdat, which(is.na(sumdat[[col]])), col, 0) 
  
  } else {  ## by plot

    setkeyv(treef, tuniqueid)
 #   tsumvarnmlst2 <- sapply(tsumvarnmlst, FIESTA::checknm, names(treef))
    datvars <- treef[, lapply(.SD, function(x) round(tfun(x), tround) ), by=key(treef), 
		.SDcols=tsumvarlst2]
    setnames(datvars, c(treeid, tsumvarnmlst2))

    ## MERGE to plt
    if (!is.null(pltx)) {
      if ("PLOT_STATUS_CD" %in% names(pltx)) pltx <- pltx[PLOT_STATUS_CD != 3, ]
      setkeyv(pltx, puniqueid)
      sumdat <- merge(pltx, datvars, by.x=puniqueid, by.y=treeid, all.x=TRUE)

      ## CHANGE NA VALUES TO 0
      for (col in tsumvarnmlst2) set(sumdat, which(is.na(sumdat[[col]])), col, 0) 
  
    } else {
      sumdat <- datvars
    }
  }  

  if (savedata) {
    ## OUTPUTS A TEXTFILE OF INPUT PARAMETERS TO OUTFOLDER
    ###########################################################
    outparamfnbase <- paste(outfn.param, format(Sys.time(), "%Y%m%d"), sep="_")
    outparamfn <- fileexistsnm(outfolder, outparamfnbase, "txt")

    tsumvarlstout <- addcommas(sapply(tsumvarlst, function(x) {paste0("'", x, "'")}))
    tsumvarnmlstout <- addcommas(sapply(tsumvarnmlst, function(x) {paste0("'", x, "'")}))
    strunitvars <- addcommas(sapply(strunitvars, function(x) {paste0("'", x, "'")}))

    outfile <- file(paste0(outfolder, "/", outparamfn, ".txt"), "w")
    cat(  "tree = ", as.character(bquote(tree)), "\n",
      "plt = ", as.character(bquote(plt)), "\n",
      "cond = ", as.character(bquote(cond)), "\n",
      "ACI = ", ACI, "\n",
      "tuniqueid = \"", tuniqueid, "\"", "\n",
      "puniqueid = \"", puniqueid, "\"", "\n",
      "cuniqueid = \"", cuniqueid, "\"", "\n",
      "bycond = ", bycond, "\n",
      "condid = \"", condid, "\"", "\n",
      "tsumvarlst = c(", tsumvarlstout, ")", "\n",
      "tsumvarnmlst = c(", tsumvarnmlstout, ")", "\n",  
      "TPA = ", TPA, "\n",
      "adjTPA = ", adjTPA, "\n",
      "tfun = ", noquote(tfunstr), "\n",
      "tfilter = \"", tfilter, "\"", "\n",
      "lbs2tons = ", lbs2tons, "\n",
      "adjtree = ", adjtree, "\n",
      "savedata = ", savedata, "\n",
      "outfolder = \"", outfolder, "\"", "\n",
      "outfn = ", outfn, "\n",
      "tround = \"", tround, "\"", "\n", "\n",
    file = outfile, sep="")

    cat(  "sumdat <- datSumTree(tree=tree, plt=plt, cond=cond, ACI=ACI, 
	tuniqueid=tuniqueid, puniqueid=puniqueid, cuniqueid=cuniqueid, bycond=bycond, 
	condid=condid, tsumvarlst=tsumvarlst, tsumvarnmlst=tsumvarnmlst, TPA=TPA, 
	adjTPA=adjTPA, tfun=tfun, tfilter=tfilter, lbs2tons=lbs2tons, adjtree=adjtree,
	savedata=savedata, outfolder=outfolder, outfn=outfn, tround=tround)",
    file = outfile, sep="")
    close(outfile)

    #### WRITE TO FILE 
    #############################################################
    if (pltshp) {  
      sumdatfn <- fileexistsnm(outfolder, outfn, "shp")
      spExportShape(sumdat, outshpnm=sumdatfn, outfolder=outfolder, 
		outfn.date=outfn.date, overwrite=overwrite)
    }
    write2csv(sumdat, outfolder=outfolder, outfilenm=outfn, outfn.date=outfn.date,
		overwrite=overwrite)
  } 


  sumtreelst <- list(treedat = setDF(sumdat))
  if (pltshp) {
    #for(nm in names(sumdat)){
    #  if (nchar(nm) > 10){
    #    newnm <- substr(nm, 0, 10)
    #    names(sumdat)[names(sumdat)== nm] <- newnm
    #  }
    #}
  
    ## GENERATE SHAPE
    sumtreelst$sptreedat <- sp::merge(plt[, puniqueid], sumdat, by=puniqueid, all.x=TRUE)
  }
  sumtreelst$sumvars <- tsumvarnmlst2

  return(sumtreelst)
} 
