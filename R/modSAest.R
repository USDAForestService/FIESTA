#' Small area module - Generate small area tree estimates.
#' 
#' Generates small area estimates by domain and/or tree domain (and estimation
#' unit).
#' 
#' If variables are NULL, then it will prompt user to input variables.
#' 
#' Necessary variables:\cr \tabular{llll}{ \tab \bold{Data} \tab
#' \bold{Variable} \tab \bold{Description}\cr \tab tree \tab tuniqueid \tab
#' Unique identifier for each plot, to link to pltstrat (ex. PLT_CN).\cr \tab
#' \tab CONDID \tab Unique identifier of each condition on plot, to link to
#' cond.  Set CONDID=1, if only 1 condition per plot.\cr \tab \tab TPA_UNADJ
#' \tab Number of trees per acre each sample tree represents (ex. DESIGNCD=1:
#' TPA_UNADJ=6.018046 for trees on subplot; 74.965282 for trees on
#' microplot).\cr \tab cond \tab cuniqueid \tab Unique identifier for each
#' plot, to link to pltstrat (ex. PLT_CN).\cr \tab \tab CONDID \tab Unique
#' identfier of each condition on plot.  Set CONDID=1, if only 1 condition per
#' plot.\cr \tab \tab CONDPROP_UNADJ \tab Unadjusted proportion of condition on
#' each plot.  Set CONDPROP_UNADJ=1, if only 1 condition per plot.\cr \tab \tab
#' COND_STATUS_CD \tab Status of each forested condition on plot (i.e.
#' accessible forest, nonforest, water, etc.)\cr \tab \tab NF_COND_STATUS_CD
#' \tab If ACI=TRUE. Status of each nonforest condition on plot (i.e.
#' accessible nonforest, nonsampled nonforest)\cr \tab \tab SITECLCD \tab If
#' landarea=TIMBERLAND. Measure of site productivity.\cr \tab \tab RESERVCD
#' \tab If landarea=TIMBERLAND. Reserved status.\cr \tab \tab SUBPROP_UNADJ
#' \tab Unadjusted proportion of subplot conditions on each plot.  Set
#' SUBPROP_UNADJ=1, if only 1 condition per subplot.\cr \tab \tab
#' MICRPROP_UNADJ \tab If microplot tree attributes. Unadjusted proportion of
#' microplot conditions on each plot. Set MICRPROP_UNADJ=1, if only 1 condition
#' per microplot.\cr \tab \tab MACRPROP_UNADJ \tab If macroplot tree
#' attributes. Unadjusted proportion of macroplot conditions on each plot. Set
#' MACRPROP_UNADJ=1, if only 1 condition per macroplot.\cr \tab pltassign \tab
#' puniqueid \tab Unique identifier for each plot, to link to cond (ex. CN).\cr
#' \tab \tab STATECD \tab Identifies state each plot is located in.\cr \tab
#' \tab INVYR \tab Identifies inventory year of each plot.\cr \tab \tab
#' PLOT_STATUS_CD \tab Status of each plot (i.e. sampled, nonsampled).  If not
#' included, all plots are assumed as sampled.\cr }
#' 
#' Reference names are available for the following variables: \cr ADFORCD,
#' AGENTCD, CCLCD, DECAYCD, DSTRBCD, KINDCD, OWNCD, OWNGRPCD, FORTYPCD,
#' FLDTYPCD, FORTYPCDCALC, TYPGRPCD, FORINDCD, RESERVCD, LANDCLCD, STDSZCD,
#' FLDSZCD, PHYSCLCD, MIST_CL_CD, PLOT_STATUS_CD, STATECD, TREECLCD, TRTCD,
#' SPCD, SPGRPCD
#' 
#' @param SApopdatlst List. List of population data objects returned from
#' modSApop().
#' @param SAdomsdf DF/DT or comma-delimited file(*.csv). Dataframe from SAdoms
#' with attributes from smallbnd to add to estimation table.
#' @param prednames String vector. Name(s) of predictor variables to use in
#' model.
#' @param SApackage String. Small area package to use ('JoSAE', 'sae')
#' @param SAmethod String. Small area method to use ('unit', 'area')
#' @param esttype String. Estimation type ('TREE', 'AREA').
#' @param totals Logical. If TRUE, returns total estimate (mean * AREAUSED).
#' @param estseed String. Use seedling data only or add to tree data. Seedling
#' estimates are only for counts (estvar='TPA_UNADJ')-('none', 'only', 'add').
#' @param largebnd.unique String. Name of the large boundary unique identifer
#' to define plots within a model extent. If NULL, all plots are used for model
#' extent.
#' @param landarea String. The sample area filter for estimates ('ALL',
#' 'FOREST', 'TIMBERLAND').  If landarea=FOREST, filtered to COND_STATUS_CD =
#' 1; If landarea=TIMBERLAND, filtered to SITECLCD in(1:6) and RESERVCD = 0.
#' @param pcfilter String. A filter for plot or cond attributes (including
#' pltassgn).  Must be R logical syntax.
#' @param estvar String. Name of the tree estimate variable.
#' @param estvar.filter String. A tree filter for estimate variable. Must be R
#' syntax (e.g., "STATUSCD == 1").
#' @param allin1 Logical. If TRUE, both estimates and percent sample error are
#' output in one table as: estimates (percent sample error).
#' @param metric Logical. If TRUE, output area is in metric units (hectares).
#' @param variable.select Logical. If TRUE, selects useful predictors using
#' mase:ElasticNet.
#' @param estround Integer. Number of decimal places for estimates.
#' @param pseround Integer. Number of decimal places for percent sampling
#' error.
#' @param estnull Number or character. The number or symbol to use to indicate
#' 'not sampled' for estimate.
#' @param psenull Number or character. The number or symbol to use to indicate
#' 'not sampled' for percent standard errror.
#' @param divideby String. Conversion number for output ('hundred', 'thousand',
#' 'million').
#' @param savedata Logical. If TRUE, saves table(s) to outfolder.
#' @param savesteps Logical. Saves graphs of predictors and response with
#' labels whether selected or not for both area- and unit-level models.
#' @param rawdata Logical. If TRUE, returns a list of raw data tables that are
#' used for estimation (See Value). If savedata = TRUE, also written to
#' outfolder.
#' @param rawonly Logical. If TRUE, output rawdata only. If dataset includes
#' many estimation units, and only raw data tables are needed, it is more
#' efficient to output raw data only.
#' @param multest Logical. If TRUE, returns a data frame of SA estimates using
#' both unit-level and area-level estimates.
#' @param addSAdomsdf Logical. If TRUE, sppends SAdomdf to unit.multest table
#' for output.
#' @param SAdomvars String vector. List of attributes from SAdoms to include in
#' multest output.
#' @param outfolder String. The outfolder to write files to. If NULL, files are
#' written to working directory, or if gui, a window to browse.
#' @param outfn.pre String. A prefix for outfile name, if savedata=TRUE.
#' @param outfn.date Logical. If TRUE, add date to end of outfile (e.g.,
#' outfn_'date'.csv).
#' @param addtitle Logical. If TRUE and savedata=TRUE, adds title to outfile.
#' @param raw_fmt String. Format for raw output tables ('csv', 'sqlite',
#' 'gpkg').
#' @param raw_dsn String. Name of database if raw_fmt = c('sqlite', 'gpkg').
#' @param savemultest Logical. If TRUE, save table with area- and unit-level
#' estimates.
#' @param multest_fmt String. Format for multest output tables ('csv',
#' 'sqlite', 'gpkg').
#' @param multest_outfolder String. Outfolder for multest. If NULL, same as
#' outfolder.
#' @param multest_dsn String. Name of database if multest_fmt = c('sqlite',
#' 'gpkg').
#' @param multest_layer String. Name of database layer if multest_fmt =
#' c('sqlite', 'gpkg').
#' @param multest.append Logical. If TRUE, appends multest dataframe to output.
#' @param multest.AOIonly Logical. If TRUE, appends multest dataframe (AOI=1)
#' to output.
#' @param overwrite_dsn Logical. If TRUE, overwrites raw_dsn, if exists.
#' @param overwrite_layer Logical. If TRUE, overwrites the output. If
#' rawdata=TRUE, overwrites out_layer in rawdata folder (if raw_fmt = 'csv') or
#' out_layers in raw_dsn (if raw_fmt != 'csv').
#' @param append_layer Logical. If TRUE, and rawdata=TRUE, appends raw data to
#' existing *.csv files (if raw_fmt = 'csv') or raw_dsn layers (if raw_fmt !=
#' 'csv".
#' @param returntitle Logical. If TRUE, returns title(s) of the estimation
#' table(s).
#' @param title.main String. TITLE, if savedata=TRUE and/or returntitle=TRUE:
#' the complete title used for table. If title.main=NULL, the title.*
#' parameters are used to generate title string. Note: if title.ref is not
#' NULL, it is added to title.main.
#' @param title.ref String. TITLE, if savedata=TRUE and/or returntitle=TRUE:
#' the ending text of the table title (e.g., Nevada, 2004-2005). If NULL, = "".
#' @param title.dunitvar String. TITLE, if savedata=TRUE and/or
#' returntitle=TRUE: pretty name for the estimation unit variable. If NULL, =
#' unitvar.
#' @param title.estvar String. TITLE: if savedata=TRUE and/or returntitle=TRUE:
#' pretty name for the estimate variable. If NULL, title.estvar = estvar.name.
#' @param title.filter String. TITLE, if savedata=TRUE and/or returntitle=TRUE:
#' pretty name for filter(s). If title.filter=NULL, a default is generated from
#' cfilter.  If title.filter="", no title.filter is used.
#' @param save4testing Logical. If TRUE, saves intermediate steps as R objects
#' to outfolder for testing (pdomdat, dunitlut).
#' @param ...  Parameters for modSApop() if SApopdat is NULL.
#' @return \item{est}{ Data frame. Tree estimates and percent sampling error by
#' domain.  Estimates are based on the SApackage and SAmethod parameters
#' defined. } \item{titlelst}{ List. List of titles used for table output. }
#' \item{raw}{ List of raw data. If rawdata=TRUE, a list including raw data
#' components used for calculating estimate. } \item{dunit.multest}{ Data
#' frame. Table comparing different estimation strategies for SAE. }
#' 
#' Raw data
#' 
#' \item{domdat}{ Data frame. Domain-level data used for estimation. }
#' \item{estvar}{ String. Name of estimation variable. } \item{estvar.filter}{
#' String. Logical filter specified for tree data. } \item{dunit.totest}{
#' String. Table of estimates, including more details. }
#' @note
#' 
#' ADJUSTMENT FACTOR:\cr The adjustment factor is necessary to account for
#' nonsampled conditions.  For model-based estimation, we calculate adjustment
#' factors by plot.
#' 
#' It is calculated by dividing 1 / summed condition proportions by plot. An
#' adjustment factor is determined for each tree based on the size of the plot
#' it was measured on. This is identified using TPA_UNADJ as follows:
#' 
#' \tabular{llr}{ \tab \bold{PLOT SIZE} \tab \bold{TPA_UNADJ} \cr \tab SUBPLOT
#' \tab 6.018046 \cr \tab MICROPLOT \tab 74.965282 \cr \tab MACROPLOT \tab
#' 0.999188 \cr }
#' 
#' If ACI=FALSE, only nonsampled forest conditions are accounted for in the
#' adjustment factor. \cr If ACI=TRUE, the nonsampled nonforest conditions are
#' removed as well and accounted for in adjustment factor.  This is if you are
#' interested in estimates for all lands or nonforest lands in the
#' All-Condition-Inventory.
#' 
#' Common tree filters for estvar.filter: \cr
#' 
#' \tabular{llr}{ \tab \bold{FILTER} \tab \bold{DESCRIPTION} \cr \tab "STATUSCD
#' == 1" \tab Live trees \cr \tab "STATUSCD == 2" \tab Dead trees \cr \tab
#' "TPAMORT_UNADJ > 0" \tab Mortality trees \cr \tab "STATUSCD == 2 & DIA >=
#' 5.0" \tab Dead trees >= 5.0 inches diameter \cr \tab "STATUSCD == 2 &
#' AGENTCD == 30" \tab Dead trees from fire \cr }
#' @author Tracey S. Frescino, Paul L. Patterson, Elizabeth A. Freeman
#' @references Breidenbach, J. 2018. JoSAE: Unit-Level and Area-Level Small
#' Area Estimation.  R package version 0.3.0.
#' https://CRAN.R-project.org/package=JoSAE.
#' 
#' Molina I, Marhuenda Y. 2015. sae: An R Package for Small Area Estimation.
#' The R Journal 7(1), 81-98.
#' https://journal.r-project.org/archive/2015/RJ-2015-007/RJ-2015-007.
#' @keywords data
#' @export modSAest
modSAest <- function(SApopdatlst=NULL, SAdomsdf=NULL, prednames=NULL, 
	SApackage="JoSAE", SAmethod="area", esttype="TREE", totals=FALSE, 
	estseed="none", largebnd.unique=NULL, landarea="FOREST", pcfilter=NULL, 
	estvar=NULL, estvar.filter=NULL, allin1=FALSE, 
	metric=FALSE, variable.select=TRUE, estround=3, pseround=3, 
	estnull=0, psenull="--", divideby=NULL, savedata=FALSE, 
	savesteps=FALSE, rawdata=FALSE, rawonly=FALSE, multest=TRUE, 
	addSAdomsdf=TRUE, SAdomvars=NULL, outfolder=NULL, outfn.pre=NULL, 
	outfn.date=FALSE, addtitle=TRUE, raw_fmt="csv", raw_dsn="rawdata", 
	savemultest=FALSE, multest_fmt="csv", multest_outfolder=NULL, 
	multest_dsn=NULL, multest_layer=NULL, multest.append=FALSE, 
	multest.AOIonly=FALSE, overwrite_dsn=FALSE, overwrite_layer=TRUE, 
	append_layer=FALSE, returntitle=FALSE, title.main=NULL, title.ref=NULL, 
	title.dunitvar=NULL, title.estvar=NULL, title.filter=NULL, 
	save4testing=FALSE, ...){


  ######################################################################################
  ## DESCRIPTION: 
  ## Generates model-assisted estimates by domain (and estimation unit)
  ## if saveraw...  and raw_fmt = 'csv', a new folder is created within the outfolder
  ##			named as raw_dsn. If raw_fmt != 'csv', a database is created
  ##			within the outfolder names as raw_dsn. 
  ######################################################################################
  gui <- FALSE
  returnlst <- list()
  set.seed(66)

  ## Check input parameters
#  input.params <- names(as.list(match.call()))[-1]
#  formallst <- c(names(formals(FIESTA::modSAtree)),
#		names(formals(FIESTA::modSApop))) 
#  if (!all(input.params %in% formallst)) {
#    miss <- input.params[!input.params %in% formallst]
#    stop("invalid parameter: ", toString(miss))
#  }

  ## CHECK GUI - IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  if (nargs() == 0 && is.null(SApopdat)) {
    gui <- TRUE
  } 

  ## If gui.. set variables to NULL
  if (gui) { 
    tree=landarea <- NULL
    if (!row.FIAname) row.FIAname <- NULL
    if (!col.FIAname) col.FIAname <- NULL
  }

  ## Set global variables
  ONEUNIT=n.total=n.strata=strwt=TOTAL=AOI=rowvar.filter=colvar.filter=
	title.rowvar=title.colvar=TOTAL=JoSAE=JU.EBLUP=JFH=JoSAE.se=
	JU.EBLUP.se.1=pse=AREAUSED=JoSAE.pse=JoSAE.total=treef=seedf <- NULL


  ##################################################################
  ## INITIALIZE SETTINGS
  ##################################################################
# divideby=NULL
# allin1=FALSE
# addtitle=FALSE
# returntitle=TRUE
# rawdata=TRUE
# estround=0
# pseround=3
# rowvar=NULL
# colvar=NULL
# row.FIAname=FALSE
# col.FIAname=FALSE
# row.orderby=NULL
# col.orderby=NULL
# row.add0=FALSE
# col.add0=FALSE
# rowlut=NULL
# collut=NULL
# rowgrp=FALSE
# rowgrpnm=NULL
# rowgrpord=NULL
# title.rowvar=NULL
# title.colvar=NULL
# title.main=NULL
# title.ref=NULL
# title.rowvar=NULL
# title.colvar=NULL
# title.dunitvar=NULL
# title.estvar=NULL
# title.filter=NULL

  ## SET OPTIONS
  options.old <- options()
  options(scipen=8) # bias against scientific notation
  on.exit(options(options.old), add=TRUE) 
  title.rowgrp <- NULL
  pvars2keep <- c("DOMAIN", "AOI")
  returnSApopdat <- TRUE
  sumunits=FALSE
  prior=NULL

  rowvar=NULL
  colvar=NULL
  row.FIAname=FALSE
  col.FIAname=FALSE
  row.orderby=NULL
  col.orderby=NULL
  row.add0=FALSE
  col.add0=FALSE
  rowlut=NULL
  collut=NULL
  rowgrp=FALSE
  rowgrpnm=NULL
  rowgrpord=NULL 
  showsteps=FALSE
  sumunits=FALSE

  ## Check SApackage 
  SApackagelst <- c("JoSAE", "sae")
  SApackage <- pcheck.varchar(var2check=SApackage, varnm="SApackage", gui=gui, 
		checklst=SApackagelst, caption="SApackage", multiple=FALSE, stopifnull=TRUE)

  ## Check for JoSAE library
  if (SApackage == "JoSAE") {
    if (!"JoSAE" %in% rownames(installed.packages())) {
	 message("SApackage JoSAE requires package JoSAE")
    }
  } else {
    if (!"sae" %in% rownames(installed.packages())) {
	 message("SApackage sae requires package sae")
    }
  }

  ## Check SAmethod 
  SAmethodlst <- c("unit", "area", "combo")
  SAmethod <- pcheck.varchar(var2check=SAmethod, varnm="SAmethod", gui=gui, 
		checklst=SAmethodlst, caption="SAmethod", multiple=FALSE, stopifnull=TRUE)

  ###################################################################################
  ## Check data and generate population information 
  ###################################################################################
  if (is.null(SApopdatlst)) {
    stop("need to include SApopdatlst... from modSApop")
    #SApopdatlst <- modSApop(gui=gui, prednames=prednames, ...)
  } else {
    if (class(SApopdatlst) == "list") {
      list.items <- c("condx", "pltcondx", "treex", "cuniqueid", "condid", 
		"tuniqueid", "ACI.filter", "dunitarea", "dunitvar", "dunitlut",
		"prednames", "plotsampcnt", "condsampcnt")
      popchk <- tryCatch(pcheck.object(SApopdatlst, list.items=list.items),
     	 	error=function(e) {
			return(NULL) })
      if (!is.null(popchk)) {
        SApopdatlst <- list(SApopdatlst)
      }
    }
    returnSApopdat <- FALSE
  }

    
  ###################################################################################
  ## Check output parameters 
  ###################################################################################
  outparams <- check.outparams(esttype=esttype, totals=totals,
	allin1=allin1, estround=estround, pseround=pseround, divideby=divideby,
 	addtitle=addtitle, returntitle=returntitle, rawdata=rawdata, rawonly=rawonly, 
	savedata=savedata, outfolder=outfolder, overwrite_dsn=overwrite_dsn, 
	overwrite_layer=overwrite_layer, outfn.pre=outfn.pre, outfn.date=outfn.date, 
	append_layer=append_layer, raw_fmt=raw_fmt, raw_dsn=raw_dsn, gui=gui)
  sumunits <- outparams$sumunits
  allin1 <- outparams$allin1
  estround <- outparams$estround
  pseround <- outparams$pseround
  divideby <- outparams$divideby
  addtitle <- outparams$addtitle
  returntitle <- outparams$returntitle
  rawdata <- outparams$rawdata
  rawonly <- outparams$rawonly
  savedata <- outparams$savedata
  outfolder <- outparams$outfolder
  overwrite_layer <- outparams$overwrite_layer
  append_layer <- outparams$append_layer
  layer.pre <- outparams$layer.pre
  raw_fmt <- outparams$raw_fmt
  raw_dsn <- outparams$raw_dsn
  rawfolder <- outparams$rawfolder
  totals <- outparams$totals

  ## Check multest 
  ########################################################
  multest <- pcheck.logical(multest, varnm="multest", 
		title="Multiple estimates?", first="YES", gui=gui, stopifnull=TRUE)

  ## Check output for multest 
  ########################################################
  if (savedata || savemultest) {
    fmtlst <- c("sqlite", "sqlite3", "db", "db3", "gpkg", "csv", "gdb")

    if (multest) {
      multest_outfolder <- pcheck.outfolder(multest_outfolder, gui)
      if (is.null(multest_outfolder)) multest_outfolder <- outfolder
      multest.append <- pcheck.logical(multest.append, varnm="multest.append", 
		title="Append multest data?", first="NO", gui=gui) 

      multest_fmt <- pcheck.varchar(var2check=multest_fmt, varnm="multest_fmt", 
		checklst=fmtlst, gui=gui, caption="Output multest format?") 
      if (multest_fmt == "csv") {
        multest_dsn <- NULL
      } else {
        if (is.null(multest_dsn)) {
          multest_dsn <- paste0("SAmultest_", SApackage, ".", multest_fmt)
#        }
#        if (multest_fmt == "gdb") {
#          multest_dsn <- DBtestESRIgdb(gdbfn=multest_dsn, outfolder=outfolder, 
#			overwrite=overwrite_dsn, showlist=FALSE, returnpath=FALSE)
#        }	else if (multest_fmt %in% c("sqlite", "gpkg")) {
#          gpkg <- ifelse(multest_fmt == "gpkg", TRUE, FALSE)
#          if (multest.append || !overwrite_dsn) {
#            multest_dsn <- DBtestSQLite(SQLitefn=multest_dsn, gpkg=gpkg, outfolder=outfolder, 
#			showlist=FALSE, returnpath=FALSE, createnew=TRUE)
#          } else {
#            multest_dsn <- DBcreateSQLite(SQLitefn=multest_dsn, gpkg=gpkg, outfolder=outfolder, 
#			overwrite=overwrite_dsn, returnpath=FALSE, outfn.date=outfn.date)
#          }
        }	
      }
    }
  }
  if (savesteps) {
    outfolder <- pcheck.outfolder(outfolder) 
    stepfolder <- file.path(outfolder, "SApred_steps")
    if (!dir.exists(stepfolder)) {
      dir.create(stepfolder)
    }
  }

  #####################################################################################
  ## GENERATE ESTIMATES
  #####################################################################################
  #setnames(cdomdat, dunitvar, "DOMAIN")


  multestlst <- list()
  predselectlst <- list()
  dunitareabind <- {}
  if (addSAdomsdf) {
    SAdomsdfbind <- {}
  }
  if (save4testing) {
    pdomdatlst <- list()
    dunitlutlst <- list()
  }
 
  for (i in 1:length(SApopdatlst)) {
    SApopdatnm <- names(SApopdatlst)[i]
    if (is.null(SApopdatnm)) {
      SApopdatnm <- paste0("SApopdat", i)
    }
    SApopdat <- SApopdatlst[[i]]
    SApopdat <- pcheck.object(SApopdat, "SApopdat", list.items=list.items)
    if (is.null(SApopdat)) {
      break
    }
    message("generating estimates for... ", SApopdatnm)

    SAdomsdf <- SApopdat$SAdomsdf
    condx <- setDT(copy(SApopdat$condx))
    pltcondx <- copy(SApopdat$pltcondx)
    treex <- copy(SApopdat$treex)
    seedx <- copy(SApopdat$seedx)
    if (is.null(treex) && is.null(seedx)) {
      stop("must include tree data for tree estimates")
    }
    cuniqueid <- SApopdat$cuniqueid
    condid <- SApopdat$condid
    tuniqueid <- SApopdat$tuniqueid
    ACI.filter <- SApopdat$ACI.filter
    dunitarea <- setDT(SApopdat$dunitarea)
    areavar <- SApopdat$areavar
    areaunits <- SApopdat$areaunits
    dunitvar <- SApopdat$dunitvar
    dunitvar2 <- SApopdat$dunitvar2
    dunitlut <- data.table(SApopdat$dunitlut)
    plotsampcnt <- SApopdat$plotsampcnt
    condsampcnt <- SApopdat$condsampcnt
    states <- SApopdat$states
    invyrs <- SApopdat$invyrs
    adj <- SApopdat$adj
    estvar.area <- SApopdat$estvar.area
    predfac <- SApopdat$predfac

    ## check smallbnd.dom
    ########################################################
    smallbnd.dom <- "DOMAIN"
#    if (is.null(smallbnd.dom)) {
#      smallbnd.dom <- "DOMAIN"
#    }
#    smallbnd.dom <- pcheck.varchar(var2check=smallbnd.dom, 
#    		varnm="smallbnd.dom", checklst=names(dunitlut), 
#		caption="smallbnd attribute for output?", multiple=FALSE)

    ## check SAdomsdf
    ########################################################
    SAdomsdf <- pcheck.table(SAdomsdf, tabnm="SAdomsdf", caption="SAdoms?")
    if (addSAdomsdf) {
      if (is.null(SAdomsdf)) {
        message("need to add SAdomsdf when addSAdomsdf = TRUE")
        addSAdomsdf <- FALSE
      } else {
        SAdomsdfbind <- rbind(SAdomsdfbind, SAdomsdf)
      }
    } 
 
    ## Check prednames
    if (is.null(prednames)) {
      prednames <- SApopdat$prednames
    } else {
      if (!all(prednames %in% SApopdat$prednames)) {
        stop("invalid prednames... must be in: ", toString(SApopdat$prednames))
      }
    }

    ########################################
    ## Check area units
    ########################################
    unitchk <- pcheck.areaunits(unitarea=dunitarea, areavar=areavar, 
			areaunits=areaunits, metric=metric)
    dunitarea <- unitchk$unitarea
    areavar <- unitchk$areavar
    areaunits <- unitchk$outunits
    dunitareabind <- rbind(dunitareabind, unitchk$unitarea)
    

    ###################################################################################
    ## Check filter parameters and apply plot and condition filters
    ###################################################################################
    estdat <- check.estfilters(esttype=esttype, pltcondf=pltcondx, 
		cuniqueid=cuniqueid, treex=treex, seedx=seedx, estseed=estseed, 
		landarea=landarea, ACI.filter=ACI.filter, pcfilter=pcfilter)
    if (is.null(estdat)) return(NULL)
    pltcondf <- estdat$pltcondf
    landarea <- estdat$landarea
    if (esttype %in% c("TREE", "RATIO")) {
      treef <- estdat$treef
      seedf <- estdat$seedf
      estseed <- estdat$estseed
    }
    if (esttype == "P2VEG") {
      vcondf <- estdat$vcondf
    }

 
    ###################################################################################
    ### GET ROW AND COLUMN INFO FROM condf
    ###################################################################################
    if (!sumunits) col.add0 <- TRUE
    if (!is.null(rowvar) && rowvar == "TOTAL") rowvar <- NULL
    rowcolinfo <- check.rowcol(gui=gui, esttype=esttype, treef=treef, seedf=seedf,
		condf=pltcondf, cuniqueid=cuniqueid, rowvar=rowvar, rowvar.filter=rowvar.filter, 
		colvar=colvar, colvar.filter=colvar.filter, row.FIAname=row.FIAname, 
		col.FIAname=col.FIAname, row.orderby=row.orderby, col.orderby=col.orderby,
 		row.add0=row.add0, col.add0=col.add0, title.rowvar=title.rowvar, 
		title.colvar=title.colvar, rowlut=rowlut, collut=collut, rowgrp=rowgrp, 
		rowgrpnm=rowgrpnm, rowgrpord=rowgrpord, landarea=landarea) 
    treef <- rowcolinfo$treef
    seedf <- rowcolinfo$seedf
    condf <- rowcolinfo$condf
    uniquerow <- rowcolinfo$uniquerow
    uniquecol <- rowcolinfo$uniquecol
    domainlst <- rowcolinfo$domainlst
    rowvar <- rowcolinfo$rowvar
    colvar <- rowcolinfo$colvar
    row.orderby <- rowcolinfo$row.orderby
    col.orderby <- rowcolinfo$col.orderby
    row.add0 <- rowcolinfo$row.add0
    col.add0 <- rowcolinfo$col.add0
    title.rowvar <- rowcolinfo$title.rowvar
    title.colvar <- rowcolinfo$title.colvar
    bytdom <- rowcolinfo$bytdom
    tdomvar <- rowcolinfo$tdomvar
    tdomvar2 <- rowcolinfo$tdomvar2
    grpvar <- rowcolinfo$grpvar

    #rm(rowcolinfo)  

    ## Generate a uniquecol for estimation units
    if (!sumunits && colvar == "NONE") {
      uniquecol <- data.table(dunitarea[[dunitvar]])
      setnames(uniquecol, dunitvar)
      uniquecol[[dunitvar]] <- factor(uniquecol[[dunitvar]])
    }

    if (esttype == "AREA") {
      estvar.name <- "AREA"
      if (adj != "none") {
        estvar.name <- paste0(estvar.name, "_ADJ")
      }
      estvarunits <- areaunits

      setkeyv(condx, c(cuniqueid, condid))
      setkeyv(condf, c(cuniqueid, condid))
      cdomdat <- merge(condx, condf, by=c(cuniqueid, condid), all.x=TRUE)
      cdomdat[, (estvar.name) := ifelse(is.na(TOTAL), 0, get(estvar.area))] 
  
    } else {
      #####################################################################################
      ### Get estimation data from tree table, with plot-level adjustment for nonresponse
      #####################################################################################
      adjtree <- ifelse(adj %in% c("samp", "plot"), TRUE, FALSE)
      treedat <- check.tree(gui=gui, treef=treef, seedf=seedf, estseed=estseed, 
		bycond=TRUE, condf=condf, bytdom=bytdom, tuniqueid=tuniqueid, 
		cuniqueid=cuniqueid, esttype=esttype, estvarn=estvar, 
		estvarn.filter=estvar.filter, esttotn=TRUE, tdomvar=tdomvar, 
		adjtree=adjtree, metric=metric)
      if (is.null(treedat)) return(NULL) 
      estvar <- treedat$estvar
      estvar.name <- treedat$estvar.name
      estvar.filter <- treedat$estvar.filter
      tdomvarlst <- treedat$tdomvarlst
      estvarunits <- treedat$estunits

      tdomdat <- treedat$tdomdat
      if (rowvar != "TOTAL") {
        if (!row.add0 && any(tdomdat[[rowvar]] == 0)) {
          tdomdat <- tdomdat[tdomdat[[rowvar]] != 0,]
        }
        if (colvar != "NONE") {
          if (!col.add0 && any(tdomdat[[colvar]] == 0)) {
            tdomdat <- tdomdat[tdomdat[[colvar]] != 0,]
          }
        }
      }
      cdomdat <- merge(condx, tdomdat, by=c(cuniqueid, condid), all.x=TRUE)
      #cdomdat <- DT_NAto0(tdomdat, estvar.name, 0)
    }
 
    #####################################################################################
    ## GENERATE ESTIMATES
    #####################################################################################
    dunit_totest=dunit_rowest=dunit_colest=dunit_grpest=rowunit=totunit <- NULL
    response <- estvar.name
    #setnames(cdomdat, dunitvar, "DOMAIN")

    if (i == 1) {
      message("getting estimates for ", response, "...")
      message("using the following predictors...", toString(prednames))
    }

    ############################################################################
    ## Generate models
    ############################################################################
    ## Note: not sure why you would want to run by largebnd.unique
    ## Maybe, if generated SAdoms by province, but want to run by section.

    ## check largebnd.unique
    ########################################################
    if (!is.null(largebnd.unique) && !is.null(SAdomsdf)) {
      cdomdat <- merge(cdomdat, 
		unique(setDT(SAdomsdf)[, c(smallbnd.dom, largebnd.unique), with=FALSE]),
 		by=smallbnd.dom)
      #addSAdomsdf <- TRUE
      #SAdomvars <- unique(c(SAdomvars, largebnd.unique))
      lunique <- largebnd.unique
    } else {
      cdomdat$LARGEBND <- 1
      lunique <- "LARGEBND"
    }

    ## get unique largebnd values
    largebnd.vals <- sort(unique(cdomdat[[lunique]]))
    largebnd.vals <- largebnd.vals[table(cdomdat[[lunique]]) > 30]

    ## Get estimate for total
    ######################################
    ## Sum estvar.name by dunitvar (DOMAIN), plot, domain
    tdomdattot <- setDT(cdomdat)[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(lunique, dunitvar, cuniqueid, "TOTAL", prednames), 
		.SDcols=estvar.name]
    domain <- "TOTAL"

    ## get estimate by domain, by largebnd value
    #message("generating JoSAE unit-level estimates for ", response, " using ", SApackage, "...")

#dunitlut <- data.table(SApopdat$dunitlut)
#dat=tdomdattot
#largebnd.val=largebnd.vals
#domain="TOTAL"

    dunit_multestlst <- 
	tryCatch(
		lapply(largebnd.vals, SAest.large, 
			dat=tdomdattot, cuniqueid=cuniqueid, 
			largebnd.unique=lunique, dunitlut=dunitlut, dunitvar=dunitvar,
			prednames=prednames, domain="TOTAL",
			response=response, showsteps=showsteps, savesteps=savesteps,
			stepfolder=stepfolder, prior=prior, variable.select=variable.select),
     	 error=function(e) {
			message("error with estimates of ", response, "...")
			message(e, "\n")
			return(NULL) })
    if (length(largebnd.vals) > 1) {
      dunit_multest <- do.call(rbind, do.call(rbind, dunit_multestlst)[,"est.large"])
      predselect.unit <- do.call(rbind, dunit_multestlst)[,"predselect.unit"]
      predselect.area <- do.call(rbind, dunit_multestlst)[,"predselect.area"]
      #names(prednames.select) <- largebnd.vals
      if (save4testing) {
        pdomdat <- do.call(rbind, do.call(rbind, dunit_multestlst)[,"pltdat.dom"])
        dunitlut <- do.call(rbind, do.call(rbind, dunit_multestlst)[,"dunitlut.dom"])
      }
    } else {
      dunit_multest <- do.call(rbind, dunit_multestlst)[,"est.large"]$est.large
      predselect.unit <- do.call(rbind, dunit_multestlst)[,"predselect.unit"]$predselect.unit
      predselect.area <- do.call(rbind, dunit_multestlst)[,"predselect.area"]$predselect.area
      if (save4testing) {
        pdomdat <- do.call(rbind, dunit_multestlst)[,"pltdat.dom"]$pltdat.dom
        dunitlut <- do.call(rbind, dunit_multestlst)[,"dunitlut.dom"]$dunitlut.dom
      }
    }

    multestlst[[SApopdatnm]] <- dunit_multest
    predselectlst[[SApopdatnm]] <- 
		list(predselect.unit=predselect.unit, predselect.area=predselect.area)
    if (save4testing) {
      ## Merge SAdom attributes to dunit_totest
      if (addSAdomsdf) {
        pdomdat <- merge(setDT(SAdomsdf)[, 
			unique(c(dunitvar, "AOI", lunique, SAdomvars)), with=FALSE], 
			pdomdat, by=c(dunitvar, "AOI"))
        dunitlut <- merge(setDT(SAdomsdf)[, 
			unique(c(dunitvar, "AOI", lunique, SAdomvars)), with=FALSE], 
			dunitlut, by=c(dunitvar, "AOI"))
      }
      pdomdatlst[[SApopdatnm]] <- pdomdat
      dunitlutlst[[SApopdatnm]] <- dunitlut
    }
  }    #### end SApopdat loop


  ## Generate estimates
  #################################################################################
  dunit_totest=dunit_rowest=dunit_colest=dunit_grpest=rowunit=totunit <- NULL
  response <- estvar.name

  ## rbind multiple esimates
  #########################################################
  multestdf <- do.call(rbind, multestlst)

  ## Merge SAdom attributes to multestdf
  if (addSAdomsdf && is.null(SAdomvars)) {
    SAdomvars <- unique(names(SAdomsdfbind)[!names(SAdomsdfbind) %in% multestdf])
    multestdf[, AOI := NULL]
    multestdf <- merge(setDF(SAdomsdfbind)[,SAdomvars], multestdf, by=dunitvar)
    multestdf <- multestdf[order(-multestdf$AOI, multestdf[[dunitvar]]),]
  } else if (addSAdomsdf && !is.null(SAdomvars)) {
    SAdomvars <- SAdomvars[SAdomvars %in% names(SAdomsdfbind)]
    SAdomvars <- unique(SAdomvars[!SAdomvars %in% multestdf])
    
    if (length(SAdomvars) == 0) stop("invalid SAdomvars")
    multestdf <- merge(setDF(SAdomsdfbind)[, unique(c(dunitvar, SAdomvars))], 
					multestdf, by=dunitvar)
    multestdf <- multestdf[order(-multestdf$AOI, multestdf[[dunitvar]]),]
  } else {
    multestdf <- multestdf[order(-multestdf$AOI, multestdf[[dunitvar]]),]
  }

  if (SAmethod == "unit") {
    nhat <- "JU.EBLUP"
    nhat.se <- "JU.EBLUP.se.1"
    nhat.var <- "JU.EBLUP.var"
    nhat.cv <- "JU.EBLUP.cv"

  } else if (SAmethod == "area") {
    nhat <- "JFH"
    nhat.se <- "JFH.se"
    nhat.var <- "JFH.var"
    nhat.cv <- "JFH.cv"

  } else if (SAmethod == "combo") {
    nhat <- "JoSAE"
    nhat.se <- "JoSAE.se"
    nhat.var <- "JoSAE.var"
    nhat.cv <- "JoSAE.cv"
  } 

  ## Subset multest to estimation output
  dunit_totest <- setDT(multestdf)[AOI==1, 
		unique(c(dunitvar, nhat, nhat.se, "NBRPLT.gt0")), with=FALSE]
  setkeyv(dunit_totest, dunitvar)

  ## Merge dunitarea
  tabs <- check.matchclass(dunitareabind, dunit_totest, dunitvar)
  dunitareabind <- tabs$tab1
  dunit_totest <- tabs$tab2
  dunit_totest <- merge(dunit_totest, 
		dunitareabind[, c(dunitvar, "AREAUSED"), with=FALSE], by=dunitvar)

  if (!is.null(dunit_totest)) {
    if (totals) {
      dunit_totest <- getarea(dunit_totest, areavar=areavar, esttype=esttype,
				nhatcol=nhat, nhatcol.var=nhat.var)
      estnm <- "est"
    } else {
      dunit_totest[, (nhat.var) := get(nhat.se)^2]
      dunit_totest[, (nhat.cv) := get(nhat.se)/get(nhat)]
      dunit_totest[, pse := get(nhat.cv) * 100]
      estnm <- nhat
    }
  }

  #####################################################################################
  ### GET TITLES FOR OUTPUT TABLES
  #####################################################################################
  if (is.null(title.dunitvar)) {
    title.dunitvar <- smallbnd.dom
  }
  alltitlelst <- check.titles(esttype=esttype, estseed=estseed, 
	sumunits=sumunits, title.main=title.main, title.ref=title.ref, 
	title.rowvar=title.rowvar, title.colvar=title.colvar, 
	title.unitvar=title.dunitvar, title.filter=title.filter, 
	title.unitsn=estvarunits, title.estvarn=title.estvar, unitvar=dunitvar, 
	rowvar=rowvar, colvar=colvar, estvarn=estvar, 
	estvarn.filter=estvar.filter, addtitle=addtitle, returntitle=returntitle, 
	rawdata=rawdata, states=states, invyrs=invyrs, landarea=landarea, 
	pcfilter=pcfilter, allin1=allin1, divideby=divideby, parameters=FALSE, 
	outfn.pre=outfn.pre)
  title.dunitvar <- alltitlelst$title.unitvar
  title.est <- alltitlelst$title.est
  title.pse <- alltitlelst$title.pse
  title.estpse <- alltitlelst$title.estpse
  title.ref <- alltitlelst$title.ref
  outfn.estpse <- alltitlelst$outfn.estpse
  outfn.param <- alltitlelst$outfn.param
  if (rawdata) {
    outfn.rawdat <- alltitlelst$outfn.rawdat
    outfn.rawdat <- paste0(outfn.rawdat, "_modSA_", SApackage, "_", SAmethod) 
  } 
  ## Append name of package and method to outfile name
  outfn.estpse <- paste0(outfn.estpse, "_modSA_", SApackage, "_", SAmethod) 


  ###################################################################################
  ## GENERATE OUTPUT TABLES
  ###################################################################################
  message("getting output...")
  tabs <- est.outtabs(esttype=esttype, sumunits=sumunits, areavar=areavar, 
	unitvar=smallbnd.dom, unit_totest=dunit_totest, unit_rowest=dunit_rowest, 
	unit_colest=dunit_colest, unit_grpest=dunit_grpest, rowvar=rowvar, colvar=colvar, 
	uniquerow=uniquerow, uniquecol=uniquecol, rowgrp=rowgrp, rowgrpnm=rowgrpnm, 
	rowunit=rowunit, totunit=totunit, allin1=allin1, savedata=savedata, 
	addtitle=addtitle, title.ref=title.ref, title.colvar=title.colvar, 
	title.rowvar=title.rowvar, title.rowgrp=title.rowgrp, title.unitvar=title.dunitvar,
 	title.estpse=title.estpse, title.est=title.est, title.pse=title.pse, 
	rawdata=rawdata, rawonly=rawonly, outfn.estpse=outfn.estpse, outfolder=outfolder, 
	outfn.date=outfn.date, overwrite=overwrite_layer, estnm=estnm, estround=estround, 
	pseround=pseround, divideby=divideby, returntitle=returntitle,
	estnull=estnull, psenull=psenull) 

  est2return <- tabs$tabest
  pse2return <- tabs$tabpse

  est2return[is.na(est2return$Estimate), "Estimate"] <- estnull 
  if ("Percent Sampling Error" %in% names(est2return)) {
    est2return[is.na(est2return$"Percent Sampling Error"), 
		"Percent Sampling Error"] <- psenull 
  }

  if (!is.null(est2return)) {
    returnlst$est <- est2return
  } 
  if (!is.null(pse2return)) {
    returnlst$pse <- pse2return 
  }
  if (returntitle) {
    returnlst$titlelst <- alltitlelst
  }
 

  if (multest && !is.null(multestdf)) {
    ## Merge dunitarea
    #tabs <- check.matchclass(dunitarea, multestdf, dunitvar)
    #dunitarea <- tabs$tab1
    #dunit_multest <- tabs$tab2
 
    multestdf <- merge(multestdf, 
		dunitareabind[, c(dunitvar, "AREAUSED"), with=FALSE], by=dunitvar)
    #multestdf[, JoSAE.total := get(nhat) * AREAUSED]
    #multestdf[, JoSAE.pse := get(nhat.se)/get(nhat) * 100]

    ## Remove TOTAL column from multestdf
    if (domain == "TOTAL" && "TOTAL" %in% names(multestdf)) {
      multestdf[, TOTAL := NULL]
    }
    if (multest.AOIonly) {
      ## Subset multestdf, where AOI = 1
      multestdf <- multestdf[multestdf$AOI == 1, ]
    }
    ## Save multest table
    if (savemultest) {

      ## Remove TOTAL column from est
      if (domain == "TOTAL" && "TOTAL" %in% names(multestdf)) {
        multestdf[, TOTAL := NULL]
      }
      ## Remove column headings if appending to csv file
      if (multest.append && multest_fmt == "csv") {
        multestdf <- setDF(multestdf)
        colnames(multestdf) <- NULL
      }
      if (is.null(multest_layer)) {
        if (multest_fmt == "csv") {
          #multest_layer <- paste0("SAmultest_", SApackage, "_", response, ".csv")
          multest_layer <- paste0("SAmultest_", response, ".csv")
        } else {
          #multest_layer <- paste0(SApackage, "_", response)
          multest_layer <- response
        }
      }
 
      ## Export multestdf
      overwrite_layer <- ifelse(multest.append, FALSE, overwrite_layer)
      datExportData(multestdf, out_fmt=multest_fmt, outfolder=multest_outfolder, 
 		out_dsn=multest_dsn, out_layer=multest_layer, overwrite_layer=overwrite_layer, 
		append_layer=multest.append)
    }
  } 

  if (rawdata) {
    rawdat <- tabs$rawdat
    names(rawdat)[names(rawdat) == "unit_totest"] <- "dunit_totest"
    rawdat$domdat <- setDF(cdomdat)

    if (savedata) {
      if (!is.null(title.estpse)) {
        title.raw <- paste(title.estpse, title.ref)
      } else {
        title.raw <- title.est
      }

      for (i in 1:length(rawdat)) {
        tabnm <- names(rawdat[i])
        if (!tabnm %in% c(prednames)) {
          rawtab <- rawdat[[i]]

          outfn.rawtab <- paste0(outfn.rawdat, "_", tabnm) 
          if (tabnm %in% c("plotsampcnt", "condsampcnt")) {
            write2csv(rawtab, outfolder=rawfolder, outfilenm=outfn.rawtab, 
			outfn.date=outfn.date, overwrite=overwrite_layer)
          } else if (is.data.frame(rawtab)) {
            if (raw_fmt != "csv") {
              out_layer <- tabnm 
            } else {
              out_layer <- outfn.rawtab
            }
            datExportData(rawtab, out_fmt=raw_fmt, outfolder=rawfolder, 
 			out_dsn=raw_dsn, out_layer=out_layer, 
			overwrite_layer=overwrite_layer, add_layer=TRUE, 
			append_layer=append_layer)
          }
        }
      }
    }

    rawdat$esttype <- esttype
    rawdat$SApackage <- SApackage
    rawdat$SAmethod <- SAmethod
    rawdat$predselect.unit <- predselect.unit
    rawdat$predselect.area <- predselect.area
    rawdat$estvar <- response
    if (esttype == "TREE") {
      rawdat$estvar.filter <- estvar.filter
    }
    if (!is.null(rowvar)) rawdat$rowvar <- rowvar
    if (!is.null(colvar)) rawdat$colvar <- colvar
    rawdat$areaunits <- areaunits
    rawdat$estunits <- estvarunits
    returnlst$raw <- rawdat  
  }

  if (multest) {
    returnlst$multest <- setDF(multestdf)
  }
  if (returnSApopdat) {
    returnlst$SApopdat <- SApopdat
  }

  ## Save objects for testing
  if (save4testing) {
    message("saving object for testing")

    returnlst$pdomdat <- pdomdat
    returnlst$dunitlut <- dunitlut
    returnlst$cuniqueid <- cuniqueid
  }


  return(returnlst)
}


