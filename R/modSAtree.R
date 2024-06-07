#' Small area module - Generate small area tree estimates.
#' 
#' Generates small area estimates by domain and/or tree domain (and estimation
#' unit).
#' 
#' If variables are NULL, then it will prompt user to input variables.
#' 
#' Necessary variables:\cr \tabular{llll}{ \tab \bold{Data} \tab
#' \bold{Variable} \tab \bold{Description}\cr \tab tree \tab tuniqueid \tab
#' Unique identifier for each plot, to link to pltstrat (e.g., PLT_CN).\cr \tab
#' \tab CONDID \tab Unique identifier of each condition on plot, to link to
#' cond.  Set CONDID=1, if only 1 condition per plot.\cr \tab \tab TPA_UNADJ
#' \tab Number of trees per acre each sample tree represents (e.g. DESIGNCD=1:
#' TPA_UNADJ=6.018046 for trees on subplot; 74.965282 for trees on
#' microplot).\cr \tab cond \tab cuniqueid \tab Unique identifier for each
#' plot, to link to pltstrat (ex. PLT_CN).\cr \tab \tab CONDID \tab Unique
#' identifier of each condition on plot.  Set CONDID=1, if only 1 condition per
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
#' @param prednames String vector. Name(s) of predictor variables to use in
#' model.
#' @param SApackage String. Small area package to use ('JoSAE', 'sae', 'hbsae')
#' @param SAmethod String. Small area method to use ('unit', 'area')
#' @param estseed String. Use seedling data only or add to tree data. Seedling
#' estimates are only for counts (estvar='TPA_UNADJ')-('none', 'only', 'add').
#' @param woodland String. If woodland = 'Y', include woodland tree species  
#' where measured. If woodland = 'N', only include timber species. See 
#' FIESTA::ref_species$WOODLAND ='Y/N'. If woodland = 'only', only include
#' woodland species.
#' @param largebnd.unique String. Name of the large boundary unique identifier
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
#' @param rowvar String. Name of the row domain variable in cond or tree. If
#' only one domain, rowvar = domain variable. If more than one domain, include
#' colvar. If no domain, rowvar = NULL.
#' @param modelselect Logical. If TRUE, selects useful predictors using
#' mase:ElasticNet.
#' @param prior Function. A prior function to use for hbsae models.
#' @param na.fill String. An estimate to fill in for NA values (i.e., when 
#' model is unstable or no predictors are selected). Choose from the following 
#' list that does not include SApackage used ('NONE', 'DIR', 'JoSAE', 'sae', 
#' 'hbsae'). DIR is suggested value for no NA values. 
#' @param savedata Logical. If TRUE, saves table(s) to outfolder.
#' @param savesteps Logical. Saves graphs of predictors and response with
#' labels whether selected or not for both area- and unit-level models.
#' @param multest Logical. If TRUE, returns a data frame of SA estimates using
#' both unit-level and area-level estimates.
#' @param addSAdomsdf Logical. If TRUE, appends SAdomdf to unit.multest table
#' for output.
#' @param SAdomvars String vector. List of attributes from SAdoms to include in
#' multest output.
#' @param savemultest Logical. If TRUE, save table with area- and unit-level
#' estimates.
#' @param returntitle Logical. If TRUE, returns title(s) of the estimation
#' table(s).
#' @param table_opts List. See help(table_options()) for a list of
#' options.
#' @param title_opts List. See help(title_options()) for a list of options.
#' @param savedata_opts List. See help(savedata_options()) for a list
#' of options. Only used when savedata = TRUE.  
#' @param multest_opts List. See help(multest_options()) for a list of options.
#' Only used when multest = TRUE.
#' @param save4testing Logical. If TRUE, saves intermediate steps as R objects
#' to outfolder for testing (pdomdat, dunitlut).
#' @param gui Logical. If gui, user is prompted for parameters.
#' @param ...  Parameters for modSApop() if SApopdat is NULL.
#' 
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
#' @examples 
#' \donttest{
#' # Set up population dataset (see ?modSApop() for more information)
#' SApopdat <- modSApop(popTabs = list(tree = FIESTA::WYtree,
#'                                     cond = FIESTA::WYcond),
#'                      pltassgn = FIESTA::WYpltassgn,
#'                      pltassgnid = "CN",
#'                      dunitarea = FIESTA::WYunitarea,
#'                      dunitvar = "ESTN_UNIT",
#'                      dunitzonal = FIESTA::WYunitzonal,
#'                      prednames = c("dem", "tcc", "tpi", "tnt"),
#'                      predfac = "tnt")
#' 
#' # Use an area level Fay-Herriot model to estimate total net cubic-foot volume 
#' # of live trees (at least 5 inches diameter) 
#' modSAtree(SApopdatlst = SApopdat,
#'           SApackage = "JoSAE",        
#'           SAmethod = "unit",           
#'           landarea = "FOREST",      
#'           estvar = "VOLCFNET",         
#'           estvar.filter = "STATUSCD == 1")   
#'           
#' # Use a unit level EBLUP to estimate basal area of live trees (at least 5
#' # inches diameter) 
#' modSAtree(SApopdatlst = SApopdat,    
#'           SApackage = "JoSAE",        
#'           SAmethod = "unit",         
#'           landarea = "FOREST",      
#'           estvar = "BA",              
#'           estvar.filter = "STATUSCD == 1")  
#' }
#' @export modSAtree
modSAtree <- function(SApopdatlst = NULL, 
                      prednames = NULL, 
                      SApackage = "JoSAE", 
                      SAmethod = "area", 
                      estseed = "none",
                      woodland = "Y",
                      largebnd.unique = NULL, 
                      landarea = "FOREST", 
                      pcfilter = NULL, 
                      estvar = NULL, 
                      estvar.filter = NULL, 
                      rowvar = NULL, 
                      modelselect = FALSE, 
                      prior = function(x) 1/(sqrt(x)*(1+x)),
                      na.fill = "NONE", 
                      savedata = FALSE, 
                      savesteps = FALSE, 
                      multest = TRUE, 
                      addSAdomsdf = TRUE, 
                      SAdomvars = NULL, 
                      savemultest = FALSE, 
                      returntitle = FALSE, 
                      table_opts = NULL, 
                      title_opts = NULL, 
                      savedata_opts = NULL, 
                      multest_opts = NULL, 
                      save4testing = FALSE, 
                      gui = FALSE, 
                      ...){
  ######################################################################################
  ## DESCRIPTION: 
  ## Generates model-assisted estimates by domain (and estimation unit)
  ## if saveraw...  and raw_fmt = 'csv', a new folder is created within the outfolder
  ##			named as raw_dsn. If raw_fmt != 'csv', a database is created
  ##			within the outfolder names as raw_dsn. 
  ######################################################################################
  
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
  ONEUNIT=n.total=n.strata=strwt=TOTAL=AOI=
	title.rowvar=title.colvar=TOTAL=JoSAE=JU.EBLUP=JFH=JoSAE.se=
	JU.EBLUP.se.1=pse=AREAUSED=JoSAE.pse=JoSAE.total=treef=seedf=nhat.var <- NULL


  ## Set parameters
  title.rowgrp <- NULL
  pvars2keep <- c("DOMAIN", "AOI")
  returnSApopdat <- TRUE
  sumunits=FALSE
  SAdomsdf=multestdf_row <- NULL
  
  colvar=NULL
  col.FIAname=FALSE
  col.orderby=NULL
  col.add0=FALSE
  collut=NULL
  rowgrp=FALSE
  rowgrpnm=NULL
  rowgrpord=NULL 
  showsteps=FALSE
  sumunits=FALSE
  
  gui <- FALSE
  returnlst <- list()
  set.seed(66)
  esttype="TREE"
  rawdata <- TRUE
  lt0 <- FALSE
  
  
  ##################################################################
  ## CHECK PARAMETER NAMES
  ##################################################################
  
  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- c(names(formals(modSAtree)),
                 names(formals(modSApop))) 
  if (!all(input.params %in% formallst)) {
    miss <- input.params[!input.params %in% formallst]
    stop("invalid parameter: ", toString(miss))
  }
  
  ## Check parameter lists
  pcheck.params(input.params, table_opts=table_opts, title_opts=title_opts, 
                savedata_opts=savedata_opts, multest_opts=multest_opts)
  
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
  
  ## Set table defaults
  table_defaults_list <- formals(table_options)[-length(formals(table_options))]
  
  for (i in 1:length(table_defaults_list)) {
    assign(names(table_defaults_list)[[i]], table_defaults_list[[i]])
  }
  
  ## Set user-supplied table values
  if (length(table_opts) > 0) {
    for (i in 1:length(table_opts)) {
      if (names(table_opts)[[i]] %in% names(table_defaults_list)) {
        assign(names(table_opts)[[i]], table_opts[[i]])
      } else {
        stop(paste("Invalid parameter: ", names(table_opts)[[i]]))
      }
    }
  }
  
  ## Set title defaults
  title_defaults_list <- formals(title_options)[-length(formals(title_options))]
  
  for (i in 1:length(title_defaults_list)) {
    assign(names(title_defaults_list)[[i]], title_defaults_list[[i]])
  }
  
  ## Set user-supplied title values
  if (length(title_opts) > 0) {
    for (i in 1:length(title_opts)) {
      if (names(title_opts)[[i]] %in% names(title_defaults_list)) {
        assign(names(title_opts)[[i]], title_opts[[i]])
      } else {
        stop(paste("Invalid parameter: ", names(title_opts)[[i]]))
      }
    }
  }
  
  ## Set multest defaults
  multest_defaults_list <- formals(multest_options)[-length(formals(multest_options))]
  
  for (i in 1:length(multest_defaults_list)) {
    assign(names(multest_defaults_list)[[i]], multest_defaults_list[[i]])
  }
  
  ## Set user-supplied multest values
  if (length(multest_opts) > 0) {
    for (i in 1:length(multest_opts)) {
      if (names(multest_opts)[[i]] %in% names(multest_defaults_list)) {
        assign(names(multest_opts)[[i]], multest_opts[[i]])
      } else {
        stop(paste("Invalid parameter: ", names(multest_opts)[[i]]))
      }
    }
  }


  ##################################################################
  ## CHECK PARAMETER INPUTS
  ##################################################################

  ## Check SApackage 
  SApackagelst <- c("JoSAE", "sae", "hbsae", "spAbundance")
  SApackage <- pcheck.varchar(var2check=SApackage, varnm="SApackage", gui=gui, 
		checklst=SApackagelst, caption="SApackage", multiple=FALSE, stopifnull=TRUE)

  ## Check SAmethod 
  SAmethodlst <- c("unit", "area")
  SAmethod <- pcheck.varchar(var2check=SAmethod, varnm="SAmethod", gui=gui, 
		checklst=SAmethodlst, caption="SAmethod", multiple=FALSE, stopifnull=TRUE)

  if (SApackage == "sae" && SAmethod == "unit") {
    stop("sae unit-level estimates are not available\n")
  }

  ## Check na.fill 
  na.filllst <- c("NONE", "DIR", "JoSAE", "sae", "hbsae")
  na.filllst <- na.filllst[na.filllst != SApackage]
  na.fill <- pcheck.varchar(var2check=na.fill, varnm="na.fill", gui=gui, 
                            checklst=na.filllst, caption="na.fill", 
                            multiple=FALSE, stopifnull=TRUE)
  
  
  ###################################################################################
  ## Check data and generate population information 
  ###################################################################################
  if (is.null(SApopdatlst)) {
    stop("need to include SApopdatlst... from modSApop")
    #SApopdatlst <- modSApop(gui=gui, prednames=prednames, ...)
  } else {
    if (!is(SApopdatlst, "list")) {
      SApopdatlst <- list(SApopdatlst)
    } else if ("condx" %in% names(SApopdatlst)) {
      SApopdatlst <- list(SApopdatlst)
    }  

    if (is(SApopdatlst, "list")) {
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
                  allin1=allin1, estround=estround, pseround=pseround, 
                  divideby=divideby, addtitle=addtitle, returntitle=returntitle, 
                  rawdata=rawdata, rawonly=rawonly, savedata=savedata, 
                  outfolder=outfolder, overwrite_dsn=overwrite_dsn, 
                  overwrite_layer=overwrite_layer, outfn.pre=outfn.pre, 
                  outfn.date=outfn.date, append_layer=append_layer, 
                  raw_fmt=raw_fmt, raw_dsn=raw_dsn, gui=gui)
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
      if (is.null(multest_outfolder)) {
        multest_outfolder <- rawfolder
      } else {
        multest_outfolder <- pcheck.outfolder(multest_outfolder, gui)
      }
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
  #setnames(tdomdat, dunitvar, "DOMAIN")


  ## Define empty lists
  estlst <- list()
  predselectlst <- list()
  if (multest || SAmethod == "unit") {
    predselectlst.unit <- list()
  }
  if (multest || SAmethod == "area") {
    predselectlst.area <- list()
  }
  SAobjlst <- list()
  dunitareabind <- {}
  if (addSAdomsdf) {
    SAdomsdfbind <- {}
  }
  if (save4testing) {
    pdomdatlst <- list()
    dunitlutlst <- list()
  }

  if (!is.null(rowvar)) {
    estlst_row <- list()
    if (multest || SAmethod == "unit") {
      predselectlst.unit_row <- list()
    }
    if (multest || SAmethod == "area") {
      predselectlst.area_row <- list()
    }
    SAobjlst_row <- list()
    if (save4testing) {
      pdomdatlst_row <- list()
      dunitlutlst_row <- list()
    }
  }

  ## Loop through SApopdatlst
  #############################################
  #largebnd.unique2 <- largebnd.unique
  
  for (i in 1:length(SApopdatlst)) {
    SApopdatnm <- names(SApopdatlst)[i]
    if (is.null(SApopdatnm)) {
      if (length(SApopdatlst) > 1) {
        SApopdatnm <- paste0("SApopdat", i)
      } else {
        SApopdatnm <- "SApopdat"
      }
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
    pltassgnx <- SApopdat$pltassgnx
    pltassgnid <- SApopdat$pltassgnid
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
    pop_fmt <- SApopdat$pop_fmt
    pop_dsn <- SApopdat$pop_dsn


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
        if (any(prednames %in% SApopdat$predfac)) {
          predfacnames <- prednames[prednames %in% SApopdat$predfac]
          for (nm in predfacnames) {           
            prednames[prednames == nm] <- SApopdat$prednames[grepl(nm, SApopdat$prednames)]
          }
        } else {
          stop("invalid prednames... must be in: ", toString(SApopdat$prednames))
        }
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
    
    if (is.null(key(dunitarea))) {
      setkeyv(dunitarea, dunitvar)
    }
    
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
    rowcolinfo <- 
      check.rowcol(gui=gui, esttype=esttype, 
	                 treef=treef, seedf=seedf, 
                   condf=pltcondf, cuniqueid=cuniqueid,                      
					         tuniqueid=tuniqueid, estseed=estseed,
					         rowvar=rowvar, colvar=colvar, 
                   row.FIAname=row.FIAname, col.FIAname=col.FIAname, 
                   row.orderby=row.orderby, col.orderby=col.orderby, 
                   row.add0=row.add0, col.add0=col.add0, 
                   title.rowvar=title.rowvar, title.colvar=title.colvar, 
                   rowlut=rowlut, collut=collut, rowgrp=rowgrp, 
                   rowgrpnm=rowgrpnm, rowgrpord=rowgrpord, 
                   landarea=landarea, states=states, 
					         cvars2keep="COND_STATUS_CD") 
    treef <- rowcolinfo$treef
    seedf <- rowcolinfo$seedf
    condf <- rowcolinfo$condf
    uniquerow <- rowcolinfo$uniquerow
    uniquecol <- rowcolinfo$uniquecol
    domainlst <- rowcolinfo$domainlst
    #rowvar <- rowcolinfo$rowvar
    #colvar <- rowcolinfo$colvar
    #rowvarnm <- rowcolinfo$rowvarnm
    #colvarnm <- rowcolinfo$colvarnm
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
	
	
    if (esttype == "TREE") {
      #####################################################################################
      ### Get estimation data from tree table, with plot-level adjustment for nonresponse
      #####################################################################################
      adjtree <- ifelse(adj %in% c("samp", "plot"), TRUE, FALSE)
      treedat <- check.tree(gui=gui, treef=rowcolinfo$treef, 
                        seedf=rowcolinfo$seedf, estseed=estseed, woodland=woodland,
                        bycond=TRUE, condf=rowcolinfo$condf, bytdom=rowcolinfo$bytdom, 
                        tuniqueid=tuniqueid, cuniqueid=cuniqueid, esttype=esttype, 
                        estvarn=estvar, estvarn.filter=estvar.filter, esttotn=TRUE, 
                        tdomvar=rowcolinfo$tdomvar, adjtree=adjtree, metric=metric)
      if (is.null(treedat)) return(NULL) 
      tdomdat <- treedat$tdomdat
	  
      ## Merge tdomdat with condx
      xchk <- check.matchclass(condx, tdomdat, c(cuniqueid, condid))
      condx <- xchk$tab1
      tdomdat <- xchk$tab2
      tdomdat <- merge(condx, tdomdat, by=c(cuniqueid, condid), all.x=TRUE)	  
	  
      estvar <- treedat$estvar
      estvar.name <- treedat$estvar.name
      estvar.filter <- treedat$estvar.filter
      tdomvarlst <- treedat$tdomvarlst
      estvarunits <- treedat$estunits
 
      ## Check for matching levels in x and xunique
      if (!is.null(uniquerow)) {
        chklevels <- checklevels(x = tdomdat, 
	                               uniquex = uniquerow,
							                   xvar = rowvar) 
	      tdomdat <- chklevels$x
        uniquerow <- chklevels$uniquex	
      }
      if (!is.null(uniquecol)) {
        chklevels <- checklevels(x = tdomdat, 
	                               uniquex = uniquecol,
							                   xvar = colvar) 
	      tdomdat <- chklevels$x
        uniquecol <- chklevels$uniquex	
      }
    }
	
    ## Generate a uniquecol for estimation units
    if (!sumunits && rowcolinfo$colvar == "NONE") {
      uniquecol <- data.table(dunitarea[[dunitvar]])
      setnames(uniquecol, dunitvar)
      uniquecol[[dunitvar]] <- factor(uniquecol[[dunitvar]])
    }

    #####################################################################################
    ## GENERATE ESTIMATES
    #####################################################################################
    dunit_totest=dunit_rowest=dunit_colest=dunit_grpest=rowunit=totunit <- NULL
    response <- estvar.name
    #setnames(tdomdat, dunitvar, "DOMAIN")

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
    vars2keep <- NULL
    if (!is.null(largebnd.unique)) {
      if (largebnd.unique %in% names(tdomdat) && largebnd.unique %in% names(pltassgnx)) {
        tdomdat <- merge(pltassgnx, tdomdat, 
                         by.x = c(largebnd.unique, pltassgnid, "DOMAIN"), 
                         by.y = c(largebnd.unique, cuniqueid, "DOMAIN"), , all.x=TRUE)
      } else if (largebnd.unique %in% names(pltassgnx)) {
        tdomdat <- merge(pltassgnx, tdomdat, 
                         by.x = c(pltassgnid, "DOMAIN"), 
                         by.y = c(cuniqueid, "DOMAIN"), all.x=TRUE)
      } else if (!is.null(SAdomsdf)) {
        tdomdat <- merge(tdomdat, 
		        unique(setDT(SAdomsdf)[, c(smallbnd.dom, largebnd.unique), with=FALSE]),
 		        by=smallbnd.dom)
      } else {
        tdomdat$LARGEBND <- 1
        largebnd.unique <- "LARGEBND"
      }
      #addSAdomsdf <- TRUE
      #SAdomvars <- unique(c(SAdomvars, largebnd.unique))
    } else {
      tdomdat$LARGEBND <- 1
      largebnd.unique <- "LARGEBND"
      tdomdat <- merge(pltassgnx, tdomdat, 
                       by.x=c(pltassgnid, "DOMAIN"), 
                       by.y=c(cuniqueid, "DOMAIN"), all.x=TRUE)
    }
    if (pltassgnid != cuniqueid) {
      setnames(tdomdat, pltassgnid, cuniqueid)
    }
    if (SApackage == "spAbundance") {
      bayes <- TRUE
    } else {
      bayes <- FALSE
    }
    if (bayes) {
      vars2keep <- largebnd.unique
      tdomdat$LARGEBND <- 1
      largebnd.unique <- "LARGEBND"
      largebnd.vals <- 1
    }

    ## Get estimate for total
    ######################################
    byvars <- unique(c(vars2keep, largebnd.unique, dunitvar, "AOI", cuniqueid, "TOTAL", prednames))
    if (all(c("X", "Y") %in% names(pltassgnx))) {
      byvars <- c(byvars, "X","Y")
    }
    ## Sum estvar.name by dunitvar (DOMAIN), plot, domain
    tdomdattot <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
                        by=byvars, 
                        .SDcols=estvar.name]
  

    ## get unique largebnd values
    largebnd.vals <- sort(unique(tdomdattot[[largebnd.unique]]))
    largebnd.vals <- largebnd.vals[table(tdomdattot[[largebnd.unique]]) > 30]

    ## Add AOI if not in data
    ######################################
#    if (!"AOI" %in% names(tdomdat)) {
#      tdomdat$AOI <- 1
#      dunitlut$AOI <- 1
#    }
   
    ## get estimate by domain, by largebnd value
    #message("generating JoSAE unit-level estimates for ", response, " using ", SApackage, "...")

#    if (!"AOI" %in% names(tdomdattot)) {
#      tdomdattot$AOI <- 1
#      dunitlut$AOI <- 1
#    }
    

## Testing
#dunitlut <- data.table(SApopdat$dunitlut)
#dat=tdomdattot
#largebnd.val=largebnd.vals
#domain="TOTAL"
#largebnd.unique="LARGEBND"
    #source("C:\\_tsf\\_GitHub\\FIESTAutils\\R\\SAest.pbar.R")
    dunit_totestlst <- 
      tryCatch(
        lapply(largebnd.vals, SAest.large, 
               dat=tdomdattot, 
               cuniqueid=cuniqueid, largebnd.unique=largebnd.unique, 
               dunitlut=dunitlut, dunitvar="DOMAIN", 
               prednames=prednames, domain="TOTAL", response=response, 
               showsteps=showsteps, savesteps=savesteps, 
               stepfolder=stepfolder, prior=prior, 
               modelselect=modelselect, multest=multest,
               SApackage=SApackage, SAmethod=SAmethod, bayes=bayes, # TODO: pass bayes_opts
               save4testing=FALSE, vars2keep=vars2keep),
        error=function(e) {
          message("error with estimates of ", response, "...")
          message(e, "\n")
          return(NULL) })
    
    if (is.null(dunit_totestlst)) {
      return(NULL)
    }

    if (length(largebnd.vals) > 1) {
      dunit_est <- do.call(rbind, do.call(rbind, dunit_totestlst)[,"est.large"])
      if (multest || SAmethod == "unit") {
        predselect.unit <- do.call(rbind, dunit_totestlst)[,"predselect.unit"]
      }
      if (multest || SAmethod == "area") {
        predselect.area <- do.call(rbind, dunit_totestlst)[,"predselect.area"]
      }
      #names(prednames.select) <- largebnd.vals
      if (save4testing) {
        pdomdat <- do.call(rbind, do.call(rbind, dunit_totestlst)[,"pltdat.dom"])
        dunitlut <- do.call(rbind, do.call(rbind, dunit_totestlst)[,"dunitlut.dom"])
      }
      SAobjlst[[SApopdatnm]] <- do.call(rbind, dunit_totestlst)[,"SAobjlst.dom"]

    } else {
      dunit_est <- do.call(rbind, dunit_totestlst)[,"est.large"]$est.large
      if (multest || SAmethod == "unit") {
        predselect.unit <- do.call(rbind, dunit_totestlst)[,"predselect.unit"]$predselect.unit
      }
      if (multest || SAmethod == "area") {
        predselect.area <- do.call(rbind, dunit_totestlst)[,"predselect.area"]$predselect.area
      }
      if (save4testing) {
        pdomdat <- do.call(rbind, dunit_totestlst)[,"pltdat.dom"]$pltdat.dom
        dunitlut <- do.call(rbind, dunit_totestlst)[,"dunitlut.dom"]$dunitlut.dom
      }
      SAobjlst[[SApopdatnm]] <- do.call(rbind, dunit_totestlst)[,"SAobjlst.dom"]$SAobjlst.dom
    }

    if (multest || SAmethod == "unit") {
      predselectlst.unit[[SApopdatnm]] <- predselect.unit
    }
    if (multest || SAmethod == "area") {
      predselectlst.area[[SApopdatnm]] <- predselect.area
    }

    if (save4testing) {
      ## Merge SAdom attributes to dunit_totest
      if (addSAdomsdf) {
        pdomdat <- merge(setDT(SAdomsdf)[, 
			       unique(c("DOMAIN", "AOI", SAdomvars)), with=FALSE], 
			       pdomdat, by=c("DOMAIN", "AOI"))
        dunitlut <- merge(setDT(SAdomsdf)[, 
			       unique(c("DOMAIN", "AOI", SAdomvars)), with=FALSE], 
			       dunitlut, by=c("DOMAIN", "AOI"))
      }
      pdomdatlst[[SApopdatnm]] <- pdomdat
      dunitlutlst[[SApopdatnm]] <- dunitlut
    }
    estlst[[SApopdatnm]] <- dunit_est

    if (rowcolinfo$rowvar != "TOTAL") {
      tdomdat <- tdomdat[!is.na(tdomdat[[rowvar]]),] 
      tdomdatsum <- setDT(tdomdat)[, lapply(.SD, sum, na.rm=TRUE), 
                      by=c(largebnd.unique, dunitvar, cuniqueid, 
                           rowcolinfo$rowvar, prednames), .SDcols=estvar.name]

      if (!"DOMAIN" %in% names(tdomdatsum)) {
        tdomdatsum$DOMAIN <- tdomdatsum[[dunitvar]]
        tdomdatsum[[dunitvar]] <- NULL
      }
      if (!"AOI" %in% names(tdomdatsum)) {
        tdomdatsum$AOI <- 1
      }

#dunitlut <- data.table(SApopdat$dunitlut)
#dat=tdomdatsum
#largebnd.val=largebnd.vals
#domain=rowcolinfo$rowvar
#largebnd.unique="LARGEBND"

      dunit_rowestlst <- 
		tryCatch(
			lapply(largebnd.vals, SAest.large, 
				dat=tdomdatsum, 
				cuniqueid=cuniqueid, largebnd.unique=largebnd.unique, 
				dunitlut=dunitlut, dunitvar="DOMAIN",
				prednames=prednames, domain=rowcolinfo$rowvar,
				response=response, 
				showsteps=showsteps, savesteps=savesteps,
				stepfolder=stepfolder, prior=prior, 
				modelselect=modelselect, multest=multest, 
				SApackage=SApackage, SAmethod=SAmethod, bayes=bayes,
				vars2keep=vars2keep),
     	 	error=function(e) {
			message("error with estimates of ", response, " by ", rowvar, "...")
			message(e, "\n")
			return(NULL) })
      
      if (length(largebnd.vals) > 1) {
        dunit_est_row <- do.call(rbind, do.call(rbind, dunit_rowestlst)[,"est.large"])
        if (multest || SAmethod == "unit") {
          predselect.unit_row <- do.call(rbind, dunit_rowestlst)[,"predselect.unit"]
        }
        if (multest || SAmethod == "area") {
          predselect.area_row <- do.call(rbind, dunit_rowestlst)[,"predselect.area"]
        }
        if (save4testing) {
          pdomdat_row <- do.call(rbind, do.call(rbind, dunit_rowestlst)[,"pltdat.dom"])
          dunitlut_row <- do.call(rbind, do.call(rbind, dunit_rowestlst)[,"dunitlut.dom"])
        }
        SAobjlst_row[[SApopdatnm]] <- do.call(rbind, dunit_rowestlst)[,"SAobjlst.dom"]
      } else {
        dunit_est_row <- do.call(rbind, dunit_rowestlst)[,"est.large"]$est.large
        if (multest || SAmethod == "unit") {
          predselect.unit_row <- do.call(rbind, dunit_rowestlst)[,"predselect.unit"]$predselect.unit
        }
        if (multest || SAmethod == "area") {
          predselect.area_row <- do.call(rbind, dunit_rowestlst)[,"predselect.area"]$predselect.area
        }
        if (save4testing) {
          pdomdat_row <- do.call(rbind, dunit_rowestlst)[,"pltdat.dom"]$pltdat.dom
          dunitlut_row <- do.call(rbind, dunit_rowestlst)[,"dunitlut.dom"]$dunitlut.dom
        }
        SAobjlst_row[[SApopdatnm]] <- do.call(rbind, dunit_rowestlst)[,"SAobjlst.dom"]$SAobjlst.dom
      }

      if (multest || SAmethod == "unit") {
        predselectlst.unit_row[[SApopdatnm]] <- predselect.unit_row
      }
      if (multest || SAmethod == "area") {
        predselectlst.area_row[[SApopdatnm]] <- predselect.area_row
      }

      if (save4testing) {
        ## Merge SAdom attributes to dunit_totest
        if (addSAdomsdf) {
          pdomdat_row <- merge(setDT(SAdomsdf)[, 
			            unique(c("DOMAIN", "AOI", SAdomvars)), with=FALSE], 
			            pdomdat_row, by=c("DOMAIN", "AOI"))
          dunitlut_row <- merge(setDT(SAdomsdf)[, 
			            unique(c("DOMAIN", "AOI", SAdomvars)), with=FALSE], 
			            dunitlut_row, by=c("DOMAIN", "AOI"))
        }
        pdomdatlst_row[[SApopdatnm]] <- pdomdat_row
        dunitlutlst_row[[SApopdatnm]] <- dunitlut_row
      }
      estlst_row[[SApopdatnm]] <- dunit_est_row
    }
  }    #### end SApopdat loop

  ## Combine estimates
  estdf <- do.call(rbind, estlst)

  ## Check for AOI column
  if (!"AOI" %in% names(estdf)) {
    estdf$AOI <- 1
  }	

  if (multest || SAmethod == "unit") {
    predselect.unitdf <- data.frame(DOMAIN=names(predselectlst.unit), 
					do.call(rbind, predselectlst.unit))
    setnames(predselect.unitdf, "DOMAIN", largebnd.unique)
    predselect.unitdf[is.na(predselect.unitdf)] <- 0
  }
  if (multest || SAmethod == "area") {
    predselect.areadf <- data.frame(DOMAIN=names(predselectlst.area), 
					do.call(rbind, predselectlst.area))
    setnames(predselect.areadf, "DOMAIN", largebnd.unique)
    predselect.areadf[is.na(predselect.areadf)] <- 0
  }

  ## Merge SAdom attributes to estdf_row
  ################################################
  if (addSAdomsdf && is.null(SAdomvars)) {
    SAdomvars2 <- unique(names(SAdomsdfbind)[!names(SAdomsdfbind) %in% names(estdf)])
    estdf <- merge(setDF(SAdomsdfbind)[,c("DOMAIN", SAdomvars2)], estdf, by="DOMAIN")
    estdf <- estdf[order(-estdf$AOI, estdf[["DOMAIN"]]),]
    #estdf$AOI <- NULL

  } else if (addSAdomsdf && !is.null(SAdomvars)) {
    SAdomvars2 <- SAdomvars[SAdomvars %in% names(SAdomsdfbind)]
    SAdomvars2 <- unique(SAdomvars2[!SAdomvars2 %in% names(estdf)])
 
    if (length(SAdomvars2) != 0) 
    estdf <- merge(setDF(SAdomsdfbind)[, unique(c("DOMAIN", SAdomvars2))], 
					estdf, by="DOMAIN")
    estdf <- estdf[order(-estdf$AOI, estdf[["DOMAIN"]]),]
  } else {
    estdf <- estdf[order(-estdf$AOI, estdf[["DOMAIN"]]),]
  }

  if (rowcolinfo$rowvar != "TOTAL") {

    ## Combine estimates
    estdf_row <- do.call(rbind, estlst_row)

    ## Check for AOI column
    if (!"AOI" %in% names(estdf)) {
      estdf_row$AOI <- 1
    }	

    if (multest || SAmethod == "unit") {
      predselect.unitdf_row <- data.frame(DOMAIN=names(predselectlst.unit_row), 
			do.call(rbind, predselectlst.unit_row))
      setnames(predselect.unitdf_row, "DOMAIN", largebnd.unique)
      predselect.unitdf_row[is.na(predselect.unitdf_row)] <- 0
    }
    if (multest || SAmethod == "area") {
      predselect.areadf_row <- data.frame(DOMAIN=names(predselectlst.area_row), 
			do.call(rbind, predselectlst.area_row))
      setnames(predselect.areadf_row, "DOMAIN", largebnd.unique)
      predselect.areadf_row[is.na(predselect.areadf_row)] <- 0
    }

    ## Merge SAdom attributes to estdf_row
    if (addSAdomsdf && is.null(SAdomvars)) {
      SAdomvars2 <- unique(names(SAdomsdfbind)[!names(SAdomsdfbind) %in% names(estdf_row)])
      estdf_row <- merge(setDF(SAdomsdfbind)[, c("DOMAIN", SAdomvars2)], 
					estdf_row, by="DOMAIN")
      estdf_row <- estdf_row[order(-estdf_row$AOI, estdf_row[["DOMAIN"]]),]

    } else if (addSAdomsdf && !is.null(SAdomvars)) {
      SAdomvars2 <- SAdomvars[SAdomvars %in% names(SAdomsdfbind)]
      SAdomvars2 <- unique(SAdomvars2[!SAdomvars2 %in% names(estdf_row)])
    
      if (length(SAdomvars) == 0) stop("invalid SAdomvars")
      estdf_row <- merge(setDF(SAdomsdfbind)[, unique(c("DOMAIN", SAdomvars2))], 
					multestdf_row, by="DOMAIN")
      estdf_row <- estdf_row[order(-estdf_row$AOI, estdf_row[["DOMAIN"]]),]
    } else {
      estdf_row <- estdf_row[order(-estdf_row$AOI, estdf_row[["DOMAIN"]]),]
    }
  }
 
  ## Define nhat
  ##################################
  if (SAmethod == "unit") {
    if (SApackage == "hbsae") {
      nhat <- "hbsaeU"
      nhat.se <- "hbsaeU.se"
    } else if (SApackage == "JoSAE") {
      nhat <- "JU.EBLUP"
      nhat.se <- "JU.EBLUP.se.1"
    } else if (SApackage == "spAbundance") {
      nhat <- "bayes"
      nhat.se <- "bayes.se"
    }
  } else if (SAmethod == "area") {
    if (SApackage == "JoSAE") {
      nhat <- "JFH"
      nhat.se <- "JFH.se"
    } else if (SApackage == "sae") {
      nhat <- "saeA"
      nhat.se <- "saeA.se"
    } else if (SApackage == "hbsae") {
      nhat <- "hbsaeA"
      nhat.se <- "hbsaeA.se"
    }
  } 
  
  if (multest) {
    multestdf <- estdf
    multestdf[is.na(multestdf$AOI), "AOI"] <- 0
    if (rowcolinfo$rowvar != "TOTAL") {
      multestdf_row <- estdf_row
      multestdf_row[is.na(multestdf_row$AOI), "AOI"] <- 0
    }
  }

  ## Set up estimates. If estimate is NULL, use direct estimator
  estdf <- setDT(estdf)
  estdf[, c("nhat", "nhat.se") := .SD, .SDcols=c(nhat, nhat.se)]
  estdf$estimator <- nhat

  if (na.fill != "NONE") {
    estdf[is.na(estdf$nhat), "estimator"] <- na.fill
    na.fill.se <- paste0(na.fill, ".se")
    estdf[is.na(estdf$nhat), c("nhat", "nhat.se")] <- 
      estdf[is.na(estdf$nhat), c(na.fill, na.fill.se), with=FALSE]
  }

  ## Change values that are less than 0 to 0
  if (!lt0 && any(!is.na(estdf$nhat)) && any(na.omit(estdf$nhat) < 0)) {
    estdf[estdf$nhat < 0, "nhat"] <- 0
  } 
 
  ## Subset multest to estimation output
  dunit_totest <- setDT(estdf)[AOI==1, 
		unique(c("DOMAIN", "nhat", "nhat.se", "NBRPLT.gt0", "estimator")), with=FALSE]
  setkeyv(dunit_totest, "DOMAIN")

  ## Merge dunitarea
  tabs <- check.matchclass(dunitareabind, dunit_totest, "DOMAIN")
  dunitareabind <- tabs$tab1
  dunit_totest <- tabs$tab2
  dunit_totest <- merge(dunit_totest, 
		dunitareabind[, c("DOMAIN", "AREAUSED"), with=FALSE], by="DOMAIN")

  if (!is.null(dunit_totest)) {
    dunit_totest[, nhat.var := nhat.se^2]

    if (totals) {
      dunit_totest <- getpse(dunit_totest, areavar=areavar, esttype=esttype)
    } else {
      dunit_totest <- getpse(dunit_totest, esttype=esttype)
    }
  }
  
  if (rowcolinfo$rowvar != "TOTAL") {
    ## Set up estimates. If estimate is NULL, use direct estimator
    estdf_row <- setDT(estdf_row)
    estdf_row[, c("nhat", "nhat.se") := .SD, .SDcols=c(nhat, nhat.se)]
    estdf_row$estimator <- nhat

    if (na.fill != "NONE") {
      estdf_row[is.na(estdf_row$nhat), "estimator"] <- na.fill
      na.fill.se <- paste0(na.fill, ".se")
      estdf_row[is.na(estdf_row$nhat), c("nhat", "nhat.se")] <- 
        estdf_row[is.na(estdf_row$nhat), c(na.fill, na.fill.se), with=FALSE]
    }

    ## Change values that are less than 0 to 0
    if (!lt0 && any(!is.na(estdf_row$nhat)) && any(na.omit(estdf_row$nhat) < 0)) {
      estdf_row[estdf_row$nhat < 0, "nhat"] <- 0
    } 

    ## Subset multest to estimation output
    dunit_rowest <- setDT(estdf_row)[AOI==1, 
                     unique(c("DOMAIN", rowcolinfo$rowvar, 
					 "nhat", "nhat.se", "NBRPLT.gt0", "estimator")),
 				with=FALSE]
    setkeyv(dunit_rowest, "DOMAIN")
	
    ###############################################################################
    ## Check add0 and Add area
    ###############################################################################
    if (!is.null(dunit_rowest)) {
      dunit_rowest <- add0unit(x=dunit_rowest, xvar=rowcolinfo$rowvar, uniquex=uniquerow, 
		       unitvar=dunitvar, xvar.add0=row.add0)
      tabs <- check.matchclass(dunitareabind, dunit_rowest, dunitvar)
      dunitareabind <- tabs$tab1
      dunit_rowest <- tabs$tab2
	}
    
    if (!is.null(dunit_rowest)) {
      dunit_rowest[, nhat.var := nhat.se^2]

      if (totals) {
        dunit_rowest <- getpse(dunit_rowest, areavar=areavar, esttype=esttype)
      } else {
        dunit_rowest <- getpse(dunit_rowest, esttype=esttype)
      }
    }
  }
  estnm <- "est"

  #####################################################################################
  ### GET TITLES FOR OUTPUT TABLES
  #####################################################################################
  title.dunitvar <- ifelse(is.null(title.unitvar), smallbnd.dom, title.unitvar)
  alltitlelst <- check.titles(esttype=esttype, estseed=estseed, 
                    sumunits=sumunits, title.main=title.main, title.ref=title.ref, 
                    title.rowvar=rowcolinfo$title.rowvar, 
					title.colvar=rowcolinfo$title.colvar, 
                    title.unitvar=title.dunitvar, title.filter=title.filter, 
                    title.unitsn=estvarunits, unitvar="DOMAIN", 
					title.estvarn=title.estvar, 
                    rowvar=rowcolinfo$rowvar, colvar=rowcolinfo$colvar, 
                    estvarn=estvar, estvarn.filter=estvar.filter, 
                    addtitle=addtitle, returntitle=returntitle, rawdata=rawdata, 
                    states=states, invyrs=invyrs, landarea=landarea, 
                    pcfilter=pcfilter, allin1=allin1, divideby=divideby, 
                    parameters=FALSE)
  title.dunitvar <- alltitlelst$title.unitvar
  title.est <- alltitlelst$title.est
  title.pse <- alltitlelst$title.pse
  title.estpse <- alltitlelst$title.estpse
  title.ref <- alltitlelst$title.ref
  title.rowvar <- alltitlelst$title.rowvar
  outfn.estpse <- alltitlelst$outfn.estpse
  outfn.param <- alltitlelst$outfn.param
  if (rawdata) {
    outfn.rawdat <- alltitlelst$outfn.rawdat
    outfn.rawdat <- paste0(outfn.rawdat, "_modSA_", SApackage, "_", SAmethod) 
  } 
  ## Append name of package and method to outfile name
  outfn.estpse2 <- paste0(outfn.estpse, "_modSA_", SApackage, "_", SAmethod) 

  ###################################################################################
  ## GENERATE OUTPUT TABLES
  ###################################################################################
  message("getting output...")
  tabs <- est.outtabs(esttype=esttype, sumunits=sumunits, areavar=areavar, 
              unitvar="DOMAIN", unit_totest=dunit_totest, 
			  unit_rowest=dunit_rowest, unit_colest=dunit_colest, 
			  unit_grpest=dunit_grpest, 
              rowvar=rowcolinfo$rowvarnm, colvar=rowcolinfo$colvarnm, 
              uniquerow=rowcolinfo$uniquerow, uniquecol=rowcolinfo$uniquecol, 
              rowgrp=rowgrp, rowgrpnm=rowgrpnm, 
			  rowunit=rowunit, totunit=totunit, allin1=allin1, 
			  savedata=savedata, addtitle=addtitle, title.ref=title.ref, 
              title.colvar=rowcolinfo$title.colvar, title.rowvar=rowcolinfo$title.rowvar, 
              title.rowgrp=title.rowgrp, title.unitvar=title.dunitvar, 
			  title.estpse=title.estpse, title.est=title.est, 
			  title.pse=title.pse, rawdata=rawdata, rawonly=rawonly, 
			  outfn.estpse=outfn.estpse2, outfolder=outfolder, 
              outfn.date=outfn.date, overwrite=overwrite_layer, 
			  estnm=estnm, estround=estround, pseround=pseround, 
			  divideby=divideby, returntitle=returntitle, 
			  estnull=estnull, psenull=psenull) 
  est2return <- tabs$tabest
  pse2return <- tabs$tabpse

#  if (!is.null(est2return)) {
#    est2return[is.na(est2return$Estimate), "Estimate"] <- estnull 
#    if ("Percent Sampling Error" %in% names(est2return)) {
#      est2return[is.na(est2return$"Percent Sampling Error"), 
#		"Percent Sampling Error"] <- psenull 
#    }

    returnlst$est <- est2return
#  } 
  if (!is.null(pse2return)) {
#    if ("Percent Sampling Error" %in% names(pse2return)) {
#      pse2return[is.na(pse2return$"Percent Sampling Error"), 
#		"Percent Sampling Error"] <- psenull 
#    }
    returnlst$pse <- pse2return 
  }
  if (returntitle) {
    returnlst$titlelst <- alltitlelst
  }
 
  domain <- "TOTAL"
  if (multest && !is.null(multestdf)) {
    ## Merge dunitarea
    #tabs <- check.matchclass(dunitarea, multestdf, dunitvar)
    #dunitarea <- tabs$tab1
    #dunit_multest <- tabs$tab2
 
    multestdf <- merge(multestdf, 
		dunitareabind[, c("DOMAIN", "AREAUSED"), with=FALSE], by="DOMAIN")
    #multestdf[, JoSAE.total := get(nhat) * AREAUSED]
    #multestdf[, JoSAE.pse := get(nhat.se)/get(nhat) * 100]

    ## Remove TOTAL column from multestdf
    if (domain == "TOTAL" && "TOTAL" %in% names(multestdf)) {
      multestdf$TOTAL <- NULL
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
          #multest_layer <- paste0("SAmultest_", response, ".csv")
          multest_layer <- paste0(outfn.estpse, "_multest.csv")
        } else {
          #multest_layer <- paste0(SApackage, "_", response)
          multest_layer <- response
        }
      }

      ## Export multestdf
      overwrite_layer <- ifelse(multest.append, FALSE, overwrite_layer)
      datExportData(multestdf, 
                    savedata_opts=list(outfolder=multest_outfolder, 
                                       out_fmt=multest_fmt, 
                                       out_dsn=multest_dsn, 
                                       out_layer=multest_layer,
                                       outfn.pre=outfn.pre, 
                                       outfn.date=outfn.date, 
                                       overwrite_layer=overwrite_layer,
                                       append_layer=multest.append,
                                       add_layer=TRUE))
    }
  } 

  if (multest && rowcolinfo$rowvar != "TOTAL" && !is.null(multestdf_row)){
    ## Merge dunitarea
    #tabs <- check.matchclass(dunitarea, multestdf, dunitvar)
    #dunitarea <- tabs$tab1
    #dunit_multest <- tabs$tab2
 
    multestdf_row <- merge(multestdf_row, 
		dunitareabind[, c("DOMAIN", "AREAUSED"), with=FALSE], by="DOMAIN")
    #multestdf[, JoSAE.total := get(nhat) * AREAUSED]
    #multestdf[, JoSAE.pse := get(nhat.se)/get(nhat) * 100]

    ## Remove TOTAL column from multestdf
    if (domain == "TOTAL" && "TOTAL" %in% names(multestdf_row)) {
      multestdf_row[, TOTAL := NULL]
    }
    if (multest.AOIonly) {
      ## Subset multestdf_row, where AOI = 1
      multestdf_row <- multestdf_row[multestdf_row$AOI == 1, ]
    }
    ## Save multest table
    if (savemultest) {

      ## Remove TOTAL column from est
      if (domain == "TOTAL" && "TOTAL" %in% names(multestdf_row)) {
        multestdf_row[, TOTAL := NULL]
      }
      ## Remove column headings if appending to csv file
      if (multest.append && multest_fmt == "csv") {
        multestdf_row <- setDF(multestdf_row)
        colnames(multestdf_row) <- NULL
      }
      if (is.null(multest_layer)) {
        if (multest_fmt == "csv") {
          #multest_layer <- paste0("SAmultest_", SApackage, "_", response, ".csv")
          multest_layer_row <- paste0(outfn.estpse, "_multest.csv")
        } else {
          #multest_layer <- paste0(SApackage, "_", response)
          multest_layer_row <- paste0(response, "_", rowvar)
        }
      }
 
      ## Export multestdf
      overwrite_layer <- ifelse(multest.append, FALSE, overwrite_layer)
      datExportData(multestdf_row, 
            savedata_opts=list(outfolder=multest_outfolder, 
                                out_fmt=multest_fmt, 
                                out_dsn=multest_dsn, 
                                out_layer=multest_layer_row,
                                outfn.pre=outfn.pre, 
                                outfn.date=outfn.date, 
                                overwrite_layer=overwrite_layer,
                                append_layer=multest.append,
                                add_layer=TRUE))
    }
  } 


  if (rawdata) {
    rawdat <- tabs$rawdat
    names(rawdat)[names(rawdat) == "unit_totest"] <- "dunit_totest"
    names(rawdat)[names(rawdat) == "unit_rowest"] <- "dunit_rowest"
    rawdat$domdat <- setDF(tdomdat)

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
            datExportData(rawtab, 
                  savedata_opts=list(outfolder=rawfolder, 
                                      out_fmt=raw_fmt, 
                                      out_dsn=raw_dsn, 
                                      out_layer=out_layer,
                                      outfn.pre=outfn.pre, 
                                      outfn.date=outfn.date, 
                                      overwrite_layer=overwrite_layer,
                                      append_layer=append_layer,
                                      add_layer=TRUE))
          }
        }
      }
    }

    rawdat$module <- "SA"
    rawdat$esttype <- esttype
    rawdat$SApackage <- SApackage
    rawdat$SAmethod <- SAmethod
    rawdat$estnm <- estnm
    if (multest || SAmethod == "unit") {
      rawdat$predselect.unit <- predselect.unitdf
    }
    if (multest || SAmethod == "area") {
      rawdat$predselect.area <- predselect.areadf
    }
    rawdat$SAobjlst <- SAobjlst 
    rawdat$estvar <- response
    if (esttype == "TREE") {
      rawdat$estvar.filter <- estvar.filter
    }
    if (rowcolinfo$rowvar != "TOTAL") {
      rawdat$rowvar <- rowvar
      rawdat$rowvar <- rowvar
      if (multest || SAmethod == "unit") {
        rawdat$predselect.unit_row <- predselect.unitdf_row
      }
      if (multest || SAmethod == "area") {
        rawdat$predselect.area_row <- predselect.areadf_row
      }
    }
    if (rowcolinfo$colvar != "NONE") {
	  rawdat$colvar <- rowcolinfo$colvar
	}
    rawdat$areaunits <- areaunits
    rawdat$estunits <- estvarunits
    returnlst$raw <- rawdat  
  }

  if (multest) {
    returnlst$multest <- setDF(multestdf)
    if (rowcolinfo$rowvar != "TOTAL") {
      returnlst$multest_row <- setDF(multestdf_row)
    }
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


