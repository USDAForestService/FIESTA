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
#' \tab Number of trees per acre each sample tree represents (ex. DESIGNCD=1:
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
#' @param largebnd.unique String. Name of the large boundary unique identifer
#' to define plots within a model extent. If NULL, all plots are used for model
#' extent.
#' @param landarea String. The sample area filter for estimates ('ALL',
#' 'FOREST', 'TIMBERLAND').  If landarea=FOREST, filtered to COND_STATUS_CD =
#' 1; If landarea=TIMBERLAND, filtered to SITECLCD in(1:6) and RESERVCD = 0.
#' @param pcfilter String. A filter for plot or cond attributes (including
#' pltassgn).  Must be R logical syntax.
#' @param rowvar String. Name of the row domain variable in cond or tree. If
#' only one domain, rowvar = domain variable. If more than one domain, include
#' colvar. If no domain, rowvar = NULL.
#' @param modelselect Logical. If TRUE, selects useful predictors using
#' mase:ElasticNet.
#' @param prior Function. A prior function to use for hbsae models.
#' @param na.fill String. An estimate to fill in for NA values (i.e., when 
#' model is unstable or no predictors are selected). Choose from the following 
#' list that does not include SApackage used ('NONE', 'DIR', 'JU.GREG', 
#' 'JU.EBLUP','JFH','hbsaeU','hbsaeA'). DIR is suggested value to fill NA values. 
#' @param savedata Logical. If TRUE, saves table(s) to outfolder.
#' @param savesteps Logical. Saves graphs of predictors and response with
#' labels whether selected or not for both area- and unit-level models.
#' @param multest Logical. If TRUE, returns a data frame of SA estimates using
#' both unit-level and area-level estimates.
#' @param addSAdomsdf Logical. If TRUE, appends SAdomdf to unit.multest table
#' for output.
#' @param SAdomvars String vector. List of attributes from SAdoms to include in
#' multest output.
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
#' to outfolder for testing (domdat, dunitlut).
#' @param ...  Parameters for modSApop if SApopdat is NULL.
#' 
#' @return \item{est}{ Data frame. Tree estimates and percent sampling error by
#' domain. Estimates are based on the SApackage and SAmethod parameters defined. } 
#' \item{titlelst}{ List. List of titles used for table output. }
#' \item{raw}{ List of raw data. If rawdata=TRUE, a list including raw data
#' components used for calculating estimate. } 
#' \item{dunit.multest}{ Data frame. Table comparing different estimation 
#' strategies for SAE. }
#' 
#' Raw data
#' 
#' \item{domdat}{ Data frame. Domain-level data used for estimation. }
#' \item{dunit.totest}{String. Table of estimates, including more details. }
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
#'
#' # Fit an area level Fay-Herriot EBLUP with `sae`, while using Elastic Net
#' # variable selection
#' modSAarea(SApopdatlst = SApopdat,
#'           SApackage = "JoSAE",
#'           SAmethod = "area",
#'           modelselect = TRUE) 
#' }
#' @export modSAarea
modSAarea <- function(SApopdatlst = NULL, 
                      prednames = NULL, 
                      SApackage = "JoSAE", 
                      SAmethod = "area", 
                      largebnd.unique = NULL, 
                      landarea = "FOREST", 
                      pcfilter = NULL, 
                      rowvar = NULL, 
                      modelselect = FALSE, 
                      prior = function(x) 1/(sqrt(x)*(1+x)),
                      na.fill = "NONE", 
                      savedata = FALSE, 
                      savesteps = FALSE, 
                      multest = TRUE, 
                      addSAdomsdf = TRUE, 
                      SAdomvars = NULL, 
                      returntitle = FALSE, 
                      table_opts = NULL, 
                      title_opts = NULL, 
                      savedata_opts = savedata_options(), 
                      multest_opts = multest_options(),
                      save4testing = FALSE,
                      ...){


  ######################################################################################
  ## DESCRIPTION: 
  ## Generates model-assisted estimates by domain (and estimation unit)
  ## if saveraw...  and raw_fmt = 'csv', a new folder is created within the outfolder
  ##			named as raw_dsn. If raw_fmt != 'csv', a database is created
  ##			within the outfolder names as raw_dsn. 
  ######################################################################################

  gui <- FALSE

  ## If gui.. set variables to NULL
  if (gui) { 
    tree=landarea <- NULL
    if (!row.FIAname) row.FIAname <- NULL
    if (!col.FIAname) col.FIAname <- NULL
  }

  ## Set parameters
  esttype <- "AREA"
  popType <- "CURR"
  rawdata <- TRUE 
  vars2keep <- c("DOMAIN", "AOI")
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
  lt0 <- FALSE
  addSAdomsdf = FALSE 
  SAdomvars = NULL
  returnlst <- list()
  showsteps <- FALSE
  savemultest <- savedata
  
  ## Set global variables
  ONEUNIT=n.total=n.strata=strwt=TOTAL=domclassify=AOI=
    title.rowvar=title.colvar=title.rowgrp=TOTAL=JoSAE=JU.EBLUP=JFH=JoSAE.se=
    JU.EBLUP.se.1=pse=AREAUSED=JoSAE.pse=JoSAE.total=treef=seedf=nhat.var=
    SAEarea_estimators=SAEunit_estimators=predselect.areadf=predselect.unitdf <- NULL
  
  ## Set estimator list
  estimatorlst <- c('JU.GREG','JU.EBLUP','JFH','hbsaeU','hbsaeA')
  
  ## Set seed
  set.seed(66)
  
  
  ##################################################################
  ## CHECK PARAMETER NAMES
  ##################################################################
  
  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- c(names(formals(modSAarea)),
                 names(formals(modSApop))) 
  if (!all(input.params %in% formallst)) {
    miss <- input.params[!input.params %in% formallst]
    stop("invalid parameter: ", toString(miss))
  }

  ## Check parameter lists
  pcheck.params(input.params = input.params,
                title_opts = title_opts, 
                table_opts = table_opts, 
                savedata_opts = savedata_opts,
                multest_opts = multest_opts)
  
  ## Check parameter option lists
  optslst <- pcheck.opts(optionlst = list(
                         title_opts = title_opts,
                         table_opts = table_opts,
                         savedata_opts = savedata_opts,
                         multest_opts = multest_opts)) 
  title_opts <- optslst$title_opts  
  table_opts <- optslst$table_opts  
  multest_opts <- optslst$multest_opts  
  savedata_opts <- optslst$savedata_opts  
  
  for (i in 1:length(title_opts)) {
    assign(names(title_opts)[[i]], title_opts[[i]])
  }
  for (i in 1:length(table_opts)) {
    assign(names(table_opts)[[i]], table_opts[[i]])
  }
  for (i in 1:length(multest_opts)) {
    assign(names(multest_opts)[[i]], multest_opts[[i]])
  }
  
  
  ##################################################################
  ## CHECK PARAMETER INPUTS
  ##################################################################
  
  ## Check SApackage 
  SApackagelst <- c("JoSAE", "sae", "hbsae", "spAbundance")
  SApackage <- pcheck.varchar(var2check=SApackage, varnm="SApackage", 
                  gui=gui, checklst=SApackagelst, caption="SApackage", 
		          multiple=FALSE, stopifnull=TRUE)

  ## Check SAmethod 
  SAmethodlst <- c("unit", "area")
  SAmethod <- pcheck.varchar(var2check=SAmethod, varnm="SAmethod", 
                 gui=gui, checklst=SAmethodlst, caption="SAmethod", 
				 multiple=FALSE, stopifnull=TRUE)

  if (SApackage == "sae" && SAmethod == "unit") {
    stop("sae unit-level estimates are not available\n")
  }
  
  ## Check na.fill 
  na.filllst <- c("NONE", "DIR", estimatorlst)
  na.filllst <- na.filllst[na.filllst != SApackage]
  na.fill <- pcheck.varchar(var2check=na.fill, varnm="na.fill", gui=gui, 
                              checklst=na.filllst, caption="na.fill", 
                              multiple=FALSE, stopifnull=TRUE)
  

  ###################################################################################
  ## Check data and generate population information 
  ###################################################################################
  if (is.null(SApopdatlst)) {
    stop("need to include SApopdatlst... from modSApop")
  } else {
    if (!is.list(SApopdatlst)) {
      SApopdatlst <- list(SApopdatlst)
    } else if ("pltcondx" %in% names(SApopdatlst)) {
      SApopdatlst <- list(SApopdatlst)
    }  

    list.items <- c("pltcondx", "dunitarea", "dunitvar", "dunitlut")
    returnSApopdat <- FALSE
  }

  ##########################################################################
  ## Check output parameters 
  ##########################################################################
  outparams <- 
    check.outparams(esttype = esttype, totals=totals,
	                  allin1 = allin1, 
                    estround = estround, pseround = pseround, 
			              divideby = divideby, 
			              returntitle = returntitle, 
			              rawdata = rawdata, rawonly = rawonly, 
			              savedata = savedata, 
	                  savedata_opts = savedata_opts, 
			              gui = gui)
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
  outfn.pre <- outparams$outfn.pre
  outfn.date <- outparams$outfn.date
  append_layer <- outparams$append_layer
  layer.pre <- outparams$layer.pre
  raw_fmt <- outparams$raw_fmt
  raw_dsn <- outparams$raw_dsn
  rawfolder <- outparams$rawfolder
  totals <- outparams$totals

  ## Check multest 
  ########################################################
  multest <- pcheck.logical(multest,
                            varnm = "multest", title = "Multiple estimates?", 
                            first = "YES", gui = gui, stopifnull= TRUE)
  if (multest) {

    ## Define objects
    for (i in 1:length(multest_opts)) {
      assign(names(multest_opts)[[i]], multest_opts[[i]])
    }
    multest_outfolder <- pcheck.outfolder(multest_outfolder)
    
    estimatorlst <- c('JU.GREG','JU.EBLUP','JFH','hbsaeU','hbsaeA')
    estimatorSElst <- c('JU.GREG.se','JU.EBLUP.se.1','JFH.se','hbsaeU.se','hbsaeA.se')
    multest_estimators <- pcheck.varchar(var2check = estimatorlst, 
                                  varnm = "multest_estimators", checklst = c("all", estimatorlst), 
                                  gui = gui, caption = "Output multest format?", multiple = TRUE) 
    multest_estimators <- sort(c(multest_estimators, estimatorSElst[which(estimatorlst %in% multest_estimators)]))
  } 

  ## Check na.fill
  if (!is.null(na.fill) && na.fill != "NONE") {
    if (!na.fill %in% c("DIR", estimatorlst)) {
      stop("na.fill must be in following list: ", toString(c("DIR", estimatorlst)))
    }
    if (!multest) {
      multest <- TRUE
      multest_estimators <- na.fill
    } else {
      if (!"all" %in% multest_estimators && !na.fill %in% multest_estimators) {
        multest_estimators <- c(multest_estimators, na.fill)
      } 
    }
  }  
  
  
  ## Check output for multest 
  ########################################################
  if (savedata) {
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
  } else {
    stepfolder <- NULL
  }
  

  #####################################################################################
  ## GENERATE ESTIMATES
  #####################################################################################
  #setnames(cdomdat, dunitvar, "DOMAIN")

  ## Define empty lists
  estlst <- list()
  predselectlst <- list()
  predselectlst.unit <- list()
  predselectlst.area <- list()

  SAobjlst <- list()
  dunitareabind <- {}
  if (addSAdomsdf) {
    SAdomsdfbind <- {}
  }
  if (save4testing) {
    pdomdatlst <- list()
    dunitlutlst <- list()
  } else {
    pdomdatlst <- NULL
    dunitlutlst <- NULL
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
  } else {
    estlst_row <- NULL
    predselectlst.unit_row <- NULL
    predselectlst.area_row <- NULL
    SAobjlst_row <- NULL
    pdomdatlst_row <- NULL
    dunitlutlst_row <- NULL
  }
 
 
  ## Loop through SApopdatlst
  ##############################################################################
  dunit_totest=dunit_rowest=dunit_colest=dunit_grpest=rowunit=totunit <- NULL
  
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
    pltidsadj <- SApopdat$pltidsadj
    pltcondx <- SApopdat$pltcondx
    cuniqueid <- SApopdat$cuniqueid
    pltassgnid <- SApopdat$pltassgnid
    condid <- SApopdat$condid
    ACI <- SApopdat$ACI
    pltassgnx <- SApopdat$pltassgnx
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
    popdatindb <- SApopdat$popdatindb
    pop_fmt <- SApopdat$pop_fmt
    pop_dsn <- SApopdat$pop_dsn
    pop_schema <- SApopdat$pop_schema
    popconn <- SApopdat$popconn
    dbqueries <- SApopdat$dbqueries
    dbqueriesWITH <- SApopdat$dbqueriesWITH
    areawt <- SApopdat$areawt
    areawt2 <- SApopdat$areawt2
    adjcase <- SApopdat$adjcase
    pltidsid <- SApopdat$pjoinid
    pltassgnid <- SApopdat$pltassgnid
    SAdoms <- SApopdat$SAdoms
    largebnd.unique <- SApopdat$largebnd.unique
    
    ## check smallbnd.dom
    ########################################################
    smallbnd.dom <- "DOMAIN"
    
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

    if (popdatindb) {
      if (is.null(popconn) || !DBI::dbIsValid(popconn)) {
        if (!is.null(pop_dsn)) {
          if (pop_fmt == "sqlite") {
            popconn <- DBtestSQLite(pop_dsn, dbconnopen = TRUE)
          }
        } else {
          stop("invalid database connection")
        }
      }
      #pltcondx <- dbqueries$pltcondx
      pltcondxWITHqry <- dbqueriesWITH$pltcondxWITH
      pltcondxadjWITHqry <- dbqueriesWITH$pltcondxadjWITH
    } else {
      pltcondxWITHqry=pltcondxadjWITHqry <- NULL
    }
    
    ########################################
    ## Check area units
    ########################################
    unitchk <- pcheck.areaunits(unitarea = dunitarea, 
                                areavar = areavar, 
                                areaunits = areaunits, 
                                metric = metric)
    dunitarea <- unitchk$unitarea
    areavar <- unitchk$areavar
    areaunits <- unitchk$outunits
    dunitareabind <- rbind(dunitareabind, unitchk$unitarea)
    
    if (is.null(key(dunitarea))) {
      setkeyv(dunitarea, dunitvar)
    }
    
    
    ###################################################################################
    ## Check parameter inputs and plot/condition filters
    ###################################################################################
    estdat <- 
      check.estdata(esttype = esttype, 
                    popType = popType,
                    popdatindb = popdatindb, 
                    popconn = popconn, pop_schema = pop_schema,
                    pltcondx = pltcondx,
                    totals = totals,
                    pop_fmt = pop_fmt, pop_dsn = pop_dsn, 
                    landarea = landarea,
                    ACI = ACI, 
                    pcfilter = pcfilter,
                    allin1 = allin1, divideby = divideby,
                    estround = estround, pseround = pseround,
                    returntitle = returntitle, 
                    rawonly = rawonly, 
                    savedata = savedata, 
                    savedata_opts = savedata_opts, 
                    gui = gui)
    if (is.null(estdat)) return(NULL)
    esttype <- estdat$esttype
    totals <- estdat$totals
    landarea <- estdat$landarea
    allin1 <- estdat$allin1
    divideby <- estdat$divideby
    estround <- estdat$estround
    pseround <- estdat$pseround
    addtitle <- estdat$addtitle
    returntitle <- estdat$returntitle
    rawonly <- estdat$rawonly
    savedata <- estdat$savedata
    outfolder <- estdat$outfolder
    overwrite_layer <- estdat$overwrite_layer
    outfn.pre <- estdat$outfn.pre
    outfn.date <- estdat$outfn.date
    append_layer = estdat$append_layer
    rawfolder <- estdat$rawfolder
    raw_fmt <- estdat$raw_fmt
    raw_dsn <- estdat$raw_dsn
    pcwhereqry <- estdat$where.qry
    SCHEMA. <- estdat$SCHEMA.
    pltcondflds <- estdat$pltcondflds
    
    
    ###################################################################################
    ### Check row and column data
    ###################################################################################
    rowcolinfo <- 
      check.rowcol(esttype = esttype, 
                   popType = popType,
                   popdatindb = popdatindb,
                   popconn = popconn, SCHEMA. = SCHEMA.,
                   pltcondx = pltcondx,
                   pltcondflds = pltcondflds,
                   withqry = pltcondxWITHqry,
                   cuniqueid = cuniqueid, condid = condid,
                   rowvar = rowvar, colvar = colvar, 
                   row.FIAname = row.FIAname, col.FIAname = col.FIAname, 
                   row.orderby = row.orderby, col.orderby = col.orderby, 
                   row.add0 = row.add0, col.add0 = col.add0, 
                   row.classify = row.classify, col.classify = col.classify,
                   title.rowvar = title.rowvar, title.colvar = title.colvar, 
                   rowlut = rowlut, collut = collut, 
                   rowgrp = rowgrp, rowgrpnm = rowgrpnm, 
                   rowgrpord = rowgrpord, title.rowgrp = NULL, 
                   gui = gui)
    uniquerow <- rowcolinfo$uniquerow
    uniquecol <- rowcolinfo$uniquecol
    bydomainlst <- rowcolinfo$domainlst
    rowvar <- rowcolinfo$rowvar
    colvar <- rowcolinfo$colvar
    rowvarnm <- rowcolinfo$rowvarnm
    colvarnm <- rowcolinfo$colvarnm
    row.orderby <- rowcolinfo$row.orderby
    col.orderby <- rowcolinfo$col.orderby
    row.add0 <- rowcolinfo$row.add0
    col.add0 <- rowcolinfo$col.add0
    title.rowvar <- rowcolinfo$title.rowvar
    title.colvar <- rowcolinfo$title.colvar
    rowgrpnm <- rowcolinfo$rowgrpnm
    title.rowgrp <- rowcolinfo$title.rowgrp
    grpvar <- rowcolinfo$grpvar
    classifyrow <- rowcolinfo$classifyrow
    classifycol <- rowcolinfo$classifycol
    #rm(rowcolinfo)
    
    ## if classified columns, create domclassify list for summarizing tree data
    if (any(!is.null(classifyrow), !is.null(classifycol))) {
      domclassify <- list()
      if (!is.null(classifyrow)) {
        domclassify[[rowvar]] <- classifyrow$row.classify
      }
      if (!is.null(classifycol)) {
        domclassify[[colvar]] <- classifycol$col.classify
      }
    }
    
    
    ###################################################################################
    ### Get condition-level domain data
    ###################################################################################
    conddat <- 
      check.cond(areawt = areawt,
                 areawt2 = areawt2,
                 adj = adj,
                 adjcase = adjcase,
                 cuniqueid = cuniqueid, 
                 condid = condid,
                 rowvar = rowvar, 
                 colvar = colvar, 
                 pcdomainlst = unique(c(bydomainlst, "TOTAL")),
                 popdatindb = popdatindb,
                 popconn = popconn,
                 pltcondx = pltcondx,
                 pltidsadj = pltidsadj,
                 pltidsid = pltidsid,
                 pltcondxadjWITHqry = pltcondxadjWITHqry,
                 pcwhereqry = pcwhereqry,
                 classifyrow = classifyrow,
                 classifycol = classifycol)
    if (is.null(conddat)) stop(NULL)
    cdomdat <- conddat$cdomdat
    cdomdatqry <- conddat$cdomdatqry
    estnm <- conddat$estnm
    rowvar <- conddat$rowvar
    colvar <- conddat$colvar
    grpvar <- conddat$grpvar
    

    #####################################################################################
    ## GENERATE ESTIMATES
    #####################################################################################
    SAestimates <- 
      getSAestimates(esttype = esttype, i = i,
                     largebnd.unique = largebnd.unique,
                     estvar.name = estnm,
                     domdat = cdomdat,
                     pltassgnx = pltassgnx,
                     dunitlut = dunitlut,
                     dunitvar = dunitvar,
                     uniqueid = pltassgnid,
                     prednames = prednames,
                     rowvar = rowvar,
                     SApopdatnm = SApopdatnm,
                     SAdomsDF = SAdomsdf,
                     smallbnd.dom = smallbnd.dom,
                     vars2keep = vars2keep,
                     SApackage = SApackage,
                     SAmethod = SAmethod,
                     showsteps = showsteps,
                     savesteps = savesteps,
                     stepfolder = stepfolder,
                     prior = prior,
                     modelselect = modelselect,
                     multest = multest,
                     multest_estimators = multest_estimators,
                     SAobjlst = SAobjlst,
                     estlst = estlst,
                     pdomdatlst = pdomdatlst,
                     dunitlutlst = dunitlutlst,
                     uniquerow = uniquerow,
                     uniquecol = uniquecol,
                     SAdomvars = SAdomvars,
                     SAobjlst_row = SAobjlst_row,
                     estlst_row = estlst_row,
                     predselectlst.unit = predselectlst.unit,
                     predselectlst.area = predselectlst.area,
                     predselectlst.unit_row = predselectlst.unit_row,
                     predselectlst.area_row = predselectlst.area_row,
                     pdomdatlst_row = pdomdatlst_row,
                     dunitlutlst_row = dunitlutlst_row,
                     row.NAname = row.NAname,
                     col.NAname = col.NAname,
                     save4testing = save4testing) 
    if (is.numeric(SAestimates) && SAestimates == 0) stop()
    largebnd.unique <-  SAestimates$largebnd.unique
    response <- SAestimates$response
    domdat <- SAestimates$domdat
    SAobjlst <- SAestimates$SAobjlst
    estlst <- SAestimates$estlst
    pdomdatlst <- SAestimates$pdomdatlst
    dunitlutlst <- SAestimates$dunitlutlst
    SAobjlst_row <- SAestimates$SAobjlst_row
    estlst_row <- SAestimates$estlst_row
    predselectlst.unit <- SAestimates$predselectlst.unit
    predselectlst.area <- SAestimates$predselectlst.area
    predselectlst.unit_row <- SAestimates$predselectlst.unit_row
    predselectlst.area_row <- SAestimates$predselectlst.area_row
    pdomdatlst_row <- SAestimates$pdomdatlst_row
    dunitlutlst_row <- SAestimates$dunitlutlst_row

  }    #### end SApopdat loop
  
  ## Combine estimates
  estdf <- do.call(rbind, estlst)

  ## Check for AOI column
  if (!"AOI" %in% names(estdf)) {
    estdf$AOI <- 1
  }	

  SAEunit_estimators <- c("all", "saeU", "JU.EBLUP", "JU.EBLUP.se.1", "JU.GREG", "JU.GREG.se", 
                          "JU.Synth", "hbsaeU", "hbsaeU.se")
  SAEarea_estimators <- c("all", "saeA", "JFH", "JFH.se", "JA.Synth", "hbsaeA", "hbsaeA.se")
  
  if ((multest && any(multest_estimators %in% SAEunit_estimators)) || SAmethod == "unit" &&
      length(predselectlst.unit) > 0) {
    predselect.unitdf <- data.frame(DOMAIN=names(predselectlst.unit), 
					                          do.call(rbind, predselectlst.unit))

    setnames(predselect.unitdf, "DOMAIN", largebnd.unique)
    predselect.unitdf[is.na(predselect.unitdf)] <- 0
  } 
  if ((multest && any(multest_estimators %in% SAEarea_estimators)) || SAmethod == "area"&&
      length(predselectlst.area) > 0) {
    predselect.areadf <- data.frame(DOMAIN=names(predselectlst.area), 
					                          do.call(rbind, predselectlst.area))

    setnames(predselect.areadf, "DOMAIN", largebnd.unique)
    predselect.areadf[is.na(predselect.areadf)] <- 0
  }

  ## Merge SAdom attributes to estdf
  ################################################
  if (addSAdomsdf && is.null(SAdomvars)) {
    
    SAdomvars2 <- unique(names(SAdomsdfbind)[!names(SAdomsdfbind) %in% names(estdf)])
    estdf <- merge(setDF(SAdomsdfbind)[,c("DOMAIN", SAdomvars2)], estdf, by="DOMAIN")
    estdf <- estdf[order(-estdf$AOI, estdf[["DOMAIN"]]),]

  } else if (addSAdomsdf && !is.null(SAdomvars)) {
    
    SAdomvars2 <- SAdomvars[SAdomvars %in% names(SAdomsdfbind)]
    SAdomvars2 <- unique(SAdomvars2[!SAdomvars2 %in% names(estdf)])
  
    if (length(SAdomvars2) != 0) {
      estdf <- merge(setDF(SAdomsdfbind)[, unique(c("DOMAIN", SAdomvars2))], 
					           estdf, by="DOMAIN")
      estdf <- estdf[order(-estdf$AOI, estdf[["DOMAIN"]]),]
    }
    
  } else {

    estdf <- estdf[order(-estdf$AOI, estdf[["DOMAIN"]]),]
    
  }

  ################################################################################
  ## Get estimates by row
  ################################################################################
  if (rowcolinfo$rowvar != "TOTAL") {

    ## Combine estimates
    estdf_row <- do.call(rbind, estlst_row)

    ## Check for AOI column
    if (!"AOI" %in% names(estdf)) {
      estdf_row$AOI <- 1
    }	

    if ((multest && any(multest_estimators %in% SAEunit_estimators)) || SAmethod == "unit") {
      
      predselect.unitdf_row <- data.frame(DOMAIN=names(predselectlst.unit_row), 
			                                    do.call(rbind, predselectlst.unit_row))
      setnames(predselect.unitdf_row, "DOMAIN", largebnd.unique)
      predselect.unitdf_row[is.na(predselect.unitdf_row)] <- 0
      
    } 
    if ((multest && any(multest_estimators %in% SAEarea_estimators)) || SAmethod == "area") {
      
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
                         estdf_row, by="DOMAIN")
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
    multestdf <- copy(estdf)
    multestdf[is.na(multestdf$AOI), "AOI"] <- 0
    if (rowcolinfo$rowvar != "TOTAL") {
      multestdf_row <- estdf_row
      multestdf_row[is.na(multestdf_row$AOI), "AOI"] <- 0
    }
  }

  ## Set up estimates. If estimate is NULL, use direct estimator
  estdf <- data.table::setDT(estdf)
  estdf[, c("nhat", "nhat.se") := .SD, .SDcols=c(nhat, nhat.se)]
  estdf$estimator <- nhat
  estdf <- data.table::setDF(estdf)
  
  if (na.fill != "NONE") {
    if (any(is.na(estdf$nhat))) {
      message("filling NA values with estimates generated from: ", na.fill)
    }
    estdf[is.na(estdf$nhat), "estimator"] <- na.fill
    if (na.fill == "JU.EBLUP") {
      na.fill.se <- "JU.EBLUP.se.1"
    } else {
      na.fill.se <- paste0(na.fill, ".se")
    }
    estdf[is.na(estdf$nhat), c("nhat", "nhat.se")] <- 
            estdf[is.na(estdf$nhat), c(na.fill, na.fill.se)]
  }

  ## Change values that are less than 0 to 0
  if (!lt0 && any(!is.na(estdf$nhat)) && any(na.omit(estdf$nhat) < 0)) {
    estdf[!is.na(estdf$nhat) & estdf$nhat < 0, "nhat"] <- 0
  } 

  ## Subset multest to estimation output
  subvars <- c("DOMAIN", "nhat", "nhat.se", "NBRPLT.gt0", "estimator")
  dunit_totest <- setDT(estdf[estdf$AOI==1, subvars])
  setkeyv(dunit_totest, "DOMAIN")
  
  ## Merge dunitarea
  tabs <- check.matchclass(dunitareabind, dunit_totest, "DOMAIN")
  dunitareabind <- tabs$tab1
  dunit_totest <- tabs$tab2
  dunit_totest <- merge(dunit_totest, 
                        dunitareabind[, c("DOMAIN", "AREAUSED"), with=FALSE],
                        by="DOMAIN")
  
  if (!is.null(dunit_totest)) {
    dunit_totest[, nhat.var := nhat.se^2]
    if (totals) {
      dunit_totest <- getpse(dunit_totest,
                             areavar=areavar,
                             esttype=esttype)
    } else {
      dunit_totest <- getpse(dunit_totest,
                             esttype=esttype)
    }
  }

  if (rowcolinfo$rowvar != "TOTAL") {
    
    ## Set up estimates. If estimate is NULL, use direct estimator
    estdf_row <- data.table::setDT(estdf_row)
    estdf_row[, c("nhat", "nhat.se") := .SD, .SDcols=c(nhat, nhat.se)]
    estdf_row$estimator <- nhat
    estdf_row <- data.table::setDF(estdf_row)
    
    if (na.fill != "NONE") {
      if (any(is.na(estdf_row$nhat))) {
        message("filling NA values for row estimates with estimates generated from: ", na.fill)
      }
      estdf_row[is.na(estdf_row$nhat), "estimator"] <- na.fill
      if (na.fill == "JU.EBLUP") {
        na.fill.se <- "JU.EBLUP.se.1"
      } else {
        na.fill.se <- paste0(na.fill, ".se")
      }
      estdf_row[is.na(estdf_row$nhat), c("nhat", "nhat.se")] <- 
        estdf_row[is.na(estdf_row$nhat), c(na.fill, na.fill.se)]
    }
    
    ## Change values that are less than 0 to 0
    if (!lt0 && any(!is.na(estdf_row$nhat)) && any(na.omit(estdf_row$nhat) < 0)) {
      estdf_row[!is.na(estdf_row$nhat) & estdf_row$nhat < 0, "nhat"] <- 0
    } 
    
    ## Subset multest to estimation output
    subvars <- c("DOMAIN", "nhat", "nhat.se", "NBRPLT.gt0", "estimator")
    dunit_rowest <- setDT(estdf_row[estdf_row$AOI==1, c(subvars, rowvar)])
    setkeyv(dunit_rowest, "DOMAIN")
    
    ## Merge dunitarea
    tabs <- check.matchclass(dunitareabind, dunit_rowest, "DOMAIN")
    dunitareabind <- tabs$tab1
    dunit_rowest <- tabs$tab2
    dunit_rowest <- merge(dunit_rowest, 
		dunitareabind[, c("DOMAIN", "AREAUSED"), with=FALSE], by="DOMAIN")

    if (!is.null(dunit_rowest)) {
      
      dunit_rowest <- add0unit(x=dunit_rowest,
                               xvar=rowcolinfo$rowvar,
                               uniquex=uniquerow, 
                               unitvar=dunitvar,
                               xvar.add0=row.add0)
      
      tabs <- check.matchclass(dunitareabind, dunit_rowest, dunitvar)
      dunitareabind <- tabs$tab1
      dunit_rowest <- tabs$tab2
      
	}
	
	if (!is.null(dunit_rowest)) {
	  
      dunit_rowest[, nhat.var := nhat.se^2]

      if (totals) {
        dunit_rowest <- getpse(dunit_rowest,
                               areavar=areavar,
                               esttype=esttype)
      } else {
        dunit_rowest <- getpse(dunit_rowest
                               , esttype=esttype)
      }
	  
    }
  }
  
  #####################################################################################
  ### GET TITLES FOR OUTPUT TABLES
  #####################################################################################
  title.dunitvar <- ifelse(is.null(title.unitvar), smallbnd.dom, title.unitvar)
  alltitlelst <- 
    check.titles(dat = cdomdat, esttype = esttype, 
                 title.main = title.main, 
                 title.ref = title.ref, 
                 title.rowvar = title.rowvar, 
                 title.rowgrp = title.rowgrp, 
                 title.colvar = title.colvar, 
                 title.unitvar = title.unitvar, 
                 title.filter = title.filter, 
                 title.unitsn = areaunits, 
                 unitvar = dunitvar, 
                 rowvar = rowvar, colvar=colvar, 
                 addtitle = addtitle, 
                 returntitle = returntitle, 
                 rawdata = rawdata, 
                 states = states, invyrs = invyrs, 
                 landarea = landarea, pcfilter = pcfilter, 
                 allin1 = allin1, divideby = divideby, 
                 outfn.pre = outfn.pre)
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
  estnm <- "est"
  tabs <- 
    est.outtabs(esttype = esttype, 
                sumunits = sumunits, 
                areavar = areavar, 
                unitvar ="DOMAIN", 
                unit_totest = dunit_totest, 
                unit_rowest = dunit_rowest, 
                unit_colest = dunit_colest, 
                unit_grpest = dunit_grpest, 
                rowvar = rowvarnm, colvar = colvarnm, 
                uniquerow = uniquerow, uniquecol = uniquecol, 
                rowgrp = rowgrp, rowgrpnm = rowgrpnm, 
                rowunit = rowunit, totunit = totunit, 
                allin1 = allin1, 
                savedata = savedata, addtitle = addtitle, 
                title.ref= title.ref, 
                title.colvar = title.colvar, title.rowvar = title.rowvar, 
                title.rowgrp = title.rowgrp, title.unitvar = title.dunitvar, 
                title.estpse = title.estpse, 
                title.est = title.est, title.pse = title.pse, 
                rawdata = rawdata, rawonly = rawonly, 
                outfn.estpse = outfn.estpse2, outfolder = outfolder, 
                outfn.date = outfn.date, overwrite = overwrite_layer, 
                estnm = estnm, 
                estround = estround, pseround = pseround, 
                divideby = divideby, 
                returntitle = returntitle, 
                estnull = estnull, psenull = psenull) 
  est2return <- tabs$tabest
  pse2return <- tabs$tabpse


  if (!is.null(est2return)) {
    returnlst$est <- est2return
  }  
  if (!is.null(pse2return)) {
    returnlst$pse <- pse2return 
  }
  if (returntitle) {
    returnlst$titlelst <- alltitlelst
  }

  domain <- "TOTAL"
  if (multest && !is.null(multestdf)) {
 
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
    #tabs <- check.matchclass(dunitarea, multestdf, )
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

      if (multest_fmt == "csv") {
        multest_basename <- basename.NoExt(multest_layer)
        #multest_layer <- paste0("SAmultest_", SApackage, "_", response, ".csv")
        #multest_layer_row <- paste0("SAmultest_", response, "_", rowvar, ".csv")
        multest_layer_row <- paste0(multest_basename, "_multest_row.csv")
      } else {
        #multest_layer <- paste0(SApackage, "_", response)
        multest_layer_row <- paste0(response, "_", rowvar)
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
    if (rowcolinfo$rowvar != "TOTAL") {
      rawdat$rowvar <- rowvar
      if ((multest && any(multest_estimators %in% SAEunit_estimators)) || SAmethod == "unit") {
        rawdat$predselect.unit_row <- predselect.unitdf_row
      }
      if ((multest && any(multest_estimators %in% SAEarea_estimators)) || SAmethod == "area") {
        rawdat$predselect.area_row <- predselect.areadf_row
      }
    }
    if (rowcolinfo$colvar != "NONE") {
	  rawdat$colvar <- rowcolinfo$colvar
	  }
    rawdat$areaunits <- areaunits
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

    returnlst$domdat <- domdat
    returnlst$dunitlut <- dunitlut
    returnlst$cuniqueid <- cuniqueid
  }

  return(returnlst)
  
}


