#' Model-Assisted module - Generate model-assisted tree estimates.
#' 
#' Generates tree estimates by estimation unit. Estimates are calculated from
#' McConville et al. (2018)'s mase R package.
#' 
#' If variables are NULL, then it will prompt user to input variables.
#' 
#' Necessary variables:\cr \tabular{llll}{ \tab \bold{Data} \tab
#' \bold{Variable} \tab \bold{Description}\cr \tab tree \tab tuniqueid \tab
#' Unique identifier for each plot, to link to pltassgn (e.g. PLT_CN).\cr \tab
#' \tab CONDID \tab Unique identifier of each condition on plot, to link to
#' cond.  Set CONDID=1, if only 1 condition per plot.\cr \tab \tab TPA_UNADJ
#' \tab Number of trees per acre each sample tree represents (e.g., DESIGNCD=1:
#' TPA_UNADJ=6.018046 for trees on subplot; 74.965282 for trees on
#' microplot).\cr \tab cond \tab cuniqueid \tab Unique identifier for each
#' plot, to link to pltassgn (ex. PLT_CN).\cr \tab \tab CONDID \tab Unique
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
#' MACRPROP_UNADJ=1, if only 1 condition per macroplot.\cr \tab pltassgn \tab
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
#' @param MApopdat List. Population data objects returned from modMApop().
#' @param MAmethod String. mase (i.e., model-assisted) method to use 
#' ('greg', 'gregEN', 'ratio').
#' @param estvar String. Name of the tree-level estimate variable (e.g.,
#' 'VOLCFNET').
#' @param estvar.filter String. A tree-level filter for estvar. Must be R
#' syntax (e.g., 'STATUSCD == 1').
#' @param estseed String. Use seedling data only or add to tree data. Seedling
#' estimates are only for counts (estvar='TPA_UNADJ')-('none', 'only', 'add').
#' @param landarea String. The condition-level filter for defining land area
#' ('ALL', 'FOREST', 'TIMBERLAND'). If landarea='FOREST', COND_STATUS_CD = 1;
#' if landarea='TIMBERLAND', SITECLCD in(1:6) & RESERVCD = 0.
#' @param pcfilter String. A filter for plot or cond attributes (including
#' pltassgn).  Must be R logical syntax.
#' @param rowvar String. Optional. Name of domain variable to group estvar by
#' for rows in table output. Rowvar must be included in an input data frame
#' (i.e., plt, cond, tree). If no rowvar is included, an estimate is returned
#' for the total estimation unit. Include colvar for grouping by 2 variables.
#' @param colvar String. Optional. If rowvar != NULL, name of domain variable
#' to group estvar by for columns in table output. Colvar must be included in
#' an input data frame (i.e., plt, cond, tree).
#' @param prednames String vector. Name(s) of predictor variables to include in
#' model.
#' @param modelselect Logical. If TRUE, variable selection occurs. 
#' @param FIA Logical. If TRUE, the finite population term is removed from
#' estimator to match FIA estimates.
#' @param bootstrap Logical. If TRUE, returns bootstrap variance estimates,
#' otherwise uses Horvitz-Thompson estimator under simple random sampling
#' without replacement.
#' @param returntitle Logical. If TRUE, returns title(s) of the estimation
#' table(s).
#' @param savedata Logical. If TRUE, saves table(s) to outfolder.
#' @param table_opts List. See help(table_options()) for a list of
#' options.
#' @param title_opts List. See help(title_options()) for a list of options.
#' @param savedata_opts List. See help(savedata_options()) for a list
#' of options. Only used when savedata = TRUE.  
#' @param gui Logical. If gui, user is prompted for parameters.
#' @param ...  Parameters for modMApop() if MApopdat is NULL.
#' @return If FIA=TRUE or unitvar=NULL and colvar=NULL, one data frame is
#' returned with tree estimates and percent sample errors. Otherwise, a list is
#' returned with tree estimates in one data frame (est) and percent sample
#' errors in another data frame (est.pse). If rawdata=TRUE, another list is
#' returned including raw data used in the estimation process.  If
#' addtitle=TRUE and returntitle=TRUE, the title for est/pse is returned. If
#' savedata=TRUE, all data frames are written to outfolder.
#' 
#' \item{est}{ Data frame. Tree estimates by rowvar, colvar (and estimation
#' unit). If FIA=TRUE or one estimation unit and colvar=NULL, estimates and
#' percent sampling error are in one data frame. } \item{pse}{ Data frame.
#' Percent sampling errors for estimates by rowvar and colvar (and estimation
#' unit). } \item{titlelst}{ List with 1 or 2 string vectors. If
#' returntitle=TRUE a list with table title(s). The list contains one title if
#' est and pse are in the same table and two titles if est and pse are in
#' separate tables. } \item{raw}{ List of data frames. If rawdata=TRUE, a list
#' including: number of plots by plot status, if in dataset (plotsampcnt);
#' number of conditions by condition status (condsampcnt); data used for
#' post-stratification (stratdat); and 1-8 tables with calculated variables
#' used for processing estimates and percent sampling error for table cell
#' values and totals (See processing data below). }
#' 
#' Raw data
#' 
#' \item{plotsampcnt}{ Table. Number of plots by plot status (ex. sampled
#' forest on plot, sampled nonforest, nonsampled). } \item{condsampcnt}{ DF.
#' Number of conditions by condition status (forest land, nonforest land,
#' noncensus water, census water, nonsampled). }
#' 
#' \item{stratdat}{ Data frame. Strata information by estimation unit. }
#' \tabular{lll}{ \tab \bold{Variable} \tab \bold{Description}\cr \tab ESTUNIT
#' \tab estimation unit\cr \tab STRATA \tab strata \cr \tab ACRES \tab area by
#' strata for estimation unit\cr \tab n.strata \tab number of plots in strata
#' (and estimation unit) \cr \tab n.total \tab number of plots for estimation
#' unit \cr \tab TOTACRES \tab total area for estimation unit \cr \tab strwt
#' \tab proportion of area (or number of plots) by strata (strata weight) \cr
#' \tab expfac.strata \tab expansion factor (in area unit (e.g., acres) by
#' strata (areavar/n.strata) \cr }
#' 
#' \item{processing data}{ Data frames. Separate data frames containing
#' calculated variables used in estimation process. The number of processing
#' tables depends on the input parameters. The tables include: total by
#' estimation unit (unit.totest); rowvar totals (unit.rowest), and if colvar is
#' not NULL, colvar totals, (unit.colvar); and a combination of rowvar and
#' colvar (unit.grpvar). If FIA=TRUE, the raw data for the summed estimation
#' units are also included (totest, rowest, colest, grpest, respectively).
#' These tables do not included estimate proportions (nhat and nhat.var).
#' 
#' The data frames include the following information: \tabular{lll}{ \tab
#' \bold{Variable} \tab \bold{Description}\cr \tab nhat \tab estimated
#' proportion of trees \cr \tab nhat.var \tab estimated variance of estimated
#' proportion of trees \cr \tab ACRES \tab total area for estimation unit \cr
#' \tab est \tab estimated area of trees nhat*ACRES \cr \tab est.var \tab
#' estimated variance of estimated area of trees nhat.var*areavar^2 \cr \tab
#' est.se \tab standard error of estimated area of trees sqrt(est.var) \cr \tab
#' est.cv \tab coefficient of variation of estimated area of trees est.se/est
#' \cr \tab pse \tab percent sampling error of estimate est.cv*100 \cr \tab
#' CI99left \tab left tail of 99 percent confidence interval for estimated area
#' \cr \tab CI99right \tab right tail of 99 percent confidence interval for
#' estimated area \cr \tab CI95left \tab left tail of 95 percent confidence
#' interval for estimated area \cr \tab CI95right \tab right tail of 95 percent
#' confidence interval for estimated area \cr \tab CI67left \tab left tail of
#' 67 percent confidence interval for estimated area \cr \tab CI67right \tab
#' right tail of 67 percent confidence interval for estimated area \cr } }
#' 
#' Table(s) are also written to outfolder.
#' @note
#' 
#' ADJUSTMENT FACTOR:\cr The adjustment factor is necessary to account for
#' nonsampled conditions. It is calculated for each estimation unit by strata.
#' by summing the unadjusted proportions of the subplot, microplot, and
#' macroplot (i.e. *PROP_UNADJ) and dividing by the number of plots in the
#' strata/estimation unit).
#' 
#' An adjustment factor is determined for each tree based on the size of the
#' plot it was measured on. This is identified using TPA_UNADJ as follows:
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
#' sumunits:\cr An estimation unit is a population, or area of interest, with
#' known area and number of plots. Individual counties or combined
#' Super-counties are common estimation units for FIA. An estimation unit may
#' also be a subpopulation of a larger population (e.g., Counties within a
#' State). Subpopulations are mutually exclusive and independent within a
#' population, therefore estimated totals and variances are additive. For
#' example, State-level estimates are generated by summing estimates from all
#' subpopulations within the State (Bechtold and Patterson. 2005. Chapter 2).
#' Each plot must be assigned to only one estimation unit.
#' 
#' stratcombine:\cr If MAmethod='PS', and stratcombine=TRUE, and less than 2
#' plots in any one estimation unit, all estimation units with 10 or less plots
#' are combined. The current method for combining is to group the estimation
#' unit with less than 10 plots with the estimation unit following in
#' consecutive order (numeric or alphabetical), restrained by survey unit
#' (UNITCD) if included in dataset, and continuing until the number of plots
#' equals 10. If there are no estimation units following in order, it is
#' combined with the estimation unit previous in order.
#' 
#' autoxreduce:\cr If MAmethod='GREG', and autoxreduce=TRUE, and there is an
#' error because of multicolinearity, a variable reduction method is applied to
#' remove correlated variables. The method used is based on the
#' variance-inflation factor (vif) from a linear model. The vif estimates how
#' much the variance of each x variable is inflated due to mulitcolinearity in
#' the model.
#' 
#' rowlut/collut:\cr There are several objectives for including rowlut/collut
#' look-up tables: 1) to include descriptive names that match row/column codes
#' in the input table; 2) to use number codes that match row/column names in
#' the input table for ordering rows; 3) to add rows and/or columns with 0
#' values for consistency. No duplicate names are allowed.
#' 
#' Include 2 columns in the table:\cr 1-the merging variable with same name as
#' the variable in the input merge table;\cr 2-the ordering or descriptive
#' variable.\cr If the ordering variable is the rowvar/colvar in the input
#' table and the descriptive variable is in rowlut/collut, set
#' row.orderby/col.orderby equal to rowvar/colvar. If the descriptive variable
#' is the rowvar/colvar in the input table, and the ordering code variable is
#' in rowlut/collut, set row.orderby/col.orderby equal to the variable name of
#' the code variable in rowlut/collut.
#' 
#' UNITS:\cr The following variables are converted from pounds (from FIA
#' database) to short tons by multiplying the variable by 0.0005.  DRYBIO_AG,
#' DRYBIO_BG, DRYBIO_WDLD_SPP, DRYBIO_SAPLING, DRYBIO_STUMP, DRYBIO_TOP,
#' DRYBIO_BOLE, DRYBIOT, DRYBIOM, DRYBIOTB, JBIOTOT, CARBON_BG, CARBON_AG
#' 
#' MORTALITY:\cr For Interior-West FIA, mortality estimates are mainly based on
#' whether a tree has died within the last 5 years of when the plot was
#' measured. If a plot was remeasured, mortality includes trees that were alive
#' the previous visit but were dead in the next visit. If a tree was standing
#' the previous visit, but was not standing in the next visit, no diameter was
#' collected (DIA = NA) but the tree is defined as mortality.
#' 
#' Common tree filters: \cr
#' 
#' \tabular{llr}{ \tab \bold{FILTER} \tab \bold{DESCRIPTION} \cr \tab "STATUSCD
#' == 1" \tab Live trees \cr \tab "STATUSCD == 2" \tab Dead trees \cr \tab
#' "TPAMORT_UNADJ > 0" \tab Mortality trees \cr \tab "STATUSCD == 2 & DIA >=
#' 5.0" \tab Dead trees >= 5.0 inches diameter \cr \tab "STATUSCD == 2 &
#' AGENTCD == 30" \tab Dead trees from fire \cr }
#' @author Tracey S. Frescino
#' @references Kelly McConville, Becky Tang, George Zhu, Shirley Cheung, and
#' Sida Li (2018). mase: Model-Assisted Survey Estimation. R package version
#' 0.1.2 https://cran.r-project.org/package=mase
#' @keywords data
#' @examples
#' \donttest{
#' # Set up population dataset (see ?modMApop() for more information)
#' MApopdat <- modMApop(popTabs = list(tree = FIESTA::WYtree,
#'                                     cond = FIESTA::WYcond),
#'                      pltassgn = FIESTA::WYpltassgn,
#'                      pltassgnid = "CN",
#'                      unitarea = FIESTA::WYunitarea,
#'                      unitvar = "ESTN_UNIT",
#'                      unitzonal = FIESTA::WYunitzonal,
#'                      prednames = c("dem", "tcc", "tpi", "tnt"),
#'                      predfac = "tnt")
#'                      
#' # Use GREG Estimator to Estimate cubic foot volume of live trees in our
#' # population
#' modMAtree(MApopdat = MApopdat,
#'           MAmethod = "greg",
#'           estvar = "VOLCFNET",
#'           estvar.filter = "STATUSCD == 1")
#'           
#' # Use GREG Elastic Net Estimator to Estimate basal area of live trees in our
#' # population
#' modMAtree(MApopdat = MApopdat,
#'           MAmethod = "gregEN",
#'           estvar = "BA",
#'           estvar.filter = "STATUSCD == 1")
#' }
#' @export modMAtree
modMAtree <- function(MApopdat, 
                      MAmethod, 
                      estvar, 
                      estvar.filter = NULL, 
                      estseed = "none", 
                      landarea = "FOREST", 
                      pcfilter = NULL, 
                      rowvar = NULL, 
                      colvar = NULL, 
                      prednames = NULL, 
                      modelselect = FALSE, 
                      FIA = TRUE,
                      bootstrap = FALSE,
                      returntitle = FALSE, 
                      savedata = FALSE, 
                      table_opts = NULL, 
                      title_opts = NULL, 
                      savedata_opts = NULL, 
                      gui = FALSE, 
                      ...){

  ########################################################################################
  ## DESCRIPTION: 
  ## Generates model-assisted estimates by domain (and estimation unit)
  ######################################################################################

  ## CHECK GUI - IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  if (nargs() == 0 && is.null(MApopdat)) {
    gui <- TRUE
  } 
  
  ## If gui.. set variables to NULL
  if (gui) { 
    tree=landarea=strvar=areavar <- NULL
    if (!row.FIAname) row.FIAname <- NULL
    if (!col.FIAname) col.FIAname <- NULL
  }
  
  ## Set parameters
  minplotnum <- 10
  esttype="TREE"
  parameters <- FALSE
  returnlst <- list()
  sumunits <- FALSE
  
  ## Set global variables
  ONEUNIT=n.total=n.strata=strwt=TOTAL=rowvar.filter=colvar.filter <- NULL
  rawdata <- TRUE 
  
  
  ##################################################################
  ## CHECK PARAMETER NAMES
  ##################################################################
  
  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- c(names(formals(modMAtree)),
		names(formals(modMApop))) 
  if (!all(input.params %in% formallst)) {
    miss <- input.params[!input.params %in% formallst]
    stop("invalid parameter: ", toString(miss))
  }


  ## Check parameter lists
  pcheck.params(input.params, table_opts=table_opts, title_opts=title_opts, 
                savedata_opts=savedata_opts)
  
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

  
  ##################################################################
  ## CHECK PARAMETER INPUTS
  ##################################################################
  
  ## Check MAmethod 
  MAmethodlst <- c("greg", "gregEN", "ratio")
  MAmethod <- pcheck.varchar(var2check=MAmethod, varnm="MAmethod", gui=gui, 
		checklst=MAmethodlst, caption="MAmethod", multiple=FALSE, stopifnull=TRUE)

  if (MAmethod %in% c("greg", "gregEN")) {
    predselectlst <- list()
  }

  list.items <- c("condx", "pltcondx", "cuniqueid", "condid", 
		"ACI.filter", "unitarea", "unitvar", "unitlut", "npixels",
		"npixelvar", "plotsampcnt", "condsampcnt")
    #if (MAmethod == "PS") {
    #  list.items <- c(list.items, "strvar")
    #}
    #if (MAmethod == "greg") {
    #  list.items <- c(list.items, "prednames")
    #}
  MApopdat <- pcheck.object(MApopdat, "MApopdat", list.items=list.items)
  	
  if (is.null(MApopdat)) return(NULL)	
  condx <- MApopdat$condx
  pltcondx <- MApopdat$pltcondx
  treex <- MApopdat$treex
  seedx <- MApopdat$seedx
  if (is.null(treex) && is.null(seedx)) {
    stop("must include tree data for tree estimates")
  }
  cuniqueid <- MApopdat$cuniqueid
  condid <- MApopdat$condid
  tuniqueid <- MApopdat$tuniqueid
  ACI.filter <- MApopdat$ACI.filter
  unitarea <- MApopdat$unitarea
  areavar <- MApopdat$areavar
  areaunits <- MApopdat$areaunits
  unitvar <- MApopdat$unitvar
  unitlut <- MApopdat$unitlut
  unitvars <- MApopdat$unitvars
  npixels <- MApopdat$npixels
  npixelvar <- MApopdat$npixelvar
  expcondtab <- MApopdat$expcondtab
  plotsampcnt <- MApopdat$plotsampcnt
  condsampcnt <- MApopdat$condsampcnt
  states <- MApopdat$states
  invyrs <- MApopdat$invyrs
  stratcombinelut <- MApopdat$stratcombinelut
  predfac <- MApopdat$predfac
  strvar <- MApopdat$strvar
  adj <- MApopdat$adj

  if (MAmethod %in% c("greg", "gregEN", "ratio")) {
    if (is.null(prednames)) {
      prednames <- MApopdat$prednames
    } else {
      if (!all(prednames %in% MApopdat$prednames))
        stop("invalid prednames: ", 
			toString(prednames[!prednames %in% MApopdat$prednames]))
      predfac <- predfac[predfac %in% prednames]
    }
  } 

  ########################################
  ## Check area units
  ########################################
  unitchk <- pcheck.areaunits(unitarea=unitarea, areavar=areavar, 
			areaunits=areaunits, metric=metric)
  unitarea <- unitchk$unitarea
  areavar <- unitchk$areavar
  areaunits <- unitchk$outunits

  if (is.null(key(unitarea))) {
    setkeyv(unitarea, unitvar)
  }
  
  
  ###################################################################################
  ## Check parameters and apply plot and condition filters
  ###################################################################################
  estdat <- check.estdata(esttype=esttype, pltcondf=pltcondx, 
                cuniqueid=cuniqueid, condid=condid, treex=treex, seedx=seedx, 
                estseed=estseed, sumunits=sumunits, landarea=landarea, 
                ACI.filter=ACI.filter, pcfilter=pcfilter, allin1=allin1, 
                estround=estround, pseround=pseround, divideby=divideby, 
                addtitle=addtitle, returntitle=returntitle, 
                rawdata=rawdata, rawonly=rawonly, savedata=savedata, 
                outfolder=outfolder, overwrite_dsn=overwrite_dsn, 
                overwrite_layer=overwrite_layer, outfn.pre=outfn.pre, 
                outfn.date=outfn.date, append_layer=append_layer, 
                raw_fmt=raw_fmt, raw_dsn=raw_dsn, gui=gui)
  if (is.null(estdat)) return(NULL)
  pltcondf <- estdat$pltcondf
  cuniqueid <- estdat$cuniqueid
  treef <- estdat$treef
  seedf <- estdat$seedf
  tuniqueid <- estdat$tuniqueid
  estseed <- estdat$estseed
  sumunits <- estdat$sumunits
  landarea <- estdat$landarea
  allin1 <- estdat$allin1
  estround <- estdat$estround
  pseround <- estdat$pseround
  divideby <- estdat$divideby
  addtitle <- estdat$addtitle
  returntitle <- estdat$returntitle
  rawdata <- estdat$rawdata
  rawonly <- estdat$rawonly
  savedata <- estdat$savedata
  outfolder <- estdat$outfolder
  overwrite_layer <- estdat$overwrite_layer
  raw_fmt <- estdat$raw_fmt
  raw_dsn <- estdat$raw_dsn
  rawfolder <- estdat$rawfolder

  if ("STATECD" %in% names(pltcondf)) {
    states <- pcheck.states(sort(unique(pltcondf$STATECD)))
  }
  if ("INVYR" %in% names(pltcondf)) {
    invyr <- sort(unique(pltcondf$INVYR))
  }

  ###################################################################################
  ### GET ROW AND COLUMN INFO FROM condf
  ###################################################################################
  rowcolinfo <- check.rowcol(gui=gui, esttype=esttype, treef=treef, seedf=seedf, 
                    condf=pltcondf, cuniqueid=cuniqueid, 
                    tuniqueid=tuniqueid, estseed=estseed,
                    rowvar=rowvar, rowvar.filter=rowvar.filter, 
                    colvar=colvar, colvar.filter=colvar.filter, 
                    row.FIAname=row.FIAname, col.FIAname=col.FIAname, 
                    row.orderby=row.orderby, col.orderby=col.orderby, 
                    row.add0=row.add0, col.add0=col.add0, 
                    title.rowvar=title.rowvar, title.colvar=title.colvar, 
                    rowlut=rowlut, collut=collut, rowgrp=rowgrp, 
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
  rowgrpnm <- rowcolinfo$rowgrpnm
  title.rowgrp <- rowcolinfo$title.rowgrp
  bytdom <- rowcolinfo$bytdom
  tdomvar <- rowcolinfo$tdomvar
  tdomvar2 <- rowcolinfo$tdomvar2
  grpvar <- rowcolinfo$grpvar
  #rm(rowcolinfo)  
  
  ## Generate a uniquecol for estimation units
  if (!sumunits && colvar == "NONE") {
    uniquecol <- data.table(unitarea[[unitvar]])
    setnames(uniquecol, unitvar)
    uniquecol[[unitvar]] <- factor(uniquecol[[unitvar]])
  }

  #####################################################################################
  ### GET ESTIMATION DATA FROM TREE TABLE
  #####################################################################################
  adjtree <- ifelse(adj %in% c("samp", "plot"), TRUE, FALSE)
  treedat <- check.tree(gui=gui, treef=treef, seedf=seedf, estseed=estseed, 
                  bycond=TRUE, condf=condf, bytdom=bytdom, 
                  tuniqueid=tuniqueid, cuniqueid=cuniqueid, 
                  esttype=esttype, estvarn=estvar, estvarn.filter=estvar.filter, 
                  esttotn=TRUE, tdomvar=tdomvar, tdomvar2=tdomvar2, 
                  adjtree=adjtree, metric=metric)
  if (is.null(treedat)) return(NULL)
  tdomdat <- treedat$tdomdat

  if (rowvar != "TOTAL") {
    if (!row.add0) {
      if (any(is.na(tdomdat[[rowvar]]))) {
        tdomdat <- tdomdat[!is.na(tdomdat[[rowvar]]), ]
      } else if (any(is.na(tdomdat[[rowvar]]))) {
        tdomdat <- tdomdat[!is.na(tdomdat[[rowvar]]),]
      } else if (any(as.character(tdomdat[[rowvar]]) == "0")) {
        tdomdat <- tdomdat[tdomdat[[rowvar]] != 0,]
      }
    }
    if (colvar != "NONE") {
      if (!col.add0) {
        if (any(is.na(tdomdat[[colvar]]))) {
          tdomdat <- tdomdat[!is.na(tdomdat[[colvar]]), ]
        } else if (any(is.na(tdomdat[[colvar]]))) {
          tdomdat <- tdomdat[!is.na(tdomdat[[colvar]]),]
        } else if (any(as.character(tdomdat[[colvar]]) == "0")) {
          tdomdat <- tdomdat[tdomdat[[colvar]] != 0,]
        }
      }
    }
  }

  ## Merge tdomdat with condx
  xchk <- check.matchclass(condx, tdomdat, c(cuniqueid, condid))
  condx <- xchk$tab1
  tdomdat <- xchk$tab2
  
  tdomdat <- merge(condx, tdomdat, by=c(cuniqueid, condid), all.x=TRUE)
  estvar <- treedat$estvar
  estvar.name <- treedat$estvar.name
  estvar.filter <- treedat$estvar.filter
  tdomvarlst <- treedat$tdomvarlst
  estunits <- treedat$estunits

  #####################################################################################
  ### GET TITLES FOR OUTPUT TABLES
  #####################################################################################
  alltitlelst <- check.titles(dat=tdomdat, esttype=esttype, estseed=estseed, 
                      sumunits=sumunits, title.main=title.main, title.ref=title.ref, 
                      title.rowvar=title.rowvar, title.rowgrp=title.rowgrp, 
                      title.colvar=title.colvar, title.unitvar=title.unitvar, 
                      title.filter=title.filter, title.unitsn=estunits, 
                      title.estvarn=title.estvar, unitvar=unitvar, 
                      rowvar=rowvar, colvar=colvar, estvarn=estvar, 
                      estvarn.filter=estvar.filter, addtitle=addtitle, 
                      returntitle=returntitle, rawdata=rawdata, 
                      states=states, invyrs=invyrs, landarea=landarea, pcfilter=pcfilter, 
                      allin1=allin1, divideby=divideby, outfn.pre=outfn.pre)
  title.unitvar <- alltitlelst$title.unitvar
  title.est <- alltitlelst$title.est
  title.pse <- alltitlelst$title.pse
  title.estpse <- alltitlelst$title.estpse
  title.ref <- alltitlelst$title.ref
  outfn.estpse <- alltitlelst$outfn.estpse
  outfn.param <- alltitlelst$outfn.param
  if (rawdata) {
    outfn.rawdat <- alltitlelst$outfn.rawdat
    outfn.rawdat <- paste0(outfn.rawdat, "_modMA_mase", "_", MAmethod) 
  } 
  ## Append name of package and method to outfile name
  outfn.estpse <- paste0(outfn.estpse, "_modMA_mase", "_", MAmethod) 

  #####################################################################################
  ## GENERATE ESTIMATES
  #####################################################################################
  unit_totest=unit_rowest=unit_colest=unit_grpest=rowunit=totunit <- NULL
  addtotal <- ifelse(((rowvar == "TOTAL" || length(unique(tdomdat[[rowvar]])) > 1) ||
		(!is.null(tdomvarlst) && length(tdomvarlst) > 1)), TRUE, FALSE)
  response <- estvar.name
  estunits <- sort(unique(tdomdat[[unitvar]]))

  masemethod <- ifelse(MAmethod == "PS", "postStrat", 
	ifelse(MAmethod == "greg", "greg", 
	ifelse(MAmethod == "gregEN", "gregElasticNet", 
	ifelse(MAmethod == "ratio", "ratioEstimator", "horvitzThompson"))))
  message("generating estimates using mase::", masemethod, " function...\n")
  if (!MAmethod %in% c("HT", "PS")) {
    message("using the following predictors...", toString(prednames))
  }
  getweights <- ifelse(MAmethod %in% c("greg", "PS", "HT"), TRUE, FALSE) 
  getweights <- FALSE
  
  if(bootstrap) {
    if(MAmethod %in% c('greg', 'gregEN', 'ratio')) {
      var_method <- "bootstrapSRS"
    }
  } else {
    if(MAmethod %in% c('greg', 'gregEN', 'ratio')) {
      var_method <- "LinHTSRS"
    }
  }
  
#  if (addtotal) {
    ## Get total estimate and merge area
    tdomdattot <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(unitvar, cuniqueid, "TOTAL", strvar, prednames), .SDcols=response]
    unit_totestlst <- lapply(estunits, MAest.unit, 
                        dat=tdomdattot, cuniqueid=cuniqueid, 
                        unitlut=unitlut, unitvar=unitvar, esttype=esttype, 
                        MAmethod=MAmethod, strvar=strvar, prednames=prednames, 
                        domain="TOTAL", response=response, npixels=npixels, 
                        FIA=FIA, modelselect=modelselect, getweights=getweights,
                        var_method=var_method)
    unit_totest <- do.call(rbind, sapply(unit_totestlst, '[', "unitest"))
    unit_weights <- do.call(rbind, sapply(unit_totestlst, '[', "weights")) 
    unit_weights$areaweights <- unit_weights$weights * sum(unitarea[[areavar]])
    if (MAmethod %in% c("greg", "gregEN")) {
      predselectlst$totest <- do.call(rbind, sapply(unit_totestlst, '[', "predselect"))
    }
    tabs <- check.matchclass(unitarea, unit_totest, unitvar)
    unitarea <- tabs$tab1
    unit_totest <- tabs$tab2
    setkeyv(unit_totest, unitvar)
    unit_totest <- unit_totest[unitarea, nomatch=0]
    if (totals) {
      unit_totest <- getpse(unit_totest, areavar=areavar, esttype=esttype)
    } else {
      unit_totest <- getpse(unit_totest, esttype=esttype)
    }      
#  }

  ## Get row, column, cell estimate and merge area if row or column in cond table 
  if (rowvar != "TOTAL") {
    tdomdatsum <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(unitvar, cuniqueid, rowvar, strvar, prednames), .SDcols=response]
    unit_rowestlst <- lapply(estunits, MAest.unit, 
                        dat=tdomdatsum, cuniqueid=cuniqueid, 
                        unitlut=unitlut, unitvar=unitvar, esttype=esttype, 
                        MAmethod=MAmethod, strvar=strvar, prednames=prednames, 
                        domain=rowvar, response=response, npixels=npixels, 
                        FIA=FIA, modelselect=modelselect, getweights=getweights,
                        var_method=var_method)
    unit_rowest <- do.call(rbind, sapply(unit_rowestlst, '[', "unitest"))
    if (MAmethod %in% c("greg", "gregEN")) {
      predselectlst$rowest <- do.call(rbind, sapply(unit_totestlst, '[', "predselect"))
    }

    if (colvar != "NONE") {
      tdomdatsum <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(unitvar, cuniqueid, colvar, strvar, prednames), .SDcols=response]
      unit_colestlst <- lapply(estunits, MAest.unit, 
                            dat=tdomdatsum, cuniqueid=cuniqueid, 
                            unitlut=unitlut, unitvar=unitvar, esttype=esttype, 
                            MAmethod=MAmethod, strvar=strvar, prednames=prednames, 
                            domain=colvar, response=response, npixels=npixels, 
                            FIA=FIA, modelselect=modelselect, var_method=var_method)
      unit_colest <- do.call(rbind, sapply(unit_colestlst, '[', "unitest"))
      if (MAmethod %in% c("greg", "gregEN")) {
        predselectlst$colest <- do.call(rbind, sapply(unit_colestlst, '[', "predselect"))
      }

      tdomdatsum <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		        by=c(unitvar, cuniqueid, grpvar, strvar, prednames), .SDcols=response]
      tdomdatsum[, grpvar := do.call(paste, c(.SD, sep="#")), .SDcols=grpvar]

      unit_grpestlst <- lapply(estunits, MAest.unit, 
                               dat=tdomdatsum, cuniqueid=cuniqueid, 
                               unitlut=unitlut, unitvar=unitvar, esttype=esttype, 
                               MAmethod=MAmethod, strvar=strvar, prednames=prednames, 
                               domain="grpvar", response=response, npixels=npixels, 
                               FIA=FIA, modelselect=modelselect, var_method=var_method)
      unit_grpest <- do.call(rbind, sapply(unit_grpestlst, '[', "unitest"))
      if (MAmethod %in% c("greg", "gregEN")) {
        predselectlst$grpest <- do.call(rbind, sapply(unit_grpestlst, '[', "predselect"))
      }
      if (any(unit_grpest$grpvar == "NA#NA")) {
        unit_grpest <- unit_grpest[unit_grpest$grpvar != "NA#NA", ]
      }
      unit_grpest[, c(rowvar, colvar) := tstrsplit(grpvar, "#", fixed=TRUE)]
    }
  }

  ###################################################################################
  ## Check add0 and Add area
  ###################################################################################
  if (!sumunits && nrow(unitarea) > 1) col.add0 <- TRUE
  if (!is.null(unit_rowest)) {
    unit_rowest <- add0unit(x=unit_rowest, xvar=rowvar, uniquex=uniquerow, 
		unitvar=unitvar, xvar.add0=row.add0)
    tabs <- check.matchclass(unitarea, unit_rowest, unitvar)
    unitarea <- tabs$tab1
    unit_rowest <- tabs$tab2

    if (!is.null(row.orderby) && row.orderby != "NONE") {
      setorderv(unit_rowest, c(row.orderby))
    }
    setkeyv(unit_rowest, unitvar)
    unit_rowest <- unit_rowest[unitarea, nomatch=0]
    if (totals) {
      unit_rowest <- getpse(unit_rowest, areavar=areavar, esttype=esttype)
    } else {
      unit_rowest <- getpse(unit_rowest, esttype=esttype)
    }
    setkeyv(unit_rowest, c(unitvar, rowvar))
  }
 
  if (!is.null(unit_colest)) {
    unit_colest <- add0unit(x=unit_colest, xvar=colvar, uniquex=uniquecol, 
		unitvar=unitvar, xvar.add0=col.add0)
    tabs <- check.matchclass(unitarea, unit_colest, unitvar)
    unitarea <- tabs$tab1
    unit_colest <- tabs$tab2

    if (!is.null(col.orderby) && col.orderby != "NONE") {
      setorderv(unit_colest, c(col.orderby))
    }
    setkeyv(unit_colest, unitvar)
    unit_colest <- unit_colest[unitarea, nomatch=0]
    if (totals) {
      unit_colest <- getpse(unit_colest, areavar=areavar, esttype=esttype)
    } else {
      unit_colest <- getpse(unit_colest, esttype=esttype)
    }      
    setkeyv(unit_colest, c(unitvar, colvar))
  }

  if (!is.null(unit_grpest)) {
    unit_grpest <- add0unit(x=unit_grpest, xvar=rowvar, uniquex=uniquerow, 
		unitvar=unitvar, xvar.add0=row.add0, xvar2=colvar, uniquex2=uniquecol,
		xvar2.add0=col.add0)
    tabs <- check.matchclass(unitarea, unit_grpest, unitvar)
    unitarea <- tabs$tab1
    unit_grpest <- tabs$tab2

    if (!is.null(row.orderby) && row.orderby != "NONE") {
      if (!is.null(col.orderby) && col.orderby != "NONE") {
        setorderv(unit_grpest, c(row.orderby, col.orderby))
      } else {
        setorderv(unit_grpest, c(row.orderby))
      }         
    } else if (!is.null(col.orderby) && col.orderby != "NONE") {
      setorderv(unit_grpest, c(col.orderby))
    }         
    setkeyv(unit_grpest, unitvar)
    unit_grpest <- unit_grpest[unitarea, nomatch=0]
    if (totals) {
      unit_grpest <- getpse(unit_grpest, areavar=areavar, esttype=esttype)
    } else {
      unit_grpest <- getpse(unit_grpest, esttype=esttype)
    }      
    setkeyv(unit_grpest, c(unitvar, rowvar, colvar))
  }
 
  ###################################################################################
  ## GENERATE OUTPUT TABLES
  ###################################################################################
  message("getting output...")
  estnm <- "est"
  tabs <- est.outtabs(esttype=esttype, sumunits=sumunits, areavar=areavar, 
                unitvar=unitvar, unitvars=unitvars, unit_totest=unit_totest, 
                unit_rowest=unit_rowest, unit_colest=unit_colest, unit_grpest=unit_grpest, 
                rowvar=rowvar, colvar=colvar, uniquerow=uniquerow, uniquecol=uniquecol, 
                rowgrp=rowgrp, rowgrpnm=rowgrpnm, rowunit=rowunit, totunit=totunit, 
                allin1=allin1, savedata=savedata, addtitle=addtitle, 
                title.ref=title.ref, title.colvar=title.colvar, 
                title.rowvar=title.rowvar, title.rowgrp=title.rowgrp, 
                title.unitvar=title.unitvar, title.estpse=title.estpse, 
                title.est=title.est, title.pse=title.pse, rawdata=rawdata, 
                outfn.estpse=outfn.estpse, outfolder=outfolder, outfn.date=outfn.date, 
                overwrite=overwrite_layer, estnm=estnm, 
                estround=estround, pseround=pseround, divideby=divideby, 
                returntitle=returntitle, estnull=estnull, psenull=psenull) 

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

  if (rawdata) {
    rawdat <- tabs$rawdat
    rawdat$domdat <- setDF(tdomdat)
    #rawdat$expcondtab <- unit_weights
    rawdat$plotweights <- unit_weights
    rawdat$estvar <- estvar.name
    rawdat$estvar.filter <- estvar.filter
  
    if (savedata) {
      if (!is.null(title.estpse)) {
        title.raw <- paste(title.estpse, title.ref)
      } else {
        title.raw <- title.est
      }

      for (i in 1:length(rawdat)) {
        tabnm <- names(rawdat[i])
        if (!tabnm %in% c(estvar, prednames)) {
          rawtab <- rawdat[[i]]
          outfn.rawtab <- paste0(outfn.rawdat, "_", tabnm) 
          if (tabnm %in% c("plotsampcnt", "condsampcnt", "stratcombinelut")) {
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
                                      overwrite_layer=overwrite_layer,
                                      append_layer=append_layer,
                                      add_layer=TRUE))
          }
        }
      }
    }
    rawdat$module <- "MA"
    rawdat$esttype <- "TREE"
    rawdat$MAmethod <- MAmethod
    rawdat$predselectlst <- predselectlst
    rawdat$estvar <- estvar
    rawdat$estvar.filter <- estvar.filter
    if (!is.null(rowvar)) rawdat$rowvar <- rowvar
    if (!is.null(colvar)) rawdat$colvar <- colvar
    rawdat$areaunits <- areaunits
    rawdat$estunits <- estunits
    returnlst$raw <- rawdat
  }
  if ("STATECD" %in% names(pltcondf)) {
    returnlst$statecd <- sort(unique(pltcondf$STATECD))
  }
  if ("INVYR" %in% names(pltcondf)) {
    returnlst$invyr <- sort(unique(pltcondf$INVYR))
  }
    
  return(returnlst)
}

