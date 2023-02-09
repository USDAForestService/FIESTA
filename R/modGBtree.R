#' Green-Book module - Generate tree estimates.
#' 
#' Generates tree and or seedling estimates by domain and/or tree domain (and
#' estimation unit).  Calculations are based on Scott et al. 2005 ('the
#' green-book') for mapped forest inventory plots. The non-ratio estimator for
#' estimating tree attributes by stratum and domain is used. Plots that are
#' totally nonsampled are excluded from estimation dataset. Next, an adjustment
#' factor is calculated by strata to adjust for nonsampled (nonresponse)
#' conditions that have proportion less than 1. Attributes adjusted to a
#' per-acre value are summed by plot, divided by the adjustment factor, and
#' averaged by stratum. Strata means are combined using the strata weights and
#' then expanded to using the total land area in the population.
#' 
#' If variables are NULL, then it will prompt user to input variables.
#' 
#' Necessary variables:\cr \tabular{llll}{ \tab \bold{Data} \tab
#' \bold{Variable} \tab \bold{Description}\cr \tab tree \tab tuniqueid \tab
#' Unique identifier for each plot, to link to pltassgn (ex. PLT_CN).\cr \tab
#' \tab CONDID \tab Unique identifier of each condition on plot, to link to
#' cond.  Set CONDID=1, if only 1 condition per plot.\cr \tab \tab TPA_UNADJ
#' \tab Number of trees per acre each sample tree represents (ex. DESIGNCD=1:
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
#' For available reference tables: sort(unique(FIESTAutils::ref_codes$VARIABLE)) \cr
#' 
#' @param GBpopdat List. Population data objects returned from
#' FIESTA::modGBpop().
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
#' @param sumunits Logical. If TRUE, estimation units are summed and returned
#' in one table.
#' @param returntitle Logical. If TRUE, returns title(s) of the estimation
#' table(s).
#' @param savedata Logical. If TRUE, saves table(s) to outfolder.
#' @param table_opts List. See help(table_options()) for a list of
#' options.
#' @param title_opts List. See help(title_options()) for a list of options.
#' @param savedata_opts List. See help(savedata_options()) for a list
#' of options. Only used when savedata = TRUE.  
#' @param gui Logical. If gui, user is prompted for parameters.
#' @param ...  Parameters for modGBpop() if GBpopdat is NULL.
#' @return A list with estimates with percent sampling error for rowvar (and
#' colvar).  If sumunits=TRUE or unitvar=NULL and colvar=NULL, one data frame
#' is returned.  Otherwise, a list object is returned with the following
#' information.  If savedata=TRUE, all data frames are written to outfolder.
#' 
#' \item{est}{ Data frame. Tree estimates by rowvar, colvar (and estimation
#' unit). If sumunits=TRUE or one estimation unit and colvar=NULL, estimates
#' and percent sampling error are in one data frame. } \item{pse}{ Data frame.
#' Percent sampling errors (Confidence level 68%) for estimates by rowvar and
#' colvar (and estimation unit). Note: for 95% confidence level, multiply
#' percent sampling error by 1.96. } \item{titlelst}{ List with 1 or 2 string
#' vectors. If returntitle=TRUE a list with table title(s). The list contains
#' one title if est and pse are in the same table and two titles if est and pse
#' are in separate tables. } \item{raw}{ List of data frames. If rawdata=TRUE,
#' a list including the processing data used for estimation including: number
#' of plots and conditions; stratification information; and 1 to 8 tables with
#' calculated values for table cells and totals (See processing data below). }
#' 
#' Raw data
#' 
#' \item{plotsampcnt}{ Table. Number of plots by plot status (ex. sampled
#' forest on plot, sampled nonforest, nonsampled). } \item{condsampcnt}{ DF.
#' Number of conditions by condition status (forest land, nonforest land,
#' noncensus water, census water, nonsampled). } \item{unitarea}{ DF. Area by
#' estimation unit. } \item{expcondtab}{ DF. Condition-level area expansion
#' factors. } \item{tdomdat}{ DF. Final data table used for estimation. }
#' 
#' \item{stratdat}{ Data frame. Strata information by estimation unit. }
#' \tabular{lll}{ \tab \bold{Variable} \tab \bold{Description}\cr \tab unitvar
#' \tab estimation unit \cr \tab strvar \tab stratum value \cr \tab strwtvar
#' \tab number of pixels by strata and estimation unit \cr \tab n.strata \tab
#' number of plots in strata (after totally nonsampled plots removed) \cr \tab
#' n.total \tab number of plots for estimation unit \cr \tab strwt \tab
#' proportion of area (or plots) by strata and estimation unit (i.e., strata
#' weight) \cr \tab CONDPROP_UNADJ_SUM \tab summed condition proportion by
#' strata and estimation unit \cr \tab CONDPROP_ADJFAC \tab adjusted condition
#' proportion by strata after nonsampled plots removed \cr }
#' 
#' \item{processing data}{ Data frames. Separate data frames containing
#' calculated variables used in estimation process. The number of processing
#' tables depends on the input parameters. The tables include: total by
#' estimation unit (unit.totest); rowvar totals (unit.rowest), and if colvar is
#' not NULL, colvar totals, (unit.colvar); and a combination of rowvar and
#' colvar (unit.grpvar). If sumunits=TRUE, the raw data for the summed
#' estimation units are also included (totest, rowest, colest, grpest,
#' respectively). These tables do not included estimate proportions (nhat and
#' nhat.var).
#' 
#' The data frames include the following information: \tabular{lll}{ \tab
#' \bold{Variable} \tab \bold{Description}\cr \tab nhat \tab estimated
#' proportion of trees \cr \tab nhat.var \tab variance estimate of estimated
#' proportion of trees \cr \tab NBRPLT.gt0 \tab Number of non-zero plots used
#' in estimates \cr \tab ACRES \tab total area for estimation unit \cr \tab est
#' \tab estimated area of trees nhat*ACRES \cr \tab est.var \tab variance
#' estimate of estimated area of trees nhat.var*areavar^2 \cr \tab est.se \tab
#' standard error of estimated area of trees sqrt(est.var) \cr \tab est.cv \tab
#' coefficient of variation of estimated area of trees est.se/est \cr \tab pse
#' \tab percent sampling error of estimate est.cv*100 \cr \tab CI99left \tab
#' left tail of 99 percent confidence interval for estimated area \cr \tab
#' CI99right \tab right tail of 99 percent confidence interval for estimated
#' area \cr \tab CI95left \tab left tail of 95 percent confidence interval for
#' estimated area \cr \tab CI95right \tab right tail of 95 percent confidence
#' interval for estimated area \cr \tab CI67left \tab left tail of 67 percent
#' confidence interval for estimated area \cr \tab CI67right \tab right tail of
#' 67 percent confidence interval for estimated area \cr } }
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
#' If sumunits=TRUE, estimates are generated by estimation unit, summed
#' together, and returned as one estimate. If rawdata=TRUE, estimates by
#' individual estimation unit are also returned.
#' 
#' If sumunits=FALSE, estimates are generated and returned by estimation unit
#' as one data frame. If savedata=TRUE, a separate file is written for each
#' estimation unit.
#' 
#' stratcombine:\cr If TRUE and less than 2 plots in any one estimation unit,
#' all estimation units with 10 or less plots are combined. The current method
#' for combining is to group the estimation unit with less than 10 plots with
#' the estimation unit following in consecutive order (numeric or
#' alphabetical), restrained by survey unit (UNITCD) if included in dataset,
#' and continuing until the number of plots equals 10. If there are no
#' estimation units following in order, it is combined with the estimation unit
#' previous in order.
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
#' @author Tracey S. Frescino, Paul L. Patterson, Elizabeth A. Freeman
#' @references Scott, Charles T.; Bechtold, William A.; Reams, Gregory A.;
#' Smith, William D.; Westfall, James A.; Hansen, Mark H.; Moisen, Gretchen G.
#' 2005. Sample-based estimators used by the Forest Inventory and Analysis
#' national information management system. Gen. Tech. Rep. SRS-80. Asheville,
#' NC: U.S. Department of Agriculture, Forest Service, Southern Research
#' Station, p.53-77.
#' @keywords data
#' @examples
#' GBpopdat <- modGBpop(
#'   popTabs = list(cond = FIESTA::WYcond,  
#'                  tree = FIESTA::WYtree,        
#'                  seed = FIESTA::WYseed),      
#'   popTabIDs = list(cond = "PLT_CN"),            
#'   pltassgn = FIESTA::WYpltassgn,  
#'   pltassgnid = "CN",        
#'   pjoinid = "PLT_CN",         
#'   unitarea = FIESTA::WYunitarea,
#'   unitvar = "ESTN_UNIT",        
#'   strata = TRUE,           
#'   stratalut = WYstratalut,    
#'   strata_opts = strata_options(getwt = TRUE)   
#' )
#' 
#' tree1.1 <- modGBtree(
#'   GBpopdat = GBpopdat,         # pop - population calculations
#'   landarea = "FOREST",         # est - forest land filter
#'   sumunits = TRUE,             # est - sum estimation units to population
#'   estvar = "VOLCFNET",               # est - net cubic-foot volume
#'   estvar.filter = "STATUSCD == 1",   # est - live trees only
#'   returntitle = TRUE           # out - return title information
#' )
#' str(tree1.1, max.level = 1)
#' 
#' tree1.2 <- modGBtree(
#'   GBpopdat = GBpopdat,         # pop - population calculations
#'   landarea = "FOREST",         # est - forest land filter
#'   sumunits = TRUE,             # est - sum estimation units to population
#'   estvar = "VOLCFNET",               # est - net cubic-foot volume
#'   estvar.filter = "STATUSCD == 1",   # est - live trees only
#'   rowvar = "FORTYPCD",         # est - row domain 
#'   returntitle = TRUE           # out - return title information
#' )
#' str(tree1.2, max.level = 1)
#' @export modGBtree
modGBtree <- function(GBpopdat, 
                      estvar, 
                      estvar.filter=NULL, 
                      estseed="none", 
                      landarea="FOREST", 
                      pcfilter=NULL, 
                      rowvar=NULL, 
                      colvar=NULL, 
                      sumunits=TRUE, 
                      returntitle=FALSE, 
                      savedata=FALSE, 
                      table_opts=NULL, 
                      title_opts=NULL, 
                      savedata_opts=NULL, 
                      gui=FALSE, 
                      ...){

  ##################################################################################
  ## DESCRIPTION:
  ## Generates estimates of trees by domain using non-ratio estimators.
  ##################################################################################

  ## CHECK GUI - IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  if (nargs() == 0 && is.null(GBpopdat)) {
    gui <- TRUE
  } 
  
  ## If gui.. set variables to NULL
  if (gui) { 
    landarea=strvar=areavar=sumunits=adjplot=strata=getwt=cuniqueid=ACI=
      tuniqueid=savedata=addtitle=returntitle=rawdata=rawonly=unitvar <- NULL
    #if (!row.FIAname) row.FIAname <- NULL 
    #if (!col.FIAname) col.FIAname <- NULL 
  }
  
  ## Set parameter
  esttype <- "TREE"
  parameters <- FALSE
  returnlst <- list()
  rawdata <- TRUE  
  
  ## Set global variables
  ONEUNIT=n.total=n.strata=strwt=TOTAL=rowvar.filter=colvar.filter <- NULL
  
  
  ##################################################################
  ## CHECK PARAMETER NAMES
  ##################################################################
  
  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- c(names(formals(modGBtree)),
		names(formals(modGBpop))) 
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
  
  list.items <- c("condx", "pltcondx", "treex", "cuniqueid", "condid", 
	                "tuniqueid", "ACI.filter", "unitarea", "unitvar", "stratalut",
                  "strvar", "plotsampcnt", "condsampcnt")
  GBpopdat <- pcheck.object(GBpopdat, "GBpopdat", list.items=list.items)
  
  if (is.null(GBpopdat)) return(NULL)
  condx <- GBpopdat$condx
  pltcondx <- GBpopdat$pltcondx
  treex <- GBpopdat$treex
  seedx <- GBpopdat$seedx
  if (is.null(treex) && is.null(seedx)) {
    stop("must include tree data for tree estimates")
  }
  cuniqueid <- GBpopdat$cuniqueid
  condid <- GBpopdat$condid
  tuniqueid <- GBpopdat$tuniqueid
  ACI.filter <- GBpopdat$ACI.filter
  unitarea <- GBpopdat$unitarea
  areavar <- GBpopdat$areavar
  areaunits <- GBpopdat$areaunits
  unitvar <- GBpopdat$unitvar
  unitvars <- GBpopdat$unitvars
  stratalut <- GBpopdat$stratalut
  strvar <- GBpopdat$strvar
  expcondtab <- GBpopdat$expcondtab
  plotsampcnt <- GBpopdat$plotsampcnt
  condsampcnt <- GBpopdat$condsampcnt
  states <- GBpopdat$states
  invyrs <- GBpopdat$invyrs
  stratcombinelut <- GBpopdat$stratcombinelut
  strwtvar <- GBpopdat$strwtvar
  adj <- GBpopdat$adj
  strunitvars <- c(unitvar, strvar)
  strata <- GBpopdat$strata

 
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
                cuniqueid=cuniqueid, condid=condid, 
                treex=treex, seedx=seedx, estseed=estseed, sumunits=sumunits, 
                landarea=landarea, ACI.filter=ACI.filter, pcfilter=pcfilter, 
                allin1=allin1, estround=estround, pseround=pseround, 
                divideby=divideby, addtitle=addtitle, returntitle=returntitle, 
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
  append_layer <- estdat$append_layer
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
  ### Check row and column data
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
	                rowgrpnm=rowgrpnm, rowgrpord=rowgrpord, 
	                landarea=landarea)
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
  ### Get estimation data from tree table
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
  tdomdat <- merge(condx, tdomdat, by=c(cuniqueid, condid))

  estvar <- treedat$estvar
  estvar.name <- treedat$estvar.name
  estvar.filter <- treedat$estvar.filter
  tdomvarlst <- treedat$tdomvarlst
  estunits <- treedat$estunits
 
  #####################################################################################
  ### Get titles for output tables
  #####################################################################################
  alltitlelst <- check.titles(dat=tdomdat, esttype=esttype, estseed=estseed, 
	                sumunits=sumunits, title.main=title.main, title.ref=title.ref, 
	                title.rowvar=title.rowvar, title.rowgrp=title.rowgrp, 
	                title.colvar=title.colvar, title.unitvar=title.unitvar, 
	                title.filter=title.filter, title.unitsn=estunits, title.estvarn=title.estvar, 
	                unitvar=unitvar, rowvar=rowvar, colvar=colvar, 
 	                estvarn=estvar, estvarn.filter=estvar.filter, 
	                addtitle=addtitle, returntitle=returntitle, 
	                rawdata=rawdata, states=states, invyrs=invyrs,
	                landarea=landarea, pcfilter=pcfilter, allin1=allin1, 
	                divideby=divideby, outfn.pre=outfn.pre)
  title.unitvar <- alltitlelst$title.unitvar
  title.est <- alltitlelst$title.est
  title.pse <- alltitlelst$title.pse
  title.estpse <- alltitlelst$title.estpse
  title.ref <- alltitlelst$title.ref
  outfn.estpse <- alltitlelst$outfn.estpse
  outfn.param <- alltitlelst$outfn.param
  if (rawdata) {
    outfn.rawdat <- alltitlelst$outfn.rawdat
  }
 
  ############################################################################
  ## GENERATE ESTIMATES
  ############################################################################
  unit_totest=unit_rowest=unit_colest=unit_grpest=rowunit=totunit=tdomdattot <- NULL
  addtotal <- ifelse(((rowvar == "TOTAL" || length(unique(tdomdat[[rowvar]])) > 1) ||
		(!is.null(tdomvarlst) && length(tdomvarlst) > 1)), TRUE, FALSE)
  stratalut <- setDT(stratalut)

  ## Note: tdomdat is the summed response by condition (not domain)
  #if (addtotal) {
    ## Get estimate for total
    tdomdattot <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		    by=c(strunitvars, cuniqueid, "TOTAL"), .SDcols=estvar.name]
    unit_totest <- GBest.pbar(sumyn = estvar.name, 
                              ysum = tdomdattot,
                              esttype = esttype, 
                              uniqueid = cuniqueid, 
                              stratalut = stratalut,
                              unitvar = unitvar, 
                              strvar = strvar, 
                              domain = "TOTAL")
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
  #}

  ## Get row, column, cell estimate and merge area if row or column in cond table 
  if (rowvar != "TOTAL") {
    tdomdatsum <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		    by=c(strunitvars, cuniqueid, rowvar), .SDcols=estvar.name]
    tdomdatsum <- tdomdatsum[!is.na(tdomdatsum[[rowvar]]),]

    unit_rowest <- GBest.pbar(sumyn = estvar.name, 
                              ysum = tdomdatsum,
                              uniqueid = cuniqueid, 
                              stratalut = stratalut,
                              unitvar = unitvar, 
                              strvar = strvar, 
                              domain = rowvar)

    if (colvar != "NONE") {
      tdomdatsum <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		      by=c(strunitvars, cuniqueid, colvar), .SDcols=estvar.name]
      tdomdatsum <- tdomdatsum[!is.na(tdomdatsum[[colvar]]),]
      unit_colest <- GBest.pbar(sumyn = estvar.name, 
                                ysum = tdomdatsum,
                                uniqueid = cuniqueid, 
                                stratalut = stratalut,
                                unitvar = unitvar, 
                                strvar = strvar, 
                                domain = colvar)

      tdomdatsum <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		      by=c(strunitvars, cuniqueid, grpvar), .SDcols=estvar.name]
      unit_grpest <- GBest.pbar(sumyn =estvar.name, 
                                ysum = tdomdatsum,
                                uniqueid = cuniqueid, 
                                stratalut = stratalut,
                                unitvar = unitvar, 
                                strvar = strvar, 
                                domain = grpvar)
    }
  }

  ###################################################################################
  ## Check add0 and Add area
  ###################################################################################
  if (!sumunits && nrow(unitarea) > 1) col.add0 <- TRUE
  if (!is.null(unit_rowest)) {
    unit_rowest <- add0unit(x=unit_rowest, xvar=rowvar, 
                            uniquex=uniquerow, unitvar=unitvar, 
                            xvar.add0=row.add0)
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
    unit_colest <- add0unit(x=unit_colest, xvar=colvar, 
                            uniquex=uniquecol, unitvar=unitvar, 
                            xvar.add0=col.add0)
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
    unit_grpest <- add0unit(x=unit_grpest, xvar=rowvar, 
                            uniquex=uniquerow, unitvar=unitvar, 
                            xvar.add0=row.add0, xvar2=colvar, 
                            uniquex2=uniquecol, xvar2.add0=col.add0)
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
  ## Get row and column totals for units if sumunits=FALSE
  ###################################################################################

  ## For sumunits=FALSE, get estimation unit totals
  if (!sumunits && (length(unique(unitarea[[unitvar]])) > 1 && rowvar != "TOTAL")) {

    ## AGGREGATE UNIT stratalut FOR ROWVAR and GRAND TOTAL
    stratalut2 <- data.table(stratalut, ONEUNIT=1)
    strunitvars2 <- c("ONEUNIT", strvar)
    stratalut2 <- stratalut2[, lapply(.SD, sum, na.rm=TRUE), 
		by=strunitvars2, .SDcols=c(strwtvar, "n.strata")]
    stratalut2[, strwt:=prop.table(get(strwtvar)), by="ONEUNIT"]
    stratalut2[, n.total := sum(n.strata)]
    setkeyv(stratalut2, strunitvars2)

    unitarea2 <- data.table(unitarea, ONEUNIT=1)
    unitarea2 <- unitarea2[, lapply(.SD, sum, na.rm=TRUE), by="ONEUNIT", 
		.SDcols=areavar]
    setkey(unitarea2, "ONEUNIT")

    tdomdat[, ONEUNIT := 1]

    ## Calculate unit totals for rowvar
    tdomdatsum <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars2, tuniqueid, rowvar), .SDcols=estvar.name]
    rowunit <- GBest.pbar(sumyn = estvar.name, 
                          ysum = tdomdatsum, 
                          esttype = esttype,
                          uniqueid = tuniqueid, 
                          stratalut = stratalut2,
                          unitvar = "ONEUNIT", 
                          strvar = strvar, 
                          domain = rowvar)
    rowunit <- add0unit(x=rowunit, xvar=rowvar, 
                        uniquex=uniquerow, unitvar="ONEUNIT", 
                        xvar.add0=row.add0)
    tabs <- check.matchclass(unitarea2, rowunit, "ONEUNIT")
    unitarea2 <- tabs$tab1
    rowunit <- tabs$tab2
    setkeyv(rowunit, "ONEUNIT")
    rowunit <- rowunit[unitarea2, nomatch=0]
    if (totals) {
      rowunit <- getpse(rowunit, areavar=areavar, esttype=esttype)
    } else {
      rowunit <- getpse(rowunit, esttype=esttype)
    }      
    setkeyv(rowunit, c("ONEUNIT", rowvar))

    ## Calculate grand total for all units
    tdomdatsum <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars2, tuniqueid, "TOTAL"), .SDcols=estvar.name]
    totunit <- GBest.pbar(sumyn = estvar.name, 
                          ysum = tdomdatsum, 
                          esttype = esttype,
                          uniqueid = tuniqueid, 
                          stratalut = stratalut2,
                          unitvar = "ONEUNIT", 
                          strvar = strvar, 
                          domain = "TOTAL")
    tabs <- check.matchclass(unitarea2, totunit, "ONEUNIT")
    unitarea2 <- tabs$tab1
    totunit <- tabs$tab2
    setkeyv(totunit, "ONEUNIT")
    totunit <- totunit[unitarea2, nomatch=0]
    if (totals) {
      totunit <- getpse(totunit, areavar=areavar, esttype=esttype)
    } else {
      totunit <- getpse(totunit, esttype=esttype)
    }
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
	      allin1=allin1, savedata=savedata, addtitle=addtitle, title.ref=title.ref, 
	      title.colvar=title.colvar, title.rowvar=title.rowvar, title.rowgrp=title.rowgrp, 
	      title.unitvar=title.unitvar, title.estpse=title.estpse, title.est=title.est, 
	      title.pse=title.pse, rawdata=rawdata, rawonly=rawonly, outfn.estpse=outfn.estpse, 
	      outfolder=outfolder, outfn.date=outfn.date, overwrite=overwrite_layer, 
	      estnm=estnm, estround=estround, pseround=pseround, divideby=divideby, 
	      returntitle=returntitle, estnull=estnull, psenull=psenull, raw.keep0=raw.keep0) 
  est2return <- tabs$tabest
  pse2return <- tabs$tabpse

  if (!is.null(est2return)) {
    returnlst$est <- setDF(est2return)
  }
  if (!is.null(pse2return)) {
    returnlst$pse <- setDF(pse2return) 
  }
  if(returntitle) {
    returnlst$titlelst <- alltitlelst
  }
 
  if (rawdata) {
    rawdat <- tabs$rawdat
    rawdat$domdat <- setDF(tdomdat) 
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
                                           add_layer=TRUE)
          )
        }
      }
    }
    rawdat$module <- "GB"
    rawdat$esttype <- esttype
    rawdat$GBmethod <- ifelse(strata, "PS", "HT")
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
