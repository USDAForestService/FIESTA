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
#' 'VOLCFNET'). If estvar.derive, estvar is the list name.
#' @param estvar.filter String. A tree-level filter for estvar. Must be R
#' syntax (e.g., 'STATUSCD == 1').
#' @param estvar.derive List. A derivation of a tree variable to estimate.
#' Must be a named list with one element (e.g., 
#' list(SDI='SUM(POWER(DIA/10,1.605) * TPA_UNADJ)'). Set estvar = NULL.
#' @param estseed String. Use seedling data only or add to tree data. Seedling
#' estimates are only for counts (estvar='TPA_UNADJ')-('none', 'only', 'add').
#' @param woodland String. If woodland = 'Y', include woodland tree species  
#' where measured. If woodland = 'N', only include timber species. See 
#' FIESTA::ref_species$WOODLAND ='Y/N'. If woodland = 'only', only include
#' woodland species.
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
#' \donttest{
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
#' }
#' @export modGBtree
modGBtree <- function(GBpopdat, 
                      estvar, 
                      estvar.filter = NULL, 
                      estvar.derive = NULL,
                      estseed = "none",
                      woodland = "Y",
                      landarea = "FOREST", 
                      pcfilter = NULL, 
                      rowvar = NULL, 
                      colvar = NULL, 
                      sumunits = TRUE, 
                      returntitle = FALSE, 
                      savedata = FALSE, 
                      table_opts = NULL, 
                      title_opts = NULL, 
                      savedata_opts = NULL, 
                      ...){

  ##################################################################################
  ## DESCRIPTION:
  ## Generates estimates of trees by domain using non-ratio estimators.
  ##################################################################################

  ## CHECK GUI - IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  #if (nargs() == 0 && is.null(GBpopdat)) gui <- TRUE
  gui <- FALSE
 
  ## If gui.. set variables to NULL
  if (gui) { 
    landarea=strvar=areavar=sumunits=adjplot=strata=getwt=cuniqueid=ACI=
      tuniqueid=savedata=addtitle=returntitle=rawdata=rawonly=unitvar <- NULL
    #if (!row.FIAname) row.FIAname <- NULL 
    #if (!col.FIAname) col.FIAname <- NULL 
  }
  
  ## Set parameter
  esttype <- "TREE"
  popType <- "VOL"
  nonresp <- FALSE
  substrvar <- FALSE
  rawdata <- TRUE  
  returnlst <- list()
  TPA <- TRUE

  ## Set global variables
  ONEUNIT=n.total=n.strata=strwt=TOTAL=rawfolder=domclassify <- NULL
  
 
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
  pcheck.params(input.params = input.params,
                title_opts = title_opts, 
                table_opts = table_opts, 
                savedata_opts = savedata_opts)
  
  ## Check parameter option lists
  optslst <- pcheck.opts(optionlst = list(
    title_opts = title_opts,
    table_opts = table_opts,
    savedata_opts = savedata_opts)) 
  title_opts <- optslst$title_opts  
  table_opts <- optslst$table_opts  
  savedata_opts <- optslst$savedata_opts  
  
  for (i in 1:length(title_opts)) {
    assign(names(title_opts)[[i]], title_opts[[i]])
  }
  for (i in 1:length(table_opts)) {
    assign(names(table_opts)[[i]], table_opts[[i]])
  }
  
  ##################################################################
  ## CHECK PARAMETER INPUTS
  ##################################################################
  list.items <- c("pltcondx", "cuniqueid", "condid", 
	                "treex", "tuniqueid", 
                  "unitarea", "unitvar", "stratalut", "strvar",
                  "plotsampcnt", "condsampcnt")
  
  GBpopdat <- pcheck.object(GBpopdat, "GBpopdat", list.items=list.items)
  if (is.null(GBpopdat)) return(NULL)
  pltidsadj <- GBpopdat$pltidsadj
  pltcondx <- GBpopdat$pltcondx
  cuniqueid <- GBpopdat$cuniqueid
  condid <- GBpopdat$condid
  treex <- GBpopdat$treex
  seedx <- GBpopdat$seedx
  if (is.null(treex) && is.null(seedx)) {
    stop("must include tree data for tree estimates")
  }
  tuniqueid <- GBpopdat$tuniqueid
  ACI <- GBpopdat$ACI
  pltassgnx <- GBpopdat$pltassgnx
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
  popdatindb <- GBpopdat$popdatindb
  pop_fmt <- GBpopdat$pop_fmt
  pop_dsn <- GBpopdat$pop_dsn
  pop_schema <- GBpopdat$pop_schema
  popconn <- GBpopdat$popconn
  dbqueries <- GBpopdat$dbqueries
  dbqueriesWITH <- GBpopdat$dbqueriesWITH
  adjcase <- GBpopdat$adjcase
  pltidsid <- GBpopdat$pjoinid
  pltassgnid <- GBpopdat$pltassgnid
  
  #adjfactors <- GBpopdat$adjfactors
  #popVOL_compare <- checkpop(FIADBpop, FIESTApop = adjfactors, evaltype="01")
  #popVOL_compare
  
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
    pltcondxWITHqry <- NULL
    pltcondxadjWITHqry <- NULL
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
                  sumunits = sumunits, 
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
  sumunits <- estdat$sumunits
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
  ## Check parameter inputs and tree filters
  ###################################################################################
  estdatVOL <- 
    check.estdataVOL(esttype = esttype,
                     popdatindb = popdatindb,
                     popconn = popconn,
                     cuniqueid = cuniqueid, condid = condid,
                     treex = treex, seedx = seedx,
                     tuniqueid = tuniqueid,
                     estseed = estseed,
                     woodland = woodland,
                     gui = gui)
  treex <- estdatVOL$treex
  treeflds <- estdatVOL$treeflds
  tuniqueid <- estdatVOL$tuniqueid
  estseed <- estdatVOL$estseed
  woodland <- estdatVOL$woodland
  
  seedx <- estdatVOL$seedx
  seedflds <- estdatVOL$seedflds
  
  
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
                 estseed = estseed,
                 treex = treex, treeflds = treeflds,
                 seedx = seedx, seedflds = seedflds,
                 cuniqueid = cuniqueid, condid = condid,
                 rowvar = rowvar, colvar = colvar, 
                 row.FIAname = row.FIAname, col.FIAname = col.FIAname, 
                 row.orderby = row.orderby, col.orderby = col.orderby,
                 row.classify = row.classify, col.classify = col.classify,
                 row.add0 = row.add0, col.add0 = col.add0, 
                 title.rowvar = title.rowvar, title.colvar = title.colvar, 
                 rowlut = rowlut, collut = collut, 
                 rowgrp = rowgrp, rowgrpnm = rowgrpnm, 
                 rowgrpord = rowgrpord, title.rowgrp = NULL, 
                 gui = gui)
  uniquerow <- rowcolinfo$uniquerow
  uniquecol <- rowcolinfo$uniquecol
  domainlst <- rowcolinfo$domainlst
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
  bytdom <- rowcolinfo$bytdom
  bypcdom <- rowcolinfo$bypcdom
  tdomvar <- rowcolinfo$tdomvar
  tdomvar2 <- rowcolinfo$tdomvar2
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

  ## Generate a uniquecol for estimation units
  if (!sumunits && colvar == "NONE") {
    uniquecol <- data.table(unitarea[[unitvar]])
    setnames(uniquecol, unitvar)
    uniquecol[[unitvar]] <- factor(uniquecol[[unitvar]])
  }

  ###############################################################################
  ### Get estimation data from tree table
  ###############################################################################
  adjtree <- ifelse(adj %in% c("samp", "plot"), TRUE, FALSE)
  treedat <- 
    check.tree(treex = treex, 
               seedx = seedx, 
               estseed = estseed,
               bycond = TRUE, 
               condx = pltcondx, 
               tuniqueid = tuniqueid, cuniqueid = cuniqueid, 
               esttype = esttype, 
               estvarn = estvar, 
               estvarn.filter = estvar.filter, 
               estvarn.derive = estvar.derive,
               esttotn = TRUE, 
               bydomainlst = domainlst,
               tdomvar = tdomvar, tdomvar2 = tdomvar2,
               adjtree = adjtree, 
               adjvar = "tadjfac",
               metric = metric, 
               woodland = woodland,
               ACI = ACI,
               domclassify = domclassify,
               dbconn = popconn, schema = pop_schema,
               pltidsWITHqry = pltcondxadjWITHqry,
               pcwhereqry = pcwhereqry,
               pltidsid = pltidsid,
               bytdom = bytdom,
               gui = gui)
  if (is.null(treedat)) stop() 
  tdomdat <- treedat$tdomdat
  estvar.name <- treedat$estvar.name
  estvar.filter <- treedat$estvar.filter
  tdomvarlst <- treedat$tdomvarlst
  estunits <- treedat$estunits
  treeqry <- treedat$treeqry
  classifynmlst <- treedat$classifynmlst
  
  if (sum(tdomdat[[estvar.name]], na.rm=TRUE) == 0) {
    stop("invalid estimate... no tree data returned...")
  }
  
  ## If classified rowvar or colvar, get class names
  if (!is.null(classifynmlst)) {
    if (!is.null(classifynmlst[[rowvar]])) {
      rowvar <- classifynmlst[[rowvar]]
    }
    if (!is.null(classifynmlst[[colvar]])) {
      colvar <- classifynmlst[[colvar]]
    }
    if (!is.null(grpvar)) {
      grpvar <- c(rowvar, colvar)
    }
  }


  ###############################################################################
  ### Get titles for output tables
  ###############################################################################
  alltitlelst <- 
    check.titles(dat = tdomdat, 
                 esttype = esttype, 
                 estseed = estseed, 
                 woodland = woodland, 
	               sumunits = sumunits, 
                 title.main = title.main, 
                 title.ref = title.ref, 
                 title.rowvar = title.rowvar, 
                 title.rowgrp = title.rowgrp, 
	               title.colvar = title.colvar, 
                 title.unitvar = title.unitvar, 
	               title.filter = title.filter, 
                 title.unitsn = estunits,
                 title.estvarn = title.estvar, 
	               unitvar = unitvar, 
                 rowvar = rowvar, colvar = colvar, 
 	               estvarn = estvar.name, 
                 estvarn.filter = estvar.filter, 
	               addtitle = addtitle, 
                 returntitle = returntitle, 
	               rawdata = rawdata, 
                 states = states, invyrs = invyrs,
	               landarea = landarea, 
                 pcfilter = pcfilter, 
                 allin1 = allin1, 
	               divideby = divideby, 
                 outfn.pre = outfn.pre)
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

  
  ###################################################################################
  ## GENERATE ESTIMATES
  ###################################################################################
  estdat <- 
    getGBestimates(esttype = esttype,
                   domdatn = tdomdat,
                   uniqueid = pltassgnid,
                   estvarn.name = estvar.name,
                   rowvar = rowvar, colvar = colvar, 
                   grpvar = grpvar,
                   pltassgnx = pltassgnx,
                   unitarea = unitarea,
                   unitvar = unitvar,
                   areavar = areavar,
                   stratalut = stratalut,
                   strvar = strvar,
                   strwtvar = strwtvar,
                   totals = totals,
                   sumunits = sumunits,
                   uniquerow = uniquerow,
                   uniquecol = uniquecol,
                   row.orderby = row.orderby,
                   col.orderby = col.orderby,
                   row.add0 = row.add0,
                   col.add0 = col.add0,
                   row.NAname = row.NAname,
                   col.NAname = col.NAname)
  if (is.null(estdat)) stop()
  unit_totest <- estdat$unit_totest
  unit_rowest <- estdat$unit_rowest
  unit_colest <- estdat$unit_colest
  unit_grpest <- estdat$unit_grpest
  rowunit <- estdat$rowunit
  totunit <- estdat$totunit
  unitvar <- estdat$unitvar

  
  ###################################################################################
  ## GENERATE OUTPUT TABLES
  ###################################################################################
  message("getting output...")
  estnm <- "est" 
  tabs <- 
    est.outtabs(esttype = esttype, 
                sumunits = sumunits, areavar = areavar, 
                unitvar = unitvar, unitvars = unitvars, 
                unit_totest = unit_totest, 
                unit_rowest = unit_rowest, unit_colest = unit_colest, 
                unit_grpest = unit_grpest,
                rowvar = rowvarnm, colvar = colvarnm, 
                uniquerow = uniquerow, uniquecol = uniquecol,
                rowgrp = rowgrp, rowgrpnm = rowgrpnm, 
                rowunit = rowunit, totunit = totunit, 
                allin1 = allin1, 
                savedata = savedata, addtitle = addtitle, 
                title.ref = title.ref, 
                title.rowvar = title.rowvar, title.colvar = title.colvar, 
                title.rowgrp = title.rowgrp,
                title.unitvar = title.unitvar, title.estpse = title.estpse, 
                title.est = title.est, title.pse = title.pse, 
                rawdata = rawdata, rawonly = rawonly, 
                outfn.estpse = outfn.estpse, 
                outfolder = outfolder, outfn.date = outfn.date, 
                overwrite = overwrite_layer, estnm = estnm, 
                estround = estround, pseround = pseround, 
                divideby = divideby, 
                returntitle = returntitle, 
                estnull = estnull, psenull = psenull, 
                raw.keep0 = raw.keep0) 
  
  est2return <- tabs$tabest
  pse2return <- tabs$tabpse

  if (!row.add0 && any(est2return$Total == "--")) {
    est2return <- est2return[est2return$Total != "--",]
  }
  if (!is.null(est2return)) {
    if (!row.add0 && any(est2return$Total == "--")) {
      est2return <- est2return[est2return$Total != "--",]
    }
    returnlst$est <- setDF(est2return)
  }
  if (!is.null(pse2return)) {
    if (!row.add0 && any(pse2return$Total == "--")) {
      pse2return <- pse2return[pse2return$Total != "--",]
    }
    returnlst$pse <- setDF(pse2return) 
  }
  if(returntitle) {
    returnlst$titlelst <- alltitlelst
  }

  if (rawdata) {
    ## Add total number of plots in population to unit_totest and totest (if sumunits=TRUE)
    UNITStot <- sort(unique(unit_totest[[unitvar]]))
    NBRPLTtot <- stratalut[stratalut[[unitvar]] %in% UNITStot, list(NBRPLT = sum(n.strata, na.rm=TRUE)), 
	                 by=unitvars]

	#setnames(NBRPLTtot, "V1", "NBRPLT")
	if ("unit_totest" %in% names(tabs$rawdat)) {
	  tabs$rawdat$unit_totest <- merge(tabs$rawdat$unit_totest, NBRPLTtot, by=unitvars)
	}
	if (sumunits && "totest" %in% names(tabs$rawdat)) {
	  tabs$rawdat$totest <- data.frame(tabs$rawdat$totest, NBRPLT = sum(NBRPLTtot$NBRPLT))
	}

    rawdat <- tabs$rawdat
    rawdat$domdat <- setDF(tdomdat)
    rawdat$domdatqry <- treeqry
    rawdat$estvar <- estvar.name
    rawdat$estvar.filter <- estvar.filter
    if (!is.null(rawdat$estvar.derive)) rawdat$estvar.derive <- estvar.derive
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
                savedata_opts=list(outfolder = rawfolder, 
                                   out_fmt = raw_fmt, 
                                   out_dsn = raw_dsn, 
                                   out_layer = out_layer,
                                   overwrite_layer = overwrite_layer,
                                   append_layer = append_layer,
                                   add_layer = TRUE)
          )
        }
      }
    }
    rawdat$module <- "GB"
    rawdat$esttype <- esttype
    rawdat$GBmethod <- ifelse(strata, "PS", "HT")
    rawdat$estvar <- estvar.name
    rawdat$estvar.filter <- estvar.filter
    if (!is.null(rowvar)) rawdat$rowvar <- rowvar
    if (!is.null(colvar)) rawdat$colvar <- colvar
    rawdat$areaunits <- areaunits
    rawdat$estunits <- estunits
    returnlst$raw <- rawdat
  }
  returnlst$statecd <- sort(pcheck.states(states, statereturn = "VALUE"))
  returnlst$states <- states
  returnlst$invyr <- sort(unique(unlist(invyrs)))
  
  
  return(returnlst)
}
