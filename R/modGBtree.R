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
#' For available reference tables: sort(unique(FIESTA::ref_codes$VARIABLE)) \cr
#' 
#' @param GBpopdat List. Population data objects returned from
#' FIESTA::modGBpop().
#' @param estseed String. Use seedling data only or add to tree data. Seedling
#' estimates are only for counts (estvar='TPA_UNADJ')-('none', 'only', 'add').
#' @param landarea String. The condition-level filter for defining land area
#' ('ALL', 'FOREST', 'TIMBERLAND'). If landarea='FOREST', COND_STATUS_CD = 1;
#' if landarea='TIMBERLAND', SITECLCD in(1:6) & RESERVCD = 0.
#' @param pcfilter String. A filter for plot or cond attributes (including
#' pltassgn).  Must be R logical syntax.
#' @param estvar String. Name of the tree-level estimate variable (e.g.,
#' 'VOLCFNET').
#' @param estvar.filter String. A tree-level filter for estvar. Must be R
#' syntax (e.g., 'STATUSCD == 1').
#' @param rowvar String. Optional. Name of domain variable to group estvar by
#' for rows in table output. Rowvar must be included in an input data frame
#' (i.e., plt, cond, tree). If no rowvar is included, an estimate is returned
#' for the total estimation unit. Include colvar for grouping by 2 variables.
#' @param colvar String. Optional. If rowvar != NULL, name of domain variable
#' to group estvar by for columns in table output. Colvar must be included in
#' an input data frame (i.e., plt, cond, tree).
#' @param row.FIAname Logical. If TRUE, retrieves default FIA reference names
#' for rowvar located in FIESTA::ref_codes data frame. Names are only available
#' for certain variables (Check sort(unique(FIESTA::ref_codes$VARIABLE)) for
#' available names.  If row.FIAname = TRUE and rowvar is in FIESTA::ref_codes,
#' the rowvar name is used for the output table, and the rowvar code is used to
#' sort.
#' @param col.FIAname Logical. If TRUE, retrieves default FIA reference names
#' for colvar located in FIESTA::ref_codes data frame. Names are only available
#' for certain variables. Check: sort(unique(FIESTA::ref_codes$VARIABLE)) for
#' available names.  If col.FIAname = TRUE and rowvar is in FIESTA::ref_codes,
#' the colvar name is used for the output table, and the colvar code is used to
#' sort.
#' @param row.orderby String. Optional. Name of variable to sort table rows.
#' Both the rowvar and row.orderby variables must be included in the same input
#' data.frame.  if NULL, and row.FIAname=FALSE or rowvar is not in
#' FIESTA::ref_codes, the rows are ordered by rowvar.
#' @param col.orderby String. Optional. Name of variable to sort table columns.
#' Both the colvar and col.orderby variables must be included in the same input
#' data.frame.  if NULL, and col.FIAname=FALSE or colvar is not in
#' FIESTA::ref_codes, the columns are ordered by colvar.
#' @param row.add0 Logical. If TRUE, include rows with 0 values to the output
#' table.
#' @param col.add0 Logical. If TRUE, include columns with 0 values to the
#' output table.
#' @param rowlut Data frame. A lookup table with variable codes and code names
#' to include as rows of output table (See notes for more information and
#' format).
#' @param collut Data frame. A lookup table with variable codes and code names
#' to include as columns of output table (See notes for more information and
#' format).
#' @param rowgrp Logical. If TRUE, appends row groups to first column of table.
#' Only available if group category exists in ref_codes table or defined in
#' rowgrpnm (e.g., FORTYPGRPCD, OWNGRPCD).
#' @param rowgrpnm String. Name of variable for grouping rowvar. Variable must
#' be included in same input table as rowvar.
#' @param rowgrpord String. Name of variable to sort row group variable.
#' Variable must be included in same input table as rowgrpnm.
#' @param sumunits Logical. If TRUE, estimation units are summed and returned
#' in one table.
#' @param allin1 Logical. If TRUE, both estimates and percent sample error are
#' output in one table as: estimates (percent sample error).
#' @param metric Logical. If TRUE, output area is in metric units (hectares).
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
#' @param outfolder String. The outfolder to write files to. If NULL, files are
#' written to working directory, or if gui, a window to browse.
#' @param outfn.pre String. If savedata=TRUE, prefix for output files. If
#' rawdata=TRUE, prefix for rawdata files (if raw_fmt = 'csv') or raw_dsn (if
#' raw_fmt != 'csv').
#' @param outfn.date Logical. If TRUE, add current date to out_dsn.
#' @param addtitle Logical. If TRUE and savedata=TRUE, adds title to outfile.
#' @param rawdata Logical. If TRUE, returns a list of raw data tables that are
#' used for estimation (See Value). If savedata = TRUE, tables are written to
#' outfolder (if raw_fmt='csv') or raw_dsn (if raw_fmt != 'csv').
#' @param rawonly Logical. If TRUE, only rawdata are output. If dataset
#' includes many estimation units, and only raw data tables are desired, it is
#' more efficient to output raw data only.
#' @param raw_fmt String. Format for output rawdata tables ('sqlite',
#' 'sqlite3', 'db', 'db3', 'gpkg', 'csv', 'gdb', 'shp').
#' @param raw_dsn String. Data source name for rawdata output. If extension is
#' not included, out_fmt is used. Use full path if outfolder=NULL.
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
#' @param title.rowvar String. TITLE, if savedata=TRUE and/or returntitle=TRUE:
#' pretty name for the row domain variable. If NULL, = rowvar.
#' @param title.colvar String. TITLE, if savedata=TRUE and/or returntitle=TRUE:
#' pretty name for the column domain variable. If NULL, = colvar.
#' @param title.unitvar String. TITLE, if savedata=TRUE and/or
#' returntitle=TRUE: pretty name for the estimation unit variable. If NULL, =
#' unitvar.
#' @param title.estvar String. TITLE: if savedata=TRUE and/or returntitle=TRUE:
#' pretty name for the estimate variable. If NULL, title.estvar = estvar.name.
#' @param title.filter String. TITLE, if savedata=TRUE and/or returntitle=TRUE:
#' pretty name for filter(s). If title.filter=NULL, a default is generated from
#' cfilter.  If title.filter="", no title.filter is used.
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
#' 
#' 
#'   ## Rows only; combine estimation units (sumunits=TRUE)
#'   MODest <- modGBtree(tree=WYtree, cond=WYcond, pltassgn=WYpltassgn, pltassgnid="CN",
#' 	unitarea=WYunitarea, unitvar="ESTN_UNIT", stratalut=WYstrlut, sumunits=TRUE,
#' 	landarea="FOREST", estvar="VOLCFNET", estvar.filter="STATUSCD==1",
#' 	rowvar="FORTYPCD", row.FIAname=TRUE)
#'   names(MODest)
#'   MODest$est
#' 
#'   ## Rows only; by estimation units (allin1=TRUE)
#'   MODest <- modGBtree(tree=WYtree, cond=WYcond, pltassgn=WYpltassgn, pltassgnid="CN",
#' 	unitarea=WYunitarea, unitvar="ESTN_UNIT", stratalut=WYstrlut, sumunits=TRUE,
#' 	landarea="FOREST", estvar="VOLCFNET", estvar.filter="STATUSCD==1",
#' 	rowvar="FORTYPCD", row.FIAname=TRUE, allin1=TRUE)
#'   names(MODest)
#'   MODest$est
#' 
#' 
#' @export modGBtree
modGBtree <- function(GBpopdat=NULL, estseed="none", landarea="FOREST", 
	pcfilter=NULL, estvar=NULL, estvar.filter=NULL, rowvar=NULL, colvar=NULL, 
	row.FIAname=FALSE, col.FIAname=FALSE, row.orderby=NULL, col.orderby=NULL, 
	row.add0=FALSE, col.add0=FALSE, rowlut=NULL, collut=NULL, 
	rowgrp=FALSE, rowgrpnm=NULL, rowgrpord=NULL, sumunits=TRUE, allin1=FALSE, 
	metric=FALSE, estround=1, pseround=2, estnull="--", psenull="--", divideby=NULL, 
	savedata=FALSE, outfolder=NULL, outfn.pre=NULL, outfn.date=FALSE, addtitle=TRUE,
 	rawdata=FALSE, rawonly=FALSE, raw_fmt="csv", raw_dsn=NULL, overwrite_dsn=FALSE,
 	overwrite_layer=TRUE, append_layer=FALSE, returntitle=FALSE, title.main=NULL,
 	title.ref=NULL, title.rowvar=NULL, title.colvar=NULL, title.unitvar=NULL,
 	title.estvar=NULL, title.filter=NULL, gui=FALSE, ...){

  ##################################################################################
  ## DESCRIPTION:
  ## Generates estimates of trees by domain using non-ratio estimators.
  ##################################################################################

  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- c(names(formals(modGBtree)),
		names(formals(FIESTA::modGBpop))) 
  if (!all(input.params %in% formallst)) {
    miss <- input.params[!input.params %in% formallst]
    stop("invalid parameter: ", toString(miss))
  }

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

  ## Set global variables
  ONEUNIT=n.total=n.strata=strwt=TOTAL=rowvar.filter=colvar.filter <- NULL

  ###################################################################################
  ## INITIALIZE SETTINGS
  ###################################################################################
  options.old <- options()
  options(scipen=8) # bias against scientific notation
  on.exit(options(options.old), add=TRUE)
  esttype <- "TREE"
  returnGBpopdat <- TRUE 
  parameters <- FALSE
  returnlst <- list()


  ###################################################################################
  ## Check data and generate population information 
  ###################################################################################
  if (is.null(GBpopdat)) {
    GBpopdat <- modGBpop(gui=gui, ...)
  } else {
    returnGBpopdat <- FALSE
    list.items <- c("condx", "pltcondx", "treex", "cuniqueid", "condid", 
		"tuniqueid", "ACI.filter", "unitarea", "unitvar", "stratalut", "strvar",
		"plotsampcnt", "condsampcnt")
    GBpopdat <- FIESTA::pcheck.object(GBpopdat, "GBpopdat", list.items=list.items)
  }		
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
 

  ########################################
  ## Check area units
  ########################################
  unitchk <- pcheck.areaunits(unitarea=unitarea, areavar=areavar, 
			areaunits=areaunits, metric=metric)
  unitarea <- unitchk$unitarea
  areavar <- unitchk$areavar
  areaunits <- unitchk$outunits


  ###################################################################################
  ## Check parameters and apply plot and condition filters
  ###################################################################################
  estdat <- check.estdata(esttype=esttype, pltcondf=pltcondx, cuniqueid=cuniqueid,
 	condid=condid, treex=treex, seedx=seedx, estseed=estseed, sumunits=sumunits,
 	landarea=landarea, ACI.filter=ACI.filter, pcfilter=pcfilter, 
	allin1=allin1, estround=estround, pseround=pseround, divideby=divideby,
 	addtitle=addtitle, returntitle=returntitle, rawdata=rawdata, rawonly=rawonly,
 	savedata=savedata, outfolder=outfolder, overwrite_dsn=overwrite_dsn,
 	overwrite_layer=overwrite_layer, outfn.pre=outfn.pre, outfn.date=outfn.date,
 	append_layer=append_layer, raw_fmt=raw_fmt, raw_dsn=raw_dsn, gui=gui)
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
	condf=pltcondf, cuniqueid=cuniqueid, tuniqueid=tuniqueid, estseed=estseed,
	rowvar=rowvar, rowvar.filter=rowvar.filter, colvar=colvar, 
	colvar.filter=colvar.filter, row.FIAname=row.FIAname, col.FIAname=col.FIAname,
 	row.orderby=row.orderby, col.orderby=col.orderby, row.add0=row.add0, 
	col.add0=col.add0, title.rowvar=title.rowvar, title.colvar=title.colvar, 
	rowlut=rowlut, collut=collut, rowgrp=rowgrp, rowgrpnm=rowgrpnm, 
	rowgrpord=rowgrpord, landarea=landarea)
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
	bycond=TRUE, condf=condf, bytdom=bytdom, tuniqueid=tuniqueid, 
	cuniqueid=cuniqueid, esttype=esttype, estvarn=estvar, 
	estvarn.filter=estvar.filter, esttotn=TRUE, tdomvar=tdomvar, 
	tdomvar2=tdomvar2, adjtree=adjtree, metric=metric)
  if (is.null(treedat)) return(NULL) 

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
	title.rowvar=title.rowvar, title.rowgrp=title.rowgrp, title.colvar=title.colvar,
 	title.unitvar=title.unitvar, title.filter=title.filter, title.unitsn=estunits, 
	title.estvarn=title.estvar, unitvar=unitvar, rowvar=rowvar, colvar=colvar, 
 	estvarn=estvar, estvarn.filter=estvar.filter, addtitle=addtitle,
 	returntitle=returntitle, rawdata=rawdata, states=states, invyrs=invyrs,
 	landarea=landarea, pcfilter=pcfilter, allin1=allin1, divideby=divideby,
 	outfn.pre=outfn.pre)
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
    unit_totest <- GBest.pbar(sumyn=estvar.name, ysum=tdomdattot, 
		esttype=esttype, uniqueid=cuniqueid, stratalut=stratalut, unitvar=unitvar, 
		strvar=strvar, domain="TOTAL")
    tabs <- FIESTA::check.matchclass(unitarea, unit_totest, unitvar)
    unitarea <- tabs$tab1
    unit_totest <- tabs$tab2
    setkeyv(unit_totest, unitvar)
    unit_totest <- unit_totest[unitarea, nomatch=0]
    unit_totest <- getarea(unit_totest, areavar=areavar, esttype=esttype)
  #}

  ## Get row, column, cell estimate and merge area if row or column in cond table 
  if (rowvar != "TOTAL") {
    tdomdatsum <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars, cuniqueid, rowvar), .SDcols=estvar.name]
    tdomdatsum <- tdomdatsum[!is.na(tdomdatsum[[rowvar]]),]
    unit_rowest <- GBest.pbar(sumyn=estvar.name, ysum=tdomdatsum, 
		uniqueid=cuniqueid, stratalut=stratalut, unitvar=unitvar, strvar=strvar, 
		domain=rowvar)
    if (colvar != "NONE") {
      tdomdatsum <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars, cuniqueid, colvar), .SDcols=estvar.name]
      tdomdatsum <- tdomdatsum[!is.na(tdomdatsum[[colvar]]),]
      unit_colest <- GBest.pbar(sumyn=estvar.name, ysum=tdomdatsum, 
		uniqueid=cuniqueid, stratalut=stratalut, unitvar=unitvar, strvar=strvar, 
		domain=colvar)

      tdomdatsum <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars, cuniqueid, grpvar), .SDcols=estvar.name]
      unit_grpest <- GBest.pbar(sumyn=estvar.name, ysum=tdomdatsum, 
		uniqueid=cuniqueid, stratalut=stratalut, unitvar=unitvar, strvar=strvar, 
		domain=grpvar)
    }
  }

  ###################################################################################
  ## Check add0 and Add area
  ###################################################################################
  if (!sumunits && nrow(unitarea) > 1) col.add0 <- TRUE
  if (!is.null(unit_rowest)) {
    unit_rowest <- FIESTA::add0unit(x=unit_rowest, xvar=rowvar, uniquex=uniquerow, 
		unitvar=unitvar, xvar.add0=row.add0)
    tabs <- FIESTA::check.matchclass(unitarea, unit_rowest, unitvar)
    unitarea <- tabs$tab1
    unit_rowest <- tabs$tab2
    setkeyv(unit_rowest, unitvar)
    unit_rowest <- unit_rowest[unitarea, nomatch=0]
    unit_rowest <- FIESTA::getarea(unit_rowest, areavar=areavar, esttype=esttype)
    setkeyv(unit_rowest, c(unitvar, rowvar))
  }

  if (!is.null(unit_colest)) {
    unit_colest <- FIESTA::add0unit(x=unit_colest, xvar=colvar, uniquex=uniquecol, 
		unitvar=unitvar, xvar.add0=col.add0)
    tabs <- FIESTA::check.matchclass(unitarea, unit_colest, unitvar)
    unitarea <- tabs$tab1
    unit_colest <- tabs$tab2
    setkeyv(unit_colest, unitvar)
    unit_colest <- unit_colest[unitarea, nomatch=0]
    unit_colest <- FIESTA::getarea(unit_colest, areavar=areavar, esttype=esttype)
    setkeyv(unit_colest, c(unitvar, colvar))
  }

  if (!is.null(unit_grpest)) {
    unit_grpest <- add0unit(x=unit_grpest, xvar=rowvar, uniquex=uniquerow, 
		unitvar=unitvar, xvar.add0=row.add0, xvar2=colvar, uniquex2=uniquecol,
		xvar2.add0=col.add0)
    tabs <- FIESTA::check.matchclass(unitarea, unit_grpest, unitvar)
    unitarea <- tabs$tab1
    unit_grpest <- tabs$tab2
    setkeyv(unit_grpest, unitvar)
    unit_grpest <- unit_grpest[unitarea, nomatch=0]
    unit_grpest <- FIESTA::getarea(unit_grpest, areavar=areavar, esttype=esttype)
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

    unitacres2 <- data.table(unitarea, ONEUNIT=1)
    unitacres2 <- unitacres2[, lapply(.SD, sum, na.rm=TRUE), by="ONEUNIT", 
		.SDcols=areavar]
    setkey(unitacres2, "ONEUNIT")

    tdomdat[, ONEUNIT := 1]

    ## Calculate unit totals for rowvar
    tdomdatsum <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars2, tuniqueid, rowvar), .SDcols=estvar.name]
    rowunit <- GBest.pbar(sumyn=estvar.name, ysum=tdomdatsum, esttype=esttype, 
			uniqueid=tuniqueid, stratalut=stratalut2, 
			unitvar="ONEUNIT", strvar=strvar, domain=rowvar)

    rowunit <- add0unit(x=rowunit, xvar=rowvar, uniquex=uniquerow, 
		unitvar="ONEUNIT", xvar.add0=row.add0)
    tabs <- FIESTA::check.matchclass(unitacres2, rowunit, "ONEUNIT")
    unitacres2 <- tabs$tab1
    rowunit <- tabs$tab2
    setkeyv(rowunit, "ONEUNIT")
    rowunit <- rowunit[unitacres2, nomatch=0]
    rowunit <- FIESTA::getarea(rowunit, areavar=areavar, esttype=esttype)
    setkeyv(rowunit, c("ONEUNIT", rowvar))

    ## Calculate grand total for all units
    tdomdatsum <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars2, tuniqueid, "TOTAL"), .SDcols=estvar.name]
    totunit <- GBest.pbar(sumyn=estvar.name, ysum=tdomdatsum, esttype=esttype, 
			uniqueid=tuniqueid, stratalut=stratalut2, 
			unitvar="ONEUNIT", strvar=strvar, domain="TOTAL")
    tabs <- FIESTA::check.matchclass(unitacres2, totunit, "ONEUNIT")
    unitacres2 <- tabs$tab1
    totunit <- tabs$tab2
    setkeyv(totunit, "ONEUNIT")
    totunit <- totunit[unitacres2, nomatch=0]
    totunit <- FIESTA::getarea(totunit, areavar=areavar, esttype=esttype)
  }          

  ###################################################################################
  ## GENERATE OUTPUT TABLES
  ###################################################################################
  message("getting output...")
  estnm <- "est"
  tabs <- est.outtabs(esttype=esttype, sumunits=sumunits, areavar=areavar, 
	unitvar=unitvar, unitvars=unitvars, unit_totest=unit_totest, unit_rowest=unit_rowest, 
	unit_colest=unit_colest, unit_grpest=unit_grpest, rowvar=rowvar, colvar=colvar, 
	uniquerow=uniquerow, uniquecol=uniquecol, rowgrp=rowgrp, rowgrpnm=rowgrpnm, 
	rowunit=rowunit, totunit=totunit, allin1=allin1, savedata=savedata, 
	addtitle=addtitle, title.ref=title.ref, title.colvar=title.colvar, 
	title.rowvar=title.rowvar, title.rowgrp=title.rowgrp, title.unitvar=title.unitvar,
 	title.estpse=title.estpse, title.est=title.est, title.pse=title.pse, 
	rawdata=rawdata, rawonly=rawonly, outfn.estpse=outfn.estpse, outfolder=outfolder, 
	outfn.date=outfn.date, overwrite=overwrite_layer, estnm=estnm, estround=estround,
 	pseround=pseround, divideby=divideby, returntitle=returntitle, 
	estnull=estnull, psenull=psenull) 
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
          datExportData(rawtab, out_fmt=raw_fmt, outfolder=rawfolder, 
 			out_dsn=raw_dsn, out_layer=out_layer, 
			overwrite_layer=overwrite_layer, add_layer=TRUE, 
			append_layer=append_layer)
        }
      }
    }
    rawdat$esttype <- "TREE"
    rawdat$estvar <- estvar
    rawdat$estvar.filter <- estvar.filter
    if (!is.null(rowvar)) rawdat$rowvar <- rowvar
    if (!is.null(colvar)) rawdat$colvar <- colvar
    rawdat$areaunits <- areaunits
    rawdat$estunits <- estunits
    returnlst$raw <- rawdat
  }
  if (returnGBpopdat) {
    returnlst$GBpopdat <- GBpopdat
  }
  if ("STATECD" %in% names(pltcondf)) {
    returnlst$statecd <- sort(unique(pltcondf$STATECD))
  }
  if ("INVYR" %in% names(pltcondf)) {
    returnlst$invyr <- sort(unique(pltcondf$INVYR))
  }

  return(returnlst)
}
