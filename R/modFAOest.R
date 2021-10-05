#' FAO module - Generate FAO estimates.
#' 
#' Generates per-acre and per-tree estimates by domain.
#' 
#' If variable = NULL, then it will prompt user for input.
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
#' @param FAOpopdat List. Population data objects returned from modFAOpop().
#' @param estseed String. Use seedling data only or add to tree data. Seedling
#' estimates are only for counts (estvar='TPA_UNADJ')-('none', 'only', 'add').
#' @param esttype String. Estimation type ('AREA', 'TREE', 'RATIO').
#' @param ratiotype String. The type of ratio estimates ("PERACRE", "PERTREE").
#' @param bcfilter String. A filter for base or cluster attributes. Must be R
#' syntax.
#' @param estvarn String. Name of the tree estimate variable (numerator).
#' @param estvarn.filter String. A tree filter for the estimate variable
#' (numerator).  Must be R syntax (e.g., "STATUSCD == 1").
#' @param estvard String. Name of the tree estimate variable (denominator).
#' @param estvard.filter String. A tree filter for the estimate variable
#' (denominator).  Must be R syntax (e.g., "STATUSCD == 1").
#' @param TPA Logical. If TRUE, multiply by trees per acre.
#' @param rowvar String. Name of the row domain variable in cond or tree. If
#' only one domain, rowvar = domain variable. If more than one domain, include
#' colvar. If no domain, rowvar = NULL.
#' @param colvar String. Name of the column domain variable in cond or tree.
#' @param row.FIAname Logical. If TRUE, gets FIA reference names for row
#' variable based on ref_codes. Only available for certain variables.
#' @param col.FIAname Logical. If TRUE, gets FIA reference names for column
#' variable based on ref_codes. Only available for certain variables.
#' @param row.orderby String. Name of variable to sort table rows. If
#' row.FIAname=TRUE and a ref_* exists for rowvar, the rowvar code is used to
#' sort. If NULL, the table is sorted by rowvar.
#' @param col.orderby String. Name of variable to sort table columns. If
#' col.FIAname=TRUE and a ref_* exists for colvar, the colvar code is used to
#' sort. If NULL, the table is sorted by colvar.
#' @param row.add0 Logical. If TRUE, add the rows that have 0 values.
#' @param col.add0 Logical. If TRUE, add the columns that have 0 values.
#' @param rowlut Data frame. A lookup table with variable codes and
#' descriptions to include in rows of output table (See notes for more
#' information and format).
#' @param collut Data frame. A lookup table with variable codes and
#' descriptions to include in columns of output table (See notes for more
#' information and format).
#' @param rowgrp Logical. If TRUE, appends row groups to first column of table.
#' Only available if group category exists in ref_codes table (e.g.,
#' FORTYPGRPCD, OWNGRPCD).
#' @param rowgrpnm String. Name of row group variable.
#' @param rowgrpord String. Name of row group variable to sort table rows.
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
#' the ending text of the table title (i.e. Nevada, 2004-2005). If NULL, = "".
#' @param title.rowvar String. TITLE, if savedata=TRUE and/or returntitle=TRUE:
#' pretty name for the row domain variable. If NULL, = rowvar.
#' @param title.colvar String. TITLE, if savedata=TRUE and/or returntitle=TRUE:
#' pretty name for the column domain variable. If NULL, = colvar.
#' @param title.unitvar String. TITLE, if savedata=TRUE and/or
#' returntitle=TRUE: pretty name for the estimation unit variable. If NULL, =
#' unitvar.
#' @param title.estvarn String. TITLE: if savedata=TRUE and/or
#' returntitle=TRUE: pretty name for the numerator estimate variable. If NULL,
#' title.estvar = estvarn.name.
#' @param title.estvard String. TITLE: if savedata=TRUE and/or
#' returntitle=TRUE: pretty name for the denominator estimate variable. If
#' NULL, title.estvar = estvard.name.
#' @param title.filter String. TITLE, if savedata=TRUE and/or returntitle=TRUE:
#' pretty name for filter(s). If title.filter=NULL, a default is generated from
#' cfilter.  If title.filter="", no title.filter is used.
#' @param gui Logical. If gui, user is prompted for parameters.
#' @param ...  Parameters for modGBpop() if FAOpopdat is NULL.
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
#' \item{processing data}{ Data frames. Separate data frames of variables used
#' in estimation process for the rowvar, colvar and combination of rowvar and
#' colvar (if colvar is not NULL), and grand total by estimation unit
#' (unit.rowest, unit.colest, unit.grpest, unit.totest, respectively) and
#' summed estimation units, if sumunits=TRUE (roweset, colest, grpest, totest,
#' respectively).
#' 
#' The data frames include the following information: \tabular{lll}{ \tab
#' \bold{Variable} \tab \bold{Description}\cr \tab nhat \tab estimated
#' proportion of trees for numerator \cr \tab nhat.var \tab variance estimate
#' of estimated proportion of trees for numerator \cr \tab dhat \tab estimated
#' proportion of trees for denominator \cr \tab dhat.var \tab variance estimate
#' of estimated proportion of trees for denominator \cr \tab covar \tab
#' covariance for ratio \cr \tab NBRPLT.gt0 \tab Number of non-zero plots used
#' in estimates \cr \tab ACRES \tab total area for estimation unit \cr \tab
#' estn \tab estimated area of land covered by condition, for numerator
#' nhat*areavar \cr \tab estn.var \tab variance of estimated area, for
#' numerator nhat.var*areavar \cr \tab estd \tab estimated area of land covered
#' by condition, for denominator dhat*areavar \cr \tab estd.var \tab variance
#' of estimated area, for denominator dhat.var*areavar \cr \tab estd.covar \tab
#' estimated covariance of numerator and denominator in area \cr \tab rhat \tab
#' estimated proportion \cr \tab rhat.var \tab variance estimate proportion \cr
#' \tab rhat.se \tab estimated standard error proportion \cr \tab rhat.pse \tab
#' estimated percent standard error \cr \tab CI99left \tab left tail of 99
#' percent confidence interval for estimated area \cr \tab CI99right \tab right
#' tail of 99 percent confidence interval for estimated area \cr \tab CI95left
#' \tab left tail of 95 percent confidence interval for estimated area \cr \tab
#' CI95right \tab right tail of 95 percent confidence interval for estimated
#' area \cr \tab CI67left \tab left tail of 67 percent confidence interval for
#' estimated area \cr \tab CI67right \tab right tail of 67 percent confidence
#' interval for estimated area \cr } }
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
#' STRATA:\cr Stratification is used to reduce variance in population estimates
#' by partitioning the population into homogenous classes (strata), such as
#' forest and nonforest. For stratified sampling methods, the strata sizes
#' (weights) must be either known or estimated. Remotely-sensed data is often
#' used to generate strata weights with proporation of pixels by strata. If
#' stratification is desired (strata=TRUE), the required data include: stratum
#' assignment for the center location of each plot, stored in either pltassgn
#' or cond; and a look-up table with the area or proportion of the total area
#' of each strata value by estimation unit, making sure the name of the strata
#' (and estimation unit) variable and values match the plot assignment name(s)
#' and value(s).
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
#' UNITS:\cr The following variables are converted from pounds (in NIMS) to
#' short tons by multiplying the variable by 0.0005.  DRYBIO_AG, DRYBIO_BG,
#' DRYBIO_WDLD_SPP, DRYBIO_SAPLING, DRYBIO_STUMP, DRYBIO_TOP, DRYBIO_BOLE,
#' DRYBIOT, DRYBIOM, DRYBIOTB, JBIOTOT, CARBON_BG, CARBON_AG
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
#'   \dontrun{
#' 
#'   ## Rows only; combine estimation units (sumunits=TRUE)
#'   modGBratio(tree=WYtree, cond=WYcond, pltassgn=WYpltassgn, sumunits=TRUE,
#'   	landarea="FOREST", unitarea=WYunitarea, unitvar="ESTN_UNIT", stratalut=WYstrlut,
#'   	estvarn="VOLCFNET", estvarn.filter="STATUSCD==1", rowvar="FORTYPCD", row.FIAname=TRUE)
#' 
#' 
#'   ## Rows only; by estimation units (allin1=TRUE)
#'   modGBratio(tree=WYtree, cond=WYcond, pltassgn=WYpltassgn, sumunits=TRUE,
#'   	landarea="FOREST", unitvar="ESTN_UNIT", unitarea=WYunitarea, stratalut=WYstrlut,
#'   	estvarn="VOLCFNET", estvarn.filter="STATUSCD==1", rowvar="FORTYPCD", row.FIAname=TRUE,
#'   	allin1=TRUE, estround=0)
#'   }
#' 
#' @export modFAOest
modFAOest <- function(FAOpopdat=NULL, estseed="none", esttype="RATIO", 
	ratiotype="PERACRE", bcfilter=NULL, estvarn=NULL, estvarn.filter=NULL, 
	estvard=NULL, estvard.filter=NULL, TPA=TRUE, rowvar=NULL, colvar=NULL, 
	row.FIAname=FALSE, col.FIAname=FALSE, row.orderby=NULL, col.orderby=NULL, 
	row.add0=FALSE, col.add0=FALSE, rowlut=NULL, collut=NULL, rowgrp=FALSE, 
	rowgrpnm=NULL, rowgrpord=NULL, sumunits=FALSE, allin1=FALSE, metric=FALSE,
	estround=3, pseround=2, estnull="--", psenull="--", divideby=NULL, 
	savedata=FALSE, outfolder=NULL, outfn.pre=NULL, outfn.date=FALSE,
 	addtitle=TRUE, rawdata=FALSE, rawonly=FALSE, raw_fmt="csv", raw_dsn=NULL,
 	overwrite_dsn=FALSE, overwrite_layer=TRUE, append_layer=FALSE, returntitle=FALSE,
 	title.main=NULL, title.ref=NULL, title.rowvar=NULL, title.colvar=NULL,
 	title.unitvar=NULL, title.estvarn=NULL, title.estvard=NULL, title.filter=NULL,
 	gui=FALSE, ...){

  ##################################################################################
  ## DESCRIPTION:
  ## Generates estimates of trees by domain using non-ratio estimators.
  ##################################################################################

  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- c(names(formals(FIESTA::modFAOest)),
		names(formals(FIESTA::modFAOpop))) 
  if (!all(input.params %in% formallst)) {
    miss <- input.params[!input.params %in% formallst]
    stop("invalid parameter: ", toString(miss))
  }

  ## CHECK GUI - IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  if (nargs() == 0 && is.null(FAOpopdat)) {
    gui <- TRUE
  } 

  ## If gui.. set variables to NULL
  if (gui) { 
    strvar=areavar=sumunits=adjplot=strata=getwt=cuniqueid=ACI=
	tuniqueid=savedata=addtitle=returntitle=rawdata=unitvar <- NULL
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
  landarea <- "ALL"
  nonresp <- FALSE
  substrvar <- NULL
  returnFAOpopdat <- TRUE 
  returnlst <- list()


  ###################################################################################
  ## Check data and generate population information 
  ###################################################################################
  if (is.null(FAOpopdat)) {
    FAOpopdat <- modFAOpop(gui=gui, ...)
  } else {
    returnFAOpopdat <- FALSE
    list.items <- c("basex", "clustbasex", "treex", "buniqueid", "baseid", 
		"unitarea", "areavar", "unitlevel1", "unitvars", "stratalut")
    FAOpopdat <- FIESTA::pcheck.object(FAOpopdat, "FAOpopdat", list.items=list.items)
  }	
  if (is.null(FAOpopdat)) return(NULL)
  basex <- FAOpopdat$basex
  clustbasex <- FAOpopdat$clustbasex	
  treex <- FAOpopdat$treex
  seedx <- FAOpopdat$seedx
  if (is.null(treex) && is.null(seedx)) {
    stop("must include tree data for ratio estimates")
  }
  buniqueid <- FAOpopdat$buniqueid
  baseid <- FAOpopdat$baseid
  tuniqueid <- FAOpopdat$tuniqueid
  unitarea <- FAOpopdat$unitarea
  areavar <- FAOpopdat$areavar
  areaunits <- FAOpopdat$areaunits
  unitvar <- FAOpopdat$unitvar
  unitvars <- FAOpopdat$unitvars
  stratalut <- FAOpopdat$stratalut
  strvar <- FAOpopdat$strvar
  expcondtab <- FAOpopdat$expcondtab
  plotsampcnt <- FAOpopdat$plotsampcnt
  condsampcnt <- FAOpopdat$condsampcnt
  states <- FAOpopdat$states
  invyrs <- FAOpopdat$invyrs
  estvar.area <- FAOpopdat$estvar.area
  stratcombinelut <- FAOpopdat$stratcombinelut
  strwtvar <- FAOpopdat$strwtvar
  adj <- FAOpopdat$adj
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
  ## Check data and generate population information 
  ###################################################################################
  if (is.null(FAOpopdat)) {
    FAOpopdat <- modFAOpop(gui=gui, ...)
  } else {
    returnFAOpopdat <- FALSE
    list.items <- c("basex", "clustbasex", "treex", "buniqueid", "baseid", 
		"tuniqueid", "unitarea", "unitlevel1", "stratalut", "strvar",
		"plotsampcnt", "condsampcnt")
    FAOpopdat <- FIESTA::pcheck.object(FAOpopdat, "FAOpopdat", list.items=list.items)
  }		
  if (is.null(FAOpopdat)) return(NULL)
  condx <- FAOpopdat$basex
  pltcondx <- FAOpopdat$clustbasex
  treex <- FAOpopdat$treex
  if (is.null(treex)) stop("must include tree data for tree estimates")
  cuniqueid <- FAOpopdat$buniqueid
  condid <- FAOpopdat$baseid
  tuniqueid <- FAOpopdat$tuniqueid
  unitarea <- FAOpopdat$unitarea
  areavar <- FAOpopdat$areavar
  unitvar <- FAOpopdat$unitlevel1
  unitvars <- FAOpopdat$unitvars
  stratalut <- FAOpopdat$stratalut
  strvar <- FAOpopdat$strvar
  expcondtab <- FAOpopdat$expcondtab
  plotsampcnt <- FAOpopdat$plotsampcnt
  condsampcnt <- FAOpopdat$condsampcnt
  states <- FAOpopdat$states
  invyrs <- FAOpopdat$invyrs
  estvar.area <- FAOpopdat$estvar.area
  stratcombinelut <- FAOpopdat$stratcombinelut
  if (nonresp) {
    substrvar <- FAOpopdat$substrvar
    nonsampplots <- FAOpopdat$nonsampplots
  }
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
 	condid=condid, treex=treex, seedx=seedx, tuniqueid=tuniqueid, estseed=estseed,
	sumunits=sumunits, landarea=landarea, pcfilter=bcfilter, TPA=TPA,
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
  TPA <- estdat$TPA
  allin1 <- estdat$allin1
  estround <- estdat$estround
  pseround <- estdat$pseround
  divideby <- estdat$divideby
  addtitle <- estdat$addtitle
  returntitle <- estdat$returntitle
  rawdata <- estdat$rawdata
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
  ### Check row and column data
  ###################################################################################
  rowcolinfo <- check.rowcol(gui=gui, esttype=esttype, treef=treef, seedf=seedf,
	condf=pltcondf, cuniqueid=cuniqueid, condid=condid, tuniqueid=tuniqueid, 
	estseed=estseed, rowvar=rowvar, rowvar.filter=rowvar.filter, 
	colvar=colvar, colvar.filter=colvar.filter, row.orderby=row.orderby, 
	col.orderby=col.orderby, row.add0=row.add0, col.add0=col.add0, 
	title.rowvar=title.rowvar, title.colvar=title.colvar, rowlut=rowlut, 
	collut=collut, rowgrp=rowgrp, rowgrpnm=rowgrpnm, rowgrpord=rowgrpord, 
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
  concat <- rowcolinfo$concat
  grpvar <- rowcolinfo$grpvar
  tdomvar2 <- rowcolinfo$tdomvar2
  grpvar <- rowcolinfo$grpvar
  #rm(rowcolinfo)  

  if (rowvar == "TOTAL") rowcol.total <- TRUE

  ## Generate a uniquecol for estimation units
  if (!sumunits & colvar == "NONE") {
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
	cuniqueid=cuniqueid, condid=condid, 
	esttype=esttype, ratiotype=ratiotype, 
	estvarn=estvarn, estvarn.TPA=TPA, estvarn.filter=estvarn.filter,
 	estvarn.name=estvarn.name, 
	estvard=estvard, estvard.TPA=TPA, estvard.filter=estvard.filter,
 	estvard.name=estvard.name, 
	esttotn=TRUE, esttotd=TRUE, tdomvar=tdomvar, adjtree=adjtree, metric=metric)
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
  if (!is.null(tdomvar)) {
    cdomdat <- merge(condx, condf, by=c(cuniqueid, condid))
  }
  estvarn <- treedat$estvarn
  estvarn.name <- treedat$estvarn.name
  estvarn.filter <- treedat$estvarn.filter
  tdomvarlstn <- treedat$tdomvarlstn
  estunitsn <- treedat$estunitsn
  estunitsd <- treedat$estunitsd

  if (ratiotype == "PERTREE") {
    estvard <- treedat$estvard
    estvard.name <- treedat$estvard.name
    tdomvarlstd <- treedat$tdomvarlstd
  } else {
    estvard.name <- estvar.area
    tdomvarlstd <- NULL
    estunitsd <- areaunits
  }  

  #####################################################################################
  ### Get titles for output tables
  #####################################################################################
  alltitlelst <- check.titles(dat=tdomdat, esttype=esttype, estseed=estseed, 
	ratiotype=ratiotype, sumunits=sumunits, title.main=title.main, title.ref=title.ref,
 	title.rowvar=title.rowvar, title.rowgrp=title.rowgrp, title.colvar=title.colvar,
 	title.unitvar=title.unitvar, title.filter=title.filter, title.unitsn=estunitsn,
 	title.unitsd=estunitsd, title.estvarn=title.estvarn,
 	unitvar=unitvar, rowvar=rowvar, colvar=colvar,
 	estvarn=estvarn, estvarn.filter=estvarn.filter, estvard=estvard,
 	estvard.filter=estvard.filter, addtitle=addtitle, rawdata=rawdata, states=states,
 	invyrs=invyrs, landarea=landarea, pcfilter=bcfilter, allin1=allin1, 
	divideby=divideby, outfn.pre=outfn.pre)
  title.unitvar <- alltitlelst$title.unitvar
  title.est <- alltitlelst$title.est
  title.pse <- alltitlelst$title.pse
  title.estpse <- alltitlelst$title.estpse
  title.ref <- alltitlelst$title.ref
  outfn.estpse <- alltitlelst$outfn.estpse
  outfn.param <- alltitlelst$outfn.param
  if(rawdata) outfn.rawdat <- alltitlelst$outfn.rawdat
   

  ############################################################################
  ## GENERATE ESTIMATES
  ############################################################################
  unit_totest=unit.tdomest=unit_grpest=unit_rowest=unit_colest=unit_grpest=
	rowunit=totunit <- NULL
  addtotal <- ifelse(((rowvar == "TOTAL" || length(unique(tdomdat[[rowvar]])) > 1) ||
		(!is.null(tdomvarlstn) && length(tdomvarlstn) > 1)), TRUE, FALSE)
  stratalut$prop.total <- sum(tdomdat[[estvar.area]], na.rm=TRUE) 
  stratalut$propsq.total <- sum(tdomdat[[estvar.area]]^2, na.rm=TRUE) 

  ## Note: tdomdat is the summed response by condition (not domain)
  if (addtotal) {
    ## Get estimate for total
    tdomdat$TOTAL <- 1
    tdomdattot <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars, cuniqueid, "TOTAL"), .SDcols=c(estvarn.name, estvard.name)]
    unit_totest <- Ratio2Size(sumyn=estvarn.name, sumyd=estvard.name, 
		ysum=tdomdattot, uniqueid=cuniqueid, stratalut=stratalut, unitvar=unitvar, 
		strvar=strvar, domain="TOTAL")
    tabs <- FIESTA::check.matchclass(unitarea, unit_totest, unitvar)
    unitarea <- tabs$tab1
    unit_totest <- tabs$tab2
    setkeyv(unit_totest, unitvar)
    unit_totest <- unit_totest[unitarea, nomatch=0]
    unit_totest <- FIESTA::getarea(unit_totest, areavar=areavar, esttype=esttype)
  }

  ## Get row, column, cell estimate and merge area if row or column in cond table 
  if (rowvar != "TOTAL") {
    tdomdatsum <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars, cuniqueid, rowvar), .SDcols=c(estvarn.name, estvard.name)]
    unit_rowest <- Ratio2Size(sumyn=estvarn.name, sumyd=estvard.name, 
		ysum=tdomdatsum, uniqueid=cuniqueid, 
		stratalut=stratalut, unitvar=unitvar, strvar=strvar, domain=rowvar)

    if (colvar != "NONE") {
      tdomdatsum <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars, cuniqueid, colvar), .SDcols=c(estvarn.name, estvard.name)]
      unit_colest <- Ratio2Size(sumyn=estvarn.name, sumyd=estvard.name, 
		ysum=tdomdatsum, uniqueid=cuniqueid, 
		stratalut=stratalut, unitvar=unitvar, strvar=strvar, domain=colvar)

      tdomdatsum <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars, cuniqueid, grpvar), .SDcols=c(estvarn.name, estvard.name)]
      unit_grpest <- Ratio2Size(sumyn=estvarn.name, sumyd=estvard.name, 
		ysum=tdomdatsum, uniqueid=cuniqueid, 
		stratalut=stratalut, unitvar=unitvar, strvar=strvar, domain=grpvar)

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
    unit_rowest <- getarea(unit_rowest, areavar=areavar, esttype=esttype)
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
    strlut2 <- data.table(stratalut, ONEUNIT=1)
    strunitvars2 <- c("ONEUNIT", strvar)
    if (is.null(getwtvar) || !getwtvar %in% names(strlut2)) getwtvar <- "strwt"
    strlut2 <- strlut2[, lapply(.SD, sum, na.rm=TRUE), 
		by = strunitvars2, .SDcols=c(getwtvar, "n.strata")]
    strlut2[, strwt:=prop.table(get(getwtvar)), by="ONEUNIT"]
    strlut2[, n.total := sum(n.strata)]
    setkeyv(strlut2, strunitvars2)

    unitacres2 <- data.table(unitarea, ONEUNIT=1)
    unitacres2 <- unitacres2[, lapply(.SD, sum, na.rm=TRUE), by="ONEUNIT", 
		.SDcols=areavar]
    setkey(unitacres2, "ONEUNIT")

    tdomdat[, ONEUNIT := 1]

    ## Calculate unit totals for rowvar
    tdomdatsum <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars2, tuniqueid, rowvar), .SDcols=estvarn.name]
    rowunit <- Ratio2Size(sumyn=estvarn.name, sumyd=estvard.name, 
		ysum=tdomdatsum, uniqueid=cuniqueid, 
		stratalut=strlut2, unitvar="ONEUNIT", domain=rowvar)

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
		by=c(strunitvars2, tuniqueid, "TOTAL"), .SDcols=estvarn.name]
    totunit <- Ratio2Size(sumyn=estvarn.name, sumyd=estvard.name, 
		ysum=tdomdatsum, uniqueid=cuniqueid, 
		stratalut=strlut2, unitvar="ONEUNIT", domain="TOTAL")
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
  estnm <- ifelse(esttype == "RATIO", "rhat", "est")
  tabs <- est.outtabs(esttype=esttype, sumunits=sumunits, areavar=areavar, 
	unitvar=unitvar, unitvars=unitvars, unit_totest=unit_totest, 
	unit_rowest=unit_rowest, unit_colest=unit_colest, unit_grpest=unit_grpest, 
	rowvar=rowvar, colvar=colvar, uniquerow=uniquerow, uniquecol=uniquecol, 
	rowgrp=rowgrp, rowgrpnm=rowgrpnm, rowunit=rowunit, totunit=totunit, 
	allin1=allin1, savedata=savedata, addtitle=addtitle, title.ref=title.ref,
 	title.colvar=title.colvar, title.rowvar=title.rowvar, title.rowgrp=title.rowgrp,
 	title.unitvar=title.unitvar, title.estpse=title.estpse, title.est=title.est,
 	title.pse=title.pse, rawdata=rawdata, rawonly=rawonly, 
	outfn.estpse=outfn.estpse, outfolder=outfolder, overwrite=overwrite_layer,
 	outfn.date=outfn.date, estnm=estnm, estround=estround, pseround=pseround,
 	divideby=divideby, returntitle=returntitle, estnull=estnull, psenull=psenull) 
  est2return <- tabs$tabest
  pse2return <- tabs$tabpse

  if (!is.null(est2return)) {
    returnlst$est <- setDF(est2return)
  }
  if (!is.null(pse2return)) {
    returnlst$pse <- setDF(pse2return)
  }
  if (returntitle) {
    returnlst$titlelst <- alltitlelst
  }


  if (rawdata) {
    rawdat <- tabs$rawdat
    rawdat$domdat <- setDF(tdomdat) 
    rawdat$estvarn <- estvarn.name
    rawdat$estvarn.filter <- estvarn.filter
    if (ratiotype == "PERACRE") {
      rawdat$estvard <- estvard.name
      rawdat$estvard.filter <- estvard.filter
    }
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

    rawdat$esttype <- "RATIO"
    rawdat$estvarn <- estvarn
    rawdat$estvarn.filter <- estvarn.filter
    if (!is.null(estvard)) rawdat$estvard <- estvard
    if (!is.null(estvard.filter)) rawdat$estvard.filter <- estvard.filter
    if (!is.null(rowvar)) rawdat$rowvar <- rowvar
    if (!is.null(colvar)) rawdat$colvar <- colvar
    if (ratiotype == "PERACRE") {
      rawdat$areaunits <- areaunits
    }
    rawdat$estunitsn <- estunitsn
    if (ratiotype == "PERTREE") {
      rawdat$estunitsd <- estunitsd
    }
    returnlst$raw <- rawdat
  }
  if (returnFAOpopdat) {
    returnlst$FAOpopdat <- FAOpopdat
  }
    
  if ("STATECD" %in% names(pltcondf)) {
    returnlst$statecd <- sort(unique(pltcondf$STATECD))
  }
  if ("INVYR" %in% names(pltcondf)) {
    returnlst$invyr <- sort(unique(pltcondf$INVYR))
  }

  return(returnlst)
}
