#' Green-Book module - Generate area estimates.
#' 
#' Generates area estimates by domain (and estimation unit). Calculations are
#' based on Scott et al. 2005 ('the green-book') for mapped forest inventory
#' plots. The non-ratio estimator for estimating area by stratum and domain is
#' used. Plots that are totally nonsampled are excluded from estimation
#' dataset. Next, an adjustment factor is calculated by strata to adjust for
#' nonsampled (nonresponse) conditions that have proportion less than 1. The
#' attribute is the proportion of the plot which is divided by the adjustment
#' factor, and averaged by stratum. Strata means are combined using the strata
#' weights and then expanded to area using the total land area in the
#' population.
#' 
#' If variables are NULL, then it will prompt user to input variables.
#' 
#' Necessary variables:\cr \tabular{llll}{ \tab \bold{Data} \tab
#' \bold{Variable} \tab \bold{Description}\cr \tab cond \tab cuniqueid \tab
#' Unique identifier for each plot, to link to pltassgn (ex. PLT_CN).\cr \tab
#' \tab CONDID \tab Unique identifier of each condition on plot.  Set CONDID=1,
#' if only 1 condition per plot.\cr \tab \tab CONDPROP_UNADJ \tab Unadjusted
#' proportion of condition on each plot.  Set CONDPROP_UNADJ=1, if only 1
#' condition per plot.\cr \tab \tab COND_STATUS_CD \tab Status of each forested
#' condition on plot (i.e. accessible forest, nonforest, water, etc.)\cr \tab
#' \tab NF_COND_STATUS_CD \tab If ACI=TRUE. Status of each nonforest condition
#' on plot (i.e. accessible nonforest, nonsampled nonforest)\cr \tab \tab
#' SITECLCD \tab If landarea=TIMBERLAND. Measure of site productivity.\cr \tab
#' \tab RESERVCD \tab If landarea=TIMBERLAND. Reserved status.\cr
#' 
#' \tab pltassgn \tab puniqueid \tab Unique identifier for each plot, to link
#' to cond (ex. CN).\cr \tab \tab STATECD \tab Identifies state each plot is
#' located in.\cr \tab \tab INVYR \tab Identifies inventory year of each
#' plot.\cr \tab \tab PLOT_STATUS_CD \tab Status of each plot (i.e. sampled,
#' nonsampled).  If not included, all plots are assumed as sampled.\cr }
#' 
#' For available reference tables: sort(unique(FIESTAutils::ref_codes$VARIABLE)) \cr
#' 
#' @param GBpopdat List. Population data objects returned from modGBpop().
#' @param p2vegtype String. Type of p2veg estimate ('str', 'spp').
#' @param peracre Logical. If TRUE, generates per-acre estimates.
#' @param landarea String. The sample area filter for estimates ("ALL",
#' "FOREST", "TIMBERLAND").  If landarea=FOREST, filtered to COND_STATUS_CD =
#' 1; If landarea=TIMBERLAND, filtered to SITECLCD in(1:6) and RESERVCD = 0.
#' @param pcfilter String. A filter for plot or cond attributes (including
#' pltassgn).  Must be R logical syntax.
#' @param vfilter String. A filter for the P2 vegetation table used for
#' estimate.  Must be R logical syntax.
#' @param rowvar String. Name of row domain variable in cond (e.g., 'FORTYPCD') 
#' or P2VEG_SUBP_STRUCTURE (e.g., 'GROWTH_HABIT_CD', 'LAYER') or 
#' P2VEG_SUBPLOT_SPP (e.g., 'VEG_FLDSPCD', 'VEG_SPCD', 'GROWTH_HABIT_CD', 'LAYER'). 
#' If only one domain, rowvar = domain variable. If more than one domain, include 
#' colvar. If no domain, rowvar = NULL.
#' @param colvar String. Name of column domain variable in cond.
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
#' \item{est}{ Data frame. Area estimates, in area units (e.g., acres), by
#' rowvar, colvar (and estimation unit). If sumunits=TRUE or one estimation
#' unit and colvar=NULL, or allin1=TRUE, estimates and percent sampling error
#' are in one data frame. } \item{pse}{ Data frame. Percent sampling errors
#' (Confidence level 68%) for estimates by rowvar and colvar (and estimation
#' unit). } \item{titlelst}{ List. If returntitle=TRUE a list with table
#' title(s). The list contains one title if est and pse are in the same table
#' and two titles if est and pse are in separate tables. Row and column tables
#' are also included in list. } \item{raw}{ List. If rawdata=TRUE, a list
#' including the processing data used for estimation including: number of plots
#' and conditions; stratification information; and 1 to 8 tables with
#' calculated values for table cells and totals (See processing data below). }
#' 
#' Raw data
#' 
#' \item{plotsampcnt}{ Table. Number of plots by plot status (e.g., sampled
#' forest on plot, sampled nonforest, nonsampled). } \item{condsampcnt}{ DF.
#' Number of conditions by condition status (forest land, nonforest land,
#' noncensus water, census water, nonsampled). } \item{unitarea}{ DF. Area by
#' estimation unit. } \item{expcondtab}{ DF. Condition-level area expansion
#' factors. } \item{domdat}{ DF. Final data table used for estimation. }
#' 
#' \item{stratdat}{ Data frame. Strata information by estimation unit. }
#' \tabular{lll}{ \tab \bold{Variable} \tab \bold{Description} \cr \tab unitvar
#' \tab estimation unit \cr \tab strvar \tab stratum value \cr \tab strwtvar
#' \tab number of pixels by strata and estimation unit \cr \tab n.strata \tab
#' number of plots in strata (after totally nonsampled plots removed) \cr \tab
#' n.total \tab number of plots for estimation unit \cr \tab strwt \tab
#' proportion of area (or plots) by strata and estimation unit (strata weight)
#' \cr \tab CONDPROP_UNADJ_SUM \tab summed condition proportion by strata and
#' estimation unit \cr \tab CONDPROP_ADJFAC \tab adjusted condition proportion
#' by strata after nonresponse plots removed \cr \tab AREA \tab total area for
#' estimation unit \cr \tab CONDPROP_ADJFAC \tab average area \cr }
#' 
#' \item{processing data}{ Data frames. Separate data frames containing
#' calculated variables used in estimation process. The number of processing
#' tables depends on the input parameters. The tables include: total by
#' estimation unit (unit.totest); rowvar totals (unit.rowest), colvar totals,
#' if not NULL (unit.colvar); and a combination of rowvar and colvar, if colvar
#' is not NULL (unit.grpvar). If sumunits=TRUE, the raw data for the summed
#' estimation units are also included (totest, rowest, colest, grpest,
#' respectively).  These tables do not included estimate proportions (nhat and
#' nhat.var).
#' 
#' The data frames include the following information: \tabular{lll}{ \tab
#' \bold{Variable} \tab \bold{Description}\cr \tab nhat \tab estimate
#' proportion of land \cr \tab nhat.var \tab variance estimate of estimated
#' proportion of land \cr \tab NBRPLT.gt0 \tab Number of non-zero plots used in
#' estimates \cr \tab AREA \tab total area for estimation unit \cr \tab est
#' \tab estimated area of land nhat*areavar \cr \tab est.var \tab variance
#' estimate of estimate acres of land nhat.var*areavar^2 \cr \tab est.se \tab
#' standard error of estimated area of land sqrt(est.var) \cr \tab est.cv \tab
#' coefficient of variation of estimated area of land est.se/est \cr \tab pse
#' \tab percent sampling error of estimate est.cv*100 \cr \tab CI99left \tab
#' left tail of 99 percent confidence interval for estimated area \cr \tab
#' CI99right \tab right tail of 99 percent confidence interval for estimated
#' area \cr \tab CI95left \tab left tail of 95 percent confidence interval for
#' estimated area \cr \tab CI95right \tab right tail of 95 percent confidence
#' interval for estimated area \cr \tab CI67left \tab left tail of 67 percent
#' confidence interval for estimated area \cr \tab CI67right \tab right tail of
#' 67 percent confidence interval for estimated area \cr } }
#' 
#' savedata\cr if savedata=TRUE...\cr tables with estimate and percent standard
#' error will be written as *csv files to outfolder.  if rawdata=TRUE, the
#' rawdata will be output to the outfolder in a folder named rawdata (if
#' raw_fmt="csv") or a database in the outfolder, if (raw_fmt != "csv").
#' 
#' if outfn.pre is not null...\cr a prefix is added to output files if raw_fmt
#' = 'csv', prefix is added to file names in rawdata folder if raw_fmt !=
#' 'csv', prefix is added to dsn name
#' @note
#' 
#' ADJUSTMENT FACTOR:\cr The adjustment factor is necessary to account for
#' nonsampled conditions. It is calculated for each estimation unit by strata
#' by summing the unadjusted condition proportions (CONDPROP_UNADJ) and
#' dividing by the number of plots in the strata/estimation unit.
#' 
#' If ACI=FALSE, only nonsampled forest conditions are accounted for in the
#' adjustment factor. \cr If ACI=TRUE, the nonsampled nonforest conditions are
#' removed as well and accounted for in adjustment factor. This is if you are
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
#' @author Tracey S. Frescino, Paul L. Patterson, Elizabeth A. Freeman
#' @references Scott, Charles T.; Bechtold, William A.; Reams, Gregory A.;
#' Smith, William D.; Westfall, James A.; Hansen, Mark H.; Moisen, Gretchen G.
#' 2005. Sample-based estimators used by the Forest Inventory and Analysis
#' national information management system. Gen. Tech. Rep. SRS-80.  Asheville,
#' NC: U.S. Department of Agriculture, Forest Service, Southern Research
#' Station, p.53-77.
#' @keywords data
modGBp2veg <- function(GBpopdat = NULL, 
                       p2vegtype = "str", 
                       peracre = FALSE,
                       landarea = "FOREST", 
                       pcfilter = NULL, 
                       vfilter = NULL, 
                       rowvar = NULL, 
                       colvar = NULL,
                       sumunits = TRUE,
                       returntitle = FALSE, 
                       savedata = FALSE,
                       table_opts = NULL, 
                       title_opts = NULL,
                       savedata_opts = NULL, 
                       ...){

  ###################################################################################
  ## DESCRIPTION: 
  ## Generates acre estimates by domain (and estimation unit)
  ###################################################################################

  
  ## CHECK GUI - IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  #if (nargs() == 0 && is.null(GBpopdat)) gui <- TRUE
  gui <- FALSE
  
  ## If gui.. set variables to NULL
  if (gui) { 
    landarea=strvar=areavar=sumunits=adj=strata=getwt=cuniqueid=ACI=
      puniqueid=savedata=addtitle=returntitle=rawdata=unitvar=variable <- NULL
    #if (!row.FIAname) row.FIAname <- NULL
    #if (!col.FIAname) col.FIAname <- NULL
  }
  
  ## Set parameters
  #esttype <- "P2VEG"
  popType <- "VOL"
  nonresp <- FALSE
  substrvar <- NULL
  rawdata <- TRUE
  returnlst <- list()
  
  ## Set global variables
  ONEUNIT=n.total=n.strata=strwt=TOTAL=rawfolder=domclassify=nhat=nhat.var <- NULL
  
  
  ##################################################################
  ## CHECK PARAMETER NAMES
  ##################################################################
  
  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
#  formallst <- c(names(formals(modGBp2veg)),
#                 names(formals(modGBpop))) 
  formallst <- names(formals(modGBp2veg)) 
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
		                "unitarea", "unitvar", "stratalut", "strvar",
		                "plotsampcnt", "condsampcnt")
  GBpopdat <- pcheck.object(GBpopdat, "GBpopdat", list.items=list.items)
  if (is.null(GBpopdat)) return(NULL)
  popType <- GBpopdat$popType
  pltidsadj <- GBpopdat$pltidsadj
  pltcondx <- GBpopdat$pltcondx
  cuniqueid <- GBpopdat$cuniqueid
  condid <- GBpopdat$condid
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
  areawt <- GBpopdat$areawt
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
  pltcondflds <- GBpopdat$pltcondflds
  
  vcondsppx <- GBpopdat$vcondsppx
  vcondstrx <- GBpopdat$vcondstrx
  varadjP2VEG <- GBpopdat$varadjP2VEG
  vuniqueid <- "PLT_CN"
  estvar.name <- GBpopdat$estvar.area
  estvar <- "COVER_PCT_SUM"
  
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
    pltcondxWITHqry <- dbqueriesWITH$pltcondxWITH
    pltcondxadjWITHqry <- dbqueriesWITH$pltcondxadjWITH
  } else {
    pltcondxWITHqry=pltcondxadjWITHqry <- NULL
  }
  

  ## Check peracre
  ########################################################
  peracre <- pcheck.logical(peracre, varnm="peracre",
		title="Per-acre estimates?", first="YES", gui=gui, stopifnull=TRUE)
  
  ## Define estimation type
  esttype <- ifelse(peracre, "RATIO", "TREE")
  
  
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
  estdat <- 
    check.estdata(esttype = esttype, 
                  popType = popType,
                  popdatindb = popdatindb, 
                  popconn = popconn, pop_schema = pop_schema,
                  pltcondx = pltcondx,
                  pltcondflds = pltcondflds,
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
  ## Check parameter inputs and P2VEG filters
  ###################################################################################
  estdatP2VEG <- 
    check.estdataP2VEG(esttype = esttype,
                       popdatindb = popdatindb,
                       popconn = popconn,
                       vcondsppx = vcondsppx, vcondstrx = vcondstrx,
                       vuniqueid = vuniqueid,
                       vfilter = vfilter,
                       gui = gui)
  vcondstrx <- estdatP2VEG$vcondstrx
  vcondstrflds <- estdatP2VEG$vcondstrflds
  vcondsppflds <- estdatP2VEG$vcondsppflds
  vuniqueid <- estdatP2VEG$vuniqueid

 
  ## Check p2vegtype 
  ########################################################
  p2vegtypelst <- c("str", "spp")
  p2vegtype <- pcheck.varchar(var2check=p2vegtype, varnm="p2vegtype", 
                              checklst=p2vegtypelst, caption="P2VEG type", stopifnull=TRUE)
  if(p2vegtype == "str") {
    vcondx <- vcondstrx
    vcondflds <- vcondstrflds
  } else {
    vcondx <- vcondsppx
    vcondflds <- vcondsppflds
  }
  
  
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
                 treex = vcondx, treeflds = vcondflds,
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
                 whereqry = pcwhereqry)
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
  

  ## Generate a uniquecol for estimation units
  if (!sumunits && colvar == "NONE") {
    uniquecol <- data.table(unitarea[[unitvar]])
    setnames(uniquecol, unitvar)
    uniquecol[[unitvar]] <- factor(uniquecol[[unitvar]])
  }

  
  #####################################################################################
  ### Get estimation data from vcond table
  #####################################################################################
  adjtree <- ifelse(adj %in% c("samp", "plot"), TRUE, FALSE)
  p2vegdat <- 
    check.tree(treex = vcondx, 
               bycond = TRUE, 
               condx = pltcondx, 
               tuniqueid = vuniqueid, cuniqueid = cuniqueid, 
               esttype = esttype, 
               estvarn = estvar, 
               estvarn.filter = vfilter, 
               estvarn.TPA = FALSE,
               esttotn = TRUE, 
               bydomainlst = domainlst,
               tdomvar = tdomvar, tdomvar2 = tdomvar2,
               adjtree = adjtree, 
               adjvar = varadjP2VEG,
               metric = metric, 
               woodland = NULL,
               ACI = ACI,
               domclassify = domclassify,
               dbconn = popconn, schema = pop_schema,
               pltidsWITHqry = pltcondxadjWITHqry,
               pcwhereqry = pcwhereqry,
               pltidsid = pltidsid,
               bytdom = bytdom,
               gui = gui)
  if (is.null(p2vegdat)) return(NULL) 
  vdomdat <- p2vegdat$tdomdat
  p2vegqry <- p2vegdat$treeqry
  classifynmlst <- p2vegdat$classifynmlst
  pcdomainlst <- p2vegdat$pcdomainlst
  tdomvarlstn <- p2vegdat$tdomvarlstn
  
  ## change variable in query from tree variables to veg variables for display
  p2vegqry2 <- gsub("tdat", "vdat", p2vegqry)
  p2vegqry2 <- gsub("get tree data", "get p2veg data", p2vegqry2)
  p2vegqry2 <- gsub("treex t", "p2vegx v", p2vegqry2)
  p2vegqry2 <- gsub(" 'TREE' src,", "", p2vegqry2)
  p2vegqry2 <- gsub(" t.", " v.", p2vegqry2)
  p2vegqry2 <- gsub("\\(t.", "\\(v.", p2vegqry2)
  message(p2vegqry2)  
  
  if (esttype == "RATIO") {
    estvarn.name <- p2vegdat$estvarn.name
    estvarn.filter <- p2vegdat$estvarn.filter
    estvard.name <- areawt
    estvard.filter <- p2vegdat$estvard.filter
    tdomvarlstd <- p2vegdat$tdomvarlstd
    estunitsn <- "percent"
    estunitsd <- areaunits
  } else {
    estvarn.name <- p2vegdat$estvar.name
    estvarn.filter <- p2vegdat$estvar.filter
    estunitsn <- p2vegdat$estunits
  }
  
  
  ###################################################################################
  ### Get condition-level domain data
  ###################################################################################
  cdomdat=estvard.name <- NULL
  if (esttype == "RATIO") {
    conddat <- 
      check.cond(areawt = areawt,
                 adj = adj,
                 adjcase = adjcase,
                 cuniqueid = cuniqueid, 
                 condid = condid,
                 rowvar = rowvar, colvar = colvar,
                 pcdomainlst = pcdomainlst,
                 popdatindb = popdatindb,
                 popconn = popconn,
                 pltcondx = pltcondx,
                 pltidsadj = pltidsadj,
                 pltcondxadjWITHqry = pltcondxadjWITHqry,
                 pltidsid = pltidsid,
                 pcwhereqry = pcwhereqry,
                 classifyrow = classifyrow,
                 classifycol = classifycol)
    cdomdat <- conddat$cdomdat
    cdomdatqry <- conddat$cdomdatqry
    estvard.name <- conddat$estnm
  }
  

  ###################################################################################
  ### Get titles for output tables
  ###################################################################################
  alltitlelst <- 
    check.titles(dat = vdomdat, esttype = "AREA", 
                 sumunits = sumunits, 
                 title.main = title.main, 
                 title.ref = title.ref, 
                 title.rowvar = title.rowvar, 
                 title.rowgrp = title.rowgrp, 
                 title.colvar = title.colvar, 
                 title.unitvar = title.unitvar, 
                 title.filter = title.filter, 
                 title.unitsn = areaunits, 
                 unitvar = unitvar, 
                 rowvar = rowvar, colvar=colvar, 
                 addtitle = addtitle, 
                 returntitle = returntitle, 
                 rawdata = rawdata, 
                 states = states, invyrs = invyrs, 
                 landarea = landarea, pcfilter = pcfilter, 
                 allin1 = allin1, divideby = divideby, 
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
  

  ############################################################################
  ## GENERATE ESTIMATES
  ############################################################################
  estdat <- 
    getGBestimates(esttype = esttype,
                   domdatn = vdomdat,
                   domdatd = cdomdat,
                   uniqueid = pltassgnid,
                   estvarn.name = estvarn.name,
                   estvard.name = estvard.name,
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
  estnm <- ifelse(esttype == "RATIO", "estn", "est")
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
    
    if ("unit_totest" %in% names(tabs$rawdat)) {
      tabs$rawdat$unit_totest <- merge(tabs$rawdat$unit_totest, NBRPLTtot, by=unitvars)
    }
    if (sumunits && "totest" %in% names(tabs$rawdat)) {
      tabs$rawdat$totest <- data.frame(tabs$rawdat$totest, NBRPLT = sum(NBRPLTtot$NBRPLT))
    }
    
    rawdat <- tabs$rawdat
    rawdat$domdat <- setDF(vdomdat) 
    rawdat$domdatqry <- p2vegqry2
    rawdat$estvarn <- estvarn.name
    rawdat$estvarn.filter <- estvarn.filter
    if (esttype == "RATIO") {
      rawdat$estvard <- estvard.name
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
    
    rawdat$module <- "GB"
    rawdat$esttype <- esttype
    rawdat$GBmethod <- ifelse(strata, "PS", "HT")
    rawdat$estvarn <- estvarn.name
    rawdat$estvarn.filter <- estvarn.filter

    if (!is.null(rowvar)) rawdat$rowvar <- rowvar
    if (!is.null(colvar)) rawdat$colvar <- colvar
    if (esttype == "RATIO") {
      rawdat$areaunits <- areaunits
    }
    rawdat$estunitsn <- estunitsn
    returnlst$raw <- rawdat
  }
  
  return(returnlst)
}
