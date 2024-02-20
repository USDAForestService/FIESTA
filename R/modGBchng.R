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
#' Necessary variables:\cr 
#' \tabular{llll}{ \tab \bold{Data} 
#' \tab \bold{Variable} \tab \bold{Description}\cr 
#' \tab cond \tab cuniqueid \tab Unique identifier for each plot, to link to pltassgn (ex. PLT_CN).\cr \tab
#' \tab CONDID \tab Unique identifier of each condition on plot.  Set CONDID=1,
#' if only 1 condition per plot.\cr 
#' \tab \tab CONDPROP_UNADJ \tab Unadjusted proportion of condition on each plot. Set CONDPROP_UNADJ=1, if only 1
#' condition per plot.\cr 
#' \tab \tab COND_STATUS_CD \tab Status of each forested condition on plot (i.e. accessible forest, nonforest, water, etc.)\cr \tab
#' \tab NF_COND_STATUS_CD \tab If ACI=TRUE. Status of each nonforest condition
#' on plot (i.e. accessible nonforest, nonsampled nonforest)\cr 
#' \tab \tab SITECLCD \tab If landarea=TIMBERLAND. Measure of site productivity.\cr \tab
#' \tab RESERVCD \tab If landarea=TIMBERLAND. Reserved status.\cr
#' 
#' \tab pltassgn \tab puniqueid \tab Unique identifier for each plot, to link
#' to cond (e.g., CN).\cr 
#' \tab \tab STATECD \tab Identifies state each plot is located in.\cr \tab \tab INVYR \tab Identifies inventory year of each
#' plot.\cr 
#' \tab \tab PLOT_STATUS_CD \tab Status of each plot (i.e. sampled, nonsampled). If not included, all plots are assumed as sampled.\cr }
#' 
#' For available reference tables: sort(unique(FIESTAutils::ref_codes$VARIABLE)) \cr
#' 
#' @param GBpopdat List. Population data objects returned from modGBpop().
#' @param chngtype String. The type of change estimates ('total', 'annual').
#' @param landarea String. The sample area filter for estimates ("ALL",
#' "FOREST", "TIMBERLAND").  If landarea=FOREST, filtered to COND_STATUS_CD =
#' 1; If landarea=TIMBERLAND, filtered to SITECLCD in(1:6) and RESERVCD = 0.
#' @param pcfilter String. A filter for plot or cond attributes (including
#' pltassgn).  Must be R logical syntax.
#' @param rowvar String. Name of row domain variable in cond. If only one
#' domain, rowvar = domain variable. If more than one domain, include colvar.
#' If no domain, rowvar = NULL.
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
#' @param gui Logical. If gui, user is prompted for parameters.
#' @param ...  Parameters for modGBpop() if GBpopdat is NULL.
#' @return A list with estimates with percent sampling error for rowvar (and
#' colvar).  If sumunits=TRUE or unitvar=NULL and colvar=NULL, one data frame
#' is returned.  Otherwise, a list object is returned with the following
#' information.  If savedata=TRUE, all data frames are written to outfolder.
#' 
#' \item{est}{ Data frame. Area estimates, in area units (e.g., acres), by
#' rowvar, colvar (and estimation unit). If sumunits=TRUE or one estimation
#' unit and colvar=NULL, or allin1=TRUE, estimates and percent sampling error
#' are in one data frame. } 
#' \item{pse}{ Data frame. Percent sampling errors (Confidence level 68%) 
#' for estimates by rowvar and colvar (and estimation unit). } 
#' \item{titlelst}{ List. If returntitle=TRUE a list with table title(s). The 
#' list contains one title if est and pse are in the same table and two titles 
#' if est and pse are in separate tables. Row and column tables are also included 
#' in list. } 
#' \item{raw}{ List. If rawdata=TRUE, a list including the processing data used 
#' for estimation including: number of plots and conditions; stratification 
#' information; and 1 to 8 tables with calculated values for table cells and 
#' totals (See processing data below). }
#' 
#' Raw data
#' 
#' \item{plotsampcnt}{ Table. Number of plots by plot status (e.g., sampled
#' forest on plot, sampled nonforest, nonsampled). } 
#' \item{condsampcnt}{ DF. Number of conditions by condition status 
#' (forest land, nonforest land, noncensus water, census water, nonsampled). } 
#' \item{unitarea}{ DF. Area by estimation unit. } 
#' \item{expcondtab}{ DF. Condition-level area expansion factors. } 
#' \item{domdat}{ DF. Final data table used for estimation. }
#' 
#' \item{stratdat}{ Data frame. Strata information by estimation unit. }
#' \tabular{lll}{ \tab \bold{Variable} \tab \bold{Description} \cr
#' \tab unitvar \tab estimation unit \cr \tab strvar \tab stratum value \cr 
#' \tab strwtvar \tab number of pixels by strata and estimation unit \cr 
#' \tab n.strata \tab number of plots in strata (after totally nonsampled 
#' plots removed) \cr 
#' \tab n.total \tab number of plots for estimation unit \cr 
#' \tab strwt \tab proportion of area (or plots) by strata and estimation 
#' unit (strata weight) \cr \tab CONDPROP_UNADJ_SUM \tab summed condition 
#' proportion by strata and estimation unit \cr 
#' \tab CONDPROP_ADJFAC \tab adjusted condition proportion by strata after 
#' nonresponse plots removed \cr 
#' \tab AREA \tab total area for estimation unit \cr 
#' \tab CONDPROP_ADJFAC \tab average area \cr }
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
#' \bold{Variable} \tab \bold{Description}\cr 
#' \tab nhat \tab estimate proportion of land \cr 
#' \tab nhat.var \tab variance estimate of estimated proportion of land \cr 
#' \tab NBRPLT.gt0 \tab Number of non-zero plots used in estimates \cr 
#' \tab AREA \tab total area for estimation unit \cr 
#' \tab est \tab estimated area of land nhat*areavar \cr 
#' \tab est.var \tab variance estimate of estimate acres of land 
#' nhat.var*areavar^2 \cr 
#' \tab est.se \tab standard error of estimated area of land sqrt(est.var) \cr 
#' \tab est.cv \tab coefficient of variation of estimated area of land est.se/est 
#' \cr \tab pse \tab percent sampling error of estimate est.cv*100 \cr 
#' \tab CI99left \tab left tail of 99 percent confidence interval for 
#' estimated area \cr 
#' \tab CI99right \tab right tail of 99 percent confidence interval for 
#' estimated area \cr 
#' \tab CI95left \tab left tail of 95 percent confidence interval for 
#' estimated area \cr 
#' \tab CI95right \tab right tail of 95 percent confidence interval for 
#' estimated area \cr 
#' \tab CI67left \tab left tail of 67 percent confidence interval for 
#' estimated area \cr 
#' \tab CI67right \tab right tail of 67 percent confidence interval for 
#' estimated area \cr } }
#' 
#' savedata\cr 
#' if savedata=TRUE...\cr 
#' tables with estimate and percent standard error will be written as *csv 
#' files to outfolder.  if rawdata=TRUE, the rawdata will be output to the 
#' outfolder in a folder named rawdata (if raw_fmt="csv") or a database in 
#' the outfolder, if (raw_fmt != "csv").
#' 
#' if outfn.pre is not null...\cr 
#' a prefix is added to output files if raw_fmt = 'csv', prefix is added to 
#' file names in rawdata folder if raw_fmt != 'csv', prefix is added to dsn name
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
#' @export modGBchng
modGBchng <- function(GBpopdat, 
                      chngtype = "total",
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
                      gui = FALSE, 
                      ...){

  ###################################################################################
  ## DESCRIPTION: 
  ## Generates acre estimates by domain (and estimation unit)
  ###################################################################################

  
  ## CHECK GUI - IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  if (nargs() == 0 && is.null(GBpopdat)) {
    gui <- TRUE
  } 
  
  ## If gui.. set variables to NULL
  if (gui) { 
    landarea=strvar=areavar=sumunits=adj=strata=getwt=cuniqueid=ACI=
      puniqueid=savedata=addtitle=returntitle=rawdata=unitvar <- NULL
    #if (!row.FIAname) row.FIAname <- NULL
    #if (!col.FIAname) col.FIAname <- NULL
  }
  
  
  ## INITIALIZE SETTINGS
  esttype <- "AREA" 
  nonresp <- FALSE
  substrvar <- NULL
  parameters <- FALSE
  returnlst <- list()
  rawdata <- TRUE
  colvar <- NULL
  
  ## Set global variables
  ONEUNIT=n.total=n.strata=strwt=TOTAL=
    rawfolder <- NULL
  #estvar <- "CONDPROP_ADJ"
  
  ##################################################################
  ## CHECK PARAMETER NAMES
  ##################################################################
  
  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- c(names(formals(modGBchng)),
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
  
  list.items <- c("condx", "pltcondx", "cuniqueid", "condid", 
  "ACI.filter", "unitarea", "unitvar", "stratalut", "strvar",
  "plotsampcnt", "condsampcnt")
  GBpopdat <- pcheck.object(GBpopdat, "GBpopdat", list.items=list.items)
  if (is.null(GBpopdat)) return(NULL)
  condx <- GBpopdat$condx
  sccmx <- GBpopdat$sccmx
  pltcondx <- GBpopdat$pltcondx
  cuniqueid <- GBpopdat$cuniqueid
  condid <- GBpopdat$condid
  ACI.filter <- GBpopdat$ACI.filter
  unitarea <- GBpopdat$unitarea
  areavar <- GBpopdat$areavar
  areaunits <- GBpopdat$areaunits
  unitvar <- GBpopdat$unitvar
  unitvars <- GBpopdat$unitvars
  strata <- GBpopdat$strata
  stratalut <- GBpopdat$stratalut
  strvar <- GBpopdat$strvar
  expcondtab <- GBpopdat$expcondtab
  plotsampcnt <- GBpopdat$plotsampcnt
  condsampcnt <- GBpopdat$condsampcnt
  states <- GBpopdat$states
  invyrs <- GBpopdat$invyrs
  estvar.name <- GBpopdat$estvar.area
  stratcombinelut <- GBpopdat$stratcombinelut
  strwtvar <- GBpopdat$strwtvar
  strunitvars <- c(unitvars, strvar)
  strata <- GBpopdat$strata
  pop_fmt <- GBpopdat$pop_fmt
  pop_dsn <- GBpopdat$pop_dsn
  pltassgnx <- GBpopdat$pltassgnx
  pltassgnid <- GBpopdat$pltassgnid

  ## Check chngtype
  ########################################################
  chngtypelst <- c('total', 'annual')
  chngtype <- pcheck.varchar(var2check=chngtype, varnm="chngtype", gui=gui,
		checklst=chngtypelst, caption="chngtype", multiple=FALSE, stopifnull=TRUE)

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
  estdat <- check.estdata(esttype=esttype, pop_fmt=pop_fmt, pop_dsn=pop_dsn,
                pltcondf=pltcondx, cuniqueid=cuniqueid, condid=condid,  
                sumunits=sumunits, totals=totals, landarea=landarea, 
				ACI.filter=ACI.filter, pcfilter=pcfilter, 
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
  ### Check row and column data
  ###################################################################################
  rowcolinfo <- check.rowcol(gui=gui, esttype=esttype, 
                  condf=pltcondf, cuniqueid=cuniqueid, 
                  rowvar=rowvar, colvar=colvar, 
                  row.FIAname=row.FIAname, col.FIAname=col.FIAname, 
                  row.orderby=row.orderby, col.orderby=col.orderby, 
                  row.add0=row.add0, col.add0=col.add0, 
                  title.rowvar=title.rowvar, title.colvar=title.colvar, 
                  rowlut=rowlut, collut=collut, 
                  rowgrp=rowgrp, rowgrpnm=rowgrpnm, rowgrpord=rowgrpord, 
                  landarea=landarea, states=states, 
                  cvars2keep=c("PREV_PLT_CN","REMPER","PROP_BASIS",
				               "COND_STATUS_CD","CONDPROP_UNADJ", 
							   "COND_NONSAMPLE_REASN_CD"))
  condf <- rowcolinfo$condf
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
  rm(rowcolinfo)

  ## Generate a uniquecol for estimation units
  if (!sumunits && colvar == "NONE") {
    uniquecol <- data.table(unitarea[[unitvar]])
    setnames(uniquecol, unitvar)
    uniquecol[[unitvar]] <- factor(uniquecol[[unitvar]])
  }

#  ## Add a column for totals
#  addtotal <- ifelse(rowvar == "TOTAL" || length(unique(condf[[rowvar]])) > 1, TRUE, FALSE)
#  if (addtotal) {
#    condf$TOTAL <- 1
#  }

  ## Merge filtered condition data (condf) to all conditions (condx)
  ###################################################################################
  setkeyv(condx, c(cuniqueid, condid))
  setkeyv(condf, c(cuniqueid, condid))


#  if (rowvar == "TOTAL" || colvar == "NONE") {
#    stop("must include a rowvar and a colvar for area change estimates")
#  }
  
#  groupby.qry <- paste0("c.", cuniqueid, ", c.", condid)

  groupby.qry <- paste0(toString(paste0("sccm.", strunitvars)), ", c.", cuniqueid, ", c.", condid)
#  groupby.qry <- paste0(toString(strunitvars), ", "c.", cuniqueid)
  prev_rowvar <- paste0("PREV_", rowvar)

  if (rowvar != "TOTAL") {
    select.qry <- paste0(groupby.qry, ", pcond.", rowvar, " AS ", prev_rowvar)
    if (colvar != "NONE") {
      select.qry <- paste0(select.qry, ", c.", colvar, " AS ", colvar)
    } else {

      ## Copy rowvar information to colvar
      colvar <- rowvar
	  colvarnm <- rowvarnm
      uniquecol <- copy(uniquerow)
      title.colvar <- title.rowvar
      if (!is.null(row.orderby)) {
        col.orderby <- row.orderby
      }
      select.qry <- paste0(select.qry, ", c.", rowvar)

      ## Add prefix to rowvar
      names(uniquerow) <- paste0("PREV_", names(uniquerow))
      rowvar <- prev_rowvar 
      rowvarnm <- paste0("PREV_", rowvarnm)
      if (!is.null(row.orderby)) {
        row.orderby <- paste0("PREV_", row.orderby)
      }

      ## Get group variable for cell values
      grpvar <- c(rowvar, colvar)
    }
#  } else {
#    select.qry <- groupby.qry
  }
 
  if (chngtype == "annual") {
    condf_chng.qry <- paste0("SELECT ", select.qry, ",", 
                              "\n       SUM(", estvar.name, " / 4 / c.REMPER) AS ysum")
  } else {
    condf_chng.qry <- paste0("SELECT ", select.qry, ",", 
                              "\n       SUM(", estvar.name, " / 4) AS ysum")
  }
 
  condf_chng.qry <- paste0(condf_chng.qry,
               "\nFROM condf c",
               "\nJOIN condf pcond ON (pcond.PLT_CN = c.PREV_PLT_CN)",
			   "\nJOIN sccmx sccm ON (sccm.plt_cn = c.plt_cn 
                          AND sccm.prev_plt_cn = pcond.plt_cn 
                          AND sccm.condid = c.condid 
                          AND sccm.prevcond = pcond.condid)",
               "\nWHERE c.CONDPROP_UNADJ IS NOT NULL 
			            AND ((sccm.SUBPTYP = 3 AND c.PROP_BASIS = 'MACR') OR 
	                              (sccm.SUBPTYP = 1 AND c.PROP_BASIS = 'SUBP'))
						AND COALESCE(c.COND_NONSAMPLE_REASN_CD, 0) = 0 
                        AND COALESCE(pcond.COND_NONSAMPLE_REASN_CD, 0) = 0",
               "\nGROUP BY ", groupby.qry)
			   
  # condf_chng.qry <- paste0(condf_chng.qry,
               # "\nFROM condf c",
               # "\nJOIN condf pcond ON (pcond.PLT_CN = c.PREV_PLT_CN)",
			   # "\nJOIN condx sccm ON (sccm.plt_cn = c.plt_cn 
                          # AND sccm.prev_plt_cn = pcond.plt_cn 
                          # AND sccm.condid = c.condid 
                          # AND sccm.prevcond = pcond.condid)",
               # "\nGROUP BY ", groupby.qry)


  if (rowvar != "TOTAL") {
    condf_chng.qry <- paste0(condf_chng.qry, ", ", rowvar, ", c.", colvar)
  }
  message(condf_chng.qry)

  condf_chng <- data.table(sqldf::sqldf(condf_chng.qry))
  setkeyv(condf_chng, c(cuniqueid, condid))  

  #cdomdat <- condx[condf_chng]
  cdomdat <- condf_chng
  cdomdat$TOTAL <- 1

  if (length(unitvars) == 2) {
    cdomdat[, (unitvar) := paste(cdomdat[[unitvars[1]]], cdomdat[[unitvars[2]]], sep="-")]
	strunitvars <- c(unitvar, strvar)
  }
	
	
  ###################################################################################
  ### Get titles for output tables
  ###################################################################################
  alltitlelst <- check.titles(dat=cdomdat, esttype=esttype, tabtype="CHNG",
                  sumunits=sumunits, title.main=title.main, title.ref=title.ref, 
                  title.rowvar=title.rowvar, title.rowgrp=title.rowgrp, 
                  title.colvar=title.colvar, title.unitvar=title.unitvar, 
                  title.filter=title.filter, title.unitsn=areaunits, 
                  unitvar=unitvar, rowvar=rowvar, colvar=colvar, 
                  addtitle=addtitle, returntitle=returntitle, 
                  rawdata=rawdata, states=states, invyrs=invyrs, 
                  landarea=landarea, pcfilter=pcfilter, 
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
  }

  ###################################################################################
  ## GENERATE ESTIMATES
  ###################################################################################
  unit_totest=unit_rowest=unit_colest=unit_grpest=rowunit=totunit=tdomdattot <- NULL
  addtotal <- ifelse(rowvar == "TOTAL" || length(unique(condf[[rowvar]])) > 1, TRUE, FALSE)
  estvar.name <- "ysum" 
  stratalut <- setDT(stratalut)

  message("getting estimates using GB...")
#  if (addtotal) {
    ## Get total estimate and merge area
    cdomdattot <- cdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars, cuniqueid, "TOTAL"), .SDcols=estvar.name]
    unit_totest <- GBest.pbar(sumyn = estvar.name, 
	                          ysum = cdomdattot,
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
#  }

  ## Get row estimate  
  if (rowvar != "TOTAL") {
    cdomdatsum <- cdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars, cuniqueid, rowvar), .SDcols=estvar.name]
    unit_rowest <- GBest.pbar(sumyn = estvar.name, 
	                          ysum = cdomdatsum,
							  uniqueid = cuniqueid, 
							  stratalut = stratalut, 
							  unitvar = unitvar, 
							  strvar = strvar, 
							  domain = rowvar)
  }

  ## Get column (and cell) estimate  
  if (colvar != "NONE") {
    cdomdatsum <- cdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars, cuniqueid, colvar), .SDcols=estvar.name]
    unit_colest <- GBest.pbar(sumyn = estvar.name, 
	                          ysum = cdomdatsum,
							  uniqueid = cuniqueid, 
							  stratalut = stratalut,
							  unitvar = unitvar, 
							  strvar = strvar, 
							  domain = colvar)

    cdomdatsum <- cdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars, cuniqueid, grpvar), .SDcols=estvar.name]
    unit_grpest <- GBest.pbar(sumyn = estvar.name, 
	                          ysum = cdomdatsum,
							  uniqueid = cuniqueid, 
							  stratalut = stratalut,
							  unitvar = unitvar, 
							  strvar = strvar, 
							  domain = grpvar)
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
                            uniquex=uniquecol,unitvar=unitvar, 
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
  if (!sumunits && (length(unique(unitarea[[unitvar]])) > 1 && !is.null(grpvar))) {

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

    cdomdat[, ONEUNIT := 1]

    ## CALCULATE UNIT TOTALS FOR ROWVAR
    cdomdatsum <- cdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars2, cuniqueid, rowvar), .SDcols=estvar.name]
    rowunit <- GBest.pbar(sumyn=estvar.name, ysum=cdomdatsum, 
                    uniqueid=cuniqueid, stratalut=stratalut2, 
                    unitvar="ONEUNIT", strvar=strvar, domain=rowvar)
    rowunit <- add0unit(x=rowunit, xvar=rowvar, uniquex=uniquerow, 
		unitvar="ONEUNIT", xvar.add0=row.add0)
    tabs <- check.matchclass(unitacres2, rowunit, "ONEUNIT")
    unitacres2 <- tabs$tab1
    rowunit <- tabs$tab2
    setkeyv(rowunit, "ONEUNIT")
    rowunit <- rowunit[unitacres2, nomatch=0]
    if (totals) {
      rowunit <- getpse(rowunit, areavar=areavar, esttype=esttype)
    } else {
      rowunit <- getpse(rowunit, esttype=esttype)
    }      
    setkeyv(rowunit, c("ONEUNIT", rowvar))

    ## CALCULATE GRAND TOTAL FOR ALL UNITS
    cdomdatsum <- cdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars2, cuniqueid, "TOTAL"), .SDcols=estvar.name]
    totunit <- GBest.pbar(sumyn=estvar.name, ysum=cdomdatsum, 
                    uniqueid=cuniqueid, stratalut=stratalut2, 
                    unitvar="ONEUNIT", strvar=strvar, domain="TOTAL")
    tabs <- check.matchclass(unitacres2, totunit, "ONEUNIT")
    unitacres2 <- tabs$tab1
    totunit <- tabs$tab2
    setkeyv(totunit, "ONEUNIT")
    totunit <- totunit[unitacres2, nomatch=0]
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
 	      rowvar=rowvarnm, colvar=colvarnm, uniquerow=uniquerow, uniquecol=uniquecol,
 	      rowgrp=rowgrp, rowgrpnm=rowgrpnm, rowunit=rowunit, totunit=totunit, 
	      allin1=allin1, savedata=savedata, addtitle=addtitle, title.ref=title.ref,
 	      title.rowvar=title.rowvar, title.colvar=title.colvar, title.rowgrp=title.rowgrp,
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
  if (returntitle) {
    returnlst$titlelst <- alltitlelst
  }
 
  if (rawdata) {
    rawdat <- tabs$rawdat
    rawdat$domdat <- setDF(cdomdat)
    if (savedata) {
      if (!is.null(title.estpse)) {
        title.raw <- paste(title.estpse, title.ref)
      } else {
        title.raw <- paste(title.est, title.ref, sep="; ")
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
    rawdat$chngtype <- chngtype
    rawdat$GBmethod <- ifelse(strata, "PS", "HT")
    if (!is.null(rowvar)) rawdat$rowvar <- rowvar
    if (!is.null(colvar)) rawdat$colvar <- colvar
    rawdat$areaunits <- areaunits
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
