#' Model-Assisted module - Generate model-assisted area estimates.
#' 
#' Generates area estimates by estimation unit. Estimates are calculated from
#' McConville et al. (2018)'s mase R package.
#' 
#' If variables are NULL, then it will prompt user to input variables.
#' 
#' Necessary variables:\cr \tabular{llll}{ \tab \bold{Data} \tab
#' \bold{Variable} \tab \bold{Description}\cr \tab tree \tab tuniqueid \tab
#' Unique identifier for each plot, to link to pltstrat (ex. PLT_CN).\cr \tab
#' \tab CONDID \tab Unique identifier of each condition on plot, to link to
#' cond.  Set CONDID=1, if only 1 condition per plot.\cr \tab cond \tab
#' cuniqueid \tab Unique identifier for each plot, to link to pltstrat (ex.
#' PLT_CN).\cr \tab \tab CONDID \tab Unique identifier of each condition on
#' plot.  Set CONDID=1, if only 1 condition per plot.\cr \tab \tab
#' CONDPROP_UNADJ \tab Unadjusted proportion of condition on each plot.  Set
#' CONDPROP_UNADJ=1, if only 1 condition per plot.\cr \tab \tab COND_STATUS_CD
#' \tab Status of each forested condition on plot (i.e. accessible forest,
#' nonforest, water, etc.)\cr \tab \tab NF_COND_STATUS_CD \tab If ACI=TRUE.
#' Status of each nonforest condition on plot (i.e. accessible nonforest,
#' nonsampled nonforest)\cr \tab \tab SITECLCD \tab If landarea=TIMBERLAND.
#' Measure of site productivity.\cr \tab \tab RESERVCD \tab If
#' landarea=TIMBERLAND. Reserved status.\cr \tab pltstrat \tab puniqueid \tab
#' Unique identifier for each plot, to link to cond (ex. CN).\cr \tab \tab
#' STATECD \tab Identifies state each plot is located in.\cr \tab \tab INVYR
#' \tab Identifies inventory year of each plot.\cr \tab \tab PLOT_STATUS_CD
#' \tab Status of each plot (i.e. sampled, nonsampled).  If not included, all
#' plots are assumed as sampled.\cr }
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
#' @param FIA Logical. If TRUE, the finite population term is removed from
#' estimator to match FIA estimates.
#' @param prednames String vector. Name(s) of predictor variables to include in
#' model.
#' @param modelselect Logical. If TRUE, an elastic net regression model is fit 
#' to the entire plot level data, and the variables selected in that model are 
#' used for the proceeding estimation.
#' @param landarea String. The sample area filter for estimates ('ALL',
#' 'FOREST', 'TIMBERLAND').  If landarea=FOREST, filtered to COND_STATUS_CD =
#' 1; If landarea=TIMBERLAND, filtered to SITECLCD in(1:6) and RESERVCD = 0.
#' @param pcfilter String. A filter for plot or cond attributes (including
#' pltassgn).  Must be R logical syntax.
#' @param rowvar String. Name of the row domain variable in cond or tree. If
#' only one domain, rowvar = domain variable. If more than one domain, include
#' colvar. If no domain, rowvar = NULL.
#' @param colvar String. Name of the column domain variable in cond or tree.
#' @param sumunits Logical. If TRUE, estimation units are summed and returned
#' in one table.
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
#' @param modelselect_bydomain Logical. If TRUE, modelselection will occur at 
#' the domain level as specified by rowvar and/or colvar and not at the level of
#' the entire sample.
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
#' # Use GREG estimator to estimate area of forest land in our population
#' mod1 <- modMAarea(MApopdat = MApopdat, 
#'           MAmethod = "greg", 
#'           landarea = "FOREST")
#' 
#' str(mod1)
#'           
#' # Use GREG estimator to estimate area of forest land by forest type and
#' # stand-size class
#' mod2 <- modMAarea(MApopdat = MApopdat,
#'           MAmethod = "greg",
#'           landarea = "FOREST",
#'           rowvar = "FORTYPCD",
#'           colvar = "STDSZCD")
#'           
#' str(mod2)
#' }
#' @export modMAarea
modMAarea <- function(MApopdat, 
                      MAmethod, 
                      FIA = TRUE, 
                      prednames = NULL, 
                      modelselect = FALSE, 
                      landarea = "FOREST", 
                      pcfilter = NULL, 
                      rowvar = NULL, 
                      colvar = NULL, 
                      sumunits = FALSE,
                      bootstrap = FALSE,
                      returntitle = FALSE, 
                      savedata = FALSE, 
                      table_opts = NULL, 
                      title_opts = NULL, 
                      savedata_opts = NULL, 
                      gui = FALSE, 
                      modelselect_bydomain = FALSE,
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
    landarea=strvar=areavar <- NULL
    if (!row.FIAname) row.FIAname <- NULL
    if (!col.FIAname) col.FIAname <- NULL
  }
  
  ## Set parameters
  minplotnum <- 10
  title.rowgrp=NULL
  esttype="AREA"
  parameters <- FALSE
  returnlst <- list()
  rawdata <- TRUE
  
  ## Set global variables
  ONEUNIT=n.total=n.strata=strwt=TOTAL <- NULL
  
  ##################################################################
  ## CHECK PARAMETER NAMES
  ##################################################################
  
  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- c(names(formals(modMAarea)),
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
  MAmethodlst <- c("HT", "PS", "greg", "gregEN", "ratio")
  MAmethod <- pcheck.varchar(var2check=MAmethod, varnm="MAmethod", gui=gui, 
		checklst=MAmethodlst, caption="MAmethod", multiple=FALSE, stopifnull=TRUE)

  if (MAmethod %in% c("greg", "gregEN")) {
    predselectlst <- list()
  }

  ###################################################################################
  ## Check data and generate population information 
  ###################################################################################
  list.items <- c("condx", "pltcondx", "cuniqueid", "condid", 
		"ACI.filter", "unitarea", "unitvar", "unitlut", "npixels",
		"npixelvar", "plotsampcnt", "condsampcnt")
#    if (MAmethod == "PS") {
#      list.items <- c(list.items, "strvar")
#    }
#    if (MAmethod == "greg") {
#      list.items <- c(list.items, "prednames")
#    }
  MApopdat <- pcheck.object(MApopdat, "MApopdat", list.items=list.items)
		
  if (is.null(MApopdat)) return(NULL)
  condx <- MApopdat$condx
  pltcondx <- MApopdat$pltcondx
  cuniqueid <- MApopdat$cuniqueid
  condid <- MApopdat$condid
  ACI.filter <- MApopdat$ACI.filter
  unitarea <- MApopdat$unitarea
  areavar <- MApopdat$areavar
  areaunits <- MApopdat$areaunits
  unitvar <- MApopdat$unitvar
  unitvars <- MApopdat$unitvars
  unitlut <- MApopdat$unitlut
  npixels <- MApopdat$npixels
  npixelvar <- MApopdat$npixelvar
  expcondtab <- MApopdat$expcondtab
  plotsampcnt <- MApopdat$plotsampcnt
  condsampcnt <- MApopdat$condsampcnt
  states <- MApopdat$states
  invyrs <- MApopdat$invyrs
  estvar.area <- MApopdat$estvar.area
  stratcombinelut <- MApopdat$stratcombinelut
  predfac <- MApopdat$predfac
  strvar <- MApopdat$strvar
  adj <- MApopdat$adj
  pop_fmt <- MApopdat$pop_fmt
  pop_dsn <- MApopdat$pop_dsn
  
 
  if (MAmethod %in% c("greg", "gregEN", "ratio")) {
    if (is.null(prednames)) {
      prednames <- MApopdat$prednames
    } else {
      if (!all(prednames %in% MApopdat$prednames)) {
        if (any(prednames %in% MApopdat$predfac)) {
          predfacnames <- prednames[prednames %in% MApopdat$predfac]
          for (nm in predfacnames) {           
            prednames[prednames == nm] <- MApopdat$prednames[grepl(nm, MApopdat$prednames)]
          }
        } else {
          stop("invalid prednames... must be in: ", toString(MApopdat$prednames))
        }
      }
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
  ### GET ROW AND COLUMN INFO FROM condf
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
				    cvars2keep="COND_STATUS_CD")
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
  
  ## Merge filtered condition data (condf) to all conditions (condx)
  #####################################################################################
  setkeyv(setDT(condx), c(cuniqueid, condid))
  setkeyv(setDT(condf), c(cuniqueid, condid))

  estvar.name <- "AREA"
  if (adj != "none") {
    estvar.name <- paste0(estvar.name, "_ADJ")
  }
  cdomdat <- merge(condx, condf, by=c(cuniqueid, condid), all.x=TRUE)
  cdomdat[, (estvar.name) := ifelse(is.na(TOTAL), 0, get(estvar.area))] 


  #####################################################################################
  ### GET TITLES FOR OUTPUT TABLES
  #####################################################################################
  alltitlelst <- check.titles(dat=cdomdat, esttype=esttype, 
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
    outfn.rawdat <- paste0(outfn.rawdat, "_modMA_mase", "_", MAmethod) 
  } 
  ## Append name of package and method to outfile name
  outfn.estpse <- paste0(outfn.estpse, "_modMA_mase", "_", MAmethod) 
  
  #####################################################################################
  ## GENERATE ESTIMATES
  #####################################################################################
  unit_totest=unit_rowest=unit_colest=unit_grpest=rowunit=totunit <- NULL
  addtotal <- ifelse(rowvar == "TOTAL" || length(unique(condf[[rowvar]])) > 1, TRUE, FALSE)
  estunits <- sort(unique(cdomdat[[unitvar]]))
  response <- estvar.name
  
  masemethod <- ifelse(MAmethod == "PS", "postStrat", 
	ifelse(MAmethod == "greg", "greg", 
	ifelse(MAmethod == "gregEN", "gregElasticNet", 
	ifelse(MAmethod == "ratio", "ratioEstimator", "horvitzThompson"))))
  message("generating estimates using mase::", masemethod, " function...\n")
  
  predselect.overall <- NULL
  if (MAmethod == "greg" && modelselect == T) {
    
    # want to do variable selection on plot level data...
    pltlvl <- cdomdat[ , lapply(.SD, sum, na.rm = TRUE), 
                       by=c(unitvar, cuniqueid, "TOTAL", strvar, prednames),
                       .SDcols=response]
    
    y <- pltlvl[[response]]
    xsample <- pltlvl[ , prednames, with = F, drop = F]
    
    # need to go means -> totals -> summed totals
    xpop <- unitlut[ , c(unitvar, prednames), with = F, drop = F]
    if (is.factor(xpop[[unitvar]])) {
      npix_temp <- npixels[ ,(unitvar) := as.factor(get(unitvar))]
    } else {
      npix_temp <- npixels
    }
    
    xpop_npix <- merge(xpop, npix_temp, by = unitvar, all.x = TRUE)
    # multiply unitvar level population means by corresponding npixel values to get population level totals
    xpop_npix[ ,2:ncol(xpop)] <- lapply(xpop_npix[ ,2:ncol(xpop)], function(x) xpop_npix[["npixels"]] * x)
    # sum those values
    xpop_totals <- colSums(xpop_npix[ ,2:ncol(xpop)])
    # format xpop for mase input
    xpop_totals <- data.frame(as.list(xpop_totals))
    
    N <- sum(npixels[["npixels"]])
    xpop_means <- xpop_totals/N
    
    coefs_select <- MAest.greg(y = y,
                               N = N,
                               x_sample = setDF(xsample),
                               x_pop = xpop_means,
                               modelselect = TRUE)
    
    predselect.overall <- coefs_select$predselect
    prednames <- names(predselect.overall[ ,(!is.na(predselect.overall))[1,], with = F])
    message(paste0("Predictors ", "[", paste0(prednames, collapse = ", "), "]", " were chosen in model selection.\n"))
    
  }
  
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

  if (addtotal) {
    ## Get total estimate and merge area
    cdomdattot <- cdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(unitvar, cuniqueid, "TOTAL", strvar, prednames), .SDcols=estvar.name]
    unit_totestlst <- lapply(estunits, MAest.unit, 
                          dat=cdomdattot, cuniqueid=cuniqueid, 
                          unitlut=unitlut, unitvar=unitvar, esttype=esttype, 
                          MAmethod=MAmethod, strvar=strvar, prednames=prednames, 
                          domain="TOTAL", response=estvar.name, npixels=npixels, 
                          FIA=FIA, modelselect=modelselect_bydomain, getweights=getweights,
                          var_method=var_method)
    unit_totest <- do.call(rbind, sapply(unit_totestlst, '[', "unitest"))
    if (getweights) {
      unit_weights <- do.call(rbind, sapply(unit_totestlst, '[', "weights")) 
      unit_weights$areaweights <- unit_weights$weights * sum(unitarea[[areavar]])
    }

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
  }
  
  ## Get row, column, cell estimate and merge area if row or column in cond table 
  if (rowvar != "TOTAL") {
    cdomdat <- cdomdat[!is.na(cdomdat[[rowvar]]),] 
    cdomdatsum <- cdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(unitvar, cuniqueid, rowvar, strvar, prednames), .SDcols=estvar.name]

    unit_rowestlst <- lapply(estunits, MAest.unit, 
                        dat=cdomdatsum, cuniqueid=cuniqueid, 
                        unitlut=unitlut, unitvar=unitvar, esttype=esttype, 
                        MAmethod=MAmethod, strvar=strvar, prednames=prednames, 
                        domain=rowvar, response=estvar.name, npixels=npixels, 
                        FIA=FIA, modelselect=modelselect_bydomain, getweights=getweights,
                        var_method=var_method)
    unit_rowest <- do.call(rbind, sapply(unit_rowestlst, '[', "unitest"))
    if (MAmethod %in% c("greg", "gregEN")) {
      predselectlst$rowest <- do.call(rbind, sapply(unit_totestlst, '[', "predselect"))
    }
  }
  if (colvar != "NONE") {
    cdomdat <- cdomdat[!is.na(cdomdat[[colvar]]),] 	
    cdomdatsum <- cdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(unitvar, cuniqueid, colvar, strvar, prednames), .SDcols=estvar.name]

    unit_colestlst <- lapply(estunits, MAest.unit, 
                      dat=cdomdatsum, cuniqueid=cuniqueid, 
                      unitlut=unitlut, unitvar=unitvar, esttype=esttype, 
                      MAmethod=MAmethod, strvar=strvar, prednames=prednames, 
                      domain=colvar, response=estvar.name, npixels=npixels, 
                      FIA=FIA, modelselect=modelselect_bydomain, var_method=var_method)
    unit_colest <- do.call(rbind, sapply(unit_colestlst, '[', "unitest"))
    if (MAmethod %in% c("greg", "gregEN")) {
      predselectlst$grpest <- do.call(rbind, sapply(unit_grpest, '[', "predselect"))
    }

    cdomdatsum <- cdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(unitvar, cuniqueid, grpvar, strvar, prednames), .SDcols=estvar.name]
    cdomdatsum[, grpvar := do.call(paste, c(.SD, sep="#")), .SDcols=grpvar]

    unit_grpestlst <- lapply(estunits, MAest.unit, 
                        dat=cdomdatsum, cuniqueid=cuniqueid, 
                        unitlut=unitlut, unitvar=unitvar, esttype=esttype, 
                        MAmethod=MAmethod, strvar=strvar, prednames=prednames, 
                        domain="grpvar", response=estvar.name, npixels=npixels, 
                        FIA=FIA, modelselect=modelselect_bydomain, var_method=var_method)
    unit_grpest <- do.call(rbind, sapply(unit_grpestlst, '[', "unitest"))
    preds_grpest <- do.call(rbind, sapply(unit_grpestlst, '[', "predselect"))
    if (any(unit_grpest$grpvar == "NA#NA")) {
        unit_grpest <- unit_grpest[unit_grpest$grpvar != "NA#NA", ]
    }
    unit_grpest[, c(rowvar, colvar) := tstrsplit(grpvar, "#", fixed=TRUE)]
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
            rowvar=rowvarnm, colvar=colvarnm, uniquerow=uniquerow, uniquecol=uniquecol, 
            rowgrp=rowgrp, rowgrpnm=rowgrpnm, rowunit=rowunit, totunit=totunit, 
            allin1=allin1, savedata=savedata, addtitle=addtitle, 
			title.ref=title.ref, title.colvar=title.colvar, 
			title.rowvar=title.rowvar, title.rowgrp=title.rowgrp, 
            title.unitvar=title.unitvar, title.estpse=title.estpse, 
            title.est=title.est, title.pse=title.pse, 
			rawdata=rawdata, rawonly=rawonly, outfn.estpse=outfn.estpse, 
			outfolder=outfolder, outfn.date=outfn.date, 
			overwrite=overwrite_layer, estnm=estnm, 
            estround=estround, pseround=pseround, divideby=divideby, 
			returntitle=returntitle, estnull=estnull, psenull=psenull,
			raw.keep0=raw.keep0)
 
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
    rawdat$domdat <- setDF(cdomdat)
    #rawdat$expcondtab <- unit_weights
    if (getweights) {
      rawdat$plotweights <- unit_weights
    }
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
    rawdat$esttype <- esttype
    rawdat$MAmethod <- MAmethod
    rawdat$predselectlst <- predselectlst
    rawdat$predselect.overall <- predselect.overall
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

