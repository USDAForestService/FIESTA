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
#' @param prednames String vector. Name(s) of predictor variables to include in
#' model.
#' @param modelselect Logical. If TRUE, an elastic net regression model is fit 
#' to the entire plot level data, and the variables selected in that model are 
#' used for the proceeding estimation.
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
#' mod1 <- modMAtree(MApopdat = MApopdat,
#'           MAmethod = "greg",
#'           estvar = "VOLCFNET",
#'           estvar.filter = "STATUSCD == 1")
#'           
#' str(mod1)
#' }
#' @export modMAtree
modMAtree <- function(MApopdat, 
                      MAmethod, 
                      estvar, 
                      estvar.filter = NULL, 
                      estvar.derive = NULL,
                      estseed = "none",
                      woodland = "Y",
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
                      modelselect_bydomain = FALSE,
                      ...){

  ##################################################################################
  ## DESCRIPTION: 
  ## Generates model-assisted estimates by domain (and estimation unit)
  ##################################################################################

  gui <- FALSE
  
  ## If gui.. set variables to NULL
  if (gui) { 
    tree=landarea=strvar=areavar <- NULL
    if (!row.FIAname) row.FIAname <- NULL
    if (!col.FIAname) col.FIAname <- NULL
  }
  
  ## Set parameters
  esttype <- "TREE"
  popType <- "VOL"
  rawdata <- TRUE 
  sumunits <- FALSE
  returnlst <- list()
  
  ## Set global variables
  ONEUNIT=n.total=n.strata=strwt=TOTAL=domclassify<- NULL
  
  
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
  
  ## Check MAmethod 
  MAmethodlst <- c("greg", "gregEN", "ratio")
  MAmethod <- pcheck.varchar(var2check=MAmethod, varnm="MAmethod", gui=gui, 
		checklst=MAmethodlst, caption="MAmethod", multiple=FALSE, stopifnull=TRUE)

  if (MAmethod %in% c("greg", "gregEN")) {
    predselectlst <- list()
  }

  ###################################################################################
  ## Check data and generate population information 
  ###################################################################################
  list.items <- c("pltcondx", "cuniqueid", "condid",
                  "treex", "tuniqueid",
                  "unitarea", "unitvar", "unitlut", "npixels",
		              "npixelvar", "plotsampcnt", "condsampcnt")

  MApopdat <- pcheck.object(MApopdat, "MApopdat", list.items=list.items)
  if (is.null(MApopdat)) return(NULL)	
  pltidsadj <- MApopdat$pltidsadj
  pltcondx <- MApopdat$pltcondx
  cuniqueid <- MApopdat$cuniqueid
  pltassgnid <- MApopdat$pltassgnid
  condid <- MApopdat$condid
  treex <- MApopdat$treex
  seedx <- MApopdat$seedx
  if (is.null(treex) && is.null(seedx)) {
    stop("must include tree data for tree estimates")
  }
  tuniqueid <- MApopdat$tuniqueid
  ACI <- MApopdat$ACI
  pltassgnx <- MApopdat$pltassgnx
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
  predfac <- MApopdat$predfac
  adj <- MApopdat$adj
  popdatindb <- MApopdat$popdatindb
  pop_fmt <- MApopdat$pop_fmt
  pop_dsn <- MApopdat$pop_dsn
  pop_schema <- MApopdat$pop_schema
  popconn <- MApopdat$popconn
  dbqueries <- MApopdat$dbqueries
  dbqueriesWITH <- MApopdat$dbqueriesWITH
  adjcase <- MApopdat$adjcase
  pltidsid <- MApopdat$pjoinid
  pltassgnid <- MApopdat$pltassgnid
  pltcondflds <- MApopdat$pltcondflds
  
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
    pltcondxWITHqry=pltcondxadjWITHqry <- NULL
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
  estdat <- 
    check.estdata(esttype = esttype,
                  popType = popType,
                  popdatindb = popdatindb,
                  popconn = popconn, pop_schema = pop_schema,
                  pltcondx = pltcondx,
                  pltcondflds = pltcondflds,
                  totals = totals,
                  pop_fmt=pop_fmt, pop_dsn=pop_dsn,
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

  #################################################################################
  ### GET ESTIMATION DATA FROM TREE TABLE
  #################################################################################
  adjtree <- ifelse(adj %in% c("samp", "plot"), TRUE, FALSE)
  if (popdatindb) {
    pltidsWITHqry <- dbqueriesWITH$pltcondxadjWITH
  } else {
    pltidsWITHqry <- NULL
  }
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
               bydomainlst = c(domainlst, "TOTAL"),
               tdomvar = tdomvar, tdomvar2 = tdomvar2,
               adjtree = adjtree, 
               adjvar = "tadjfac",
               metric = metric, 
               woodland = woodland,
               ACI = ACI,
               domclassify = domclassify,
               dbconn = popconn, schema = pop_schema,
               pltidsWITHqry = pltidsWITHqry,
               pcwhereqry = pcwhereqry,
               pltidsid = pltidsid,
               bytdom = bytdom,
               gui = gui)
  if (is.null(treedat)) stop(NULL) 
  tdomdat <- treedat$tdomdat
  estvar <- treedat$estvar
  estvar.name <- treedat$estvar.name
  estvar.filter <- treedat$estvar.filter
  tdomvarlst <- treedat$tdomvarlst
  estunits <- treedat$estunits
  treeqry <- treedat$treeqry
  classifynmlst <- treedat$classifynmlst
  
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

  #################################################################################
  ### GET TITLES FOR OUTPUT TABLES
  #################################################################################
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
                 estvarn = estvar,
                 estvarn.filter = estvar.filter, 
                 addtitle = addtitle,
                 returntitle = returntitle, 
                 rawdata = rawdata,
                 states = states, invyrs = invyrs, 
                 landarea = landarea,
                 pcfilter =pcfilter,
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
    outfn.rawdat <- paste0(outfn.rawdat, "_modMA_mase", "_", MAmethod) 
  } 
  ## Append name of package and method to outfile name
  outfn.estpse <- paste0(outfn.estpse, "_modMA_mase", "_", MAmethod) 

 
  #################################################################################
  ## GENERATE ESTIMATES
  #################################################################################
  estdat <- 
    getMAestimates(esttype = esttype,
                   domdatn = tdomdat,
                   uniqueid = pltassgnid,
                   estvarn.name = estvar.name,
                   rowvar = rowvar, colvar = colvar, 
                   grpvar = grpvar,
                   MAmethod = MAmethod,
                   modelselect = modelselect,
                   prednames = prednames,
                   FIA = FIA,
                   bootstrap = bootstrap,
                   pltassgnx = pltassgnx,
                   unitarea = unitarea,
                   unitvar = unitvar,
                   areavar = areavar,
                   unitlut = unitlut,
                   npixels = npixels,
                   totals = totals,
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
  predselectlst <- estdat$predselectlst
  predselect.overall <- estdat$predselect.overall
  unit_weights <- estdat$unit_weights
 
  
  ###############################################################################
  ## GENERATE OUTPUT TABLES
  ###############################################################################
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
    rawdat <- tabs$rawdat
    rawdat$domdat <- setDF(tdomdat)
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
    rawdat$predselect.overall <- predselect.overall
    rawdat$predselectlst <- predselectlst
    rawdat$estvar <- estvar
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

