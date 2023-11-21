#' Photo-Based module - Generate photo-based estimates.
#' 
#' Generates percent, area or ratio-of-means estimates, with associated
#' sampling error by domain (and estimation unit). Calculations are based on
#' Patterson (2012) photo-based estimators for the Nevada photo-based
#' inventory.
#' 
#' If variables are NULL, then it will prompt user to input variables.
#' 
#' @param PBpopdat List. Population data objects returned from modPBpop().
#' @param tabtype String. Type of units for the table ("PCT", "AREA").
#' @param sumunits Logical. If TRUE, estimation units are combined to one table
#' for output. Note: only available if tabtype="AREA". Acres
#' @param ratio Logical. If TRUE, ratio estimates are generated.
#' @param landarea String. Sample area for estimates ("ALL", "CHANGE"). Used to
#' describe landarea.filter.
#' @param landarea.filter String. filter for land area. Must be R syntax.
#' @param nonsamp.pntfilter String. An expression for filtering nonsampled
#' points (e.g., cloud coverage). Must be R syntax.
#' @param pntfilter String. A global filter for the pnt file. Must be R syntax.
#' @param pfilter String. A global filter for the plt file. Must be R syntax.
#' @param rowvar String. Name of domain variable in pnt used for output
#' estimation table rows. If only 1 domain, must be rowvar. If no domain,
#' rowvar=NULL.
#' @param colvar String. Name of domain variable in pnt used for output
#' estimation table columns. If only 1 domain, colvar=NULL.
#' @param domlut DF/DT or comma-delimited (*.csv). Look-up table to define the
#' variables in the pnt table with category codes (DOMCODE) and code names
#' (DOMNAME), and to set a pretty name for the variable to use in output table
#' (DOMTITLE). This table is also used to populate rowvar/colvar,
#' row.orderby/col.orderby, and title.rowvar/title.colvar parameters. Optional.
#' @param domvarlst String vector. A vector of variable names that can be row
#' or column domains (codes and names). Optional.
#' @param ratioden String. ("ROWVAR" or "COLVAR"). If ratio, defines whether
#' the rowvar or colvar in estimation output table is the denominator.
#' @param gainloss Logical. If TRUE, a table with the difference of gain and
#' loss along with the variance and standard error, in percent, is generated.
#' @param gainloss.vals String vector. A vector of names for values in gainloss
#' table.
#' @param savedata Logical. If TRUE, saves table(s) to outfolder.
#' @param addtitle Logical. If TRUE and savedata=TRUE, adds title to outfile.
#' @param returntitle Logical. If TRUE, returns a character string of the title
#' of the output data frame.
#' @param savedata Logical. If TRUE, saves table(s) to outfolder.
#' @param table_opts List. See help(table_options()) for a list of
#' options.
#' @param title_opts List. See help(title_options()) for a list of options.
#' @param savedata_opts List. See help(savedata_options()) for a list
#' of options. Only used when savedata = TRUE.  
#' @param gui Logical. If gui, user is prompted for parameters.
#' @param ...  Parameters for modPBpop() if PBpopdat is NULL.
#' 
#' @return A list with estimates with percent sampling error for rowvar (and
#' colvar).  If sumunits=TRUE or unitvar=NULL and colvar=NULL, one data frame
#' is returned.  Otherwise, a list object is returned with the following
#' information.  If savedata=TRUE, all data frames are written to outfolder.
#' 
#' \item{est}{ DF. Estimated percent cover or area by rowvar, colvar, (and
#' estimation unit).  } \item{pse}{ DF. Percent sampling error of estimates by
#' rowvar, colvar (and estimation unit). } \item{titlelst}{ List with 1 or 2
#' string vectors. If returntitle=TRUE a list with table title(s). The list
#' contains one title if est and pse are in the same table and two titles if
#' est and pse are in separate tables. Row and column tables are also included
#' in list. } \item{raw}{ List of data frames. If rawdata=TRUE, a list
#' including the processing data used for estimation including: number of plots
#' and conditions; stratification information; and 1 to 8 tables with
#' calculated values for table cells and totals (See processing data below). }
#' 
#' Raw data
#' 
#' \item{pntsampcnt}{ Table. Number of points by rowvar/colvar (sampled and
#' nonsampled). }
#' 
#' \item{stratdat}{ Data frame. Strata information by estimation unit. }
#' \tabular{lll}{ \tab \bold{Variable} \tab \bold{Description}\cr \tab unitvar
#' \tab estimation unit\cr \tab strvar \tab strata \cr \tab areavar \tab If
#' tabtype='AREA', area by strata for estimation unit\cr \tab n.strata \tab
#' number of plots in strata (after totally nonsampled plots removed) \cr \tab
#' n.total \tab number of plots for estimation unit \cr \tab TOTAREA \tab If
#' tabtype='AREA', total area for estimation unit \cr \tab strwt \tab
#' proportion of area (or number of plots) by strata (strata weight) \cr }
#' 
#' \item{processing data}{ Data frames. Separate data frames of variables used
#' in estimation process for the rowvar, colvar and combination of rowvar and
#' colvar (if colvar is not NULL), and grand total by estimation unit
#' (unit.rowest, unit.colest, unit.grpest, respectively) and summed estimation
#' units, if FIA=TRUE (rowest, colest, grpest, respectively).
#' 
#' The data frames include the following information: \tabular{lll}{ \tab
#' \bold{Variable} \tab \bold{Description}\cr \tab phat \tab estimated
#' proportion of covered land \cr \tab phat.var \tab variance of estimated
#' proportion of covered land \cr \tab areavar \tab If tabtype='AREA', total
#' area for estimation unit\cr \tab est \tab If tabtype='AREA', estimated area
#' of land phat*areavar If tabtype='PCT', estimated percent cover of land
#' phat*100 \cr \tab est.var \tab variance of estimated area of land
#' phat.var*areavar \cr If tabtype='PCT', estimated percent cover of land
#' phat.var*100 \cr \tab est.se \tab standard error of estimated area or
#' percent cover sqrt(est.var) \cr \tab est.cv \tab coefficient of variance of
#' estimated area or percent cover est.se/est \cr \tab est.pse \tab percent
#' sampling error of estimated area of percent cover est.cv*100 \cr \tab
#' CI99left \tab left tail of 99 percent confidence interval for estimate \cr
#' \tab CI99right \tab right tail of 99 percent confidence interval for
#' estimate \cr \tab CI95left \tab left tail of 95 percent confidence interval
#' for estimate \cr \tab CI95right \tab right tail of 95 percent confidence
#' interval for estimate \cr \tab CI67left \tab left tail of 67 percent
#' confidence interval for estimate \cr \tab CI67right \tab right tail of 67
#' percent confidence interval for estimate \cr }
#' 
#' if ratio=TRUE: \tabular{lll}{ \tab \bold{Variable} \tab
#' \bold{Description}\cr \tab phat.n \tab estimated proportion of covered land,
#' for numerator (colvar) \cr \tab phat.var.n \tab variance of estimated
#' proportion of covered land, for numerator (colvar) \cr \tab phat.d \tab
#' estimated proportion of covered land, for denominator (rowvar) \cr \tab
#' phat.var.d \tab variance of estimated proportion of covered land, for
#' denominator (rowvar) \cr \tab covar \tab covariance of estimated proportion
#' of numerator (rowvar) and denominator (colvar) \cr \tab rhat \tab ratio of
#' estimated proportions (numerator-colvar / denominator-rowvar) \cr \tab
#' rhat.var \tab variance of ratio of estimated proportions \cr \tab rhat.se
#' \tab standard error of ratio of estimated proportions sqrt(rhat.var) \cr
#' \tab rhat.cv \tab coefficient of variation of ratio of estimated proportions
#' rhat.se/rhat \cr \tab areavar \tab If tabtype='AREA', total area for
#' estimation unit\cr \tab est \tab If tabtype='AREA', estimated area of land
#' rhat*areavar If tabtype='PCT', estimated percent cover of land rhat*100 \cr
#' \tab est.var \tab variance of estimated area of land rhat.var*areavar \cr If
#' tabtype='PCT', estimated percent cover of land rhat.var*100 \cr \tab est.se
#' \tab standard error of estimated area or percent cover sqrt(est.var) \cr
#' \tab est.cv \tab coefficient of variance of estimated area or percent cover
#' est.se/est \cr \tab est.pse \tab percent sampling error of estimated area of
#' percent cover est.cv*100 \cr \tab CI99left \tab left tail of 99 percent
#' confidence interval for estimated area \cr \tab CI99right \tab right tail of
#' 99 percent confidence interval for estimated area \cr \tab CI95left \tab
#' left tail of 95 percent confidence interval for estimated area \cr \tab
#' CI95right \tab right tail of 95 percent confidence interval for estimated
#' area \cr \tab CI67left \tab left tail of 67 percent confidence interval for
#' estimated area \cr \tab CI67right \tab right tail of 67 percent confidence
#' interval for estimated area \cr } }
#' @note
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
#' @references Frescino, Tracey S.; Moisen, Gretchen G.; Megown, Kevin A.;
#' Nelson, Val J.; Freeman, Elizabeth A.; Patterson, Paul L.; Finco, Mark;
#' Brewer, Ken; Menlove, James 2009.  Nevada Photo-Based Inventory Pilot (NPIP)
#' photo sampling procedures. Gen. Tech. Rep.  RMRS-GTR-222. Fort Collins, CO:
#' U.S. Department of Agriculture, Forest Service, Rocky Mountain Research
#' Station. 30 p.
#' 
#' Patterson, Paul L. 2012. Photo-based estimators for the Nevada photo-based
#' inventory.  Res. Pap. RMRS-RP-92. Fort Collins, CO: U.S. Department of
#' Agriculture, Forest Service, Rocky Mountain Research Station. 14 p.
#' @keywords data
#' @examples 
#' \donttest{
#' # Load necessary data from FIESTA
#' ## Point data
#' icepntfn <- system.file("extdata",
#'                         "PB_data/icepnt_utco1135.csv",
#'                          package = "FIESTA")
#' icepnt <- read.csv(icepntfn)
#' 
#' ## Plot data
#' icepltfn <- system.file("extdata",
#'                         "PB_data/icepltassgn_utco1135.csv",
#'                          package = "FIESTA")
#' iceplt <- read.csv(icepltfn)
#' 
#' ## County data
#' unitareafn <- system.file("extdata", 
#'                           "PB_data/unitarea_utco1135.csv",
#'                            package = "FIESTA")
#' unitarea <- read.csv(unitareafn)
#' 
#' ## ICE Cover
#' icecoverfn <- system.file("extdata",
#'                           "PB_data/cover_LUT.csv",
#'                            package = "FIESTA")
#' icecover <- read.csv(icecoverfn)
#' names(icecover) <- sub("cover", "cover_1", names(icecover))
#' 
#' # Set up population data (see ?modPBpop() for more information)
#' PBpopunit <- modPBpop(pnt = icepnt, 
#'                       pltassgn = iceplt, 
#'                       pltassgnid = "plot_id", 
#'                       pntid = "dot_cnt",
#'                       unitarea = unitarea, 
#'                       unitvar = "ESTN_UNIT")
#' # Photo-based estimation with point-level data by estimation unit (county)                       
#' ## Without summing units
#' cover1.unit.area <- modPB(
#'   PBpopdat = PBpopunit,
#'   tabtype = "AREA",
#'   rowvar = "cover_1",
#'   nonsamp.pntfilter = "cover_1 != 999",
#'   table_opts = list(rowlut = icecover),
#'   title_opts = list(title.rowvar = "Land Cover (2011)")
#' ) 
#' 
#' cover1.unit.area$est
#'                           
#' ## With summing units
#' cover1.unit.area.sum <- modPB(
#'   PBpopdat = PBpopunit,
#'   tabtype = "AREA",
#'   rowvar = "cover_1",
#'   nonsamp.pntfilter = "cover_1 != 999",
#'   sumunits = TRUE,
#'   table_opts = list(rowlut = icecover),
#'   title_opts = list(title.rowvar = "Land Cover (2011)")
#' )
#' 
#' cover1.unit.area.sum$est   
#' }          
#' @export modPB
modPB <- function(PBpopdat = NULL, 
                  tabtype = "PCT", 
                  sumunits =FALSE, 
                  ratio = FALSE, 
                  landarea = "ALL", 
                  landarea.filter = NULL, 
                  nonsamp.pntfilter = NULL, 
                  pntfilter = NULL, 
                  pfilter = NULL, 
                  rowvar = NULL, 
                  colvar = NULL, 
                  domlut = NULL, 
                  domvarlst = NULL, 
                  ratioden = "ROWVAR",
                  gainloss = FALSE, 
                  gainloss.vals = NULL, 
                  addtitle = FALSE, 
                  returntitle = FALSE,
                  savedata = FALSE,
                  table_opts = NULL,
                  title_opts = NULL,
                  savedata_opts = NULL,
                  gui = FALSE, 
                  ...){
  ###################################################################################
  ## DESCRIPTION: 
  ## Generates percent or acre estimates by domain (and estimation unit)
  ###################################################################################

  ## CHECK GUI - IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  if (nargs() == 0 && is.null(PBpopdat)) {
    gui <- TRUE
  } 

  ## If gui.. set variables to NULL
  if (gui) {
    pntid=plotid=puniqueid=landarea=strvar=areavar=PBvars2keep <- NULL
  }

  ## Set parameters
  minplotnum <- 10
  parameters <- FALSE
  returnlst <- list()
  rawdata <- TRUE
  
  ## Set global variables
  TOTAL=ONEUNIT=n.total=n.strata=strwt=NBRPNTS=psq.pltdom=
    uniqueid=p.pltdom.n=nbrpts.pltdom.n=PtsPerPlot=nbrpts.pltdom=
    value=p.pltdom=PBvars2keep=title.est=title.pse=title.estpse=
    outfn.estpse <- NULL
  

  ##################################################################
  ## CHECK PARAMETER NAMES
  ##################################################################
  
  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- names(formals(modPB))
  if (!all(input.params %in% formallst)) {
    miss <- input.params[!input.params %in% formallst]
    stop("invalid parameter: ", toString(miss))
  }
  
  ## Check parameter lists
  pcheck.params(input.params, table_opts=table_opts, title_opts=title_opts, 
                savedata_opts=savedata_opts)
  
  
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
  
  ##################################################################
  ## CHECK PARAMETER INPUTS
  ##################################################################
  if (!is.list(PBpopdat))
    stop("PBpopdat must be a list")
  listitems <- c("PBx", "plotid", "pntid", "getprop")
  if (!all(listitems %in% names(PBpopdat))) {
    items.miss <- listitems[!listitems %in% names(PBpopdat)]
    stop("invalid PBpopdat... missing items: ", paste(items.miss, collapse=", "))
  }	
  PBx <- PBpopdat$PBx
  pltassgnx <- PBpopdat$pltassgnx
  plotid <- PBpopdat$plotid
  pntid <- PBpopdat$pntid
  pltassgnid <- PBpopdat$pltassgnid
  unitvar <- PBpopdat$unitvar
  unitvar2 <- PBpopdat$unitvar2
  unitvars <- PBpopdat$unitvars
  unitarea <- PBpopdat$unitarea
  areavar <- PBpopdat$areavar
  areaunits <- PBpopdat$areaunits
  strata <- PBpopdat$strata
  strtype <- PBpopdat$strtype
  stratalut <- PBpopdat$stratalut
  strvar <- PBpopdat$strvar
  strwtvar <- PBpopdat$strwtvar
  plotsampcnt <- PBpopdat$plotsampcnt
  stratcombinelut <- PBpopdat$stratcombinelut
  getprop <- PBpopdat$getprop
  if (!getprop) {
    rowvar <- PBpopdat$rowvar
    PBvars2keep <- "p.pltdom"
  }
  unitvars <- c(unitvar, unitvar2)
  if (strata) {
    strtype <- PBpopdat$strtype
  }
  strunitvars <- c(unitvar, strvar)
  
  ########################################
  ## Check area units
  ########################################
  if (!is.null(unitarea)) {
    unitchk <- pcheck.areaunits(unitarea=unitarea, areavar=areavar, 
                              areaunits=areaunits, metric=metric)
    unitarea <- unitchk$unitarea
    areavar <- unitchk$areavar
    areaunits <- unitchk$outunits

    if (is.null(key(unitarea))) {
      setkeyv(unitarea, unitvar)
    }
  }

  ###################################################################################
  ## Check parameters and apply plot and pnt filters
  ###################################################################################
  estdat <- check.estdataPB(PBx=PBx, plotid=plotid, pntid=pntid, 
                  tabtype=tabtype, ratio=ratio, pfilter=pfilter, 
                  nonsamp.pntfilter=nonsamp.pntfilter, 
                  landarea=landarea, landarea.filter=landarea.filter, 
                  pntfilter=pntfilter, sumunits=sumunits, 
                  allin1=allin1, estround=estround, pseround=pseround, 
                  divideby=divideby, addtitle=addtitle, returntitle=returntitle, 
                  rawdata=rawdata, rawonly=rawonly, gainloss=gainloss,
                  savedata=savedata, outfolder=outfolder, 
                  overwrite_dsn=overwrite_dsn, overwrite_layer=overwrite_layer, 
                  outfn.pre=outfn.pre, outfn.date=outfn.date, append_layer=append_layer, 
                  raw_fmt=raw_fmt, raw_dsn=raw_dsn, gui=gui)
  if (is.null(estdat)) return(NULL)
  PBx <- estdat$PBf	
  plotid <- estdat$plotid
  pntid <- estdat$pntid
  filterids <- estdat$filterids
  sumunits <- estdat$sumunits
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
  tabtype <- estdat$tabtype
  ratio <- estdat$ratio
  gainloss <- estdat$gainloss

  if (tabtype == "AREA" && is.null(unitarea)) {
    stop("must include unitarea in population data for area table")
  }

  
  #################################################################################
  ### GET ROW AND COLUMN INFO
  #################################################################################
  #if (!is.null(domlut)) domlut <- setDF(domlut) 
  rowcolinfo <- check.rowcolPB(gui=gui, ratio=ratio, PBx=PBx, 
                    plotid=plotid, pntid=pntid, 
                    rowvar=rowvar, colvar=colvar, 
                    row.orderby=row.orderby, col.orderby=col.orderby, 
                    domvarlst=domvarlst, domlut=domlut, 
                    title.rowvar=title.rowvar, title.colvar=title.colvar, 
                    filterids=filterids, row.add0=row.add0, col.add0=col.add0, 
                    rowlut=rowlut, collut=collut, PBvars2keep=PBvars2keep)
  PBx <- rowcolinfo$PBx
  setkeyv(PBx, c(plotid, pntid))
  uniquerow <- rowcolinfo$uniquerow
  uniquecol <- rowcolinfo$uniquecol
  domainlst <- rowcolinfo$domainlst
  rowvar <- rowcolinfo$rowvar
  colvar <- rowcolinfo$colvar
  row.orderby <- rowcolinfo$row.orderby
  col.orderby <- rowcolinfo$col.orderby
  title.rowvar <- rowcolinfo$title.rowvar
  title.colvar <- rowcolinfo$title.colvar 
  row.add0 <- rowcolinfo$row.add0
  col.add0 <- rowcolinfo$col.add0
  if (ratio) {
    PBx.d <- rowcolinfo$PBx.d
  }


  ###################################################################################
  ## MERGE FILTERED DATA TO ALL PLOTS
  ###################################################################################
  tabs <- check.matchclass(PBx, pltassgnx, plotid, pltassgnid,
			tab1txt="pnt", tab2txt="pltassgn")

  PBx <- tabs$tab1
  pltassgnx <- tabs$tab2
  PBall <- merge(PBx, pltassgnx, by.x=plotid, by.y=pltassgnid, all.x=TRUE)

  if (ratio) {
    tabs <- check.matchclass(PBx.d, pltassgnx, plotid, pltassgnid,
			tab1txt="pnt.d", tab2txt="pltassgn")
    PBx.d <- tabs$tab1
    pltassgnx <- tabs$tab2
    PBall.d <- merge(PBx.d, pltassgnx, by.x=plotid, by.y=pltassgnid, all.x=TRUE)
  }

  ###################################################################################
  ### GET TITLES FOR OUTPUT TABLES
  ###################################################################################
  phototype <- ifelse(ratio, tolower(paste("ratio", tabtype, sep="_")), 
		tolower(paste("nratio", tabtype, sep="_")))
  pcfilter <- NULL
  if (!is.null(pfilter)) {
    pcfilter <- pfilter
    if (!is.null(pntfilter)) {
      pcfilter <- paste(pcfilter, "and", pntfilter)
    } 
  } else if (!is.null(pntfilter)) {
    pcfilter <- pntfilter
  }
  alltitlelst <- check.titles(dat=PBall, esttype="PHOTO", phototype=phototype, 
                      tabtype=tabtype, sumunits=sumunits, title.main=title.main, 
                      title.ref=title.ref, 
                      title.rowvar=title.rowvar, title.colvar=title.colvar, 
                      title.unitvar=title.unitvar, title.filter=title.filter, 
                      title.unitsn=areaunits, unitvar=unitvar, 
                      rowvar=rowvar, colvar=colvar, addtitle=addtitle, 
                      returntitle=returntitle, rawdata=rawdata, landarea=landarea, 
                      pcfilter=pcfilter, allin1=allin1, divideby=divideby, 
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


  ###########################################################
  ## DO WORK
  ###########################################################

  ## Define columns
  estnm <- "est"
  psenm <- "pse"

  ####################################################################### 
  ## Get proportion of points by domain and plot and number of points
  #######################################################################
  pltdom.tot=pltdom.row=pltdom.col=pltdom.grp <- NULL
  totvar <- "TOTAL"

  if (getprop) {

    if (!ratio) {
      if (totvar %in% names(PBall)) {
        pltdom.tot <- getpltdom.prop(PBall, uniqueid=plotid, domain="TOTAL", strunitvars)

        totest.pntcnt <- pltdom.tot[get(totvar) != "NOTinDOMAIN", 
			list(NBRPNTS=sum(nbrpts.pltdom)), by=c(unitvar, totvar)]
      }
      if (rowvar != "Total") {
        pltdom.row <- getpltdom.prop(PBall, uniqueid=plotid, domain=rowvar, strunitvars)
        rowest.pntcnt <- pltdom.row[get(rowvar) != "NOTinDOMAIN", 
                list(NBRPNTS=sum(nbrpts.pltdom)), by=c(unitvar, rowvar)]
        setkeyv(rowest.pntcnt, c(unitvar, rowvar))

        ## for kelly
        #tpltdom.row <- FIESTA::transpose2col(pltdom.row, plotid, tvar=rowvar,
        #		   value.var="p.pltdom")

      } 
 
      if (colvar != "NONE") {
        grpvar <- c(rowvar, colvar)
        pltdom.col <- getpltdom.prop(PBall, uniqueid=plotid, domain=colvar, strunitvars)
        pltdom.grp <- getpltdom.prop(PBall, uniqueid=plotid, domain=grpvar, strunitvars)
 
        colest.pntcnt <- pltdom.col[get(colvar) != "NOTinDOMAIN", 
        list(NBRPNTS=sum(nbrpts.pltdom)), by=c(unitvar, colvar)]
        setkeyv(colest.pntcnt, c(unitvar, colvar)) 

        grpest.pntcnt <- pltdom.grp[(get(rowvar) != "NOTinDOMAIN" & get(colvar) != "NOTinDOMAIN"), 
                list(NBRPNTS=sum(nbrpts.pltdom)), by=c(unitvar, grpvar)]
        setkeyv(grpest.pntcnt, c(unitvar, grpvar)) 
      }

    } else {

      domain <- ifelse(ratioden == "ROWVAR", rowvar, colvar)
      attribute <- ifelse(ratioden == "ROWVAR", colvar, rowvar)
      grpvar <- c(domain, attribute)

      ## Get proportion of points for domain (denominator) by plot
      chgnames <- c("nbrpts.pltdom", "p.pltdom")
      pltdom.d <- getpltdom.prop(PBall.d, uniqueid=plotid, domain=domain, strunitvars)
      setnames(pltdom.d, chgnames, paste0(chgnames, ".d")) 
      setkeyv(pltdom.d, c(strunitvars, plotid))

      ## Get proportion of points for domain and attribute (numerator) by plot
      chgnames <- c("nbrpts.pltdom", "p.pltdom")
      pltdom.n <- getpltdom.prop(PBall, uniqueid=plotid, domain=c(domain, attribute),
 			strunitvars)
      setnames(pltdom.n, chgnames, paste0(chgnames, ".n")) 
      setkeyv(pltdom.n, c(strunitvars, plotid))

      ## Check if class of join columns in pltdom.n matches pltdom.d
      tabs <- check.matchclass(pltdom.d, pltdom.n, 
			c(strunitvars, plotid, domain))
      pltdom.d <- tabs$tab1
      pltdom.n <- tabs$tab2

      grpest.pntcnt <- pltdom.n[get(attribute) != "NOTinDOMAIN", 
			list(NBRPNTS=sum(nbrpts.pltdom.n)), by=c(unitvar, domain, attribute)]
      setkeyv(grpest.pntcnt, c(unitvar, grpvar)) 
    }

  } else {
    pltdom.row <- PBall
  }
 
  ####################################################################### 
  ## GENERATE ESTIMATES
  ####################################################################### 
  unit_totest=unit_rowest=unit_colest=unit_grpest=rowunit=totunit=
	unit_totest.str=unit_rowest.str=unit_colest.str=unit_grpest.str <- NULL

  if (!ratio) {
    phatcol <- "phat"
    phatcol.var <- "phat.var"

    if (!is.null(pltdom.tot)) {
      ## Get estimate for TOTAL
      #######################################################################
      pbar.totest <- PBest.pbar(dom.prop=pltdom.tot, uniqueid=plotid, 
                                domain=totvar, strtype="post", 
                                stratalut=stratalut, strunitvars=strunitvars, 
                                unitvars=unitvar, strvar=strvar)
      unit_totest <- pbar.totest$est.unit
      if (rawdata) unit_totest.str <- pbar.totest$ybardat
      setkeyv(unit_totest, c(unitvar, totvar))

      ## Merge NBRPNTS
      tabs <- check.matchclass(unit_totest, totest.pntcnt, c(unitvar, totvar))
      unit_totest <- tabs$tab1
      totest.pntcnt <- tabs$tab2
      unit_totest <- unit_totest[totest.pntcnt, nomatch=0]
    
      ## Add acres (tabtype="AREA") or round values (tabtype="PCT")
      if (tabtype == "AREA" || sumunits) {
        tabs <- check.matchclass(unitarea, unit_totest, unitvar)
        unitarea <- tabs$tab1
        unit_totest <- tabs$tab2
        setkeyv(unit_totest, unitvar)
        unit_totest <- unit_totest[unitarea, nomatch=0]
        unit_totest <- PBgetest(unit_totest, areavar, phatcol=phatcol, phatcol.var=phatcol.var)
      } else {
        unit_totest <- PBgetest(unit_totest, phatcol=phatcol, phatcol.var=phatcol.var)
      }

      ## Remove rows that are filtered out (NOTinDOMAIN or 9999)
      ## Remove rows that are filtered out (NOTinDOMAIN or 9999)
      tot.filterval <- ifelse (is.numeric(unit_totest[[totvar]]), 9999, "NOTinDOMAIN")
      unit_totest <- unit_totest[get(eval(totvar)) != tot.filterval,]
    }

    ## Get estimate for row and columns
    #######################################################################
    if (rowvar != "TOTAL") {
      pbar.rowest <- PBest.pbar(dom.prop=pltdom.row, uniqueid=plotid, 
                    domain=rowvar, strtype="post", 
                    stratalut=stratalut, strunitvars=strunitvars, 
                    unitvars=unitvar, strvar=strvar)
      unit_rowest <- pbar.rowest$est.unit
      if (rawdata) {
        unit_rowest.str <- pbar.rowest$ybardat
      }
      setkeyv(unit_rowest, c(unitvar, rowvar))

      ## Get filter value for row and column
      row.filterval <- ifelse (is.numeric(unit_rowest[[rowvar]]), 9999, "NOTinDOMAIN")
    }
   
    ## Get column (and cell) estimate  
    if (colvar != "NONE") {
      pbar.colest <- PBest.pbar(dom.prop=pltdom.col, uniqueid=puniqueid, 
                  domain=colvar, strtype="post", 
                  stratalut=stratalut, strunitvars=strunitvars, 
                  unitvars=unitvar, strvar=strvar)
      unit_colest <- pbar.colest$est.unit
      if (rawdata) { 
        unit_colest.str <- pbar.colest$ybardat
      }
      setkeyv(unit_colest, c(unitvar, colvar))

      ## Get filter value for column
      col.filterval <- ifelse (is.numeric(unit_colest[[colvar]]), 9999, "NOTinDOMAIN")

      pbar.grpest <- PBest.pbar(dom.prop=pltdom.grp, uniqueid=puniqueid, 
                            domain=grpvar, strtype="post", 
                            stratalut=stratalut, strunitvars=strunitvars, 
                            unitvars=unitvar, strvar=strvar)
      unit_grpest <- pbar.grpest$est.unit
      if (rawdata) {
        unit_grpest.str <- pbar.grpest$ybardat
      }
      setkeyv(unit_grpest, c(unitvar, grpvar))
    }
  } else {  ## ratio=TRUE

    phatcol <- "rhat"
    phatcol.var <- "rhat.var"

    unit_grpest <- PBest.pbarRatio(dom.prop.n=pltdom.n, dom.prop.d=pltdom.d, 
                          uniqueid=plotid, domain=domain, attribute=attribute, 
                          stratalut=stratalut, strunitvars=strunitvars, 
                          unitvars=unitvar, strvar=strvar)
    setkeyv(unit_grpest, c(unitvar, grpvar))

    if (tabtype == "AREA") {
      pltdom <- pltdom.n
      names(pltdom) <- sub("\\.n", "", names(pltdom.n))
      unit_grpest.domtot <- PBest.pbar(dom.prop=pltdom, 
                              uniqueid=plotid, domain=domain, 
                              stratalut=stratalut, strunitvars=strunitvars, 
                              unitvars=unitvar, strvar=strvar, strtype="post")$est.unit
      setkeyv(unit_grpest.domtot, c(unitvar, domain))
    }

    ## Get filter value for row and column
    row.filterval <- ifelse (is.numeric(unit_rowest[[rowvar]]), 9999, "NOTinDOMAIN")
    col.filterval <- ifelse (is.numeric(unit_colest[[colvar]]), 9999, "NOTinDOMAIN")
  }

  ###################################################################################
  ## Check add0 and Add acres
  ###################################################################################
  if (!is.null(unit_rowest)) {
    if (getprop) {
      ## Merge number of points
      tabs <- check.matchclass(unit_rowest, rowest.pntcnt, c(unitvar, rowvar))
      unit_rowest <- tabs$tab1
      rowest.pntcnt <- tabs$tab2
      unit_rowest <- unit_rowest[rowest.pntcnt, nomatch=0]
    }

    ## Remove rows that are filtered out (NOTinDOMAIN or 9999)
    unit_rowest <- unit_rowest[get(eval(rowvar)) != row.filterval,]

    ## Merge uniquerow
    unit_rowest <- add0unit(x=unit_rowest, xvar=rowvar, uniquex=uniquerow, 
		unitvar=unitvar, xvar.add0=row.add0)

    ## Add acres (tabtype="AREA") or round values (tabtype="PCT")
    if (!is.null(unit_rowest) && !ratio) {
      if (tabtype == "AREA" || sumunits) {
        tabs <- check.matchclass(unitarea, unit_rowest, unitvar)
        unitarea <- tabs$tab1
        unit_rowest <- tabs$tab2
        setkeyv(unit_rowest, unitvar)
        unit_rowest <- unit_rowest[unitarea, nomatch=0]
        unit_rowest <- PBgetest(unit_rowest, areavar, phatcol=phatcol, phatcol.var=phatcol.var)
        setkeyv(unit_rowest, c(unitvar, rowvar))
      } else {
        unit_rowest <- PBgetest(xdat=copy(unit_rowest), phatcol=phatcol, phatcol.var=phatcol.var)
      }
    }
  }

  if (!is.null(unit_colest) && !ratio) {
    ## Merge number of points
    tabs <- check.matchclass(unit_colest, colest.pntcnt, c(unitvar, colvar))
    unit_colest <- tabs$tab1
    colest.pntcnt <- tabs$tab2
    unit_colest <- unit_colest[colest.pntcnt, nomatch=0]

    ## Remove rows that are filtered out (NOTinDOMAIN or 9999)
    unit_colest <- unit_colest[get(eval(colvar)) != col.filterval,]

    ## Merge uniquecol
    unit_colest <- add0unit(x=unit_colest, xvar=colvar, uniquex=uniquecol, 
		unitvar=unitvar, xvar.add0=col.add0)

    ## Add acres (tabtype="AREA") or round values (tabtype="PCT")
    if (tabtype == "AREA" || sumunits) {
      tabs <- check.matchclass(unitarea, unit_colest, unitvar)
      unitarea <- tabs$tab1
      unit_colest <- tabs$tab2
      setkeyv(unit_colest, unitvar)
      unit_colest <- unit_colest[unitarea, nomatch=0]
      unit_colest <- PBgetest(unit_colest, areavar, phatcol=phatcol, phatcol.var=phatcol.var)
      setkeyv(unit_colest, c(unitvar, colvar))
    } else {
      unit_colest <- PBgetest(xdat=copy(unit_colest), phatcol=phatcol, phatcol.var=phatcol.var)
    }
  }
  if (!is.null(unit_grpest)) {
    unit_grpest <- add0unit(x=unit_grpest, xvar=rowvar, uniquex=uniquerow, 
		unitvar=unitvar, xvar.add0=row.add0, xvar2=colvar, uniquex2=uniquecol,
		xvar2.add0=col.add0)

    if (tabtype == "AREA" || sumunits) {
      tabs <- check.matchclass(unitarea, unit_grpest, unitvar)
      unitarea <- tabs$tab1
      unit_grpest <- tabs$tab2
      setkeyv(unit_grpest, unitvar)
      unit_grpest <- unit_grpest[unitarea, nomatch=0]
      unit_colest <- PBgetest(unit_grpest, areavar, phatcol=phatcol, phatcol.var=phatcol.var)
      setkeyv(unit_grpest, c(unitvar, rowvar, colvar))
    } else {
      unit_grpest <- PBgetest(xdat=copy(unit_grpest), phatcol=phatcol, phatcol.var=phatcol.var)
    }
  }
        
  if (!sumunits && length(unique(stratalut[[unitvar]])) > 1 && !ratio && tabtype != "PCT") {
    ## AGGREGATE UNIT stratalut FOR ROWVAR and GRAND TOTAL
    stratalut2 <- data.table(stratalut, ONEUNIT=1)
    strunitvars2 <- c("ONEUNIT", strvar)
    stratalut2 <- stratalut2[, lapply(.SD, sum, na.rm=TRUE), 
		by=strunitvars2, .SDcols=c(strwtvar, "n.strata")]
    stratalut2[, strwt:=prop.table(get(strwtvar)), by="ONEUNIT"]
    stratalut2[, n.total := sum(n.strata)]
    setkeyv(stratalut2, strunitvars2)

    if (!is.null(unitarea)) {
      unitarea2 <- data.table(unitarea, ONEUNIT=1)
      unitarea2 <- unitarea2[, lapply(.SD, sum, na.rm=TRUE), by="ONEUNIT", 
		.SDcols=areavar]
      setkey(unitarea2, "ONEUNIT")
    }

    PBall[, ONEUNIT := 1]

    ## CALCULATE UNIT TOTALS FOR ROWVAR
    pltdom.prop <- getpltdom.prop(PBall, uniqueid=plotid, domain=rowvar, strunitvars2)
    rowunit <- PBest.pbar(dom.prop=pltdom.prop, uniqueid=plotid, 
                    domain=rowvar, strtype="post", 
                    stratalut=stratalut2, strunitvars=strunitvars2, 
                    unitvars="ONEUNIT", strvar=strvar)$est.unit
    rowunit <- add0unit(x=rowunit, xvar=rowvar, uniquex=uniquerow, 
		unitvar="ONEUNIT", xvar.add0=row.add0)
    ## Add acres (tabtype="AREA") or round values (tabtype="PCT")
    if (tabtype == "AREA") {
      tabs <- check.matchclass(rowunit, unitarea2, "ONEUNIT")
      rowunit <- tabs$tab1
      unitarea2 <- tabs$tab2
      setkeyv(rowunit, "ONEUNIT")
      rowunit <- rowunit[unitarea2, nomatch=0]
      rowunit <- PBgetest(rowunit, areavar, phatcol=phatcol, phatcol.var=phatcol.var)
      setkeyv(rowunit, c("ONEUNIT", rowvar))

    } else {
      rowunit <- PBgetest(rowunit, phatcol=phatcol, phatcol.var=phatcol.var)
    }  

    ## CALCULATE GRAND TOTAL FOR ALL UNITS
    pltdom.prop <- getpltdom.prop(PBall, uniqueid=plotid, domain="TOTAL", 
		strunitvars2)
    totunit <- PBest.pbar(dom.prop=pltdom.prop, uniqueid=plotid, 
                    domain="TOTAL", strtype="post", 
                    stratalut=stratalut2, strunitvars=strunitvars2, 
                    unitvars="ONEUNIT", strvar=strvar)$est.unit

    ## Add acres (tabtype="AREA") or round values (tabtype="PCT")
    tabs <- check.matchclass(totunit, unitarea2, "ONEUNIT")
    totunit <- tabs$tab1
    unitarea2 <- tabs$tab2
    setkeyv(totunit, "ONEUNIT")
    if (tabtype == "AREA") {
      totunit <- totunit[unitarea2, nomatch=0]
      totunit <- PBgetest(totunit, areavar, phatcol=phatcol, phatcol.var=phatcol.var)
    } else {
      PBgetest(totunit, phatcol=phatcol, phatcol.var=phatcol.var)
    }
   # totunit[, TOTAL:= "Total"]
   # setnames(totunit, names(totunit), names(rowunit))
  }          

  ###################################################################
  ## GENERATE OUTPUT TABLES
  ###################################################################
  CI <- TRUE
  estnm <- "est"
  tabs <- est.outtabs(esttype="PHOTO", phototype=tabtype, photoratio=ratio, 
                sumunits=sumunits, areavar=areavar, 
                unitvar=unitvar, unitvars=unitvars, unit_totest=unit_totest, 
                unit_rowest=unit_rowest, unit_colest=unit_colest, unit_grpest=unit_grpest, 
                rowvar=rowvar, colvar=colvar, 
                uniquerow=uniquerow, uniquecol=uniquecol, 
                rowunit=rowunit, totunit=totunit, allin1=allin1, 
                savedata=savedata, addtitle=addtitle, title.ref=title.ref, 
                title.rowvar=title.rowvar, title.colvar=title.colvar, 
                title.unitvar=title.unitvar, title.estpse=title.estpse, 
                title.est=title.est, title.pse=title.pse, rawdata=rawdata, 
                rawonly=rawonly, outfn.estpse=outfn.estpse, outfolder=outfolder, 
                outfn.date=outfn.date, overwrite=overwrite_layer, estnm=estnm, 
                estround=estround, pseround=pseround, estnull=estnull, psenull=psenull, 
                divideby=divideby, CI=CI) 

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
    rawdat$module <- "PB"
    rawdat$esttype <- ifelse(ratio, "RATIO", "AREA")
    rawdat$PBmethod <- ifelse(strata, "PS", "HT")
    rawdat$strtype <- strtype

    if (!is.null(rowvar) && rowvar != "NONE") {
      rawdat$rowvar <- rowvar
      rawdat$pltdom.row <- pltdom.row
    }
    if (!is.null(colvar) && colvar != "NONE") {
      rawdat$colvar <- colvar 
      rawdat$pltdom.col <- pltdom.col
      rawdat$pltdom.grp <- pltdom.grp
    }
    if (!is.null(unitarea)) {
      rawdat$areaunits <- areaunits
    }
    ## Generate sample counts by attribute
#    if (is.null(sampcnt)) {
#      byvars <- c(unitvar, rowvar)
#      if (colvar != "NONE") byvars <- c(byvars, colvar)
#      sampcnt <- PBall[, .N, by=byvars]
#      setnames(sampcnt, "N", "NBRPNTS")
#      setorderv(sampcnt, byvars)
#    }

    if (!is.null(plotsampcnt)) {

      if (!is.null(row.orderby)) {
        mergevar.row <- ifelse (rowvar %in% names(plotsampcnt), rowvar, row.orderby)
      } else {
        mergevar.row <- rowvar
      }
      if (colvar != "NONE") {
        if (col.orderby != "NONE") {
          mergevar.col <- ifelse (colvar %in% names(plotsampcnt), colvar, col.orderby)
        } else {
          mergevar.col <- colvar
        }

        if (ncol(uniquerow) > 1 && ncol(uniquecol) > 1) {
          if (unitvar %in% names(plotsampcnt)) {
            plotsampcnt <- add0unit(x=plotsampcnt, xvar=mergevar.row, 
                                uniquex=uniquerow, unitvar=unitvar, xvar2=mergevar.col, 
                                uniquex2=uniquecol, xvar.add0=row.add0)
          } else {
            plotsampcnt <- add0unit(x=plotsampcnt, xvar=mergevar.row, 
                                uniquex=uniquerow, xvar2=mergevar.col, 
                                uniquex2=uniquecol, xvar.add0=row.add0)
          }
        } else if (ncol(uniquerow) > 1) {
          plotsampcnt <- add0unit(x=plotsampcnt, xvar=mergevar.row, 
                               uniquex=uniquerow, xvar.add0=row.add0)
        } else if (ncol(uniquecol) > 1) {
          plotsampcnt <- add0unit(x=plotsampcnt, xvar=mergevar.col, 
                              uniquex=uniquecol, xvar.add0=col.add0)
        }

      } else {
 
        if (!is.null(uniquerow) && ncol(uniquerow) > 1) {
          xvar <- ifelse (rowvar %in% names(plotsampcnt), rowvar, row.orderby)
          plotsampcnt <- add0unit(x=plotsampcnt, xvar=xvar, 
                              uniquex=uniquerow, xvar.add0=row.add0)
        }
      }
      rawdat$plotsampcnt <- setDF(plotsampcnt)
    }

    if (!is.null(pltdom.row)) { 
      if ("psq.pltdom" %in% names(pltdom.row)) {
        pltdom.row[, psq.pltdom := NULL]
        rawdat$pltdom.row <- pltdom.row 
      }
    }
    if (!is.null(pltdom.col)) { 
      if ("psq.pltdom" %in% names(pltdom.col)) {
        pltdom.col[, psq.pltdom := NULL]
        rawdat$pltdom.col <- pltdom.col 
      }
    }
    if (!is.null(pltdom.grp)) { 
      if ("psq.pltdom" %in% names(pltdom.grp)) {
        pltdom.grp[, psq.pltdom := NULL]
        rawdat$pltdom.grp <- pltdom.grp 
      }
    }

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
    if (!ratio) {
      if (!is.null(unit_rowest.str)) {
        rawdat$unit_rowest.str <- unit_rowest.str
      }
      if (!is.null(unit_colest.str)) {
        rawdat$unit_colest.str <- unit_colest.str
      }
      if (!is.null(unit_grpest.str)) {
        rawdat$unit_grpest.str <- unit_grpest.str
      }
    }
  }
 
  ## GAIN/LOSS
  if (gainloss) {

    if (is.null(rowvar) || is.null(colvar)) {
      stop("must have rowvar and colvar to calculate gain/loss") 
    }
    ## Check
    rowcolvals <- unique(c(pltdom.grp[[rowvar]], pltdom.grp[[colvar]]))

    if (is.null(gainloss.vals)) {
      gainloss.vals <- rowcolvals
    } else {
      if (any(!gainloss.vals %in% rowcolvals)) {
       valsnotin <- gainloss.vals[which(!gainloss.vals %in% rowcolvals)]
       stop(paste("invalid gainloss.vals.. ", paste(valsnotin, collapse=", "), 
		"not in data"))
      }
    }
 
    numvars <- c("gain.est", "gain.se", "loss.est", "loss.se", "diff.est", "diff.se")
    charvars <- c(unitvars, "gain.val", "loss.val")

    if (length(rowcolvals) == 2) {
      est.gainloss <- data.frame(t(sapply(gainloss.vals, getgainloss, 
                            pltdom.grp, plotid, rowvar, colvar, 
                            stratalut, unitvars, strvar, 
                            tabtype, areavar, unitarea, sumunits)))
    } else {
      est.gainloss <- data.frame(t(sapply(gainloss.vals, getgainloss, 
                            pltdom.grp, plotid, rowvar, colvar, 
                            stratalut, unitvars, strvar, 
                            tabtype, areavar, unitarea, sumunits)))
    }
    est.gainloss[, numvars] <- lapply(est.gainloss[, numvars], as.numeric)
    est.gainloss[, charvars] <- lapply(est.gainloss[, charvars], as.character)

    ## Add 95 and 68% confidence intervals for gain.est, loss.est, diff.est
    CInames <- c("CI95left", "CI95right", "CI68left", "CI68right")

    est.gainloss <- addCI(est.gainloss, estnm="gain.est", 
		senm="gain.se", gainloss=gainloss)
    setnames(est.gainloss, CInames, paste0("gain.", CInames))

    est.gainloss <- addCI(est.gainloss, estnm="loss.est", 
		senm="loss.se", gainloss=gainloss)
    setnames(est.gainloss, CInames, paste0("loss.", CInames))

    est.gainloss <- addCI(est.gainloss, estnm="diff.est", 
		senm="diff.se", gainloss=gainloss)
    setnames(est.gainloss, CInames, paste0("diff.", CInames))

    ## Add to return list
    rawdat$est.gainloss <- est.gainloss

    if (savedata) {
      out_layer <- paste(outfn.rawdat, "gainloss", sep="_")
      
      datExportData(est.gainloss, 
            savedata_opts=list(outfolder=rawfolder, 
                                out_fmt=raw_fmt, 
                                out_dsn=raw_dsn, 
                                out_layer=out_layer,
                                overwrite_layer=overwrite_layer,
                                append_layer=append_layer,
                                add_layer=TRUE))
    }          
  }
  
  if (rawdata) {
    returnlst$raw <- rawdat
  }
  return(returnlst)
}

