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
#' @param ratiotype String. The type of ratio estimates ("PERACRE", "PERTREE").
#' @param woodland String. If woodland = 'Y', include woodland tree species  
#' where measured. If woodland = 'N', only include timber species. See 
#' FIESTA::ref_species$WOODLAND ='Y/N'. If woodland = 'only', only include
#' woodland species.
#' @param landarea String. The sample area filter for estimates ("FOREST",
#' "TIMBERLAND").  If landarea=FOREST, filtered to COND_STATUS_CD = 1; If
#' landarea=TIMBERLAND, filtered to SITECLCD in(1:6) and RESERVCD = 0.
#' @param estseed String. Use seedling data only or add to tree data. Seedling
#' estimates are only for counts (estvar='TPA_UNADJ')-('none', 'only', 'add').
#' @param pcfilter String. A filter for plot or cond attributes (including
#' pltassgn).  Must be R logical syntax.
#' @param estvarn String. Name of the tree estimate variable (numerator).
#' @param estvarn.filter String. A tree filter for the estimate variable
#' (numerator).  Must be R syntax (e.g., "STATUSCD == 1").
#' @param estvard String. Name of the tree estimate variable (denominator).
#' @param estvard.filter String. A tree filter for the estimate variable
#' (denominator).  Must be R syntax (e.g., "STATUSCD == 1").
#' @param prednames String vector. Name(s) of predictor variables to include in
#' model.
#' @param FIA Logical. If TRUE, the finite population term is removed from
#' estimator to match FIA estimates.
#' @param rowvar String. Optional. Name of domain variable to group estvarn and
#' estvard by for rows in table output. Rowvar must be included in an input data frame
#' (i.e., plt, cond, tree). If no rowvar is included, an estimate is returned
#' for the total estimation unit. Include colvar for grouping by 2 variables.
#' @param colvar String. Optional. If rowvar != NULL, name of domain variable
#' to group estvarn and estvard by for columns in table output. Colvar must be included in
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
#' @param bootstrap Logical. If TRUE, returns bootstrap variance estimates,
#' otherwise uses Horvitz-Thompson estimator under simple random sampling
#' without replacement.
#' @param modelselect Logical. If TRUE, an elastic net regression model is fit 
#' to the entire plot level data, and the variables selected in that model are 
#' used for the proceeding estimation.
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
#' \item{plotsampcnt}{ Table. Number of plots by plot status (ex. sampled
#' forest on plot, sampled nonforest, nonsampled). } \item{condsampcnt}{ DF.
#' Number of conditions by condition status (forest land, nonforest land,
#' noncensus water, census water, nonsampled). } \item{unitarea}{ DF. Area by
#' estimation unit. } \item{expcondtab}{ DF. Condition-level area expansion
#' factors. } \item{tdomdat}{ DF. Final data table used for estimation. }
#' 
#' The data frames include the following information: \tabular{lll}{ \tab
#' \bold{Variable} \tab \bold{Description}\cr \tab rhat \tab estimated ratio estn/estd
#' \cr \tab rhat.var \tab variance estimate
#' of estimated ratio estn/estd \cr \tab NBRPLT Number of plots used in estimates \cr \tab
#' NBRPLT.gt0 \tab Number of non-zero plots used in estimates \cr \tab 
#' ACRES \tab total area for estimation unit \cr \tab rhat.se \tab
#' estimated standard error of ratio sqrt(rhat.var) \cr \tab rhat.cv \tab
#' estimated coefficient of variation of ratio rhat.se/rhat \cr \tab rhat.pse
#' \tab estimated percent standard error or ratio rhat.cv*100 \cr \tab CI99left
#' \tab left tail of 99 percent confidence interval for estimated area \cr \tab
#' CI99right \tab right tail of 99 percent confidence interval for estimated
#' area \cr \tab CI95left \tab left tail of 95 percent confidence interval for
#' estimated area \cr \tab CI95right \tab right tail of 95 percent confidence
#' interval for estimated area \cr \tab CI67left \tab left tail of 67 percent
#' confidence interval for estimated area \cr \tab CI67right \tab right tail of
#' 67 percent confidence interval for estimated area \cr } 
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
#' @author Josh Yamamoto
#' @references Kelly McConville, Becky Tang, George Zhu, Shirley Cheung, and
#' Sida Li (2018). mase: Model-Assisted Survey Estimation. R package version
#' 0.1.4 https://cran.r-project.org/package=mase
#' @keywords data
#' @export modMAratio          
modMAratio <- function(MApopdat, 
                       ratiotype = "PERACRE",
                       woodland = "Y",
                       landarea = "FOREST",
                       estseed = "none",
                       pcfilter = NULL, 
                       estvarn = NULL, 
                       estvarn.filter = NULL, 
                       estvard = NULL, 
                       estvard.filter = NULL, 
                       prednames = NULL,
                       FIA = TRUE,
                       rowvar = NULL, 
                       colvar = NULL, 
                       sumunits = TRUE, 
                       returntitle = FALSE, 
                       savedata = FALSE, 
                       table_opts = NULL, 
                       title_opts = NULL, 
                       savedata_opts = NULL, 
                       gui = FALSE, 
                       bootstrap = FALSE,
                       modelselect = FALSE,
                       ...){
  
  if (nargs() == 0 && is.null(MApopdat)) {
    gui <- TRUE
  } 
  
  ## If gui.. set variables to NULL
  if (gui) { 
    tree=landarea=areavar=sumunits=adj=getwt=cuniqueid=ACI=
      tuniqueid=savedata=addtitle=returntitle=rawdata=unitvar <- NULL
  }
  
  ## Set parameters
  rowcol.total <- TRUE
  esttype <- "RATIO"
  parameters <- FALSE
  returnlst <- list()
  rawdata <- TRUE
  
  ## Set global variables
  ONEUNIT=n.total=TOTAL=tdom=estvar.name=
    variable=estvard.name=rhat=rhat.se=rhat.var=rhat.cv=pse <- NULL
  
  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- c(names(formals(modMAratio)),
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
  
  # only option for now
  MAmethod <- "gregRatio"
  var_method <- "LinHTSRS"
  
  list.items <- c("condx", "pltcondx", "cuniqueid", "condid", 
                  "ACI.filter", "unitarea", "unitvar", "unitlut", "npixels",
                  "npixelvar", "plotsampcnt", "condsampcnt")
  
  
  MApopdat <- pcheck.object(MApopdat, "MApopdat", list.items=list.items)
  
  if (is.null(MApopdat)) return(NULL)
  condx <- MApopdat$condx
  pltcondx <- MApopdat$pltcondx	
  treex <- MApopdat$treex
  seedx <- MApopdat$seedx
  if (is.null(treex) && is.null(seedx)) {
    stop("must include tree data for ratio estimates")
  }
  
  cuniqueid <- MApopdat$cuniqueid
  npixels <- MApopdat$npixels
  npixelvar <- MApopdat$npixelvar
  condid <- MApopdat$condid
  tuniqueid <- MApopdat$tuniqueid
  ACI.filter <- MApopdat$ACI.filter
  unitarea <- MApopdat$unitarea
  areavar <- MApopdat$areavar
  areaunits <- MApopdat$areaunits
  unitvar <- MApopdat$unitvar
  unitvars <- MApopdat$unitvars
  unitlut <- MApopdat$unitlut
  unitvars <- MApopdat$unitvars
  expcondtab <- MApopdat$expcondtab
  plotsampcnt <- MApopdat$plotsampcnt
  condsampcnt <- MApopdat$condsampcnt
  predfac <- MApopdat$predfac
  states <- MApopdat$states
  invyrs <- MApopdat$invyrs
  estvar.area <- MApopdat$estvar.area
  adj <- MApopdat$adj
  expcondtab <- MApopdat$expcondtab
  pop_fmt <- MApopdat$pop_fmt
  pop_dsn <- MApopdat$pop_dsn
  
  
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
  
  
  unitchk <- pcheck.areaunits(unitarea = unitarea, areavar = areavar, 
                              areaunits = areaunits, metric = metric)
  unitarea <- unitchk$unitarea
  areavar <- unitchk$areavar
  areaunits <- unitchk$outunits
  
  if (is.null(key(unitarea))) {
    setkeyv(unitarea, unitvar)
  }
  
  estdat <- check.estdata(esttype=esttype, pop_fmt=pop_fmt, pop_dsn=pop_dsn, 
                pltcondf=pltcondx, cuniqueid=cuniqueid, condid=condid, 
                treex=treex, seedx=seedx, estseed=estseed, woodland=woodland,
				sumunits=sumunits, landarea=landarea,
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
  treef <- estdat$treef
  seedf <- estdat$seedf
  tuniqueid <- estdat$tuniqueid
  estseed <- estdat$estseed
  woodland <- estdat$woodland
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
  
  rowcolinfo <- check.rowcol(gui=gui, esttype=esttype, 
                    treef=treef, seedf=seedf, 
					condf=pltcondf, cuniqueid=cuniqueid, 
					tuniqueid=tuniqueid, estseed=estseed, 
					rowvar=rowvar, colvar=colvar, 
					row.FIAname=row.FIAname, col.FIAname=col.FIAname, 
					row.orderby=row.orderby, col.orderby=col.orderby,
					row.add0=row.add0, col.add0=col.add0,
					title.rowvar=title.rowvar, title.colvar=title.colvar,
					rowlut=rowlut, collut=collut, rowgrp=rowgrp,
					rowgrpnm=rowgrpnm, rowgrpord=rowgrpord, 
					landarea=landarea, states=states, 
					cvars2keep="COND_STATUS_CD") 
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
  rm(rowcolinfo) 
  
  
  if (rowvar == "TOTAL") rowcol.total <- TRUE
    
  #####################################################################################
  ### Get estimation data from tree table
  #####################################################################################
  adjtree <- ifelse(adj %in% c("samp", "plot"), TRUE, FALSE)
  treedat <- check.tree(gui=gui, treef=treef, seedf=seedf, estseed=estseed,
                        bycond=TRUE, condf=condf, bytdom=bytdom, 
                        tuniqueid=tuniqueid, cuniqueid=cuniqueid, 
                        esttype=esttype, ratiotype=ratiotype,
                        estvarn=estvarn, estvarn.filter=estvarn.filter, 
                        estvard=estvard, estvard.filter=estvard.filter, 
                        esttotn=TRUE, esttotd=TRUE, 
                        tdomvar=tdomvar, tdomvar2=tdomvar2, 
                        adjtree=adjtree, metric=metric, woodland=woodland)
  
  if (is.null(treedat)) return(NULL)
  tdomdat <- treedat$tdomdat
    
  ## Merge tdomdat with condx
  xchk <- check.matchclass(condx, tdomdat, c(cuniqueid, condid))
  condx <- xchk$tab1
  tdomdat <- xchk$tab2  
  tdomdat <- merge(condx, tdomdat, by=c(cuniqueid, condid))
  
  if (!is.null(tdomvar)) {
    ## Merge condf with condx
    xchk <- check.matchclass(condx, condf, c(cuniqueid, condid))
    condx <- xchk$tab1
    condf <- xchk$tab2   
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
 
  ## Generate a uniquecol for estimation units
  if (!sumunits & colvar == "NONE") {
    uniquecol <- data.table(unitarea[[unitvar]])
    setnames(uniquecol, unitvar)
    uniquecol[[unitvar]] <- factor(uniquecol[[unitvar]])
  }
 
  #####################################################################################
  ### Get titles for output tables
  #####################################################################################
  alltitlelst <- check.titles(dat=tdomdat, esttype=esttype, estseed=estseed, 
                              ratiotype=ratiotype, sumunits=sumunits, 
                              title.main=title.main, title.ref=title.ref,
                              title.rowvar=title.rowvar, title.rowgrp=title.rowgrp, 
                              title.colvar=title.colvar, title.unitvar=title.unitvar, 
                              title.filter=title.filter, title.unitsn=estunitsn,
                              title.unitsd=estunitsd, title.estvarn=title.estvarn,
                              unitvar=unitvar, rowvar=rowvar, colvar=colvar,
                              estvarn=estvarn, estvarn.filter=estvarn.filter, 
                              estvard=estvard, estvard.filter=estvard.filter, 
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
  if(rawdata) {
    outfn.rawdat <- alltitlelst$outfn.rawdat
  }
  
  #####################################################################################
  ## GENERATE ESTIMATES
  #####################################################################################
  unit_totest=unit_grpest=unit_rowest=unit_colest=unit_grpest=rowunit=totunit <- NULL
  addtotal <- ifelse(((rowvar == "TOTAL" || length(unique(tdomdat[[rowvar]])) > 1) ||
                        (!is.null(tdomvarlstn) && length(tdomvarlstn) > 1)), TRUE, FALSE)
  
  if (modelselect == TRUE) {
    
    # want to do variable selection on plot level data...
    pltlvl <- tdomdat[ , lapply(.SD, sum, na.rm = TRUE), 
                       by=c(unitvar, cuniqueid, "TOTAL", prednames),
                       .SDcols=estvarn.name]
    
    y <- pltlvl[[estvarn.name]]
    xsample <- pltlvl[ , prednames, with = F, drop = F]
    xpop <- unitlut[ , prednames, with = F, drop = F]
    N <- sum(npixels[["npixels"]])
    
    preds.selected <- gregEN.select(y = y,
                                    x_sample = xsample,
                                    x_pop = xpop,
                                    N = N,
                                    alpha = 0.5)
    
    if (length(preds.selected) == 0 || is.null(preds.selected)) {
      
      warning("No variables selected in model selection, proceeding with all possible predictors. \n")
      
    } else {
      
      prednames <- preds.selected 
      message(paste0("Predictors ", "[", paste0(prednames, collapse = ", "), "]", " were chosen in model selection.\n"))
      
    }
    
    
  }
  
  # can do modelselect above this if we want
  message("using the following predictors...", toString(prednames))
  
  response <- estvarn.name
  response_d <- estvard.name
  # use this for now since estunitsn is NULL...
  estunits <- unitarea[[unitvar]]
  # estunits <- treedat$estunits
  
  if (bootstrap) {
    var_method <- "bootstrapSRS"
  } else {
    var_method <- "LinHTSRS"
  }
  
  # not sure what this part does...
  # generally seems like tdomvar is rowvar and tdomvar2 is colvar (if they exist)
  if (!is.null(tdomvar2)) {
    ddomvar <- "TOTAL"
    tdomdat <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
                       by=c(unitvar, cuniqueid, ddomvar), .SDcols=tdomvarlstn]
    tdomdat <- transpose2row(tdomdat, uniqueid=c(unitvar, cuniqueid, ddomvar),
                             tvars=tdomvarlstn)
    setnames(tdomdat, "value", estvarn.name)
    suppressWarnings(tdomdat[, (grpvar) := tstrsplit(variable, "#")])[, variable := NULL]
    
    cdomdattot <- cdomdat[, lapply(.SD, sum, na.rm=TRUE), 
                          by=c(unitvar, cuniqueid, "TOTAL"), .SDcols=estvard.name]   
    tdomdat <- merge(tdomdat, cdomdattot, by=c(unitvar, cuniqueid, "TOTAL"))
  }
  
  if (addtotal) {
    ## Get estimate for total
    if (!is.null(tdomvar)) {
      tdomdattot <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
                            by=c(unitvar, cuniqueid, "TOTAL", prednames), .SDcols=estvarn.name]
      cdomdattot <- cdomdat[, lapply(.SD, sum, na.rm=TRUE), 
                            by=c(cuniqueid, "TOTAL"), .SDcols=estvard.name]   
      tdomdattot <- merge(tdomdattot, cdomdattot, by=c(cuniqueid, "TOTAL"))
    } else {
      tdomdattot <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
                            by=c(unitvar, cuniqueid, "TOTAL", prednames), .SDcols=c(estvarn.name, estvard.name)]
    }
    
    # unitarea should get passed as it includes the acres for each estimation unit
    # npixels should also get passed 
    
    unit_totestlst <- lapply(estunits, MAest.unit, 
                             dat=tdomdattot, cuniqueid=cuniqueid, 
                             unitlut=unitlut, unitvar=unitvar, esttype=esttype, 
                             MAmethod=MAmethod, prednames=prednames, strvar = NULL,
                             domain="TOTAL", response=response, response_d=response_d,
                             npixels=npixels, unitarea=unitarea, FIA=FIA,
                             var_method=var_method)
    
  }
  
  unit_totest <- do.call(rbind, sapply(unit_totestlst, '[', "unitest"))
  
  
  tabs <- check.matchclass(unitarea, unit_totest, unitvar)
  unitarea <- tabs$tab1
  unit_totest <- tabs$tab2
  setkeyv(unit_totest, unitvar)
  unit_totest <- unit_totest[unitarea, nomatch=0]
  
  suppressWarnings(
    unit_totest[, rhat.se := sqrt(rhat.var)][,
                                             rhat.cv := rhat.se/rhat][,
                                                                      pse := rhat.cv*100]
  )
  
  if (rowvar != "TOTAL") {
    if (!is.null(tdomvar)) {
      if (!is.null(tdomvar2)) {
        tdomdatsum <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
                              by=c(unitvar, cuniqueid, rowvar, prednames), .SDcols=estvarn.name]    
      } else {
        if (tdomvar == rowvar) {
          tdomdatsum <- transpose2row(tdomdat, uniqueid=c(unitvar, cuniqueid),
                                      tvars=tdomvarlstn)
          setnames(tdomdatsum, c("variable", "value"), c(rowvar, estvarn.name))
          tdomdatsum <- tdomdatsum[, lapply(.SD, sum, na.rm=TRUE), 
                                   by=c(unitvar, cuniqueid, rowvar, prednames), .SDcols=estvarn.name]
        } else {  
          tdomdatsum <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
                                by=c(unitvar, cuniqueid, rowvar, prednames), .SDcols=estvarn.name]
        }
      }
      if (rowvar %in% names(cdomdat)) {
        cdomdatsum <- cdomdat[, lapply(.SD, sum, na.rm=TRUE), 
                              by=c(unitvar, cuniqueid, rowvar, prednames), .SDcols=estvard.name]
        tdomdatsum <- merge(tdomdatsum, cdomdatsum, 
                            by=c(unitvar, cuniqueid, rowvar))
      } else {
        cdomdatsum <- cdomdat[, lapply(.SD, sum, na.rm=TRUE),
                              by=c(unitvar, cuniqueid, prednames), .SDcols=estvard.name]
        tdomdatsum <- merge(tdomdatsum, cdomdatsum, by=c(unitvar, cuniqueid))
      }
    } else {
      tdomdatsum <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
                            by=c(unitvar, cuniqueid, rowvar, prednames), .SDcols=c(estvarn.name, estvard.name)]
    }
    
    unit_rowestlst <- lapply(estunits, MAest.unit, 
                             dat=tdomdatsum, cuniqueid=cuniqueid, 
                             unitlut=unitlut, unitvar=unitvar, esttype=esttype, 
                             MAmethod=MAmethod, prednames=prednames, strvar = NULL,
                             domain=rowvar, response=response, response_d=response_d,
                             npixels=npixels, unitarea=unitarea, FIA=FIA,
                             var_method=var_method)
    
    unit_rowest <- do.call(rbind, sapply(unit_rowestlst, '[', "unitest"))
    
    if (colvar != "NONE") {
      if (!is.null(tdomvar)) {
        if (!is.null(tdomvar2)) {
          tdomdatsum <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
                                by=c(unitvar, cuniqueid, colvar, prednames), .SDcols=estvarn.name]    
        } else {
          if (tdomvar == colvar) {
            tdomdatsum <- transpose2row(tdomdat, uniqueid=c(unitvar, cuniqueid),
                                        tvars=tdomvarlstn)
            setnames(tdomdatsum, c("variable", "value"), c(colvar, estvarn.name))
            tdomdatsum <- tdomdatsum[, lapply(.SD, sum, na.rm=TRUE), 
                                     by=c(unitvar, cuniqueid, colvar, prednames), .SDcols=estvarn.name]
          } else {     
            tdomdatsum <- tdomdat[, lapply(.SD, sum, na.rm=TRUE),
                                  by=c(unitvar, cuniqueid, colvar, prednames), .SDcols=estvarn.name]
          }
        }
        if (colvar %in% names(cdomdat)) {
          cdomdatsum <- cdomdat[, lapply(.SD, sum, na.rm=TRUE),
                                by=c(unitvar, cuniqueid, colvar, prednames), .SDcols=estvard.name]
          tdomdatsum <- merge(tdomdatsum, cdomdatsum, by=c(unitvar, cuniqueid, colvar))
        } else {
          cdomdatsum <- cdomdat[, lapply(.SD, sum, na.rm=TRUE), 
                                by=c(unitvar, cuniqueid, prednames), .SDcols=estvard.name]
          tdomdatsum <- merge(tdomdatsum, cdomdatsum, by=c(unitvar, cuniqueid))
        }
      } else {
        tdomdatsum <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
                              by=c(unitvar, cuniqueid, colvar, prednames), .SDcols=c(estvarn.name, estvard.name)]
      }
      
      unit_colestlst <- lapply(estunits, MAest.unit, 
                               dat=tdomdatsum, cuniqueid=cuniqueid, 
                               unitlut=unitlut, unitvar=unitvar, esttype=esttype, 
                               MAmethod=MAmethod, prednames=prednames, strvar = NULL,
                               domain=colvar, response=response, response_d=response_d,
                               npixels=npixels, unitarea=unitarea, FIA=FIA,
                               var_method=var_method)
      
      unit_colest <- do.call(rbind, sapply(unit_colestlst, '[', "unitest"))
      
      if (!is.null(tdomvar)) {
        if (!is.null(tdomvar2)) {
          tdomdatsum <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
                                by=c(unitvar, cuniqueid, grpvar, prednames), .SDcols=c(estvarn.name, estvard.name)]
        } else {
          ddomvar <- grpvar[grpvar != tdomvar]
          tdomdatsum <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
                                by=c(unitvar, cuniqueid, ddomvar, prednames), .SDcols=tdomvarlstn]
          tdomdatsum <- transpose2row(tdomdatsum, 
                                      uniqueid=c(unitvar, cuniqueid, ddomvar), 
                                      tvars=tdomvarlstn)
          setnames(tdomdatsum, c("variable", "value"), c(tdomvar, estvarn.name))      
          
          if (any(grpvar %in% names(cdomdat))) {
            mergevar <- grpvar[grpvar %in% names(cdomdat)]
            cdomdatsum <- cdomdat[, lapply(.SD, sum, na.rm=TRUE), 
                                  by=c(unitvar, cuniqueid, mergevar, prednames), .SDcols=estvard.name]
            tdomdatsum <- merge(tdomdatsum, cdomdatsum, by=c(unitvar, cuniqueid, mergevar))
          } else {
            cdomdatsum <- cdomdat[, lapply(.SD, sum, na.rm=TRUE), 
                                  by=c(unitvar, cuniqueid, prednames), .SDcols=estvard.name]
            tdomdatsum <- merge(tdomdatsum, cdomdatsum, by=c(unitvar, cuniqueid))
          }
        }
      } else {
        tdomdatsum <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
                              by=c(unitvar, cuniqueid, grpvar, prednames), .SDcols=c(estvarn.name, estvard.name)]
      }
      tdomdatsum[, grpvar := do.call(paste, c(.SD, sep="#")), .SDcols=grpvar]
      
      unit_grpestlst <- lapply(estunits, MAest.unit, 
                               dat=tdomdatsum, cuniqueid=cuniqueid, 
                               unitlut=unitlut, unitvar=unitvar, esttype=esttype, 
                               MAmethod=MAmethod, prednames=prednames, strvar = NULL,
                               domain="grpvar", response=response, response_d=response_d,
                               npixels=npixels, unitarea=unitarea, FIA=FIA,
                               var_method=var_method)
      
      unit_grpest <- do.call(rbind, sapply(unit_grpestlst, '[', "unitest"))
      
      if (any(unit_grpest$grpvar == "NA#NA")) {
        unit_grpest <- unit_grpest[unit_grpest$grpvar != "NA#NA", ]
      }
      
      unit_grpest[, c(rowvar, colvar) := tstrsplit(grpvar, "#", fixed=TRUE)]
      
    }
    
  }
  
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
    
    suppressWarnings(
      unit_rowest[, rhat.se := sqrt(rhat.var)][,
                                               rhat.cv := rhat.se/rhat][,
                                                                        pse := rhat.cv*100]
    )
    
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
    
    suppressWarnings(
      unit_colest[, rhat.se := sqrt(rhat.var)][,
                                               rhat.cv := rhat.se/rhat][,
                                                                        pse := rhat.cv*100]
    )
    
    setkeyv(unit_colest, c(unitvar, colvar))
  }
  if (!is.null(unit_grpest)) {
    unit_grpest <- add0unit(x=unit_grpest, xvar=rowvar, 
                            uniquex=uniquerow, unitvar=unitvar, 
                            xvar.add0=row.add0, xvar2=colvar, 
                            uniquex2=uniquecol,
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
    
    suppressWarnings(
      unit_grpest[, rhat.se := sqrt(rhat.var)][,
                                               rhat.cv := rhat.se/rhat][,
                                                                        pse := rhat.cv*100]
    )
    
    setkeyv(unit_grpest, c(unitvar, rowvar, colvar))
  }
  
  message("getting output...")
  estnm <- "rhat"
  
  # use esttype = "TREE" because this ratio format is different than in GB
  
  tabs <- est.outtabs(esttype="TREE", sumunits=sumunits, areavar=areavar, 
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
                      overwrite=overwrite_layer, estnm=estnm, psenm = "pse",
                      estround=estround, pseround, divideby=divideby, 
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
        if (!tabnm %in% c(estvarn, prednames)) {
          rawtab <- rawdat[[i]]
          outfn.rawtab <- paste0(outfn.rawdat, "_", tabnm) 
          if (tabnm %in% c("plotsampcnt", "condsampcnt")) {
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
    rawdat$esttype <- "RATIO"
    rawdat$MAmethod <- MAmethod
    rawdat$predselect <- prednames
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
