#' Data - Aggregates numeric condition data to plot level.
#' 
#' Aggregates CONDPROP_UNADJ variable or other continuous condition variables
#' to plot level with option to apply condition filters. If condition variable
#' is not CONDPROP_UNADJ the variable is multiplied by CONDPROP_UNADJ for
#' weighted sum.
#' 
#' If variable = NULL, then it will prompt user for input.
#' 
#' @param cond Data frame or comma-delimited file (*.csv). Condition-level
#' table with aggregate variable and CONDPROP_UNADJ.
#' @param datsource String. Source of data ('obj', 'csv', 'sqlite', 'gdb').
#' @param data_dsn String. If datsource='sqlite', the name of SQLite database
#' (*.sqlite).
#' @param plt Data frame, comma-delimited file (*.csv), shapefile (*.shp), or
#' database file. Plot-level table to join the aggregated tree data to (if
#' bycond=FALSE). Nonsampled plots (PLOT_STATUS_CD = 3) are removed. Optional.
#' @param subp_cond Dataframe, comma-delimited file (*.csv), or shapefile (*.shp).
#' Subplot condition-level table to use to sum condition proportions, 
#' if bysubp=TRUE. 
#' @param subplot Dataframe, comma-delimited file (*.csv), or shapefile (*.shp).
#' Subplot-level table to used to calculate adjustment factors, to remove 
#' nonsampled conditions (SUBP_STATUS_CD = 3). This table is optional.
#' @param cuniqueid String. Unique identifier of cond (default = "PLT_CN").
#' @param puniqueid String. Unique identifier of plt (default = "CN").
#' @param condid String. Unique identifier for conditions.
#' @param bycond Logical. If TRUE, the data are aggregated to the condition
#' level (by: cuniqueid, condid). If FALSE, the data are aggregated to the plot
#' level (by: puniqueid). 
#' @param bysubp Logical. If TRUE, data are aggregated to the subplot level.
#' @param subpid String. Unique identifier of each subplot.
#' @param csumvar String. One or more variable names to sum to plot level.
#' @param csumvarnm String. Name of the resulting aggregated plot-level
#' variable(s).  Default = csumvar + '_PLT'.
#' @param cfilter String. A filter to subset the cond data before aggregating
#' (e.g., "COND_STATUS_CD == 1"). Must be R syntax.
#' @param getadjplot Logical. If TRUE, adjustments are calculated for
#' nonsampled conditions on plot.
#' @param adjcond Logical. If TRUE, csumvar condition variables are adjusted
#' for nonsampled conditions by plot.
#' @param NAto0 Logical. If TRUE, convert NA values to 0.
#' @param cround Number. The number of digits to round to. If NULL, default=5.
#' @param returnDT Logical. If TRUE, returns data.table object(s). If FALSE,
#' returns data.frame object(s).
#' @param savedata Logical. If TRUE, saves data to outfolder.
#' @param savedata_opts List. See help(savedata_options()) for a list
#' of options. Only used when savedata = TRUE. If out_layer = NULL,
#' default = 'condsum'.
#' @param dbconn Open database connection.
#' @param dbconnopen Logical. If TRUE, keep database connection open. 
#' @param gui Logical. If gui, user is prompted for parameters.
#' 
#' @return A list of the following items: \item{condsum}{ Data frame.
#' Plot-level table with aggregated condition attribute. } \item{cfilter}{
#' Condition filter. }
#' 
#' If savedata=TRUE, condsum is saved to the outfolder.
#' @note Nonsampled plots are removed from table.
#' @author Tracey S. Frescino
#' @keywords data
#' @examples
#' # Aggregate LIVE_CANOPY_CVR_PCT to plot, weighted by CONDPROP_UNADJ
#' condsum <- datSumCond(cond = FIESTA::WYcond,
#'                       csumvar = "LIVE_CANOPY_CVR_PCT")$condsum
#' 
#' # Check results
#' condsum[condsum$PLT_CN == 40404737010690,]
#' FIESTA::WYcond[FIESTA::WYcond$PLT_CN == 40404737010690,]
#' @export datSumCond
datSumCond <- function(cond = NULL, 
                       datsource = "obj", 
                       data_dsn = NULL, 
                       plt = NULL, 
                       subp_cond = NULL,  
                       subplot = NULL, 
                       cuniqueid = "PLT_CN",
                       puniqueid = "CN", 
                       condid = "CONDID", 
                       bycond = FALSE,                        
                       bysubp = FALSE, 
                       subpid = "SUBP", 
                       csumvar = NULL, 
                       csumvarnm = NULL, 
                       cfilter = NULL, 
                       getadjplot = FALSE,
                       adjcond = FALSE, 
                       NAto0 = FALSE, 
                       cround = 5, 
                       returnDT = TRUE,
                       savedata = FALSE, 
                       savedata_opts = NULL,
                          dbconn = NULL,
                          dbconnopen = FALSE,
                       gui = FALSE){
  
  #####################################################################################
  ## DESCRIPTION: Aggregates CONDPROP_UNADJ variable or other continuous condition 
  ##	variables to plot level with option to apply condition filters. If condition 
  ##	variable is not CONDPROP_UNADJ the variable is multiplied by CONDPROP_UNADJ  
  ##	for weighted sum.
  #####################################################################################

  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- ifelse(nargs() == 0, TRUE, FALSE)

  ## If gui.. set variables to NULL
  if(gui){ puniqueid=cuniqueid=csumvarnm=savedata <- NULL }

  ## Set global variables
  CONDPROP_ADJ=CONDPROP_UNADJ=NF_COND_STATUS_CD <- NULL
  ACI <- FALSE


  ##################################################################
  ## CHECK PARAMETER NAMES
  ##################################################################
  
  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- names(formals(datSumCond)) 
  if (!all(input.params %in% formallst)) {
    miss <- input.params[!input.params %in% formallst]
    stop("invalid parameter: ", toString(miss))
  }
  
  ## Check parameter lists
  pcheck.params(input.params, savedata_opts=savedata_opts)
  
  ## Set savedata defaults
  savedata_defaults_list <- formals(FIESTA::savedata_options)[-length(formals(FIESTA::savedata_options))]
  
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
  noplt <- TRUE
  nocond=pltsp <- FALSE

  ## Set datsource
  ########################################################
  datsourcelst <- c("obj", "csv", "sqlite", "gdb")
  datsource <- pcheck.varchar(var2check=datsource, varnm="datsource", 
		checklst=datsourcelst, gui=gui, caption="Data source?") 
  if (is.null(datsource)) {
    if (!is.null(data_dsn) && file.exists(data_dsn)) {
      dsn.ext <- getext(data_dsn)
      if (!is.na(dsn.ext) && dsn.ext != "") {
        datsource <- ifelse(dsn.ext == "gdb", "gdb", 
		ifelse(dsn.ext %in% c("db", "db3", "sqlite", "sqlite3"), "sqlite", 
             ifelse(dsn.ext == "csv", "csv",
			ifelse(dsn.ext == "shp", "shp", "datamart"))))
      } 
    } else {
      stop("datsource is invalid")
    }
  }

  ## Check cond table
  condx <- pcheck.table(cond, tab_dsn=data_dsn, caption="Condition table?", 
			stopifnull=TRUE)

  ## Check cuniqueid
  condnmlst <- names(condx)
  cuniqueid <- pcheck.varchar(var2check=cuniqueid, varnm="cuniqueid", 
	checklst=condnmlst, caption="UniqueID variable - cond", 
	warn="cuniqueid not in cond table", stopifnull=TRUE)

  ## Check for CONDPROP_UNADJ
  if (!"CONDPROP_UNADJ" %in% condnmlst) stop("CONDPROP_UNADJ not in cond")


  ## Check condid in tree table and setkey to tuniqueid, condid
  condid <- pcheck.varchar(var2check=condid, varnm="condid", 
		checklst=names(condx), caption="cond ID - tree", 
		warn=paste(condid, "not in tree table"))
 
  if (is.null(condid)) {
    message("assuming all 1 condition plots")
    condx$CONDID <- 1
    condid <- "CONDID"
  }


  ## Check bycond
  ###################################################################################
  bycond <- pcheck.logical(bycond, varnm="bycond", title="By condition?", 
		first="YES", gui=gui, stopifnull=TRUE)

  ## Check bysubp
  ###################################################################################
  bysubp <- pcheck.logical(bysubp, varnm="bysubp", title="By subplot?", 
		first="YES", gui=gui, stopifnull=TRUE)

  ## Check checkNA
  ###################################################################################
  NAto0 <- pcheck.logical(NAto0, varnm="NAto0", title="Convert NA to 0?", 
		first="YES", gui=gui)
  if (is.null(NAto0)) NAto0 <- FALSE


  ## Check unique ids and set keys
  if (bycond) {
    csumuniqueid <- c(cuniqueid, condid)
    setkeyv(condx, csumuniqueid)
           
  } else {
    csumuniqueid <- cuniqueid
  }


  if (bysubp) {
    subpuniqueid <- "PLT_CN"
    subpids <- c(subpuniqueid, subpid)

    ## Check subplot
    subplotx <- pcheck.table(subplot, tab_dsn=data_dsn, tabnm="subplot", gui=gui, 
			caption="Subplot table?")
  
    ## Check subpid
    if (!is.null(subplotx)) {
      if (!all(subpids %in% names(subplotx))) {
        stop("uniqueids not in subplot: ", toString(subpids))
      }
      setkeyv(subplotx, subpids)
    }

    ## Check subplot
    subpcondx <- pcheck.table(subp_cond, tab_dsn=data_dsn, tabnm="subp_cond", gui=gui, 
			caption="Subp_cond table?", stopifnull=TRUE)
    if (!all(subpids %in% names(subpcondx))) {
      stop("uniqueids not in subp_cond: ", toString(subpids))
    }
    setkeyv(subpcondx, subpids)

    ## Check unique ids and set keys
    if (bycond) {
      csumuniqueid <- c(subpuniqueid, subpid, condid)
      setkeyv(subpcondx, csumuniqueid)           
    } else {
      csumuniqueid <- c(subpuniqueid, subpid)
    }

    ## Set pltx to NULL   
    pltx <- NULL
  } else {

    pltx <- pcheck.table(plt, tab_dsn=data_dsn, gui=gui, tabnm="plt", 
			caption="Plot table?")
  }

  if (!is.null(pltx)) {
    noplt <- FALSE

    ## Remove totally nonsampled plots
    if ("PLOT_STATUS_CD" %in% names(pltx)) {
      if (3 %in% unique(pltx[["PLOT_STATUS_CD"]])) {
        warning(paste("There are", sum(pltx[["PLOT_STATUS_CD"]] == 3), 
		"nonsampled plots"))
        pltx <- pltx[pltx[["PLOT_STATUS_CD"]] != 3,]
      }
    } 

    if ("sf" %in% class(pltx))
      pltsp <- TRUE
      
    ## Check puniqueid
    pltnmlst <- names(pltx)
    nmlst <- names(pltx)
    puniqueid <- pcheck.varchar(var2check=puniqueid, varnm="puniqueid", 
		checklst=pltnmlst, caption="UniqueID variable - plt", 
		warn="puniqueid not in plot table", stopifnull=TRUE)

    ## Check that the values of cuniqueid in condx are all in puniqueid in pltx
    check.matchval(condx, pltx, cuniqueid, puniqueid)

    ## Check if class of cuniqueid matches class of puniqueid
    tabs <- check.matchclass(condx, pltx, cuniqueid, puniqueid)
    condx <- tabs$tab1
    pltx <- tabs$tab2  
  }

  ## Check csumvar
  csumvar <- pcheck.varchar(var2check=csumvar, varnm="csumvar", 
		checklst=condnmlst, caption="csumvar(s)", multiple=TRUE,
		stopifnull=TRUE, gui=gui)

  ## Check csumvarnm
  if (is.null(csumvarnm)) csumvarnm <- paste(csumvar, "PLT", sep="_")
  condnmlst <- sapply(csumvarnm, checknm, condnmlst)

  ## Check getadjplot
  getadjplot <- pcheck.logical(getadjplot, varnm="getadjplot", 
		title="Get plot adjustment?", first="NO", gui=gui)
  if (getadjplot && is.null(condx)) {
    stop("must include condx to adjust to plot")
  }

  ## Check adjcond
  adjcond <- pcheck.logical(adjcond, varnm="adjcond", 
		title="Adjust conditions?", first="NO", gui=gui)
  if (getadjplot && !adjcond) {
    message("getadjplot=TRUE, and adjcond=FALSE... setting adjcond=TRUE")
    adjcond <- TRUE
  }

  ## CHECK tround
  if (is.null(cround) | !is.numeric(cround)) {
    warning("cround is invalid.. rounding to 5 digits")
    tround <- 5
  }

  ## Check savedata 
  savedata <- pcheck.logical(savedata, varnm="savedata", title="Save data table?", 
                             first="NO", gui=gui)
  
  ## Check output parameters
  if (savedata) {
    outlst <- pcheck.output(outfolder=outfolder, out_dsn=out_dsn, 
        out_fmt=out_fmt, outfn.pre=outfn.pre, outfn.date=outfn.date, 
        overwrite_dsn=overwrite_dsn, overwrite_layer=overwrite_layer,
        add_layer=add_layer, append_layer=append_layer, out_conn=dbconn, 
         dbconnopen=TRUE, gui=gui)
    outfolder <- outlst$outfolder
    out_dsn <- outlst$out_dsn
    out_fmt <- outlst$out_fmt
    overwrite_layer <- outlst$overwrite_layer
    append_layer <- outlst$append_layer
    outfn.date <- outlst$outfn.date
    outfn.pre <- outlst$outfn.pre
    if (is.null(out_layer)) {
      out_layer <- "condsum"
    }
    out_conn = outlst$out_conn
  }
  

  ################################################################################  
  ### DO WORK
  ################################################################################  

  if (getadjplot) {

    if (bysubp) {
      ## Remove nonsampled conditions by subplot and summarize to condition-level
      subpcx <- subpsamp(cond = condx, 
                         subp_cond = subpcondx, 
                         subplot = subplotx, 
                         subpuniqueid = subpuniqueid, 
                         subpid = subpid)

      adjfacdata <- getadjfactorPLOT(condx = subpcx, 
		                          cuniqueid = c(subpuniqueid, subpid), 
                                     areawt = csumvar)
      subpcx <- adjfacdata$condx
      mergecols <- unique(c(cuniqueid, condid, names(condx)[!names(condx) %in% names(subpcx)]))
      condx <- merge(condx[, mergecols, with=FALSE], subpcx, 
                     by.x=c(cuniqueid, condid), by.y=c(subpuniqueid, condid))
      

    } else {

      ## Remove nonsampled conditions  
      if ("COND_STATUS_CD" %in% names(condx)) {
        cond.nonsamp.filter <- "COND_STATUS_CD != 5"
        nonsampn <- sum(condx[["COND_STATUS_CD"]] == 5, na.rm=TRUE)
        if (length(nonsampn) > 0) {
          message("removing ", nonsampn, " nonsampled forest conditions")
        } else {
          message("assuming all sampled conditions in cond")
        }
      } else {
        message("assuming all sampled conditions in cond")
      }
      if (ACI && "NF_COND_STATUS_CD" %in% names(condx)) {
        cond.nonsamp.filter.ACI <- "(is.na(NF_COND_STATUS_CD) | NF_COND_STATUS_CD != 5)"
        message("removing ", sum(is.na(NF_COND_STATUS_CD) & NF_COND_STATUS_CD == 5, na.rm=TRUE), 
		" nonsampled nonforest conditions")
        if (!is.null(cond.nonsamp.filter)) {
          cond.nonsamp.filter <- paste(cond.nonsamp.filter, "&", cond.nonsamp.filter.ACI)
        }
      }
      condx <- datFilter(x = condx, 
                         xfilter = cond.nonsamp.filter, 
		              title.filter = "cond.nonsamp.filter")$xf


      adjfacdata <- getadjfactorPLOT(condx = condx, 
                                     cuniqueid = cuniqueid, 
                                     areawt = csumvar)
      condx <- adjfacdata$condx
    }
  }
 
  ### Filter cond data 
  ###########################################################  
  cdat <- datFilter(x = condx, 
                    xfilter = cfilter, 
                    title.filter = "cfilter",
                    stopifnull = TRUE, 
                    gui = gui)
  condf <- cdat$xf
  cfilter <- cdat$xfilter


  if (getadjplot) {
    csumvaradj <- ifelse(csumvar == "CONDPROP_UNADJ", "CONDPROP_ADJ", paste0(csumvar, "_ADJ"))
    csumvarnm <- paste0(csumvarnm, "_ADJ")
    condf.sum <- condf[, lapply(.SD, sum, na.rm=TRUE), by=csumuniqueid, .SDcols=csumvaradj]
  } else {
    condf.sum <- condf[, lapply(.SD, sum, na.rm=TRUE), by=csumuniqueid, .SDcols=csumvar]
  }
  names(condf.sum) <- c(csumuniqueid, csumvarnm)


  ######################################################################## 
  ######################################################################## 
 
  ## Merge to cond or plot
  ###################################
  if (bycond && !nocond) {
    ## Merge to cond
    sumdat <- merge(condx, condf.sum, by=csumuniqueid, all.x=TRUE)
  } else if (!noplt) {
    if (is.data.table(pltx)) {
      setkeyv(condf.sum, cuniqueid)
      setkeyv(pltx, puniqueid)
    }
    sumdat <- merge(pltx, condf.sum, by.x=puniqueid, by.y=cuniqueid, all.x=TRUE)
  } else if (bysubp && !is.null(subplotx)) {
    sumdat <- merge(subplotx, condf.sum, by=csumuniqueid, all.x=TRUE)
  } else {
    sumdat <- condf.sum
  }

  ## Change NA values TO 0
  if (NAto0) {
    for (col in csumvarnm) set(sumdat, which(is.na(sumdat[[col]])), col, 0)
  }


  #### WRITE TO FILE 
  #############################################################
  if (savedata) {
    if (pltsp) {
      spExportSpatial(sumdat, 
              savedata_opts=list(outfolder = outfolder, 
                                  out_fmt = out_fmt, 
                                  out_dsn = out_dsn, 
                                  out_layer = out_layer,
                                  outfn.pre = outfn.pre, 
                                  outfn.date = outfn.date, 
                                  overwrite_layer = overwrite_layer,
                                  append_layer = append_layer, 
                                  add_layer = TRUE))
    } else {
      datExportData(sumdat, dbconn = out_conn, dbconnopen = FALSE,
              savedata_opts=list(outfolder = outfolder, 
                                  out_fmt = out_fmt, 
                                  out_dsn = out_dsn, 
                                  out_layer = out_layer,
                                  outfn.pre = outfn.pre, 
                                  outfn.date = outfn.date, 
                                  overwrite_layer = overwrite_layer,
                                  append_layer = append_layer,
                                  add_layer = TRUE)) 
    }
  }  

  ## Round values
  sumdat[,(csumvarnm) := lapply(.SD, round, cround), .SDcols=csumvarnm]

  if (!returnDT) {     
    sumdat <- data.frame(sumdat)
  }
  sumcondlst <- list(condsum=sumdat, csumvarnm=csumvarnm)
  if (!is.null(cfilter)) {
    sumcondlst$cfilter <- cfilter
  }
  return(sumcondlst)
} 
