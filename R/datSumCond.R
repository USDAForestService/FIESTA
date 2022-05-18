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
#' @param plt Data frame, comma-delimited file (*.csv), shapefile (*.shp), or
#' database file. Plot-level table to join the aggregated tree data to (if
#' bycond=FALSE). Nonsampled plots (PLOT_STATUS_CD = 3) are removed. Optional.
#' @param plt_dsn String. The data source name (dsn; i.e., folder or database
#' name) of plt. The dsn varies by driver. See gdal OGR vector formats
#' (https://www.gdal.org/ogr_formats.html). Optional.
#' @param cuniqueid String. Unique identifier of cond (default = "PLT_CN").
#' @param puniqueid String. Unique identifier of plt (default = "CN").
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
#' @param returnDT Logical. If TRUE, returns data.table object(s). If FALSE,
#' returns data.frame object(s).
#' @param savedata Logical. If TRUE, saves data to outfolder.
#' @param savedata_opts List. See help(savedata_options()) for a list
#' of options. Only used when savedata = TRUE. If out_layer = NULL,
#' default = 'condsum'. 
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
                       plt = NULL, 
                       plt_dsn = NULL, 
                       cuniqueid = "PLT_CN",
                       puniqueid = "CN", 
                       csumvar = NULL, 
                       csumvarnm = NULL, 
                       cfilter = NULL, 
                       getadjplot = FALSE,
                       adjcond = FALSE, 
                       NAto0 = FALSE, 
                       returnDT = TRUE,
                       savedata = FALSE, 
                       savedata_opts = NULL,
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
  CONDPROP_ADJ=CONDPROP_UNADJ <- NULL


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

  ## Check cond table
  condx <- pcheck.table(cond, caption="Condition table?", stopifnull=TRUE)

  ## Check cuniqueid
  condnmlst <- names(condx)
  cuniqueid <- pcheck.varchar(var2check=cuniqueid, varnm="cuniqueid", 
	checklst=condnmlst, caption="UniqueID variable - cond", 
	warn="cuniqueid not in cond table", stopifnull=TRUE)

  ## Check for CONDPROP_UNADJ
  if (!"CONDPROP_UNADJ" %in% condnmlst) stop("CONDPROP_UNADJ not in cond")


  ## Check plt table
  noplt <- TRUE
  pltsp <- FALSE
  pltx <- pcheck.table(plt, gui=gui, tabnm="plt", caption="Plot table?")
  if (!is.null(pltx)) {
    noplt <- FALSE

    ## Remove totally nonsampled plots
    if ("PLOT_STATUS_CD" %in% names(pltx)) {
      if (3 %in% unique(pltx[["PLOT_STATUS_CD"]]))
        warning(paste("There are", sum(pltx[["PLOT_STATUS_CD"]] == 3), 
		"nonsampled plots"))
      pltx <- pltx[pltx$PLOT_STATUS_CD != 3,]
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
  if (any(csumvar == "CONDPROP_UNADJ")) {
    condx$CONDPROP <- 1
    csumvar[csumvar == "CONDPROP_UNADJ"] <- "CONDPROP"
  }


  ## Check csumvarnm
  if (is.null(csumvarnm)) csumvarnm <- paste(csumvar, "PLT", sep="_")
  condnmlst <- sapply(csumvarnm, checknm, condnmlst)

  ## Check getadjplot
  getadjplot <- pcheck.logical(getadjplot, varnm="getadjplot", 
		title="Get plot adjustment?", first="NO", gui=gui)
  if (getadjplot && is.null(condx)) 
    stop("must include condx to adjust to plot")

  ## Check adjcond
  adjcond <- pcheck.logical(adjcond, varnm="adjcond", 
		title="Adjust conditions?", first="NO", gui=gui)


  ## Check savedata 
  savedata <- pcheck.logical(savedata, varnm="savedata", title="Save data table?", 
                             first="NO", gui=gui)
  
  ## Check output parameters
  if (savedata) {
    outlst <- pcheck.output(outfolder=outfolder, out_dsn=out_dsn, 
        out_fmt=out_fmt, outfn.pre=outfn.pre, outfn.date=outfn.date, 
        overwrite_dsn=overwrite_dsn, overwrite_layer=overwrite_layer,
        add_layer=add_layer, append_layer=append_layer, gui=gui)
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
  }
  


  ################################################################################  
  ### DO WORK
  ################################################################################  

  if (getadjplot) {
    if ("COND_STATUS_CD" %in% names(condx)) {
      condx <- condx[condx$COND_STATUS_CD != 5,]
    } else {
      message("assuming no nonsampled condition in dataset")
    }
    adjfacdata <- getadjfactorPLOT(condx=condx, cuniqueid=cuniqueid)
    condx <- adjfacdata$condx
  }

  ## Filter cond
  cdat <- FIESTA::datFilter(x=condx, xfilter=cfilter, title.filter="tfilter",
			 stopifnull=TRUE, gui=gui)
  condf <- cdat$xf
  cfilter <- cdat$xfilter

  if (getadjplot) {
    if ("cadjcnd" %in% names(condf))
      stop("cadjcnd not in cond... must get adjustment factor")
    csumvarnm <- paste0(csumvarnm, "_ADJ")
    condf.sum <- condf[, lapply(.SD, function(x) sum(x * CONDPROP_ADJ, na.rm=TRUE)),
 		by=cuniqueid, .SDcols=csumvar]
  } else {
    condf.sum <- condf[, lapply(.SD, function(x) sum(x * CONDPROP_UNADJ, na.rm=TRUE)),
 		by=cuniqueid, .SDcols=csumvar]
  }
  names(condf.sum) <- c(cuniqueid, csumvarnm)

  ## Merge to plt
  if (!noplt) {
    if (is.data.table(pltx)) {
      setkeyv(condf.sum, cuniqueid)
      setkeyv(pltx, puniqueid)
    }
    condf.sum <- merge(pltx, condf.sum, by.x=puniqueid, by.y=cuniqueid, all.x=TRUE)
    if (NAto0) {
      for (col in csumvarnm) set(condf.sum, which(is.na(condf.sum[[col]])), col, 0)
    }
  }


  #### WRITE TO FILE 
  #############################################################
  if (savedata) {
    if (pltsp) {
      spExportSpatial(condf.sum, 
              savedata_opts=list(outfolder=outfolder, 
                                  out_fmt=out_fmt, 
                                  out_dsn=out_dsn, 
                                  out_layer=out_layer,
                                  outfn.pre=outfn.pre, 
                                  outfn.date=outfn.date, 
                                  overwrite_layer=overwrite_layer,
                                  append_layer=append_layer, 
                                  add_layer=TRUE))
    } else {
      datExportData(condf.sum, 
              savedata_opts=list(outfolder=outfolder, 
                                  out_fmt=out_fmt, 
                                  out_dsn=out_dsn, 
                                  out_layer=out_layer,
                                  outfn.pre=outfn.pre, 
                                  outfn.date=outfn.date, 
                                  overwrite_layer=overwrite_layer,
                                  append_layer=append_layer,
                                  add_layer=TRUE)) 
    }
  }  

  if (!returnDT) {     
    condf.sum <- setDF(condf.sum)
  }
  sumcondlst <- list(condsum=condf.sum, csumvarnm=csumvarnm)
  if (!is.null(cfilter))
    sumcondlst$cfilter <- cfilter

  return(sumcondlst)
} 
