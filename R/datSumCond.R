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
#' @param savedata Logical. If TRUE, writes output data to outfolder.
#' @param outfolder String. Name of the output folder. If savedata=TRUE, output
#' is saved to the outfolder.
#' @param out_fmt String. Format for output tables ('csv', 'sqlite', 'gpkg').
#' @param out_dsn String. Data source name for output. If extension is not
#' included, out_fmt is used. Use full path if outfolder=NULL.
#' @param out_layer String. Name of output layer in database or *.csv file, if
#' savedata=TRUE. If NULL, the file will be named tsum_'date'.csv.
#' @param outfn.pre String. Prefix for out_dsn.
#' @param layer.pre String. Prefix for out_layer.
#' @param outfn.date Logical. If TRUE, adds current date to outfile name.
#' @param overwrite_dsn Logical. If TRUE, overwrites raw_dsn, if exists.
#' @param overwrite_layer Logical. If TRUE, overwrites the out_layer in raw_dsn
#' or *.csv raw data layer, if datsource="csv".
#' @param append_layer Logical. If TRUE, and rawdata=TRUE, appends raw data
#' data frames to existing out_dsn layer or *.csv file.
#' @param returnDT Logical. If TRUE, returns data.table object(s). If FALSE,
#' returns data.frame object(s).
#' @return A list of the following items: \item{condsum}{ Data frame.
#' Plot-level table with aggregated condition attribute. } \item{cfilter}{
#' Condition filter. }
#' 
#' If savedata=TRUE, condsum is saved to the outfolder.
#' @note Nonsampled plots are removed from table.
#' @author Tracey S. Frescino
#' @keywords data
#' @examples
#' 
#' 
#'   ## Aggregate LIVE_CANOPY_CVR_PCT to plot, weighted by CONDPROP_UNADJ
#'   condsum <- datSumCond(cond=FIESTA::WYcond, csumvar="LIVE_CANOPY_CVR_PCT")$condsum
#' 
#'   ## Check results
#'   condsum[condsum$PLT_CN == 40404737010690,]
#'   FIESTA::WYcond[FIESTA::WYcond$PLT_CN == 40404737010690,]
#' 
#' @export datSumCond
datSumCond <- function(cond=NULL, plt=NULL, plt_dsn=NULL, cuniqueid="PLT_CN", 
	puniqueid="CN", csumvar=NULL, csumvarnm=NULL, cfilter=NULL, getadjplot=FALSE, 
	adjcond=FALSE, NAto0=FALSE, savedata=FALSE, outfolder=NULL, out_fmt="csv",
	out_dsn=NULL, out_layer=NULL, outfn.pre=NULL, layer.pre=NULL, outfn.date=TRUE, 
	overwrite_dsn=FALSE, overwrite_layer=FALSE, append_layer=FALSE, returnDT=TRUE){
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

  ## SET OPTIONS
  options(scipen = 6) # bias against scientific notation
  options(stringsAsFactors=FALSE)

  ## SET glopbal variables
  CONDPROP_ADJ=CONDPROP_UNADJ <- NULL


  ##################################################################
  ## CHECK INPUT PARAMETERS
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
  savedata <- pcheck.logical(savedata, varnm="savedata", 
		title="Save data tables?", first="NO", gui=gui)

  ## If savedata, check output file names
  ################################################################
  if (savedata) { 
    outlst <- pcheck.output(gui=gui, out_dsn=out_dsn, out_fmt=out_fmt, 
		outfolder=outfolder, outfn.pre=outfn.pre, outfn.date=outfn.date, 
		overwrite_dsn=overwrite_dsn, overwrite_layer=overwrite_layer)
    out_dsn <- outlst$out_dsn
    outfolder <- outlst$outfolder
    out_fmt <- outlst$out_fmt

    ## outfn
    if (is.null(out_layer) || gsub(" ", "", out_layer) == "") {
      out_layer <- "condsum"
      if (!is.null(layer.pre)) {
        out_layer <- paste(layer.pre, out_layer, sep="_")
      }
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
      spExportSpatial(condf.sum, out_dsn=plt_dsn, out_layer=out_layer,
			outfolder=outfolder, outfn.date=outfn.date, 
			overwrite_layer=overwrite_layer, append_layer=append_layer)
    }
    datExportData(condf.sum, outfolder=outfolder, out_fmt=out_fmt, 
		out_dsn=out_dsn, out_layer=out_layer, outfn.date=outfn.date, 
		overwrite_layer=overwrite_layer, append_layer=append_layer)
    
  }  

  if (!returnDT) {     
    condf.sum <- setDF(condf.sum)
  }
  sumcondlst <- list(condsum=condf.sum, csumvarnm=csumvarnm)
  if (!is.null(cfilter))
    sumcondlst$cfilter <- cfilter

  return(sumcondlst)
} 
