check.estdataPB <- function(PBx=NULL, plotid="plot_id", pntid="dot_cnt",
	tabtype="PCT", ratio=FALSE, pfilter=NULL, landarea="ALL", landarea.filter=NULL,
	nonsamp.pntfilter=NULL, pntfilter=NULL, sumunits=FALSE, allin1=FALSE,
	estround=3, pseround=3, divideby=NULL, addtitle=TRUE, returntitle=TRUE,
	rawdata=FALSE, rawonly=FALSE, gainloss=FALSE, savedata=FALSE, outfolder=NULL,
	overwrite_dsn=FALSE, overwrite_layer=TRUE, outfn.pre=NULL, outfn.date=TRUE,
	append_layer=FALSE, raw_fmt="csv", raw_dsn=NULL, gui=FALSE){

  ###################################################################################
  ## DESCRIPTION: Checks data inputs
  ## Applies plot filters (e.g., COUNTYCD == 1")
  ## Applies pnt filters (e.g., cover_1 == "TREE")
  ## Applies nonsamp.pntfilter (e.g., cover_1 == "99")
  ## Applies landarea filter (e.g., change plots only)
  ## Set filtered points to 'NOTinDOMAIN'
  ## Check sumunits (whether to generate estimates by unitvar and output sum)
  ## Check rounding parameters (estround, pseround)
  ## Check output parameters
  ## VALUE:
  ## Return parameters and filterids for NotinDomain plots
  ###################################################################################

  ## Set global variables
  filterids=rawfolder <- NULL

  ## Check logical parameters
  ###################################################################################
  ## Check ratio
  ratio <- pcheck.logical(ratio, varnm="ratio", title="ratio estimates",
	first="NO", gui=gui)


  #############################################################################
  ## Check landarea
  #############################################################################
  landarealst <- c("ALL", "CHANGE")
  landarea <- pcheck.varchar(var2check=landarea, varnm="landarea", gui=gui,
	checklst=landarealst, caption="Sample land area?")


  ## Add a column to PBx for the total estimate
  ##############################################################################
  PBx$TOTAL <- 1


  ##############################################################################
  ## Apply filters
  ##############################################################################

  ## pfilter to plt table
  PBf <- datFilter(x=PBx, xfilter=pfilter, title.filter="plt filter?",
		gui=gui, filternm="pfilter", stopifnull=TRUE)$xf
  if (is.null(PBf)) {
    message(paste(pfilter, "removed all records"))
    return(NULL)
  }

  ## nonsamp.pntfilter
  PBf <- FIESTA::datFilter(PBf, nonsamp.pntfilter, title.filter="pnt.nonsample filter",
			filternm="nonsamp.pntfilter")$xf
  if (is.null(PBf)) {
    message(paste(nonsamp.pntfilter, "removed all records"))
    return(NULL)
  }

  ## landarea.filter (e.g., change plots)
  PBf <- FIESTA::datFilter(x=PBf, xfilter=landarea.filter, gui=gui,
		title.filter="landarea filter", stopifnull=TRUE)$xf
  if (is.null(PBf)) {
    message(paste(landarea.filter, "removed all records"))
    return(NULL)
  }

  ## pntfilter
  PBf <- FIESTA::datFilter(PBf, pntfilter, gui=gui,
			title.filter="pnt filter")$xf
  if (is.null(PBf)) {
    message(paste(pntfilter, "removed all records"))
    return(NULL)
  }

  ## Get filter ids for NOTinDOMAIN points
  ########################################################################
  if (!is.null(PBf) && (nrow(PBf) < nrow(PBx))) {
    filterids <- paste(PBf[[plotid]], PBf[[pntid]])
  }

  #####################################################################################
  ### Check other table parameters
  #####################################################################################

  ## Check gainloss
  if (!ratio) {
    gainloss <- pcheck.logical(gainloss, varnm="gainloss",
		title="gainloss estimates", first="NO", gui=gui)
  } else {
    gainloss <- FALSE
  }

  ## Check sumunits
  ########################################################
  sumunits <- pcheck.logical(sumunits, varnm="sumunits",
		title="Sum estimation units?", first="YES", gui=gui)
  if (!is.null(sumunits) && sumunits && tabtype == "PCT") {
    warning("must include unitarea to calculate unit weights for summing units")
    sumunits <- FALSE
  }

  ## Check allin1
  allin1 <- pcheck.logical(allin1, varnm="allin1",
		title="All 1 table - Est (%error)?", first="NO", gui=gui)

  ## Check divideby
  ########################################################
  dividebylst <- c("hundred", "thousand", "million")
  if (!is.null(divideby) || gui) {
    divideby <- pcheck.varchar(var2check=divideby, varnm="divideby",
		gui=gui, checklst=dividebylst, caption="Divide estimates?")
  }

  ## Check addtitle
  addtitle <- pcheck.logical(addtitle, varnm="addtitle",
		title="Add title to output?", first="YES", gui=gui)

  ## Check returntitle
  returntitle <- pcheck.logical(returntitle, varnm="returntitle",
		title="Save output titles?", first="YES", gui=gui)

  ## Check rawtable
  rawdata <- pcheck.logical(rawdata, varnm="rawdata", title="Output raw data?",
		first="NO", gui=gui, stopifnull=TRUE)

  ## Check rawonly
  rawonly <- pcheck.logical(rawonly, varnm="rawonly", title="Raw data only?",
		first="NO", gui=gui, stopifnull=TRUE)
  if (rawonly && !rawdata) rawdata <- TRUE

  ## Check output info
  ########################################################
  if (savedata) {
    if (!rawonly) {
      outlst <- pcheck.output(out_fmt="csv", outfolder=outfolder,
		outfn.pre=outfn.pre, outfn.date=outfn.date,
		overwrite_layer=overwrite_layer, append_layer=append_layer, gui=gui)
      outfolder <- outlst$outfolder
      overwrite_layer <- outlst$overwrite_layer
      outfn.pre <- outfn.pre
    }
    if (rawdata) {
      if (!is.null(raw_fmt) && raw_fmt == "csv") {
        rawfolder <- paste(outfolder, "rawdata", sep="/")
        if (!file.exists(rawfolder)) dir.create(rawfolder)
      } else {
        if (is.null(raw_dsn)) {
          raw_dsn <- "rawdata"
        }
        outlst <- pcheck.output(out_dsn=raw_dsn, out_fmt=raw_fmt,
		outfolder=outfolder, outfn.pre=outfn.pre, outfn.date=outfn.date,
		overwrite_dsn=overwrite_dsn, overwrite_layer=overwrite_layer,
		append_layer=append_layer, gui=gui)
        rawfolder <- outlst$outfolder
        raw_fmt <- outlst$out_fmt
        raw_dsn <- outlst$out_dsn
        overwrite_layer <- outlst$overwrite_layer
      }
    }
  }

  ## Check rounding variables
  if (is.null(estround)) {
    estround <- ifelse(allin1, 0, 3)
  } else {
    if (!is.numeric(estround)) stop("estround must be a numeric")
    if (estround > 12) {
      estround <- ifelse(allin1, 0, 3)
      warning("check estround... very high number, setting to ", estround)
    }
  }
  if (is.null(pseround)) {
    pseround <- ifelse(allin1, 0, 3)
  } else {
    if (!is.numeric(pseround)) stop("pseround must be a numeric")
    if (pseround > 12) {
      pseround <- ifelse(allin1, 0, 3)
      warning("check pseround... very high number, setting to ", pseround)
    }
  }


  ## Set up list of variables to return
  ######################################################################################
  returnlst <- list(PBf=PBf, plotid=plotid, pntid=pntid, filterids=filterids,
	landarea=landarea, sumunits=sumunits, allin1=allin1, estround=estround,
	pseround=pseround, divideby=divideby, addtitle=addtitle, returntitle=returntitle,
 	rawdata=rawdata, rawonly=rawonly, savedata=savedata, outfolder=outfolder,
 	overwrite_layer=overwrite_layer, append_layer=append_layer, rawfolder=rawfolder,
	raw_fmt=raw_fmt, raw_dsn=raw_dsn, tabtype=tabtype, ratio=ratio, gainloss=gainloss)


  return(returnlst)
}

