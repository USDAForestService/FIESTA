check.outparams <- function(esttype, totals=TRUE, sumunits=FALSE, allin1=FALSE,
	estround=6, pseround=3, divideby=NULL, addtitle=TRUE, returntitle=TRUE,
	rawdata=FALSE, rawonly=FALSE, savedata=FALSE, outfolder=NULL,
	overwrite_dsn=FALSE, overwrite_layer=TRUE, outfn.pre=NULL, outfn.date=TRUE,
	append_layer=FALSE, raw_fmt="csv", raw_dsn=NULL, gui=FALSE){

  ###################################################################################
  ## DESCRIPTION: Checks output parameters
  ## - divideby, divides final estimates by (hundred, thousand, million)
  ## - if sumunits=TRUE, aggregates all estimation units
  ## - If allin1=TRUE, puts estimate (% sample error) in each cell
  ## - If savedata=TRUE, saves tables to outfolder
  ## - If addtitle=TRUE, adds title(s) to saved tables
  ## - If returntitle, saves title(s) of tables
  ## - If rawdata=TRUE, generates and saves rawdata variables for estimates
  ## - If savedata, checks output folder
  ## - If rawdata, adds a folder named 'rawdata' to outfolder to add raw data
  ## - estround - round estimate values
  ## - pseround - round percent standard error values
  ###################################################################################

  ## Set global variables
  rawfolder <- NULL



  ## Check divideby
  ########################################################
  dividebylst <- c("hundred", "thousand", "million")
  if (!is.null(divideby) || gui) {
    divideby <- pcheck.varchar(var2check=divideby, varnm="divideby",
		gui=gui, checklst=dividebylst, caption="Divide estimates?")
  }

  ## Check sumunits
  ########################################################
  sumunits <- pcheck.logical(sumunits, varnm="sumunits",
		title="Sum estimation units?", first="YES", gui=gui, stopifnull=TRUE)

  ## Check allin1
  ########################################################
  allin1 <- pcheck.logical(allin1, varnm="allin1",
		title="All 1 table - Est (%error)?", first="NO", gui=gui)

  ### Check savedata
  savedata <- pcheck.logical(savedata, varnm="savedata",
		title="Save data tables?", first="YES", gui=gui, stopifnull=TRUE)

  ### Check addtitle
  addtitle <- pcheck.logical(addtitle, varnm="addtitle",
		title="Add title to output?", first="YES", gui=gui, stopifnull=TRUE)

  ### Check returntitle
  returntitle <- pcheck.logical(returntitle, varnm="returntitle",
		title="Save output titles?", first="YES", gui=gui, stopifnull=TRUE)

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
    estround <- ifelse(allin1, 0, 6)
  } else {
    if (!is.numeric(estround)) stop("estround must be a numeric")
    if (estround > 16) {
      estround <- ifelse(allin1, 0, 6)
      message("check estround... very high number, setting to ", estround)
    }
  }
  if (is.null(pseround)) {
    pseround <- ifelse(allin1, 0, 6)
  } else {
    if (!is.numeric(pseround)) stop("pseround must be a numeric")
    if (pseround > 16) {
      pseround <- ifelse(allin1, 0, 6)
      warning("check pseround... very high number, setting to ", pseround)
    }
  }

  ## Set up list of variables to return
  ######################################################################################
  returnlst <- list(sumunits=sumunits, allin1=allin1, estround=estround, pseround=pseround,
 	divideby=divideby, addtitle=addtitle, returntitle=returntitle, estround=estround,
 	pseround=pseround, rawdata=rawdata, rawonly=rawonly, savedata=savedata,
	outfolder=outfolder, overwrite_layer=overwrite_layer, append_layer=append_layer,
	rawfolder=rawfolder, raw_fmt=raw_fmt, raw_dsn=raw_dsn)

  if (esttype %in% c("AREA", "TREE")) {
    returnlst$totals <- totals
  }


  return(returnlst)
}

