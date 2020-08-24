check.estdataPB <- function(gui, PBx=NULL, plotid="plot_id", pntid="dot_cnt", 
	tabtype="PCT", ratio=FALSE, plt.filter=NULL, landarea="ALL", landarea.filter=NULL, 
	pnt.nonsamp.filter=NULL, pnt.filter=NULL, sumunits=FALSE, allin1=FALSE, 
	estround=3, pseround=3, divideby=NULL, savedata=FALSE, addtitle=TRUE, 
	returntitle=TRUE, rawdata=FALSE, outfolder=NULL){

  ###################################################################################
  ## DESCRIPTION: Checks data inputs 
  ## Check landarea ("FOREST", "ALL", "TIMBERLAND")
  ## Import and check cond and plt tables and check unique identifiers
  ## Check plt table
  ## - Check for NA values
  ## - Apply plt filter, if plt exists.
  ## - Check missing pdoms2keep variables
  ## - Check predfac variable(s) and strvar - for factor status
  ## Apply cond filters to condf: landarea.filter, ACI.filter, cond.filter
  ## Check other table parameters: sumunits, allin1, estround, pseround, divideby, 
  ##		savedata, addtitle, returntitle, rawdata, outfolder
  ## - If sumunits=TRUE, estimation units are summed to 1 estimate
  ## - If allin1=TRUE, puts estimate (% sample error) in each cell
  ## - If savedata=TRUE, saves tables to outfolder
  ## - If addtitle=TRUE, adds title(s) to saved tables
  ## - If returntitle, saves title(s) of tables
  ## - If rawdata=TRUE, generates and saves rawdata variables for estimates
  ## Return data and nosamp.filter
  ###################################################################################

  ## Set global variables
  filterids <- NULL

  ## Check logical parameters
  ###################################################################################      
  ## Check ratio
  ratio <- FIESTA::pcheck.logical(ratio, varnm="ratio", title="ratio estimates", 
	first="NO", gui=gui)


  #############################################################################
  ## Check landarea 
  #############################################################################
  landarealst <- c("ALL", "CHANGE")
  landarea <- FIESTA::pcheck.varchar(var2check=landarea, varnm="landarea", gui=gui,
	checklst=landarealst, caption="Sample land area?")



  ## Add a column to PBx for the total estimate
  ##############################################################################
  PBx$TOTAL <- 1


  ##############################################################################
  ## Apply filters
  ##############################################################################

  ## plt.filter to plt table
  PBf <- datFilter(x=PBx, xfilter=plt.filter, title.filter="plt filter?",
		gui=gui, filternm="plt.filter", stopifnull=TRUE)$xf
  if (is.null(PBf)) {
    message(paste(plt.filter, "removed all records"))
    return(NULL)
  }

  ## pnt.nonsamp.filter
  PBf <- FIESTA::datFilter(PBf, pnt.nonsamp.filter, title.filter="pnt.nonsample filter",
			filternm="pnt.nonsamp.filter")$xf
  if (is.null(PBf)) {
    message(paste(pnt.nonsamp.filter, "removed all records"))
    return(NULL)
  }

  ## landarea.filter (e.g., change plots)
  PBf <- FIESTA::datFilter(x=PBf, xfilter=landarea.filter, gui=gui, 
		title.filter="landarea filter", stopifnull=TRUE)$xf
  if (is.null(PBf)) {
    message(paste(landarea.filter, "removed all records"))
    return(NULL)
  }

  ## pnt.filter
  PBf <- FIESTA::datFilter(PBf, pnt.filter, gui=gui, 
			title.filter="pnt filter")$xf
  if (is.null(PBf)) {
    message(paste(pnt.filter, "removed all records"))
    return(NULL)
  }

  ## Get filter ids for NOTinDOMAIN points
  ########################################################################
  if (!is.null(PBf) && (nrow(PBf) < nrow(PBx))) 
    filterids <- paste(PBf[[plotid]], PBf[[pntid]])
 

  #####################################################################################
  ### Check other table parameters
  #####################################################################################

  ## Check sumunits 
  ########################################################
  sumunits <- FIESTA::pcheck.logical(sumunits, varnm="sumunits", 
		title="Sum estimation units?", first="YES", gui=gui)
  if (!is.null(sumunits) && sumunits && tabtype == "PCT") {
    warning("must include unitarea to calculate unit weights for summing units")
    sumunits <- FALSE
  }

  ## Check allin1
  allin1 <- FIESTA::pcheck.logical(allin1, varnm="allin1", 
		title="All 1 table - Est (%error)?", first="NO", gui=gui)

  ## Check divideby
  ########################################################
  dividebylst <- c("hundred", "thousand", "million")
  if (!is.null(divideby) || gui)
    divideby <- FIESTA::pcheck.varchar(var2check=divideby, varnm="divideby", 
		gui=gui, checklst=dividebylst, caption="Divide estimates?")


  ## Check addtitle 
  addtitle <- FIESTA::pcheck.logical(addtitle, varnm="addtitle", 
		title="Add title to output?", first="YES", gui=gui)

  ## Check returntitle 
  returntitle <- FIESTA::pcheck.logical(returntitle, varnm="returntitle", 
		title="Save output titles?", first="YES", gui=gui)

  ## Check rawtable
  rawdata <- FIESTA::pcheck.logical(rawdata, varnm="rawdata", 
		title="Output raw data?", first="NO", gui=gui)

  ## Check savedata 
  savedata <- FIESTA::pcheck.logical(savedata, varnm="savedata", 
		title="Save data tables?", first="YES", gui=gui)

  ## Check outfolder 
  if (savedata) {
    outfolder <- FIESTA::pcheck.outfolder(outfolder, gui)
    if (rawdata && !file.exists(paste(outfolder, "rawdata", sep="/"))) 
      dir.create(paste(outfolder, "rawdata", sep="/"))
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
	landarea=landarea, sumunits=sumunits, allin1=allin1, estround=estround, pseround=pseround, 
	divideby=divideby, addtitle=addtitle, returntitle=returntitle, rawdata=rawdata, 
	savedata=savedata, outfolder=outfolder)


  return(returnlst)
}

