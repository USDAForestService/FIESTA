check.estdata <- function(esttype, pltcondf=NULL, cuniqueid="PLT_CN", 
	condid="CONDID", treex=NULL, tuniqueid="PLT_CN", sumunits=FALSE, 
	landarea=NULL, ACI.filter=NULL, plt.filter=NULL, cond.filter=NULL, 
	allin1=FALSE, estround=6, pseround=3, divideby=NULL, addtitle=TRUE, 
	returntitle=TRUE, rawdata=FALSE, savedata=FALSE, outfolder=NULL, gui=FALSE){

  ###################################################################################
  ## DESCRIPTION: Checks data inputs 
  ## Apply plot filter
  ## - plt.filter (e.g., COUNTY == 3)
  ## Check landarea ("FOREST", "ALL", "TIMBERLAND") and create landarea.filter
  ## - if landarea = FOREST, "COND_STATUS_CD == 1"
  ## - if landarea = TIMBERLAND, "SITECLCD %in% c(1:6) & RESERVCD == 0"
  ## Apply condition filters
  ## - landarea.filter
  ## - cond.filter (e.g., FORTYPCD == 122)
  ## - ACI.filter (e.g., if, ACI=FALSE, COND_STATUS_CD == 1)
  ## Check output parameters
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


  ###########################################################################
  ## Apply plt.filter to plt table
  ###########################################################################
  pltcondnmlst <- names(pltcondf)
  pltcondf <- datFilter(x=pltcondf, xfilter=plt.filter, title.filter="plt filter?",
		gui=gui, filternm="plt.filter", stopifnull=TRUE)$xf
  if (is.null(pltcondf)) {
    message(paste(plt.filter, "removed all records"))
    return(NULL)
  }

  #############################################################################
  ## Check landarea 
  #############################################################################
  if (esttype == "RATIO") {
    landarealst <- c("FOREST", "TIMBERLAND")
  } else {
    landarealst <- c("FOREST", "ALL", "TIMBERLAND")
  }
  landarea <- FIESTA::pcheck.varchar(var2check=landarea, varnm="landarea", gui=gui,
	checklst=landarealst, caption="Sample land area?")


  ## Create landarea.filter 
  #############################################################################
  landarea.filter <- NULL
  if (landarea != "ALL") {
    if (landarea == "FOREST") {
      landarea.filter <- "COND_STATUS_CD == 1"
      landcols <- "COND_STATUS_CD"
    } else if (landarea == "TIMBERLAND") {
      landcols <- c("SITECLCD", "RESERVCD")
      if (any(!landcols %in% pltcondnmlst)) {
        landcols.miss <- landcols[which(!landcols %in% pltcondnmlst)]
        stop(paste("missing variables for TIMBERLAND landarea filter:", 
		paste(landcols.miss, collapse=", ")))
      } 
      landarea.filter <- "SITECLCD %in% c(1:6) & RESERVCD == 0"
    }  
    ## Check for missing landcols 
    landcolsmiss <- landcols[which(!landcols %in% pltcondnmlst)]
    if (length(landcolsmiss) > 0) 
      stop("missing variables: ", paste(landcolsmiss, collapse=", "))
  }


  ###################################################################################
  ## Apply cond filters
  ###################################################################################

  ## Apply landarea.filter to condf
  pltcondf <- FIESTA::datFilter(x=pltcondf, xfilter=landarea.filter, 
		title.filter="landarea filter", gui=gui, stopifnull=FALSE)$xf
  if (is.null(pltcondf)) {
    message(paste(landarea.filter, "removed all records"))
    return(NULL)
  }
  ## Apply cond.filter to condf
  pltcondf <- datFilter(x=pltcondf, xfilter=cond.filter, 
		title.filter="cond filter", gui=gui, stopifnull=FALSE)$xf
  if (is.null(pltcondf)) {
    message(paste(cond.filter, "removed all records"))
    return(NULL)
  }
  ## Apply ACI.filter to condf
  if (landarea != "ALL") {
    pltcondf <- FIESTA::datFilter(x=pltcondf, xfilter=ACI.filter, 
			title.filter="ACI.filter", gui=gui, stopifnull=FALSE)$xf
    if (is.null(pltcondf)) {
      message(paste(ACI.filter, "removed all records"))
      return(NULL)
    }
  }
  

  #####################################################################################
  ### Check other table parameters
  #####################################################################################

  ## Check divideby
  ########################################################
  dividebylst <- c("hundred", "thousand", "million")
  if (!is.null(divideby) || gui)
    divideby <- FIESTA::pcheck.varchar(var2check=divideby, varnm="divideby", 
		gui=gui, checklst=dividebylst, caption="Divide estimates?")

  ## Check sumunits 
  ########################################################
  sumunits <- FIESTA::pcheck.logical(sumunits, varnm="sumunits", 
		title="Sum estimation units?", first="YES", gui=gui, stopifnull=TRUE)

  ## Check allin1
  ########################################################
  allin1 <- FIESTA::pcheck.logical(allin1, varnm="allin1", 
		title="All 1 table - Est (%error)?", first="NO", gui=gui)

  ### Check savedata 
  savedata <- FIESTA::pcheck.logical(savedata, varnm="savedata", 
		title="Save data tables?", first="YES", gui=gui, stopifnull=TRUE)

  ### Check addtitle 
  addtitle <- FIESTA::pcheck.logical(addtitle, varnm="addtitle", 
		title="Add title to output?", first="YES", gui=gui, stopifnull=TRUE)

  ### Check returntitle 
  returntitle <- FIESTA::pcheck.logical(returntitle, varnm="returntitle", 
		title="Save output titles?", first="YES", gui=gui, stopifnull=TRUE)

  ## Check rawtable
  ########################################################
  rawdata <- FIESTA::pcheck.logical(rawdata, varnm="rawdata", title="Output raw data?", 
		first="NO", gui=gui, stopifnull=TRUE)


  ## Check outfolder 
  ########################################################
  if (savedata) 
    outfolder <- FIESTA::pcheck.outfolder(outfolder, gui)
  

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
  returnlst <- list(pltcondf=pltcondf, cuniqueid=cuniqueid, sumunits=sumunits, 
	allin1=allin1, estround=estround, pseround=pseround, divideby=divideby, 
	addtitle=addtitle, returntitle=returntitle, rawdata=rawdata, savedata=savedata,
 	outfolder=outfolder, estround=estround, pseround=pseround, landarea=landarea)


  if (esttype %in% c("TREE", "RATIO")) {
    ## Check that the values of tuniqueid in treex are all in cuniqueid in condf
    treef <- check.matchval(treex, pltcondf, tuniqueid, cuniqueid, tab1txt="tree", 
		tab2txt="cond", subsetrows=TRUE)
    returnlst$treef <- treef
    returnlst$tuniqueid <- tuniqueid
  }
 
  return(returnlst)
}

