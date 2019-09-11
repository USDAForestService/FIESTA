check.dataPB <- function(gui, pnt=NULL, pltpct=NULL, pltpctvars=NULL, plt=NULL, 
	plotid="plot_id", pntid="dot_cnt", puniqueid="CN", unitvar=NULL, unitvar2=NULL, 
	autocombine=NULL, auxvars=NULL, ratio=ratio, sumunits=NULL, tabtype="PCT", 
	strata=FALSE, strvar=NULL, landarea="ALL", landarea.filter=NULL, 
	nonsamp.filter=NULL, rowvar=NULL, colvar=NULL, row.orderby=NULL,
 	col.orderby=NULL, pnt.filter=NULL, plt.filter=NULL, nullcheck=FALSE,
	allin1=FALSE, estround=3, pseround=3, savedata=FALSE, addtitle=TRUE, 
	returntitle=TRUE, rawdata=FALSE, outfolder=NULL, pvars2keep=NULL){

  ###################################################################################
  ## DESCRIPTION: Checks data inputs 
  ## Define necessary plot and condition-level variables: 
  ## - plt (pvars2keep) - "STATECD", unitvars, auxvars
  ## Check tabtype, module, and MAmethod
  ## - module in("GB", "MA")
  ## - MAmethod in("HT", "PS", "GREG")
  ## Check logical parameters: ratio, sumunits, strata
  ## - If sumunits=TRUE, estimation units are summed to 1 estimate
  ## - If strata, only 1 auxvar allowed
  ## Import pnt or pltpct table and check unique identifiers (plotid):
  ## If pltpct, 
  ## - Check for NA values in uniqueid (plotid)
  ## - Check pltpctvars and transpose to rows
  ## - Apply landarea and pnt filters
  ## - Add a column, TOTAL=1 to pntprop table for total estimate
  ## - Get filter ids for NOTinDOMAIN points
  ## If pnt,
  ## - Check for NA values in uniqueids (plotid, pntid)
  ## - Create table of sampled/nonsampled points
  ## - Add a column, TOTAL=1 to pnt table for total estimate
  ## - Apply nonsample, landarea, and pnt filters
  ## - Get filter ids for NOTinDOMAIN points
  ## Import and check plot table (optional)
  ## - Check unique identifier of plot
  ## - Check class of unique identifier of plot and pnt tables
  ## - If no plot table, create one from pnt table's unique identifier 
  ## - Apply plot filters
  ## Check table parameters: allin1, savedata, addtitle, returntitle, add0, rawdata:
  ## - If allin1=TRUE, puts estimate (% sample error) in each cell
  ## - If savedata=TRUE, saves tables to outfolder
  ## - If addtitle=TRUE, adds title(s) to saved tables
  ## - If returntitle, saves title(s) of tables
  ## - If rawdata=TRUE, generates and saves rawdata variables for estimates
  ###################################################################################

  ## Set global variables
  pltdomainlst=pntprop=sampcnt=TOTAL=condid=filterids=invyrs=p.pltdom=value=
	ONEUNIT=PBvars2keep <- NULL

  ###################################################################################
  ## Define necessary plot and condition level variables
  ###################################################################################
  pvars2keep <- unique(c(pvars2keep, "STATECD", unitvar, unitvar2, auxvars))
  PBvars2keep <- unique(c(rowvar, colvar, row.orderby, col.orderby))

  
  ## Check logical parameters
  ###################################################################################

  ## Check tabtype
  tabtype <- FIESTA::pcheck.varchar(var2check=tabtype, varnm="tabtype", gui=gui, 
		checklst=c("PCT", "AREA"), caption="Table output type", 
		warn="invalid tabtype")
      
  ## GET ratio
  ratio <- FIESTA::pcheck.logical(ratio, varnm="ratio", title="ratio estimates", 
	first="NO", gui=gui)

  ## Check sumunits 
  ########################################################
  sumunits <- FIESTA::pcheck.logical(sumunits, varnm="sumunits", 
		title="Sum estimation units?", first="YES", gui=gui)
  if (!is.null(sumunits) && sumunits && tabtype == "PCT") {
    warning("must include unitarea to calculate unit weights for summing units")
    sumunits <- FALSE
  }

  ## Check autocombine
  ########################################################
  autocombine <- FIESTA::pcheck.logical(autocombine, varnm="autocombine", 
		title="Combine estimation units?", first="YES", gui=gui, stopifnull=TRUE)

  ## Check strata, strvars
  ###################################################################################
  strata <- FIESTA::pcheck.logical(strata, varnm="strata", 
		title="Post stratify?", first="YES", gui=gui, stopifnull=TRUE)
  if (strata) {
    if (is.null(strvar)) stop("must include strvar for post-strat estimates")
    pvars2keep <- c(pvars2keep, strvar)
  } else {
    strvar <- NULL
  }

  ## Check auxvars
  ###################################################################################
#  if ((module == "MA" && MAmethod != "PS"))
#    if (!is.null(auxvars)) pvars2keep <- pvars2keep[pvars2keep != auxvars]

 
  #############################################################################
  ## Check landarea 
  #############################################################################
  landarealst <- c("ALL", "CHANGE")
  landarea <- FIESTA::pcheck.varchar(var2check=landarea, varnm="landarea", gui=gui,
	checklst=landarealst, caption="Sample land area?")
 
  ##################################################################
  ## Import and check tables 
  ##################################################################
  pntx <- FIESTA::pcheck.table(pnt, gui=gui, tabnm="pnt", caption="Point table?", 
		nullcheck=TRUE)
  pltpctx <- FIESTA::pcheck.table(pltpct, gui=gui, tabnm="pltpct", 
		caption="Point counts?", nullcheck=TRUE)

  if (is.null(pntx) && is.null(pltpctx)) 
    stop("must include pnt or pltpct")

  if (!is.null(pntx) && !is.null(pltpctx)) 
    stop("only input pnt or pltpct")

  if (!is.null(pltpctx)) {
    getprop <- FALSE
    pltpctvarlst <- names(pltpctx)

    ## Check unique identifiers of pnt table (puniqueid and condid)
    #############################################################################
    plotid <- FIESTA::pcheck.varchar(var2check=plotid, varnm="plotid", gui=gui, 
		checklst=pltpctvarlst, caption="UniqueID variable for pltpct", 
		warn="plotid not in pltpct table", stopifnull=TRUE)
    setkeyv(pltpctx, plotid) 

    ## Check for NA values in unique identifier
    ##############################################################################
    navars <- c(plotid)
    pltpctx.na <- sapply(navars,
		function(x, pltpctx){ sum(is.na(pltpctx[,x, with=FALSE])) }, pltpctx)
    if (any(pltpctx.na) > 0) 
      stop(pltpctx.na[pltpctx.na > 0], " NA values in variable: ", 
		paste(names(pltpctx.na[pltpctx.na > 0]), collapse=", "))

    ## Check pltpctvars
    ##############################################################################
    pltpctvarlst <- pltpctvarlst[pltpctvarlst != plotid]
    pltpctvars <- FIESTA::pcheck.varchar(pltpctvars, varnm="pltpctvars", 
		checklst=pltpctvarlst, gui=gui, caption="invalid variable names", 
		stopifnull=TRUE, multiple=TRUE)
    if (is.null(pltpctvars)) {
      pltpctvars <- pltpctvarlst     
      warning("pltpctvars=NULL... using all variables in pltpct for estimation")
    }

    ## Transpose pltpctvars to rows
    ##############################################################################
    pntprop <- FIESTA::transpose2row(pltpctx, plotid, tvars=pltpctvars, na.rm=TRUE)
    pntprop[, p.pltdom := value/100][, value := NULL]
    rowvar <- "variable"
    PBvars2keep <- "p.pltdom"

    ## Add a column to pltpct for the total estimate
    pntprop$TOTAL <- 1

    ##############################################################################
    ## Apply filters
    ##############################################################################

    ## landarea.filter (e.g., change plots)
    pntprop <- FIESTA::datFilter(x=pntprop, xfilter=landarea.filter, gui=gui, 
			title.filter="landarea filter")$xf
    ## pnt.filter
    pltpctf <- FIESTA::datFilter(pntprop, pnt.filter, gui=gui, 
		title.filter="pnt filter")$xf

    ## Get filter ids for NOTinDOMAIN points
    ########################################################################
    if (!is.null(pltpctf) && (nrow(pltpctf) < nrow(pntprop))) 
      filterids <- paste(pltpctf[[plotid]], pntf[[pntid]]) 

    PBx <- pntprop   

  } else if (!is.null(pntx)) {

    getprop <- TRUE
    pntnmlst <- names(pntx)

    ## Check unique identifiers of pnt table (puniqueid and pntid)
    #############################################################################
    plotid <- FIESTA::pcheck.varchar(var2check=plotid, varnm="plotid", gui=gui, 
		checklst=pntnmlst, caption="UniqueID variable for plot - pnt", 
		warn="plotid not in pnt table", stopifnull=TRUE)
    
    pntid <- FIESTA::pcheck.varchar(var2check=pntid, varnm="pntid", gui=gui, 
		checklst=pntnmlst[pntnmlst != plotid], caption="UniqueID variable for points - pnt", 
		warn="pntid not in pnt table", stopifnull=TRUE)
    setkeyv(pntx, c(plotid, pntid)) 

#    condid <- FIESTA::pcheck.varchar(var2check=condid, varnm="condid", gui=gui, 
#		checklst=pntnmlst, caption="UniqueID variable - cond", 
#		warn=paste(condid, "not in cond table"))
#
#    ## If condid = NULL, add a variable CONDID=1 to cond
#    if (is.null(condid)) {
#      pntx[, CONDID := 1]
#      condid <- "CONDID"
#    } 
  
    ## Check if there are any NA values in these variables
    ##############################################################################
#    navars <- c(plotid, pntid, condid)
    navars <- c(plotid, pntid, PBvars2keep)
    pntx.na <- sapply(navars,
		function(x, pntx){ sum(is.na(pntx[,x, with=FALSE])) }, pntx)
    if (any(pntx.na) > 0) 
      stop(pntx.na[pntx.na > 0], " NA values in variable: ", 
		paste(names(pntx.na[pntx.na > 0]), collapse=", "))

    ## Get table of sampled / nonsampled points
    ##############################################################################
    sampcnt <- pntx[, .N, by=c(rowvar, colvar)]


    ## Add a column to pnt for the total estimate
    ##############################################################################
    pntx$TOTAL <- 1

    ##############################################################################
    ## Apply filters
    ##############################################################################

    ## nonsamp.filter
    pntx <- FIESTA::datFilter(pntx, nonsamp.filter, title.filter="nonsample filter",
			filternm="nonsamp.filter")$xf
    ## landarea.filter (e.g., change plots)
    pntf <- FIESTA::datFilter(x=pntx, xfilter=landarea.filter, gui=gui, 
		title.filter="landarea filter", stopifnull=TRUE)$xf
    ## pnt.filter
    pntf <- FIESTA::datFilter(pntf, pnt.filter, gui=gui, 
			title.filter="pnt filter")$xf

    ## Get filter ids for NOTinDOMAIN points
    ########################################################################
    if (!is.null(pntf) && (nrow(pntf) < nrow(pntx))) 
      filterids <- paste(pntf[[plotid]], pntf[[pntid]])

    PBx <- pntx
    rm(pntx)
    gc() 
  }

  ####################################################################################
  ### Import and check plot table (optional), check unique identifiers
  ####################################################################################
  pltx <- FIESTA::pcheck.table(plt, gui=gui, tabnm="plt", 
	caption="plt table?", nullcheck=TRUE)

  ## Note: If no plt table, generate one to include puniqueid, and strata variables.
  if (!is.null(pltx)) {
    if (isS4(pltx)) pltx <- pltx@data
    pltnmlst <- names(pltx)

    ## Check puniqueid
    puniqueid <- FIESTA::pcheck.varchar(var2check=puniqueid, varnm="puniqueid", gui=gui, 
		checklst=pltnmlst, caption="UniqueID variable for plot - plt", 
		warn="puniqueid not in plt table", stopifnull=TRUE)
    if (length(unique(pltx[[puniqueid]])) > nrow(pltx))
      warning("plt records are not unique")

    ## Check class of puniqueid in pnt table and plt and match with cond
    tabs <- FIESTA::check.matchclass(PBx, pltx, plotid, puniqueid, 
				tab1txt="pntx", tab2txt="pltx")
    PBx <- tabs$tab1
    pltx <- tabs$tab2

    ## Check for 1 unique identifier per plot
    ##########################################################################
    nbrplots.plt <- length(unique(pltx[[puniqueid]]))
    nbrplots.pnt <- length(unique(PBx[[plotid]]))

    ## If there are more FIA plots in the pnt file than the plt, stop
    ## If there are more FIA plot in plt than pnts, remove from plt
    if (nbrplots.pnt > nbrplots.plt) {
      missplots <- nbrplots.pnt - nbrplots.plt
      warn <-  paste("there are more plots in pnt than plt: ", missplots)
      ## Stop if greater than 5%
      if (missplots / nbrplots.pnt * 100 > 5) stop(warn)
      warning(warn)
      PBx <- PBx[PBx[[plotid]] %in% pltx[[puniqueid]],]
    } else if(nbrplots.plt > nbrplots.pnt) {
      missplots <- nbrplots.plt - nbrplots.pnt
      warning(paste("there are more plots in plt than pnt: ", missplots))
      pltx <- pltx[get(eval(puniqueid)) %in% unique(PBx[[plotid]]),]
    }

    ## Apply plt.filter if plt is not null
    ##################################################################################
    pltf <- FIESTA::datFilter(x=pltx, xfilter=plt.filter, gui=gui, 
		title.filter="plt filter?")
    pltx <- pltf$xf
    if(is.null(pltx)) stop("plt.filter is invalid")
    plt.filter <- pltf$xfilter
    
  } else {  ## if pltx is NULL 
 
    ## If no pltx, create 1 from PBx
    if (!is.null(PBx)) {
      pvars <- names(PBx)[which(names(PBx) %in% pvars2keep)]
      pltx <- unique(PBx[, unique(c(plotid, pvars)), with=FALSE])
    } else {
      pvars <- names(pntprop)[which(names(pntprop) %in% pvars2keep)]
      pltx <- unique(pntprop[, plotid, with=FALSE])
    }
    puniqueid <- plotid
    pltnmlst <- names(pltx)
  }

  ## Set key of pltx to puniqueid
  ###########################################################
  setkeyv(pltx, puniqueid)


  ## Check missing plot variables
  ###########################################################################
  pmissvars <- pvars2keep[which(!pvars2keep %in% names(pltx))]
  if (length(pmissvars) > 0) {
#    if ("STATECD" %in% pmissvars) 
#      message("STATECD not in dataset.. assuming 1 state in dataset")

    if (any(auxvars %in% pmissvars)) {
      auxvars[which(!auxvars %in% pmissvars)]
      stop("auxvar not in tables: ", paste(auxvars, collapse=", "))
    }
    unitvars <- c(unitvar, unitvar2)
    if (any(unitvars %in% pmissvars)) {
      unitvars[which(!unitvars %in% pmissvars)]
      stop("unitvar not in tables: ", paste(unitvars, collapse=", "))
    }
  }
  pvars2keep <- pvars2keep[which(!pvars2keep %in% pmissvars)]


  ## Check for NA values in necessary variables in plt table
  pltx.na <- sapply(pvars2keep, function(x, pltx){ sum(is.na(pltx[,x, with=FALSE])) }, pltx)
  if (any(pltx.na) > 0) 
    warning(pltx.na[pltx.na > 0], " NA values in variable: ", 
		paste(names(pltx.na[pltx.na > 0]), collapse=", "))

  ## Add unitvar to plt table if NULL
  if (is.null(unitvar)) {
    pltx[, ONEUNIT := 1] 
    unitvar <- "ONEUNIT"
  }

  ###############################################################################
  ## Check table parameters
  ###############################################################################

  ## Check allin1
  allin1 <- FIESTA::pcheck.logical(allin1, varnm="allin1", 
		title="All 1 table - Est (%error)?", first="NO", gui=gui)

  ## Check savedata 
  savedata <- FIESTA::pcheck.logical(savedata, varnm="savedata", 
		title="Save data tables?", first="YES", gui=gui)

  ## Check addtitle 
  addtitle <- FIESTA::pcheck.logical(addtitle, varnm="addtitle", 
		title="Add title to output?", first="YES", gui=gui)

  ## Check returntitle 
  returntitle <- FIESTA::pcheck.logical(returntitle, varnm="returntitle", 
		title="Save output titles?", first="YES", gui=gui)

  ## Check rawtable
  rawdata <- FIESTA::pcheck.logical(rawdata, varnm="rawdata", 
		title="Output raw data?", first="NO", gui=gui)

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

  returnlst <- list(PBx=PBx, pntprop=pntprop, pltx=pltx, plotid=plotid,
 	puniqueid=puniqueid, tabtype=tabtype, ratio=ratio, sumunits=sumunits, 
	unitvar=unitvar, unitvar2=unitvar2, autocombine=autocombine, auxvars=auxvars, 
 	strata=strata, strvar=strvar, rowvar=rowvar, colvar=colvar, 
	row.orderby=row.orderby, col.orderby=col.orderby, plt.filter=plt.filter,
 	pnt.filter=pnt.filter, invyrs=invyrs, sampcnt=sampcnt, filterids=filterids, 
	allin1=allin1, savedata=savedata, addtitle=addtitle, returntitle=returntitle, 
	rawdata=rawdata, outfolder=outfolder, getprop=getprop, rowvar=rowvar,
	colvar=colvar, row.orderby=row.orderby, col.orderby=col.orderby, 
	PBvars2keep=PBvars2keep, estround=estround, pseround=pseround,
	landarea=landarea)

  return(returnlst)
}
