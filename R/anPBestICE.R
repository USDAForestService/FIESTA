anPBestICE <- function(icedat, pltassgn=NULL, state, T1, T2, puniqueid="CN", 
	plotid="plot_id", pntid="dot_id", tabtype="PCT", sumunits=FALSE, strata=FALSE, 
	ratio=FALSE, landarea="ALL", pnt.filter=NULL, plt.filter=NULL, unitvar=NULL, 
	unitvar2=NULL, unitarea=NULL, areavar="ACRES", unitcombine=FALSE, stratalut=NULL, 
	strvar="STRATA", getwt=TRUE, getwtvar="P1POINTCNT", stratcombine=FALSE, 
	rowvar=NULL, colvar=NULL, row.add0=FALSE, col.add0=FALSE, domlut=NULL, 
	ratioden="ROWVAR", lutfolder=NULL, allin1=FALSE, savedata=FALSE, 
	outfolder=NULL, outfn=NULL, outfn.pre=NULL, outfn.date=TRUE, overwrite=FALSE, 
	addtitle=TRUE, title.main=NULL, title.ref=NULL, title.rowvar=NULL, 
	title.colvar=NULL, title.unitvar=NULL, title.filter=NULL, returntitle=FALSE, 
	estround=NULL, pseround=NULL, estnull=0, psenull="--", rowlut=NULL, 
	collut=NULL, rawdata=FALSE, gainloss=FALSE, gainloss.vals=NULL, stabbr=NULL){
 
  #####################################################################################
  ## Requirements:
  ## 1. ICE data
  ## 2. In a folder (lutfolder), a comma-delimited file with list of potential 
  ##		domains (table row/columns) - domlut.csv
  ##		Includes 3 columns:
  ##			DOMCODE (variable name with codes)
  ##			DOMNAME (variable name with code descriptions
  ##			DOMTITLE (pretty name for title in tables, if saving to file)
  ##	
  ## DESCRIPTION: Defines the following parameters for modPB():
  ## 1. Import ICE data (and plot-level data, with strata info)
  ## 2. Import NAIP schedule from lutfolder and generate title.ref
  ## 3. Import domlut
  ## 4. Set pnt.nonsamp.filter, excluding codes that are uninterpretable (ex. chg_ag_2 != 99)
  ## 5. Create 3 new variables, concatenating T1 and T2: 
  ##		use_1_2_NEW, cover_1_2_NEW, use_1_2_FOR) and add names to domlut, if not NULL.
  ## 6. Set domain variable list (domvarlst), if domlut is not NULL, set domvarlst as
  ##		DOMCODE & DOMNAME columns, otherwise set as all variables in ICE data, 
  ##		except plotid, pntid
  ## 7. Define landarea filter.
  ## 8. Check for gainloss parameter. 
  ##		If TRUE, must set rawdata=TRUE and must include rowvar and colvar
  ##
  ## VALUES: 
  ## 1. Table with estimate and percent sampling error, and title.
  ## 2. Table with total number of points per plot, 
  ##		and the proportion and number of points by domain(s) and plot
  ## 3. Table with stratification information: total number of plots by estimation unit, 
  ##		number of plots by strata (and estimation unit), and stratum weight
  ## 4. Table of raw data with calculation steps, for table cells, row totals, column totals
  #####################################################################################

  ## CHECK GUI - IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- ifelse(nargs() == 0, TRUE, FALSE)

  ##################################################################
  ## INITIALIZE SETTINGS AND VARIABLES
  ##################################################################
  use_LUT=cover_LUT=COLOR=chg_ag_2_LUT=change_1_2_LUT=use_1_2_NEW=use_1_2=
	use_1_NEW=use_1=use_2_NEW=use_2=use_1_2_NEW_nm=use_1_2_nm=use_1_NEW_nm=
	use_1_nm=use_2_NEW_nm=use_2_nm=cover_1_2_NEW=cover_1_2=cover_1=
	cover_1_NEW=cover_2_NEW=cover_2=cover_1_2_NEW_nm=cover_1_2_nm=
	cover_1_NEW_nm=cover_1_nm=cover_2_NEW_nm=cover_2_nm=
	use_1_FOR=USE_2_FOR=use_1_2_FOR=use_1_FOR_nm=use_2_FOR=use_2_FOR_nm=
	use_1_2_FOR_nm=n.strata <- NULL



  ## If gui.. set variables to NULL
  if (gui) tabtype=allin1=addtitle=rawtable=savedata <- NULL 

  ## SET OPTIONS
  options.old <- options()
  options(scipen=8) # bias against scientific notation
  on.exit(options(options.old), add=TRUE) 

  ## Get state abbreviation 
  ST <- FIESTA::ref_statecd[FIESTA::ref_statecd$MEANING == state, "ABBR"]
  if (length(ST) == 0) {
    if (is.null(stabbr)) {
      stop("state is not in ref_statecd... include stabbr")
    } else {
      ST <- stabbr
    }
  }   
 
#  oldstates <- c("VT", "NH", "UT", "NV", "NE", "NY", "NJ", "MD", "OH", "WI")
  oldstates <- {}


  ## SET VECTOR OF FILTER VARIABLES TO KEEP
  fvars2keep <- {}
  
  ## 1. IMPORT ICE POINT TABLE (AND PLOT TABLE)
  ##################################################################
  ice <- FIESTA::pcheck.table(icedat)
  setkeyv(ice, c(plotid, pntid))

  pltassgnx <- FIESTA::pcheck.table(pltassgn, tabnm="pltassgn", nullcheck=TRUE, 
	gui=gui)


  #########################################################################
  ## 2. Generate title.ref
  #########################################################################
  if (is.null(title.ref)) title.ref <- paste(state, paste0(T1, "-", T2))


  #########################################################################
  ## 3. IMPORT DOMLUT
  #########################################################################
  if (is.null(domlut)) {
    if (ST %in% oldstates) {
      if (file.exists(paste0(lutfolder, "/domlut_old.csv")))
        domlut <- paste0(lutfolder, "/domlut_old.csv")
    } else {
      if (file.exists(paste0(lutfolder, "/domlut.csv")))
        domlut <- paste0(lutfolder, "/domlut.csv")
    }
  }
  if (!is.null(domlut)) {
    domlut <- FIESTA::pcheck.table(domlut, tabnm="domlut", nullcheck=TRUE, gui=gui)
    domlutnames <- c("DOMCODE", "DOMNAME", "DOMTITLE")
    namesnot <- domlutnames[!which(domlutnames %in% names(domlut))]
    if (length(namesnot) > 0)
      stop("domlut must have column names of DOMCODE, DOMNAME, DOMTITLE")
  }

  #########################################################################
  ## 4. SET pnt.nonsamp.filter
  #########################################################################
  pnt.nonsamp.filter <- NULL
  if (!is.null(rowvar) && rowvar %in% names(ice)) {
    if (rowvar == "change_pnt" | length(grep("chg_ag_2", rowvar)) > 0) {
      pnt.nonsamp.filter <- paste(rowvar, "!= 99")
    } else {
      pnt.nonsamp.filter <- paste(rowvar, "!= 999")
    }
    if (!is.null(colvar) && colvar %in% names(ice)) {
      if (colvar == "change_pnt" | length(grep("chg_ag_2", colvar)) > 0) {
        pnt.nonsamp.filter2 <- paste(colvar, "!= 99")
      } else {
        pnt.nonsamp.filter2 <- paste(colvar, "!= 999")
      }
      pnt.nonsamp.filter <- paste(pnt.nonsamp.filter, "&", pnt.nonsamp.filter2)
    }
  }

  ###############################################################################
  ## 5. DEFINE change variables (use_1_2, cover_1_2, use_1_2_FOR) 
  ###############################################################################
  if ("use_1_NEW" %in% names(ice) && "use_2_NEW" %in% names(ice)) {
    ice[, use_1_2_NEW := paste(use_1, use_2, sep="_to_")]
    if ("use_1_NEW_nm" %in% names(ice) & "use_2_nm" %in% names(ice)) {
      ice[, use_1_2_NEW_nm := paste(use_1_NEW_nm, use_2_nm, sep="_to_")]
  
      ## Add to domlut
      if (!is.null(domlut)) 
        domlut <- rbind(domlut, list("use_1_2_NEW", "use_1_2_NEW_nm", "Use_Change"))
    }
  }
  if ("cover_1_NEW" %in% names(ice) && "cover_2_NEW" %in% names(ice)) {
    ice[, cover_1_2_NEW := paste(cover_1_NEW, cover_2_NEW, sep="_to_")]
    if ("cover_1_NEW_nm" %in% names(ice) & "cover_2_NEW_nm" %in% names(ice)) {
      ice[, cover_1_2_NEW_nm := paste(cover_1_NEW_nm, cover_2_NEW_nm, sep="_to_")]
  
      ## Add to domlut
      if (!is.null(domlut)) 
        domlut <- rbind(domlut, list("cover_1_2_NEW", "cover_1_2_NEW_nm", "Cover_Change"))
    }
  }
  if ("use_1" %in% names(ice) && "use_2" %in% names(ice)) {
    ice[, use_1_2 := paste(use_1, use_2, sep="_to_")]
    if ("use_1_nm" %in% names(ice) & "use_2_nm" %in% names(ice)) {
      ice[, use_1_2_nm := paste(use_1_nm, use_2_nm, sep="_to_")]
  
      ## Add to domlut
      if (!is.null(domlut)) 
        domlut <- rbind(domlut, list("use_1_2", "use_1_2_nm", "Use_Change"))
    }
  }
  if ("cover_1" %in% names(ice) && "cover_2" %in% names(ice)) {
    ice[, cover_1_2 := paste(cover_1, cover_2, sep="_to_")]
    if ("cover_1_nm" %in% names(ice) & "cover_2_nm" %in% names(ice)) {
      ice[, cover_1_2_nm := paste(cover_1_nm, cover_2_nm, sep="_to_")]
  
      ## Add to domlut
      if (!is.null(domlut)) 
        domlut <- rbind(domlut, list("cover_1_2", "cover_1_2_nm", "Cover_Change"))
    }
  }

  if ("use_1_FOR" %in% names(ice) && "use_2_FOR" %in% names(ice)) {
    ice[, use_1_2_FOR:= paste(use_1_FOR, use_2_FOR, sep="_to_")]
    if ("use_1_FOR_nm" %in% names(ice) & "use_2_FOR_nm" %in% names(ice)) {
      ice[, use_1_2_FOR_nm := paste(use_1_FOR_nm, use_2_FOR_nm, sep="_to_")]
  
      ## Add to domlut
      if (!is.null(domlut)) 
        domlut <- rbind(domlut, list("use_1_2_FOR", "use_1_2_FOR_nm", "Use_FOR_Change"))
    }
  }

  #########################################################################
  ## 6. SET DOMAIN VARIABLE LIST
  #########################################################################
  if (is.null(domlut)) {
    domvarlst <- names(ice)[!names(ice) %in% c(plotid, pntid)]
  } else {
    domvarlst <- c(domlut$DOMCODE, domlut$DOMNAME)
  }


  #########################################################################
  ## 7. Define landarea filter
  #########################################################################
  ## Check landarea and for necessary landarea variables in cond table and 
  ## create landarea.filter, which is applied later
  ###########################################################
  landarealst <- c("ALL", "CHANGE")
  landarea <- FIESTA::pcheck.varchar(var2check=landarea, varnm="landarea", gui=gui,
	checklst=landarealst, caption="Sample land area?")

  landarea.filter <- NULL
  if (landarea == "CHANGE") 
    landarea.filter <- "change_1_2 == 1"


 
  #########################################################################
  ## GET ESTIMATE AND RAW DATA
  #########################################################################
  est <- modPB(pnt=ice, pltassgn=pltassgnx, pntid=pntid, plotid=plotid, 
	sumunits=sumunits, puniqueid=puniqueid, tabtype=tabtype, strata=strata, 
	ratio=ratio, landarea=landarea, landarea.filter=landarea.filter, 
	pnt.filter=pnt.filter, pnt.nonsamp.filter=pnt.nonsamp.filter, plt.filter=plt.filter, 
	unitvar=unitvar, unitarea=unitarea, areavar=areavar, unitcombine=unitcombine,
	stratalut=stratalut, strvar=strvar, getwt=getwt, getwtvar=getwtvar, 
	stratcombine=stratcombine, rowvar=rowvar, colvar=colvar, row.orderby=NULL, 
	col.orderby=NULL, row.add0=row.add0, col.add0=col.add0, rowlut=rowlut, 
	collut=collut, domlut=domlut, domvarlst=domvarlst, allin1=allin1, savedata=savedata, 
	outfolder=outfolder, outfn=outfn, outfn.pre=outfn.pre, outfn.date=outfn.date,
	overwrite=overwrite, addtitle=addtitle, title.main=title.main, title.ref=title.ref, 
	title.rowvar=title.rowvar, title.colvar=title.colvar, title.unitvar=title.unitvar, 
	title.filter=title.filter, returntitle=returntitle, estround=estround, 
	pseround=pseround, estnull=estnull, psenull=psenull, rawdata=rawdata, 
	gainloss=gainloss, gainloss.vals=gainloss.vals, gui=gui)


  return(est)
}
    