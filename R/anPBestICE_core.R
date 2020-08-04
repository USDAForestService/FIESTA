anPBestICE_core <- function(icedat, state=NULL, T1, T2, pltassgn=NULL, puniqueid="CN", 
	plotid="plot_id", pntid="dot_id", tabtype="PCT", sumunits=FALSE, strata=FALSE, 
	stratalut=NULL, unitvar=NULL, unitvar2=NULL, unitarea=NULL, unitcombine=FALSE, 
	strvar=NULL, getwt=TRUE, getwtvar="P1POINTCNT", stratcombine=TRUE, lutfolder="_luts", 
	savedata=TRUE, returntitle=TRUE, outfolder=NULL, outfn.date=TRUE, overwrite=FALSE, 
	rawdata=TRUE, pnt1=FALSE, estname=NULL, stabbr=NULL){


  ##################################################################
  ## INITIALIZE SETTINGS AND VARIABLES
  ##################################################################

  ## Set global variables
  #datstrat=datSTRATA=datACRES <- NULL

  ## CHECK GUI - IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- FALSE

  ## Get state abbreviation 
  state <- pcheck.states(state)

  ## Get state abbreviation 
  ST <- pcheck.states(state, statereturn="ABBR")
  if (is.null(ST)) {
    if (is.null(stabbr)) {
      stop("state is not in ref_statecd... include stabbr")
    } else {
      ST <- stabbr
    }
  }


  #########################################################################
  ## IMPORT NAIP SCHEDULE AND GENERATE title.ref
  ##################################################################
  title.ref <- paste(state, paste0(T1, "-", T2))
  if (pnt1) title.ref <- paste(title.ref, "- 1pt")
  if (!is.null(estname)) title.ref <- paste(title.ref, estname, sep=" - ")

  areavar <- "ACRES"
  oldstates <- c("VT", "NH", "UT", "NV", "NE", "NY", "NJ", "MD", "OH", "WI")
  oldstates <- {}
  coverlutfn <- ifelse(ST %in% oldstates, "cover_LUT_old", "cover_LUT")

  coverlutfn <- "cover_LUT"
  coverlut <- paste0(lutfolder, "/", coverlutfn, ".csv")
  coverlut <- data.table::fread(coverlut)

  uselutfn <- ifelse(ST %in% oldstates, "use_LUT_old", "use_LUT")
  uselutfn <- "use_LUT"
  uselut <- paste0(lutfolder, "/", uselutfn, ".csv")
  uselut <- data.table::fread(uselut)

  chgaglutfn <- ifelse(ST %in% oldstates, "chg_ag_2_LUT_old", "chg_ag_2_LUT")
  chgaglutfn <- "chg_ag_2_LUT"
  chgaglut <- paste0(lutfolder, "/", chgaglutfn, ".csv")
  chgaglut <- data.table::fread(chgaglut)

  if (!file.exists(outfolder)) dir.create(outfolder)
  outfolder <- pcheck.outfolder(outfolder)


  ###############################################################################
  ## 01. Estimates of change_pnt on all lands; Vermont 2012-2014
  ## Percent changed.
  ###############################################################################
  rowvar <- "change_pnt"
  outfn.pre <- ifelse(pnt1, paste("01", ST, "1pt", sep="_"), paste("01", ST, sep="_"))
  if (!is.null(estname)) outfn.pre <- paste(outfn.pre, estname, sep="_")
  outfn.pre <- paste(outfn.pre, T1, T2, sep="_") 

  est.change_pnt <- anPBestICE(icedat=icedat, pltassgn=pltassgn, state=state, 
	strata=strata, puniqueid=puniqueid, plotid=plotid, pntid=pntid, tabtype=tabtype, 
	unitarea=unitarea, unitvar=unitvar, stratalut=stratalut, strvar=strvar, 
	stratcombine=stratcombine, rowvar=rowvar, lutfolder=lutfolder, savedata=savedata, 
	outfolder=outfolder, outfn.date=outfn.date, overwrite=overwrite, 
	outfn.pre=outfn.pre, rawdata=rawdata, title.ref=title.ref, returntitle=returntitle, 
	sumunits=sumunits, stabbr=stabbr)
  est.change_pnt


  ###############################################################################
  ## 02. Estimates of chg_ag_2_NEW on all lands; Vermont 2012-2014
  ## Percent changed by change agent
  ###############################################################################
  rowvar <- ifelse(ST %in% oldstates, "chg_ag_2_NEWGRP", "chg_ag_2_GRP")
  rowvarnm <- paste(rowvar, "nm", sep="_")
  rowlut <- unique(chgaglut[!get(rowvar) %in% c(99), c(rowvar, rowvarnm), with=FALSE])
  rowlut <- rowlut[rowlut[[rowvar]] != 0,]
  pnt.filter <- "change_pnt == 1"
  outfn.pre <- ifelse(pnt1, paste("02", ST, "1pt", sep="_"), paste("02", ST, sep="_"))
  if (!is.null(estname)) outfn.pre <- paste(outfn.pre, estname, sep="_")
  outfn.pre <- paste(outfn.pre, T1, T2, sep="_") 

  est.chg_ag_2grp <- anPBestICE(icedat=icedat, pltassgn=pltassgn, state=state,
	strata=strata, puniqueid=puniqueid, plotid=plotid, pntid=pntid, tabtype=tabtype, 
	unitarea=unitarea, unitvar=unitvar, stratalut=stratalut, strvar=strvar, 
	stratcombine=stratcombine, rowvar=rowvar, lutfolder=lutfolder, savedata=savedata,
 	outfolder=outfolder, outfn.date=outfn.date, overwrite=overwrite, 
	outfn.pre=outfn.pre, rawdata=rawdata, title.ref=title.ref, pnt.filter=pnt.filter, 
	row.add0=TRUE, rowlut=rowlut, returntitle=returntitle, sumunits=sumunits, 
	stabbr=stabbr)
  est.chg_ag_2grp

 
  ###############################################################################
  ## 03. Estimates of land use at Time 1 on all lands; Vermont 2012-2014
  ###############################################################################
  rowvar <- ifelse(ST %in% oldstates, "use_1_NEW", "use_1")

  rowvar2 <- sub("_1", "", rowvar)
  rowvar2nm <- paste(rowvar2, "nm", sep="_")
  rowlut <- unique(uselut[!get(rowvar2) %in% c(-1,0,999), c(rowvar2, rowvar2nm), 
			with=FALSE])
  names(rowlut) <- c(rowvar, paste(rowvar, "nm", sep="_"))
  outfn.pre <- ifelse(pnt1, paste("03", ST, "1pt", sep="_"), paste("03", ST, sep="_"))
  if (!is.null(estname)) outfn.pre <- paste(outfn.pre, estname, sep="_")
  outfn.pre <- paste(outfn.pre, T1, T2, sep="_") 

  use_1 <- anPBestICE(icedat=icedat, pltassgn=pltassgn, state=state,
	strata=strata, puniqueid=puniqueid, plotid=plotid, pntid=pntid, tabtype=tabtype, 
	unitarea=unitarea, unitvar=unitvar, stratalut=stratalut, strvar=strvar, 
	stratcombine=stratcombine, rowvar=rowvar, lutfolder=lutfolder, savedata=savedata, 
	outfolder=outfolder, outfn.date=outfn.date, overwrite=overwrite, 
	outfn.pre=outfn.pre, rawdata=rawdata, title.ref=title.ref, row.add0=TRUE, 
	rowlut=rowlut, returntitle=returntitle, sumunits=sumunits, stabbr=stabbr)
  #use_1


  ###############################################################################
  ## 04. Estimates of land use at Time 2 on all lands; Vermont 2012-2014
  ###############################################################################
  rowvar <- ifelse(ST %in% oldstates, "use_2_NEW", "use_2")

  rowvar2 <- sub("_2", "", rowvar)
  rowvar2nm <- paste(rowvar2, "nm", sep="_")
  rowlut <- unique(uselut[!get(rowvar2) %in% c(-1,0,999), c(rowvar2, rowvar2nm), 
			with=FALSE])
  names(rowlut) <- c(rowvar, paste(rowvar, "nm", sep="_"))
  outfn.pre <- ifelse(pnt1, paste("04", ST, "1pt", sep="_"), paste("04", ST, sep="_"))
  if (!is.null(estname)) outfn.pre <- paste(outfn.pre, estname, sep="_")
  outfn.pre <- paste(outfn.pre, T1, T2, sep="_") 

  use_2 <- anPBestICE(icedat=icedat, pltassgn=pltassgn, state=state,
	strata=strata, puniqueid=puniqueid, plotid=plotid, pntid=pntid, tabtype=tabtype, 
	unitarea=unitarea, unitvar=unitvar, stratalut=stratalut, strvar=strvar, 
	stratcombine=stratcombine, rowvar=rowvar, lutfolder=lutfolder, savedata=savedata, 
	outfolder=outfolder, outfn.date=outfn.date, overwrite=overwrite, outfn.pre=outfn.pre, 
	rawdata=rawdata, title.ref=title.ref, row.add0=TRUE, rowlut=rowlut, 
	returntitle=returntitle, sumunits=sumunits, stabbr=stabbr)
  #use_2


  ###############################################################################
  ## 05. Estimates of land cover at Time 1 all lands; Vermont 2012-2014
  ###############################################################################
  rowvar <- ifelse(ST %in% oldstates, "cover_1_NEW", "cover_1")

  rowvar2 <- sub("_1", "", rowvar)
  rowvar2nm <- paste(rowvar2, "nm", sep="_")
  rowlut <- unique(coverlut[!get(rowvar2) %in% c(-1,0,999), c(rowvar2, rowvar2nm),
 	with=FALSE])
  names(rowlut) <- c(rowvar, paste(rowvar, "nm", sep="_"))
  outfn.pre <- ifelse(pnt1, paste("05", ST, "1pt", sep="_"), paste("05", ST, sep="_"))
  if (!is.null(estname)) outfn.pre <- paste(outfn.pre, estname, sep="_")
  outfn.pre <- paste(outfn.pre, T1, T2, sep="_") 

  cover_1 <- anPBestICE(icedat=icedat, pltassgn=pltassgn, state=state,
	strata=strata, puniqueid=puniqueid, plotid=plotid, pntid=pntid, tabtype=tabtype, 
	unitarea=unitarea, unitvar=unitvar, stratalut=stratalut, strvar=strvar, 
	stratcombine=stratcombine, rowvar=rowvar, lutfolder=lutfolder, savedata=savedata, 
	outfolder=outfolder, outfn.date=outfn.date, overwrite=overwrite, 
	outfn.pre=outfn.pre, rawdata=rawdata, title.ref=title.ref, row.add0=TRUE, 
	rowlut=rowlut, returntitle=returntitle, sumunits=sumunits, stabbr=stabbr)
  #cover_1


  ###############################################################################
  ## 06. Estimates of land cover at Time 2 on all lands; Vermont 2012-2014
  ###############################################################################
  rowvar <- ifelse(ST %in% oldstates, "cover_2_NEW", "cover_2")

  rowvar2 <- sub("_2", "", rowvar)
  rowvar2nm <- paste(rowvar2, "nm", sep="_")
  rowlut <- unique(coverlut[!get(rowvar2) %in% c(-1,0,999), c(rowvar2, rowvar2nm),
 	with=FALSE])
  names(rowlut) <- c(rowvar, paste(rowvar, "nm", sep="_"))
  outfn.pre <- ifelse(pnt1, paste("06", ST, "1pt", sep="_"), paste("06", ST, sep="_"))
  if (!is.null(estname)) outfn.pre <- paste(outfn.pre, estname, sep="_")
  outfn.pre <- paste(outfn.pre, T1, T2, sep="_") 

  cover_2 <- anPBestICE(icedat=icedat, pltassgn=pltassgn, state=state,
	strata=strata, puniqueid=puniqueid, plotid=plotid, pntid=pntid, tabtype=tabtype, 
	unitarea=unitarea, unitvar=unitvar, stratalut=stratalut, strvar=strvar, 
	stratcombine=stratcombine, rowvar=rowvar, lutfolder=lutfolder, savedata=savedata, 
	outfolder=outfolder, outfn.pre=outfn.pre, outfn.date=outfn.date, 
	overwrite=overwrite, rawdata=rawdata, title.ref=title.ref, row.add0=TRUE, 
	rowlut=rowlut, returntitle=returntitle, sumunits=sumunits, stabbr=stabbr)
  #cover_2


  ###############################################################################
  ## 07. Estimates of use_2_FOR by use_1_FOR
  ## Percent of forest land use change.
  ###############################################################################
  rowvar <- "use_1_FOR"  
  colvar <- "use_2_FOR" 
  outfn.pre <- ifelse(pnt1, paste("07", ST, "1pt", sep="_"), paste("07", ST, sep="_"))
  if (!is.null(estname)) outfn.pre <- paste(outfn.pre, estname, sep="_")
  outfn.pre <- paste(outfn.pre, T1, T2, sep="_") 

  use_1_2_FOR <- anPBestICE(icedat=icedat, pltassgn=pltassgn, state=state,  
	strata=strata, puniqueid=puniqueid, plotid=plotid, pntid=pntid, tabtype=tabtype, 
	unitarea=unitarea, unitvar=unitvar, stratalut=stratalut, strvar=strvar, 
	stratcombine=stratcombine, rowvar=rowvar, colvar=colvar, lutfolder=lutfolder, 
	savedata=savedata, outfolder=outfolder, outfn.date=outfn.date, 
	overwrite=overwrite, outfn.pre=outfn.pre, rawdata=rawdata, title.ref=title.ref, 
	returntitle=returntitle, row.add0=TRUE, col.add0=TRUE, gainloss=TRUE, 
	sumunits=sumunits, stabbr=stabbr)
  use_1_2_FOR

  ###############################################################################
  ## 08. Estimates of cover_2_NEWGRP by cover_1_NEWGRP
  ## Percent of land cover change where land use was forest at Time 1.
  ###############################################################################
  rowvar <- ifelse(ST %in% oldstates, "cover_1_NEWGRP", "cover_1_GRP")
  colvar <- ifelse(ST %in% oldstates, "cover_2_NEWGRP", "cover_2_GRP")
  #pnt.filter="use_1_FOR == 1"
  pnt.filter <- NULL
  outfn.pre <- ifelse(pnt1, paste("08", ST, "1pt", sep="_"), paste("08", ST, sep="_"))
  if (!is.null(estname)) outfn.pre <- paste(outfn.pre, estname, sep="_")
  outfn.pre <- paste(outfn.pre, T1, T2, sep="_") 

  rowvar2 <- sub("_1", "", rowvar)
  rowvar2nm <- paste(rowvar2, "nm", sep="_")
  rowlut <- unique(coverlut[!get(rowvar2) %in% c(-1,0,999), c(rowvar2, rowvar2nm),
 			with=FALSE])
  names(rowlut) <- c(rowvar, paste(rowvar, "nm", sep="_"))

  colvar2 <- sub("_2", "", colvar)
  colvar2nm <- paste(colvar2, "nm", sep="_")
  collut <- unique(coverlut[!get(colvar2) %in% c(-1,0,999), c(colvar2, colvar2nm),
 			with=FALSE])
  names(collut) <- c(colvar, paste(colvar, "nm", sep="_"))

  cover_1_2_NEWGRP <- anPBestICE(icedat=icedat, pltassgn=pltassgn, state=state, 
	strata=strata, puniqueid=puniqueid, plotid=plotid, pntid=pntid, tabtype=tabtype, 
	unitarea=unitarea, unitvar=unitvar, stratalut=stratalut, strvar=strvar, 
	stratcombine=stratcombine, rowvar=rowvar, colvar=colvar, pnt.filter=pnt.filter, 
	lutfolder=lutfolder, savedata=savedata, outfolder=outfolder, outfn.pre=outfn.pre, 
	outfn.date=outfn.date, overwrite=overwrite, rawdata=rawdata, title.ref=title.ref, 
	returntitle=returntitle, row.add0=TRUE, col.add0=TRUE, gainloss=TRUE, 
	rowlut=rowlut, collut=collut, sumunits=sumunits, stabbr=stabbr)
  #cover_1_2_NEWGRP



  ###############################################################################
  ## 09. Estimates of use_2_NEW by use_1_NEW
  ## Percent of land use change from Time 1 to Time 2.
  ###############################################################################
  rowvar <- ifelse(ST %in% oldstates, "use_1_NEW", "use_1")
  colvar <- ifelse(ST %in% oldstates, "use_2_NEW", "use_2")

  rowvar2 <- sub("_1", "", rowvar)
  rowvar2nm <- paste(rowvar2, "nm", sep="_")
  rowlut <- unique(uselut[!get(rowvar2) %in% c(-1,0,999), c(rowvar2, rowvar2nm),
 			with=FALSE])
  names(rowlut) <- c(rowvar, paste(rowvar, "nm", sep="_"))

  colvar2 <- sub("_2", "", colvar)
  colvar2nm <- paste(colvar2, "nm", sep="_")
  collut <- unique(uselut[!get(colvar2) %in% c(-1,0,999), c(colvar2, colvar2nm),
 			with=FALSE])
  names(collut) <- c(colvar, paste(colvar, "nm", sep="_"))
  outfn.pre <- ifelse(pnt1, paste("09", ST, "1pt", sep="_"), paste("09", ST, sep="_"))
  if (!is.null(estname)) outfn.pre <- paste(outfn.pre, estname, sep="_")
  outfn.pre <- paste(outfn.pre, T1, T2, sep="_") 

  use_1_2_NEW <- anPBestICE(icedat=icedat, pltassgn=pltassgn, state=state,  
	strata=strata, puniqueid=puniqueid, plotid=plotid, pntid=pntid, tabtype=tabtype, 
	unitarea=unitarea, unitvar=unitvar, stratalut=stratalut, strvar=strvar, 
	stratcombine=stratcombine, rowvar=rowvar, colvar=colvar, lutfolder=lutfolder, 
	savedata=savedata, outfolder=outfolder, outfn.pre=outfn.pre, 
	outfn.date=outfn.date, overwrite=overwrite, rawdata=rawdata, title.ref=title.ref, 
	returntitle=returntitle, row.add0=TRUE, col.add0=TRUE, gainloss=TRUE, 
	rowlut=rowlut, collut=collut, sumunits=sumunits, stabbr=stabbr)
  #use_1_2_NEW


  ###############################################################################
  ## 10. Estimates of cover_2_NEW by cover_1_NEW
  ## Percent of land cover change from Time 1 to Time 2.
  ###############################################################################
  rowvar <- ifelse(ST %in% oldstates, "cover_1_NEW", "cover_1")
  colvar <- ifelse(ST %in% oldstates, "cover_2_NEW", "cover_2")

  rowvar2 <- sub("_1", "", rowvar)
  rowvar2nm <- paste(rowvar2, "nm", sep="_")
  rowlut <- unique(coverlut[!get(rowvar2) %in% c(-1,0,999), c(rowvar2, rowvar2nm),
 			with=FALSE])
  names(rowlut) <- c(rowvar, paste(rowvar, "nm", sep="_"))

  colvar2 <- sub("_2", "", colvar)
  colvar2nm <- paste(colvar2, "nm", sep="_")
  collut <- unique(coverlut[!get(colvar2) %in% c(-1,0,999), c(colvar2, colvar2nm),
 			with=FALSE])
  names(collut) <- c(colvar, paste(colvar, "nm", sep="_"))
  outfn.pre <- ifelse(pnt1, paste("10", ST, "1pt", sep="_"), paste("10", ST, sep="_"))
  if (!is.null(estname)) outfn.pre <- paste(outfn.pre, estname, sep="_")
  outfn.pre <- paste(outfn.pre, T1, T2, sep="_") 

  cover_1_2_NEW <- anPBestICE(icedat=icedat, pltassgn=pltassgn, state=state,  
	strata=strata, puniqueid=puniqueid, plotid=plotid, pntid=pntid, tabtype=tabtype, 
	unitarea=unitarea, unitvar=unitvar, stratalut=stratalut, strvar=strvar, 
	stratcombine=stratcombine, rowvar=rowvar, colvar=colvar, lutfolder=lutfolder, 
	savedata=savedata, outfolder=outfolder, outfn.pre=outfn.pre, 
	outfn.date=outfn.date, overwrite=overwrite, rawdata=rawdata, 
	title.ref=title.ref, returntitle=returntitle, row.add0=TRUE, col.add0=TRUE, 
	gainloss=TRUE, rowlut=rowlut, collut=collut, sumunits=sumunits, stabbr=stabbr)
  #cover_1_2_NEW


  ###############################################################################
  ## 11. Estimates of cover_1_NEW by use_1_NEW
  ## Percent of land cover by land use at Time 1.
  ###############################################################################
  rowvar <- ifelse(ST %in% oldstates, "use_1_NEW", "use_1")
  colvar <- ifelse(ST %in% oldstates, "cover_1_NEW", "cover_1")

  rowvar2 <- sub("_1", "", rowvar)
  rowvar2nm <- paste(rowvar2, "nm", sep="_")
  rowlut <- unique(uselut[!get(rowvar2) %in% c(-1,0,999), c(rowvar2, rowvar2nm),
 			with=FALSE])
  names(rowlut) <- c(rowvar, paste(rowvar, "nm", sep="_"))

  colvar2 <- sub("_1", "", colvar)
  colvar2nm <- paste(colvar2, "nm", sep="_")
  collut <- unique(coverlut[!get(colvar2) %in% c(-1,0,999), c(colvar2, colvar2nm),
 			with=FALSE])
  names(collut) <- c(colvar, paste(colvar, "nm", sep="_"))
  outfn.pre <- ifelse(pnt1, paste("11", ST, "1pt", sep="_"), paste("11", ST, sep="_"))
  if (!is.null(estname)) outfn.pre <- paste(outfn.pre, estname, sep="_")
  outfn.pre <- paste(outfn.pre, T1, T2, sep="_") 

  use_1_cover_1_NEW <- anPBestICE(icedat=icedat, pltassgn=pltassgn, state=state, 
	strata=strata, puniqueid=puniqueid, plotid=plotid, pntid=pntid, tabtype=tabtype, 
	unitarea=unitarea, unitvar=unitvar, stratalut=stratalut, strvar=strvar, 
	stratcombine=stratcombine, rowvar=rowvar, colvar=colvar, lutfolder=lutfolder, 
	savedata=savedata, outfolder=outfolder, outfn.pre=outfn.pre, 
	outfn.date=outfn.date, overwrite=overwrite, rawdata=rawdata, 
	title.ref=title.ref, returntitle=returntitle, row.add0=TRUE, col.add0=TRUE, 
	rowlut=rowlut, collut=collut, sumunits=sumunits, stabbr=stabbr)
  #use_1_cover_1_NEW


  ###############################################################################
  ## 12. Estimates of cover_2_NEW by use_2_NEW
  ## Percent of land cover by land use at Time 2.
  ###############################################################################
  rowvar <- ifelse(ST %in% oldstates, "use_2_NEW", "use_2")
  colvar <- ifelse(ST %in% oldstates, "cover_2_NEW", "cover_2")

  rowvar2 <- sub("_2", "", rowvar)
  rowvar2nm <- paste(rowvar2, "nm", sep="_")
  rowlut <- unique(uselut[!get(rowvar2) %in% c(-1,0,999), c(rowvar2, rowvar2nm),
 			with=FALSE])
  names(rowlut) <- c(rowvar, paste(rowvar, "nm", sep="_"))

  colvar2 <- sub("_2", "", colvar)
  colvar2nm <- paste(colvar2, "nm", sep="_")
  collut <- unique(coverlut[!get(colvar2) %in% c(-1,0,999), c(colvar2, colvar2nm),
 			with=FALSE])
  names(collut) <- c(colvar, paste(colvar, "nm", sep="_"))
  outfn.pre <- ifelse(pnt1, paste("12", ST, "1pt", sep="_"), paste("12", ST, sep="_"))
  if (!is.null(estname)) outfn.pre <- paste(outfn.pre, estname, sep="_")
  outfn.pre <- paste(outfn.pre, T1, T2, sep="_") 

  use_2_cover_2_NEW <- anPBestICE(icedat=icedat, pltassgn=pltassgn, state=state, 
	strata=strata, puniqueid=puniqueid, plotid=plotid, pntid=pntid, tabtype=tabtype, 
	unitarea=unitarea, unitvar=unitvar, stratalut=stratalut, strvar=strvar, 
	stratcombine=stratcombine, rowvar=rowvar, colvar=colvar, lutfolder=lutfolder, 
	savedata=savedata, outfolder=outfolder, outfn.pre=outfn.pre, outfn.date=outfn.date, 
	overwrite=overwrite, rawdata=rawdata, title.ref=title.ref, returntitle=returntitle, 
	row.add0=TRUE, col.add0=TRUE, rowlut=rowlut, collut=collut, sumunits=sumunits, 
	stabbr=stabbr)
  #use_2_cover_2_NEW


  ###############################################################################
  ## 13. Ratio estimates of use_2_NEW within use_1_NEW
  ## Percent change of land use at Time 1 within land use at Time 2.
  ###############################################################################
  rowvar <- ifelse(ST %in% oldstates, "use_1_NEW", "use_1")
  colvar <- ifelse(ST %in% oldstates, "use_2_NEW", "use_2")

  rowvar2 <- sub("_1", "", rowvar)
  rowvar2nm <- paste(rowvar2, "nm", sep="_")
  rowlut <- unique(uselut[!get(rowvar2) %in% c(-1,0,999), c(rowvar2, rowvar2nm), 
			with=FALSE])
  names(rowlut) <- c(rowvar, paste(rowvar, "nm", sep="_"))

  colvar2 <- sub("_2", "", colvar)
  colvar2nm <- paste(colvar2, "nm", sep="_")
  collut <- unique(uselut[!get(colvar2) %in% c(-1,0,999), c(colvar2, colvar2nm), 
			with=FALSE])
  names(collut) <- c(colvar, paste(colvar, "nm", sep="_"))
  outfn.pre <- ifelse(pnt1, paste("13", ST, "1pt", sep="_"), paste("13", ST, sep="_"))
  if (!is.null(estname)) outfn.pre <- paste(outfn.pre, estname, sep="_")
  outfn.pre <- paste(outfn.pre, T1, T2, sep="_") 

  rat1 <- anPBestICE(icedat=icedat, pltassgn=pltassgn, state=state, ratio=TRUE, 
	strata=strata, puniqueid=puniqueid, plotid=plotid, pntid=pntid, tabtype=tabtype, 
	unitarea=unitarea, unitvar=unitvar, stratalut=stratalut, strvar=strvar, 
	stratcombine=stratcombine, rowvar=rowvar, colvar=colvar, lutfolder=lutfolder, 
	savedata=savedata, outfolder=outfolder, outfn.pre=outfn.pre, outfn.date=outfn.date, 
	overwrite=overwrite, rawdata=rawdata, title.ref=title.ref, returntitle=returntitle, 
	row.add0=TRUE, col.add0=TRUE, rowlut=rowlut, collut=collut, sumunits=sumunits, 
	stabbr=stabbr)
  #rat1



  ###############################################################################
  ## 13a. Ratio estimates of use_2_NEW within use_1_NEW
  ## Percent change of land use at Time 1 within land use at Time 2.
  ###############################################################################
  nbr <- "13a"
  rowvar <- ifelse(ST %in% oldstates, "use_2_NEW", "use_2")
  colvar <- ifelse(ST %in% oldstates, "cover_2_NEW", "cover_2")

  rowvar2 <- sub("_2", "", rowvar)
  rowvar2nm <- paste(rowvar2, "nm", sep="_")
  rowlut <- unique(uselut[!get(rowvar2) %in% c(-1,0,999), c(rowvar2, rowvar2nm), 
			with=FALSE])
  names(rowlut) <- c(rowvar, paste(rowvar, "nm", sep="_"))

  colvar2 <- sub("_2", "", colvar)
  colvar2nm <- paste(colvar2, "nm", sep="_")
  collut <- unique(coverlut[!get(colvar2) %in% c(-1,0,999), c(colvar2, colvar2nm),
 			with=FALSE])
  names(collut) <- c(colvar, paste(colvar, "nm", sep="_"))

  outfn.pre <- ifelse(pnt1, paste(nbr, ST, "1pt", sep="_"), paste(nbr, ST, sep="_"))
  if (!is.null(estname)) outfn.pre <- paste(outfn.pre, estname, sep="_")
  outfn.pre <- paste(outfn.pre, T1, T2, sep="_") 

  rat1 <- anPBestICE(icedat=icedat, pltassgn=pltassgn, state=state, ratio=TRUE, 
	strata=strata, puniqueid=puniqueid, plotid=plotid, pntid=pntid, tabtype=tabtype, 
	unitarea=unitarea, unitvar=unitvar, stratalut=stratalut, strvar=strvar, 
	stratcombine=stratcombine, rowvar=rowvar, colvar=colvar, lutfolder=lutfolder, 
	savedata=savedata, outfolder=outfolder, outfn.pre=outfn.pre, outfn.date=outfn.date, 
	overwrite=overwrite, rawdata=rawdata, title.ref=title.ref, returntitle=returntitle, 
	row.add0=TRUE, col.add0=TRUE, rowlut=rowlut, collut=collut, sumunits=sumunits, 
	stabbr=stabbr)
  rat1

#source("C:\\_tsf\\_GitHub\\FIESTA\\R\\datBarStacked.R")
#est <- rat1$raw$unit.grpest
#datBarStacked(est, "use_2_nm", "cover_2_nm", response="est",
#		main.order=rev(uselut$use_nm[-1]), sub.order=collut$cover_2_nm,
#		savedata=TRUE, outfolder=outfolder, 
#		device.width=12, bar.lim=c(0,75), bar.ratio=0.70)
#		legend.fit=TRUE, legend.inset=.9)

#x=est
#main.attribute="use_2_nm" 
#sub.attribute="cover_2_nm" 
#response="phat.n"
#percent=TRUE
#legend.fit=TRUE
#main.order=uselut$use_nm[-1]
#sub.order=collut$cover_2_nm

  ###############################################################################
  ## 14. Ratio estimates of cover_2_NEW within cover_1_NEW
  ## Percent change of land cover at Time 1 within land use at Time 2.
  ###############################################################################
  rowvar <- ifelse(ST %in% oldstates, "cover_1_NEW", "cover_1")
  colvar <- ifelse(ST %in% oldstates, "cover_2_NEW", "cover_2")

  rowvar2 <- sub("_1", "", rowvar)
  rowvar2nm <- paste(rowvar2, "nm", sep="_")
  rowlut <- unique(coverlut[!get(rowvar2) %in% c(-1,0,999), c(rowvar2, rowvar2nm),
 			with=FALSE])
  names(rowlut) <- c(rowvar, paste(rowvar, "nm", sep="_"))

  colvar2 <- sub("_2", "", colvar)
  colvar2nm <- paste(colvar2, "nm", sep="_")
  collut <- unique(coverlut[!get(colvar2) %in% c(-1,0,999), c(colvar2, colvar2nm),
 			with=FALSE])
  names(collut) <- c(colvar, paste(colvar, "nm", sep="_"))
  outfn.pre <- ifelse(pnt1, paste("14", ST, "1pt", sep="_"), paste("14", ST, sep="_"))
  if (!is.null(estname)) outfn.pre <- paste(outfn.pre, estname, sep="_")
  outfn.pre <- paste(outfn.pre, T1, T2, sep="_") 

  rat2 <- anPBestICE(icedat=icedat, pltassgn=pltassgn, state=state, ratio=TRUE, 
	strata=strata, puniqueid=puniqueid, plotid=plotid, pntid=pntid, tabtype=tabtype, 
	unitarea=unitarea, unitvar=unitvar, stratalut=stratalut, strvar=strvar, 
	stratcombine=stratcombine, rowvar=rowvar, colvar=colvar, lutfolder=lutfolder, 
	savedata=savedata, outfolder=outfolder, outfn.pre=outfn.pre, outfn.date=outfn.date, 
	overwrite=overwrite, rawdata=rawdata, title.ref=title.ref, returntitle=returntitle, 
	row.add0=TRUE, col.add0=TRUE, rowlut=rowlut, collut=collut, sumunits=sumunits, 
	stabbr=stabbr)
  #rat2


  ###############################################################################
  ## 15. Ratio estimates of use_1_NEW within chg_ag_2_NEWGRP
  ## Percent change of land use at Time 1 within within change agent group.
  ###############################################################################
  rowvar <- ifelse(ST %in% oldstates, "chg_ag_2_NEWGRP", "chg_ag_2_GRP")
  colvar <- ifelse(ST %in% oldstates, "use_1_NEW", "use_1")
  pnt.filter <- ifelse(ST %in% oldstates, "use_1_NEW != use_2_NEW", "use_1 != use_2")

  rowvarnm <- paste(rowvar, "nm", sep="_")
  rowlut <- unique(chgaglut[!get(rowvar) %in% c(99), c(rowvar, rowvarnm), with=FALSE])

  colvar2 <- sub("_1", "", colvar)
  colvar2nm <- paste(colvar2, "nm", sep="_")
  collut <- unique(uselut[!get(colvar2) %in% c(-1,0,999), c(colvar2, colvar2nm), 
			with=FALSE])
  names(collut) <- c(colvar, paste(colvar, "nm", sep="_"))
  outfn.pre <- ifelse(pnt1, paste("15", ST, "1pt", sep="_"), paste("15", ST, sep="_"))
  if (!is.null(estname)) outfn.pre <- paste(outfn.pre, estname, sep="_")
  outfn.pre <- paste(outfn.pre, T1, T2, sep="_") 

  rat3 <- anPBestICE(icedat=icedat, pltassgn=pltassgn, state=state, ratio=TRUE, 
	strata=strata, puniqueid=puniqueid, plotid=plotid, pntid=pntid, tabtype=tabtype, 
	unitarea=unitarea, unitvar=unitvar, stratalut=stratalut, strvar=strvar, 
	stratcombine=stratcombine, rowvar=rowvar, colvar=colvar, lutfolder=lutfolder, 
	savedata=savedata, outfolder=outfolder, outfn.pre=outfn.pre, outfn.date=outfn.date, 
	overwrite=overwrite, rawdata=rawdata, title.ref=title.ref, returntitle=returntitle, 
	pnt.filter=pnt.filter, row.add0=TRUE, col.add0=TRUE, rowlut=rowlut, collut=collut, 
	sumunits=sumunits, stabbr=stabbr)
  #rat3


  ###############################################################################
  ## 16. Ratio estimates of cover_1_NEW within chg_ag_2_NEWGRP
  ## Percent change of land cover at Time 1 within change agent group.
  ###############################################################################
  rowvar <- ifelse(ST %in% oldstates, "chg_ag_2_NEWGRP", "chg_ag_2_GRP")
  colvar <- ifelse(ST %in% oldstates, "cover_1_NEW", "cover_1")
  pnt.filter <- ifelse(ST %in% oldstates, "cover_1_NEW != cover_2_NEW", 
		"cover_1 != cover_2")

  rowvarnm <- paste(rowvar, "nm", sep="_")
  rowlut <- unique(chgaglut[!get(rowvar) %in% c(99) , c(rowvar, rowvarnm), with=FALSE])

  colvar2 <- sub("_1", "", colvar)
  colvar2nm <- paste(colvar2, "nm", sep="_")
  collut <- unique(coverlut[!get(colvar2) %in% c(-1,0,999), c(colvar2, colvar2nm),
 			with=FALSE])
  names(collut) <- c(colvar, paste(colvar, "nm", sep="_"))
  outfn.pre <- ifelse(pnt1, paste("16", ST, "1pt", sep="_"), paste("16", ST, sep="_"))
  if (!is.null(estname)) outfn.pre <- paste(outfn.pre, estname, sep="_")
  outfn.pre <- paste(outfn.pre, T1, T2, sep="_") 

  rat4 <- anPBestICE(icedat=icedat, pltassgn=pltassgn, state=state, ratio=TRUE, 
	strata=strata, puniqueid=puniqueid, plotid=plotid, pntid=pntid, tabtype=tabtype, 
	unitarea=unitarea, unitvar=unitvar, stratalut=stratalut, strvar=strvar, 
	stratcombine=stratcombine, rowvar=rowvar, colvar=colvar, lutfolder=lutfolder, 
	savedata=savedata, outfolder=outfolder, outfn.pre=outfn.pre, outfn.date=outfn.date, 
	overwrite=overwrite, rawdata=rawdata, title.ref=title.ref, returntitle=returntitle, 
	pnt.filter=pnt.filter, row.add0=TRUE, col.add0=TRUE, rowlut=rowlut, collut=collut, 
	sumunits=sumunits, stabbr=stabbr)
  #rat4


  ###############################################################################
  ## 17. Transitions - Land Use
  ## Percent change of Land Use from Time 1 to Time 2.
  ###############################################################################
  rowvar <- "use_1_2_nm"
  pnt.filter <- "use_1 != use_2"

  outfn.pre <- ifelse(pnt1, paste("17", ST, "1pt", sep="_"), paste("17", ST, sep="_"))
  if (!is.null(estname)) outfn.pre <- paste(outfn.pre, estname, sep="_")
  outfn.pre <- paste(outfn.pre, T1, T2, sep="_") 

  est.use_1_2 <- anPBestICE(icedat=icedat, pltassgn=pltassgn, state=state, 
	strata=strata, puniqueid=puniqueid, plotid=plotid, pntid=pntid, tabtype=tabtype, 
	unitarea=unitarea, unitvar=unitvar, stratalut=stratalut, strvar=strvar, 
	stratcombine=stratcombine, rowvar=rowvar, lutfolder=lutfolder, savedata=savedata, 
	outfolder=outfolder, outfn.pre=outfn.pre, outfn.date=outfn.date, 
	overwrite=overwrite, rawdata=rawdata, title.ref=title.ref, 
	returntitle=returntitle, sumunits=sumunits, pnt.filter=pnt.filter, stabbr=stabbr)
  #use_1_2 <- est.use_1_2$raw$unit.rowest


  ###############################################################################
  ## 18. Transitions - Land Cover
  ## Percent change of Land Cover from Time 1 to Time 2.
  ###############################################################################
  rowvar <- "cover_1_2_nm"
  pnt.filter <- "cover_1 != cover_2"

  outfn.pre <- ifelse(pnt1, paste("18", ST, "1pt", sep="_"), paste("18", ST, sep="_"))
  if (!is.null(estname)) outfn.pre <- paste(outfn.pre, estname, sep="_")
  outfn.pre <- paste(outfn.pre, T1, T2, sep="_") 

  est.cover_1_2 <- anPBestICE(icedat=icedat, pltassgn=pltassgn, state=state, 
	strata=strata, puniqueid=puniqueid, plotid=plotid, pntid=pntid, tabtype=tabtype, 
	unitarea=unitarea, unitvar=unitvar, stratalut=stratalut, strvar=strvar, 
	stratcombine=stratcombine, rowvar=rowvar, lutfolder=lutfolder, savedata=savedata, 
	outfolder=outfolder, outfn.pre=outfn.pre, outfn.date=outfn.date, 
	overwrite=overwrite, rawdata=rawdata, title.ref=title.ref, returntitle=returntitle, 
	sumunits=sumunits, pnt.filter=pnt.filter, stabbr=stabbr)
  #cover_1_2 <- est.cover_1_2$raw$unit.rowest


  ###############################################################################
  ## 19. Transitions - Land Use - Forest/Nonforest
  ## Percent change of Land Use (Forest/Nonforest) from Time 1 to Time 2.
  ###############################################################################
  rowvar <- "use_1_2_FOR_nm"
  pnt.filter <- "use_1_FOR != use_2_FOR"

  outfn.pre <- ifelse(pnt1, paste("19", ST, "1pt", sep="_"), paste("19", ST, sep="_"))
  if (!is.null(estname)) outfn.pre <- paste(outfn.pre, estname, sep="_")
  outfn.pre <- paste(outfn.pre, T1, T2, sep="_") 

  est.use_1_2_FOR <- anPBestICE(icedat=icedat, pltassgn=pltassgn, state=state, 
	strata=strata, puniqueid=puniqueid, plotid=plotid, pntid=pntid, tabtype=tabtype, 
	unitarea=unitarea, unitvar=unitvar, stratalut=stratalut, strvar=strvar, 
	stratcombine=stratcombine, rowvar=rowvar, lutfolder=lutfolder, savedata=savedata, 
	outfolder=outfolder, outfn.pre=outfn.pre, outfn.date=outfn.date, 
	overwrite=overwrite, rawdata=rawdata, title.ref=title.ref, returntitle=returntitle, 
	sumunits=sumunits, pnt.filter=pnt.filter, stabbr=stabbr)
  #use_1_2_FOR <- est.use_1_2_FOR$raw$unit.rowest
 
}

