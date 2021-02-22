anPBpopICE_core <- function(ice.pntfn, ice.plotfn=NULL, AOInm, 
	T1, T2, plotid="plot_id", pntid="dot_cnt", pltassgn=NULL, 
	pltassgnid=NULL, unitvar=NULL, unitvar2=NULL, unitarea=NULL, 
	areavar="ACRES", unitcombine=FALSE, strata=FALSE, stratalut=NULL, 
	strvar=NULL, getwt=TRUE, getwtvar="P1POINTCNT", stratcombine=TRUE, 
	tabtype="PCT", sumunits=FALSE, rawdata=TRUE, returntitle=TRUE, 
	outfolder=NULL, outfn.pre=NULL, outfn.date=TRUE, overwrite=FALSE, 
	pnt1=FALSE, domlut=NULL, PBpopdatICE=NULL, ...){


  ##################################################################
  ## INITIALIZE SETTINGS AND VARIABLES
  ##################################################################

  ## Set global variables
  #datstrat=datSTRATA=datACRES <- NULL

  ## CHECK GUI - IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- FALSE
  savedata <- TRUE


  ## Check outfolder
  ###########################################################
  outfolder <- pcheck.outfolder(outfolder)
  outfolder <- paste0(outfolder, "/estimates")
  if (!dir.exists(outfolder)) dir.create(outfolder)

  ## Create outfn.pre
  ###########################################################
  if (is.null(outfn.pre)) outfn.pre <- AOInm

  ## Generate title.ref
  ###########################################################
  title.ref <- paste0(AOInm, paste0(", ", T1, "-", T2))
  if (pnt1) title.ref <- paste(title.ref, "- 1pt")



  #########################################################################
  ## Get ICE population data
  #########################################################################
  if (is.null(PBpopdatICE)) {
    PBpopdatICE <- anPBpopICE(ice.pntfn=ice.pntfn, ice.plotfn=ice.plotfn, 
		T1=T1, T2=T2, plotid="plot_id", pntid="dot_cnt", 
		puniqueid=plotid, pltassgn=pltassgn, pltassgnid=pltassgnid, 
		unitarea=unitarea, unitcombine=unitcombine, 
		strata=strata, strvar=strvar, getwt=getwt, getwtvar=getwtvar, 
		stratcombine=stratcombine, sumunits=sumunits, ...)
  } 
  changelut <- setDT(PBpopdatICE$reflst$changelut)
  coverlut <- setDT(PBpopdatICE$reflst$coverlut)
  uselut <- setDT(PBpopdatICE$reflst$uselut)
  agentlut <- setDT(PBpopdatICE$reflst$agentlut)

  plotid <- PBpopdatICE$plotid
  pntid <- PBpopdatICE$pntid

  if (is.null(domlut))
    domlut <- PBpopdatICE$domlut 


  ###############################################################################
  ## 01. Estimates of change_pnt on all lands
  ## Percent changed.
  ###############################################################################
  outfn.pre2 <- paste0("01_", outfn.pre)
  rowvar <- "change_pnt"

  est.change_pnt <- modPB(PBpopdat=PBpopdatICE, tabtype=tabtype, 
	rowvar=rowvar, sumunits=sumunits, domlut=domlut, 
	savedata=savedata, outfolder=outfolder, outfn.date=outfn.date, 
	overwrite=overwrite, outfn.pre=outfn.pre2, rawdata=rawdata, 
	title.ref=title.ref, returntitle=returntitle)


  ###############################################################################
  ## 02. Estimates of chg_ag_2_GRP on all lands
  ## Percent change by change agent
  ###############################################################################
  outfn.pre2 <- paste0("02_", outfn.pre)
  rowvar <- "chg_ag_2_GRP"
  rowvarnm <- paste(rowvar, "nm", sep="_")
  rowlut <- unique(agentlut[!agentlut[[rowvar]] %in% c(99,0), 
			c(rowvar, rowvarnm), with=FALSE])
  pnt.filter <- "change_pnt == 1"

  est.chg_ag_2grp <- modPB(PBpopdat=PBpopdatICE, tabtype=tabtype, 
	rowvar=rowvarnm, rowlut=rowlut, domlut=domlut, row.add0=TRUE, 
  	pnt.filter <- pnt.filter, sumunits=sumunits, 
	savedata=savedata, outfolder=outfolder, outfn.date=outfn.date, 
	overwrite=overwrite, outfn.pre=outfn.pre2, rawdata=rawdata, 
	title.ref=title.ref, returntitle=returntitle)
  #est.chg_ag_2grp$est

 
  ###############################################################################
  ## 03. Estimates of land use at Time 1 on all lands
  ###############################################################################
  outfn.pre2 <- paste0("03_", outfn.pre)
  rowvar <- "use_1"
  rowvar2 <- sub("_1", "", rowvar)
  rowvarnm <- paste(rowvar, "nm", sep="_")
  rowlut <- unique(uselut[!uselut[[rowvar2]] %in% c(-1,0,999), 
			c(rowvar2, paste(rowvar2, "nm", sep="_")), with=FALSE])
  setnames(rowlut, c(rowvar, rowvarnm))

  use_1 <- modPB(PBpopdat=PBpopdatICE, tabtype=tabtype, 
	rowvar=rowvar, rowlut=rowlut, domlut=domlut, row.add0=TRUE, 
	sumunits=sumunits, savedata=savedata, outfolder=outfolder, 
	outfn.date=outfn.date, overwrite=overwrite, outfn.pre=outfn.pre2, 
	rawdata=rawdata, title.ref=title.ref, returntitle=returntitle)
  #use_1$est


  ###############################################################################
  ## 04. Estimates of land use at Time 2 on all lands
  ###############################################################################
  outfn.pre2 <- paste0("04_", outfn.pre)
  rowvar <- "use_2"
  rowvar2 <- sub("_2", "", rowvar)
  rowvarnm <- paste(rowvar, "nm", sep="_")
  rowlut <- unique(uselut[!uselut[[rowvar2]] %in% c(-1,0,999), 
			c(rowvar2, paste(rowvar2, "nm", sep="_")), with=FALSE])
  setnames(rowlut, c(rowvar, rowvarnm))

  use_2 <- modPB(PBpopdat=PBpopdatICE, tabtype=tabtype, 
	rowvar=rowvar, rowlut=rowlut, domlut=domlut, row.add0=TRUE, 
	sumunits=sumunits, savedata=savedata, outfolder=outfolder, 
	outfn.date=outfn.date, overwrite=overwrite, outfn.pre=outfn.pre2, 
	rawdata=rawdata, title.ref=title.ref, returntitle=returntitle)
  #use_2$est


  ###############################################################################
  ## 05. Estimates of land cover at Time 1 all lands
  ###############################################################################
  outfn.pre2 <- paste0("05_", outfn.pre)
  rowvar <- "cover_1"
  rowvar2 <- sub("_1", "", rowvar)
  rowvarnm <- paste(rowvar, "nm", sep="_")
  rowlut <- unique(coverlut[!coverlut[[rowvar2]] %in% c(-1,0,999), 
			c(rowvar2, paste(rowvar2, "nm", sep="_")), with=FALSE])
  setnames(rowlut, c(rowvar, rowvarnm))

  cover_1 <- modPB(PBpopdat=PBpopdatICE, tabtype=tabtype, 
	rowvar=rowvar, rowlut=rowlut, domlut=domlut, row.add0=TRUE, 
	sumunits=sumunits, savedata=savedata, outfolder=outfolder, 
	outfn.date=outfn.date, overwrite=overwrite, outfn.pre=outfn.pre2, 
	rawdata=rawdata, title.ref=title.ref, returntitle=returntitle)


  ###############################################################################
  ## 06. Estimates of land cover at Time 2 on all lands
  ###############################################################################
  outfn.pre2 <- paste0("06_", outfn.pre)
  rowvar <- "cover_2"
  rowvar2 <- sub("_2", "", rowvar)
  rowvarnm <- paste(rowvar, "nm", sep="_")
  rowlut <- unique(coverlut[!coverlut[[rowvar2]] %in% c(-1,0,999), 
			c(rowvar2, paste(rowvar2, "nm", sep="_")), with=FALSE])
  setnames(rowlut, c(rowvar, rowvarnm))

  cover_2 <- modPB(PBpopdat=PBpopdatICE, tabtype=tabtype, 
	rowvar=rowvar, rowlut=rowlut, domlut=domlut, row.add0=TRUE, 
	sumunits=sumunits, savedata=savedata, outfolder=outfolder, 
	outfn.date=outfn.date, overwrite=overwrite, outfn.pre=outfn.pre2, 
	rawdata=rawdata, title.ref=title.ref, returntitle=returntitle)


  ###############################################################################
  ## 07. Estimates of use_2_FOR by use_1_FOR
  ## Percent of forest land use change.
  ###############################################################################
  outfn.pre2 <- paste0("07_", outfn.pre)
  rowvar <- "use_1_FOR"  
  colvar <- "use_2_FOR" 

  use_1_2_FOR <- modPB(PBpopdat=PBpopdatICE, tabtype=tabtype, 
	rowvar=rowvar, colvar=colvar, domlut=domlut, row.add0=TRUE, col.add0=TRUE, 
	sumunits=sumunits, savedata=savedata, outfolder=outfolder, 
	outfn.date=outfn.date, overwrite=overwrite, outfn.pre=outfn.pre2, 
	rawdata=rawdata, title.ref=title.ref, returntitle=returntitle,
	gainloss=TRUE)
  #use_1_2_FOR$est


  ###############################################################################
  ## 08. Estimates of cover_2_GRP by cover_1_GRP
  ## Percent of land cover change where land use was forest at Time 1.
  ###############################################################################
  outfn.pre2 <- paste0("08_", outfn.pre)
  rowvar <- "cover_1_GRP"
  colvar <- "cover_2_GRP"

  cover_1_2_GRP <- modPB(PBpopdat=PBpopdatICE, tabtype=tabtype, 
	rowvar=rowvar, colvar=colvar, domlut=domlut, row.add0=TRUE, col.add0=TRUE, 
	sumunits=sumunits, savedata=savedata, outfolder=outfolder, 
	outfn.date=outfn.date, overwrite=overwrite, outfn.pre=outfn.pre2, 
	rawdata=rawdata, title.ref=title.ref, returntitle=returntitle,
	gainloss=TRUE)
  #cover_1_2_GRP$est


  ###############################################################################
  ## 09. Estimates of use_2 by use_1
  ## Percent of land use change from Time 1 to Time 2.
  ###############################################################################
  outfn.pre2 <- paste0("09_", outfn.pre)
  rowvar <- "use_1"
  colvar <- "use_2"

  use_1_2 <- modPB(PBpopdat=PBpopdatICE, tabtype=tabtype, 
	rowvar=rowvar, colvar=colvar, domlut=domlut, row.add0=TRUE, col.add0=TRUE, 
	sumunits=sumunits, savedata=savedata, outfolder=outfolder, 
	outfn.date=outfn.date, overwrite=overwrite, outfn.pre=outfn.pre2, 
	rawdata=rawdata, title.ref=title.ref, returntitle=returntitle, 
	gainloss=TRUE)
  #use_1_2$est


  ###############################################################################
  ## 10. Estimates of cover_2 by cover_1
  ## Percent of land cover change from Time 1 to Time 2.
  ###############################################################################
  outfn.pre2 <- paste0("10_", outfn.pre)
  rowvar <- "cover_1"
  colvar <- "cover_2"

  cover_1_2 <- modPB(PBpopdat=PBpopdatICE, tabtype=tabtype, 
	rowvar=rowvar, colvar=colvar, domlut=domlut, row.add0=TRUE, col.add0=TRUE, 
	sumunits=sumunits, savedata=savedata, outfolder=outfolder, 
	outfn.date=outfn.date, overwrite=overwrite, outfn.pre=outfn.pre2, 
	rawdata=rawdata, title.ref=title.ref, returntitle=returntitle, 
	gainloss=TRUE)
  #cover_1_2$est


  ###############################################################################
  ## 11. Estimates of cover_1 by use_1
  ## Percent of land cover by land use at Time 1.
  ###############################################################################
  outfn.pre2 <- paste0("11_", outfn.pre)
  rowvar <- "use_1"
  colvar <- "cover_1"

  use_1_cover_1 <- modPB(PBpopdat=PBpopdatICE, tabtype=tabtype, 
	rowvar=rowvar, colvar=colvar, domlut=domlut, row.add0=TRUE, col.add0=TRUE, 
	sumunits=sumunits, savedata=savedata, outfolder=outfolder, 
	outfn.date=outfn.date, overwrite=overwrite, outfn.pre=outfn.pre2, 
	rawdata=rawdata, title.ref=title.ref, returntitle=returntitle, 
	gainloss=TRUE)
  #use_1_cover_1$est


  ###############################################################################
  ## 12. Estimates of cover_2 by use_2
  ## Percent of land cover by land use at Time 2.
  ###############################################################################
  outfn.pre2 <- paste0("12_", outfn.pre)
  rowvar <- "use_2"
  colvar <- "cover_2"

  use_2_cover_2 <- modPB(PBpopdat=PBpopdatICE, tabtype=tabtype, 
	rowvar=rowvar, colvar=colvar, domlut=domlut, row.add0=TRUE, col.add0=TRUE, 
	sumunits=sumunits, savedata=savedata, outfolder=outfolder, 
	outfn.date=outfn.date, overwrite=overwrite, outfn.pre=outfn.pre2, 
	rawdata=rawdata, title.ref=title.ref, returntitle=returntitle, 
	gainloss=TRUE)
  #use_2_cover_2$est


  ###############################################################################
  ## 13. Ratio estimates of use_2 within use_1
  ## Percent change of land use at Time 1 within land use at Time 2.
  ###############################################################################
  outfn.pre2 <- paste0("13_", outfn.pre)
  rowvar <- "use_1"
  colvar <- "use_2"

  use_1_use_2_rat <- modPB(PBpopdat=PBpopdatICE, tabtype=tabtype, ratio=TRUE,
	rowvar=rowvar, colvar=colvar, domlut=domlut, row.add0=TRUE, col.add0=TRUE, 
	sumunits=sumunits, savedata=savedata, outfolder=outfolder, 
	outfn.date=outfn.date, overwrite=overwrite, outfn.pre=outfn.pre2, 
	rawdata=rawdata, title.ref=title.ref, returntitle=returntitle)
  #use_1_use_2_rat$est


  ###############################################################################
  ## 13a. Ratio estimates of use_2 within cover_2
  ## Percent change of land use at Time 1 within land cover at Time 2.
  ###############################################################################
  outfn.pre2 <- paste0("13a_", outfn.pre)
  rowvar <- "use_2"
  colvar <- "cover_2"

  use_2_cover_2_rat <- modPB(PBpopdat=PBpopdatICE, tabtype=tabtype, ratio=TRUE,
	rowvar=rowvar, colvar=colvar, domlut=domlut, row.add0=TRUE, col.add0=TRUE, 
	sumunits=sumunits, savedata=savedata, outfolder=outfolder, 
	outfn.date=outfn.date, overwrite=overwrite, outfn.pre=outfn.pre2, 
	rawdata=rawdata, title.ref=title.ref, returntitle=returntitle)
  #use_2_cover_2_rat$est


  ###############################################################################
  ## 14. Ratio estimates of cover_2 within cover_1
  ## Percent change of land cover at Time 1 within land use at Time 2.
  ###############################################################################
  outfn.pre2 <- paste0("14_", outfn.pre)
  rowvar <- "cover_1"
  colvar <- "cover_2"

  cover_1_cover_2_rat <- modPB(PBpopdat=PBpopdatICE, tabtype=tabtype, ratio=TRUE,
	rowvar=rowvar, colvar=colvar, domlut=domlut, row.add0=TRUE, col.add0=TRUE, 
	sumunits=sumunits, savedata=savedata, outfolder=outfolder, 
	outfn.date=outfn.date, overwrite=overwrite, outfn.pre=outfn.pre2, 
	rawdata=rawdata, title.ref=title.ref, returntitle=returntitle)
  #cover_1_cover_2_rat$est


  ###############################################################################
  ## 15. Ratio estimates of use_1 within chg_ag_2_GRP
  ## Percent change of land use at Time 1 within within change agent group.
  ###############################################################################
  outfn.pre2 <- paste0("15_", outfn.pre)
  rowvar <- "chg_ag_2_GRP"
  colvar <- "use_1"
  pnt.filter <- "use_1 != use_2"

  agent_use_1_rat <- modPB(PBpopdat=PBpopdatICE, tabtype=tabtype, ratio=TRUE,
	rowvar=rowvar, colvar=colvar, domlut=domlut, row.add0=TRUE, col.add0=TRUE, 
	pnt.filter=pnt.filter, sumunits=sumunits, savedata=savedata, outfolder=outfolder, 
	outfn.date=outfn.date, overwrite=overwrite, outfn.pre=outfn.pre2, 
	rawdata=rawdata, title.ref=title.ref, returntitle=returntitle)
  #agent_use_1_rat$est


  ###############################################################################
  ## 16. Ratio estimates of cover_1 within chg_ag_2_GRP
  ## Percent change of land cover at Time 1 within change agent group.
  ###############################################################################
  outfn.pre2 <- paste0("16_", outfn.pre)
  rowvar <- "chg_ag_2_GRP"
  colvar <- "cover_1"
  pnt.filter <- "cover_1 != cover_2"

  agent_cover_1_rat <- modPB(PBpopdat=PBpopdatICE, tabtype=tabtype, ratio=TRUE,
	rowvar=rowvar, colvar=colvar, domlut=domlut, row.add0=TRUE, col.add0=TRUE, 
	pnt.filter=pnt.filter, sumunits=sumunits, savedata=savedata, outfolder=outfolder, 
	outfn.date=outfn.date, overwrite=overwrite, outfn.pre=outfn.pre2, 
	rawdata=rawdata, title.ref=title.ref, returntitle=returntitle)
  #agent_cover_1_rat$est


  ###############################################################################
  ## 17. Transitions - Land Use
  ## Percent change of Land Use from Time 1 to Time 2.
  ###############################################################################
  outfn.pre2 <- paste0("17_", outfn.pre)
  rowvar <- "use_1_2"
  pnt.filter <- "use_1 != use_2"

  use_1_2t <- modPB(PBpopdat=PBpopdatICE, tabtype=tabtype, 
	rowvar=rowvar, domlut=domlut, row.add0=TRUE, 
	pnt.filter=pnt.filter, sumunits=sumunits, savedata=savedata, outfolder=outfolder, 
	outfn.date=outfn.date, overwrite=overwrite, outfn.pre=outfn.pre2, 
	rawdata=rawdata, title.ref=title.ref, returntitle=returntitle)
  #use_1_2t$est


  ###############################################################################
  ## 18. Transitions - Land Cover
  ## Percent change of Land Cover from Time 1 to Time 2.
  ###############################################################################
  outfn.pre2 <- paste0("18_", outfn.pre)
  rowvar <- "cover_1_2"
  pnt.filter <- "cover_1 != cover_2"

  cover_1_2t <- modPB(PBpopdat=PBpopdatICE, tabtype=tabtype, 
	rowvar=rowvar, domlut=domlut, row.add0=TRUE, 
	pnt.filter=pnt.filter, sumunits=sumunits, savedata=savedata, outfolder=outfolder, 
	outfn.date=outfn.date, overwrite=overwrite, outfn.pre=outfn.pre2, 
	rawdata=rawdata, title.ref=title.ref, returntitle=returntitle)
  #cover_1_2t$est


  ###############################################################################
  ## 19. Transitions - Land Use - Forest/Nonforest
  ## Percent change of Land Use (Forest/Nonforest) from Time 1 to Time 2.
  ###############################################################################
  outfn.pre2 <- paste0("19_", outfn.pre)
  rowvar <- "use_1_2_FOR"
  pnt.filter <- "use_1_FOR != use_2_FOR"

  use_1_2_FORt <- modPB(PBpopdat=PBpopdatICE, tabtype=tabtype, 
	rowvar=rowvar, domlut=domlut, row.add0=TRUE, 
	pnt.filter=pnt.filter, sumunits=sumunits, savedata=savedata, outfolder=outfolder, 
	outfn.date=outfn.date, overwrite=overwrite, outfn.pre=outfn.pre2, 
	rawdata=rawdata, title.ref=title.ref, returntitle=returntitle)
  #use_1_2_FORt$est
 
}

