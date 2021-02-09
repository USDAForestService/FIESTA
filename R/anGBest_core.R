anGBest_core <- function(state=NULL, evalCur=TRUE, evalEndyr=NULL, 
		evalType=c("ALL", "VOL", "GRM"), fill=TRUE, addSEcol=FALSE, 
		allin1=FALSE, savedata=FALSE, outfolder=NULL, outfn.date=TRUE, 
		divideby.vol="million", datCore=NULL) {

  ## Set global variables
  SPGRPCD=footnote3=footnote4 <- NULL
  divideby <- "thousand"

  ## This works 
  if (Sys.getenv("JAVA_HOME")!="") 
    Sys.setenv(JAVA_HOME="") 
  #library(rJava)
  #library(xlsx)
  gui <- FALSE
  cellwidth <- ifelse(allin1, 12, 10)
  estround <- 1


  ## Notes:
  ## 1. If want to add row groups with no subtotals, use GBest*() with rowgrp=TRUE.
  ## 2. If want to add row groups with subtotals, use rowgrptab().
  ## 3. If want to add SE columns side-by-side, use write2xlsx() with addSEcol=TRUE.
  ## 4. If want to add SE values in same cell, use GBest*() with allin1=TRUE or 
  ##	rowgrptab() with allin1=TRUE if subtotals and write2xlsx() with allin1=TRUE.
  ## 5. If want all columns in ref table, use GBest*() or rowgrptab() with col.add0=TRUE.


  ##################################################################
  ## CHECK INPUT PARAMETERS
  ##################################################################
  ref_codes <- FIESTA::ref_codes

  ## Check outfolder 
  ########################################################
  outfolder <- FIESTA::pcheck.outfolder(outfolder)

  if (is.null(datCore)) {
    datCore <- anGBest_core_data(state=state, evalCur=evalCur, evalEndyr=evalEndyr,
		evalType=evalType, savedata=savedata, outfolder=outfolder, 
		outfn.date=outfn.date) 
  } else {

    ## Extract data from coredata object
    ########################################################################
    if (!"datPlots" %in% names(datCore))
      stop("coredata must include datPlots... run anGBest_core_data()")
    if (!"datStrata" %in% names(datCore))
      stop("coredata must include datStrata... run anGBest_core_data()")
  }

  datPlots <- datCore$datPlots
  datStrata <- datCore$datStrata
  ref_countycd <- datCore$ref_countycd
  ref_unitcd <- datCore$ref_unitcd

  state <- datPlots$states
  evalid <- datPlots$evalid
  plt <- datPlots$plt
  cond <- datPlots$cond
  tree <- datPlots$tree 
  vspspp <- datPlots$vspspp
  vspstr <- datPlots$vspstr
  pltassgn <- datStrata$pltassgn
  unitarea <- datStrata$unitarea
  stratalut <- datStrata$stratalut
  pltassgnid <- datStrata$pltassgnid

  if (lapply(evalid, length) > 1) {
    pltassgn00 <- pltassgn[endsWith(as.character(pltassgn[["EVALID"]]), "0"),]
    pltassgn03 <- pltassgn[endsWith(as.character(pltassgn[["EVALID"]]), "3"),]
    pltassgn <- pltassgn[endsWith(as.character(pltassgn[["EVALID"]]), "1"),]

    plt00 <- plt[plt$CN %in% pltassgn00[[pltassgnid]],]
    plt03 <- plt[plt$CN %in% pltassgn03[[pltassgnid]],]
    plt <- plt[plt$CN %in% pltassgn[[pltassgnid]],]

    cond00 <- cond[cond$PLT_CN %in% pltassgn00[[pltassgnid]],]
    cond03 <- cond[cond$PLT_CN %in% pltassgn03[[pltassgnid]],]
    cond <- cond[cond$PLT_CN %in% pltassgn[[pltassgnid]],]

    tree03 <- tree[tree$PLT_CN %in% pltassgn03[[pltassgnid]],]
    tree <- tree[tree$PLT_CN %in% pltassgn[[pltassgnid]],]

    vspspp <- vspspp[vspspp$PLT_CN %in% pltassgn[[pltassgnid]],]
    vspstr <- vspstr[vspstr$PLT_CN %in% pltassgn[[pltassgnid]],]

    unitarea00 <- unitarea[endsWith(as.character(unitarea[["EVALID"]]), "0"),]
    unitarea03 <- unitarea[endsWith(as.character(unitarea[["EVALID"]]), "3"),]
    unitarea <- unitarea[endsWith(as.character(unitarea[["EVALID"]]), "1"),]

    stratalut00 <- stratalut[endsWith(as.character(stratalut[["EVALID"]]), "0"),]
    stratalut03 <- stratalut[endsWith(as.character(stratalut[["EVALID"]]), "3"),]
    stratalut <- stratalut[endsWith(as.character(stratalut[["EVALID"]]), "1"),]

  } else {
    pltassgn00 <- pltassgn
    cond00 <- cond
    pltassgn03 <- pltassgn
    cond03 <- cond
    plt03 <- plt

    unitarea00 <- unitarea
    stratalut00 <- stratalut
    unitarea03 <- unitarea
    stratalut03 <- stratalut
  }

  stcd <- unique(pltassgn$STATECD)
  if (length(state) > 1) stop("only 1 state allowed")
  st <- FIESTA::pcheck.states(stcd, statereturn="ABBR")
  state <- FIESTA::pcheck.states(stcd, statereturn="MEANING")

  pltcounty <- merge(plt[, c("CN", "UNITCD", "COUNTYCD")], ref_countycd, by="COUNTYCD")
  pltcounty <- merge(pltcounty, ref_unitcd, by.x="UNITCD", by.y="VALUE")
  names(pltcounty)[names(pltcounty) == "MEANING"] <- "UNITNM"
  cond <- merge(cond, pltcounty, by.x="PLT_CN", by.y="CN")

  ## Check allin1 
  ########################################################
  allin1 <- FIESTA::pcheck.logical(allin1, varnm="allin1", 
		title="SE in same cell?", first="YES", gui=gui)
  if (allin1) addSEcol <- FALSE

  ## Check addSEcol 
  ########################################################
  addSEcol <- FIESTA::pcheck.logical(addSEcol, varnm="addSEcol", 
		title="Add SE columns?", first="YES", gui=gui)


  ## Set workbook name
  eval <- evalid[[1]][1]
  outfn <- paste0(state, "_eval20", substr(eval, nchar(eval)-3, nchar(eval) - 2), "_core_tables")
  if (fill) outfn <- paste(outfn, "fill", sep="_")
  if (!fill) outfn <- paste(outfn, "nofill", sep="_")
  if (allin1) outfn <- paste(outfn, "allin1", sep="_")
  if (addSEcol) outfn <- paste(outfn, "SEcol", sep="_")

  ## Check Excel workbook
  wbnm <- FIESTA::pcheck.xlsx(wbnm=NULL, savewb=TRUE, outfn=outfn, outfolder=outfolder)

  ## Define footnotes
  footnote1 <- "Numbers in rows and columns may not sum to totals due to rounding."
  footnote2 <- "A dash (--) indicates no sample for the cell; 0 indicates a value of greater than 0 but less than 0.1."
  if (addSEcol && !allin1) 
    footnote3 <- "PSE refers to estimated percent standard error of the Estimate."

  ########################################################################
  ## Add classes and other data to tables
  ########################################################################

  ## Diameter classes
  #############################################################
  if (!"DIACL2IN" %in% names(tree)) { 
    DIALUT <- FIESTA::ref_diacl2in[FIESTA::ref_diacl2in$MIN <= 37, ]
    DIALUT[DIALUT$MIN == 37, "MAX"] <- 300
    DIALUT[DIALUT$MIN == 37, "DIACL2IN"] <- "37+"
    DIALUT$DIACL2INCD <- seq(1:nrow(DIALUT))
    DIALUT2 <- DIALUT[DIALUT$MIN >= 5,]

    ## Append diameter classes to tree table
    datlut <- FIESTA::datLUTclass(x=tree, xvar="DIA", LUT=DIALUT, LUTclassnm="DIACL2IN",
		vars2keep="DIACL2INCD")
    tree <- datlut$xLUT

    datlut <- FIESTA::datLUTclass(x=tree03, xvar="DIA", LUT=DIALUT, LUTclassnm="DIACL2IN",
		vars2keep="DIACL2INCD")
    tree03 <- datlut$xLUT


    ## or
    #treedia.brks <- c(seq(1,40,2), 300)
    #datlut <- datLUTclass(x=tree, xvar="DIA", cutbreaks=treedia.brks, LUTclassnm="DIACL2IN")
    #head(datlut$xLUT)
  }

  ## Stand age classes
  #############################################################
  if (!"STDAGECL" %in% names(cond)) {
    stdage.brks <- c(0, seq(1,201,20),1000)
    stdage.labs <- c("Nonstocked","1-20","21-40","41-60","61-80","81-100","101-120",
			"121-140","141-160","161-180","181-200", "201+")
    datlut <- datLUTclass(x=cond, xvar="STDAGE", cutbreaks=stdage.brks, cutlabels=stdage.labs, 
			LUTclassnm="STDAGECL")
    cond <- datlut$xLUT

    datlut <- datLUTclass(x=cond03, xvar="STDAGE", cutbreaks=stdage.brks, cutlabels=stdage.labs, 
			LUTclassnm="STDAGECL")
    cond03 <- datlut$xLUT
  }

 
  ## Add disturbance groups to cond
  #############################################################
  if ("DSTRBCD1" %in% names(cond)) {
    datlut <- datLUTnm(x=cond, xvar="DSTRBCD1", FIAname=TRUE, group=TRUE)
    cond <- datlut$xLUT

    datlut <- datLUTnm(x=cond03, xvar="DSTRBCD1", FIAname=TRUE, group=TRUE)
    cond03 <- datlut$xLUT
  }

  ## Change TIMBERCD to include SITECLCD in 1:6
  #############################################################
  ## Note: 1:Nonreserved; 2:Reserved
  cond$TIMBERCD.PROD <- 2
  cond[cond$SITECLCD %in% 1:6, "TIMBERCD.PROD"] <- 1
  cond$TIMBERCD <- 2
  cond[cond$SITECLCD %in% 1:6 & cond$RESERVCD == 0, "TIMBERCD"] <- 1

  cond03$TIMBERCD.PROD <- 2
  cond03[cond03$SITECLCD %in% 1:6, "TIMBERCD.PROD"] <- 1
  cond03$TIMBERCD <- 2
  cond03[cond03$SITECLCD %in% 1:6 & cond03$RESERVCD == 0, "TIMBERCD"] <- 1

  cond00$TIMBERCD.PROD <- 2
  cond00[cond00$SITECLCD %in% 1:6, "TIMBERCD.PROD"] <- 1
  cond00$TIMBERCD <- 2
  cond00[cond00$SITECLCD %in% 1:6 & cond00$RESERVCD == 0, "TIMBERCD"] <- 1


  ## Add Species grouping
  #############################################################
  if ("SPGRPCD" %in% names(tree)) {
    pines <- c(1:5,11,14,15,21)
    othersoft <- c(1:24)[!c(1:24) %in% pines]
    softhard <- c(25:47)
    hardhard <- c(25:54)[!c(25:54) %in% softhard]

    tree <- FIESTA::datLUTnm(x=tree, xvar="SPGRPCD", FIAname=TRUE, group=TRUE)$xLUT
    tree[SPGRPCD %in% pines, ':=' (SPGRPGRPCD2=1, SPGRPGRPNM2="Pine")]
    tree[SPGRPCD %in% othersoft, ':=' (SPGRPGRPCD2=2, SPGRPGRPNM2="Other softwoods")]
    tree[SPGRPCD %in% softhard, ':=' (SPGRPGRPCD2=3, SPGRPGRPNM2="Soft hardwoods")]
    tree[SPGRPCD %in% hardhard, ':=' (SPGRPGRPCD2=4, SPGRPGRPNM2="Hard hardwoods")]

    tree03 <- FIESTA::datLUTnm(x=tree03, xvar="SPGRPCD", FIAname=TRUE, group=TRUE)$xLUT
    tree03[SPGRPCD %in% pines, ':=' (SPGRPGRPCD2=1, SPGRPGRPNM2="Pine")]
    tree03[SPGRPCD %in% othersoft, ':=' (SPGRPGRPCD2=2, SPGRPGRPNM2="Other softwoods")]
    tree03[SPGRPCD %in% softhard, ':=' (SPGRPGRPCD2=3, SPGRPGRPNM2="Soft hardwoods")]
    tree03[SPGRPCD %in% hardhard, ':=' (SPGRPGRPCD2=4, SPGRPGRPNM2="Hard hardwoods")]
  }

  ## Get population data
  #############################################################
  popdat03 <- modGBpop(cond=cond03, plt=plt03, tree=tree03, 
			pltassgn=pltassgn03, pltassgnid=pltassgnid, 
			unitvar="ESTN_UNIT", unitarea=unitarea03, minplotnum.unit=2,
			stratalut=stratalut03)
  popdat <- modGBpop(cond=cond, plt=plt, tree=tree, 
			pltassgn=pltassgn, pltassgnid=pltassgnid, 
			unitvar="ESTN_UNIT", unitarea=unitarea, minplotnum.unit=2, 
			stratalut=stratalut)


  ## Set up Excel Workbook
  wb <- xlsx::loadWorkbook(file=wbnm)
  sheetnm <- "TOC"

  newsheet <- xlsx::createSheet(wb, sheetName=as.character(as.character(sheetnm)))
  datsheet <- xlsx::getSheets(wb)[[sheetnm]]
  message(paste("saving to sheet", sheetnm))


  #######################################################################################
  ## Table of Contents (TOC)
  #######################################################################################
  B1 <- "Table B1. Percentage of area by land status"
  B2 <- "Table B2. Area of forest land by owner group, ownership class, reserved status, and forest land status"
  B3 <- "Table B3. Area of forest land by forest type group and site productivity class"
  B4 <- "Table B4. Area of forest land by forest-type group and forest land status"
  B5 <- "Table B5. Area of forest land by forest type group and stand-size class"
  B6 <- "Table B6. Area of forest land by forest type group and stand-age class"
  B7 <- "Table B7. Area of forest land by forest type group and stand origin"
  B8 <- "Table B8. Area of forest land by forest type group and primary disturbance"
  B9 <- "Table B9. Area of timberland by forest type group and stand-size class"
  B10 <- "Table B10. Number of live trees on forest land by species group and diameter class"
  B11 <- "Table B11. Number of standing-dead trees on forest land by species group and diameter class"
  B12 <- "Table B12. Number of growing-stock trees on timberland by species group and diameter class"
  B13 <- "Table B13. Net volume of live trees on forest land by owner group, ownership class, reserved status, and forest land status"
  B14 <- "Table B14. Net volume of standing-dead trees of forest land by ownership group, ownership class, and forest land status"
  B15 <- "Table B15. Net volume of live trees on forest land by forest type group and stand-size class"
  B16 <- "Table B16. Net volume of standing-dead trees on forest land by forest type group and stand-size class"
  B17 <- "Table B17. Net volume of live trees on forest land by species group and ownership group"
  B18 <- "Table B18. Net volume of standing-dead trees on forest land by species group and ownership group"
  B19 <- "Table B19. Net volume of live trees on forest land by species group and diameter class"
  B20 <- "Table B20. Net volume of standing-dead trees on forest land by species group and diameter class"
  B21 <- "Table B21. Net volume of live trees on forest land by forest type group and stand origin"
  B22 <- "Table B22. Net volume of standing-dead trees on forest land by forest type group and stand origin"
  B23 <- "Table B23. Net volume of growing-stock trees on timberland by species group and diameter class"
  B24 <- "Table B24. Net volume of growing-stock trees on timberland by species group, ownership group, and diameter class"
  B25 <- "Table B25. Net volume of sawtimber trees, in million board feet (International 1/4-inch rule) on timberland by species group and diameter class"
  B26 <- "Table B26. Net volume of sawtimber trees, in million board feet (International 1/4-inch rule) on forest land by species group and ownership group"
  B27 <- "Table B27. Aboveground dry weight of live trees on forest land by owner class and forest land status"
  B28 <- "Table B28. Aboveground dry weight of standing-dead trees on forest land by owner class, reserved status, and forest land status"
  B29 <- "Table B29. Aboveground dry weight of live trees on forest land by species group and diameter class"
  B30 <- "Table B30. Aboveground dry weight of standing-dead trees on forest land by species group and diameter class"
  B31 <- "Table B31. Aboveground carbon in live trees on forest land by ownership group, ownership class, reserved status, and forest land status"
  B32 <- "Table B32. Aboveground carbon in standing-dead trees on forest land by ownership group, ownership class, reserved status, and forest land status"
  B33 <- "Table B33. Average annual net growth of live trees by ownership group, ownership class, reserved status, and forest land status"
  B34 <- "Table B34. Average annual net growth of live trees on forest land by forest type group and stand-size class"
  B35 <- "Table B35. Average annual net growth of live trees on forest land by species group and stand-size class"
  B36 <- "Table B36. Average annual net growth of growing-stock trees on timberland by species group and stand-size class"
  B37 <- "Table B37. Average annual mortality of trees by owner class, reserved status, and forest land status"
  B38 <- "Table B38. Average annual mortality on forest land by forest type group and stand-size class"
  B39 <- "Table B39. Average annual mortality of trees on forest land by species group and stand-size class"
  B40 <- "Table B40. Average annual mortality of growing-stock trees on timberland by species group and stand-size class"
  B41 <- "Table B41. Area of forest land by inventory unit, county and forest land status"
  B42 <- "Table B42. Area of forest land by county, forest type group, ownership group, and forest land status"
  B43 <- "Table B43. Area of forest land by county, forest type group and stand-size class"
  B44 <- "Table B44. Area of forest land by county, forest type group and stocking class"
  B45 <- "Table B45. Net volume of growing-stock trees on timberland by inventory unit, county, and major species group"

  toc <- data.frame(rbind(B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, 
		B16, B17, B18, B19, B20, B21, B22, B23, B24, B25, B26, B27, B28, B29, B30, B31, B32,
		B33, B34, B35, B36, B37, B38, B39, B40, B41, B42, B43, B44, B45), stringsAsFactors=FALSE)


  ## Create row and cells for title
  toctitle.row <- xlsx::createRow(datsheet, 1)
  toctitle.cells <- xlsx::createCell(toctitle.row, colIndex=1)
  xlsx::setRowHeight(toctitle.row, multiplier=2)
  xlsx::setCellValue(toctitle.cells[[1]], "Table of Contents")


  xlsx:: addDataFrame(toc, datsheet, col.names=FALSE, row.names=FALSE, startRow=3)

  ## SAVE EXCEL WORKBOOK
  ###############################################################
  xlsx::saveWorkbook(wb, wbnm)  


  #######################################################################################
  ## B1 - Percentage of area by land status
  #######################################################################################
  tabnm <- "B1"
  ref_reservcd <- ref_codes[ref_codes$VARIABLE == "RESERVCD",]
  ref_title <- paste0(state, ", ", min(plt$INVYR), "-", max(plt$INVYR))

  ## Accessible forest land
  ################################################################
  b1.names <- c("Land status", "Land status2", "Land status3", "Estimate", 
			"Percent Sampling Error")
  b1.1 <- {}

  rescds <- sort(unique(ref_reservcd[["VALUE"]]))
  rescds <- rescds[rescds >= 0]
  for (res in rescds) {
    resnm <- ref_reservcd[ref_reservcd[["VALUE"]] == res, "MEANING"] 
    print(resnm)

    cfilter <- paste("RESERVCD ==", res)
    est <- modGBarea(cond=cond00, plt=plt00, pltassgn=pltassgn00, 
		strata=TRUE, unitvar="ESTN_UNIT", unitarea=unitarea00, minplotnum.unit=2,
		stratalut=stratalut00, landarea="FOREST", cfilter=cfilter, 
		rowvar="TIMBERCD.PROD", row.FIAname=TRUE, adj="none", sumunits=TRUE, 
		estround=estround)$est
    est <- data.frame(paste(resnm, "forest land"), est, stringsAsFactors=FALSE)
    names(est) <- c("landstatus1", "landstatus2", "Estimate", "Percent Sampling Error")
    if (res == 0) est[est$landstatus2 == "Productive", "landstatus2"] <- "Timberland"
    b1.1 <- rbind(b1.1, est)
  }
  cond2.filter <- "COND_STATUS_CD == 1"
  est2 <- modGBarea(cond=cond00, plt=plt00, pltassgn=pltassgn00, strata=TRUE, 
		unitvar="ESTN_UNIT", unitarea=unitarea00, minplotnum.unit=2, 
		stratalut=stratalut00, cfilter=cond2.filter, adj="none", 
		sumunits=TRUE, estround=estround)$est
  est2 <- data.frame("Total", est2, stringsAsFactors=FALSE)
  names(est2) <- c("landstatus1", "landstatus2", "Estimate", "Percent Sampling Error")
  b1.1 <- rbind(b1.1, est2)

  b1.1 <- data.frame(rgroup="Accessible forest land", b1.1, stringsAsFactors=FALSE)
  names(b1.1) <- b1.names

  ## Nonforest and other land
  ################################################################
  cfilter <- "COND_STATUS_CD == 2"
  est <- modGBarea(cond=cond00, plt=plt00, pltassgn=pltassgn00, strata=TRUE, 
		unitvar="ESTN_UNIT", unitarea=unitarea00, minplotnum.unit=2, 
		stratalut=stratalut00, landarea="ALL", cfilter=cfilter, 
		adj="none", sumunits=TRUE, estround=estround)$est
  b1.2 <- data.frame(landstatus1="Nonforest and other land", "Nonforest land", "", est[, -1],
		stringsAsFactors=FALSE)
  names(b1.2) <- b1.names

  cfilter <- "COND_STATUS_CD %in% c(3,4)"
  est <- modGBarea(cond=cond00, plt=plt00, pltassgn=pltassgn00, strata=TRUE, 
		unitvar="ESTN_UNIT", unitarea=unitarea00, minplotnum.unit=2, 
		stratalut=stratalut00, landarea="ALL", cfilter=cfilter, 
		rowvar="COND_STATUS_CD", row.FIAname=TRUE, adj="none", sumunits=TRUE, 
		estround=estround)$est
  b1.3 <- data.frame(landstatus1="Nonforest and other land", "Water", est, stringsAsFactors=FALSE)
  names(b1.3) <- b1.names

  cfilter <- "COND_STATUS_CD %in% c(2,3,4)"
  est <- modGBarea(cond=cond00, plt=plt00, pltassgn=pltassgn00, strata=TRUE, 
		unitvar="ESTN_UNIT", unitarea=unitarea00, minplotnum.unit=2, 
		stratalut=stratalut00, landarea="ALL", cfilter=cfilter, 
		adj="none", sumunits=TRUE, estround=estround)$est
  b1.4 <- data.frame(landstatus1="Nonforest and other land", "Total", "", est[, -1],
		stringsAsFactors=FALSE)
  names(b1.4) <- b1.names


  ## Nonsampled land
  ################################################################
  cfilter <- "COND_STATUS_CD == 5"
  est <- modGBarea(cond=cond00, plt=plt00, pltassgn=pltassgn00, strata=TRUE, 
		unitvar="ESTN_UNIT", unitarea=unitarea00, minplotnum.unit=2, 
		stratalut=stratalut00, landarea="ALL", cfilter=cfilter, 
		rowvar="COND_NONSAMPLE_REASN_CD", row.FIAname=TRUE, adj="none", 
		sumunits=TRUE, estround=estround)$est
  b1.5 <- data.frame(landstatus1="Nonsampled land", est[,1], "", est[, -1])
  names(b1.5) <- b1.names


  ## Combine tables
  #################################################################
  b1 <- rbind(b1.1, b1.2, b1.3, b1.4, b1.5)
  b1$Estimate <- as.numeric(b1$Estimate)

  tot <- sum(unitarea$ACRES)
  b1$Estimate <- round(b1$Estimate / tot * 100, 1)

  b1.6 <- data.frame(landstatus1="All land", "", "", 100, 0)
  names(b1.6) <- b1.names

  b1 <- rbind(b1, b1.6)
  subtotvals <- c("Unreserved forest land", "Reserved forest land", "Water")

  estnm <- "Percentage of area"
  psenm <- "Percent Sampling Error"

  if (allin1) {
    esttab <- b1
    esttab[[psenm]] <- allin1f(esttab$Estimate, esttab[[psenm]])
    esttab$'Estimate' <- NULL
  } else {
    esttab <- b1
    names(esttab)[names(esttab) == "Estimate"] <- estnm
  }

  tabtitle <- paste("Table B1. Percentage of all area by land status;", ref_title)
  write2xlsx(esttab=esttab, tabtitle=tabtitle,  
		outfolder=outfolder, rowgrp2=TRUE, fill=fill, allin1=allin1, 
		addSEcol=addSEcol, rowtottxt="All land", coltotal=FALSE,
		rowgrp.subtot=TRUE, rowgrp2.subtot=TRUE, subtotvals=subtotvals, 
		wbnm=wbnm, sheetnm=tabnm, footnote1=footnote1, footnote2=footnote2, 
		estnm=estnm, psenm.out=psenm, cellWidth.grp=16)



  #########################################################################################
  ## B2 - Area of forest land by owner class and forest land status
  #########################################################################################
  tabnm <- "B2"
print(paste("Table", tabnm))
  esttype <- "AREA"
  rowvar <- "OWNCD"
  rowgrp <- TRUE
  colvar <- "TIMBERCD.PROD"
  colgrp <- TRUE
  colgrpcd <- "RESERVCD"
  landarea <- "FOREST"
  title.rowgrp <- "Owner group"
  title.colvar <- "Forest land status"

  row.FIAname=TRUE
  col.FIAname=TRUE
  col.add0=TRUE
  returntitle=TRUE
  rowgrp.subtot=FALSE
  coltottxt="All forest land"

#  footnote <- "Other forest land is classified as not capable of growing 20 cubic feet/acre/year."
#  if (is.null(footnote3)) {
#    footnote3 <- footnote
#  } else {
#    footnote4 <- footnote
#  }
  estdat2 <- tabgrp(GBpopdat=popdat, esttype="AREA", sumunits=TRUE, landarea=landarea, 
	rowvar=rowvar, colvar=colvar, rowgrp=rowgrp, colgrp=colgrp, colgrpcd=colgrpcd, 
	row.FIAname=row.FIAname, col.FIAname=col.FIAname, rowgrp.subtot=rowgrp.subtot, 
	title.rowgrp=title.rowgrp, title.colvar=title.colvar, allin1=allin1, 
	estnull="--", divideby=divideby) 
  esttab <- estdat2$esttab
  tabtitle <- estdat2$esttab.title
  title.rowvar <- estdat2$title.rowvar
  title.colvar <- estdat2$title.colvar
  cnames <- estdat2$cnames
  title.rowgrp <- estdat2$title.rgrp
  psetab <- estdat2$psetab
  psetab.title <- estdat2$psetab.title

  write2xlsx(esttab=esttab, psetab=psetab, tabtitle=paste0("Table ", tabnm, ". ", tabtitle),  
		title.colvar=NULL, outfolder=outfolder, allin1=allin1,
		title.rowvar=title.rowvar, title.rowgrp=title.rowgrp, rowgrp=rowgrp,
		rowgrp.subtot=rowgrp.subtot, addSEcol=addSEcol, fill=fill, colgrp=colgrp,
		cnames=cnames, coltottxt=coltottxt, wbnm=wbnm, sheetnm=tabnm, footnote1=footnote1, 
		footnote2=footnote2, footnote3=footnote3, cellwidth=cellwidth+3)

  #########################################################################################
  ## B3 - Area of forest land by forest type group and productivity class
  #########################################################################################
  tabnm <- "B3"
print(paste("Table", tabnm))
  esttype <- "AREA"
  rowvar <- "FORTYPGRPCD"
  colvar <- "SITECLCD"
  landarea <- "FOREST"

  row.FIAname=TRUE
  col.FIAname=TRUE
  col.add0=TRUE
  returntitle=TRUE
  coltottxt <- "Total all classes"

  estdat <- modGBarea(GBpopdat=popdat, sumunits=TRUE, landarea=landarea, 
		rowvar=rowvar, colvar=colvar, row.FIAname=row.FIAname, col.FIAname=col.FIAname, 
		col.add0=col.add0, returntitle=TRUE, divideby=divideby, allin1=allin1, estnull="--")
  esttab <- estdat$est
  psetab <- estdat$pse
  tabtitle <- ifelse (allin1, estdat$titlelst$title.estpse, estdat$titlelst$title.est)
  title.colvar <- estdat$titlelst$title.colvar
  title.rowvar <- estdat$titlelst$title.rowvar

  write2xlsx(esttab=esttab, psetab=psetab, tabtitle=paste0("Table ", tabnm, ". ", tabtitle),  
		title.colvar=title.colvar, outfolder=outfolder,
		title.rowvar=title.rowvar, fill=fill, allin1=allin1, addSEcol=addSEcol,
		coltottxt=coltottxt, cellwidth=cellwidth, wbnm=wbnm, sheetnm=tabnm,
           footnote1=footnote1, footnote2=footnote2, footnote3=footnote3)


  #########################################################################################
  ## B4 - Area of forest land by forest type group, ownership group, and forest land status
  #########################################################################################
  tabnm <- "B4"
print(paste("Table", tabnm))
  esttype <- "AREA"
  rowvar <- "FORTYPGRPCD"
  colvar <- "TIMBERCD"
  colgrp <- TRUE
  rowgrp <- FALSE
  colgrpcd <- "OWNGRPCD"
  landarea <- "FOREST"

  row.FIAname=TRUE
  col.FIAname=TRUE
  col.add0=TRUE
  returntitle=TRUE
  rowgrp.subtot=FALSE
  coltottxt="All forest land"

#  footnote <- "Other forest land is classified as not capable of growing 20 cubic feet/acre/year."
#  if (is.null(footnote3)) {
#    footnote3 <- footnote
#  } else {
#    footnote4 <- footnote
#  }

  estdat2 <- tabgrp(GBpopdat=popdat, esttype="AREA", sumunits=TRUE, landarea=landarea,
	rowvar=rowvar, colvar=colvar, rowgrp=FALSE, colgrp=TRUE, colgrpcd=colgrpcd,
	row.FIAname=row.FIAname, col.FIAname=col.FIAname, rowgrp.subtot=rowgrp.subtot, 
	allin1=allin1, estnull="--", divideby=divideby) 
  esttab <- estdat2$esttab
  tabtitle <- estdat2$esttab.title
  title.rowvar <- estdat2$title.rowvar
  title.colvar <- estdat2$title.colvar
  cnames <- estdat2$cnames
  title.rowgrp <- estdat2$title.rgrp
  psetab <- estdat2$psetab
  psetab.title <- estdat2$psetab.title

  write2xlsx(esttab=esttab, psetab=psetab, tabtitle=paste0("Table ", tabnm, ". ", tabtitle),  
		title.colvar=NULL, outfolder=outfolder, allin1=allin1,
		title.rowvar=title.rowvar, title.rowgrp=title.rowgrp, rowgrp=FALSE,
		rowgrp.subtot=rowgrp.subtot, addSEcol=addSEcol, fill=fill, colgrp=TRUE,
		cnames=cnames, coltotal=TRUE, coltottxt=coltottxt, cellwidth=cellwidth+1,
		wbnm=wbnm, sheetnm=tabnm, footnote1=footnote1, footnote2=footnote2,
		footnote3=footnote3)


  #########################################################################################
  ## B5 - Area of forest land by forest type group and stand-size class
  #########################################################################################
  tabnm <- "B5"
print(paste("Table", tabnm))
  esttype <- "AREA"
  rowvar <- "FORTYPGRPCD"
  colvar <- "STDSZCD"
  landarea <- "FOREST"

  row.FIAname=TRUE
  col.FIAname=TRUE
  col.add0=TRUE
  returntitle=TRUE
  coltottxt="All size classes"

  estdat <- modGBarea(GBpopdat=popdat, sumunits=TRUE, landarea=landarea,
		rowvar=rowvar, colvar=colvar, row.FIAname=row.FIAname, col.FIAname=col.FIAname, 
		col.add0=col.add0, returntitle=TRUE, allin1=allin1, estnull="--", divideby=divideby)
  esttab <- estdat$est
  psetab <- estdat$pse
  tabtitle <- ifelse (allin1, estdat$titlelst$title.estpse, estdat$titlelst$title.est)
  title.colvar <- estdat$titlelst$title.colvar
  title.rowvar <- estdat$titlelst$title.rowvar

  write2xlsx(esttab=esttab, psetab=psetab, tabtitle=paste0("Table ", tabnm, ". ", tabtitle),  
		title.colvar=title.colvar, outfolder=outfolder, title.rowvar=title.rowvar, 
		fill=fill, allin1=allin1, addSEcol=addSEcol, coltottxt=coltottxt, 
		cellwidth=cellwidth, wbnm=wbnm, sheetnm=tabnm, footnote1=footnote1, 
		footnote2=footnote2, footnote3=footnote3)


  #########################################################################################
  ## B6 - Area of forest land by forest type group and stand-age class
  #########################################################################################
  tabnm <- "B6"
print(paste("Table", tabnm))
  esttype <- "AREA"
  rowvar <- "FORTYPGRPCD"
  colvar <- "STDAGECL"
  title.colvar <- "Stand-age class (years)" 
  landarea <- "FOREST"

  row.FIAname=TRUE
  col.add0=TRUE
  returntitle=TRUE
  coltottxt="All classes"

  estdat <- modGBarea(GBpopdat=popdat, sumunits=TRUE, landarea=landarea,
		rowvar=rowvar, colvar=colvar, row.FIAname=row.FIAname, col.add0=col.add0, 
		returntitle=TRUE, estnull="--", divideby=divideby, allin1=allin1,  
		title.colvar=title.colvar)
  esttab <- estdat$est
  psetab <- estdat$pse
  tabtitle <- ifelse (allin1, estdat$titlelst$title.estpse, estdat$titlelst$title.est)
  title.colvar <- estdat$titlelst$title.colvar
  title.rowvar <- estdat$titlelst$title.rowvar

  write2xlsx(esttab=esttab, psetab=psetab, tabtitle=paste0("Table ", tabnm, ". ", tabtitle),  
		title.colvar=title.colvar, outfolder=outfolder, title.rowvar=title.rowvar, 
		fill=fill, allin1=allin1, addSEcol=addSEcol, coltottxt=coltottxt, 
		cellwidth=cellwidth+1, wbnm=wbnm, sheetnm=tabnm, footnote1=footnote1, 
		footnote2=footnote2, footnote3=footnote3)


  #########################################################################################
  ## B7 - Area of forest land by forest type group and stand origin
  #########################################################################################
  tabnm <- "B7"
print(paste("Table", tabnm))
  esttype <- "AREA"
  rowvar <- "FORTYPGRPCD"
  colvar <- "STDORGCD"
  landarea <- "FOREST"

  row.FIAname=TRUE
  col.FIAname=TRUE
  col.add0=TRUE
  returntitle=TRUE

  coltottxt="All forest land"
  cellwidth=15

  estdat <- modGBarea(GBpopdat=popdat, sumunits=TRUE, landarea=landarea,
		rowvar=rowvar, colvar=colvar, row.FIAname=row.FIAname, col.FIAname=col.FIAname, 
		col.add0=col.add0, returntitle=TRUE, allin1=allin1, estnull="--", divideby=divideby)
  esttab <- estdat$est
  psetab <- estdat$pse
  tabtitle <- ifelse (allin1, estdat$titlelst$title.estpse, estdat$titlelst$title.est)
  title.colvar <- estdat$titlelst$title.colvar
  title.rowvar <- estdat$titlelst$title.rowvar

  write2xlsx(esttab=esttab, psetab=psetab, tabtitle=paste0("Table ", tabnm, ". ", tabtitle),  
		title.colvar=title.colvar, outfolder=outfolder, title.rowvar=title.rowvar, 
		fill=fill, allin1=allin1, addSEcol=addSEcol, coltottxt=coltottxt, 
		cellwidth=cellwidth, wbnm=wbnm, sheetnm=tabnm, footnote1=footnote1, 
		footnote2=footnote2, footnote3=footnote3)


  #########################################################################################
  ## B8 - Area of forest land by forest type group and primary disturbance
  #########################################################################################
  tabnm <- "B8"
print(paste("Table", tabnm))
  esttype <- "AREA"
  rowvar <- "FORTYPGRPCD"
  colvar <- "DSTRBGRPNM"
  col.orderby <- "DSTRBGRPCD"
  title.colvar <- "Disturbance class"
  landarea <- "FOREST"

  row.FIAname=TRUE
  col.add0=TRUE
  returntitle=TRUE
  coltottxt="All forest land"

  estdat <- modGBarea(GBpopdat=popdat, sumunits=TRUE, landarea=landarea,
		rowvar=rowvar, colvar=colvar, row.FIAname=row.FIAname, col.orderby=col.orderby, 
		col.add0=col.add0, returntitle=TRUE, divideby=divideby, allin1=allin1, 
		estnull="--", title.colvar=title.colvar)
  esttab <- estdat$est
  psetab <- estdat$pse
  tabtitle <- ifelse (allin1, estdat$titlelst$title.estpse, estdat$titlelst$title.est)
  title.colvar <- estdat$titlelst$title.colvar
  title.rowvar <- estdat$titlelst$title.rowvar

  write2xlsx(esttab=esttab, psetab=psetab, tabtitle=paste0("Table ", tabnm, ". ", tabtitle),  
		title.colvar=title.colvar, outfolder=outfolder, title.rowvar=title.rowvar, 
		fill=fill, allin1=allin1, addSEcol=addSEcol, coltottxt=coltottxt, 
		cellwidth=cellwidth, wbnm=wbnm, sheetnm=tabnm, footnote1=footnote1, 
		footnote2=footnote2, footnote3=footnote3)


  #########################################################################################
  ## B9 - Area of timberland by forest type group and stand-size class
  #########################################################################################
  tabnm <- "B9"
print(paste("Table", tabnm))
  esttype <- "AREA"
  rowvar <- "FORTYPGRPCD"
  colvar <- "STDSZCD"
  landarea <- "TIMBERLAND"

  row.FIAname=TRUE
  col.FIAname=TRUE
  col.add0=TRUE
  returntitle=TRUE
  coltottxt="All size classes"

  estdat <- modGBarea(GBpopdat=popdat, sumunits=TRUE, landarea=landarea,
		rowvar=rowvar, colvar=colvar, row.FIAname=row.FIAname, col.FIAname=col.FIAname, 
		col.add0=col.add0, returntitle=TRUE, allin1=allin1, estnull="--", divideby=divideby)
  esttab <- estdat$est
  psetab <- estdat$pse
  tabtitle <- ifelse (allin1, estdat$titlelst$title.estpse, estdat$titlelst$title.est)
  title.colvar <- estdat$titlelst$title.colvar
  title.rowvar <- estdat$titlelst$title.rowvar

  write2xlsx(esttab=esttab, psetab=psetab, tabtitle=paste0("Table ", tabnm, ". ", tabtitle),  
		title.colvar=title.colvar, outfolder=outfolder, title.rowvar=title.rowvar, 
		fill=fill, allin1=allin1, addSEcol=addSEcol, coltottxt=coltottxt, 
		cellwidth=cellwidth, wbnm=wbnm, sheetnm=tabnm, footnote1=footnote1, 
		footnote2=footnote2, footnote3=footnote3)

  #########################################################################################
  ## B10 - Number of live trees on forest land by species group and diameter class
  #########################################################################################
  tabnm <- "B10"
print(paste("Table", tabnm))
  esttype <- "TREE"
  rowvar <- "SPGRPCD"
  rowgrp <- TRUE
  colvar <- "DIACL2IN"
  title.colvar <- "Diameter class (inches)" 
  landarea <- "FOREST"

  estvar <- "TPA_UNADJ"
  estvar.filter="STATUSCD == 1"

  row.FIAname=TRUE
  col.add0=TRUE
  returntitle=TRUE
  rowgrp.subtot=TRUE

  coltottxt="All classes"
  cellwidth=12
  collut=DIALUT

  title.rowgrp=NULL
  title.rowvar=NULL
  rowgrpnm=NULL
  rowgrpord=NULL
  colgrpcd=NULL
  cfilter=NULL

  estdat2 <- tabgrp(GBpopdat=popdat, esttype="TREE", sumunits=TRUE, landarea=landarea,  
	estvar=estvar, estvar.filter=estvar.filter, rowvar=rowvar, colvar=colvar, 
	rowgrp=rowgrp, row.FIAname=TRUE, collut=collut, rowgrp.subtot=rowgrp.subtot, 
	allin1=allin1, divideby=divideby, estnull="--", title.colvar=title.colvar) 
 
  esttab <- estdat2$esttab
  tabtitle <- estdat2$esttab.title
  title.rowvar <- estdat2$title.rowvar
  title.colvar <- estdat2$title.colvar
  cnames <- estdat2$cnames
  title.rowgrp <- estdat2$title.rgrp
  psetab <- estdat2$psetab
  psetab.title <- estdat2$psetab.title

  write2xlsx(esttab=esttab, psetab=psetab, tabtitle=paste0("Table ", tabnm, ". ", tabtitle),  
		title.colvar=title.colvar, outfolder=outfolder, title.rowvar=title.rowvar, 
		rowgrp=rowgrp, title.rowgrp=title.rowgrp, rowgrp.subtot=rowgrp.subtot, 
		fill=fill, allin1=allin1, addSEcol=addSEcol, coltottxt=coltottxt, 
		cellwidth=cellwidth, wbnm=wbnm, sheetnm=tabnm, footnote1=footnote1, 
		footnote2=footnote2, footnote3=footnote3)

  #########################################################################################
  ## B11 - Number of standing-dead trees on forest land by species group and diameter class
  #########################################################################################
  tabnm <- "B11"
print(paste("Table", tabnm))
  esttype <- "TREE"
  rowvar <- "SPGRPCD"
  rowgrp <- TRUE
  colvar <- "DIACL2IN"
  title.colvar <- "Diameter class (inches)" 
  landarea <- "FOREST"

  estvar <- "TPA_UNADJ"
  estvar.filter="STANDING_DEAD_CD == 1 & STATUSCD == 2 & DIA >= 5"

  row.FIAname=TRUE
  col.add0=TRUE
  returntitle=TRUE
  rowgrp.subtot=TRUE

  coltottxt="All classes"
  collut=DIALUT[DIALUT$MIN >= 5,]

  title.rowgrp=NULL
  title.rowvar=NULL
  rowgrpnm=NULL
  rowgrpord=NULL
  colgrpcd=NULL
  cfilter=NULL

  estdat2 <- tabgrp(GBpopdat=popdat, esttype="TREE", sumunits=TRUE, landarea=landarea, 	
	estvar=estvar, estvar.filter=estvar.filter, rowvar=rowvar, colvar=colvar, rowgrp=rowgrp, 
	row.FIAname=TRUE, collut=collut, rowgrp.subtot=rowgrp.subtot, allin1=allin1, 
	divideby=divideby, estnull="--", title.colvar=title.colvar) 
 
  esttab <- estdat2$esttab
  tabtitle <- estdat2$esttab.title
  title.rowvar <- estdat2$title.rowvar
  title.colvar <- estdat2$title.colvar
  cnames <- estdat2$cnames
  title.rowgrp <- estdat2$title.rgrp
  psetab <- estdat2$psetab
  psetab.title <- estdat2$psetab.title

  write2xlsx(esttab=esttab, psetab=psetab, tabtitle=paste0("Table ", tabnm, ". ", tabtitle),  
		title.colvar=title.colvar, outfolder=outfolder, title.rowvar=title.rowvar, 
		rowgrp=rowgrp, title.rowgrp=title.rowgrp, rowgrp.subtot=rowgrp.subtot, 
		fill=fill, allin1=allin1, addSEcol=addSEcol, coltottxt=coltottxt, 
		cellwidth=cellwidth, wbnm=wbnm, sheetnm=tabnm, footnote1=footnote1, 
		footnote2=footnote2, footnote3=footnote3)


  #########################################################################################
  ## B12 - Number of growing-stock trees on timberland by species group and diameter class
  #########################################################################################
  tabnm <- "B12"
print(paste("Table", tabnm))
  esttype <- "TREE"
  rowvar <- "SPGRPCD"
  rowgrp <- TRUE
  colvar <- "DIACL2IN"
  title.colvar <- "Diameter class (inches)" 
  landarea <- "TIMBERLAND"

  estvar <- "TPA_UNADJ"
  estvar.filter="TREECLCD == 2 & DIA >= 5.0"

  row.FIAname=TRUE
  col.add0=TRUE
  returntitle=TRUE
  rowgrp.subtot=TRUE

  coltottxt="All classes"
  collut=DIALUT2

  estdat2 <- tabgrp(GBpopdat=popdat, esttype="TREE", landarea=landarea, 
	estvar=estvar, estvar.filter=estvar.filter, rowvar=rowvar, colvar=colvar, rowgrp=rowgrp, 
	row.FIAname=row.FIAname, collut=collut, rowgrp.subtot=rowgrp.subtot, 
	allin1=allin1, divideby=divideby, estnull="--", title.colvar=title.colvar) 
  esttab <- estdat2$esttab
  tabtitle <- estdat2$esttab.title
  title.rowvar <- estdat2$title.rowvar
  title.colvar <- estdat2$title.colvar
  cnames <- estdat2$cnames
  title.rowgrp <- estdat2$title.rgrp
  psetab <- estdat2$psetab
  psetab.title <- estdat2$psetab.title

  write2xlsx(esttab=esttab, psetab=psetab, tabtitle=paste0("Table ", tabnm, ". ", tabtitle),  
		title.colvar=title.colvar, outfolder=outfolder, title.rowvar=title.rowvar, 
		rowgrp=rowgrp, title.rowgrp=title.rowgrp, rowgrp.subtot=rowgrp.subtot, 
		fill=fill, allin1=allin1, addSEcol=addSEcol, coltottxt=coltottxt, 
		cellwidth=cellwidth, wbnm=wbnm, sheetnm=tabnm, footnote1=footnote1, 
		footnote2=footnote2, footnote3=footnote3)


  #########################################################################################
  ## B13 - Net volume of live trees of forest land by owner group, owner class and 
  ##		forest land status
  #########################################################################################
  tabnm <- "B13"
print(paste("Table", tabnm))
  esttype <- "TREE"
  rowvar <- "OWNCD"
  rowgrp <- TRUE
  colvar <- "TIMBERCD.PROD"
  colgrp <- TRUE
  colgrpcd <- "RESERVCD"
  landarea <- "FOREST"
  title.colvar <- "Forest land status" 

  row.FIAname=TRUE
  col.FIAname=TRUE
  col.add0=TRUE
  returntitle=TRUE
  rowgrp.subtot=FALSE

  estvar="VOLCFNET"
  estvar.filter="STATUSCD == 1"
  coltottxt="All forest land"

#  footnote <- "Other forest land is classified as not capable of growing 20 cubic feet/acre/year."
#  if (is.null(footnote3)) {
#    footnote3 <- footnote
#  } else {
#    footnote4 <- footnote
#  }
  estdat2 <- tabgrp(GBpopdat=popdat, esttype="TREE", sumunits=TRUE, landarea=landarea, 
	estvar=estvar, estvar.filter=estvar.filter, rowvar=rowvar, colvar=colvar, rowgrp=rowgrp, 
	colgrp=colgrp, colgrpcd=colgrpcd, row.FIAname=row.FIAname, col.FIAname=col.FIAname, 
	rowgrp.subtot=rowgrp.subtot, allin1=allin1, estnull="--", divideby=divideby.vol, 
	title.colvar=title.colvar) 
  esttab <- estdat2$esttab
  tabtitle <- estdat2$esttab.title
  title.rowvar <- estdat2$title.rowvar
  title.colvar <- estdat2$title.colvar
  cnames <- estdat2$cnames
  title.rowgrp <- estdat2$title.rgrp
  psetab <- estdat2$psetab
  psetab.title <- estdat2$psetab.title

  write2xlsx(esttab=esttab, psetab=psetab, tabtitle=paste0("Table ", tabnm, ". ", tabtitle),  
		title.colvar=NULL, outfolder=outfolder, allin1=allin1, title.rowvar=title.rowvar,
 		title.rowgrp=title.rowgrp, rowgrp=rowgrp, rowgrp.subtot=rowgrp.subtot, 
		addSEcol=addSEcol, fill=fill, colgrp=colgrp, cnames=cnames, coltottxt=coltottxt, 
		cellwidth=cellwidth+2, wbnm=wbnm, sheetnm=tabnm, footnote1=footnote1, 
		footnote2=footnote2, footnote3=footnote3)


  #########################################################################################
  ## B14 - Net volume of standing-dead trees of forest land by owner group, owner class and 
  ##		forest land status
  #########################################################################################
  tabnm <- "B14"
print(paste("Table", tabnm))
  esttype <- "TREE"
  rowvar <- "OWNCD"
  rowgrp <- TRUE
  colvar <- "TIMBERCD.PROD"
  colgrp <- TRUE
  colgrpcd <- "RESERVCD"
  landarea <- "FOREST"
  title.colvar <- "Forest land status" 

  row.FIAname=TRUE
  col.FIAname=TRUE
  col.add0=TRUE
  divideby=divideby.vol
  returntitle=TRUE
  rowgrp.subtot=FALSE

  estvar="VOLCFNET"
  estvar.filter="STANDING_DEAD_CD == 1 & STATUSCD == 2 & DIA >= 5"
  coltottxt="All forest land"

#  footnote <- "Other forest land is classified as not capable of growing 20 cubic feet/acre/year."
#  if (is.null(footnote3)) {
#    footnote3 <- footnote
#  } else {
#    footnote4 <- footnote
#  }
  estdat2 <- tabgrp(GBpopdat=popdat, esttype="TREE", sumunits=TRUE, landarea=landarea, 
	estvar=estvar, estvar.filter=estvar.filter, rowvar=rowvar, colvar=colvar, rowgrp=rowgrp, 
	colgrp=colgrp, colgrpcd=colgrpcd, row.FIAname=row.FIAname, col.FIAname=col.FIAname, 
	rowgrp.subtot=rowgrp.subtot, allin1=allin1, estnull="--", divideby=divideby.vol, 
	title.colvar=title.colvar)
  esttab <- estdat2$esttab
  tabtitle <- estdat2$esttab.title
  title.rowvar <- estdat2$title.rowvar
  title.colvar <- estdat2$title.colvar
  cnames <- estdat2$cnames
  title.rowgrp <- estdat2$title.rgrp
  psetab <- estdat2$psetab
  psetab.title <- estdat2$psetab.title

  write2xlsx(esttab=esttab, psetab=psetab, tabtitle=paste0("Table ", tabnm, ". ", tabtitle),  
		title.colvar=NULL, outfolder=outfolder, allin1=allin1, title.rowvar=title.rowvar,
 		title.rowgrp=title.rowgrp, rowgrp=rowgrp, rowgrp.subtot=rowgrp.subtot, 
		addSEcol=addSEcol, fill=fill, colgrp=colgrp, cnames=cnames, coltottxt=coltottxt, 
		cellwidth=cellwidth+2, wbnm=wbnm, sheetnm=tabnm, footnote1=footnote1, 
		footnote2=footnote2, footnote3=footnote3)


  #########################################################################################
  ## B15 - Net volume of live trees on forest land by forest type group and stand-size class
  #########################################################################################
  tabnm <- "B15"
print(paste("Table", tabnm))
  esttype <- "TREE"
  rowvar <- "FORTYPGRPCD"
  colvar <- "STDSZCD"
  landarea <- "FOREST"

  row.FIAname=TRUE
  col.FIAname=TRUE
  col.add0=TRUE
  returntitle=TRUE

  estvar="VOLCFNET"
  estvar.filter="STATUSCD == 1"
  coltottxt="All size classes"

  estdat <- modGBtree(GBpopdat=popdat, tree=tree, sumunits=TRUE, landarea=landarea,
		estvar=estvar, estvar.filter=estvar.filter, rowvar=rowvar, colvar=colvar, 
		row.FIAname=row.FIAname, col.FIAname=col.FIAname, col.add0=col.add0, 
		returntitle=TRUE, divideby=divideby.vol, allin1=allin1, estnull="--")
  esttab <- estdat$est
  psetab <- estdat$pse
  tabtitle <- ifelse (allin1, estdat$titlelst$title.estpse, estdat$titlelst$title.est)
  title.colvar <- estdat$titlelst$title.colvar
  title.rowvar <- estdat$titlelst$title.rowvar

  write2xlsx(esttab=esttab, psetab=psetab, tabtitle=paste0("Table ", tabnm, ". ", tabtitle),  
		title.colvar=title.colvar, outfolder=outfolder, title.rowvar=title.rowvar, 
		fill=fill, allin1=allin1, addSEcol=addSEcol, coltottxt=coltottxt, 
		cellwidth=cellwidth, wbnm=wbnm, sheetnm=tabnm, footnote1=footnote1, 
		footnote2=footnote2, footnote3=footnote3)


  #########################################################################################
  ## B16 - Net volume of standing-dead trees on forest land by forest type group and stand-size class
  #########################################################################################
  tabnm <- "B16"
print(paste("Table", tabnm))
  esttype <- "TREE"
  rowvar <- "FORTYPGRPCD"
  colvar <- "STDSZCD"
  landarea <- "FOREST"

  row.FIAname=TRUE
  col.FIAname=TRUE
  col.add0=TRUE
  returntitle=TRUE

  estvar="VOLCFNET"
  estvar.filter="STANDING_DEAD_CD == 1 & STATUSCD == 2 & DIA >= 5"
  coltottxt="All size classes"

  estdat <- modGBtree(GBpopdat=popdat, sumunits=TRUE, landarea=landarea, rowvar=rowvar, 
		estvar=estvar, estvar.filter=estvar.filter, colvar=colvar, row.FIAname=row.FIAname, 
		col.FIAname=col.FIAname, col.add0=col.add0, returntitle=TRUE, divideby=divideby.vol, 
		allin1=allin1, estnull="--")
  esttab <- estdat$est
  psetab <- estdat$pse
  tabtitle <- ifelse (allin1, estdat$titlelst$title.estpse, estdat$titlelst$title.est)
  title.colvar <- estdat$titlelst$title.colvar
  title.rowvar <- estdat$titlelst$title.rowvar

  write2xlsx(esttab=esttab, psetab=psetab, tabtitle=paste0("Table ", tabnm, ". ", tabtitle),  
		title.colvar=title.colvar, outfolder=outfolder, title.rowvar=title.rowvar, 
		fill=fill, allin1=allin1, addSEcol=addSEcol, coltottxt=coltottxt, 
		cellwidth=cellwidth, wbnm=wbnm, sheetnm=tabnm, footnote1=footnote1, 
		footnote2=footnote2, footnote3=footnote3)


  #########################################################################################
  ## B17 - Net volume of live trees on forest land by species group and ownership group
  #########################################################################################
  tabnm <- "B17"
print(paste("Table", tabnm))
  esttype <- "TREE"
  rowvar <- "SPGRPCD"
  rowgrp <- TRUE
  colvar <- "OWNGRPCD"
  landarea <- "FOREST"

  estvar="VOLCFNET"
  estvar.filter="STATUSCD == 1"

  row.FIAname=TRUE
  col.FIAname=TRUE
  col.add0=TRUE
  returntitle=TRUE
  rowgrp.subtot=TRUE
  coltottxt="All owners"
 
  estdat2 <- tabgrp(GBpopdat=popdat, esttype="TREE", sumunits=TRUE, landarea=landarea, 
	estvar=estvar, estvar.filter=estvar.filter, rowvar=rowvar, colvar=colvar, rowgrp=rowgrp, 
	row.FIAname=row.FIAname, col.FIAname=col.FIAname, rowgrp.subtot=rowgrp.subtot, 
	allin1=allin1, estnull="--", divideby=divideby.vol) 
  esttab <- estdat2$esttab
  tabtitle <- estdat2$esttab.title
  title.rowvar <- estdat2$title.rowvar
  title.colvar <- estdat2$title.colvar
  cnames <- estdat2$cnames
  title.rowgrp <- estdat2$title.rgrp
  psetab <- estdat2$psetab
  psetab.title <- estdat2$psetab.title

  write2xlsx(esttab=esttab, psetab=psetab, tabtitle=paste0("Table ", tabnm, ". ", tabtitle),  
		title.colvar=title.colvar, outfolder=outfolder, title.rowvar=title.rowvar, 
		rowgrp=rowgrp, title.rowgrp=title.rowgrp, rowgrp.subtot=rowgrp.subtot, 
		fill=fill, allin1=allin1, addSEcol=addSEcol, coltottxt=coltottxt, 
		cellwidth=cellwidth, wbnm=wbnm, sheetnm=tabnm, footnote1=footnote1, 
		footnote2=footnote2, footnote3=footnote3)


  #########################################################################################
  ## B18 - Net volume of standing-dead trees on forest land by species group and ownership group
  #########################################################################################
  tabnm <- "B18"
print(paste("Table", tabnm))
  esttype <- "TREE"
  rowvar <- "SPGRPCD"
  rowgrp <- TRUE
  colvar <- "OWNGRPCD"
  landarea <- "FOREST"

  estvar="VOLCFNET"
  estvar.filter="STANDING_DEAD_CD == 1 & STATUSCD == 2 & DIA >= 5"

  row.FIAname=TRUE
  col.FIAname=TRUE
  col.add0=TRUE
  returntitle=TRUE
  rowgrp.subtot=TRUE
  coltottxt="All owners"

  estdat2 <- tabgrp(GBpopdat=popdat, esttype="TREE", sumunits=TRUE, landarea=landarea,
	estvar=estvar, estvar.filter=estvar.filter, rowvar=rowvar, colvar=colvar, rowgrp=rowgrp, 
	row.FIAname=row.FIAname, col.FIAname=col.FIAname, rowgrp.subtot=rowgrp.subtot, 
	allin1=allin1, estnull="--", divideby=divideby.vol) 
  esttab <- estdat2$esttab
  tabtitle <- estdat2$esttab.title
  title.rowvar <- estdat2$title.rowvar
  title.colvar <- estdat2$title.colvar
  cnames <- estdat2$cnames
  title.rowgrp <- estdat2$title.rgrp
  psetab <- estdat2$psetab
  psetab.title <- estdat2$psetab.title

  write2xlsx(esttab=esttab, psetab=psetab, tabtitle=paste0("Table ", tabnm, ". ", tabtitle),  
		title.colvar=title.colvar, outfolder=outfolder, title.rowvar=title.rowvar, 
		rowgrp=rowgrp, title.rowgrp=title.rowgrp, rowgrp.subtot=rowgrp.subtot, 
		fill=fill, allin1=allin1, addSEcol=addSEcol, coltottxt=coltottxt, 
		cellwidth=cellwidth, wbnm=wbnm, sheetnm=tabnm, footnote1=footnote1, 
		footnote2=footnote2, footnote3=footnote3)


  #########################################################################################
  ## B19 - Net volume of live trees on forest land by species group and diameter class
  #########################################################################################
  tabnm <- "B19"
print(paste("Table", tabnm))
  esttype <- "TREE"
  rowvar <- "SPGRPCD"
  rowgrp <- TRUE
  colvar <- "DIACL2IN"
  col.orderby <- "DIACL2INCD"
  title.colvar <- "Diameter class (inches)" 
  landarea <- "FOREST"

  estvar <- "VOLCFNET"
  estvar.filter="STATUSCD == 1"

  row.FIAname=TRUE
  col.add0=TRUE
  returntitle=TRUE
  rowgrp.subtot=TRUE

  coltottxt="All classes"
  collut=DIALUT2


  estdat2 <- tabgrp(GBpopdat=popdat, esttype="TREE", sumunits=TRUE, landarea=landarea, 
	estvar=estvar, estvar.filter=estvar.filter, rowvar=rowvar, colvar=colvar, rowgrp=rowgrp, 
	row.FIAname=TRUE, collut=collut, rowgrp.subtot=rowgrp.subtot, allin1=allin1, estnull="--", 
	divideby=divideby.vol, title.colvar=title.colvar) 
  esttab <- estdat2$esttab
  tabtitle <- estdat2$esttab.title
  title.rowvar <- estdat2$title.rowvar
  title.colvar <- estdat2$title.colvar
  cnames <- estdat2$cnames
  title.rowgrp <- estdat2$title.rgrp
  psetab <- estdat2$psetab
  psetab.title <- estdat2$psetab.title

  write2xlsx(esttab=esttab, psetab=psetab, tabtitle=paste0("Table ", tabnm, ". ", tabtitle),  
		title.colvar=title.colvar, outfolder=outfolder, title.rowvar=title.rowvar, 
		rowgrp=rowgrp, title.rowgrp=title.rowgrp, rowgrp.subtot=rowgrp.subtot, 
		fill=fill, allin1=allin1, addSEcol=addSEcol, coltottxt=coltottxt, 
		cellwidth=cellwidth, wbnm=wbnm, sheetnm=tabnm, footnote1=footnote1, 
		footnote2=footnote2, footnote3=footnote3)



  #########################################################################################
  ## B20 - Net volume of standing-dead trees on forest land by species group and diameter class
  #########################################################################################
  tabnm <- "B20"
print(paste("Table", tabnm))
  esttype <- "TREE"
  rowvar <- "SPGRPCD"
  rowgrp <- TRUE
  colvar <- "DIACL2IN"
  col.orderby <- "DIACL2INCD"
  title.colvar <- "Diameter class (inches)" 
  landarea <- "FOREST"

  estvar <- "VOLCFNET"
  estvar.filter="STANDING_DEAD_CD == 1 & STATUSCD == 2 & DIA >= 5"

  row.FIAname=TRUE
  col.add0=TRUE
  returntitle=TRUE
  rowgrp.subtot=TRUE

  coltottxt="All classes"
  collut=DIALUT2


  estdat2 <- tabgrp(GBpopdat=popdat, esttype="TREE", sumunits=TRUE, landarea=landarea, 
	estvar=estvar, estvar.filter=estvar.filter, rowvar=rowvar, colvar=colvar, rowgrp=rowgrp, 
	row.FIAname=TRUE, collut=collut, rowgrp.subtot=rowgrp.subtot, 
	allin1=allin1, estnull="--", divideby=divideby.vol, title.colvar=title.colvar) 
  esttab <- estdat2$esttab
  tabtitle <- estdat2$esttab.title
  title.rowvar <- estdat2$title.rowvar
  title.colvar <- estdat2$title.colvar
  cnames <- estdat2$cnames
  title.rowgrp <- estdat2$title.rgrp
  psetab <- estdat2$psetab
  psetab.title <- estdat2$psetab.title

  write2xlsx(esttab=esttab, psetab=psetab, tabtitle=paste0("Table ", tabnm, ". ", tabtitle),  
		title.colvar=title.colvar, outfolder=outfolder, title.rowvar=title.rowvar, 
		rowgrp=rowgrp, title.rowgrp=title.rowgrp, rowgrp.subtot=rowgrp.subtot, 
		fill=fill, allin1=allin1, addSEcol=addSEcol, coltottxt=coltottxt, 
		cellwidth=cellwidth, wbnm=wbnm, sheetnm=tabnm, footnote1=footnote1, 
		footnote2=footnote2, footnote3=footnote3)


  #########################################################################################
  ## B21 - Net volume of live trees on forest land by forest type group and stand origin
  #########################################################################################
  tabnm <- "B21"
print(paste("Table", tabnm))
  esttype <- "TREE"
  rowvar <- "FORTYPGRPCD"
  colvar <- "STDORGCD"
  landarea <- "FOREST"

  row.FIAname=TRUE
  col.FIAname=TRUE
  col.add0=TRUE
  returntitle=TRUE

  estvar="VOLCFNET"
  estvar.filter="STATUSCD == 1"

  coltottxt="All forest land"
  cellwidth=15

  estdat <- modGBtree(GBpopdat=popdat, sumunits=TRUE, landarea=landarea,
		estvar=estvar, estvar.filter=estvar.filter, rowvar=rowvar, colvar=colvar, 
		row.FIAname=row.FIAname, col.FIAname=col.FIAname, col.add0=col.add0, returntitle=TRUE, 
		divideby=divideby.vol, estnull="--", allin1=allin1)
  esttab <- estdat$est
  psetab <- estdat$pse
  tabtitle <- ifelse (allin1, estdat$titlelst$title.estpse, estdat$titlelst$title.est)
  title.colvar <- estdat$titlelst$title.colvar
  title.rowvar <- estdat$titlelst$title.rowvar

  write2xlsx(esttab=esttab, psetab=psetab, tabtitle=paste0("Table ", tabnm, ". ", tabtitle),  
		title.colvar=title.colvar, outfolder=outfolder, title.rowvar=title.rowvar, 
		fill=fill, allin1=allin1, addSEcol=addSEcol, coltottxt=coltottxt, 
		cellwidth=cellwidth, wbnm=wbnm, sheetnm=tabnm, footnote1=footnote1, 
		footnote2=footnote2, footnote3=footnote3)


  #########################################################################################
  ## B22 - Net volume of standing-dead trees on forest land by forest type group and stand origin
  #########################################################################################
  tabnm <- "B22"
print(paste("Table", tabnm))
  esttype <- "TREE"
  rowvar <- "FORTYPGRPCD"
  colvar <- "STDORGCD"
  landarea <- "FOREST"

  row.FIAname=TRUE
  col.FIAname=TRUE
  col.add0=TRUE
  returntitle=TRUE

  estvar="VOLCFNET"
  estvar.filter="STANDING_DEAD_CD == 1 & STATUSCD == 2 & DIA >= 5"

  coltottxt="All forest land"
  cellwidth=15

  estdat <- modGBtree(GBpopdat=popdat, sumunits=TRUE, landarea=landarea,
		estvar=estvar, estvar.filter=estvar.filter, rowvar=rowvar, colvar=colvar, 
		row.FIAname=row.FIAname, col.FIAname=col.FIAname, col.add0=col.add0, returntitle=TRUE, 
		divideby=divideby.vol, estnull="--", allin1=allin1)
  esttab <- estdat$est
  psetab <- estdat$pse
  tabtitle <- ifelse (allin1, estdat$titlelst$title.estpse, estdat$titlelst$title.est)
  title.colvar <- estdat$titlelst$title.colvar
  title.rowvar <- estdat$titlelst$title.rowvar

  write2xlsx(esttab=esttab, psetab=psetab, tabtitle=paste0("Table ", tabnm, ". ", tabtitle),  
		title.colvar=title.colvar, outfolder=outfolder, title.rowvar=title.rowvar, 
		fill=fill, allin1=allin1, addSEcol=addSEcol, coltottxt=coltottxt, 
		cellwidth=cellwidth, wbnm=wbnm, sheetnm=tabnm, footnote1=footnote1, 
		footnote2=footnote2, footnote3=footnote3)


  #########################################################################################
  ## B23 - Net volume of growing-stock trees on timberland by species group and diameter class
  #########################################################################################
  tabnm <- "B23"
print(paste("Table", tabnm))
  esttype <- "TREE"
  rowvar <- "SPGRPCD"
  rowgrp <- TRUE
  colvar <- "DIACL2IN"
  col.orderby <- "DIACL2INCD"
  title.colvar <- "Diameter class (inches)" 
  landarea <- "TIMBERLAND"

  estvar <- "VOLCFNET"
  estvar.filter="TREECLCD == 2 & DIA >= 5.0"

  row.FIAname=TRUE
  col.add0=TRUE
  returntitle=TRUE
  rowgrp.subtot=TRUE

  coltottxt="All classes"
  cellwidth=12
  collut=DIALUT2

  estdat2 <- tabgrp(GBpopdat=popdat, esttype="TREE", sumunits=TRUE, landarea=landarea, 
	estvar=estvar, estvar.filter=estvar.filter, rowvar=rowvar, colvar=colvar, 
	rowgrp=rowgrp, row.FIAname=row.FIAname, collut=collut, rowgrp.subtot=rowgrp.subtot, 
	allin1=allin1, estnull="--", divideby=divideby.vol, title.colvar=title.colvar) 
  esttab <- estdat2$esttab
  tabtitle <- estdat2$esttab.title
  title.rowvar <- estdat2$title.rowvar
  title.colvar <- estdat2$title.colvar
  cnames <- estdat2$cnames
  title.rowgrp <- estdat2$title.rgrp
  psetab <- estdat2$psetab
  psetab.title <- estdat2$psetab.title

  write2xlsx(esttab=esttab, psetab=psetab, tabtitle=paste0("Table ", tabnm, ". ", tabtitle),  
		title.colvar=title.colvar, outfolder=outfolder, title.rowvar=title.rowvar, 
		rowgrp=rowgrp, title.rowgrp=title.rowgrp, rowgrp.subtot=rowgrp.subtot, 
		fill=fill, allin1=allin1, addSEcol=addSEcol, coltottxt=coltottxt, 
		cellwidth=cellwidth, wbnm=wbnm, sheetnm=tabnm, footnote1=footnote1, 
		footnote2=footnote2, footnote3=footnote3)


  #########################################################################################
  ## B24 - Net volume of growing-stock trees on timberland by species group and 
  ##			ownership group
  #########################################################################################
  tabnm <- "B24"
print(paste("Table", tabnm))
  esttype <- "TREE"
  rowvar <- "SPGRPCD"
  rowgrp <- TRUE
  colvar <- "OWNGRPCD"
  landarea <- "TIMBERLAND"

  estvar <- "VOLCFNET"
  estvar.filter="TREECLCD == 2 & DIA >= 5.0"

  row.FIAname=TRUE
  col.FIAname=TRUE
  col.add0=TRUE
  returntitle=TRUE
  rowgrp.subtot=TRUE

  coltottxt="All owners"
  cellwidth=15

  estdat2 <- tabgrp(GBpopdat=popdat, esttype="TREE", sumunits=TRUE, landarea=landarea, 
	estvar=estvar, estvar.filter=estvar.filter, rowvar=rowvar, colvar=colvar, 
	rowgrp=rowgrp, row.FIAname=row.FIAname, col.FIAname=col.FIAname, 
	rowgrp.subtot=rowgrp.subtot, allin1=allin1, divideby=divideby.vol, estnull="--") 
  esttab <- estdat2$esttab
  tabtitle <- estdat2$esttab.title
  title.rowvar <- estdat2$title.rowvar
  title.colvar <- estdat2$title.colvar
  cnames <- estdat2$cnames
  title.rowgrp <- estdat2$title.rgrp
  psetab <- estdat2$psetab
  psetab.title <- estdat2$psetab.title

  write2xlsx(esttab=esttab, psetab=psetab, tabtitle=paste0("Table ", tabnm, ". ", tabtitle),  
		title.colvar=title.colvar, outfolder=outfolder, title.rowvar=title.rowvar, 
		rowgrp=rowgrp, title.rowgrp=title.rowgrp, rowgrp.subtot=rowgrp.subtot, 
		fill=fill, allin1=allin1, addSEcol=addSEcol, coltottxt=coltottxt, 
		cellwidth=cellwidth, wbnm=wbnm, sheetnm=tabnm, footnote1=footnote1, 
		footnote2=footnote2, footnote3=footnote3)


  #########################################################################################
  ## B25 - Net volume of sawtimber trees on timberland by species group and diameter class
  #########################################################################################
  tabnm <- "B25"
print(paste("Table", tabnm))
  esttype <- "TREE"
  rowvar <- "SPGRPCD"
  rowgrp <- TRUE
  colvar <- "DIACL2IN"
  col.orderby <- "DIACL2INCD"
  title.colvar <- "Diameter class (inches)" 
  landarea <- "TIMBERLAND"

  estvar <- "VOLBFNET"
  estvar.filter="TREECLCD == 2 & STATUSCD == 1 & DIA >= 9.0"

  row.FIAname=TRUE
  col.add0=TRUE
  returntitle=TRUE
  rowgrp.subtot=TRUE

  coltottxt="All classes"
  cellwidth=12
  collut=DIALUT2[DIALUT2$MIN >= 9, ]

  estdat2 <- tabgrp(GBpopdat=popdat, esttype="TREE", sumunits=TRUE, landarea=landarea, 
	estvar=estvar, estvar.filter=estvar.filter, rowvar=rowvar, colvar=colvar, 
	rowgrp=rowgrp, row.FIAname=row.FIAname, collut=collut, rowgrp.subtot=rowgrp.subtot, 
	allin1=allin1, estnull="--", divideby=divideby.vol, title.colvar=title.colvar) 
  esttab <- estdat2$esttab
  tabtitle <- estdat2$esttab.title
  title.rowvar <- estdat2$title.rowvar
  title.colvar <- estdat2$title.colvar
  cnames <- estdat2$cnames
  title.rowgrp <- estdat2$title.rgrp
  psetab <- estdat2$psetab
  psetab.title <- estdat2$psetab.title

  write2xlsx(esttab=esttab, psetab=psetab, tabtitle=paste0("Table ", tabnm, ". ", tabtitle),  
		title.colvar=title.colvar, outfolder=outfolder, title.rowvar=title.rowvar, 
		rowgrp=rowgrp, title.rowgrp=title.rowgrp, rowgrp.subtot=rowgrp.subtot, 
		fill=fill, allin1=allin1, addSEcol=addSEcol, coltottxt=coltottxt, 
		cellwidth=cellwidth, wbnm=wbnm, sheetnm=tabnm, footnote1=footnote1, 
		footnote2=footnote2, footnote3=footnote3)


  #########################################################################################
  ## B26 - Net volume of sawtimber trees on timberland by species group and 
  ##			ownership group
  #########################################################################################
  tabnm <- "B26"
print(paste("Table", tabnm))
  esttype <- "TREE"
  rowvar <- "SPGRPCD"
  rowgrp <- TRUE
  colvar <- "OWNGRPCD"
  landarea <- "FOREST"

  estvar <- "VOLBFNET"
  estvar.filter="TREECLCD == 2 & STATUSCD == 1 & DIA >= 9.0"

  row.FIAname=TRUE
  col.FIAname=TRUE
  col.add0=TRUE
  returntitle=TRUE
  rowgrp.subtot=TRUE

  coltottxt="All owners"
  cellwidth=15

  estdat2 <- tabgrp(GBpopdat=popdat, esttype="TREE", sumunits=TRUE, landarea=landarea, 
	estvar=estvar, estvar.filter=estvar.filter, rowvar=rowvar, colvar=colvar, 
	rowgrp=rowgrp, row.FIAname=row.FIAname, col.FIAname=col.FIAname, 
	rowgrp.subtot=rowgrp.subtot, allin1=allin1, divideby=divideby.vol, estnull="--") 
  esttab <- estdat2$esttab
  tabtitle <- estdat2$esttab.title
  title.rowvar <- estdat2$title.rowvar
  title.colvar <- estdat2$title.colvar
  cnames <- estdat2$cnames
  title.rowgrp <- estdat2$title.rgrp
  psetab <- estdat2$psetab
  psetab.title <- estdat2$psetab.title

  write2xlsx(esttab=esttab, psetab=psetab, tabtitle=paste0("Table ", tabnm, ". ", tabtitle),  
		title.colvar=title.colvar, outfolder=outfolder, title.rowvar=title.rowvar, 
		rowgrp=rowgrp, title.rowgrp=title.rowgrp, rowgrp.subtot=rowgrp.subtot, 
		fill=fill, allin1=allin1, addSEcol=addSEcol, coltottxt=coltottxt, 
		cellwidth=cellwidth, wbnm=wbnm, sheetnm=tabnm, footnote1=footnote1, 
		footnote2=footnote2, footnote3=footnote3)


  #########################################################################################
  ## B27 - Aboveground dry weight of live trees by owner class and forest land status
  #########################################################################################
  tabnm <- "B27"
print(paste("Table", tabnm))
  esttype <- "TREE"
  rowvar <- "OWNCD"
  rowgrp <- TRUE
  colvar <- "TIMBERCD.PROD"
  colgrp <- TRUE
  colgrpcd <- "RESERVCD"
  landarea <- "FOREST"

  row.FIAname=TRUE
  col.FIAname=TRUE
  col.add0=TRUE
  returntitle=TRUE
  rowgrp.subtot=FALSE

  estvar="DRYBIO_AG"
  estvar.filter="STATUSCD == 1"

  coltottxt="All forest land"
  cellwidth=13

  estdat2 <- tabgrp(GBpopdat=popdat, esttype="TREE", sumunits=TRUE, landarea=landarea,
	estvar=estvar, estvar.filter=estvar.filter, rowvar=rowvar, colvar=colvar, 
	rowgrp=rowgrp, colgrp=colgrp, colgrpcd=colgrpcd, row.FIAname=row.FIAname, 
	col.FIAname=col.FIAname, rowgrp.subtot=rowgrp.subtot, allin1=allin1, divideby=divideby) 
  esttab <- estdat2$esttab
  tabtitle <- estdat2$esttab.title
  title.rowvar <- estdat2$title.rowvar
  title.colvar <- estdat2$title.colvar
  cnames <- estdat2$cnames
  title.rowgrp <- estdat2$title.rgrp
  psetab <- estdat2$psetab
  psetab.title <- estdat2$psetab.title

  write2xlsx(esttab=esttab, psetab=psetab, tabtitle=paste0("Table ", tabnm, ". ", tabtitle),  
		title.colvar=NULL, outfolder=outfolder, allin1=allin1, title.rowvar=title.rowvar,
 		title.rowgrp=title.rowgrp, rowgrp=rowgrp, rowgrp.subtot=rowgrp.subtot, 
		addSEcol=addSEcol, fill=fill, colgrp=colgrp, cnames=cnames, coltottxt=coltottxt, 
		cellwidth=cellwidth, wbnm=wbnm, sheetnm=tabnm, footnote1=footnote1, 
		footnote2=footnote2, footnote3=footnote3, footnote4=footnote4)


  #########################################################################################
  ## B28 - Aboveground dry weight of standing-dead trees by owner class and forest land status
  #########################################################################################
  tabnm <- "B28"
print(paste("Table", tabnm))
  esttype <- "TREE"
  rowvar <- "OWNCD"
  rowgrp <- TRUE
  colvar <- "TIMBERCD.PROD"
  colgrp <- TRUE
  colgrpcd <- "RESERVCD"
  landarea <- "FOREST"

  row.FIAname=TRUE
  col.FIAname=TRUE
  col.add0=TRUE
  returntitle=TRUE
  rowgrp.subtot=FALSE

  estvar="DRYBIO_AG"
  estvar.filter="STATUSCD == 2"

  coltottxt="All forest land"
  cellwidth=13

  estdat2 <- tabgrp(GBpopdat=popdat, esttype="TREE", sumunits=TRUE, landarea=landarea,
	estvar=estvar, estvar.filter=estvar.filter, rowvar=rowvar, colvar=colvar, 
	rowgrp=rowgrp, colgrp=colgrp, colgrpcd=colgrpcd, row.FIAname=row.FIAname, 
	col.FIAname=col.FIAname, rowgrp.subtot=rowgrp.subtot, allin1=allin1, divideby=divideby) 
  esttab <- estdat2$esttab
  tabtitle <- estdat2$esttab.title
  title.rowvar <- estdat2$title.rowvar
  title.colvar <- estdat2$title.colvar
  cnames <- estdat2$cnames
  title.rowgrp <- estdat2$title.rgrp
  psetab <- estdat2$psetab
  psetab.title <- estdat2$psetab.title

  write2xlsx(esttab=esttab, psetab=psetab, tabtitle=paste0("Table ", tabnm, ". ", tabtitle),  
		title.colvar=NULL, outfolder=outfolder, allin1=allin1, title.rowvar=title.rowvar,
 		title.rowgrp=title.rowgrp, rowgrp=rowgrp, rowgrp.subtot=rowgrp.subtot, 
		addSEcol=addSEcol, fill=fill, colgrp=colgrp, cnames=cnames, coltottxt=coltottxt, 
		cellwidth=cellwidth, wbnm=wbnm, sheetnm=tabnm, footnote1=footnote1, 
		footnote2=footnote2, footnote3=footnote3, footnote4=footnote4)



  #########################################################################################
  ## B29 - Aboveground dry weight of live trees on forest land by species group 
  ##			and diameter class
  #########################################################################################
  tabnm <- "B29"
print(paste("Table", tabnm))
  esttype <- "TREE"
  rowvar <- "SPGRPCD"
  rowgrp <- TRUE
  colvar <- "DIACL2IN"
  col.orderby <- "DIACL2INCD"
  title.colvar <- "Diameter class (inches)" 
  landarea <- "FOREST"
 
  estvar <- "DRYBIO_AG"
  estvar.filter="STATUSCD == 1"

  row.FIAname=TRUE
  col.add0=TRUE
  returntitle=TRUE
  rowgrp.subtot=TRUE

  coltottxt="All classes"
  cellwidth=12
  collut=DIALUT

  estdat2 <- tabgrp(GBpopdat=popdat, esttype="TREE", sumunits=TRUE, landarea=landarea, 
	estvar=estvar, estvar.filter=estvar.filter, rowvar=rowvar, colvar=colvar, 
	rowgrp=rowgrp, row.FIAname=row.FIAname, collut=collut, rowgrp.subtot=rowgrp.subtot, 
	allin1=allin1, divideby=divideby, title.colvar=title.colvar) 
  esttab <- estdat2$esttab
  tabtitle <- estdat2$esttab.title
  title.rowvar <- estdat2$title.rowvar
  title.colvar <- estdat2$title.colvar
  cnames <- estdat2$cnames
  title.rowgrp <- estdat2$title.rgrp
  psetab <- estdat2$psetab
  psetab.title <- estdat2$psetab.title

  write2xlsx(esttab=esttab, psetab=psetab, tabtitle=paste0("Table ", tabnm, ". ", tabtitle),  
		title.colvar=title.colvar, outfolder=outfolder, title.rowvar=title.rowvar, 
		rowgrp=rowgrp, title.rowgrp=title.rowgrp, rowgrp.subtot=rowgrp.subtot, 
		fill=fill, allin1=allin1, addSEcol=addSEcol, coltottxt=coltottxt, 
		cellwidth=cellwidth, wbnm=wbnm, sheetnm=tabnm, footnote1=footnote1, 
		footnote2=footnote2, footnote3=footnote3, footnote4=footnote4)


  #########################################################################################
  ## B30 - Aboveground dry weight of standing-dead trees on forest land by species group 
  ##			and diameter class
  #########################################################################################
  tabnm <- "B30"
print(paste("Table", tabnm))
  esttype <- "TREE"
  rowvar <- "SPGRPCD"
  rowgrp <- TRUE
  colvar <- "DIACL2IN"
  col.orderby <- "DIACL2INCD"
  title.colvar <- "Diameter class (inches)" 
  landarea <- "FOREST"
 
  estvar <- "DRYBIO_AG"
  estvar.filter="STATUSCD == 2"

  row.FIAname=TRUE
  col.add0=TRUE
  returntitle=TRUE
  rowgrp.subtot=TRUE

  coltottxt="All classes"
  cellwidth=12
  collut=DIALUT

  estdat2 <- tabgrp(GBpopdat=popdat, esttype="TREE", sumunits=TRUE, landarea=landarea, 
	estvar=estvar, estvar.filter=estvar.filter, rowvar=rowvar, colvar=colvar, rowgrp=rowgrp, 
	row.FIAname=row.FIAname, collut=collut, rowgrp.subtot=rowgrp.subtot, 
	allin1=allin1, divideby=divideby, title.colvar=title.colvar) 
  esttab <- estdat2$esttab
  tabtitle <- estdat2$esttab.title
  title.rowvar <- estdat2$title.rowvar
  title.colvar <- estdat2$title.colvar
  cnames <- estdat2$cnames
  title.rowgrp <- estdat2$title.rgrp
  psetab <- estdat2$psetab
  psetab.title <- estdat2$psetab.title

  write2xlsx(esttab=esttab, psetab=psetab, tabtitle=paste0("Table ", tabnm, ". ", tabtitle),  
		title.colvar=title.colvar, outfolder=outfolder, title.rowvar=title.rowvar, 
		rowgrp=rowgrp, title.rowgrp=title.rowgrp, rowgrp.subtot=rowgrp.subtot, 
		fill=fill, allin1=allin1, addSEcol=addSEcol, coltottxt=coltottxt, 
		cellwidth=cellwidth, wbnm=wbnm, sheetnm=tabnm, footnote1=footnote1, 
		footnote2=footnote2, footnote3=footnote3, footnote4=footnote4)




  #########################################################################################
  ## B31 - Aboveground carbon in live trees by owner class and forest land status
  #########################################################################################
  tabnm <- "B31"
print(paste("Table", tabnm))
  esttype <- "TREE"
  rowvar <- "OWNCD"
  rowgrp <- TRUE
  colvar <- "TIMBERCD.PROD"
  colgrp <- TRUE
  colgrpcd <- "RESERVCD"
  landarea <- "FOREST"

  row.FIAname=TRUE
  col.FIAname=TRUE
  col.add0=TRUE
  returntitle=TRUE
  rowgrp.subtot=FALSE

  estvar="CARBON_AG"
  estvar.filter="STATUSCD == 1"

  coltottxt="All forest land"
  cellwidth=13

  estdat2 <- tabgrp(GBpopdat=popdat, esttype="TREE", sumunits=TRUE, landarea=landarea,
	estvar=estvar, estvar.filter=estvar.filter, rowvar=rowvar, colvar=colvar, 
	rowgrp=rowgrp, colgrp=colgrp, colgrpcd=colgrpcd, row.FIAname=row.FIAname, 
	col.FIAname=col.FIAname, rowgrp.subtot=rowgrp.subtot, allin1=allin1, divideby=divideby) 
  esttab <- estdat2$esttab
  tabtitle <- estdat2$esttab.title
  title.rowvar <- estdat2$title.rowvar
  title.colvar <- estdat2$title.colvar
  cnames <- estdat2$cnames
  title.rowgrp <- estdat2$title.rgrp
  psetab <- estdat2$psetab
  psetab.title <- estdat2$psetab.title

  write2xlsx(esttab=esttab, psetab=psetab, tabtitle=paste0("Table ", tabnm, ". ", tabtitle),  
		title.colvar=NULL, outfolder=outfolder, allin1=allin1, title.rowvar=title.rowvar,
 		title.rowgrp=title.rowgrp, rowgrp=rowgrp, rowgrp.subtot=rowgrp.subtot, 
		addSEcol=addSEcol, fill=fill, colgrp=colgrp, cnames=cnames, coltottxt=coltottxt, 
		cellwidth=cellwidth, wbnm=wbnm, sheetnm=tabnm, footnote1=footnote1, 
		footnote2=footnote2, footnote3=footnote3, footnote4=footnote4)


  #########################################################################################
  ## B32 - Aboveground carbon in standing-dead trees by owner class and forest land status
  #########################################################################################
  tabnm <- "B32"
print(paste("Table", tabnm))
  esttype <- "TREE"
  rowvar <- "OWNCD"
  rowgrp <- TRUE
  colvar <- "TIMBERCD.PROD"
  colgrp <- TRUE
  colgrpcd <- "RESERVCD"
  landarea <- "FOREST"

  row.FIAname=TRUE
  col.FIAname=TRUE
  col.add0=TRUE
  returntitle=TRUE
  rowgrp.subtot=FALSE

  estvar="CARBON_AG"
  estvar.filter="STATUSCD == 2"

  coltottxt="All forest land"
  cellwidth=13

  estdat2 <- tabgrp(GBpopdat=popdat, esttype="TREE", sumunits=TRUE, landarea=landarea,
	estvar=estvar, estvar.filter=estvar.filter, rowvar=rowvar, colvar=colvar, 
	rowgrp=rowgrp, colgrp=colgrp, colgrpcd=colgrpcd, row.FIAname=row.FIAname, 
	col.FIAname=col.FIAname, rowgrp.subtot=rowgrp.subtot, allin1=allin1, divideby=divideby) 
  esttab <- estdat2$esttab
  tabtitle <- estdat2$esttab.title
  title.rowvar <- estdat2$title.rowvar
  title.colvar <- estdat2$title.colvar
  cnames <- estdat2$cnames
  title.rowgrp <- estdat2$title.rgrp
  psetab <- estdat2$psetab
  psetab.title <- estdat2$psetab.title

  write2xlsx(esttab=esttab, psetab=psetab, tabtitle=paste0("Table ", tabnm, ". ", tabtitle),  
		title.colvar=NULL, outfolder=outfolder, allin1=allin1, title.rowvar=title.rowvar,
 		title.rowgrp=title.rowgrp, rowgrp=rowgrp, rowgrp.subtot=rowgrp.subtot, 
		addSEcol=addSEcol, fill=fill, colgrp=colgrp, cnames=cnames, coltottxt=coltottxt, 
		cellwidth=cellwidth, wbnm=wbnm, sheetnm=tabnm, footnote1=footnote1, 
		footnote2=footnote2, footnote3=footnote3, footnote4=footnote4)



  #########################################################################################
  ## B33 - Avg annual net growth of live trees by owner group, owner class and 
  ##			forest land status
  #########################################################################################
  tabnm <- "B33"
print(paste("Table", tabnm))
  esttype <- "TREE"
  rowvar <- "OWNCD"
  rowgrp <- TRUE
  colvar <- "TIMBERCD.PROD"
  colgrp <- TRUE
  colgrpcd <- "RESERVCD"
  landarea <- "FOREST"

  row.FIAname=TRUE
  col.FIAname=TRUE
  col.add0=TRUE
  returntitle=TRUE
  rowgrp.subtot=FALSE

  estvar="FGROWCFAL"
  estvar.filter=NULL

  coltottxt="All forest land"
  cellwidth=13

  estdat2 <- tabgrp(GBpopdat=popdat03, esttype="TREE", sumunits=TRUE, landarea=landarea,
	estvar=estvar, estvar.filter=estvar.filter, rowvar=rowvar, colvar=colvar, 
	rowgrp=rowgrp, colgrp=colgrp, colgrpcd=colgrpcd, row.FIAname=row.FIAname, 
	col.FIAname=col.FIAname, rowgrp.subtot=rowgrp.subtot, allin1=allin1, divideby=divideby.vol) 
  esttab <- estdat2$esttab
  tabtitle <- estdat2$esttab.title
  title.rowvar <- estdat2$title.rowvar
  title.colvar <- estdat2$title.colvar
  cnames <- estdat2$cnames
  title.rowgrp <- estdat2$title.rgrp
  psetab <- estdat2$psetab
  psetab.title <- estdat2$psetab.title

  write2xlsx(esttab=esttab, psetab=psetab, tabtitle=paste0("Table ", tabnm, ". ", tabtitle),  
		title.colvar=NULL, outfolder=outfolder, allin1=allin1, title.rowvar=title.rowvar,
 		title.rowgrp=title.rowgrp, rowgrp=rowgrp, rowgrp.subtot=rowgrp.subtot, 
		addSEcol=addSEcol, fill=fill, colgrp=colgrp, cnames=cnames, coltottxt=coltottxt, 
		cellwidth=cellwidth, wbnm=wbnm, sheetnm=tabnm, footnote1=footnote1, 
		footnote2=footnote2, footnote3=footnote3, footnote4=footnote4)


  #########################################################################################
  ## B34 - Avg annual net growth of live trees on forest land by forest type group 
  ##			and stand-size class
  #########################################################################################
  tabnm <- "B34"
print(paste("Table", tabnm))
  esttype <- "TREE"
  rowvar <- "FORTYPGRPCD"
  colvar <- "STDSZCD"
  landarea <- "FOREST"

  row.FIAname=TRUE
  col.FIAname=TRUE
  col.add0=TRUE
  returntitle=TRUE

  estvar="FGROWCFAL"
  estvar.filter=NULL
  coltottxt="All size classes"
  cellwidth=13

  estdat <- modGBtree(GBpopdat=popdat03, sumunits=TRUE, landarea=landarea,
		estvar=estvar, estvar.filter=estvar.filter, rowvar=rowvar, colvar=colvar, 
		row.FIAname=row.FIAname, col.FIAname=col.FIAname, col.add0=col.add0, 
		returntitle=TRUE, divideby=divideby.vol, allin1=allin1)
  esttab <- estdat$est
  psetab <- estdat$pse
  tabtitle <- ifelse (allin1, estdat$titlelst$title.estpse, estdat$titlelst$title.est)
  title.colvar <- estdat$titlelst$title.colvar
  title.rowvar <- estdat$titlelst$title.rowvar

  write2xlsx(esttab=esttab, psetab=psetab, tabtitle=paste0("Table ", tabnm, ". ", tabtitle),  
		title.colvar=title.colvar, outfolder=outfolder, title.rowvar=title.rowvar, 
		fill=fill, allin1=allin1, addSEcol=addSEcol, coltottxt=coltottxt, 
		cellwidth=cellwidth, wbnm=wbnm, sheetnm=tabnm, footnote1=footnote1, 
		footnote2=footnote2, footnote3=footnote3, footnote4=footnote4)


  #########################################################################################
  ## B35 - Avg annual net growth of live trees on forest land by species group 
  ##			and ownership group
  #########################################################################################
  tabnm <- "B35"
print(paste("Table", tabnm))
  esttype <- "TREE"
  rowvar <- "SPGRPCD"
  rowgrp <- TRUE
  colvar <- "OWNGRPCD"
  landarea <- "FOREST"

  estvar <- "FGROWCFAL"
  estvar.filter=NULL

  row.FIAname=TRUE
  col.FIAname=TRUE
  col.add0=TRUE
  returntitle=TRUE
  rowgrp.subtot=TRUE

  coltottxt="All owners"
  cellwidth=14

  estdat2 <- tabgrp(GBpopdat=popdat, esttype="TREE", sumunits=TRUE, landarea=landarea, 
	estvar=estvar, estvar.filter=estvar.filter, rowvar=rowvar, colvar=colvar, 
	rowgrp=rowgrp, row.FIAname=row.FIAname, col.FIAname=col.FIAname, 
	rowgrp.subtot=rowgrp.subtot, allin1=allin1, divideby=divideby.vol) 
  esttab <- estdat2$esttab
  tabtitle <- estdat2$esttab.title
  title.rowvar <- estdat2$title.rowvar
  title.colvar <- estdat2$title.colvar
  cnames <- estdat2$cnames
  title.rowgrp <- estdat2$title.rgrp
  psetab <- estdat2$psetab
  psetab.title <- estdat2$psetab.title

  write2xlsx(esttab=esttab, psetab=psetab, tabtitle=paste0("Table ", tabnm, ". ", tabtitle),  
		title.colvar=title.colvar, outfolder=outfolder, title.rowvar=title.rowvar, 
		rowgrp=rowgrp, title.rowgrp=title.rowgrp, rowgrp.subtot=rowgrp.subtot, 
		fill=fill, allin1=allin1, addSEcol=addSEcol, coltottxt=coltottxt, 
		cellwidth=cellwidth, wbnm=wbnm, sheetnm=tabnm, footnote1=footnote1, 
		footnote2=footnote2, footnote3=footnote3, footnote4=footnote4)



  #########################################################################################
  ## B36 - Avg annual net growth of growing-stock trees on forest land by species 
  ##			group and ownership group
  #########################################################################################
  tabnm <- "B36"
print(paste("Table", tabnm))
  esttype <- "TREE"
  rowvar <- "SPGRPCD"
  rowgrp <- TRUE
  colvar <- "OWNGRPCD"
  landarea <- "TIMBERLAND"

  estvar <- "FGROWCFGS"
  estvar.filter=NULL

  row.FIAname=TRUE
  col.FIAname=TRUE
  col.add0=TRUE
  returntitle=TRUE
  rowgrp.subtot=TRUE

  coltottxt="All owners"
  cellwidth=14

  estdat2 <- tabgrp(GBpopdat=popdat, esttype="TREE", sumunits=TRUE, landarea=landarea, 
	estvar=estvar, estvar.filter=estvar.filter, rowvar=rowvar, colvar=colvar, rowgrp=rowgrp, 
	row.FIAname=row.FIAname, col.FIAname=col.FIAname, rowgrp.subtot=rowgrp.subtot, 
	allin1=allin1, divideby=divideby.vol) 
  esttab <- estdat2$esttab
  tabtitle <- estdat2$esttab.title
  title.rowvar <- estdat2$title.rowvar
  title.colvar <- estdat2$title.colvar
  cnames <- estdat2$cnames
  title.rowgrp <- estdat2$title.rgrp
  psetab <- estdat2$psetab
  psetab.title <- estdat2$psetab.title

  write2xlsx(esttab=esttab, psetab=psetab, tabtitle=paste0("Table ", tabnm, ". ", tabtitle),  
		title.colvar=title.colvar, outfolder=outfolder, title.rowvar=title.rowvar, 
		rowgrp=rowgrp, title.rowgrp=title.rowgrp, rowgrp.subtot=rowgrp.subtot, 
		fill=fill, allin1=allin1, addSEcol=addSEcol, coltottxt=coltottxt, 
		cellwidth=cellwidth, wbnm=wbnm, sheetnm=tabnm, footnote1=footnote1, 
		footnote2=footnote2, footnote3=footnote3, footnote4=footnote4)


  #########################################################################################
  ## B37 - Avg annual mortality of trees by owner class and forest land status
  #########################################################################################
  tabnm <- "B37"
print(paste("Table", tabnm))
  esttype <- "TREE"
  rowvar <- "OWNCD"
  rowgrp <- TRUE
  colvar <- "TIMBERCD.PROD"
  colgrp <- TRUE
  colgrpcd <- "RESERVCD"
  landarea <- "FOREST"

  row.FIAname=TRUE
  col.FIAname=TRUE
  col.add0=TRUE
  returntitle=TRUE
  rowgrp.subtot=FALSE

  estvar="FMORTCFAL"
  estvar.filter <- NULL

  coltottxt="All forest land"
  cellwidth=13

  estdat2 <- tabgrp(GBpopdat=popdat03, esttype="TREE", sumunits=TRUE, landarea=landarea,
	estvar=estvar, estvar.filter=estvar.filter, rowvar=rowvar, colvar=colvar, 
	rowgrp=rowgrp, colgrp=colgrp, colgrpcd=colgrpcd, row.FIAname=row.FIAname, 
	col.FIAname=col.FIAname, rowgrp.subtot=rowgrp.subtot, allin1=allin1, divideby=divideby.vol) 
  esttab <- estdat2$esttab
  tabtitle <- estdat2$esttab.title
  title.rowvar <- estdat2$title.rowvar
  title.colvar <- estdat2$title.colvar
  cnames <- estdat2$cnames
  title.rowgrp <- estdat2$title.rowgrp
  psetab <- estdat2$psetab
  psetab.title <- estdat2$psetab.title

  write2xlsx(esttab=esttab, psetab=psetab, tabtitle=paste0("Table ", tabnm, ". ", tabtitle),  
		title.colvar=NULL, outfolder=outfolder, allin1=allin1, title.rowvar=title.rowvar,
 		title.rowgrp=title.rowgrp, rowgrp=rowgrp, rowgrp.subtot=rowgrp.subtot, 
		addSEcol=addSEcol, fill=fill, colgrp=colgrp, cnames=cnames, coltottxt=coltottxt, 
		cellwidth=cellwidth, wbnm=wbnm, sheetnm=tabnm, footnote1=footnote1, 
		footnote2=footnote2, footnote3=footnote3, footnote4=footnote4)


  #########################################################################################
  ## B38 - Avg annual mortality on forest land by forest type group and stand-size class
  #########################################################################################
  tabnm <- "B38"
print(paste("Table", tabnm))
  esttype <- "TREE"
  rowvar <- "FORTYPGRPCD"
  colvar <- "STDSZCD"
  landarea <- "FOREST"

  row.FIAname=TRUE
  col.FIAname=TRUE
  col.add0=TRUE
  returntitle=TRUE

  estvar="FMORTCFAL"
  estvar.filter <- NULL

  coltottxt="All size classes"
  cellwidth=13

  estdat <- modGBtree(GBpopdat=popdat, sumunits=TRUE, landarea=landarea, estvar=estvar, 
		estvar.filter=estvar.filter, rowvar=rowvar, colvar=colvar, row.FIAname=row.FIAname,
 		col.FIAname=col.FIAname, col.add0=col.add0, returntitle=TRUE, divideby=divideby.vol, 
		allin1=allin1)
  esttab <- estdat$est
  psetab <- estdat$pse
  tabtitle <- ifelse (allin1, estdat$titlelst$title.estpse, estdat$titlelst$title.est)
  title.colvar <- estdat$titlelst$title.colvar
  title.rowvar <- estdat$titlelst$title.rowvar

  write2xlsx(esttab=esttab, psetab=psetab, tabtitle=paste0("Table ", tabnm, ". ", tabtitle),  
		title.colvar=title.colvar, outfolder=outfolder, title.rowvar=title.rowvar, 
		fill=fill, allin1=allin1, addSEcol=addSEcol, coltottxt=coltottxt, 
		cellwidth=cellwidth, wbnm=wbnm, sheetnm=tabnm, footnote1=footnote1, 
		footnote2=footnote2, footnote3=footnote3, footnote4=footnote4)


  #########################################################################################
  ## B39 - Avg annual mortality of trees on forest land by species group and ownership group
  #########################################################################################
  tabnm <- "B39"
print(paste("Table", tabnm))
  esttype <- "TREE"
  rowvar <- "SPGRPCD"
  rowgrp <- TRUE
  colvar <- "OWNGRPCD"
  landarea <- "FOREST"

  estvar <- "FMORTCFAL"
  estvar.filter <- NULL

  row.FIAname=TRUE
  col.FIAname=TRUE
  col.add0=TRUE
  returntitle=TRUE
  rowgrp.subtot=TRUE

  coltottxt="All owners"
  cellwidth=14

  estdat2 <- tabgrp(GBpopdat=popdat03, esttype="TREE", sumunits=TRUE, landarea=landarea, 
	estvar=estvar, estvar.filter=estvar.filter, rowvar=rowvar, colvar=colvar, 
	rowgrp=rowgrp, row.FIAname=row.FIAname, col.FIAname=col.FIAname, 
	rowgrp.subtot=rowgrp.subtot, allin1=allin1, divideby=divideby.vol) 
  esttab <- estdat2$esttab
  tabtitle <- estdat2$esttab.title
  title.rowvar <- estdat2$title.rowvar
  title.colvar <- estdat2$title.colvar
  cnames <- estdat2$cnames
  title.rowgrp <- estdat2$title.rgrp
  psetab <- estdat2$psetab
  psetab.title <- estdat2$psetab.title

  write2xlsx(esttab=esttab, psetab=psetab, tabtitle=paste0("Table ", tabnm, ". ", tabtitle),  
		title.colvar=title.colvar, outfolder=outfolder, title.rowvar=title.rowvar, 
		rowgrp=rowgrp, title.rowgrp=title.rowgrp, rowgrp.subtot=rowgrp.subtot, 
		fill=fill, allin1=allin1, addSEcol=addSEcol, coltottxt=coltottxt, 
		cellwidth=cellwidth, wbnm=wbnm, sheetnm=tabnm, footnote1=footnote1, 
		footnote2=footnote2, footnote3=footnote3, footnote4=footnote4)


  #########################################################################################
  ## B40 - Avg annual mortality of growing-stock trees on timberland by species group 
  ##		and ownership group
  #########################################################################################
  tabnm <- "B40"
print(paste("Table", tabnm))
  esttype <- "TREE"
  rowvar <- "SPGRPCD"
  rowgrp <- TRUE
  colvar <- "OWNGRPCD"
  landarea <- "TIMBERLAND"

  estvar <- "FMORTCFGS"
  estvar.filter <- NULL

  row.FIAname=TRUE
  col.FIAname=TRUE
  col.add0=TRUE
  returntitle=TRUE
  rowgrp.subtot=TRUE

  coltottxt="All owners"
  cellwidth=14

  estdat2 <- tabgrp(GBpopdat=popdat, esttype="TREE", sumunits=TRUE, landarea=landarea, 
	estvar=estvar, estvar.filter=estvar.filter, rowvar=rowvar, colvar=colvar, 
	rowgrp=rowgrp, row.FIAname=row.FIAname, col.FIAname=col.FIAname, 
	rowgrp.subtot=rowgrp.subtot, allin1=allin1, divideby=divideby.vol) 
  esttab <- estdat2$esttab
  tabtitle <- estdat2$esttab.title
  title.rowvar <- estdat2$title.rowvar
  title.colvar <- estdat2$title.colvar
  cnames <- estdat2$cnames
  title.rowgrp <- estdat2$title.rgrp
  psetab <- estdat2$psetab
  psetab.title <- estdat2$psetab.title

  write2xlsx(esttab=esttab, psetab=psetab, tabtitle=paste0("Table ", tabnm, ". ", tabtitle),  
		title.colvar=title.colvar, outfolder=outfolder, title.rowvar=title.rowvar, 
		rowgrp=rowgrp, title.rowgrp=title.rowgrp, rowgrp.subtot=rowgrp.subtot, 
		fill=fill, allin1=allin1, addSEcol=addSEcol, coltottxt=coltottxt, 
		cellwidth=cellwidth, wbnm=wbnm, sheetnm=tabnm, footnote1=footnote1, 
		footnote2=footnote2, footnote3=footnote3, footnote4=footnote4)



  #########################################################################################
  ## B41 - Area of forest land by inventory unit, county and forest land status
  #########################################################################################
  tabnm <- "B41"
print(paste("Table", tabnm))
  esttype <- "AREA"
  rowvar <- "COUNTYNM"
  row.orderby="COUNTYCD"
  rowgrp <- TRUE
  rowgrpnm <- "UNITNM"
  rowgrpord <- "UNITCD"
  colvar <- "TIMBERCD.PROD"
  colgrp <- TRUE
  colgrpcd <- "RESERVCD"
  landarea <- "FOREST"

  col.FIAname=TRUE
  col.add0=TRUE
  returntitle=TRUE
  rowgrp.subtot=TRUE

  coltottxt="All forest land"
  cellwidth=13

  estdat2 <- tabgrp(GBpopdat=popdat, esttype="AREA", sumunits=TRUE, landarea=landarea, 
	rowvar=rowvar, row.orderby=row.orderby, colvar=colvar, rowgrp=rowgrp, colgrp=colgrp, 
	colgrpcd=colgrpcd, rowgrpnm=rowgrpnm, rowgrpord=rowgrpord, col.FIAname=TRUE, 
	rowgrp.subtot=rowgrp.subtot, allin1=allin1, divideby=divideby, title.rowgrp=title.rowgrp) 
  esttab <- estdat2$esttab
  tabtitle <- estdat2$esttab.title
  title.rowvar <- estdat2$title.rowvar
  title.colvar <- estdat2$title.colvar
  cnames <- estdat2$cnames
  title.rowgrp <- estdat2$title.rowgrp
  psetab <- estdat2$psetab
  psetab.title <- estdat2$psetab.title

  write2xlsx(esttab=esttab, psetab=psetab, tabtitle=paste0("Table ", tabnm, ". ", tabtitle),  
		title.colvar=NULL, outfolder=outfolder, allin1=allin1, title.rowvar=title.rowvar,
 		title.rowgrp=title.rowgrp, rowgrp=rowgrp, rowgrp.subtot=rowgrp.subtot, 
		addSEcol=addSEcol, fill=fill, colgrp=colgrp, cnames=cnames, coltottxt=coltottxt, 
		cellwidth=cellwidth, wbnm=wbnm, sheetnm=tabnm, footnote1=footnote1, 
		footnote2=footnote2, footnote3=footnote3, footnote4=footnote4)


  #########################################################################################
  ## B42 - Area of forest land by inventory unit, county, ownership group, 
  ##			and forest land status
  #########################################################################################
  tabnm <- "B42"
print(paste("Table", tabnm))
  esttype <- "AREA"
  rowvar <- "COUNTYNM"
  row.orderby="COUNTYCD"
  rowgrp <- TRUE
  rowgrpnm="UNITNM"
  rowgrpord="UNITCD"
  colvar <- "TIMBERCD"
  colgrp <- TRUE
  colgrpcd <- "OWNGRPCD"
  landarea <- "FOREST"

  col.FIAname=TRUE
  col.add0=TRUE
  returntitle=TRUE
  rowgrp.subtot=TRUE

  coltottxt="All forest land"
  cellwidth=13

  estdat2 <- tabgrp(GBpopdat=popdat, esttype="AREA", sumunits=TRUE, landarea=landarea, 
	rowvar=rowvar, colvar=colvar, rowgrp=rowgrp, colgrp=TRUE, colgrpcd=colgrpcd,
	row.orderby=row.orderby, rowgrpnm=rowgrpnm, rowgrpord=rowgrpord, 
	col.FIAname=col.FIAname, rowgrp.subtot=rowgrp.subtot, allin1=allin1, divideby=divideby) 
  esttab <- estdat2$esttab
  tabtitle <- estdat2$esttab.title
  title.rowvar <- estdat2$title.rowvar
  title.colvar <- estdat2$title.colvar
  cnames <- estdat2$cnames
  title.rowgrp <- estdat2$title.rowgrp
  psetab <- estdat2$psetab
  psetab.title <- estdat2$psetab.title

  write2xlsx(esttab=esttab, psetab=psetab, tabtitle=paste0("Table ", tabnm, ". ", tabtitle),  
		title.colvar=NULL, outfolder=outfolder, allin1=allin1, title.rowvar=title.rowvar,
 		title.rowgrp=title.rowgrp, rowgrp=rowgrp, rowgrp.subtot=rowgrp.subtot, 
		addSEcol=addSEcol, fill=fill, colgrp=TRUE, cnames=cnames, coltotal=TRUE, 
		coltottxt=coltottxt, cellwidth=cellwidth, wbnm=wbnm, sheetnm=tabnm,
		footnote1=footnote1, footnote2=footnote2, footnote3=footnote3, footnote4=footnote4)



  #########################################################################################
  ## B43 - Area of timberland by inventory unit, county, and stand-size class 
  #########################################################################################
  tabnm <- "B43"
print(paste("Table", tabnm))
  esttype <- "AREA"
  rowvar <- "COUNTYNM"
  row.orderby="COUNTYCD"
  rowgrp <- TRUE
  rowgrpnm="UNITNM"
  rowgrpord="UNITCD"
  colvar <- "STDSZCD"
  colgrp <- FALSE
  landarea <- "TIMBERLAND"

  col.FIAname=TRUE
  col.add0=TRUE
  returntitle=TRUE

  coltottxt="All size classes"
  cellwidth=13
  rowgrp.subtot=TRUE

  estdat2 <- tabgrp(GBpopdat=popdat, esttype="AREA", sumunits=TRUE, landarea=landarea, 
	rowvar=rowvar, colvar=colvar, rowgrp=rowgrp, colgrp=colgrp, colgrpcd=colgrpcd,
	row.orderby=row.orderby, rowgrpnm=rowgrpnm, rowgrpord=rowgrpord, 
	col.FIAname=col.FIAname, rowgrp.subtot=rowgrp.subtot, allin1=allin1, divideby=divideby) 
  esttab <- estdat2$esttab
  tabtitle <- estdat2$esttab.title
  title.rowvar <- estdat2$title.rowvar
  title.colvar <- estdat2$title.colvar
  cnames <- estdat2$cnames
  title.rowgrp <- estdat2$title.rowgrp
  psetab <- estdat2$psetab
  psetab.title <- estdat2$psetab.title

  write2xlsx(esttab=esttab, psetab=psetab, tabtitle=paste0("Table ", tabnm, ". ", tabtitle),  
		title.colvar=NULL, outfolder=outfolder, allin1=allin1, title.rowvar=title.rowvar,
 		title.rowgrp=title.rowgrp, rowgrp=rowgrp, rowgrp.subtot=rowgrp.subtot, 
		addSEcol=addSEcol, fill=fill, colgrp=colgrp, cnames=cnames, coltotal=TRUE, 
		coltottxt=coltottxt, cellwidth=cellwidth, wbnm=wbnm, sheetnm=tabnm,
		footnote1=footnote1, footnote2=footnote2, footnote3=footnote3, footnote4=footnote4)


  #########################################################################################
  ## B44 - Area of timberland by inventory unit, county, and stocking class 
  #########################################################################################
  tabnm <- "B44"
print(paste("Table", tabnm))
  esttype <- "AREA"
  rowvar <- "COUNTYNM"
  row.orderby="COUNTYCD"
  rowgrp <- TRUE
  rowgrpnm="UNITNM"
  rowgrpord="UNITCD"
  colvar <- "GSSTKCD"
  colgrp <- FALSE
  landarea <- "TIMBERLAND"

  col.FIAname=TRUE
  col.add0=TRUE
  returntitle=TRUE

  coltottxt="All size classes"
  cellwidth=13
  rowgrp.subtot=TRUE

  estdat2 <- tabgrp(GBpopdat=popdat, esttype="AREA", sumunits=TRUE, landarea=landarea, 
	rowvar=rowvar, colvar=colvar, rowgrp=rowgrp, colgrp=colgrp, colgrpcd=colgrpcd,
	row.orderby=row.orderby, rowgrpnm=rowgrpnm, rowgrpord=rowgrpord, 
	col.FIAname=col.FIAname, rowgrp.subtot=rowgrp.subtot, allin1=allin1, divideby=divideby) 
  esttab <- estdat2$esttab
  tabtitle <- estdat2$esttab.title
  title.rowvar <- estdat2$title.rowvar
  title.colvar <- estdat2$title.colvar
  cnames <- estdat2$cnames
  title.rowgrp <- estdat2$title.rowgrp
  psetab <- estdat2$psetab
  psetab.title <- estdat2$psetab.title

  write2xlsx(esttab=esttab, psetab=psetab, tabtitle=paste0("Table ", tabnm, ". ", tabtitle),  
		title.colvar=NULL, outfolder=outfolder, allin1=allin1, title.rowvar=title.rowvar,
 		title.rowgrp=title.rowgrp, rowgrp=rowgrp, rowgrp.subtot=rowgrp.subtot, 
		addSEcol=addSEcol, fill=fill, colgrp=colgrp, cnames=cnames, coltotal=TRUE, 
		coltottxt=coltottxt, cellwidth=cellwidth, wbnm=wbnm, sheetnm=tabnm, 
		footnote1=footnote1, footnote2=footnote2, footnote3=footnote3, footnote4=footnote4)



  #########################################################################################
  ## B45 - Net volume of growing-stock trees on timberland by inventory unit, county,
  ##			and major species group
  #########################################################################################
  tabnm <- "B45"
print(paste("Table", tabnm))
  esttype <- "TREE"
  rowvar <- "COUNTYNM"
  row.orderby="COUNTYCD"
  rowgrp <- TRUE
  rowgrpnm="UNITNM"
  rowgrpord="UNITCD"
  colvar <- "SPGRPGRPNM2"
  col.orderby <- "SPGRPGRPCD2"
  colgrp <- FALSE
  landarea <- "TIMBERLAND"
  title.colvar <- "Major species group"

  estvar <- "VOLCFNET"
  estvar.filter="TREECLCD == 2 & DIA >= 5.0"
  estvar2 <- "VOLBFNET"
  estvar.filter2 <- "TREECLCD == 2 & STATUSCD == 1"

  col.add0=TRUE
  returntitle=TRUE

  coltottxt="All size classes"
  cellwidth=13
  rowgrp.subtot=TRUE

  estdat1 <- tabgrp(GBpopdat=popdat, esttype="TREE", sumunits=TRUE, landarea=landarea, 
	rowvar=rowvar, colvar=colvar, rowgrp=rowgrp, colgrp=colgrp,
	row.orderby=row.orderby, rowgrpnm=rowgrpnm, rowgrpord=rowgrpord, 
	col.orderby=col.orderby, rowgrp.subtot=rowgrp.subtot, 
	allin1=allin1, divideby=divideby, estvar=estvar, estvar.filter=estvar.filter) 
  esttab <- estdat1$esttab
  tabtitle <- estdat1$esttab.title
  title.rowvar <- estdat1$title.rowvar
  title.colvar <- estdat1$title.colvar
  cnames <- estdat1$cnames
  title.rowgrp <- estdat1$title.rowgrp
  psetab <- estdat1$psetab
  psetab.title <- estdat1$psetab.title


  estdat2 <- tabgrp(GBpopdat=popdat, esttype="TREE", sumunits=TRUE, landarea=landarea, 
	rowvar=rowvar, colvar=colvar, rowgrp=rowgrp, colgrp=colgrp,
	row.orderby=row.orderby, rowgrpnm=rowgrpnm, rowgrpord=rowgrpord, 
	col.orderby=col.orderby, rowgrp.subtot=rowgrp.subtot, 
	allin1=allin1, divideby=divideby, estvar=estvar2, estvar.filter=estvar.filter2) 
  esttab2 <- estdat2$esttab
  tabtitle2 <- estdat2$esttab.title
  title.rowvar2 <- estdat2$title.rowvar
  title.colvar2 <- estdat2$title.colvar
  cnames2 <- estdat2$cnames
  title.rowgrp2 <- estdat2$title.rowgrp
  psetab2 <- estdat2$psetab
  psetab.title2 <- estdat2$psetab.title

  write2xlsx(esttab=esttab, psetab=psetab, tabtitle=paste0("Table ", tabnm, ". ", tabtitle),  
		title.colvar=NULL, outfolder=outfolder, allin1=allin1, title.rowvar=title.rowvar,
 		title.rowgrp=title.rowgrp, rowgrp=rowgrp, rowgrp.subtot=rowgrp.subtot, 
		addSEcol=addSEcol, fill=fill, colgrp=colgrp, cnames=cnames, coltotal=TRUE, 
		coltottxt=coltottxt, cellwidth=cellwidth, wbnm=wbnm, sheetnm=tabnm,
		footnote1=footnote1, footnote2=footnote2, footnote3=footnote3, footnote4=footnote4)

  #if (savedata) {
  #  save(datPlots, file=paste0(outfolder, "/datPlots.rda"))
  #  save(datStrata, file=paste0(outfolder, "/datStrata.rda"))
  #  save(ref_countycd, file=paste0(outfolder, "/ref_countycd.rda"))
  #  save(ref_unitcd, file=paste0(outfolder, "/ref_unitcd.rda"))
  #}
}

