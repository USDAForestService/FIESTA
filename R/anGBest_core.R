anGBest_core <- function(state, datsource="ORACLE", FS_FIADB=FALSE, evalCur=TRUE, 
	evalEndyr=NULL, evalType=c("ALL", "AREAVOL", "GRM"), fill=TRUE, 
	addSEcol=FALSE, allin1=FALSE, outfolder=NULL, datPlots=NULL, datStrata=NULL, 
	ref_countycd=NULL, ref_unitcd=NULL, returndata=FALSE, savedata=FALSE, ...) {

  ## Set global variables
  SPGRPCD=footnote3=footnote4 <- NULL

  ## This works 
  if (Sys.getenv("JAVA_HOME")!="") 
    Sys.setenv(JAVA_HOME="") 
  #library(rJava)
  #library(xlsx)
  gui <- FALSE
  cellwidth <- ifelse(allin1, 12, 10)


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
  st <- FIESTA::pcheck.states(state, statereturn = "ABBR", stopifnull=TRUE)

  if (is.null(datPlots)) {

    ## Get data from FIA database
    ####################################
    datPlots <- DBgetPlots(states=state, datsource=datsource, FS_FIADB=FS_FIADB, 
		evalCur=evalCur, evalEndyr=evalEndyr, evalType=evalType, istree=TRUE, 
		returnPOP=TRUE)
    if (savedata) 
      save(datPlots, file=paste0(outfolder, "/", st, "_datPlots_", 
					format(Sys.time(), "%Y%m%d"), ".rda"))
  }
  plt <- datPlots$plt
  POP_PLOT_STRATUM_ASSGN <- datPlots$POP_PLOT_STRATUM_ASSGN
  evalid <- datPlots$evalid
  cond <- datPlots$cond
  tree <- datPlots$tree 
  
  ## Get strata information from FIA database
  if (is.null(datStrata)) {
    datStrata <- DBgetStrata(datsource=datsource, dat=plt, evalid=evalid,
		POP_PLOT_STRATUM_ASSGN=POP_PLOT_STRATUM_ASSGN)
    if (savedata) 
      save(datStrata, file=paste0(outfolder, "/", st, "_datStrata_", 
		format(Sys.time(), "%Y%m%d"), ".rda"))
  }

  pltstrat <- datStrata$pltstrat
  unitarea <- datStrata$unitarea
  stratalut <- datStrata$strlut
  puniqueid <- datStrata$uniqueid

  if (lapply(evalid, length) > 1) {
    pltstrat00 <- pltstrat[endsWith(as.character(pltstrat[["EVALID"]]), "00"),]
    pltstrat03 <- pltstrat[endsWith(as.character(pltstrat[["EVALID"]]), "03"),]
    pltstrat <- pltstrat[endsWith(as.character(pltstrat[["EVALID"]]), "01"),]

    cond00 <- cond[cond$PLT_CN %in% pltstrat00[[puniqueid]],]
    cond03 <- cond[cond$PLT_CN %in% pltstrat03[[puniqueid]],]
    cond <- cond[cond$PLT_CN %in% pltstrat[[puniqueid]],]
    tree03 <- tree[tree$PLT_CN %in% pltstrat03[[puniqueid]],]
    tree <- tree[tree$PLT_CN %in% pltstrat[[puniqueid]],]

    unitarea00 <- unitarea[endsWith(as.character(unitarea[["EVALID"]]), "00"),]
    unitarea03 <- unitarea[endsWith(as.character(unitarea[["EVALID"]]), "03"),]
    unitarea <- unitarea[endsWith(as.character(unitarea[["EVALID"]]), "01"),]

    stratalut00 <- stratalut[endsWith(as.character(stratalut[["EVALID"]]), "00"),]
    stratalut03 <- stratalut[endsWith(as.character(stratalut[["EVALID"]]), "03"),]
    stratalut <- stratalut[endsWith(as.character(stratalut[["EVALID"]]), "01"),]

  } else {
    pltstrat00 <- pltstrat
    cond00 <- cond
    pltstrat03 <- pltstrat
    cond03 <- cond

    unitarea00 <- unitarea
    stratalut00 <- stratalut
    unitarea03 <- unitarea
    stratalut03 <- stratalut
  }

  stcd <- unique(plt$STATECD)
  if (length(state) > 1) stop("only 1 state allowed")
  st <- FIESTA::pcheck.states(stcd, statereturn="ABBR")

  if (is.null(ref_countycd) || is.null(ref_unitcd)) {

    ## Get county and survey unit names
    #############################################################
    whereqry <- paste("where statecd ==", stcd)
    if (datsource == "CSV") {
      coqry <- paste("select countycd, countynm from COUNTY", whereqry)
      unitqry <- paste("select value, meaning from REF_UNIT", whereqry)
      ref_countycd <- FIESTA::DBqryCSV(coqry, states=state, sqltables="COUNTY")
      ref_unitcd <- FIESTA::DBqryCSV(unitqry, sqltables="REF_UNIT")
    } else {
      whereqry <- paste("where statecd =", stcd)
      schema. <- ifelse (FS_FIADB, "FS_FIADB.", "FS_NIMS_FIADB_RMRS.")
      #schema. <- "FS_FIADB."
      coqry <- paste0("select countycd, countynm from ", schema., "COUNTY ", whereqry)
      unitqry <- paste0("select value, meaning from ", schema., "REF_UNIT ", whereqry)
      ref_countycd <- FIESTA::DBqryORACLE(coqry)
      ref_unitcd <- FIESTA::DBqryORACLE(unitqry)
    } 
    if (savedata) { 
      save(ref_countycd, file=paste0(outfolder, "/ref_countycd_", format(Sys.time(), "%Y%m%d"), ".rda"))
      save(ref_unitcd, file=paste0(outfolder, "/ref_unitcd_", format(Sys.time(), "%Y%m%d"), ".rda"))
    }
  }

  pltcounty <- merge(pltstrat[, c(puniqueid, "UNITCD", "COUNTYCD")], ref_countycd, by="COUNTYCD")
  pltcounty <- merge(pltcounty, ref_unitcd, by.x="UNITCD", by.y="VALUE")
  names(pltcounty)[names(pltcounty) == "MEANING"] <- "UNITNM"
  cond <- merge(cond, pltcounty, by.x="PLT_CN", by.y=puniqueid)

  ## Set workbook name
  outfn <- paste0(state, "_core_tables")
  if (fill) outfn <- paste(outfn, "fill", sep="_")
  if (!fill) outfn <- paste(outfn, "nofill", sep="_")
  if (allin1) outfn <- paste(outfn, "allin1", sep="_")
  if (addSEcol) outfn <- paste(outfn, "SEcol", sep="_")

  ## Check Excel workbook
  wbnm <- FIESTA::pcheck.xlsx(wbnm=NULL, savewb=TRUE, outfn=outfn, outfolder=outfolder)

  ## Define footnotes
  footnote1 <- "Numbers in rows and columns may not sum to totals due to rounding."
  footnote2 <- "A dash(--) indicates no sample for the cell; 0 indicates a value of greater than 0 but less than 0.5."
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
    #DIALUT$DIACL2INCD <- seq(1:nrow(DIALUT))
    DIALUT2 <- DIALUT[DIALUT$MIN >= 5,]

    ## Append diameter classes to tree table
    datlut <- FIESTA::datLUTclass(x=tree, xvar="DIA", LUT=DIALUT, LUTclassnm="DIACL2IN")
    tree <- datlut$xLUT
  }

  ## Stand age classes
  #############################################################
  if (!"STDAGECL" %in% names(cond)) {
    STDAGELUT <- data.frame(
	#STDAGECLCD=c(1,2,3,4,5,6,7,8,9,10,11,12),
	STDAGECL=c("Nonstocked","1-20","21-40","41-60","61-80","81-100","101-120",
			"121-140","141-160","161-180","181-200", "201+"),
	MIN=c(0,1,21,41,61,81,101,121,141,161,181,201),
	MAX=c(0,20,40,60,80,100,120,140,160,180,200,1000), stringsAsFactors=FALSE)

    ## Append stand age classes to condition table
    datlut <- FIESTA::datLUTclass(x=cond, xvar="STDAGE", LUT=STDAGELUT, LUTclassnm="STDAGECL")
    cond <- datlut$xLUT
  }
 
  ## Add disturbance groups to cond
  #############################################################
  datlut <- datLUTnm(x=cond, xvar="DSTRBCD1", FIAname=TRUE, group=TRUE)
  cond <- datlut$xLUT

  ## Change TIMBERCD to include SITECLCD in 1:6
  #############################################################
  ## Note: 1:Nonreserved; 2:Reserved
  cond$TIMBERCD.PROD <- 2
  cond[cond$SITECLCD %in% 1:6, "TIMBERCD.PROD"] <- 1
  cond$TIMBERCD <- 2
  cond[cond$SITECLCD %in% 1:6 & cond$RESERVCD == 0, "TIMBERCD"] <- 1

  cond00$TIMBERCD.PROD <- 2
  cond00[cond00$SITECLCD %in% 1:6, "TIMBERCD.PROD"] <- 1
  cond00$TIMBERCD <- 2
  cond00[cond00$SITECLCD %in% 1:6 & cond00$RESERVCD == 0, "TIMBERCD"] <- 1


  ## Add Species grouping
  #############################################################
  tree2 <- FIESTA::datLUTnm(x=tree, xvar="SPGRPCD", FIAname=TRUE, group=TRUE)$xLUT
  pines <- c(1:5,11,14,15,21)
  othersoft <- c(1:24)[!c(1:24) %in% pines]
  softhard <- c(25:47)
  hardhard <- c(25:54)[!c(25:54) %in% softhard]

  tree2[SPGRPCD %in% pines, ':=' (SPGRPGRPCD=1, SPGRPGRPNM="Pine")]
  tree2[SPGRPCD %in% othersoft, ':=' (SPGRPGRPCD=2, SPGRPGRPNM="Other softwoods")]
  tree2[SPGRPCD %in% softhard, ':=' (SPGRPGRPCD=3, SPGRPGRPNM="Soft hardwoods")]
  tree2[SPGRPCD %in% hardhard, ':=' (SPGRPGRPCD=4, SPGRPGRPNM="Hard hardwoods")]


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
  B30 <- "Table B30. Aboveground dry weight of live trees on forest land by species group and diameter class"
  B31 <- "Table B31. Aboveground dry weight of standing-dead trees on forest land by species group and diameter class"
  B33 <- "Table B33. Aboveground carbon in live trees on forest land by ownership group, ownership class, reserved status, and forest land status"
  B34 <- "Table B34. Aboveground carbon in standing-dead trees on forest land by ownership group, ownership class, reserved status, and forest land status"
  B35 <- "Table B35. Average annual net growth of live trees by ownership group, ownership class, reserved status, and forest land status"
  B36 <- "Table B36. Average annual net growth of live trees on forest land by forest type group and stand-size class"
  B37 <- "Table B37. Average annual net growth of live trees on forest land by species group and stand-size class"
  B38 <- "Table B38. Average annual net growth of growing-stock trees on timberland by species group and stand-size class"
  B39 <- "Table B39. Average annual mortality of trees by owner class, reserved status, and forest land status"
  B40 <- "Table B40. Average annual mortality on forest land by forest type group and stand-size class"
  B41 <- "Table B41. Average annual mortality of trees on forest land by species group and stand-size class"
  B42 <- "Table B42. Average annual mortality of growing-stock trees on timberland by species group and stand-size class"
  B43 <- "Table B43. Area of forest land by inventory unit, county and forest land status"
  B44 <- "Table B44. Area of forest land by county, forest type group, ownership group, and forest land status"
  B45 <- "Table B45. Area of forest land by county, forest type group and stand-size class"
  B46 <- "Table B46. Area of forest land by county, forest type group and stocking class"
  B47 <- "Table B47. Net volume of growing-stock trees on timberland by inventory unit, county, and major species group"

  toc <- data.frame(rbind(B1, B2, B3, B4, B5, B6, B7, B8, B9, B11, B12, B13, B14, B15, 
		B16, B17, B19, B20, B21, B22, B23, B24, B25, B26, B27, B28, B30, B31, 
		B33, B34, B35, B36, B37, B38, B39, B40, B41, B42, B43, B44, B45, B46, B47), stringsAsFactors=FALSE)


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
  adjsamp <- TRUE

  rescds <- sort(unique(ref_reservcd[["VALUE"]]))
  rescds <- rescds[rescds >= 0]
  for (res in rescds) {
    resnm <- ref_reservcd[ref_reservcd[["VALUE"]] == res, "MEANING"] 
    print(resnm)

    cond.filter <- paste("RESERVCD ==", res)
    est <- modGBarea(cond=cond00, pltstrat=pltstrat00, sumunits=TRUE, strata=TRUE, landarea="FOREST",
		cond.filter=cond.filter, unitvar="ESTN_UNIT", unitarea=unitarea00, stratalut=stratalut00, 
		rowvar="TIMBERCD.PROD", row.FIAname=TRUE, nonsamp.filter="NONE")$est
    est <- data.frame(paste(resnm, "forest land"), est, stringsAsFactors=FALSE)
    names(est) <- c("landstatus1", "landstatus2", "Estimate", "Percent Sampling Error")
    if (res == 0) est[est$landstatus2 == "Productive", "landstatus2"] <- "Timberland"
    b1.1 <- rbind(b1.1, est)
  }
  cond2.filter <- "COND_STATUS_CD == 1"
  est2 <- modGBarea(cond=cond00, pltstrat=pltstrat00, sumunits=TRUE, strata=TRUE, cond.filter=cond2.filter,
		unitvar="ESTN_UNIT", unitarea=unitarea00, stratalut=stratalut00, nonsamp.filter="NONE")$est
  est2 <- data.frame("Total", est2, stringsAsFactors=FALSE)
  names(est2) <- c("landstatus1", "landstatus2", "Estimate", "Percent Sampling Error")
  b1.1 <- rbind(b1.1, est2)

  b1.1 <- data.frame(rgroup="Accessible forest land", b1.1, stringsAsFactors=FALSE)
  names(b1.1) <- b1.names

  ## Nonforest and other land
  ################################################################
  cond.filter <- "COND_STATUS_CD == 2"
  est <- modGBarea(cond=cond00, pltstrat=pltstrat00, sumunits=TRUE, strata=TRUE, landarea="ALL",
		cond.filter=cond.filter, unitvar="ESTN_UNIT", unitarea=unitarea00, stratalut=stratalut00, 
		nonsamp.filter="NONE")$est
  b1.2 <- data.frame(landstatus1="Nonforest and other land", "Nonforest land", "", est[, -1],
		stringsAsFactors=FALSE)
  names(b1.2) <- b1.names

  cond.filter <- "COND_STATUS_CD %in% c(3,4)"
  est <- modGBarea(cond=cond00, pltstrat=pltstrat00, sumunits=TRUE, strata=TRUE, cond.filter=cond.filter,
		unitvar="ESTN_UNIT", unitarea=unitarea00, stratalut=stratalut00, rowvar="COND_STATUS_CD",
		row.FIAname=TRUE, nonsamp.filter="NONE")$est
  b1.3 <- data.frame(landstatus1="Nonforest and other land", "Water", est, stringsAsFactors=FALSE)
  names(b1.3) <- b1.names

  cond.filter <- "COND_STATUS_CD %in% c(2,3,4)"
  est <- modGBarea(cond=cond00, pltstrat=pltstrat00, sumunits=TRUE, strata=TRUE, cond.filter=cond.filter,
		unitvar="ESTN_UNIT", unitarea=unitarea00, stratalut=stratalut00, nonsamp.filter="NONE")$est
  b1.4 <- data.frame(landstatus1="Nonforest and other land", "Total", "", est[, -1],
		stringsAsFactors=FALSE)
  names(b1.4) <- b1.names

  ## Nonsampled land
  ################################################################
  cond.filter <- "COND_STATUS_CD == 5"
  est <- modGBarea(cond=cond00, pltstrat=pltstrat00, sumunits=TRUE, strata=TRUE, 
		cond.filter=cond.filter, unitvar="ESTN_UNIT", unitarea=unitarea00, stratalut=stratalut00, 
		rowvar="COND_NONSAMPLE_REASN_CD", row.FIAname=TRUE, nonsamp.filter="NONE")$est
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
  divideby="thousand"
  returntitle=TRUE
  rowgrp.subtot=FALSE
  coltottxt="All forest land"

#  footnote <- "Other forest land is classified as not capable of growing 20 cubic feet/acre/year."
#  if (is.null(footnote3)) {
#    footnote3 <- footnote
#  } else {
#    footnote4 <- footnote
#  }

  estdat2 <- tabgrp(esttype="AREA", cond=cond, pltstrat=pltstrat, strata=TRUE,
	rowvar=rowvar, colvar=colvar, rowgrp=rowgrp, colgrp=colgrp, colgrpcd=colgrpcd,
	row.FIAname=row.FIAname, sumunits=TRUE, landarea=landarea, unitvar="ESTN_UNIT", 
	unitarea=unitarea, stratalut=stratalut, rowgrp.subtot=rowgrp.subtot, 
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
		footnote2=footnote2, footnote3=footnote3, cellwidth=cellwidth)


  #########################################################################################
  ## B3 - Area of forest land by forest type group and productivity class
  #########################################################################################
  tabnm <- "B3"
  esttype <- "AREA"
  rowvar <- "FORTYPGRPCD"
  colvar <- "SITECLCD"
  landarea <- "FOREST"

  row.FIAname=TRUE
  col.FIAname=TRUE
  col.add0=TRUE
  divideby="thousand"
  returntitle=TRUE
  coltottxt <- "Total all classes"

  estdat <- modGBarea(cond=cond, pltstrat=pltstrat, sumunits=TRUE, landarea=landarea,
		unitvar="ESTN_UNIT", unitarea=unitarea, stratalut=stratalut, rowvar=rowvar, 
		colvar=colvar, row.FIAname=row.FIAname, col.FIAname=col.FIAname, col.add0=col.add0, 
		returntitle=TRUE, divideby=divideby, allin1=allin1, estnull="--")
  esttab <- estdat$est
  psetab <- estdat$pse
  tabtitle <- estdat$titlelst$title.est
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
  divideby="thousand"
  returntitle=TRUE
  rowgrp.subtot=FALSE
  coltottxt="All forest land"

  footnote <- "Other forest land is classified as not capable of growing 20 cubic feet/acre/year."
  if (is.null(footnote3)) {
    footnote3 <- footnote
  } else {
    footnote4 <- footnote
  }

  estdat2 <- tabgrp(esttype="AREA", cond=cond, pltstrat=pltstrat, strata=TRUE,
	rowvar=rowvar, colvar=colvar, rowgrp=FALSE, colgrp=TRUE, colgrpcd=colgrpcd,
	row.FIAname=row.FIAname, col.FIAname=col.FIAname, sumunits=TRUE, landarea=landarea,
 	unitvar="ESTN_UNIT", unitarea=unitarea, stratalut=stratalut, 
	rowgrp.subtot=rowgrp.subtot, allin1=allin1, estnull="--", divideby=divideby) 
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
		cnames=cnames, coltotal=TRUE, coltottxt=coltottxt, cellwidth=cellwidth,
		wbnm=wbnm, sheetnm=tabnm, footnote1=footnote1, footnote2=footnote2,
		footnote3=footnote3)


  #########################################################################################
  ## B5 - Area of forest land by forest type group and stand-size class
  #########################################################################################
  tabnm <- "B5"
  esttype <- "AREA"
  rowvar <- "FORTYPGRPCD"
  colvar <- "STDSZCD"
  landarea <- "FOREST"

  row.FIAname=TRUE
  col.FIAname=TRUE
  col.add0=TRUE
  divideby="thousand"
  returntitle=TRUE
  coltottxt="All size classes"

  estdat <- modGBarea(cond=cond, pltstrat=pltstrat, sumunits=TRUE, landarea=landarea,
		strata=TRUE, unitvar="ESTN_UNIT", unitarea=unitarea, stratalut=stratalut, 
		rowvar=rowvar, colvar=colvar, row.FIAname=row.FIAname, col.FIAname=col.FIAname, 
		col.add0=col.add0, returntitle=TRUE, allin1=allin1, estnull="--", divideby=divideby)
  esttab <- estdat$est
  psetab <- estdat$pse
  tabtitle <- estdat$titlelst$title.est
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
  esttype <- "AREA"
  rowvar <- "FORTYPGRPCD"
  colvar <- "STDAGECL"
  title.colvar <- "Stand-age class (years)" 
  landarea <- "FOREST"

  row.FIAname=TRUE
  col.add0=TRUE
  divideby="thousand"
  returntitle=TRUE
  coltottxt="All classes"

  estdat <- modGBarea(cond=cond, pltstrat=pltstrat, sumunits=TRUE, landarea=landarea,
		strata=TRUE, unitvar="ESTN_UNIT", unitarea=unitarea, stratalut=stratalut, 
		rowvar=rowvar, colvar=colvar, row.FIAname=row.FIAname, col.add0=col.add0, 
		returntitle=TRUE, estnull="--", divideby=divideby, allin1=allin1,  
		title.colvar=title.colvar)
  esttab <- estdat$est
  psetab <- estdat$pse
  tabtitle <- estdat$titlelst$title.est
  title.colvar <- estdat$titlelst$title.colvar
  title.rowvar <- estdat$titlelst$title.rowvar

  write2xlsx(esttab=esttab, psetab=psetab, tabtitle=paste0("Table ", tabnm, ". ", tabtitle),  
		title.colvar=title.colvar, outfolder=outfolder, title.rowvar=title.rowvar, 
		fill=fill, allin1=allin1, addSEcol=addSEcol, coltottxt=coltottxt, 
		cellwidth=cellwidth, wbnm=wbnm, sheetnm=tabnm, footnote1=footnote1, 
		footnote2=footnote2, footnote3=footnote3)


  #########################################################################################
  ## B7 - Area of forest land by forest type group and stand origin
  #########################################################################################
  tabnm <- "B7"
  esttype <- "AREA"
  rowvar <- "FORTYPGRPCD"
  colvar <- "STDORGCD"
  landarea <- "FOREST"

  row.FIAname=TRUE
  col.FIAname=TRUE
  col.add0=TRUE
  divideby="thousand"
  returntitle=TRUE

  coltottxt="All forest land"
  cellwidth=15

  estdat <- modGBarea(cond=cond, pltstrat=pltstrat, sumunits=TRUE, landarea=landarea,
		strata=TRUE, unitvar="ESTN_UNIT", unitarea=unitarea, stratalut=stratalut, 
		rowvar=rowvar, colvar=colvar, row.FIAname=row.FIAname, col.FIAname=col.FIAname, 
		col.add0=col.add0, returntitle=TRUE, allin1=allin1, estnull="--", divideby=divideby)
  esttab <- estdat$est
  psetab <- estdat$pse
  tabtitle <- estdat$titlelst$title.est
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
  esttype <- "AREA"
  rowvar <- "FORTYPGRPCD"
  colvar <- "DSTRBGRPNM"
  col.orderby <- "DSTRBGRPCD"
  title.colvar <- "Disturbance class"
  landarea <- "FOREST"

  row.FIAname=TRUE
  col.add0=TRUE
  divideby="thousand"
  returntitle=TRUE
  coltottxt="All forest land"

  estdat <- modGBarea(cond=cond, pltstrat=pltstrat, sumunits=TRUE, landarea=landarea,
		strata=TRUE, unitvar="ESTN_UNIT", unitarea=unitarea, stratalut=stratalut, 
		rowvar=rowvar, colvar=colvar, row.FIAname=row.FIAname, col.orderby=col.orderby, 
		col.add0=col.add0, returntitle=TRUE, divideby=divideby, allin1=allin1, 
		estnull="--", title.colvar=title.colvar)
  esttab <- estdat$est
  psetab <- estdat$pse
  tabtitle <- estdat$titlelst$title.est
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
  esttype <- "AREA"
  rowvar <- "FORTYPGRPCD"
  colvar <- "STDSZCD"
  landarea <- "TIMBERLAND"

  row.FIAname=TRUE
  col.FIAname=TRUE
  col.add0=TRUE
  divideby="thousand"
  returntitle=TRUE
  coltottxt="All size classes"

  estdat <- modGBarea(cond=cond, pltstrat=pltstrat, sumunits=TRUE, landarea=landarea,
		strata=TRUE, unitvar="ESTN_UNIT", unitarea=unitarea, stratalut=stratalut, 
		rowvar=rowvar, colvar=colvar, row.FIAname=row.FIAname, col.FIAname=col.FIAname, 
		col.add0=col.add0, returntitle=TRUE, allin1=allin1, estnull="--", divideby=divideby)
  esttab <- estdat$est
  psetab <- estdat$pse
  tabtitle <- estdat$titlelst$title.est
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
  divideby="thousand"
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
  cond.filter=NULL

  estdat2 <- tabgrp(esttype="TREE", cond=cond, tree=tree, pltstrat=pltstrat, sumunits=TRUE,  
	landarea=landarea, strata=TRUE, unitvar="ESTN_UNIT", unitarea=unitarea, 
	stratalut=stratalut, estvar=estvar, estvar.filter=estvar.filter, rowvar=rowvar, 
	colvar=colvar, rowgrp=rowgrp, row.FIAname=TRUE, collut=collut, rowgrp.subtot=rowgrp.subtot, 
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
  esttype <- "TREE"
  rowvar <- "SPGRPCD"
  rowgrp <- TRUE
  colvar <- "DIACL2IN"
  title.colvar <- "Diameter class (inches)" 
  landarea <- "FOREST"

  estvar <- "TPA_UNADJ"
  estvar.filter="STATUSCD == 2 & DIA >= 5"

  row.FIAname=TRUE
  col.add0=TRUE
  divideby="thousand"
  returntitle=TRUE
  rowgrp.subtot=TRUE

  coltottxt="All classes"
  collut=DIALUT[DIALUT$MIN >= 5,]

  title.rowgrp=NULL
  title.rowvar=NULL
  rowgrpnm=NULL
  rowgrpord=NULL
  colgrpcd=NULL
  cond.filter=NULL

  estdat2 <- tabgrp(esttype="TREE", cond=cond, tree=tree, pltstrat=pltstrat, sumunits=TRUE,  
	landarea=landarea, strata=TRUE, unitvar="ESTN_UNIT", unitarea=unitarea, stratalut=stratalut, 
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
  divideby="thousand"
  returntitle=TRUE
  rowgrp.subtot=TRUE

  coltottxt="All classes"
  collut=DIALUT2

  estdat2 <- tabgrp(esttype="TREE", cond=cond, tree=tree, pltstrat=pltstrat, sumunits=TRUE,  
	landarea=landarea, strata=TRUE, unitvar="ESTN_UNIT", unitarea=unitarea, 
	stratalut=stratalut, estvar=estvar, estvar.filter=estvar.filter, rowvar=rowvar, 
	colvar=colvar, rowgrp=rowgrp, row.FIAname=row.FIAname, collut=collut, 
	rowgrp.subtot=rowgrp.subtot, allin1=allin1, divideby=divideby, estnull="--", 
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
  divideby="million"
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

  estdat2 <- tabgrp(esttype="TREE", cond=cond, tree=tree, pltstrat=pltstrat, sumunits=TRUE,
	landarea=landarea, strata=TRUE, unitvar="ESTN_UNIT", unitarea=unitarea, 
	stratalut=stratalut, estvar=estvar, estvar.filter=estvar.filter, rowvar=rowvar, 
	colvar=colvar, rowgrp=rowgrp, colgrp=colgrp, colgrpcd=colgrpcd, row.FIAname=row.FIAname, 
	col.FIAname=col.FIAname, rowgrp.subtot=rowgrp.subtot, allin1=allin1, estnull="--", 
	divideby=divideby, title.colvar=title.colvar) 
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
		footnote2=footnote2, footnote3=footnote3)


  #########################################################################################
  ## B14 - Net volume of standing-dead trees of forest land by owner group, owner class and 
  ##		forest land status
  #########################################################################################
  tabnm <- "B14"
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
  divideby="million"
  returntitle=TRUE
  rowgrp.subtot=FALSE

  estvar="VOLCFNET"
  estvar.filter="STATUSCD == 2"
  coltottxt="All forest land"

#  footnote <- "Other forest land is classified as not capable of growing 20 cubic feet/acre/year."
#  if (is.null(footnote3)) {
#    footnote3 <- footnote
#  } else {
#    footnote4 <- footnote
#  }

  estdat2 <- tabgrp(esttype="TREE", cond=cond, tree=tree, pltstrat=pltstrat, sumunits=TRUE,
	landarea=landarea, strata=TRUE, unitvar="ESTN_UNIT", unitarea=unitarea, 
	stratalut=stratalut, estvar=estvar, estvar.filter=estvar.filter, rowvar=rowvar, 
	colvar=colvar, rowgrp=rowgrp, colgrp=colgrp, colgrpcd=colgrpcd, row.FIAname=row.FIAname, 
	col.FIAname=col.FIAname, rowgrp.subtot=rowgrp.subtot, allin1=allin1, estnull="--", 
	divideby=divideby, title.colvar=title.colvar) 
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
		footnote2=footnote2, footnote3=footnote3)


  #########################################################################################
  ## B15 - Net volume of live trees on forest land by forest type group and stand-size class
  #########################################################################################
  tabnm <- "B15"
  esttype <- "TREE"
  rowvar <- "FORTYPGRPCD"
  colvar <- "STDSZCD"
  landarea <- "FOREST"

  row.FIAname=TRUE
  col.FIAname=TRUE
  col.add0=TRUE
  divideby="million"
  returntitle=TRUE

  estvar="VOLCFNET"
  estvar.filter="STATUSCD == 1"
  coltottxt="All size classes"

  estdat <- modGBtree(tree=tree, cond=cond, pltstrat=pltstrat, sumunits=TRUE, landarea=landarea,
		strata=TRUE, unitvar="ESTN_UNIT", unitarea=unitarea, stratalut=stratalut, estvar=estvar,
 		estvar.filter=estvar.filter, rowvar=rowvar, colvar=colvar, row.FIAname=row.FIAname, 
		col.FIAname=col.FIAname, col.add0=col.add0, returntitle=TRUE, divideby=divideby, 
		allin1=allin1, estnull="--")
  esttab <- estdat$est
  psetab <- estdat$pse
  tabtitle <- estdat$titlelst$title.est
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
  esttype <- "TREE"
  rowvar <- "FORTYPGRPCD"
  colvar <- "STDSZCD"
  landarea <- "FOREST"

  row.FIAname=TRUE
  col.FIAname=TRUE
  col.add0=TRUE
  divideby="million"
  returntitle=TRUE

  estvar="VOLCFNET"
  estvar.filter="STATUSCD == 2"
  coltottxt="All size classes"

  estdat <- modGBtree(tree=tree, cond=cond, pltstrat=pltstrat, sumunits=TRUE, landarea=landarea,
		strata=TRUE, unitvar="ESTN_UNIT", unitarea=unitarea, stratalut=stratalut, rowvar=rowvar, 
		estvar=estvar, estvar.filter=estvar.filter, colvar=colvar, row.FIAname=row.FIAname, 
		col.FIAname=col.FIAname, col.add0=col.add0, returntitle=TRUE, divideby=divideby, 
		allin1=allin1, estnull="--")
  esttab <- estdat$est
  psetab <- estdat$pse
  tabtitle <- estdat$titlelst$title.est
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
  divideby="million"
  returntitle=TRUE
  rowgrp.subtot=TRUE
  coltottxt="All owners"

  estdat2 <- tabgrp(esttype="TREE", cond=cond, tree=tree, pltstrat=pltstrat, sumunits=TRUE,  
	landarea=landarea, strata=TRUE, unitvar="ESTN_UNIT", unitarea=unitarea, 
	stratalut=stratalut, estvar=estvar, estvar.filter=estvar.filter, rowvar=rowvar, 
	colvar=colvar, rowgrp=rowgrp, row.FIAname=row.FIAname, col.FIAname=col.FIAname, 
	rowgrp.subtot=rowgrp.subtot, allin1=allin1, estnull="--", divideby=divideby) 
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
  esttype <- "TREE"
  rowvar <- "SPGRPCD"
  rowgrp <- TRUE
  colvar <- "OWNGRPCD"
  landarea <- "FOREST"

  estvar="VOLCFNET"
  estvar.filter="STATUSCD == 2"

  row.FIAname=TRUE
  col.FIAname=TRUE
  col.add0=TRUE
  divideby="million"
  returntitle=TRUE
  rowgrp.subtot=TRUE
  coltottxt="All owners"

  estdat2 <- tabgrp(esttype="TREE", cond=cond, tree=tree, pltstrat=pltstrat, sumunits=TRUE,  
	landarea=landarea, strata=TRUE, unitvar="ESTN_UNIT", unitarea=unitarea, 
	stratalut=stratalut, estvar=estvar, estvar.filter=estvar.filter, rowvar=rowvar, 
	colvar=colvar, rowgrp=rowgrp, row.FIAname=row.FIAname, col.FIAname=col.FIAname, 
	rowgrp.subtot=rowgrp.subtot, allin1=allin1, estnull="--", divideby=divideby) 
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
  divideby="million"
  returntitle=TRUE
  rowgrp.subtot=TRUE

  coltottxt="All classes"
  collut=DIALUT2


  estdat2 <- tabgrp(esttype="TREE", cond=cond, tree=tree, pltstrat=pltstrat, sumunits=TRUE,  
	landarea=landarea, strata=TRUE, unitvar="ESTN_UNIT", unitarea=unitarea, 
	stratalut=stratalut, estvar=estvar, estvar.filter=estvar.filter, rowvar=rowvar, 
	colvar=colvar, rowgrp=rowgrp, row.FIAname=TRUE, collut=collut, rowgrp.subtot=rowgrp.subtot, 
	allin1=allin1, estnull="--", divideby=divideby, title.colvar=title.colvar) 
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
  esttype <- "TREE"
  rowvar <- "SPGRPCD"
  rowgrp <- TRUE
  colvar <- "DIACL2IN"
  col.orderby <- "DIACL2INCD"
  title.colvar <- "Diameter class (inches)" 
  landarea <- "FOREST"

  estvar <- "VOLCFNET"
  estvar.filter="STATUSCD == 2"

  row.FIAname=TRUE
  col.add0=TRUE
  divideby="million"
  returntitle=TRUE
  rowgrp.subtot=TRUE

  coltottxt="All classes"
  collut=DIALUT2


  estdat2 <- tabgrp(esttype="TREE", cond=cond, tree=tree, pltstrat=pltstrat, sumunits=TRUE,  
	landarea=landarea, strata=TRUE, unitvar="ESTN_UNIT", unitarea=unitarea, 
	stratalut=stratalut, estvar=estvar, estvar.filter=estvar.filter, rowvar=rowvar, 
	colvar=colvar, rowgrp=rowgrp, row.FIAname=TRUE, collut=collut, rowgrp.subtot=rowgrp.subtot, 
	allin1=allin1, estnull="--", divideby=divideby, title.colvar=title.colvar) 
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
  esttype <- "TREE"
  rowvar <- "FORTYPGRPCD"
  colvar <- "STDORGCD"
  landarea <- "FOREST"

  row.FIAname=TRUE
  col.FIAname=TRUE
  col.add0=TRUE
  divideby="million"
  returntitle=TRUE

  estvar="VOLCFNET"
  estvar.filter="STATUSCD == 1"

  coltottxt="All forest land"
  cellwidth=15

  estdat <- modGBtree(tree=tree, cond=cond, pltstrat=pltstrat, sumunits=TRUE, landarea=landarea,
		strata=TRUE, unitvar="ESTN_UNIT", unitarea=unitarea, stratalut=stratalut, 
		estvar=estvar, estvar.filter=estvar.filter, rowvar=rowvar, colvar=colvar, 
		row.FIAname=row.FIAname, col.FIAname=col.FIAname, col.add0=col.add0, returntitle=TRUE, 
		divideby=divideby, estnull="--", allin1=allin1)
  esttab <- estdat$est
  psetab <- estdat$pse
  tabtitle <- estdat$titlelst$title.est
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
  esttype <- "TREE"
  rowvar <- "FORTYPGRPCD"
  colvar <- "STDORGCD"
  landarea <- "FOREST"

  row.FIAname=TRUE
  col.FIAname=TRUE
  col.add0=TRUE
  divideby="million"
  returntitle=TRUE

  estvar="VOLCFNET"
  estvar.filter="STATUSCD == 2"

  coltottxt="All forest land"
  cellwidth=15

  estdat <- modGBtree(tree=tree, cond=cond, pltstrat=pltstrat, sumunits=TRUE, landarea=landarea,
		strata=TRUE, unitvar="ESTN_UNIT", unitarea=unitarea, stratalut=stratalut, 
		estvar=estvar, estvar.filter=estvar.filter, rowvar=rowvar, colvar=colvar, 
		row.FIAname=row.FIAname, col.FIAname=col.FIAname, col.add0=col.add0, returntitle=TRUE, 
		divideby=divideby, estnull="--", allin1=allin1)
  esttab <- estdat$est
  psetab <- estdat$pse
  tabtitle <- estdat$titlelst$title.est
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
  divideby="million"
  returntitle=TRUE
  rowgrp.subtot=TRUE

  coltottxt="All classes"
  cellwidth=12
  collut=DIALUT2

  estdat2 <- tabgrp(esttype="TREE", cond=cond, tree=tree, pltstrat=pltstrat, sumunits=TRUE,  
	landarea=landarea, strata=TRUE, unitvar="ESTN_UNIT", unitarea=unitarea, 
	stratalut=stratalut, estvar=estvar, estvar.filter=estvar.filter, rowvar=rowvar, 
	colvar=colvar, rowgrp=rowgrp, row.FIAname=row.FIAname, collut=collut, 
	rowgrp.subtot=rowgrp.subtot, allin1=allin1, estnull="--", divideby=divideby, 
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
  divideby="million"
  returntitle=TRUE
  rowgrp.subtot=TRUE

  coltottxt="All owners"
  cellwidth=15

  estdat2 <- tabgrp(esttype="TREE", cond=cond, tree=tree, pltstrat=pltstrat, sumunits=TRUE,  
	landarea=landarea, strata=TRUE, unitvar="ESTN_UNIT", unitarea=unitarea, 
	stratalut=stratalut, estvar=estvar, estvar.filter=estvar.filter, rowvar=rowvar, 
	colvar=colvar, rowgrp=rowgrp, row.FIAname=row.FIAname, col.FIAname=col.FIAname, 
	rowgrp.subtot=rowgrp.subtot, allin1=allin1, divideby=divideby, estnull="--") 
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
  divideby="million"
  returntitle=TRUE
  rowgrp.subtot=TRUE

  coltottxt="All classes"
  cellwidth=12
  collut=DIALUT2

  estdat2 <- tabgrp(esttype="TREE", cond=cond, tree=tree, pltstrat=pltstrat, sumunits=TRUE,  
	landarea=landarea, strata=TRUE, unitvar="ESTN_UNIT", unitarea=unitarea, 
	stratalut=stratalut, estvar=estvar, estvar.filter=estvar.filter, rowvar=rowvar, 
	colvar=colvar, rowgrp=rowgrp, row.FIAname=row.FIAname, collut=collut, 
	rowgrp.subtot=rowgrp.subtot, allin1=allin1, estnull="--", divideby=divideby, 
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
  divideby="million"
  returntitle=TRUE
  rowgrp.subtot=TRUE

  coltottxt="All owners"
  cellwidth=15

  estdat2 <- tabgrp(esttype="TREE", cond=cond, tree=tree, pltstrat=pltstrat, sumunits=TRUE,  
	landarea=landarea, strata=TRUE, unitvar="ESTN_UNIT", unitarea=unitarea, 
	stratalut=stratalut, estvar=estvar, estvar.filter=estvar.filter, rowvar=rowvar, 
	colvar=colvar, rowgrp=rowgrp, row.FIAname=row.FIAname, col.FIAname=col.FIAname, 
	rowgrp.subtot=rowgrp.subtot, allin1=allin1, divideby=divideby, estnull="--", 
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
		title.colvar=title.colvar, outfolder=outfolder, title.rowvar=title.rowvar, 
		rowgrp=rowgrp, title.rowgrp=title.rowgrp, rowgrp.subtot=rowgrp.subtot, 
		fill=fill, allin1=allin1, addSEcol=addSEcol, coltottxt=coltottxt, 
		cellwidth=cellwidth, wbnm=wbnm, sheetnm=tabnm, footnote1=footnote1, 
		footnote2=footnote2, footnote3=footnote3)


  #########################################################################################
  ## B27 - Aboveground dry weight of live trees by owner class and forest land status
  #########################################################################################
  tabnm <- "B27"
  esttype <- "TREE"
  rowvar <- "OWNCD"
  rowgrp <- TRUE
  colvar <- "TIMBERCD"
  colgrp <- TRUE
  colgrpcd <- "RESERVCD"
  landarea <- "FOREST"

  row.FIAname=TRUE
  col.FIAname=TRUE
  col.add0=TRUE
  divideby="thousand"
  returntitle=TRUE
  rowgrp.subtot=FALSE

  estvar="DRYBIO_AG"
  estvar.filter="STATUSCD == 1"

  coltottxt="All forest land"
  cellwidth=13

  footnote <- "Other forest land is classified as not capable of growing 20 cubic feet/acre/year."
  if (is.null(footnote3)) {
    footnote3 <- footnote
  } else {
    footnote4 <- footnote
  }

  estdat2 <- tabgrp(esttype="TREE", cond=cond, tree=tree, pltstrat=pltstrat, 
	estvar=estvar, estvar.filter=estvar.filter, rowvar=rowvar, colvar=colvar, 
	rowgrp=rowgrp, colgrp=colgrp, colgrpcd=colgrpcd, sumunits=TRUE, 
	row.FIAname=row.FIAname, col.FIAname=col.FIAname, landarea=landarea,
 	unitvar="ESTN_UNIT", unitarea=unitarea, stratalut=stratalut, 
	rowgrp.subtot=rowgrp.subtot, allin1=allin1, divideby=divideby) 
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
  esttype <- "TREE"
  rowvar <- "OWNCD"
  rowgrp <- TRUE
  colvar <- "TIMBERCD"
  colgrp <- TRUE
  colgrpcd <- "RESERVCD"
  landarea <- "FOREST"

  row.FIAname=TRUE
  col.FIAname=TRUE
  col.add0=TRUE
  divideby="thousand"
  returntitle=TRUE
  rowgrp.subtot=FALSE

  estvar="DRYBIO_AG"
  estvar.filter="STATUSCD == 2"

  coltottxt="All forest land"
  cellwidth=13

  footnote <- "Other forest land is classified as not capable of growing 20 cubic feet/acre/year."
  if (is.null(footnote3)) {
    footnote3 <- footnote
  } else {
    footnote4 <- footnote
  }

  estdat2 <- tabgrp(esttype="TREE", cond=cond, tree=tree, pltstrat=pltstrat, 
	estvar=estvar, estvar.filter=estvar.filter, rowvar=rowvar, colvar=colvar, 
	rowgrp=rowgrp, colgrp=colgrp, colgrpcd=colgrpcd, sumunits=TRUE, 
	row.FIAname=row.FIAname, col.FIAname=col.FIAname, landarea=landarea,
 	unitvar="ESTN_UNIT", unitarea=unitarea, stratalut=stratalut, 
	rowgrp.subtot=rowgrp.subtot, allin1=allin1, divideby=divideby) 
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
  ## B29 - Aboveground green weight of live trees by owner class and forest land status
  #########################################################################################




  #########################################################################################
  ## B30 - Aboveground dry weight of live trees on forest land by species group 
  ##			and diameter class
  #########################################################################################
  tabnm <- "B30"
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
  divideby="thousand"
  returntitle=TRUE
  rowgrp.subtot=TRUE

  coltottxt="All classes"
  cellwidth=12
  collut=DIALUT

  estdat2 <- tabgrp(esttype="TREE", cond=cond, tree=tree, pltstrat=pltstrat, sumunits=TRUE,  
	landarea=landarea, estvar=estvar, estvar.filter=estvar.filter, rowvar=rowvar, 
	colvar=colvar, rowgrp=rowgrp, row.FIAname=row.FIAname, collut=collut, unitvar="ESTN_UNIT", 
	unitarea=unitarea, stratalut=stratalut, rowgrp.subtot=rowgrp.subtot, 
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
  ## B31 - Aboveground dry weight of standing-dead trees on forest land by species group 
  ##			and diameter class
  #########################################################################################
  tabnm <- "B31"
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
  divideby="thousand"
  returntitle=TRUE
  rowgrp.subtot=TRUE

  coltottxt="All classes"
  cellwidth=12
  collut=DIALUT

  estdat2 <- tabgrp(esttype="TREE", cond=cond, tree=tree, pltstrat=pltstrat, sumunits=TRUE,  
	landarea=landarea, estvar=estvar, estvar.filter=estvar.filter, rowvar=rowvar, 
	colvar=colvar, rowgrp=rowgrp, row.FIAname=row.FIAname, collut=collut, unitvar="ESTN_UNIT", 
	unitarea=unitarea, stratalut=stratalut, rowgrp.subtot=rowgrp.subtot, 
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
  ## B32 - Aboveground green weight of standing-dead trees on forest land by species group 
  ##			and diameter class
  #########################################################################################




  #########################################################################################
  ## B33 - Aboveground carbon in live trees by owner class and forest land status
  #########################################################################################
  tabnm <- "B33"
  esttype <- "TREE"
  rowvar <- "OWNCD"
  rowgrp <- TRUE
  colvar <- "TIMBERCD"
  colgrp <- TRUE
  colgrpcd <- "RESERVCD"
  landarea <- "FOREST"

  row.FIAname=TRUE
  col.FIAname=TRUE
  col.add0=TRUE
  divideby="thousand"
  returntitle=TRUE
  rowgrp.subtot=FALSE

  estvar="CARBON_AG"
  estvar.filter="STATUSCD == 1"

  coltottxt="All forest land"
  cellwidth=13

  footnote <- "Other forest land is classified as not capable of growing 20 cubic feet/acre/year."
  if (is.null(footnote3)) {
    footnote3 <- footnote
  } else {
    footnote4 <- footnote
  }

  estdat2 <- tabgrp(esttype="TREE", cond=cond, tree=tree, pltstrat=pltstrat, 
	estvar=estvar, estvar.filter=estvar.filter, rowvar=rowvar, colvar=colvar, 
	rowgrp=rowgrp, colgrp=colgrp, colgrpcd=colgrpcd, sumunits=TRUE, 
	row.FIAname=row.FIAname, col.FIAname=col.FIAname, landarea=landarea,
 	unitvar="ESTN_UNIT", unitarea=unitarea, stratalut=stratalut, 
	rowgrp.subtot=rowgrp.subtot, allin1=allin1, divideby=divideby) 
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
  ## B34 - Aboveground carbon in standing-dead trees by owner class and forest land status
  #########################################################################################
  tabnm <- "B34"
  esttype <- "TREE"
  rowvar <- "OWNCD"
  rowgrp <- TRUE
  colvar <- "TIMBERCD"
  colgrp <- TRUE
  colgrpcd <- "RESERVCD"
  landarea <- "FOREST"

  row.FIAname=TRUE
  col.FIAname=TRUE
  col.add0=TRUE
  divideby="thousand"
  returntitle=TRUE
  rowgrp.subtot=FALSE

  estvar="CARBON_AG"
  estvar.filter="STATUSCD == 2"

  coltottxt="All forest land"
  cellwidth=13

  footnote <- "Other forest land is classified as not capable of growing 20 cubic feet/acre/year."
  if (is.null(footnote3)) {
    footnote3 <- footnote
  } else {
    footnote4 <- footnote
  }

  estdat2 <- tabgrp(esttype="TREE", cond=cond, tree=tree, pltstrat=pltstrat, 
	estvar=estvar, estvar.filter=estvar.filter, rowvar=rowvar, colvar=colvar, 
	rowgrp=rowgrp, colgrp=colgrp, colgrpcd=colgrpcd, sumunits=TRUE, 
	row.FIAname=row.FIAname, col.FIAname=col.FIAname, landarea=landarea,
 	unitvar="ESTN_UNIT", unitarea=unitarea, stratalut=stratalut, 
	rowgrp.subtot=rowgrp.subtot, allin1=allin1, divideby=divideby) 
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
  ## B35 - Avg annual net growth of live trees by owner group, owner class and 
  ##			forest land status
  #########################################################################################
  tabnm <- "B35"
  esttype <- "TREE"
  rowvar <- "OWNCD"
  rowgrp <- TRUE
  colvar <- "TIMBERCD"
  colgrp <- TRUE
  colgrpcd <- "RESERVCD"
  landarea <- "FOREST"

  row.FIAname=TRUE
  col.FIAname=TRUE
  col.add0=TRUE
  divideby="million"
  returntitle=TRUE
  rowgrp.subtot=FALSE

  estvar="FGROWCFAL"
  estvar.filter=NULL

  coltottxt="All forest land"
  cellwidth=13

  footnote <- "Other forest land is classified as not capable of growing 20 cubic feet/acre/year."
  if (is.null(footnote3)) {
    footnote3 <- footnote
  } else {
    footnote4 <- footnote
  }

  estdat2 <- tabgrp(esttype="TREE", cond=cond, tree=tree, pltstrat=pltstrat, 
	estvar=estvar, estvar.filter=estvar.filter, rowvar=rowvar, colvar=colvar, 
	rowgrp=rowgrp, colgrp=colgrp, colgrpcd=colgrpcd, sumunits=TRUE, 
	row.FIAname=row.FIAname, col.FIAname=col.FIAname, landarea=landarea,
 	unitvar="ESTN_UNIT", unitarea=unitarea, stratalut=stratalut, 
	rowgrp.subtot=rowgrp.subtot, allin1=allin1, divideby=divideby) 
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
  ## B36 - Avg annual net growth of live trees on forest land by forest type group 
  ##			and stand-size class
  #########################################################################################
  tabnm <- "B36"
  esttype <- "TREE"
  rowvar <- "FORTYPGRPCD"
  colvar <- "STDSZCD"
  landarea <- "FOREST"

  row.FIAname=TRUE
  col.FIAname=TRUE
  col.add0=TRUE
  divideby="million"
  returntitle=TRUE

  estvar="FGROWCFAL"
  estvar.filter=NULL
  coltottxt="All size classes"
  cellwidth=13

  estdat <- modGBtree(tree=tree, cond=cond, pltstrat=pltstrat, sumunits=TRUE, landarea=landarea,
		unitvar="ESTN_UNIT", unitarea=unitarea, stratalut=stratalut, estvar=estvar,
 		estvar.filter=estvar.filter, rowvar=rowvar, colvar=colvar, 
		row.FIAname=row.FIAname, col.FIAname=col.FIAname, col.add0=col.add0, 
		returntitle=TRUE, divideby=divideby, allin1=allin1)
  esttab <- estdat$est
  psetab <- estdat$pse
  tabtitle <- estdat$titlelst$title.est
  title.colvar <- estdat$titlelst$title.colvar
  title.rowvar <- estdat$titlelst$title.rowvar

  write2xlsx(esttab=esttab, psetab=psetab, tabtitle=paste0("Table ", tabnm, ". ", tabtitle),  
		title.colvar=title.colvar, outfolder=outfolder, title.rowvar=title.rowvar, 
		fill=fill, allin1=allin1, addSEcol=addSEcol, coltottxt=coltottxt, 
		cellwidth=cellwidth, wbnm=wbnm, sheetnm=tabnm, footnote1=footnote1, 
		footnote2=footnote2, footnote3=footnote3, footnote4=footnote4)


  #########################################################################################
  ## B37 - Avg annual net growth of live trees on forest land by species group 
  ##			and ownership group
  #########################################################################################
  tabnm <- "B37"
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
  divideby="million"
  returntitle=TRUE
  rowgrp.subtot=TRUE

  coltottxt="All owners"
  cellwidth=14

  estdat2 <- tabgrp(esttype="TREE", cond=cond, tree=tree, pltstrat=pltstrat, sumunits=TRUE,  
	landarea=landarea, estvar=estvar, estvar.filter=estvar.filter, rowvar=rowvar, 
	colvar=colvar, rowgrp=rowgrp, row.FIAname=row.FIAname, col.FIAname=col.FIAname, 
	unitvar="ESTN_UNIT", unitarea=unitarea, stratalut=stratalut, 
	rowgrp.subtot=rowgrp.subtot, allin1=allin1, divideby=divideby, title.colvar=title.colvar) 
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
  ## B38 - Avg annual net growth of growing-stock trees on forest land by species 
  ##			group and ownership group
  #########################################################################################
  tabnm <- "B38"
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
  divideby="million"
  returntitle=TRUE
  rowgrp.subtot=TRUE

  coltottxt="All owners"
  cellwidth=14

  estdat2 <- tabgrp(esttype="TREE", cond=cond, tree=tree, pltstrat=pltstrat, sumunits=TRUE,  
	landarea=landarea, estvar=estvar, estvar.filter=estvar.filter, rowvar=rowvar, 
	colvar=colvar, rowgrp=rowgrp, row.FIAname=row.FIAname, col.FIAname=col.FIAname, 
	unitvar="ESTN_UNIT", unitarea=unitarea, stratalut=stratalut, 
	rowgrp.subtot=rowgrp.subtot, allin1=allin1, divideby=divideby, title.colvar=title.colvar) 
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
  ## B39 - Avg annual mortality of trees by owner class and forest land status
  #########################################################################################
  tabnm <- "B39"
  esttype <- "TREE"
  rowvar <- "OWNCD"
  rowgrp <- TRUE
  colvar <- "TIMBERCD"
  colgrp <- TRUE
  colgrpcd <- "RESERVCD"
  landarea <- "FOREST"

  row.FIAname=TRUE
  col.FIAname=TRUE
  col.add0=TRUE
  divideby="million"
  returntitle=TRUE
  rowgrp.subtot=FALSE

  estvar="FMORTCFAL"
  estvar.filter <- NULL

  coltottxt="All forest land"
  cellwidth=13

  footnote <- "Other forest land is classified as not capable of growing 20 cubic feet/acre/year."
  if (is.null(footnote3)) {
    footnote3 <- footnote
  } else {
    footnote4 <- footnote
  }

  estdat2 <- tabgrp(esttype="TREE", cond=cond, tree=tree, pltstrat=pltstrat, 
	estvar=estvar, estvar.filter=estvar.filter, rowvar=rowvar, colvar=colvar, 
	rowgrp=rowgrp, colgrp=colgrp, colgrpcd=colgrpcd, sumunits=TRUE, 
	row.FIAname=row.FIAname, col.FIAname=col.FIAname, landarea=landarea,
 	unitvar="ESTN_UNIT", unitarea=unitarea, stratalut=stratalut, 
	rowgrp.subtot=rowgrp.subtot, allin1=allin1, divideby=divideby) 
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
  ## B40 - Avg annual mortality on forest land by forest type group and stand-size class
  #########################################################################################
  tabnm <- "B40"
  esttype <- "TREE"
  rowvar <- "FORTYPGRPCD"
  colvar <- "STDSZCD"
  landarea <- "FOREST"

  row.FIAname=TRUE
  col.FIAname=TRUE
  col.add0=TRUE
  divideby="million"
  returntitle=TRUE

  estvar="FMORTCFAL"
  estvar.filter <- NULL

  coltottxt="All size classes"
  cellwidth=13

  estdat <- modGBtree(tree=tree, cond=cond, pltstrat=pltstrat, sumunits=TRUE, landarea=landarea,
		unitvar="ESTN_UNIT", unitarea=unitarea, stratalut=stratalut, estvar=estvar,
 		estvar.filter=estvar.filter, rowvar=rowvar, colvar=colvar, row.FIAname=row.FIAname,
 		col.FIAname=col.FIAname, col.add0=col.add0, returntitle=TRUE, divideby=divideby, 
		allin1=allin1)
  esttab <- estdat$est
  psetab <- estdat$pse
  tabtitle <- estdat$titlelst$title.est
  title.colvar <- estdat$titlelst$title.colvar
  title.rowvar <- estdat$titlelst$title.rowvar

  write2xlsx(esttab=esttab, psetab=psetab, tabtitle=paste0("Table ", tabnm, ". ", tabtitle),  
		title.colvar=title.colvar, outfolder=outfolder, title.rowvar=title.rowvar, 
		fill=fill, allin1=allin1, addSEcol=addSEcol, coltottxt=coltottxt, 
		cellwidth=cellwidth, wbnm=wbnm, sheetnm=tabnm, footnote1=footnote1, 
		footnote2=footnote2, footnote3=footnote3, footnote4=footnote4)


  #########################################################################################
  ## B41 - Avg annual mortality of trees on forest land by species group and ownership group
  #########################################################################################
  tabnm <- "B41"
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
  divideby="million"
  returntitle=TRUE
  rowgrp.subtot=TRUE

  coltottxt="All owners"
  cellwidth=14

  estdat2 <- tabgrp(esttype="TREE", cond=cond, tree=tree, pltstrat=pltstrat, sumunits=TRUE,  
	landarea=landarea, estvar=estvar, estvar.filter=estvar.filter, rowvar=rowvar, 
	colvar=colvar, rowgrp=rowgrp, row.FIAname=row.FIAname, col.FIAname=col.FIAname, 
	unitvar="ESTN_UNIT", unitarea=unitarea, stratalut=stratalut, 
	rowgrp.subtot=rowgrp.subtot, allin1=allin1, divideby=divideby, title.colvar=title.colvar) 
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
  ## B42 - Avg annual mortality of growing-stock trees on timberland by species group 
  ##		and ownership group
  #########################################################################################
  tabnm <- "B42"
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
  divideby="million"
  returntitle=TRUE
  rowgrp.subtot=TRUE

  coltottxt="All owners"
  cellwidth=14

  estdat2 <- tabgrp(esttype="TREE", cond=cond, tree=tree, pltstrat=pltstrat, sumunits=TRUE,  
	landarea=landarea, estvar=estvar, estvar.filter=estvar.filter, rowvar=rowvar, 
	colvar=colvar, rowgrp=rowgrp, row.FIAname=row.FIAname, col.FIAname=col.FIAname, 
	unitvar="ESTN_UNIT", unitarea=unitarea, stratalut=stratalut, 
	rowgrp.subtot=rowgrp.subtot, allin1=allin1, divideby=divideby, title.colvar=title.colvar) 
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
  ## B43 - Area of forest land by inventory unit, county and forest land status
  #########################################################################################
  tabnm <- "B43"
  esttype <- "AREA"
  rowvar <- "COUNTYNM"
  row.orderby="COUNTYCD"
  rowgrp <- TRUE
  rowgrpnm <- "UNITNM"
  rowgrpord <- "UNITCD"
  colvar <- "TIMBERCD"
  colgrp <- TRUE
  colgrpcd <- "RESERVCD"
  landarea <- "FOREST"

  col.FIAname=TRUE
  col.add0=TRUE
  divideby="thousand"
  returntitle=TRUE
  rowgrp.subtot=TRUE

  coltottxt="All forest land"
  cellwidth=13

  footnote <- "Other forest land is classified as not capable of growing 20 cubic feet/acre/year."
  if (is.null(footnote3)) {
    footnote3 <- footnote
  } else {
    footnote4 <- footnote
  }

  estdat2 <- tabgrp(esttype="AREA", cond=cond, pltstrat=pltstrat, rowvar=rowvar,
	row.orderby=row.orderby, colvar=colvar, rowgrp=rowgrp, colgrp=colgrp, 
	colgrpcd=colgrpcd, rowgrpnm=rowgrpnm, rowgrpord=rowgrpord, col.FIAname=TRUE, 
	sumunits=TRUE, landarea=landarea, unitvar="ESTN_UNIT", unitarea=unitarea, 
	stratalut=stratalut, rowgrp.subtot=rowgrp.subtot, allin1=allin1, divideby=divideby,
 	title.rowgrp=title.rowgrp) 
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
  ## B44 - Area of forest land by inventory unit, county, ownership group, 
  ##			and forest land status
  #########################################################################################
  tabnm <- "B44"
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
  divideby="thousand"
  returntitle=TRUE
  rowgrp.subtot=TRUE

  coltottxt="All forest land"
  cellwidth=13

  footnote <- "Other forest land is classified as not capable of growing 20 cubic feet/acre/year."
  if (is.null(footnote3)) {
    footnote3 <- footnote
  } else {
    footnote4 <- footnote
  }

  estdat2 <- tabgrp(esttype="AREA", cond=cond, tree=NULL, pltstrat=pltstrat, 
	rowvar=rowvar, colvar=colvar, rowgrp=rowgrp, colgrp=TRUE, colgrpcd=colgrpcd,
	row.orderby=row.orderby, rowgrpnm=rowgrpnm, rowgrpord=rowgrpord, 
	col.FIAname=col.FIAname, sumunits=TRUE, landarea=landarea, unitvar="ESTN_UNIT", 
	unitarea=unitarea, stratalut=stratalut, rowgrp.subtot=rowgrp.subtot, 
	allin1=allin1, divideby=divideby) 
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
  ## B45 - Area of timberland by inventory unit, county, and stand-size class 
  #########################################################################################
  tabnm <- "B45"
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
  divideby="thousand"
  returntitle=TRUE

  coltottxt="All size classes"
  cellwidth=13
  rowgrp.subtot=TRUE

  estdat2 <- tabgrp(esttype="AREA", cond=cond, tree=NULL, pltstrat=pltstrat, 
	rowvar=rowvar, colvar=colvar, rowgrp=rowgrp, colgrp=colgrp, colgrpcd=colgrpcd,
	row.orderby=row.orderby, rowgrpnm=rowgrpnm, rowgrpord=rowgrpord, 
	col.FIAname=col.FIAname, sumunits=TRUE, landarea=landarea, unitvar="ESTN_UNIT", 
	unitarea=unitarea, stratalut=stratalut, rowgrp.subtot=rowgrp.subtot, 
	allin1=allin1, divideby=divideby) 
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
  ## B46 - Area of timberland by inventory unit, county, and stocking class 
  #########################################################################################
  tabnm <- "B46"
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
  divideby="thousand"
  returntitle=TRUE

  coltottxt="All size classes"
  cellwidth=13
  rowgrp.subtot=TRUE

  estdat2 <- tabgrp(esttype="AREA", cond=cond, tree=NULL, pltstrat=pltstrat, 
	rowvar=rowvar, colvar=colvar, rowgrp=rowgrp, colgrp=colgrp, colgrpcd=colgrpcd,
	row.orderby=row.orderby, rowgrpnm=rowgrpnm, rowgrpord=rowgrpord, 
	col.FIAname=col.FIAname, sumunits=TRUE, landarea=landarea, unitvar="ESTN_UNIT", 
	unitarea=unitarea, stratalut=stratalut, rowgrp.subtot=rowgrp.subtot, 
	allin1=allin1, divideby=divideby) 
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
  ## B47 - Net volume of growing-stock trees on timberland by inventory unit, county,
  ##			and major species group
  #########################################################################################
  tabnm <- "B47"
  esttype <- "TREE"
  rowvar <- "COUNTYNM"
  row.orderby="COUNTYCD"
  rowgrp <- TRUE
  rowgrpnm="UNITNM"
  rowgrpord="UNITCD"
  colvar <- "SPGRPGRPNM"
  col.orderby <- "SPGRPGRPCD"
  colgrp <- FALSE
  landarea <- "TIMBERLAND"
  title.colvar <- "Major species group"

  estvar <- "VOLCFNET"
  estvar.filter="TREECLCD == 2 & DIA >= 5.0"
  estvar2 <- "VOLBFNET"
  estvar.filter2 <- "TREECLCD == 2 & STATUSCD == 1"

  col.add0=TRUE
  divideby="thousand"
  returntitle=TRUE

  coltottxt="All size classes"
  cellwidth=13
  rowgrp.subtot=TRUE

  estdat1 <- tabgrp(esttype="TREE", cond=cond, tree=tree2, pltstrat=pltstrat, 
	rowvar=rowvar, colvar=colvar, rowgrp=rowgrp, colgrp=colgrp,
	row.orderby=row.orderby, rowgrpnm=rowgrpnm, rowgrpord=rowgrpord, 
	col.orderby=col.orderby, sumunits=TRUE, landarea=landarea, unitvar="ESTN_UNIT", 
	unitarea=unitarea, stratalut=stratalut, rowgrp.subtot=rowgrp.subtot, 
	allin1=allin1, divideby=divideby, estvar=estvar, estvar.filter=estvar.filter) 
  esttab <- estdat1$esttab
  tabtitle <- estdat1$esttab.title
  title.rowvar <- estdat1$title.rowvar
  title.colvar <- estdat1$title.colvar
  cnames <- estdat1$cnames
  title.rowgrp <- estdat1$title.rowgrp
  psetab <- estdat1$psetab
  psetab.title <- estdat1$psetab.title


  estdat2 <- tabgrp(esttype="TREE", cond=cond, tree=tree2, pltstrat=pltstrat, 
	rowvar=rowvar, colvar=colvar, rowgrp=rowgrp, colgrp=colgrp,
	row.orderby=row.orderby, rowgrpnm=rowgrpnm, rowgrpord=rowgrpord, 
	col.orderby=col.orderby, sumunits=TRUE, landarea=landarea, unitvar="ESTN_UNIT", 
	unitarea=unitarea, stratalut=stratalut, rowgrp.subtot=rowgrp.subtot, 
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

  if (returndata) 
    return(list(datPlots=datPlots, datStrata=datStrata, ref_countycd=ref_countycd, ref_unitcd=ref_unitcd))

  #if (savedata) {
  #  save(datPlots, file=paste0(outfolder, "/datPlots.rda"))
  #  save(datStrata, file=paste0(outfolder, "/datStrata.rda"))
  #  save(ref_countycd, file=paste0(outfolder, "/ref_countycd.rda"))
  #  save(ref_unitcd, file=paste0(outfolder, "/ref_unitcd.rda"))
  #}
}

