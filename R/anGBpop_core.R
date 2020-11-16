anGBpop_core <- function(GBpopdat, title.ref, xlsx=FALSE, 
		outfolder=NULL, outfn.date=TRUE, divideby.vol="million") {

  ## Set global variables
  SPGRPCD=footnote3=footnote4 <- NULL
  divideby <- "thousand"


  if (xlsx) {
    if (Sys.getenv("JAVA_HOME")!="") 
      Sys.setenv(JAVA_HOME="") 
    #library(rJava)
    #library(xlsx)
  }
  gui <- FALSE
  cellwidth <- ifelse(allin1, 12, 10)
  estround <- 1
  fill <- TRUE
  addSEcol <- FALSE

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

  ## Check xlsx 
  xlsx <- FIESTA::pcheck.logical(xlsx, varnm="xlsx", 
		title="Excel?", first="NO", gui=gui)  

  savedata <- ifelse(xlsx, FALSE, TRUE)
  rawdata <- ifelse(xlsx, FALSE, TRUE)
  returntitle <- ifelse(xlsx, FALSE, TRUE)
  allin1 <- ifelse(xlsx, TRUE, FALSE)

  if (xlsx) {
    ## Set workbook name
    outfn <- paste0(gsub(" ", "", gsub(",", "", title.ref)), "_core_tables")

    ## Check Excel workbook
    wbnm <- FIESTA::pcheck.xlsx(wbnm=NULL, savewb=TRUE, outfn=outfn, outfolder=outfolder)

    ## Define footnotes
    footnote1 <- "Numbers in rows and columns may not sum to totals due to rounding."
    footnote2 <- "A dash (--) indicates no sample for the cell; 0 indicates a value of greater than 0 but less than 0.1."


    ## Set up Excel Workbook
    wb <- xlsx::loadWorkbook(file=wbnm)
    sheetnm <- "TOC"

    newsheet <- xlsx::createSheet(wb, sheetName=as.character(as.character(sheetnm)))
    datsheet <- xlsx::getSheets(wb)[[sheetnm]]
    message(paste("saving to sheet", sheetnm))


    #######################################################################################
    ## Table of Contents (TOC)
    #######################################################################################
    t1 <- "Table 01. Area by land class and reserved status"
    t2 <- "Table 02. Area by fortyp and stand-size class"

    toc <- data.frame(rbind(t1), stringsAsFactors=FALSE)


    ## Create row and cells for title
    toctitle.row <- xlsx::createRow(datsheet, 1)
    toctitle.cells <- xlsx::createCell(toctitle.row, colIndex=1)
    xlsx::setRowHeight(toctitle.row, multiplier=2)
    xlsx::setCellValue(toctitle.cells[[1]], "Table of Contents")

    xlsx:: addDataFrame(toc, datsheet, col.names=FALSE, row.names=FALSE, startRow=3)

    ## SAVE EXCEL WORKBOOK
    ###############################################################
    xlsx::saveWorkbook(wb, wbnm) 
  } 


  #######################################################################################
  ## 01 - Area by land class and reserved status
  #######################################################################################
  tabnm <- "01"
  landarea <- "ALL"
  rowvar <- "COND_STATUS_CD"
  colvar <- "RESERVCD"
  coltottxt <- NULL

  estdat <- modGBarea(GBpopdat=GBpopdat, landarea=landarea, 
		rowvar=rowvar, row.FIAname=TRUE, sumunits=TRUE,
		colvar=colvar, col.FIAname=TRUE, col.add0=TRUE,
		rawdata=rawdata, savedata=savedata, returntitle=returntitle, 
		allin1=allin1, title.ref=title.ref)
  estdat$est
  esttab <- estdat$est
  tabtitle <- estdat$titlelst$title.estpse
  title.colvar <- estdat$titlelst$title.colvar
  title.rowvar <- estdat$titlelst$title.rowvar


  if (xlsx) {
    write2xlsx(esttab=esttab, tabtitle=paste0("Table ", tabnm, ". ", tabtitle),  
		title.colvar=title.colvar, outfolder=outfolder,
		title.rowvar=title.rowvar, fill=fill, allin1=allin1, addSEcol=addSEcol,
		coltottxt=coltottxt, cellwidth=cellwidth, wbnm=wbnm, sheetnm=tabnm,
           footnote1=footnote1, footnote2=footnote2, footnote3=footnote3)
  }



  #######################################################################################
  ## 02 - Area by forest type and stand-size class
  #######################################################################################
  tabnm <- "02"
  landarea <- "FOREST"
  rowvar <- "FORTYPCD"
  colvar <- "STDSZCD"
  coltottxt <- "All forest land"

  estdat <- modGBarea(GBpopdat=GBpopdat, landarea=landarea, 
		rowvar=rowvar, row.FIAname=TRUE, sumunits=TRUE,
		colvar=colvar, col.FIAname=TRUE, col.add0=TRUE,
		rawdata=rawdata, savedata=savedata, returntitle=returntitle, 
		allin1=allin1, title.ref=title.ref)
  estdat$est
  esttab <- estdat$est
  tabtitle <- estdat$titlelst$title.estpse
  title.colvar <- estdat$titlelst$title.colvar
  title.rowvar <- estdat$titlelst$title.rowvar

  if (xlsx) {
    write2xlsx(esttab=esttab, tabtitle=paste0("Table ", tabnm, ". ", tabtitle),  
		title.colvar=title.colvar, outfolder=outfolder,
		title.rowvar=title.rowvar, fill=fill, allin1=allin1, addSEcol=addSEcol,
		coltottxt=coltottxt, cellwidth=cellwidth, wbnm=wbnm, sheetnm=tabnm,
           footnote1=footnote1, footnote2=footnote2, footnote3=footnote3)
  }
}