#pcheck.xlsx
#wrapSE
#setCells		Populates Excel spreadsheets with table data.

#tabgrp <- function(esttype, cond=NULL, tree=NULL, pltassgn=NULL, rowvar,
#	colvar=NULL, pcfilter=NULL, title.rowvar=NULL, title.colvar=NULL,
#	title.rowgrp=NULL, row.FIAname=FALSE, col.FIAname=FALSE, estvar=NULL,
#	estvar.filter=NULL, sumunits=TRUE, landarea=NULL, unitacres=NULL, stratalut=NULL,
#	allin1=FALSE, row.add0=FALSE, col.add0=FALSE) {



pcheck.xlsx <- function(wbnm, savewb=TRUE, outfn=NULL, outfolder=NULL,
	outfn.date=TRUE, overwrite=FALSE)  {
  ## CREATE EXCEL WORKBOOK AND SHEET
  ###############################################################
  if (is.null(outfn)) outfn <- "WORKBOOK"

  newwb <- FALSE
  if (is.null(wbnm)) {
    wb <- xlsx::createWorkbook(type="xlsx")
    newwb <- TRUE
  } else {
    if (!file.exists(wbnm)) {

      if (!file.exists(dirname(wbnm))) stop("invalid directory")
      wb.basenm <- basename(wbnm)
      outfolder <- dirname(wbnm)

      ## Check if there is an extension
      if (is.na(getext(wbnm)) || getext(wbnm) == "NA") {
        message("wbnm must end in xlsx.. adding to wbnm")
        wbnm <- paste0(wbnm, ".xlsx")
        if (file.exists(wbnm)) {
          message(paste("creating workbook:", wbnm))
          wb <- xlsx::loadWorkbook(file=wbnm)
          outfn <- wb.basenm
        }
      } else {
        message(paste(wbnm, "does not exist... creating new workbook"))
        wb <- xlsx::createWorkbook(type="xlsx")
        outfn <- basename.NoExt(wb.basenm)
        newwb <- TRUE
      }
    } else {
      wb <- xlsx::loadWorkbook(file=wbnm)
    }
  }
  if (savewb) {

    ## GET NAME FOR WORKBOOK
    ###############################################################
    if (newwb) {

      ## Check outfn.date
      outfn.date <- pcheck.logical(outfn.date, varnm="outfn.date",
		title="Add date to filenm?", first="YES")
      if (outfn.date)
        outfn <- paste0(outfn, "_", format(Sys.time(), "%Y%m%d"))

      ## Check overwrite
      overwrite <- pcheck.logical(overwrite, varnm="overwrite",
		title="Overwrite file?", first="YES")

      if (!overwrite) {
        outfn <- fileexistsnm(outfolder, outfn, "xlsx")
        if (!is.null(outfolder))
          outfn <- paste(outfolder, outfn, sep="/")
      } else {
        if (is.null(outfolder)) outfolder <- dirname(outfn)
        outfn <- paste(outfolder, outfn, sep="/")
      }
      outfilenm <- paste0(outfn, ".xlsx")

      #outallin1base <- paste0(outfn, "_", format(Sys.time(), "%Y%m%d"))
      #outallin1fn <- fileexistsnm(outfolder, outfn, "xlsx")
      #outfilenm <- paste0(outfolder, "/", outallin1fn, ".xlsx")
    } else {
      outfilenm <- wbnm
    }

    ## SAVE EXCEL WORKBOOK
    ###############################################################
    xlsx::saveWorkbook(wb, outfilenm)

    cat(
    " ###############################################################################",
    "\n", paste("Table written to: "), "\n", paste(" ", outfilenm), "\n",
    "###############################################################################",
    "\n" )
  }

  return(wbnm=outfilenm)
}


wrapSE <- function(x) {
  xsplit <- strsplit(as.character(x), " \\(")[[1]]
  paste(xsplit, collapse="\r\n (")
}


setCells <- function(datsheet, estgrp, psegrp, nbrgrps=1, startrow=1, endrow=NULL,
		subtotal=TRUE, totcols, estcols, psecols, esttab.style, psetab.style,
		rnames=rnames, norname=FALSE, allin1=FALSE, addSEcol=FALSE) {

  ###############################################################################
  ## DESCRIPTION: Populates Excel spreadsheets with table data.
  ## ARGUMENTS:
  ## datsheet		jobjRef. Excel .xlsx sheet
  ## estgrp			DF. Table with estimates
  ## psegrp			DF. Table with percent standard errors
  ## nbrgrps		Number. Number of row groups
  ## startrow		Number. The starting row number for populating cells
  ## endrow			Number. The ending row number for populating cells
  ## subtotal		Logical. If TRUE, subtotal is in table.
  ## totcols		Number. Number of total columns in output table.
  ## estcols		Number. Columns identifying estimates.
  ## psecols 		Number. Columns identifying percent standard errors.
  ## esttab.style	CellStyle. Style for output estimates.
  ## psetab.style	CellStyle. Style for output percent standard errors.
  ## rnames			String vector. Row name(s).
  ## norname		Logical. If there no rname is used for output table.
  ## allin1			Logical. If TRUE, estimate and percent standard error in same cell.
  ## addSEcol		Logical. If TRUE, %SE is included in table.

  if ("data.table" %in% class(estgrp)) {
    estgrp <- data.table::setDF(estgrp)
  }
  if (!is.null(psegrp) && "data.table" %in% class(psegrp)) {
    psegrp <- data.table::setDF(psegrp)
  }


  ## Create new rows for table data
  ################################################################
  if (is.null(endrow)) endrow <- nrow(estgrp)
  endrow.tab <- startrow + endrow-1
  tab.rows <- xlsx::createRow(datsheet, startrow:endrow.tab)
  tab.cells <- xlsx::createCell(tab.rows, colIndex=1:totcols)

  if (is.null(norname) || is.na(norname)) norname <- FALSE
  colIndex.rname <- 1:(ifelse(norname, nbrgrps, nbrgrps + 1))

  rnamecol <- ifelse(norname, nbrgrps, nbrgrps + 1)
  if (rnamecol == 0) rnamecol <- 1
  tabcols <- (nbrgrps+2):ncol(estgrp)

  ## Add row names
  ################################################################
  rnamevals <- as.matrix(estgrp[, rnames[rnamecol]])
  value.cells <- mapply(xlsx::setCellValue, tab.cells[,rnamecol], rnamevals)

  ## Add table data values
  ################################################################
  estgrp.cells <- xlsx::getCells(tab.rows, colIndex=estcols)
  #tabvals <- t(as.matrix(estgrp[, -(colIndex.rname)]))

  tabvals <- t(as.matrix(estgrp[, tabcols]))

  if (allin1) tabvals <- mapply(wrapSE, tabvals)
  value.cells <- mapply(xlsx::setCellValue, estgrp.cells, tabvals)

  ## Set style for cell data values
  ################################################################
  style.cells <- lapply(estgrp.cells,
			function(x) xlsx::setCellStyle(x, esttab.style))

  ## Edit NULL values in Percent Sampling Error Columns
  ################################################################
  if (!allin1) {
    ## Change null values, 0 values, and NA values of est cells to --
    lapply(estgrp.cells, function(x) {
      cellval <- xlsx::getCellValue(x)
      if (is.null(cellval) | cellval == 0 | is.na(cellval))
			xlsx::setCellValue(x, "--")
    })

    if (addSEcol) {
      psegrp.cells <- xlsx::getCells(tab.rows, colIndex=psecols)
      psegrp <- data.frame(lapply(psegrp, function(x) gsub("--", 0, x)), stringsAsFactors=FALSE)
      psegrp[, (tabcols) := lapply(.SD, suppressMessages(check.numeric)), .SDcols=tabcols]
      value.cells <- mapply(xlsx::setCellValue, psegrp.cells,
			t(as.matrix(psegrp[, tabcols])))
      style.cells <- lapply(psegrp.cells,
			function(x) xlsx::setCellStyle(x, psetab.style))

      ## Change null values, 0 values, and NA values of pse cells to --
      lapply(psegrp.cells, function(x) {
        cellval <- xlsx::getCellValue(x)
        if (is.null(cellval) | cellval == 0 | is.na(cellval))  xlsx::setCellValue(x, "--")
      })
    }
  }
}

