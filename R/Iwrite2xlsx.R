write2xlsx <- function(esttab, psetab=NULL, wbnm=NULL, sheetnm=NULL, tabtitle=NULL,
 	subtitle=NULL, rowgrp=FALSE, rowgrp2=FALSE, colgrp=FALSE, rowgrpnm=NULL,
	rowgrp2nm=NULL, rowgrp.subtot=FALSE, rowgrp2.subtot=FALSE, subtotvals=NULL,
 	colgrp.subtot=FALSE, cnames=NULL, rowtotal=TRUE, coltotal=TRUE, rowtottxt="Total",
 	coltottxt="Total", estnm="Estimate", psenm="Percent Sampling Error", 
	title.colvar=NULL, title.rowvar=NULL, title.fontsize=12, fill=TRUE, 
	title.rowgrp=NULL, estround=1, pseround=2, allin1=FALSE, addSEcol=FALSE, 
	footnote1=NULL, footnote2=NULL, footnote3=NULL, footnote4=NULL, savewb=TRUE, 
	cellWidth.grp=6, cellwidth=11, cellwidthSE=NULL, psenm.out="PSE", outfn=NULL, 
	outfolder=NULL, autosize=FALSE) {
  #####################################################################################
  ## DESCRIPTION: Internal function to format table and write to file.
  ## FUNCTIONS CALLED: 
  ## fileexistsnm  - Internal function to check if file exists. If so, adds a 
  ## number to the basenm and returns just basenm.
  ##  
  ## ARGUMENTS: 
  ##  tab    	Data frame. The table to output.
  ##  tabtitle	String. The table title.
  ##  cellWidth.grp  	Number. Width of cell with group names.
  ##  cellWidth 		Number. Width of cells with estimates.
  ##  cellWidthSE	Number. Width of cells with percent standard errors if addSEcol=TRUE.
  ##					If NULL, cellWidthSE = cellWidth.	
  ##  psenm.out		String. The column name for percent standard errors if addSEcol=TRUE.
  ##  outfn    	String. The output file name.
  ##  outfolder  String. The name of the output folder.
  ##
  ## VALUE:
  ##  Writes out a table to a tab-separated text file in the outfolder.
  #####################################################################################
  #requireNamespace("rJava")
  #requireNamespace("xlsx")

  if (!"rJava" %in% rownames(installed.packages()))
    stop("write2xlsx requires package rJava")
  if (!"xlsx" %in% rownames(installed.packages()))
    stop("write2xlsx requires package xlsx")

  ## This works 
  if (Sys.getenv("JAVA_HOME")!="") 
    Sys.setenv(JAVA_HOME="") 

  rowgrp.sub <- ifelse(rowgrp, TRUE, FALSE)

  ## Check Excel workbook
  ###############################################################
  wbnm <- FIESTA::pcheck.xlsx(wbnm)
  wb <- xlsx::loadWorkbook(file=wbnm)

  if (is.null(sheetnm)) {
    if (nchar(outfn) > 30) {
      warning("Truncated sheet name to 30 characters")
      outfn <- substr(outfn, 0, 30)  
    }
    sheetName <- outfn
    newsheet <- xlsx::createSheet(wb, sheetName=sheetName)
    datsheet <- xlsx::getSheets(wb)[[sheetName]]
  } else {
    #if (!newwb) {
      if (!is.null(xlsx::getSheets(wb)[[sheetnm]])) {
        message("sheet exists.. overwriting")
        xlsx::removeSheet(wb, sheetnm)
      }
    #}

    newsheet <- xlsx::createSheet(wb, sheetName=as.character(as.character(sheetnm)))
    datsheet <- xlsx::getSheets(wb)[[sheetnm]]
    message(paste("saving to sheet", sheetnm))
  }

  ####################################################################################
  ## Get table size
  #################################################################################### 
  if (allin1) addSEcol <- FALSE
  if (rowgrp2) rowgrp <- TRUE

  ## Get columns for txt and numbers and total
  if (rowgrp) {
    if (rowgrp2) {
      rnames <- names(esttab)[1:3]
    } else {
      rnames <- names(esttab)[1:2]
    }
  } else {
    rnames <- names(esttab)[1]
  }

  ## If only 1 table, split into 2 tables (esttab and psetab)
  if (addSEcol && is.null(psetab)) {
    if (!"Percent Sampling Error" %in% names(esttab))
      stop("A column named Percent Sampling Error does not exist in table")
    psetab <- esttab[, c(rnames, psenm)]
    esttab <- esttab[, c(rnames, estnm)]
  }
  if (!addSEcol & !allin1) esttab <- esttab[, c(rnames, 
		names(esttab)[!names(esttab) %in% c(rnames, psenm)])]

  ## Get end of table
  nrows <- nrow(esttab)		## Number of rows
  ncols <- ncol(esttab)		## Number of columns	
 
  colIndex.txt <- 1:length(rnames)				## Columns with text (row names)
  colStart.nbr <- length(colIndex.txt) + 1			## Starting column of numbers

  ## Total columns in table with numbers (with and without total)
  tabcols.nbr <- colStart.nbr:ncols	
  if (coltotal) { 
    tabcols.nbr.notot <- colStart.nbr:(ncols-1)	
  } else {
    tabcols.nbr.notot <- colStart.nbr:ncols	
  }	

  ## Make sure all est number columns are numeric
  if (!allin1) 
    suppressWarnings(esttab[, tabcols.nbr] <-  
 			lapply(esttab[, tabcols.nbr, drop=FALSE], as.numeric)) 

  
  ## Output column number and index
  cols <- ifelse(addSEcol, length(tabcols.nbr)*2, length(tabcols.nbr))
  cols.notot <- ifelse(addSEcol, length(tabcols.nbr.notot)*2, length(tabcols.nbr.notot))
  colIndex.nbr <- colStart.nbr:(length(rnames) + cols)
  colIndex.nbr.notot <- colStart.nbr:(length(rnames) + cols.notot)
 
  if (addSEcol) {
    estcols <- colIndex.nbr[seq(1, length(colIndex.nbr), 2)]
    estcols.notot <- colIndex.nbr.notot[seq(1, length(colIndex.nbr.notot), 2)]
    psecols <- estcols + 1
    psecols.notot <- estcols.notot + 1
  } else {
    estcols <- colIndex.nbr
    estcols.notot <- colIndex.nbr.notot
  }
  totcols <- max(colIndex.nbr)
  totcols.notot <- max(colIndex.nbr.notot)

  ## Get column names
  if (is.null(cnames)) {
    if (colgrp) stop("must include column names (cnames)") 
    cnames <- data.frame(COLVAR=names(esttab)[-(colIndex.txt)], stringsAsFactors=FALSE)

    if (coltotal) cnames <- data.frame(COLVAR=cnames[-nrow(cnames),]) 
  } else {
    cnames.names <- names(cnames)
    cnames <- data.frame(cnames[-nrow(cnames),], stringsAsFactors=FALSE)
    names(cnames) <- cnames.names
  }

  ####################################################################################
  ## Title
  #################################################################################### 
  startrow <- 0
  startrow.rnames <- 1
  if (!is.null(tabtitle)) {
    startrow <- startrow + 1
    startrow.tabtitle <- startrow
    startrow.rnames <- startrow.rnames + 1

    ## Set style for title
    title.style <- xlsx::CellStyle(wb) + xlsx::Alignment(wrapText=TRUE) + 
		xlsx::Font(wb, heightInPoints=title.fontsize, isBold=TRUE) +
		xlsx::Border(color="black", position="BOTTOM", pen="BORDER_THIN")

    title.style <- xlsx::CellStyle(wb) + xlsx::Alignment(wrapText=TRUE) + 
		xlsx::Font(wb, heightInPoints=title.fontsize, isBold=TRUE) +
		xlsx::Border(color="black", position="BOTTOM", pen="BORDER_THIN")


    ## Create row and cells for title
    title.row <- xlsx::createRow(datsheet, startrow)
    title.cells <- xlsx::createCell(title.row, colIndex=1:totcols)

    xlsx::setRowHeight(title.row, multiplier=2)

    ## Add title to workbook and set style
    xlsx::setCellValue(title.cells[[1]], tabtitle)
    xlsx::setCellStyle(title.cells[[1]], title.style)   
   
    ## Add style to title
    lapply(title.cells[1:totcols], function(x) xlsx::setCellStyle(x, title.style))

 
    ## Merge cells of title
    xlsx::addMergedRegion(datsheet, startRow=startrow.tabtitle, endRow=startrow.tabtitle, 
			startColumn=1, endColumn=totcols)

    if (!is.null(subtitle)) { 
      startrow <- startrow + 1
      startrow.subtitle <- startrow
      startrow.rnames <- startrow.rnames + 1

      ## Set style for subtitle
      subtitle.style <- xlsx::CellStyle(wb) + 
		xlsx::Font(wb, heightInPoints=title.fontsize, isBold=TRUE)

      ## Create cells for subtitle
      subtitle.row <- xlsx::createRow(datsheet, startrow.subtitle)
      subtitle.cell <- xlsx::createCell(subtitle.row, colIndex=1:totcols)

      ## Add title to workbook and set style
      xlsx::setCellValue(subtitle.cell[[1]], subtitle)      
      xlsx::setCellStyle(subtitle.cell[[1]], subtitle.style)  
      xlsx::addMergedRegion(datsheet, startRow=startrow.subtitle, endRow=startrow.subtitle, 
			startColumn=1, endColumn=totcols)
    }
  } 

  ####################################################################################
  ## Column Group Names or Column Title
  #################################################################################### 
  fgColor <- "darkolivegreen"
  txtColor <- "ivory"
  bColor <- "darkkhaki"

  ## Set style for column title groups
  if (fill) {
    font.fill <- xlsx::Font(wb, color=txtColor, isBold=TRUE)
    fg.fill <- xlsx::Fill(foregroundColor=fgColor)
  } else {
    font.nofill <- xlsx::Font(wb, color="black", isBold=TRUE)
  }
  title.alignment <- xlsx::Alignment(wrapText=TRUE, horizontal="ALIGN_CENTER")

  ## Column title
  #########################################################################
  newrow <- startrow
  if (!is.null(title.colvar)) {
    newrow <- newrow + 1
    startrow.ctitle <- newrow
    #startrow.rnames <- newrow

    ## Set style for column title
    if (!is.null(tabtitle)) { 
      ctitle.border <- xlsx::Border(color="black", position="BOTTOM", pen="BORDER_THICK")
    } else {
      ctitle.border <- xlsx::Border(color="black", position=c("TOP", "BOTTOM"), 
			pen=c("BORDER_THIN", "BORDER_THICK"))
    }
    if (fill) {
      ctitle.style <- xlsx::CellStyle(wb) + title.alignment + font.fill + fg.fill + 
		xlsx::Border(color=bColor, position="BOTTOM", pen="BORDER_THIN")
    } else {
      ctitle.style <- xlsx::CellStyle(wb) + title.alignment + ctitle.border
    }

    ## Create row and cells for column title
    ctitle.row <- xlsx::createRow(datsheet, startrow.ctitle)
    ctitle.cells <- xlsx::createCell(ctitle.row, colIndex=1:totcols)
    ctitle.endcol <- max(colIndex.nbr.notot)

    xlsx::setRowHeight(ctitle.row, multiplier=1.25)
 

    ## Add column title to workbook and merge cells
    xlsx::setCellValue(ctitle.cells[[colStart.nbr]], title.colvar) 
    xlsx::addMergedRegion(datsheet, startRow=startrow.ctitle, endRow=startrow.ctitle, 
		startColumn=colStart.nbr, endColumn=ctitle.endcol)

    ## Add style to column title
    if (fill) {
      lapply(ctitle.cells[1:totcols.notot], function(x) xlsx::setCellStyle(x, ctitle.style))
    } else {
      lapply(ctitle.cells[colStart.nbr:totcols.notot], 
		function(x) xlsx::setCellStyle(x, ctitle.style))
    }
  } 

  ## Add column group names
  if (colgrp) {
    newrow <- newrow + 1
    #startrow.rnames <- newrow

    if (fill) {
      cgrpnames.style <- xlsx::CellStyle(wb) + font.fill + fg.fill + title.alignment +
      	xlsx::Border(color=bColor, position="BOTTOM", pen="BORDER_THIN")	
      cgrprnames.style <- xlsx::CellStyle(wb) + fg.fill 
    } else {
      cgrpnames.style <- xlsx::CellStyle(wb) + title.alignment +
      	xlsx::Border(color="black", position=c("TOP", "BOTTOM"), 
			pen=c("BORDER_THIN", "BORDER_THICK"))
      if (is.null(tabtitle)) {
        cgrprnames.style <- xlsx::CellStyle(wb) +
      	xlsx::Border(color="black", position="BOTTOM", pen="BORDER_THICK") 
#      } else {
#        cgrprnames.style <- xlsx::CellStyle(wb) +
#      	xlsx::Border(color="black", position=c("TOP", "BOTTOM"), 
#		pen=c("BORDER_THIN", "BORDER_THICK"))
      } 
    }

    ## Get cells for column group names
    cgrpnames.row <- xlsx::createRow(sheet=datsheet, newrow)
    cgrpnames.cells <- xlsx::createCell(cgrpnames.row, 1:totcols)
    ctitle.endcol <- max(colIndex.nbr.notot)

    ## Merge cells
    colgrpnames <- cnames$COLGRP
    cgrpmerge <- table(colgrpnames)

    if (addSEcol) {
      cgrpmerge <- cgrpmerge*2
      colgrpnames <- rep(names(cgrpmerge), cgrpmerge)
    }

    ## Add title to workbook and set style
    mapply(xlsx::setCellValue, cgrpnames.cells[colStart.nbr:ctitle.endcol], 
		colgrpnames)      
    lapply(cgrpnames.cells[colStart.nbr:ctitle.endcol], 
		function(x) xlsx::setCellStyle(x, cgrpnames.style))

    ## Merge group cells
    startColumn <- colStart.nbr
    for (i in 1:length(cgrpmerge)) {
      cmerge <- cgrpmerge[i]
      endColumn <- startColumn + cmerge - 1
      xlsx::addMergedRegion(datsheet, startRow=newrow, endRow=newrow, 
		startColumn=startColumn, endColumn=endColumn)   
      startColumn <- startColumn + cmerge  
    } 
  } 

  ####################################################################################
  ## Column Names
  ####################################################################################
  newrow <- newrow + 1
  startrow.cnames <- newrow 
  endrow.rnames <- startrow.cnames

  ## Set style for column names
  if (fill) {
    cnames.style <- xlsx::CellStyle(wb) + title.alignment + font.fill + fg.fill +		
			xlsx::Border(color=bColor, position="BOTTOM", pen="BORDER_THIN")
  } else {
    cnames.style <- xlsx::CellStyle(wb) + title.alignment + 
			xlsx::Border(color="black", position="BOTTOM", pen="BORDER_THIN")
  }

  ## Create cells for column names
  cnames.row <- xlsx::createRow(datsheet, startrow.cnames)
  cnames.cells <- xlsx::createCell(cnames.row, colIndex=1:totcols)

  ## Column names text
  coltxt <- as.character(cnames$COLVAR)

  ## Set column numbers
  if (addSEcol) 
    ## Merge column names cells for Estimate and %SE cells below
    for (startColumn in estcols.notot)
      xlsx::addMergedRegion(datsheet, startRow=newrow, endRow=newrow, 
			startColumn=startColumn, endColumn=startColumn+1)

  if (coltotal) {
    colStart.tot <- totcols.notot + 1
    if (is.null(coltottxt))  coltottxt <- "Total"
    xlsx::addMergedRegion(datsheet, startRow=startrow.rnames, endRow=startrow.cnames, 
		startColumn=colStart.tot, endColumn=totcols)
    totrow <- xlsx::getRows(datsheet, startrow.rnames)
    totcell <- xlsx::getCells(totrow, colStart.tot)
    value.cells <- mapply(xlsx::setCellValue, totcell, coltottxt) 
    lapply(totcell, function(x) xlsx::setCellStyle(x, cnames.style))

    ## Add column names to esttab cells
    #value.cells <- mapply(xlsx::setCellValue, cnames.cells[estcols], coltxt) 
#  } else {
#
#    ## Add column names to esttab cells
#    value.cells <- mapply(xlsx::setCellValue, cnames.cells[estcols.notot], coltxt) 
  }

    ## Add column names to esttab cells
    value.cells <- mapply(xlsx::setCellValue, cnames.cells[estcols.notot], coltxt) 

 
  ## Add style to column names cells
  lapply(cnames.cells[-(colIndex.txt)], function(x) xlsx::setCellStyle(x, cnames.style))


  #########################################################################
  ## If addSEcol
  #########################################################################
  if (addSEcol) {

    ## Estimate/%SE column titles
    #########################################################################
    newrow <- newrow + 1
    startrow.cnames2 <- newrow
    endrow.rnames <- startrow.cnames2

    ## Set style for Estimate/%SE column titles
    if (fill) {
      cnames2.style <- xlsx::CellStyle(wb) + title.alignment + font.fill + fg.fill +
		xlsx::Border(color="black", position="BOTTOM", pen="BORDER_THIN")
    } else {
      cnames2.style <- xlsx::CellStyle(wb) + title.alignment +
		xlsx::Border(color="black", position="BOTTOM", pen="BORDER_THIN") 
    }

    ## Create cells for column name Estimate/%SE titles
    cnames2.row <- xlsx::createRow(datsheet, startrow.cnames2)
    cnames2.cells <- xlsx::createCell(cnames2.row, colIndex=1:totcols)

    ## Add column Estimate/%SE names to cells
    value.cells <- mapply(xlsx::setCellValue, cnames2.cells[colIndex.nbr], 
			rep(c(estnm, psenm.out), length(estcols))) 

    ## Add style to column Estimate/%SE cells
    lapply(cnames2.cells[colIndex.nbr], function(x) xlsx::setCellStyle(x, cnames2.style))
  } 

  ####################################################################################
  ## Row title name(s)
  #################################################################################### 

  ## Set style for Total/%SE column titles
  if (fill) {
    rnames.style <- xlsx::CellStyle(wb) + xlsx::Alignment(wrapText=TRUE,
 		horizontal="ALIGN_LEFT") + font.fill + fg.fill
  } else {
    rnames.style <- xlsx::CellStyle(wb) + 
		xlsx::Alignment(wrapText=TRUE, horizontal="ALIGN_LEFT")
    rnames.bstyle <- xlsx::CellStyle(wb) + 
		xlsx::Border(color="black", position="BOTTOM", pen="BORDER_THIN")

    if (addSEcol) {
      lapply(cnames2.cells[colIndex.txt], function(x) xlsx::setCellStyle(x, rnames.bstyle))
    } else {
      lapply(cnames.cells[colIndex.txt], function(x) xlsx::setCellStyle(x, rnames.bstyle))
    }
  }
 
  ## Add row names and merge columns
  rname.row <- xlsx::getRows(datsheet, startrow.rnames)

  xlsx::addMergedRegion(datsheet, startRow=startrow.rnames, endRow=endrow.rnames, 
		startColumn=1, endColumn=length(rnames))
  rname.cells <- xlsx::getCells(rname.row, 1:length(rnames))
  value.cells <- mapply(xlsx::setCellValue, rname.cells, rnames)
  lapply(rname.cells, function(x) xlsx::setCellStyle(x, rnames.style))


  ####################################################################################
  ## Table Data Styles
  #################################################################################### 

  ## Set styles for table data
  ###########################################
  if (!allin1) {
    ## Set style for tables
    nbr.alignment <- xlsx::Alignment(wrapText=TRUE, horizontal="ALIGN_RIGHT")

    ## Format of est cells
    est.dformat <- "#,##0"
    if (estround > 0) 
      est.dformat <- paste(est.dformat, strrep("0", estround), sep=".")

    ## Style of est cells
    esttab.style <- xlsx::CellStyle(wb) + nbr.alignment + xlsx::DataFormat(est.dformat)

    if (rowgrp.subtot || rowgrp2.subtot) {
      ## Style for subtotals
      subtot.esttab.style <- xlsx::CellStyle(wb) + 
		xlsx::Border(color=bColor, position="TOP", pen="BORDER_THIN") + 
		nbr.alignment + xlsx::DataFormat(est.dformat)
      subtot.rname.style <- xlsx::CellStyle(wb) + 
		xlsx::Alignment(horizontal="ALIGN_LEFT") +
		xlsx::Border(color=bColor, position="TOP", pen="BORDER_THIN") 
    }
  } else {
 
    ## Style of est cells
    esttab.style <- xlsx::CellStyle(wb) + xlsx::Alignment(wrapText=TRUE,
 		horizontal="ALIGN_CENTER")

    if (rowgrp.subtot || rowgrp2.subtot) {
      ## Style for subtotals
      subtot.esttab.style <- xlsx::CellStyle(wb) + 
		xlsx::Border(color=bColor, position="TOP", pen="BORDER_THIN") + 
		xlsx::Alignment(wrapText=TRUE, horizontal="ALIGN_CENTER") 
      subtot.rname.style <- xlsx::CellStyle(wb) + 
		xlsx::Alignment(wrapText=TRUE, horizontal="ALIGN_LEFT") +
		xlsx::Border(color=bColor, position="TOP", pen="BORDER_THIN") 
    }
  }

  if (addSEcol) {
    ## Format of pse cells
    pse.dformat <- "#,##0"
    if (pseround > 0) 
      pse.dformat <- paste(pse.dformat, strrep("0", pseround), sep=".")

    ## Style of pse cells
    psetab.style <- xlsx::CellStyle(wb) + nbr.alignment + xlsx::DataFormat(pse.dformat)

    ## Style for subtotals
    subtot.psetab.style <- xlsx::CellStyle(wb) + xlsx::Border(color=bColor, 
		position="TOP", pen="BORDER_THIN") + nbr.alignment + 
		xlsx::DataFormat(pse.dformat)
  }

  ## Total row styles
  ################################################
  if (rowtotal) {
    fgColor.tot <- "lightyellow2"
    rowtot.border.fill <- xlsx::Border(color=bColor, position=c("TOP", "BOTTOM"), 
           		pen=c("BORDER_THIN", "BORDER_THICK"))
    rowtot.border.nofill <- xlsx::Border(color="black", position="TOP", 
				pen="BORDER_THIN")

    ## Total row names
    #########################################################################
    if (fill) {
      rowtotc1.style <- xlsx::CellStyle(wb) + xlsx::Fill(foregroundColor=fgColor.tot) +
			rowtot.border.fill + xlsx::Alignment(horizontal="ALIGN_LEFT")
    } else {
      rowtotc1.style <- xlsx::CellStyle(wb) + rowtot.border.nofill +
			xlsx::Alignment(horizontal="ALIGN_LEFT")
    }

    ## Total data value styles
    #########################################################################
    if (fill) {
      if (!allin1) { 
        rowtot.style <- xlsx::CellStyle(wb) + nbr.alignment + 
			xlsx::DataFormat(est.dformat) + 
			xlsx::Fill(foregroundColor=fgColor.tot) + rowtot.border.fill
        if (addSEcol) 
          rowtotSE.style <- xlsx::CellStyle(wb) + nbr.alignment + 
			xlsx::DataFormat(pse.dformat) +
			xlsx::Fill(foregroundColor=fgColor.tot) + rowtot.border.fill
      } else {
        rowtot.style <- xlsx::CellStyle(wb) + 
			xlsx::Alignment(wrapText=TRUE, horizontal="ALIGN_CENTER") + 
			xlsx::Fill(foregroundColor=fgColor.tot) + rowtot.border.fill
      }
    } else {
      if (!allin1) {
        rowtot.style <- xlsx::CellStyle(wb) + nbr.alignment + rowtot.border.nofill +
			xlsx::DataFormat(est.dformat)
        if (addSEcol) 
          rowtotSE.style <- xlsx::CellStyle(wb) + nbr.alignment + 
			xlsx::DataFormat(pse.dformat) + rowtot.border.nofill
      } else {
        rowtot.style <- xlsx::CellStyle(wb) + 
			xlsx::Alignment(wrapText=TRUE, horizontal="ALIGN_CENTER") + 
			rowtot.border.nofill
      }
    } 
  }

  ####################################################################################
  ## Table Data
  #################################################################################### 
  startrow.tab <- newrow  

  if (rowgrp2) {

    if (is.null(rowgrp2nm)) rowgrp2nm <- names(esttab)[1]

    ## Loop through row groups, not including grand total
    rowgrps2 <- unique(esttab[[rowgrp2nm]])
    rowgrps2 <- rowgrps2[rowgrps2 != rowtottxt]

    for (grp2 in rowgrps2) {
      print(grp2)
      estgrp2 <- esttab[esttab[[rowgrp2nm]] == grp2,]
      psegrp2 <- psetab[psetab[[rowgrp2nm]] == grp2,]

      ## Create new row and cells for rowgrp2 heading and set heading
      ################################################################
      startrow.tab <- startrow.tab + 1
      estgrp2.row <- xlsx::createRow(datsheet, startrow.tab)
      estgrp2.cells <- xlsx::createCell(estgrp2.row, 1:totcols)
      xlsx::setCellValue(estgrp2.cells[[1]], grp2)

      ## Get heading for row group
      if (is.null(rowgrpnm)) rowgrpnm <- names(esttab)[2]

      rowgrps <- unique(estgrp2[[rowgrpnm]])
      rowgrps <- rowgrps[rowgrps != "Total"]

      for (j in 1:length(rowgrps)) {
        ## Set style for subtotal rows, if TRUE          
        grp <- rowgrps[j]
        print(grp)
        estgrp <- estgrp2[estgrp2[[rowgrpnm]] == grp,]
        psegrp <- psegrp2[psegrp2[[rowgrpnm]] == grp,]

        norname <- ifelse (all(estgrp[,3] == ""), TRUE, FALSE) 
        nbrgrps <- 2      
 
        ## Create new row and cells for group heading and set heading
        ################################################################
        startrow.tab <- startrow.tab + 1
        estgrp.row <- xlsx::createRow(datsheet, startrow.tab)
        estgrp.cells <- xlsx::createCell(estgrp.row, 1:totcols)
        xlsx::setCellValue(estgrp.cells[[nbrgrps]], grp)
 
        if (rowgrp2.subtot && !is.null(subtotvals) && grp %in% subtotvals)
            startrow.tab <- startrow.tab + 1
        endrow.tab <- startrow.tab + nrow(estgrp)-1
 
        ## Set cell values
        ###########################################################################
        setCells(datsheet=datsheet, estgrp=estgrp, psegrp=psegrp, nbrgrps=nbrgrps, 
		startrow=startrow.tab, endrow=NULL, totcols=totcols, estcols=estcols, 
		psecols=psecols, esttab.style=esttab.style, psetab.style=psetab.style, 
		rnames=rnames, norname=norname, allin1=allin1, addSEcol=addSEcol)
        ###########################################################################
 
        if (rowgrp.subtot) {
          if (is.null(subtotvals) || (!is.null(subtotvals) & grp %in% subtotvals)) {

            ## Add style to subtotal
            subtot.row <- xlsx::getRows(datsheet, endrow.tab)
            subtot.cells <- xlsx::getCells(subtot.row, estcols)
            lapply(subtot.cells, function(x) xlsx::setCellStyle(x, subtot.esttab.style))

            subtot.rname.cell <- xlsx::getCells(subtot.row, nbrgrps+1)
            lapply(subtot.rname.cell, 
			function(x) xlsx::setCellStyle(x, subtot.rname.style))
          
            if (addSEcol) {
              ## Add style to subtotal
              subtot.cells <- xlsx::getCells(subtot.row, psecols)
              lapply(subtot.cells, 
				function(x) xlsx::setCellStyle(x, subtot.psetab.style))
            }
          }
        }
        startrow.tab <- endrow.tab
      }
 
      if (rowgrp2.subtot) {

        ## Row group2 subtotals
        ##########################################################################
        startrow.tab <- startrow.tab + 1
        grptot.rows <- xlsx::createRow(datsheet, startrow.tab)
        grptot.cells <- xlsx::createCell(grptot.rows, colIndex=1:totcols)

        ## Merge total row name cells
        xlsx::addMergedRegion(datsheet, startRow=startrow.tab, endRow=startrow.tab, 
			startColumn=1, endColumn=3)
 
        ## Add group2 subtotal row name and values
        if (allin1) {
          grpsubtotest.val <- mapply(wrapSE, estgrp2[nrow(estgrp2), estcols])
        } else {
          grpsubtotest.val <- as.numeric(estgrp2[nrow(estgrp2), estcols])
        }

        grpsubtot.txt <- paste("Total", tolower(grp2))
        value.cells <- mapply(xlsx::setCellValue, grptot.cells[1], grpsubtot.txt)
        value.cells <- mapply(xlsx::setCellValue, grptot.cells[estcols[length(estcols)]],
 			grpsubtotest.val)
        
        if (addSEcol) {
          grpsubtotpse.val <- as.numeric(psegrp2[nrow(psegrp2), ncols])
          value.cells <- mapply(xlsx::setCellValue, grptot.cells[psecols[length(psecols)]],
 			grpsubtotpse.val)
        }

        ## Set style for row group2 subtotal
        ########################################################################
        style.cells <- lapply(grptot.cells[colIndex.txt], 
			function(x) xlsx::setCellStyle(x, rowtotc1.style))
        style.cells <- lapply(grptot.cells[colStart.nbr:totcols], 
			function(x) xlsx::setCellStyle(x, rowtot.style))

      }

      xlsx::setColumnWidth(datsheet, colIndex=1:2, colWidth=cellWidth.grp)
      xlsx::autoSizeColumn(datsheet, colIndex=3)
    }
 
  } else if (rowgrp) {   ## rowgrp2 = FALSE
    ## Get heading for row group
    if (is.null(rowgrpnm)) rowgrpnm <- names(esttab)[1]

    rowgrps <- unique(esttab[[rowgrpnm]])
    rowgrps <- rowgrps[rowgrps != rowtottxt]

    for (j in 1:length(rowgrps)) {
      ## Set style for subtotal rows, if TRUE          
      grp <- rowgrps[j]
      print(grp)
      estgrp <- esttab[esttab[[rowgrpnm]] == grp,]
      psegrp <- psetab[psetab[[rowgrpnm]] == grp,]
      norname <- ifelse (all(estgrp[,2] == ""), TRUE, FALSE) 
      nbrgrps <- 1      

      ## Create new row and cells for group heading and set heading
      ################################################################
      startrow.tab <- startrow.tab + 1
      estgrp.row <- xlsx::createRow(datsheet, startrow.tab)
      estgrp.cells <- xlsx::createCell(estgrp.row, 1:totcols)
      xlsx::setCellValue(estgrp.cells[[nbrgrps]], grp)

      startrow.tab <- startrow.tab + 1
      endrow.tab <- startrow.tab + nrow(estgrp)-1

      ## Set cell values
      ###########################################################################
      setCells(datsheet=datsheet, estgrp=estgrp, psegrp=psegrp, nbrgrps=nbrgrps,
 		startrow=startrow.tab, endrow=NULL, totcols=totcols, estcols=estcols, 
		psecols=psecols, esttab.style=esttab.style, psetab.style=psetab.style, 
		rnames=rnames, norname=norname, allin1=allin1, addSEcol=addSEcol)
      ########################################################################### 

      if (rowgrp.subtot) {
        if (is.null(subtotvals) || (!is.null(subtotvals) & grp %in% subtotvals)) {
          ## Add style to subtotal
          subtot.row <- xlsx::getRows(datsheet, endrow.tab)
          subtot.cells <- xlsx::getCells(subtot.row, estcols)
          lapply(subtot.cells, function(x) xlsx::setCellStyle(x, subtot.esttab.style))

          subtot.rname.cell <- xlsx::getCells(subtot.row, nbrgrps+1)
          lapply(subtot.rname.cell, 
			function(x) xlsx::setCellStyle(x, subtot.rname.style))
          
          if (addSEcol) {
            ## Add style to subtotal
            subtot.cells <- xlsx::getCells(subtot.row, psecols)
            lapply(subtot.cells, 
				function(x) xlsx::setCellStyle(x, subtot.psetab.style))
          }
        }
      }
      startrow.tab <- endrow.tab
    }
 
    ## Grand total for all groups
    ##########################################################################
    startrow.tab <- startrow.tab + 1
    tot.rows <- xlsx::createRow(datsheet, startrow.tab)
    tot.cells <- xlsx::createCell(tot.rows, colIndex=1:totcols)

    ## Merge total row name cells
    xlsx::addMergedRegion(datsheet, startRow=startrow.tab, endRow=startrow.tab, 
			startColumn=1, endColumn=length(rnames))

    ## Add group2 subtotal row name and values
    tot.vals <- esttab[nrow(esttab), tabcols.nbr]
 
    if (!is.null(rowtottxt)) tot.txt <- "Total"
    value.cells <- mapply(xlsx::setCellValue, tot.cells[1], tot.txt)

    if (allin1) {
      tot.vals <- mapply(wrapSE, tot.vals)
    } else {
      tot.vals <- as.numeric(tot.vals)
    }
    value.cells <- mapply(xlsx::setCellValue, tot.cells[estcols], tot.vals)

    style.cells <- lapply(tot.cells[estcols], 
			function(x) xlsx::setCellStyle(x, esttab.style))

    ## Change null values, 0 values, and NA values of est cells to --
    lapply(tot.cells[estcols], function(x) {
      cellval <- xlsx::getCellValue(x)
      if (is.null(cellval) | cellval == 0 | is.na(cellval))  xlsx::setCellValue(x, "--")
    })

    ## Set style for row group total
    ########################################################################
    style.cells <- lapply(tot.cells[colIndex.txt], 
			function(x) xlsx::setCellStyle(x, rowtotc1.style))
    #style.cells <- lapply(tot.cells[colStart.nbr:totcols], 
#			function(x) xlsx::setCellStyle(x, rowtot.style))

    if (addSEcol) {
      totvals.pse <- suppressWarnings(as.numeric(psetab[psetab[,1] == rowtottxt, 
		min(colIndex.nbr):ncol(psetab)]))
      if (allin1) totvals.pse <- mapply(wrapSE, totvals.pse)
      value.cells <- mapply(xlsx::setCellValue, tot.cells[psecols], totvals.pse)
      style.cells <- lapply(tot.cells[psecols], 
			function(x) xlsx::setCellStyle(x, psetab.style))

      ## Change null values, 0 values, and NA values of pse cells to --
      lapply(tot.cells[psecols], function(x) {
        cellval <- xlsx::getCellValue(x)
        if (is.null(cellval) | cellval == 0 | is.na(cellval))  xlsx::setCellValue(x, "--")
      })
    }

    xlsx::setColumnWidth(datsheet, colIndex=1, colWidth=6)
    xlsx::autoSizeColumn(datsheet, colIndex=2)
 
  } else {     ## rowgrp2=FALSE, rowgrp=FALSE

    startrow.tab <- startrow.tab + 1
    endrow.tab <- startrow.tab + nrow(esttab)-1

    ## Set cell values
    ###########################################################################
    setCells(datsheet=datsheet, estgrp=esttab, psegrp=psetab, nbrgrps=0,
 		startrow=startrow.tab, totcols=totcols, estcols=estcols, 
		psecols=psecols, esttab.style=esttab.style, psetab.style=psetab.style, 
		rnames=rnames, norname=TRUE, allin1=allin1, addSEcol=addSEcol)
    ########################################################################### 
    startrow.tab <- endrow.tab
    xlsx::autoSizeColumn(datsheet, colIndex=colIndex.txt)
  }

  ## Set styles for data table values
  xlsx::setColumnWidth(datsheet, colIndex=colIndex.nbr, colWidth=cellwidth)

  if (addSEcol) {
    if (is.null(cellwidthSE)) cellwidthSE=cellwidth
    xlsx::setColumnWidth(datsheet, colIndex=psecols, colWidth=cellwidthSE)
  } 

  ################################################
  ## Total row 
  ################################################
  if (rowtotal) {
    startrow.tot <- startrow.tab

    ## Create row and cells for total
    #########################################################################
    rowtot.row <- xlsx::getRows(datsheet, startrow.tot)
    rowtot.cells.est <- xlsx::getCells(rowtot.row, colIndex=estcols)
    rowtot.cells.c1 <- xlsx::getCells(rowtot.row, colIndex=colIndex.txt)

    ## Add total row names
    #########################################################################
    style.cells <- lapply(rowtot.cells.c1, 
				function(x) xlsx::setCellStyle(x, rowtotc1.style))

    ## Add total data
    #########################################################################
    style.cells <- lapply(rowtot.cells.est, 
		function(x) xlsx::setCellStyle(x, rowtot.style))

    if (addSEcol) {
      rowtot.cells.pse <- xlsx::getCells(rowtot.row, colIndex=psecols)
      style.cells <- lapply(rowtot.cells.pse, 
		function(x) xlsx::setCellStyle(x, rowtotSE.style))
    }
  }

  if (is.null(footnote3)) footnote4 <- footnote3
  if (!is.null(footnote1)) {
    startrow.footnote1 <- startrow.tab + 1

    ## Set style for title
    footnote.style <- xlsx::CellStyle(wb) + xlsx::Alignment(wrapText=TRUE) + 
		xlsx::Font(wb, heightInPoints=8) 

    ## Create row and cells for footnote1
    #########################################################################
    footnote1.row <- xlsx::createRow(datsheet, startrow.footnote1)
    footnote1.cells <- xlsx::createCell(footnote1.row, colIndex=1:totcols)

    ## Add title to workbook and set style
    xlsx::setCellValue(footnote1.cells[[1]], footnote1)
    xlsx::setCellStyle(footnote1.cells[[1]], footnote.style) 
  
    ## Merge cells of footnote
    xlsx::addMergedRegion(datsheet, startRow=startrow.footnote1, 
		endRow=startrow.footnote1, startColumn=1, endColumn=totcols)

    if (!is.null(footnote2)) {
      startrow.footnote2 <- startrow.footnote1 + 1

      ## Create row and cells for footnote1
      #########################################################################
      footnote2.row <- xlsx::createRow(datsheet, startrow.footnote2)
      footnote2.cells <- xlsx::createCell(footnote2.row, colIndex=1:totcols)

      ## Add title to workbook and set style
      xlsx::setCellValue(footnote2.cells[[1]], footnote2)
      xlsx::setCellStyle(footnote2.cells[[1]], footnote.style) 
  
      ## Merge cells of footnote
      xlsx::addMergedRegion(datsheet, startRow=startrow.footnote2, 
		endRow=startrow.footnote2, startColumn=1, endColumn=totcols)
   
      if (!is.null(footnote3)) {
        startrow.footnote3 <- startrow.footnote2 + 1

        ## Create row and cells for footnote1
        #########################################################################
        footnote3.row <- xlsx::createRow(datsheet, startrow.footnote3)
        footnote3.cells <- xlsx::createCell(footnote3.row, colIndex=1:totcols)

        ## Add title to workbook and set style
        xlsx::setCellValue(footnote3.cells[[1]], footnote3)
        xlsx::setCellStyle(footnote3.cells[[1]], footnote.style) 
  
        ## Merge cells of footnote
        xlsx::addMergedRegion(datsheet, startRow=startrow.footnote3, 
		endRow=startrow.footnote3, startColumn=1, endColumn=totcols)

        if (!is.null(footnote4)) {
          startrow.footnote4 <- startrow.footnote3 + 1

          ## Create row and cells for footnote1
          #######################################################################
          footnote4.row <- xlsx::createRow(datsheet, startrow.footnote4)
          footnote4.cells <- xlsx::createCell(footnote4.row, colIndex=1:totcols)

          ## Add title to workbook and set style
          xlsx::setCellValue(footnote4.cells[[1]], footnote4)
          xlsx::setCellStyle(footnote4.cells[[1]], footnote.style) 
  
          ## Merge cells of footnote
          xlsx::addMergedRegion(datsheet, startRow=startrow.footnote4, 
			endRow=startrow.footnote4, startColumn=1, endColumn=totcols)
        }
      }
    }
  }
  if (autosize)
    xlsx::autoSizeColumn(datsheet, colIndex=1:totcols) 
    
  if (savewb) {
    ## GET NAME FOR WORKBOOK
    ###############################################################
    #if (newwb) {
    #  #outallin1base <- paste0(outfn, "_", format(Sys.time(), "%Y%m%d"))
    #  outallin1fn <- fileexistsnm(outfolder, outfn, "xlsx")
    #  outfilenm <- paste0(outfolder, "/", outallin1fn, ".xlsx")
    #} else {
    #  outfilenm <- wbnm
    #}

    ## SAVE EXCEL WORKBOOK
    ###############################################################
    #xlsx::saveWorkbook(wb, outfilenm)  
    xlsx::saveWorkbook(wb, wbnm)
    
    cat(
    " ###############################################################################", 
    "\n", paste("Table written to: "), "\n", paste(" ", wbnm), "- sheet", sheetnm, "\n", 
    "###############################################################################",
    "\n" )
  }
}


