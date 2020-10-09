save1tab <- function(tab, tab.title=NULL, outfolder, allin1=TRUE, coltitlerow=TRUE, 
	coltitle=NULL, addtitle=TRUE, rowtotal=TRUE, rnames=NULL, outfn=NULL, 
	addformat=TRUE, charvars=NULL, outfn.date=TRUE, overwrite=FALSE, cols2format=NULL){

  ## DESCRIPTION: To save 1 table to a comma-delimited file (*.csv).
  ## ARGUMENTS:
  ## tab  	- Data frame. Table of estimates and standard errors.
  ## tab.title	- String. The title for the table of estimates and standard errors.
  ## outfolder  	- String. The name of the output folder.
  ## allin1    	- Logical. If TRUE, the table has both estimates and standard errors in 1 table.
  ## coltitlerow	- Logical. If TRUE, add a row on the top of the table with a title
  ##			for the columns. 
  ## addtitle 	- Logical. If TRUE, a title is added to the file.
  ## rowtotal	- Logical. If TRUE, there is a column at the end of table with row totals.
  ## col1varnm	- String. The name of the first column variable of the table.
  ## outfn	- String. The name of the output file (no extension).
  ## charvars	- String vector. The variable names in the table that are character. 
  ##			These variables will not be formatted with commas.

  ## Make sure tab is a data.frame
  if ("data.table" %in% class(tab)) {
    tab.names <- names(tab)
    tab <- data.frame(tab)
    names(tab) <- tab.names
  }
 
  ## Default title if tab.title = NULL.
  if (is.null(tab.title))
    tab.title <- "Estimates and standard error"

  ## REMOVE COLUMNS WITH 0 VALUES.
  #if(rowtotal){
  #  tab <- tab[, tab[tab[,1] == "Total",] != 0]
  #}

  ## Get column names of tab
  tabnames <- names(tab)

  ## Get number of row names
  rnbr <- length(rnames)


  ## FORMAT TABLE VALUES
  ###########################################
  if (!allin1 && addformat) {
    if (is.null(charvars)) 
      charvars <- names(tab)[which(sapply(tab, mode) == "character")]
    
    if (!is.null(cols2format) && !all(cols2format %in% names(tab))) {
      message(cols2format, " not in tab")
      cols2format <- NULL
    }

    if (!is.null(charvars)) {
      if (all(charvars %in% tabnames)){
        cols <- which(tabnames %in% charvars)
      } else {
        colsnotin <- which(!tabnames %in% charvars)
        warning(paste("check charvars:", paste(colsnotin, collapse=", ")))
      }
    } else {
      cols <- NULL
    }

    if (!is.null(cols2format))
      cols <- unique(c(cols, which(!tabnames %in% cols2format)))

    ## FORMAT TABLES AND ADD TITLE
    #tab[,-c(1,cols)] <- noquote(apply(tab[,-c(1,cols)], 2, 
    #      function(x){format(as.numeric(x), big.mark=",")}))
    #tab[,-c(1,cols)] <- unlist(noquote(lapply(tab[,-c(1,cols)], 
    #      function(x){format(as.numeric(x), big.mark=",")})))
    tab[,-cols] <- unlist(noquote(lapply(tab[,-cols], 
          function(x){format(as.numeric(x), big.mark=",")})))
  }


  ## ADD TITLE AND FORMAT TABLE
  ###########################################
  if (coltitlerow) {
    ## ADD TITLE TO TABLE COLUMNS
    tab <- rbind(rownames = colnames(tab), tab)
    colnames(tab) <- c(rep(" ", rnbr), coltitle, rep(" ", ncol(tab)-rnbr-1))
  } else {
    tab <- tab
  }

  ## WRITE TO CSV FILE
  outfilenm <- getoutfn(outfn=outfn, outfolder=outfolder, 
	outfn.date=outfn.date, overwrite=overwrite, ext="csv")
 
  if (addtitle) {
    outfile <- file(outfilenm, "w")
    cat(gsub(",", "; ", tab.title), file=outfile, sep="\n")
    cat("\n", file=outfile)
#    outfile <- write2csv(tab, outfile=outfile, appendfile=TRUE, 
#		outfn.date=outfn.date, overwrite=overwrite, closefn=FALSE)
    outfile <- write2csv(tab, outfile=outfile, appendfile=TRUE, closefn=FALSE)
  } else { 
#    outfile <- write2csv(tab, outfilenm=outfilenm, outfn.date=outfn.date, 
#		overwrite=overwrite, closefn=FALSE) 
    outfile <- write2csv(tab, outfilenm=outfilenm, closefn=FALSE) 
  }
  close(outfile)

  message("###################################\n", 
			"Table written to: ", outfilenm, 
		"\n###################################")
}


