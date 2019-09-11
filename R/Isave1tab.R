save1tab <- function(estpse, title.estpse=NULL, outfolder, allin1=TRUE, coltitlerow=TRUE, 
	coltitle=NULL, addtitle=TRUE, rowtotal=TRUE, rnames=NULL, outfn.estpse=NULL, 
	addformat=TRUE, charvars=NULL, outfn.date=TRUE, overwrite=FALSE){

  ## DESCRIPTION: To save 1 table to a comma-delimited file (*.csv).
  ## ARGUMENTS:
  ## estpse  	- Data frame. Table of estimates and standard errors.
  ## title.estpse	- String. The title for the table of estimates and standard errors.
  ## outfolder  	- String. The name of the output folder.
  ## allin1    	- Logical. If TRUE, the table has both estimates and standard errors in 1 table.
  ## coltitlerow	- Logical. If TRUE, add a row on the top of the table with a title
  ##			for the columns. 
  ## addtitle 	- Logical. If TRUE, a title is added to the file.
  ## rowtotal	- Logical. If TRUE, there is a column at the end of table with row totals.
  ## col1varnm	- String. The name of the first column variable of the table.
  ## outfn.estpse	- String. The name of the output file (no extension).
  ## charvars	- String vector. The variable names in the table that are character. 
  ##			These variables will not be formatted with commas.

  ## Make sure estpse is a data.frame
  if ("data.table" %in% class(estpse)) {
    estpse.names <- names(estpse)
    estpse <- data.frame(estpse)
    names(estpse) <- estpse.names
  }

  ## Default title if title.estpse = NULL.
  if (is.null(title.estpse))
    title.estpse <- "Estimates and standard error"

  ## REMOVE COLUMNS WITH 0 VALUES.
  #if(rowtotal){
  #  estpse <- estpse[, estpse[estpse[,1] == "Total",] != 0]
  #}

  ## Get column names of tab
  tabnames <- names(estpse)

  ## Get number of row names
  rnbr <- length(rnames)


  ## FORMAT TABLE VALUES
  ###########################################
  if (!allin1 && addformat) {
    if (is.null(charvars)) 
      charvars <- names(estpse)[which(sapply(estpse, mode) == "character")]
    
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
    ## FORMAT TABLES AND ADD TITLE
    #estpse[,-c(1,cols)] <- noquote(apply(estpse[,-c(1,cols)], 2, 
    #      function(x){format(as.numeric(x), big.mark=",")}))
    estpse[,-c(1,cols)] <- unlist(noquote(lapply(estpse[,-c(1,cols)], 
          function(x){format(as.numeric(x), big.mark=",")})))
  }

  ## ADD TITLE AND FORMAT TABLE
  ###########################################
  if (coltitlerow) {
    ## ADD TITLE TO TABLE COLUMNS
    tab <- rbind(rownames = colnames(estpse), estpse)
    colnames(tab) <- c(rep(" ", rnbr), coltitle, rep(" ", ncol(tab)-rnbr-1))
  } else {
    tab <- estpse
  }


  ## WRITE TO CSV FILE
  if (outfn.date)
    outfn.estpse <- paste0(outfn.estpse, "_", format(Sys.time(), "%Y%m%d"))
  if (!overwrite)
    outfn.estpse <- FIESTA::fileexistsnm(outfolder, outfn.estpse, "csv")
  outfilenm <- paste0(outfolder, "/", outfn.estpse, ".csv")

  if (addtitle) {
    outfile <- file(outfilenm, "w")
    cat(gsub(",", "; ", title.estpse), file=outfile, sep="\n")
    cat("\n", file=outfile)
    outfile <- FIESTA::write2csv(tab=tab, outfile=outfile, appendfile=TRUE, closefn=FALSE)
  } else { 
    outfile <- write2csv(tab=tab, outfilenm=outfilenm, closefn=FALSE) 
  }
  close(outfile)

  cat(
  " ###########################################################################################", 
  "\n", paste("Table written to: "), "\n", paste(" ", outfilenm), "\n", 
  "###########################################################################################",
  "\n" )

}


