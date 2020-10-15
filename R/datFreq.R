datFreq <- function(x, xvar=NULL, total=FALSE, subtotal=FALSE, subtotalcol=NULL,
	savedata=FALSE, outfolder=NULL, outfn=NULL){
  #####################################################################################
  ##	Generates a frequency table from a data frame, including number of records
  ##	by a specified variable or variables in the data frame with optional
  ##	totals and/or subtotals. 
  #####################################################################################

  ## Adds to file filters to Cran R Filters table.
  if (.Platform$OS.type=="windows")
    Filters=rbind(Filters,csv=c("Comma-delimited files (*.csv)", "*.csv"))
 

  ##################################################################
  ## CHECK INPUT PARAMETERS
  ##################################################################

  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- ifelse(nargs() == 0, TRUE, FALSE)

  if (gui) x=savedata=total=subtotal <- NULL

  ## Check x
  datx <- FIESTA::pcheck.table(x, gui=gui, tabnm="x", caption="Table with variable(s)?",
		stopifnull=TRUE)

  ## Check xvar
  xvar <- FIESTA::pcheck.varchar(var2check=xvar, varnm="xvar", gui=gui, 
		checklst=names(datx), caption="X Variable", multiple=TRUE, 
		stopifnull=TRUE)

  ## Check total and subtotal
  total <- FIESTA::pcheck.logical(total, varnm="total", title="Add totals?", 
		first="YES", gui=gui)
  subtotal <- FIESTA::pcheck.logical(subtotal, varnm="subtotal", 
		title="Add subtotals?", first="YES", gui=gui)

  if (subtotal) {
    ## Check subtotal columns
    if (!all(subtotalcol %in% names(x))) {
      misscol <- subtotalcol[!subtotalcol %in% names(x)] 
      warning("invalid subtotalcol: ", toString(misscol))
      subtotalcol <- NULL
    }
  }

  ## Check savedata 
  savedata <- FIESTA::pcheck.logical(savedata, varnm="savedata", "Save data tables?", 
		first="NO", gui=gui)

  ## Check outfolder
  ###########################################################
  if (savedata) {
    outfolder <- FIESTA::pcheck.outfolder(outfolder, gui=gui)
    if (is.null(outfn) || gsub(" ", "", outfn) == "")
      outfn <- "Freq"

    freqfn <- FIESTA::fileexistsnm(outfolder, outfn, "csv")
    freqfnout <- paste0(outfolder, "/", freqfn, ".csv")
  }

  ################################################################################  
  ### DO WORK
  ################################################################################  
  yvar <- "FREQ"

  ## GET FREQUENCY BY PLOT
  freqtab <- datx[, .N, by=xvar]
  setnames(freqtab, "N", yvar)
  setorderv(freqtab, xvar)

  freqtab <- setDF(freqtab)

  ## SORT TABLE BY THE FIRST VARIABLE IN LIST
  numxvar <- length(xvar)

  if ((subtotal | total)) {
    if (is.null(subtotalcol))
      subtotalcol <- names(x)[ncol(x)-1]

    freqtab.tot <- data.frame()
    tot <- sum(freqtab[["FREQ"]])

    for (i in length(subtotalcol)) {
      varx <- subtotalcol[i]
      vals <- unique(freqtab[[varx]])
      if (length(vals) > 1) {
        for (val in vals) {
          freqtab.tot <- rbind(freqtab.tot, freqtab[freqtab[[varx]] == val,])

          if (subtotal)
            freqtab.tot <- rbind(freqtab.tot, 
				c(as.vector(paste(rep("", ncol(freqtab) - 2))),
        			"Subtotal", sum(freqtab[freqtab[[varx]] == val, "FREQ"])))
        }
      } else {
        freqtab.tot <- freqtab
      }
      if (total) 
        freqtab.tot <- rbind(freqtab.tot, 
			c(as.vector(paste(rep("", ncol(freqtab) - 2))), "Total", tot))
    }
  } else {
    freqtab.tot <- freqtab
  }

  ## WRITE TO FILE
  ###########################################
  if (savedata)
    ## WRITE TO FILE
    FIESTA::write2csv(freqtab.tot, outfilenm=freqfnout)

  return(freqtab.tot)
}
