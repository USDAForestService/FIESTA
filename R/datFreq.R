#' Data - Get frequency table for specified variable(s).
#' 
#' Generates a frequency table from a data frame, including number of records
#' by a specified variable or variables in the data frame with optional totals
#' and/or subtotals.
#' 
#' If no parameters, then user is prompted for input. If partial parameters,
#' default parameter values are used.
#' 
#' @param x Data frame or comma-delimited file (*.csv). The table with the
#' variable(s).
#' @param xvar String (vector).* The name of the variable(s) to summarize.
#' @param total Logical. If TRUE, a row is added to bottom of table with a
#' total for the whole table.
#' @param subtotal Logical. If TRUE, a row is added to bottom of each section
#' for subtotals.
#' @param subtotalcol Logical. If subtotal=TRUE, the column(s) to generate
#' subtotals.
#' @param savedata Logical. If TRUE, writes output data to outfolder.
#' @param outfolder String. The name of the output folder, if savedata=TRUE.
#' @param outfn String. The name of the output file if savedata=TRUE (*.csv).
#' Do not include extension. If NULL, the file will be named Freq_'date'.csv
#' @return \item{freqtable}{ Data frame. The frequency table. } If
#' savedata=TRUE, a comma-delimited file of the frequency table is written to
#' the outfolder.
#' @author Tracey S. Frescino
#' @keywords data
#' @examples
#' 
#' 	tab <- data.frame(cbind(	CONDCLASS=c(1,1,2,1,3,3,3,1,1,1,2,1), 
#' 		FORTYPCD=c(182,184,201,221,221,184,221,182,182,201,182,221)))
#' 	datFreq(x=tab, xvar=c("CONDCLASS", "FORTYPCD"))
#' 	datFreq(x=tab, xvar=c("CONDCLASS", "FORTYPCD"), total=TRUE, subtotal=TRUE)
#' 	datFreq(x=tab, xvar="FORTYPCD")
#' 
#'  	datFreq(x=FIESTA::WYtree, xvar=c("SPGRPCD", "SPCD", "STATUSCD"), 
#' 		subtotal=TRUE, subtotalcol="SPCD")
#' 
#' @export datFreq
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
  datx <- pcheck.table(x, gui=gui, tabnm="x", caption="Table with variable(s)?",
		stopifnull=TRUE)

  ## Check xvar
  xvar <- pcheck.varchar(var2check=xvar, varnm="xvar", gui=gui, 
		checklst=names(datx), caption="X Variable", multiple=TRUE, 
		stopifnull=TRUE)

  ## Check total and subtotal
  total <- pcheck.logical(total, varnm="total", title="Add totals?", 
		first="YES", gui=gui)
  subtotal <- pcheck.logical(subtotal, varnm="subtotal", 
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
  savedata <- pcheck.logical(savedata, varnm="savedata", "Save data tables?", 
		first="NO", gui=gui)

  ## Check outfolder
  ###########################################################
  if (savedata) {
    outfolder <- pcheck.outfolder(outfolder, gui=gui)
    if (is.null(outfn) || gsub(" ", "", outfn) == "")
      outfn <- "Freq"

    freqfn <- fileexistsnm(outfolder, outfn, "csv")
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
      subtotalcol <- names(freqtab)[ncol(freqtab)-1]

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
    write2csv(freqtab.tot, outfilenm=freqfnout)

  return(freqtab.tot)
}
