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
#' @param savedata_opts List. See help(savedata_options()) for a list
#' of options. Only used when savedata = TRUE. If out_layer = NULL,
#' default = 'datfreq'.
#' @param gui Logical. If TRUE, pop-up windows will appear for user-interface.
#'
#' @return \item{freqtable}{ Data frame. The frequency table. } If
#' savedata=TRUE, a comma-delimited file of the frequency table is written to
#' the outfolder.
#' @author Tracey S. Frescino
#' @keywords data
#' @examples
#' # Set up data for example
#' tab <- data.frame(cbind(CONDCLASS = c(1, 1, 2, 1, 3, 3, 3, 1, 1, 1, 2, 1), 
#' 		                     FORTYPCD = c(182, 184, 201, 221, 221, 184, 221, 182,
#' 		                                  182, 201, 182, 221)))
#' 
#' # Frequency table with "FORTYPCD"       
#' datFreq(x = tab,
#'         xvar = "FORTYPCD")
#' 		                                  
#' # Frequency table with "CONDCLASS" and "FORTYPCD" 		                                   
#' datFreq(x = tab,
#'         xvar = c("CONDCLASS", "FORTYPCD"))
#' 
#' # Frequency table with "CONDCLASS" and "FORTYPCD", adding total and subtotal
#' # rows 		                                   
#' datFreq(x = tab,
#'         xvar = c("CONDCLASS", "FORTYPCD"), 
#'         total = TRUE, 
#'         subtotal = TRUE)
#'         
#' # Frequency table for WYtree, multiple variables, subtotal options
#' datFreq(x = FIESTA::WYtree,
#'         xvar = c("SPGRPCD", "SPCD", "STATUSCD"), 
#' 	       subtotal = TRUE, subtotalcol = "SPCD")
#' @export datFreq
datFreq <- function(x, 
                    xvar = NULL, 
                    total = FALSE, 
                    subtotal = FALSE, 
                    subtotalcol = NULL,
	                  savedata = FALSE, 
                    savedata_opts = NULL, 
                    gui = FALSE){
  #####################################################################################
  ##	Generates a frequency table from a data frame, including number of records
  ##	by a specified variable or variables in the data frame with optional
  ##	totals and/or subtotals. 
  #####################################################################################

  ## Adds to file filters to Cran R Filters table.
  if (.Platform$OS.type=="windows")
    Filters=rbind(Filters,csv=c("Comma-delimited files (*.csv)", "*.csv"))
 
  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- ifelse(nargs() == 0, TRUE, FALSE)
  
  ## Set global variables
  if (gui) x=savedata=total=subtotal <- NULL
  

  ##################################################################
  ## CHECK PARAMETER NAMES
  ##################################################################
  
  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- names(formals(datFreq)) 
  if (!all(input.params %in% formallst)) {
    miss <- input.params[!input.params %in% formallst]
    stop("invalid parameter: ", toString(miss))
  }
  
  ## Check parameter lists
  pcheck.params(input.params, savedata_opts=savedata_opts)
  
  ## Set savedata defaults
  savedata_defaults_list <- formals(FIESTA::savedata_options)[-length(formals(FIESTA::savedata_options))]
  
  for (i in 1:length(savedata_defaults_list)) {
    assign(names(savedata_defaults_list)[[i]], savedata_defaults_list[[i]])
  }
  
  ## Set user-supplied savedata values
  if (length(savedata_opts) > 0) {
    if (!savedata) {
      message("savedata=FALSE with savedata parameters... no data are saved")
    }
    for (i in 1:length(savedata_opts)) {
      if (names(savedata_opts)[[i]] %in% names(savedata_defaults_list)) {
        assign(names(savedata_opts)[[i]], savedata_opts[[i]])
      } else {
        stop(paste("Invalid parameter: ", names(savedata_opts)[[i]]))
      }
    }
  }
  
  ##################################################################
  ## CHECK PARAMETER INPUTS
  ##################################################################

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
  savedata <- pcheck.logical(savedata, varnm="savedata", title="Save data table?", 
                             first="NO", gui=gui)
  
  ## Check output parameters
  if (savedata) {
    outlst <- pcheck.output(outfolder=outfolder, out_dsn=out_dsn, 
        out_fmt=out_fmt, outfn.pre=outfn.pre, outfn.date=outfn.date, 
        overwrite_dsn=overwrite_dsn, overwrite_layer=overwrite_layer,
        add_layer=add_layer, append_layer=append_layer, gui=gui)
    outfolder <- outlst$outfolder
    out_dsn <- outlst$out_dsn
    out_fmt <- outlst$out_fmt
    overwrite_layer <- outlst$overwrite_layer
    append_layer <- outlst$append_layer
    outfn.date <- outlst$outfn.date
    outfn.pre <- outlst$outfn.pre
    if (is.null(out_layer)) {
      out_layer <- "datfreq"
    }
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

 
  #### WRITE TO FILE 
  #############################################################
  if (savedata) {
    datExportData(freqtab.tot, 
            savedata_opts=list(outfolder=outfolder, 
                                out_fmt=out_fmt, 
                                out_dsn=out_dsn, 
                                out_layer=out_layer,
                                outfn.pre=outfn.pre, 
                                outfn.date=outfn.date, 
                                overwrite_layer=overwrite_layer,
                                append_layer=append_layer,
                                add_layer=TRUE))
  }

  return(freqtab.tot)
}
