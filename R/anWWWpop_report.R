#' ANALYSIS - Generate a report small area population.
#' 
#' Generates a standard report based on area of interest.
#' 
#' If variables are NULL, then it will prompt user to input variables.
#' 
#' @param SApopdatlst List object. Population data from modSApop().
#' @param SApackage String. Small area package to use ('JoSAE', 'sae')
#' @param SAmethod String. Small area method to use ('unit', 'area')
#' @param AOInm String. Name of area of interest.
#' @param pcfilter String. Name of variable or string to filter plots or
#' conditions.  Must be in R syntax (e.g., ADFORCD == 408, COUNTYCD == 1).
#' Optional.
#' @param fortypgrpcd Integer. A forest type group code to use as focus in the
#' report.
#' @param title.ref String. Reference information for title and output file
#' names.
#' @param totals Logical. If TRUE, per acre estimates are multiplied by acres.
#' @param outfolder String. The path of folder to output tables.
#' @author Tracey S. Frescino
#' @keywords data
#' @export
anWWWpop_report <- function(WWWpopdat, 
                           SApackage,
                           SAmethod,
                           AOInm, 
                           fortypgrpcd = NULL, 
                           title.ref = NULL, 
                           totals = TRUE, 
                           outfolder = NULL) {
  ## DESCRIPTION: Creates a report using Rmarkdown 
  ## 		Adds a folder named report in the outfolder and copies all 
  ##		components of report into folder.

  
  ##################################################################
  ## CHECK PARAMETER INPUTS
  ##################################################################
  wkdir <- getwd()
  
  ## set global variables
  report_imagefn <- NULL
  gui <- FALSE
  

  ## Check SApackage 
  SApackagelst <- c("JoSAE", "sae", "hbsae")
  SApackage <- pcheck.varchar(var2check=SApackage, varnm="SApackage", gui=gui, 
                              checklst=SApackagelst, caption="SApackage", multiple=FALSE, stopifnull=TRUE)
  
  ## Check for JoSAE library
  if (SApackage == "JoSAE") {
    if (!"JoSAE" %in% rownames(installed.packages())) {
      message("SApackage JoSAE requires package JoSAE")
    }
  } else {
    if (!"sae" %in% rownames(installed.packages())) {
      message("SApackage sae requires package sae")
    }
  }
  
  ## Check SAmethod 
  SAmethodlst <- c("unit", "area")
  SAmethod <- pcheck.varchar(var2check=SAmethod, varnm="SAmethod", gui=gui, 
                             checklst=SAmethodlst, caption="SAmethod", multiple=FALSE, stopifnull=TRUE)
  
  
  ## Check totals
  totals <- pcheck.logical(totals, varnm="totals", title="Totals?", first="YES")

  ## Check outfolder 
  outfolder <- pcheck.outfolder(outfolder)

  ## Create temporary folder for report
  reportfolder <- tempdir()
  reportrmd <- system.file("rmd", "anSApop_report.Rmd", package="FIESTAnalysis")

  ## TESTING
  #reportrmd <- "C:/_tsf/_GitHub/FIESTA/inst/rmd/anSApop_report.Rmd"

  #if (!dir.exists(reportfolder)) {
  #  message(reportfolder, " does not exist, creating...")
  #  dir.create(reportfolder)
  #}
  
  ref_fortypgrp <- FIESTAutils::ref_codes[FIESTAutils::ref_codes$VARIABLE == "FORTYPGRPCD", ]

  ftypgrpdf <- {}
  for (i in 1:length(SApopdatlst)) {
    #SApopdatlst[[i]]$treex
    ftypgrpdf <- rbind(ftypgrpdf, SApopdatlst[[i]]$pltcondx[, c("PLT_CN", "FORTYPGRPCD")])
  }

  ftypgrplst <- table(ftypgrpdf$FORTYPGRPCD)
  if (!is.null(fortypgrpcd) && length(fortypgrpcd) > 0) {
    if (!fortypgrpcd %in% names(ftypgrplst)) {
      stop("FORTYPGRPCD ", fortypgrpcd, " not in popdat")
    } 
  } else {
    fortypgrpcd <- names(ftypgrplst[ftypgrplst != 999 & ftypgrplst == max(ftypgrplst)])
  }
  fortypgrpnm <- ref_fortypgrp[!is.na(ref_fortypgrp$VALUE) & 
				ref_fortypgrp$VALUE == fortypgrpcd, "MEANING"]

  ftypgrp.prop <- round(ftypgrplst[names(ftypgrplst) == fortypgrpcd] /
		sum(ftypgrplst) * 100, 2)
  message(fortypgrpnm, " is in approximately ", ftypgrp.prop, 
		" percent of plots in area of interest")
  
  ## Copy files to outfolder
  rmdfn <- file.path(reportfolder, paste0(AOInm, '_report.Rmd'))
  reportnm <- paste0(AOInm, '_report.docx')
  reportfn <- file.path(reportfolder, reportnm)
  file.copy(reportrmd, rmdfn, overwrite=TRUE)
  file.copy(system.file("rmd", "anGBtemplate.docx", package="FIESTAnalysis"),
	file.path(reportfolder, "anGBtemplate.docx"), overwrite=TRUE)

  ## Set working directory to reportfolder
  setwd(reportfolder) 

  test <- tryCatch(
  	rmarkdown::render(
    		input = rmdfn,
    		output_file = reportfn,
    		params = list(SApopdatlst=SApopdatlst, 
    		              SApackage=SApackage, SAmethod=SAmethod, 
    		              AOInm=AOInm, pcfilter=pcfilter, 
    		              fortypgrpcd=fortypgrpcd, title.ref=title.ref, 
    		              totals=totals),
    		envir = parent.frame()
  	),
	error=function(err) {
		message(err)
		return(NULL)
  } )
  if (is.null(test)) {
    setwd(wkdir) 
    stop()
  }
	

  ## Copy report from temporary folder to outfolder
  tmp <- file.copy(reportfn, outfolder, overwrite=TRUE)
  if (tmp) {
    message("saving report to ", file.path(outfolder, reportnm))
  } else {
    message("error when copying to ", outfolder)
  }

  ## Set working directory back to original working directory
  setwd(wkdir) 

}
