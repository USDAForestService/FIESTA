anSApop_report <- function(SApopdat, AOInm, pcfilter=NULL, fortypgrpcd=NULL, 
	title.ref=NULL, domain="DOMAIN", estnm="JFH", totals=TRUE, outfolder=NULL) {
  ## DESCRIPTION: Creates a report using Rmarkdown 
  ## 		Adds a folder named report in the outfolder and copies all 
  ##		components of report into folder.

  wkdir <- getwd()

  ## set global variables
  report_imagefn <- NULL

  ## Check SApopdat
  SApopdat <- FIESTA::pcheck.object(SApopdat, "SApopdat", 
		list.items=c("treex", "seedx"))

  ## Check estnm
  if (is.null(estnm)) {
    stop("must include estnm")
  }

  ## Check totals
  totals <- FIESTA::pcheck.logical(totals, varnm="totals", title="Totals?", first="YES", gui=gui)

  ## Check outfolder 
  outfolder <- pcheck.outfolder(outfolder)

  ## Create temporary folder for report
  reportfolder <- tempdir()
  reportrmd <- system.file("rmd", "anSApop_report.Rmd", package="FIESTA")

  ## TESTING
  #reportrmd <- "C:/_tsf/_GitHub/FIESTA/inst/rmd/anSApop_report.Rmd"

  #if (!dir.exists(reportfolder)) {
  #  message(reportfolder, " does not exist, creating...")
  #  dir.create(reportfolder)
  #}

  ref_fortypgrp <- FIESTA::ref_codes[FIESTA::ref_codes$VARIABLE == "FORTYPGRPCD", ]
  ftypgrplst <- table(SApopdat$pltcondx$FORTYPGRPCD)
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
  file.copy(system.file("rmd", "anGBtemplate.docx", package="FIESTA"),
	file.path(reportfolder, "anGBtemplate.docx"), overwrite=TRUE)

  ## Set working directory to reportfolder
  setwd(reportfolder) 

  test <- tryCatch(
  	rmarkdown::render(
    		input = rmdfn,
    		output_file = reportfn,
    		params = list(SApopdat=SApopdat, AOInm=AOInm, 
		pcfilter=pcfilter, fortypgrpcd=fortypgrpcd, title.ref=title.ref,
		domain=domain, estnm=estnm, totals=totals),
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
