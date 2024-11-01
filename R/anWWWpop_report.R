anWWWpop_report <- function(WWWpopdat,
                            WWWest,
                            dbconn,
                            outfolder = NULL) {
  ## DESCRIPTION: Creates a report using Rmarkdown 
  ## 		Adds a folder named report in the outfolder and copies all 
  ##		components of report into folder.
  
  ##################################################################
  ## CHECK PARAMETER INPUTS
  ##################################################################
  wkdir <- getwd()
  
  ## set global variables
  report_imagefn=fortypgrpcd <- NULL
  gui <- FALSE
  addfortypgrp <- FALSE
  
  AOInm <- WWWpopdat$reportdata$AOI_table_name
  AOI_domain_units <- WWWpopdat$reportdata$AOI_domain_units
  
  # create domain_unit_lut
  ref_domain_unit <- WWWpopdat$reportdata$ref_domain_unit
  display_dunit_nm <- DBI::dbGetQuery(dbconn,
                                      paste0("SELECT domain_unit_name
                                              FROM ref_boundary
                                              WHERE table_name = ", "'", AOInm, "'"))[[1]]
  
  aoi.filt <- getfilter(ref_domain_unit, AOI_domain_units, syntax = 'sql')
  
  domain_unit_lut <- DBI::dbGetQuery(dbconn,
                                     paste0("SELECT ", toString(c(ref_domain_unit, display_dunit_nm)),
                                            " FROM ", AOInm,
                                            " WHERE ", aoi.filt))
                      
  
  ## Check outfolder 
  outfolder <- pcheck.outfolder(outfolder)

  ## Create temporary folder for report
  reportfolder <- tempdir()
  reportrmd <- system.file("rmd", "anWWWpop_report.Rmd", package="FIESTA")

  ## To add a focus on a fortypgrpcd
  if (addfortypgrp) {
    ref_fortypgrp <- FIESTAutils::ref_codes[FIESTAutils::ref_codes$VARIABLE == "FORTYPGRPCD", ]

    ftypgrpdf <- {}
    for (i in 1:length(SApopdatlst)) {
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
  }
  
  
  ## Copy files to outfolder
  rmdfn <- file.path(reportfolder, paste0(AOInm, '_report.Rmd'))
  reportnm <- paste0(AOInm, '_report.docx')
  reportfn <- file.path(reportfolder, reportnm)
  file.copy(reportrmd, rmdfn, overwrite=TRUE)
  file.copy(system.file("rmd", "anWWWtemplate.docx", package="FIESTA"),
	file.path(reportfolder, "anWWWtemplate.docx"), overwrite=TRUE)

  ## Set working directory to reportfolder
  setwd(reportfolder) 

  test <- tryCatch(
  	rmarkdown::render(
    		input = rmdfn,
    		output_file = reportfn,
    		params = list(WWWpopdat = WWWpopdat, 
    		              WWWest = WWWest,
    		              domain_unit_display_nm = display_dunit_nm,
    		              domain_unit_lut = domain_unit_lut,
    		              fortypgrpcd = fortypgrpcd),
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
