anGBpop_report <- function(GBpopdat, AOInm, photofn=NULL, photo_author=NULL, 
	EVALIDator_match=FALSE, pcfilter=NULL, spcd=746, title.ref=NULL, 
	outfolder=NULL) {
  ## DESCRIPTION: Creates a report using Rmarkdown 
  ## 		Adds a folder named report in the outfolder and copies all 
  ##		components of report into folder.

  wkdir <- getwd()

  ## set global variables
  report_imagefn <- NULL

  ## Check GBpopdat
  GBpopdat <- FIESTA::pcheck.object(GBpopdat, "GBpopdat", 
		list.items=c("treex", "seedx"))

  outfolder <- pcheck.outfolder(outfolder)
  reportfolder <- tempdir()
  reportrmd <- system.file("rmd", "anGBpop_report.Rmd", package="FIESTA")

  ## TESTING
  #reportrmd <- "C:/R/R-4.0.5/library/FIESTA/rmd/anGBpop_report.Rmd"

  #if (!dir.exists(reportfolder)) {
  #  message(reportfolder, " does not exist, creating...")
  #  dir.create(reportfolder)
  #}
  
  ## Copy files to outfolder
  rmdfn <- file.path(reportfolder, paste0(AOInm, '_report.Rmd'))
  reportnm <- paste0(AOInm, '_report.docx')
  reportfn <- file.path(reportfolder, reportnm)
  file.copy(reportrmd, rmdfn, overwrite=TRUE)
  file.copy(system.file("rmd", "anGBtemplate.docx", package="FIESTA"),
	file.path(reportfolder, "anGBtemplate.docx"), overwrite=TRUE)
  if (!is.null(photofn)) {
    if (!file.exists(photofn)) {
      stop(photofn, "does not exist")
    }
    file.copy(photofn, file.path(reportfolder, "report_image.PNG"), overwrite=TRUE) 
    report_imagefn <- file.path(reportfolder, "report_image.PNG")
  }


#  ref_spcd <- FIESTA::ref_codes[FIESTA::ref_codes$VARIABLE == "SPCD", ]
  spcdlst <- table(GBpopdat$seedx$SPCD)[table(GBpopdat$seedx$SPCD) > 5]
  if (!is.null(spcd) && length(spcd) > 0) {
    if (!spcd %in% names(spcdlst)) {
      stop("SPCD ", spcd, " not in popdat")
    } 
  } else {
    spcd <- names(spcdlst[spcdlst != 999 & spcdlst == max(spcdlst)])
  }
#  spnm <- ref_spcd[!is.na(ref_spcd$VALUE) & 
#				ref_spcd$VALUE == spcd, "MEANING"]

  if (is.null(photofn)) {
    # Lines 102 to 106
    #system(paste("sed -i '102,106d'", rmdfn)) 
    system(paste("sed -i '92,96d'", rmdfn)) 
  }

  ## Set working directory to reportfolder
  setwd(reportfolder) 

  rmarkdown::render(
    input = rmdfn,
    output_file = reportfn,
    params = list(GBpopdat=GBpopdat, AOInm=AOInm, 
		photo_author=photo_author, pcfilter=pcfilter, 
		spcd=spcd, EVALIDator_match=EVALIDator_match, title.ref=title.ref),
    envir = parent.frame()
  )

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
