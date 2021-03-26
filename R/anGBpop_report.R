anGBpop_report <- function(GBpopdat, AOInm, photofn=NULL, photo_author=NULL, 
	EVALIDator_match=FALSE, cfilter=NULL, pfilter=NULL, spcd=746, title.ref=NULL, 
	outfolder=NULL) {
  ## DESCRIPTION: Creates a report using Rmarkdown 
  ## 		Adds a folder named report in the outfolder and copies all 
  ##		components of report into folder.

  wkdir <- getwd()

  ## set global variables
  report_imagefn <- NULL

  outfolder <- pcheck.outfolder(outfolder)
  reportfolder <- file.path(outfolder, "report")
  reportfolder <- normalizePath(reportfolder)


  if (!dir.exists(reportfolder)) {
    message(reportfolder, " does not exist, creating...")
    dir.create(reportfolder)
  }
  
  ## Copy files to outfolder
  rmdfn <- file.path(reportfolder, paste0(AOInm, '_report.Rmd'))
  reportfn <- file.path(reportfolder, paste0(AOInm, '_report.docx'))
  file.copy(system.file("rmd", "anGBpop_report.Rmd", package="FIESTA"),
	rmdfn, overwrite=TRUE)

  file.copy(system.file("rmd", "anGBtemplate.docx", package="FIESTA"),
	file.path(reportfolder, "anGBtemplate.docx"), overwrite=TRUE)
  if (!is.null(photofn)) {
    if (!file.exists(photofn)) {
      stop(photofn, "does not exist")
    }
    file.copy(photofn, file.path(reportfolder, "report_image.PNG"), overwrite=TRUE) 
    report_imagefn <- file.path(reportfolder, "report_image.PNG")
  }


  ## TESTING
#  file.copy("C:/_tsf/_GitHub/FIESTA/inst/rmd/anGBpop_report.Rmd", rmdfn, overwrite=TRUE)


  if (is.null(photofn)) {
    # Lines 102 to 106
    system(paste("sed -i '102,106d'", rmdfn)) 
  }

  ## Set working directory to reportfolder
  setwd(reportfolder) 

  rmarkdown::render(
    input = rmdfn,
    output_file = reportfn,
    params = list(GBpopdat=GBpopdat, AOInm=AOInm, 
		photo_author=photo_author, cfilter=cfilter, pfilter=pfilter, 
		spcd=spcd, EVALIDator_match=EVALIDator_match, title.ref=title.ref),
    envir = parent.frame()
  )

  ## Set working directory back to original working directory
  setwd(wkdir) 

}
