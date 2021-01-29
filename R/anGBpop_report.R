anGBpop_report <- function(rawfolder, AOInm, photofn=NULL, title.ref=NULL, 
	outfolder=NULL) {
  ## DESCRIPTION: Creates a report using Rmarkdown 
  ## 		Adds a folder named report in the outfolder and copies all 
  ##		components of report into folder.

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
    file.copy(photofn, file.path(reportfolder, "report_image.png")) 
    report_imagefn <- file.path(reportfolder, "report_image.png")
  }


  ## TESTING
#  file.copy("C:/_tsf/_GitHub/FIESTA/inst/rmd/anGBpop_report.Rmd", rmdfn, overwrite=TRUE)


  if (is.null(photofn)) {
    # Lines 48 to 59
    system(paste("sed -i '48,59d'", rmdfn)) 
  }

  rmarkdown::render(
    input = rmdfn,
    output_file = reportfn,
    params = list(rawfolder=rawfolder, AOInm=AOInm, title.ref=title.ref),
    envir = parent.frame()
  )
}
