anGBpop_report <- function(rawfolder, AOInm, photofn=NULL, title.ref=NULL, 
	outfolder=NULL) {
  ## DESCRIPTION: Creates a report using Rmarkdown 
  ## 		Adds a folder named report in the outfolder and copies all 
  ##		components of report into folder.

  outfolder <- pcheck.outfolder(outfolder)
  reportfolder <- file.path(outfolder, "report")
  if (!dir.exists(reportfolder)) dir.create(reportfolder)
  
  ## Copy files to outfolder
  rmdfn <- file.path(reportfolder, paste0(AOInm, '_report.Rmd'))
  reportfn <- file.path(reportfolder, paste0(AOInm, '_report.docx'))
  file.copy(system.file("rmd", "anGBpop_report.Rmd", package="FIESTA"),
	rmdfn, overwrite=TRUE)
  file.copy(system.file("rmd", "template.docx", package="FIESTA"),
	file.path(reportfolder, "template.docx"), overwrite=TRUE)
  file.copy(photofn, file.path(reportfolder, "report_image.png"))


  rmarkdown::render(
    input = rmdfn,
    output_file = reportfn,
    params = list(rawfolder=rawfolder, AOInm=AOInm, photofn="report_image.png",
		title.ref=title.ref),
    envir = parent.frame()
  )
}
