anGBpop_report <- function(rawfolder, AOInm, photofn=NULL, title.ref=NULL, 
	outfolder=NULL) {
  ## Description: Creates a report using Rmarkdown  

  outfolder <- pcheck.outfolder(outfolder)
  
  ## Copy files to outfolder
  reportfn <- file.path(outfolder, paste0(AOInm, '_report.Rmd'))
  outfn <- file.path(outfolder, paste0(AOInm, '_report.docx'))
  file.copy(system.file("rmd", "anGBpop_report.Rmd", package="FIESTA"),
	reportfn, overwrite=TRUE)
  file.copy(system.file("rmd", "template.docx", package="FIESTA"),
	file.path(outfolder, "template.docx"), overwrite=TRUE)
  file.copy(photofn, file.path(outfolder, "report_image.png"))

  rmarkdown::render(
    input=reportfn,
    output_file=outfn,
    params=list(rawfolder=rawfolder, AOInm=AOInm, photofn="report_image.png",
		title.ref=title.ref),
    envir = parent.frame()
  )
}