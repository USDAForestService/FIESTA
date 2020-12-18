anPBpopICE_report <- function(rawfolder, AOInm, T1, T2, outfn.pre,
	ice.QAQCfn=NULL, photofn=NULL, outfolder=NULL) {
  ## DESCRIPTION: Creates a report using Rmarkdown 
  ## 		Adds a folder named report in the outfolder and copies all 
  ##		components of report into folder.

  outfolder <- pcheck.outfolder(outfolder)
  outfolder <- normalizePath(outfolder)
  reportfolder <- file.path(outfolder, "report")
  if (!dir.exists(reportfolder)) dir.create(reportfolder)
  
  ## Copy files to outfolder
  rmdfn <- file.path(reportfolder, paste0(AOInm, '_report.Rmd'))
  reportfn <- file.path(reportfolder, paste0(AOInm, '_report.docx'))
  file.copy(system.file("rmd", "anPBpopICE_report.Rmd", package="FIESTA"),
	rmdfn, overwrite=TRUE)
  file.copy(system.file("rmd", "anPBtemplateICE.docx", package="FIESTA"),
	file.path(reportfolder, "anPBtemplateICE.docx"), overwrite=TRUE)
  if (is.null(photofn)) {
    file.copy(system.file("rmd", "ICE.PNG", package="FIESTA"), 
		file.path(reportfolder, "ICE.PNG"))
  } else {
    file.copy(photofn, file.path(reportfolder, "ICE.PNG"))
  }

  ## TESTING
  #file.copy("C:/_tsf/_GitHub/FIESTA/inst/rmd/anPBpopICE_report.Rmd", rmdfn, overwrite=TRUE)
  #file.copy("C:/_tsf/_GitHub/FIESTA/inst/rmd/anPBtemplateICE.docx", reportfn, overwrite=TRUE)
  #file.copy("C:/_tsf/_GitHub/FIESTA/inst/rmd/ICE.PNG", file.path(reportfolder, "ICE.PNG"), overwrite=TRUE)

  if (is.null(ice.QAQCfn)) {
    # Lines 1762-1780 contain text for QAQC
    system(paste("sed -i '1762,1780d'", rmdfn)) 
  }


  rmarkdown::render(
    input = rmdfn,
    output_file = reportfn,
    params = list(rawfolder=rawfolder, AOInm=AOInm, T1=T1, T2=T2,
		outfn.pre=outfn.pre, ice.QAQCfn=ice.QAQCfn),
    envir = parent.frame()
  )

}
