anGBpop_report <- function(rawfolder, AOInm, title.ref=NULL, outfolder=NULL) {
  
  outfolder <- pcheck.outfolder(outfolder)

  rmarkdown::render(
    input=system.file("rmd", "anGBpop_report.Rmd", package="FIESTA"),
    output_file=file.path(outfolder, paste0(AOInm, '_report.docx')),
    params=list(rawfolder=rawfolder, AOInm=AOInm, title.ref=title.ref),
    envir = parent.frame()
  )
}