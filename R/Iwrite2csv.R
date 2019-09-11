write2csv <- function(tab, outfile=NULL, outfolder=NULL, outfilenm=NULL, outfn.date=FALSE, 
	overwrite=FALSE, tabtitle=NULL, appendfile=FALSE, closefn=TRUE){
  ###################################################################################
  ## DESCRIPTION: Internal function to write to csv file.
  ##  
  ## ARGUMENTS: 
  ##  tab    DF. The table to output.
  ##  outfile	An open outfile
  ##  outfilefn  String. The output file name (Full path)
  ##  outfn.date	Adds a date to the end of the file name
  ##  tabtitle  String. The title of the table.  
  ##
  ## VALUE:
  ##  Writes out a table to a comma-delimited text file.
  ####################################################################################
  ## CHECK GUI - IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  if (nargs() == 0) gui <- TRUE 

  if (is.null(outfile)) {
    ## Check outfolder
    if (!is.null(outfolder)) 
      if (!file.exists(outfolder)) stop("invalid outfolder")
      
    ## Check outfilenm
    if (is.null(outfilenm)) {
      outfilenm <- "test"
    } else if (!is.character(outfilenm)) {
      stop("outfilenm must be character")
    } 

    ## Check if .csv extension
    if (raster::extension(outfilenm) != "")
      outfilenm <- gsub(".CSV", "", outfilenm, ignore.case=TRUE)    

    ## Check outfn.date
    outfn.date <- FIESTA::pcheck.logical(outfn.date, varnm="outfn.date", 
		title="Add date to filenm?", first="YES", gui=gui)
    if (outfn.date)
      outfilenm <- paste0(outfilenm, "_", format(Sys.time(), "%Y%m%d"))

    ## Check overwrite
    overwrite <- FIESTA::pcheck.logical(overwrite, varnm="overwrite", 
		title="Overwrite file?", first="YES", gui=gui)

    if (!overwrite) {
      outfilenm <- FIESTA::fileexistsnm(outfolder, outfilenm, "csv")
      if (!is.null(outfolder))
        outfilenm <- paste(outfolder, outfilenm, sep="/")
    } else {
      if (is.null(outfolder)) outfolder <- dirname(outfilenm)
      outfilenm <- paste(outfolder, outfilenm, sep="/")
    }
    outfile <- paste0(outfilenm, ".csv")
  } 


  ## IF appendfile=FALSE, open a new file.
  if (!appendfile) {
    #if (outfn.date=TRUE) 
    #  outfile <- paste0(outfile, "_", format(Sys.time(), "%Y%m%d"))
    #if (!overwrite) 
    #  outfile <- FIESTA::fileexistsnm(outfolder, outfile, "csv")
    #if (!is.null(outfolder)) 
    #  outfile <- paste0(outfolder, "/", outfile)
    outfile <- file(outfile, "w")
  }

  ## If tabtitle is not null, add to file.
  if (!is.null(tabtitle))
    cat(tabtitle, file=outfile, sep="\n")

  ## Write table to file.
  write.table(tab, outfile, row.names=FALSE, append=TRUE, sep=",")


  ## If closefn is TRUE, close the file.
  if (closefn) {
    close(outfile)

    cat(
    " #################################################################################", 
    "\n", paste("Data frame written to: "), "\n", outfilenm, "\n", 
    "#################################################################################",
    "\n" )

  } else {

    return(outfile)
  }
}
