write2csv <- function(tab, outfile=NULL, outfolder=NULL, outfilenm=NULL, outfn.pre=NULL, 
	outfn.date=FALSE, overwrite=FALSE, tabtitle=NULL, appendfile=FALSE, closefn=TRUE, 
	outtxt=NULL, gui=FALSE){
  ###################################################################################
  ## DESCRIPTION: Internal function to write to csv file.
  ##  
  ## ARGUMENTS: 
  ##  tab    DF. The table to output.
  ##  outfile	An open outfile
  ##  outfilefn  String. The output file name (Full path) of open file or new file
  ##  outfn.date	Adds a date to the end of the file name
  ##  tabtitle  String. The title of the table.
  ##  outtxt	String. Name of file for printing to screen  
  ##
  ## VALUE:
  ##  Writes out a table to a comma-delimited text file.
  ####################################################################################
  ## CHECK GUI - IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  if (nargs() == 0) gui <- TRUE 

  if (is.null(outfile)) {
    ## Check outfilenm
    outfilenm <- getoutfn(outfilenm, outfolder=outfolder, outfn.pre=outfn.pre, 
		outfn.date=outfn.date, overwrite=overwrite, outfn.default = "outfile",
		ext="csv", append=appendfile)

    ## open file
    if (appendfile) {
      outfile <- file(outfilenm, "a")
    } else {
      outfile <- file(outfilenm, "w")
    }

    msg <- ifelse (!is.null(outtxt) && is.character(outtxt),
		paste(outtxt, "written to", outfilenm),
		paste("data frame written to", outfilenm))

  } else if (!isOpen(outfile)) {
    stop("outfile is not an open file")
  } else {
    if (!is.null(outfilenm) && is.character(outfilenm)) {    
      msg <- ifelse (!is.null(outtxt) && is.character(outtxt), 
		paste(outtxt, "written to", outfilenm),
		paste("data frame written to", outfilenm))
    } else {
      msg <- ifelse (!is.null(outtxt) && is.character(outtxt), 
        	paste(outtxt, "written to", outfolder),
		paste("data frame written to", outfolder))
    }  
  }


  ## If tabtitle is not null, add to file.
  if (!is.null(tabtitle))
    cat(tabtitle, file=outfile, sep="\n")

  ## Write table to file.
  write.table(tab, outfile, row.names=FALSE, append=TRUE, sep=",")

  ## If closefn is TRUE, close the file.
  if (closefn) {
    close(outfile)
 
    message("################################### \n", 
            msg, "\n###################################")

  } else {

    return(outfile)
  }
}
