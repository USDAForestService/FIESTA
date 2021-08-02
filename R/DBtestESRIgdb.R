DBtestESRIgdb <- function(gdbfn=NULL, outfolder=NULL, outfn.pre=NULL,
				outfn.date=FALSE, overwrite=FALSE, showlist=TRUE,
				returnpath=TRUE) {
  ## DESCRIPTION: 
  ## Test gdb access

  if (!"arcgisbinding" %in% rownames(installed.packages())) {
    message("accessing ESRI geodatabases requires package arcgisbinding with R-ArcGIS Bridge installed")
    message("see: https://esricanada-ce.github.io/r-arcgis-tutorials/1-Getting-Started.pdf")
    #stop("")
  }

  arcgisbinding::arc.check_product()
  gdbpath <- getoutfn(gdbfn, outfn.pre=outfn.pre, outfn.date=outfn.date, 
		overwrite=overwrite, outfolder=outfolder, ext=getext(gdbfn))

#  if (is.na(getext(gdbfn)) || getext(gdbfn) == "NA") {
#    gdbfn <- paste0(gdbfn, ".gdb")
#  }
   
  ## Overwrite file
  if (file.exists(gdbpath)) {
    if (overwrite) {
      arcgisbinding::arc.delete(gdbpath)
      if (file.exists(gdbpath)) {
        result <- tryCatch( {
			file.remove(gdbpath)
 		}, error = function(e) {
			stop(e)
		}, warning=function(x) {
           		stop(x)
		})			
     }
     if (!file.exists(gdbpath)) {
       message("removed ", gdbpath)
     }
   } else {
      #message("gdb connection successful")
    
      tablst <- sf::st_layers(gdbpath)		## OpenFileGDB driver
      if (showlist)
        print(tablst)
    }
  } 
  if (returnpath) {
    return(gdbpath) 
  } else {
    return(basename(gdbpath))
  }
}
