#' Database - Test access to an ESRI geodatabase.
#' 
#' Checks gdb access (See note below).
#' 
#' 
#' @param gdbfn String. Name of ESRI geodatabase (*.gdb).
#' @param outfolder String. Optional. Name of output folder. If NULL, export to
#' working directory.
#' @param outfn.pre String. Prefix for out_dsn.
#' @param outfn.date Logical. If TRUE, add current date to out_dsn.
#' @param overwrite Logical. If TRUE, overwrites the geodatabase, if exists.
#' @param showlist Logical. If TRUE, shows list of tables in database.
#' @param returnpath Logical. If TRUE, returns full path to gdb file name.  If
#' FALSE, returns gdbfn
#' @note Must have a functioning installation of ArcGIS or environment
#' variables for ArcGIS.
#' @author Tracey S. Frescino
#' @keywords data
#' @export DBtestESRIgdb
DBtestESRIgdb <- function(gdbfn = NULL, 
                          outfolder = NULL, 
                          outfn.pre = NULL, 
                          outfn.date = FALSE, 
                          overwrite = FALSE, 
                          showlist = TRUE, 
                          returnpath = TRUE) {
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

  
  ##################################################################
  ## CHECK PARAMETER NAMES
  ##################################################################
  input.params <- names(as.list(match.call()))[-1]
  if (!all(input.params %in% names(formals(DBtestESRIgdb)))) {
    miss <- input.params[!input.params %in% formals(DBtestESRIgdb)]
    stop("invalid parameter: ", toString(miss))
  }
  
  
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
    
      tablst <- sf::st_layers(gdbpath)$name		## OpenFileGDB driver
      if (showlist) {
        print(tablst)
      }
    }
  } 
  if (returnpath) {
    return(gdbpath) 
  } else {
    return(basename(gdbpath))
  }
}
