#' Database - Extracts data table(s) from FIA DataMart.
#' 
#' Downloads and extracts compressed comma-delimited file(s) (*.zip) from FIA
#' DataMart (https://apps.fs.usda.gov/fia/datamart/CSV/datamart_csv.html).
#' Only 1 table can be specified, but multiple states may be included.
#' 
#' The compressed data files are downloaded from FIA DataMart; saved to a
#' temporary space; extracted and imported; and deleted from temporary space.
#' Accessibility and download time depends on access and speed of internet
#' connection.
#' 
#' @param DBtable String. Name of table to download. Only 1 table allowed.
#' @param states String or numeric vector. Name (e.g., "Arizona", "New Mexico")
#' or code (e.g., 4, 35) of states to download data. If NULL, tables that are
#' not state-level are downloaded.
#' @param returnDT Logical. If TRUE, a data table is returned, else, a data
#' frame.
#' @param stopifnull Logical. If TRUE, stop if table is NULL.
#' @param noIDate Logical. If TRUE, do not include columns with type IDate.
#' @return Returns a data table (returnDT=TRUE), or data.frame (returnDT=FALSE)
#' of downloaded table(s). If more than one state, returned as one table.
#' @author Tracey S. Frescino
#' @examples
#' \dontrun{
#' # Get reference table for FIA research stations
#' DBgetCSV(DBtable="ref_research_station")
#' 
#' # Get plot data for multiple states
#' FIAplots <- DBgetCSV("PLOT", c("Georgia", "Utah"))
#' table(FIAplots$STATECD)
#' }
#' @export DBgetCSV
DBgetCSV <- function(DBtable, 
                     states = NULL, 
                     returnDT = FALSE, 
                     stopifnull = TRUE, 
                     noIDate = TRUE) {
  # DESCRIPTION: Import data tables from FIA Datamart

  # Stop if no arguments passed. No GUI available for this function
  if (nargs() == 0) {
    stop("must include DBtable")
  }
  
  ## Set global variables
  ZIP <- TRUE
  
  ## Set URL where data files are
  downloadfn <- "https://apps.fs.usda.gov/fia/datamart/CSV/"

  
    
  ##################################################################
  ## CHECK PARAMETER NAMES
  ##################################################################

  # Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  if (!all(input.params %in% names(formals(DBgetCSV)))) {
    miss <- input.params[!input.params %in% formals(DBgetCSV)]
    stop("invalid parameter: ", toString(miss))
  }


  ###################################################################
  ## CHECK PARAMETER INPUTS
  ###################################################################

  ## Check DBtable
  if (!is.vector(DBtable) || !is.character(DBtable) || !length(DBtable) == 1) {
    stop("DBtable must be a character vector of length 1")
  }

  ## Check states and get in proper format (abbr)
  stabbrs <- pcheck.states(states, "ABBR")

  ## Check ZIP
  ZIP <- pcheck.logical(ZIP, varnm="ZIP", title="Zip files?",
    first="YES")

  ###################################################################
  ## Define gettab function
  ###################################################################
  if (!ZIP) {
    gettab <- function(stabbr=NULL, DBtable) {
      if (is.null(stabbr)) {
        fn <- paste0(downloadfn, toupper(DBtable), ".csv")
        message(paste("downloading", DBtable, "..."))
      } else {
        fn <- paste0(downloadfn, stabbr, "_", toupper(DBtable), ".csv")
        message(paste("downloading", DBtable, "for", stabbr, "..."))
      }
      tab <- tryCatch(
			  fread(fn, integer64="character"),
		  	  error=function(e) {
				  warning(basename(fn), " does not exist")
  			  return(NULL)
             }
      )
      tab <- changeclass(tab)
      return(tab)
    }

  } else {
    gettab <- function(stabbr=NULL, DBtable) {
      if (is.null(stabbr)) {
        fn <- paste0(downloadfn, toupper(DBtable), ".zip")
        message(paste("downloading and extracting", DBtable, "..."))
      } else {
        fn <- paste0(downloadfn, stabbr, "_", toupper(DBtable), ".zip")
        message(paste("downloading and extracting", DBtable, "for", stabbr, "..."))
      }

      temp <- tempfile()
      tempdir <- tempdir()
      tab <- tryCatch(
			  utils::download.file(fn, temp, mode="wb", quiet=TRUE),
			  error=function(e) {
			    warning(basename(fn), " does not exist")
  			    return(NULL)
        }
      )
      if (is.null(tab)) {
        message(tab, " is not available")
        return(NULL)
      }

      filenm <- utils::unzip(temp, exdir=tempdir)
      tab <- fread(filenm, integer64="character")
      tab <- changeclass(tab)

      unlink(temp)
      unlink(tempdir)
      file.remove(filenm)
      return(tab)
    }
  }

  ###################################################################
  ## Get tables
  ###################################################################
  if (is.null(stabbrs)) {
    csvtable <- gettab(DBtable=DBtable)
  } else {
    csvtable <- tryCatch(
      do.call(rbindlist, list(lapply(stabbrs, gettab, DBtable))),
		    error=function(e) {
 		    message(e, "\n")
		    return(NULL)
      }
    )
  }
  if (is.null(csvtable)) {
    return(NULL)
  }
  if (!returnDT) {
    csvtable <- data.frame(csvtable, stringsAsFactors=FALSE)
  }
  
  return(csvtable)
}

