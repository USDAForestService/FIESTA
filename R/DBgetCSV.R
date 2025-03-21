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


  ## Set options
  opts <- options()
  options(timeout = max(80000, getOption("timeout")))
  on.exit(options(opts))

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


  ###################################################################
  ## Define gettab function
  ###################################################################
    
  gettab <- function(stabbr = NULL, DBtable) {
    
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
      {
        utils::download.file(fn, temp, mode = "wb", quiet = TRUE)
      },
		  error = function(e) {
		    message(e)
		    NULL
      }
    )
    
    # if download.file fails it either throws an error or invisibly returns a non-zero integer value
    if (is.null(tab) || tab != 0) {
      stop("Download of ", DBtable, " was unsuccessful")
    }

    filenm <- utils::unzip(temp, exdir = tempdir)
    tab_out <- fread(filenm, integer64 = "character")
    
    if (nrow(tab_out) == 0) {
      stop("Attempted download of ", DBtable, " returned zero rows.")
    }
    
    tab_out <- changeclass(tab_out)

    unlink(temp)
    unlink(tempdir)
    file.remove(filenm)
    
    return(tab_out)
    
  }
  

  ###################################################################
  ## Get tables
  ###################################################################
  if (is.null(stabbrs)) {
    csvtable <- gettab(DBtable=DBtable)
  } else {
    csvtable <- tryCatch(
      rbindlist(lapply(stabbrs, gettab, DBtable), fill=TRUE),
      error=function(e) {
        stop(e, "\n")
      }
    )
  }
      

  if (is.null(csvtable)) {
    stop("Unable to download table(s).")
  }
  names(csvtable) <- toupper(names(csvtable))

  if (noIDate) {
    ## Change columns of type POSIX* to character before writing to database
    if (any(grepl("POSIX", lapply(csvtable, class)))) {
      POSIXcols <- names(csvtable)[grepl("POSIX", lapply(csvtable, class))]
      csvtable <- setDF(csvtable)
      csvtable[POSIXcols] <- lapply(csvtable[POSIXcols], as.character)
      csvtable <- setDT(csvtable)
    }
  }
    
  if (!returnDT) {
    csvtable <- data.frame(csvtable, stringsAsFactors=FALSE)
  }
  
  return(csvtable)
}

