#' Database - Queries a SQLite database table.
#' 
#' Extracts and queries data from a SQLite (*.sqlite) database (Note: must use
#' SQL syntax).
#' 
#' 
#' @param states String. Vector of one or more state names.
#' @param outfolder String. The output folder path. If NULL, outfolder is the
#' working directory.
#' @return Returns nothing.
#' @author Tracey S. Frescino
#' @keywords data
#' @examples 
#' \dontrun{
#' # Extract data from Washington state
#' DBgetSQLite(states = "WA")
#' 
#' # Extract data from Utah and California, save to an outfolder
#' DBgetSQLite(states = c("UT", "CA"),
#'             outfolder = tempdir()) 
#' }
#' @export DBgetSQLite
DBgetSQLite <- function (states = NULL, 
                         outfolder = NULL) {


  ##################################################################
  ## CHECK PARAMETER NAMES
  ##################################################################
  input.params <- names(as.list(match.call()))[-1]
  if (!all(input.params %in% names(formals(DBgetSQLite)))) {
    miss <- input.params[!input.params %in% formals(DBgetSQLite)]
    stop("invalid parameter: ", toString(miss))
  }
  
  
  ## Stop if no arguments passed. No GUI available for this function
  if (nargs() == 0) stop("must include states")

  ## Set URL where data files are
  downloadfn <- "https://apps.fs.usda.gov/fia/datamart/Databases"

  ###################################################################
  ## CHECK PARAMETERS
  ###################################################################

  ## Check states
  if (!is.character(states)) 
    stop("state must be a character vector")

  ## Check state and get in proper format (abbr)
  stabbrs <- pcheck.states(states, "ABBR")

  ## Check outfolder
  outfolder <- pcheck.outfolder(outfolder)

  for (st in stabbrs) {
    ## Create file name to download
    fn <- paste0(downloadfn, "/SQLite_FIADB_", st, ".zip")

    ## Download zipfile and extract database
    message(paste("downloading SQLite database for", st, "..."))

    outfn <- paste0(outfolder, "/SQLite_FIADB_", st, ".zip")
    utils::download.file(fn, outfn, mode="wb")
    filenm <- utils::unzip(outfn, exdir=outfolder)
  }
}
