DBgetSQLite <- function (states=NULL, outfolder=NULL) {


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
  stabbrs <- FIESTA::pcheck.states(states, "ABBR")

  ## Check outfolder
  outfolder <- FIESTA::pcheck.outfolder(outfolder)

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
