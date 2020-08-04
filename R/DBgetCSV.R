DBgetCSV <- function (DBtable, states=NULL, ZIP=TRUE, returnDT=FALSE, 
			stopifnull=TRUE, gui=FALSE) {

  ## Stop if no arguments passed. No GUI available for this function
  if (nargs() == 0) stop("must include sql")

  ## Set URL where data files are
  downloadfn <- "https://apps.fs.usda.gov/fia/datamart/CSV/"

  ###################################################################
  ## CHECK PARAMETERS
  ###################################################################

  ## Check DBtable
  if (!is.vector(DBtable) || !is.character(DBtable) || !length(DBtable) == 1)
    stop("DBtable must be a character vector of length 1")


  ## Check states and get in proper format (abbr)
  stabbrs <- FIESTA::pcheck.states(states, "ABBR")

  ## Check ZIP
  ZIP <- FIESTA::pcheck.logical(ZIP, varnm="ZIP", title="Zip files?", gui=gui, 
	first="YES")

  ###################################################################
  ## Define gettab function
  ###################################################################
  if (!ZIP) {
    gettab <- function (stabbr=NULL, DBtable) {
      if (is.null(stabbr)) {
        fn <- paste0(downloadfn, toupper(DBtable), ".csv")
        if (httr::http_error(fn)) {
          if (stopifnull) {
            stop(fn, " does not exist")
          } else {
            message(fn, " does not exist")
            return(NULL)
          }
        }
        message(paste("downloading", DBtable, "..."))
      } else {
        fn <- paste0(downloadfn, stabbr, "_", toupper(DBtable), ".csv")
        if (httr::http_error(fn)) {
          if (stopifnull) {
            stop(fn, " does not exist")
          } else {
            message(fn, " does not exist")
            return(NULL)
          }
        }
        message(paste("downloading", DBtable, "for", stabbr, "..."))
      }
      fread(fn, integer64="numeric")
    }

  } else {
    gettab <- function (stabbr=NULL, DBtable) {

      if (is.null(stabbr)) {
        fn <- paste0(downloadfn, toupper(DBtable), ".zip")
        if (httr::http_error(fn)) {
          if (stopifnull) {
            stop(fn, " does not exist")
          } else {
            message(fn, " does not exist")
            return(NULL)
          }
        }
        message(paste("downloading and extracting", DBtable, "..."))
      } else {
        fn <- paste0(downloadfn, stabbr, "_", toupper(DBtable), ".zip")
        if (httr::http_error(fn)) {
          if (stopifnull) {
            stop(fn, " does not exist")
          } else {
            message(fn, " does not exist")
            return(NULL)
          }
        }
        message(paste("downloading and extracting", DBtable, "for", stabbr, "..."))
      }

      temp <- tempfile()
      tempdir <- tempdir()
      utils::download.file(fn, temp, mode="wb")
      filenm <- utils::unzip(temp, exdir=tempdir)

      dat <- suppressWarnings(fread(filenm, integer64="numeric"))

      unlink(temp)
      unlink(tempdir)
      file.remove(filenm)
      return(dat)
    }
  }

  ###################################################################
  ## Get tables
  ###################################################################
  if (is.null(stabbrs)) {
    csvtable <- gettab(DBtable=DBtable)
  } else {
    csvtable <- do.call(rbind, lapply(stabbrs, gettab, DBtable))
  }

  if (!returnDT) csvtable <- data.frame(csvtable, stringsAsFactors=FALSE)


  return(csvtable)
}
