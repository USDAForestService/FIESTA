DBgetCSV <- function(DBtable, states=NULL, returnDT=FALSE, 
			stopifnull=TRUE, noIDate=TRUE) {
  # DESCRIPTION: Import data tables from FIA Datamart

  # Stop if no arguments passed. No GUI available for this function
  if (nargs() == 0) {
    stop("must include DBtable")
  }

  # Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  if (!all(input.params %in% names(formals(FIESTA::DBgetCSV)))) {
    miss <- input.params[!input.params %in% formals(FIESTA::DBgetCSV)]
    stop("invalid parameter: ", toString(miss))
  }


  ## Set URL where data files are
  downloadfn <- "https://apps.fs.usda.gov/fia/datamart/CSV/"

  ###################################################################
  ## CHECK PARAMETERS
  ###################################################################
  ZIP <- TRUE

  ## Check DBtable
  if (!is.vector(DBtable) || !is.character(DBtable) || !length(DBtable) == 1) {
    stop("DBtable must be a character vector of length 1")
  }

  ## Check states and get in proper format (abbr)
  stabbrs <- FIESTA::pcheck.states(states, "ABBR")

  ## Check ZIP
  ZIP <- FIESTA::pcheck.logical(ZIP, varnm="ZIP", title="Zip files?",
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
			  fread(fn, integer64="numeric"),
		  	  error=function(e) {
				  warning(basename(fn), " does not exist")
  			  return(NULL)
             }
      )
      if (noIDate) {
        cols <- names(tab)[unlist(lapply(tab, function(x) any(class(x) == "IDate")))]
        tab[, (cols) := lapply(.SD, as.character), .SDcols=cols]
        if ("MODIFIED_DATE" %in% names(tab) && is.logical(tab$MODIFIED_DATE)) {
          tab$MODIFIED_DATE <- as.character(tab$MODIFIED_DATE)
        }
      } else {
        if ("MODIFIED_DATE" %in% names(tab) && is.logical(tab$MODIFIED_DATE)) {
          tab$MODIFIED_DATE <- as.IDate(tab$MODIFIED_DATE)
        }
      }
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
      tab <- suppressWarnings(fread(filenm, integer64="numeric"))
      if (noIDate) {
        cols <- names(tab)[unlist(lapply(tab, function(x) any(class(x) == "IDate")))]
        tab[, (cols) := lapply(.SD, as.character), .SDcols=cols]
        if ("MODIFIED_DATE" %in% names(tab) && is.logical(tab$MODIFIED_DATE)) {
          tab$MODIFIED_DATE <- as.character(tab$MODIFIED_DATE)
        }
      } else {
        if ("MODIFIED_DATE" %in% names(tab) && is.logical(tab$MODIFIED_DATE)) {
          tab$MODIFIED_DATE <- as.IDate(tab$MODIFIED_DATE)
        }
      }

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

