#' Database - Create a new SQLite database.
#' 
#' Create a new SQLite database.
#' 
#' 
#' @param SQLitefn String. Name of SQLite database (*.sqlite).
#' @param gpkg Logical. If TRUE, Sqlite geopackage database.
#' @param dbconnopen Logical. If TRUE, the dbconn connection is not closed.
#' @param outfolder String. Optional. Name of output folder. If NULL, export to
#' working directory.
#' @param outfn.pre String. Prefix for out_dsn.
#' @param outfn.date Logical. If TRUE, add current date to out_dsn.
#' @param overwrite Logical. If TRUE, overwrites out_dsn.
#' @param returnpath Logical. If TRUE, returns full path to SQLite file name.
#' If FALSE, returns SQLitefn
#' @param stopifnull Logical. If TRUE, and SQLitefn is NULL, stops execution of
#' program.  If FALSE, returns NULL
#' @author Tracey S. Frescino
#' @keywords data
#' @export DBcreateSQLite
DBcreateSQLite <- function(SQLitefn=NULL, gpkg=FALSE, dbconnopen=FALSE, 
	outfolder=NULL, outfn.pre=NULL, outfn.date=FALSE, overwrite=FALSE, 
	returnpath=TRUE, stopifnull=FALSE) {
  ## DESCRIPTION: 
  ## Test SQLite connection (SQLite or Geopackage database)
  ## ARGUMENTS:
  ## SQLitefn - String. SQLite filename (*.sqlite or *.gpkg)
  ## dbconnopen - Logical. If TRUE, keep connection to database open
  ## gpkg	- Logical. If TRUE, geopackage database.

  if (!"DBI" %in% rownames(installed.packages())) {
    message("accessing SQLite databases requires package DBI")
  }
  if (!"RSQLite" %in% rownames(installed.packages())) {
    message("accessing SQLite databases requires package RSQLite")
  }

  ## Check gpkg
  dbext <- ifelse(gpkg, ".gpkg", ".db")

  ## Check filename
  SQLitePath <- checkfilenm(SQLitefn, outfolder, stopifnull=stopifnull)

  if (is.null(SQLitePath) && is.null(SQLitefn)) {
    SQLitefn <- "data"
  }

  if (!is.null(outfn.pre)) {
    SQLitefn <- paste(outfn.pre, SQLitefn, sep="_")
  }
  
  if (is.na(getext(SQLitefn)) || getext(SQLitefn) == "NA") {
    SQLitefn <- paste0(SQLitefn, dbext)
  }
  if (!dir.exists(dirname(SQLitefn))) {
    stop("invalid directory path") 
  }
  SQLitepath <- getoutfn(SQLitefn, outfn.date=outfn.date, 
		outfolder=outfolder, overwrite=overwrite, ext="sqlite")

  ## Overwrite file
  if (file.exists(SQLitepath)) {
    if (overwrite) {
      file.remove(SQLitepath) 
      message("overwriting database... ", SQLitepath)
    } else {
      sqlconn <- DBtestSQLite(SQLitefn=SQLitefn, gpkg=gpkg, dbconnopen=dbconnopen,
		showlist=FALSE)
      if (dbconnopen) return(sqlconn)    
    }
  } else {
    ## Connect to database
    message("creating new SQLite database... ")
    message(SQLitepath)
    sqlconn <- DBI::dbConnect(RSQLite::SQLite(), SQLitepath, loadable.extensions = TRUE)

    if (dbconnopen) {
      return(sqlconn)
    } else {
      DBI::dbDisconnect(sqlconn)
      if (returnpath) {
        return(normalizePath(SQLitepath))
      } else {
        return(basename(SQLitepath))
      }
    }
  }

  if (returnpath) {
    return(SQLitepath)
  } else {
    return(basename(SQLitepath))
  }
}
