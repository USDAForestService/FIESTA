#' Database - Checks access to a SQLite database.
#' 
#' Checks a SQLite database.
#' 
#' 
#' @param SQLitefn String. Name of SQLite database (*.sqlite).
#' @param gpkg Logical. If TRUE, Sqlite geopackage database.
#' @param dbconnopen Logical. If TRUE, the dbconn connection is not closed.
#' @param outfolder String. Optional. Name of output folder. If NULL, export to
#' working directory.
#' @param showlist Logical. If TRUE, shows list of tables in database.
#' @param returnpath Logical. If TRUE, returns full path to SQLite file name.
#' If FALSE, returns SQLitefn.
#' @param createnew If TRUE, creates new SQLite database.
#' @param stopifnull Logical. If TRUE, stops if SQLite database doesn't exist.
#' @param overwrite Logical. If TRUE, overwrites data.
#' @author Tracey S. Frescino
#' @keywords data
#' @export DBtestSQLite
DBtestSQLite <- function(SQLitefn = NULL, 
                         gpkg = FALSE, 
                         dbconnopen = FALSE, 
                         outfolder = NULL, 
                         showlist = TRUE, 
                         returnpath = TRUE, 
                         createnew = TRUE, 
                         stopifnull = FALSE, 
                         overwrite = TRUE) {
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

  
  ##################################################################
  ## CHECK PARAMETER NAMES
  ##################################################################
  input.params <- names(as.list(match.call()))[-1]
  if (!all(input.params %in% names(formals(DBtestSQLite)))) {
    miss <- input.params[!input.params %in% formals(DBtestSQLite)]
    stop("invalid parameter: ", toString(miss))
  }
  
  ## Check gpkg
  dbext <- ifelse(gpkg, ".gpkg", ".db")

  ## Check filename
  SQLitepath <- checkfilenm(SQLitefn, outfolder)

  if (is.null(SQLitepath)) {
    if (createnew) {
      SQLitepath <- DBcreateSQLite(SQLitefn=SQLitefn, outfolder=outfolder, 
		returnpath=TRUE)
    } else if (stopifnull) {
      stop(SQLitefn, " does not exist")
    } else if (returnpath) {
      if (is.null(outfolder)) {
        outfolder <- normalizePath(dirname(SQLitefn))
        basenm <- basename(SQLitefn)
      } else {
        outfolder <- normalizePath(outfolder)
        basenm <- basename(SQLitefn)
      }
      return(file.path(outfolder, basenm))
    } else {
      return(NULL)
    }
  } else {
    if (is.na(getext(SQLitefn)) || getext(SQLitefn) == "NA") {
      SQLitefn <- paste0(SQLitefn, dbext)
    }
    if (DBI::dbCanConnect(RSQLite::SQLite(), SQLitepath)) {
      message("SQLite connection successful")
      sqlconn <- DBI::dbConnect(RSQLite::SQLite(), SQLitepath, loadable.extensions = TRUE)
      tablst <- DBI::dbListTables(sqlconn)
      if (length(tablst) != 0 && "SpatialIndex" %in% tablst) {
        message(paste(SQLitepath, "is a Spatialite database... "))
        tablst <- sf::st_layers(SQLitepath)
      } 
      if (showlist) message(paste0(capture.output(tablst), collapse = "\n"))
    } else {
      stop("SQLite connection failed")
    }
    if (dbconnopen) {
      ## Connect to database   
      sqlconn <- DBI::dbConnect(RSQLite::SQLite(), SQLitepath, loadable.extensions = TRUE)
      return(sqlconn)    
    }
  }
  if (returnpath) {
    return(SQLitepath)
  } else {
    return(basename(SQLitepath))
  }
}
