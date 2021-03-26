DBcreateSQLite <- function(SQLitefn=NULL, gpkg=FALSE, dbconnopen=FALSE, 
	outfolder=NULL, outfn.pre=NULL, outfn.date=FALSE, overwrite=FALSE, 
	returnpath=TRUE, stopifnull=FALSE) {
  ## DESCRIPTION: 
  ## Test SQLite connection (SQLite or Geopackage database)
  ## ARGUMENTS:
  ## SQLitefn - String. SQLite filename (*.sqlite or *.gpkg)
  ## dbconnopen - Logical. If TRUE, keep connection to database open
  ## gpkg	- Logical. If TRUE, geopackage database.

  if (!"DBI" %in% rownames(installed.packages()))
    stop("accessing SQLite databases requires package DBI")
  if (!"RSQLite" %in% rownames(installed.packages()))
    stop("accessing SQLite databases requires package RSQLite")

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
      sqlconn <- DBtestSQLite(SQLitefn=SQLitefn, gpkg=gpkg, dbconnopen=dbconnopen)
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
