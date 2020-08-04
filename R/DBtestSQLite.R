DBtestSQLite <- function(SQLitefn=NULL, gpkg=FALSE, dbconnopen=FALSE, 
	outfolder=NULL, showlist=TRUE, returnpath=TRUE, createnew=TRUE,
	stopifnull=FALSE) {
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
  dbext <- ifelse(gpkg, ".gpkg", ".sqlite")

  if (is.null(SQLitefn)) stop("SQLitefn is NULL")
  SQLitepath <- SQLitefn

  if (is.na(getext(SQLitefn)) || getext(SQLitefn) == "NA")
    SQLitefn <- paste0(SQLitefn, dbext)

  outfolder <- pcheck.outfolder(outfolder, default=NULL)
  if (!is.null(outfolder)) 
    SQLitepath <- file.path(outfolder, SQLitefn)

  ## Check file
  if (file.exists(SQLitepath)) {
    if (DBI::dbCanConnect(RSQLite::SQLite(), SQLitepath)) {
      message("SQLite connection successful")

      sqlconn <- DBI::dbConnect(RSQLite::SQLite(), SQLitepath, loadable.extensions = TRUE)
      tablst <- DBI::dbListTables(sqlconn)
      if (showlist)
        print(tablst)
      if (length(tablst) != 0 && "SpatialIndex" %in% tablst) {
        message(paste(SQLitepath, "is a Spatialite database... "))
        sf::st_layers(SQLitepath)
      } 
    } else {
        stop("SQLite connection failed")
    }
    if (dbconnopen) {
      ## Connect to database
      sqlconn <- DBI::dbConnect(RSQLite::SQLite(), SQLitepath, loadable.extensions = TRUE)
      return(sqlconn)    
    }
  } else {
    if (createnew) {
      SQLitepath <- DBcreateSQLite(SQLitefn=SQLitepath, returnpath=TRUE)
    } else if (stopifnull) {
      stop("SQLite database does not exist")
    } else {
      return(NULL)
    }
  }

  if (returnpath) {
    return(SQLitepath)
  } else {
    return(basename(SQLitepath))
  }
}
