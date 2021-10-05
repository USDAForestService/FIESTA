#' Database - Test a SQLite database table.
#' 
#' Checks a SQLite database.
#' 
#' 
#' @param dbname String. Name of PostgreSQL database.
#' @param dbconnopen Logical. If TRUE, the dbconn connection is not closed.
#' @author Tracey S. Frescino
#' @keywords data
#' @export DBtestPostgreSQL
DBtestPostgreSQL <- function(dbname, dbconnopen=TRUE) {

  if (!"DBI" %in% rownames(installed.packages())) {
    message("accessing SQLite databases requires package DBI")
  }
  if (!"RPostgreSQL" %in% rownames(installed.packages())) {
    message("accessing PostgreSQL databases requires package RPostgreSQL")
  }
  ## CONNECT TO ORACLE DATABASE
  ###########################################
  if (DBI::dbCanConnect(RPostgreSQL::PostgreSQL(), dbname=dbname)) {
    message("PostgreSQL connection successful")

    if (dbconnopen) {
      return(DBI::dbConnect(RPostgreSQL::PostgreSQL(), dbname=dbname))
    }     
  } else {
    stop("PostgreSQL connection failed")
  }
}
