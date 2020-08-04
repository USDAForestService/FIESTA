DBtestPostgreSQL <- function(dbname, dbconnopen=TRUE) {

  if (!"DBI" %in% rownames(installed.packages()))
    stop("accessing SQLite databases requires package DBI")
  if (!"RPostgreSQL" %in% rownames(installed.packages()))
    stop("accessing PostgreSQL databases requires package RPostgreSQL")
  
  ## CONNECT TO ORACLE DATABASE
  ###########################################
  if (DBI::dbCanConnect(RPostgreSQL::PostgreSQL(), dbname=dbname)) {
    message("PostgreSQL connection successful")

    if (dbconnopen) 
      return(DBI::dbConnect(RPostgreSQL::PostgreSQL(), dbname=dbname))     
  } else {
    stop("PostgreSQL connection failed")
  }
}
