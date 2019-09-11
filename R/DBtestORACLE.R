DBtestORACLE <- function(dbconnopen=FALSE) {

  if (!"DBI" %in% rownames(installed.packages()))
    stop("accessing Oracle requires package DBI")
  
  ## CONNECT TO ORACLE DATABASE
  ###########################################
  if (DBI::dbCanConnect(odbc::odbc(), "FIA01P")) {
    message("Oracle connection successful")

    if (dbconnopen) 
      return(DBI::dbConnect(odbc::odbc(), "FIA01P"))     
  } else {
    stop("Oracle connection failed")
  }
}
