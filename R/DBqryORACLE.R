DBqryORACLE <- function(sql, dbconn=NULL, dbconnopen=FALSE){

  if (!"DBI" %in% rownames(installed.packages()))
    stop("DBgetEvalid function requires package DBI when datsource='ORACLE'")


  ## SET OPTIONS
  options.old <- options()
  options(scipen=8) # bias against scientific notation
  on.exit(options(options.old), add=TRUE) 

  ## Stop if no arguments passed. No GUI available for this function
  if (nargs() == 0) stop("must include sql")

  ## CONNECT to database
  ########################################################
  if (is.null(dbconn)) {
    dbconn <- DBtestORACLE(dbconnopen=TRUE)
  } else {
    if (!DBI::dbIsValid(dbconn))
      message("Oracle connection is invalid")
  }      

  ## Query dataset
  dat <- DBI::dbGetQuery(dbconn, sql)

  ## Close database connection
  if (!dbconnopen) 
    DBI::dbDisconnect(dbconn)

  return(dat)
}
