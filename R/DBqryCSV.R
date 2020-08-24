DBqryCSV <- function(sql, ZIP=TRUE, states=NULL, sqltables=NULL){

  if (!"sqldf" %in% rownames(installed.packages()))
    stop("DBgetEvalid function requires package sqldf when datsource='CSV'")


  ## SET OPTIONS
  options.old <- options()
  options(scipen=8) # bias against scientific notation
  on.exit(options(options.old), add=TRUE) 

  ## Stop if no arguments passed. No GUI available for this function
  if (nargs() == 0) stop("must include sql")


  ## Check states and sqltables
  states <- FIESTA::pcheck.states(states)

  ## Check ZIP
  ZIP <- FIESTA::pcheck.logical(ZIP, varnm="ZIP", title="Zip files?", first="YES")

  if (is.null(sqltables)) stop("must include sqltables")
  if (!is.character(sqltables)) stop("sqltables must be character vector")

  ## Check tables witih sql query
  nbrtabs <- length(sqltables)
  tabfind <- sapply(sqltables, grepl, sql)
  if (sum(tabfind) < length(sqltables)) {
    tabfind2 <- sapply(sqltables, grepl, sql, ignore.case=TRUE)
    if (sum(tabfind) != length(sqltables)) stop("check case of sqltables")
    if (sum(tabfind2) != length(sqltables)) stop("check sqltables")
  }

  ## Get tables
  for (tab in sqltables) 
    assign(tab, FIESTA::DBgetCSV(DBtable=tab, states=states, ZIP=ZIP))

  ## Query dataset
  sql <- gsub("\n", " ", sql)
  dat <- sqldf::sqldf(sql, stringsAsFactors=FALSE)


  return(dat)
}
