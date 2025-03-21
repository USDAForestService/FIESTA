#' Database - Queries FIA Online Database.
#' 
#' Downloads, extracts, and queries compressed comma-delimited file(s) (*.zip)
#' from FIA DataMart
#' (https://apps.fs.usda.gov/fia/datamart/CSV/datamart_csv.html).  (Note: must
#' use SQL syntax).
#' 
#' The compressed data files are downloaded from FIA DataMart; saved to a
#' temporary space; extracted and imported; and deleted from temporary space.
#' Accessibility and download time depends on access and speed of internet
#' connection.
#' 
#' @param sql String. A sql query. Must be appropriate sql syntax.
#' @param states String vector. Name of state(s) in query. If not by state, set
#' to NULL.
#' @param sqltables String vector. Name of table(s) in sql statement to
#' download.  The sqltables must match tables in the sql statement (i.e.,
#' case-sensitive).
#' @return Returns a data frame from resulting query.
#' @author Tracey S. Frescino
#' @keywords data
#' @examples
#' \dontrun{
#' # Number of plots by inventory year for the state of Wyoming
#' sql <- "select INVYR, count(*) AS NBRPLOTS
#'         from plot
#'         where statecd=56 group by INVYR"
#' DBqryCSV(sql = sql,
#'          states = "Wyoming",
#'          sqltables = "plot")
#' }
#' @export DBqryCSV
DBqryCSV <- function(sql, states=NULL, sqltables=NULL) {

  ZIP <- TRUE 

  ## Stop if no arguments passed. No GUI available for this function
  if (nargs() == 0) stop("must include sql")


  ## Check states and sqltables
  states <- pcheck.states(states)

  ## Check ZIP (note: this was previously set up as a parameter)
  ZIP <- pcheck.logical(ZIP, varnm="ZIP", title="Zip files?", first="YES")

  if (is.null(sqltables)) {
    stop("must include sqltables")
  }
  if (!is.character(sqltables)) {
    stop("sqltables must be character vector")
  }

  ## Check input tables match tables in sql query
  nbrtabs <- length(sqltables)
  tabfind <- sapply(sqltables, grepl, sql)
  if (sum(tabfind) < length(sqltables)) {
    tabfind2 <- sapply(sqltables, grepl, sql, ignore.case=TRUE)
    if (sum(tabfind) != length(sqltables)) stop("check case of sqltables")
    if (sum(tabfind2) != length(sqltables)) stop("check sqltables")
  }

  ## Get tables
  for (tab in sqltables) {
    tb <- DBgetCSV(DBtable = tab, states = states)
    if (!is.null(tb)) {
      assign(tab, tb)
    }

  }

  ## Query dataset
  sql <- gsub("\n", " ", sql)
  dat <- sqldf::sqldf(sql, stringsAsFactors=FALSE)

  return(dat)
}
