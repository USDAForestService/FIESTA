#' Table aesthetics and output options.
#' 
#' Returns a list of user-supplied parameters and parameter values for outputting
#' tables with custom aesthetics.
#' 
#' If no parameters, an empty list is returned.
#' 
#' @param row.FIAname Logical. If TRUE, retrieves default FIA reference names
#' for rowvar located in FIESTA::ref_codes data frame. Names are only available
#' for certain variables (Check sort(unique(FIESTA::ref_codes$VARIABLE)) for
#' available names.  If row.FIAname = TRUE and rowvar is in FIESTA::ref_codes,
#' the rowvar name is used for the output table, and the rowvar code is used to
#' sort.
#' @param col.FIAname Logical. If TRUE, retrieves default FIA reference names
#' for colvar located in FIESTA::ref_codes data frame. Names are only available
#' for certain variables. Check: sort(unique(FIESTA::ref_codes$VARIABLE)) for
#' available names.  If col.FIAname = TRUE and rowvar is in FIESTA::ref_codes,
#' the colvar name is used for the output table, and the colvar code is used to
#' sort.
#' @param row.orderby String. Optional. Name of variable to sort table rows.
#' Both the rowvar and row.orderby variables must be included in the same input
#' data.frame.  if NULL, and row.FIAname=FALSE or rowvar is not in
#' FIESTA::ref_codes, the rows are ordered by rowvar.
#' @param col.orderby String. Optional. Name of variable to sort table columns.
#' Both the colvar and col.orderby variables must be included in the same input
#' data.frame.  if NULL, and col.FIAname=FALSE or colvar is not in
#' FIESTA::ref_codes, the columns are ordered by colvar.
#' @param row.add0 Logical. If TRUE, include rows with 0 values to the output
#' table.
#' @param col.add0 Logical. If TRUE, include columns with 0 values to the
#' output table.
#' @param rowlut Data frame. A lookup table with variable codes and code names
#' to include as rows of output table (See notes for more information and
#' format).
#' @param collut Data frame. A lookup table with variable codes and code names
#' to include as columns of output table (See notes for more information and
#' format).
#' @param rawdata Logical. If TRUE, returns a list of raw data tables that are
#' used for estimation (See Value). If savedata = TRUE, tables are written to
#' outfolder (if raw_fmt='csv') or raw_dsn (if raw_fmt != 'csv').
#' @param rawonly Logical. If TRUE, only rawdata are output. If dataset
#' includes many estimation units, and only raw data tables are desired, it is
#' more efficient to output raw data only.
#' @param rowgrp Logical. If TRUE, appends row groups to first column of table.
#' Only available if group category exists in ref_codes table or defined in
#' rowgrpnm (e.g., FORTYPGRPCD, OWNGRPCD).
#' @param rowgrpnm String. Name of variable for grouping rowvar. Variable must
#' be included in same input table as rowvar.
#' @param rowgrpord String. Name of variable to sort row group variable.
#' Variable must be included in same input table as rowgrpnm.
#' @param sumunits Logical. If TRUE, estimation units are summed and returned
#' in one table.
#' @param allin1 Logical. If TRUE, both estimates and percent sample error are
#' output in one table as: estimates (percent sample error).
#' @param metric Logical. If TRUE, output area is in metric units (hectares).
#' @param estround Integer. Number of decimal places for estimates.
#' @param pseround Integer. Number of decimal places for percent sampling
#' error.
#' @param estnull Number or character. The number or symbol to use to indicate
#' 'not sampled' for estimate.
#' @param psenull Number or character. The number or symbol to use to indicate
#' 'not sampled' for percent standard errror.
#' @param divideby String. Conversion number for output ('hundred', 'thousand',
#' 'million').
#' @param returntitle Logical. If TRUE, returns title(s) of the estimation
#' table(s).
#' @param title.main String. TITLE, if savedata=TRUE and/or returntitle=TRUE:
#' the complete title used for table. If title.main=NULL, the title.*
#' parameters are used to generate title string. Note: if title.ref is not
#' NULL, it is added to title.main.
#' @param title.ref String. TITLE, if savedata=TRUE and/or returntitle=TRUE:
#' the ending text of the table title (e.g., Nevada, 2004-2005). If NULL, = "".
#' @param title.rowvar String. TITLE, if savedata=TRUE and/or returntitle=TRUE:
#' pretty name for the row domain variable. If NULL, = rowvar.
#' @param title.colvar String. TITLE, if savedata=TRUE and/or returntitle=TRUE:
#' pretty name for the column domain variable. If NULL, = colvar.
#' @param title.unitvar String. TITLE, if savedata=TRUE and/or
#' returntitle=TRUE: pretty name for the estimation unit variable. If NULL, =
#' unitvar.
#' @param title.estvar String. TITLE: if savedata=TRUE and/or returntitle=TRUE:
#' pretty name for the estimate variable. If NULL, title.estvar = estvar.name.
#' @param title.filter String. TITLE, if savedata=TRUE and/or returntitle=TRUE:
#' pretty name for filter(s). If title.filter=NULL, a default is generated from
#' cfilter.  If title.filter="", no title.filter is used.
#' @param gainloss Logical. If TRUE, a table with the difference of gain and
#' loss along with the variance and standard error, in percent, is generated.
#' Used in modGBlulc.
#' @param gainloss.vals String vector. A vector of names for values in gainloss
#' table. Used in modGBlulc.
#' @param ... For extendibility.
#' @return A list of user-supplied parameters and parameter values for outputting
#' tables with custom aesthetics.
#' @author Grayson W. White
#' @keywords options
#' @examples
#' 
#' table_options(row.FIAnames = TRUE, col.FIAname = TRUE)
#' 
#' @export table_options

table_options <- function(row.FIAname=FALSE, col.FIAname=FALSE, row.orderby=NULL,
                          col.orderby=NULL, row.add0=FALSE, col.add0=FALSE,
                          rowlut=NULL, collut=NULL, rawdata=FALSE, rawonly=FALSE,
                          rowgrp=FALSE, rowgrpnm=NULL, rowgrpord=NULL, sumunits=TRUE,
                          allin1=FALSE, metric=FALSE, estround=1, pseround=2,
                          estnull="--", psenull="--", divideby=NULL,
                          returntitle=FALSE, title.main=NULL, title.ref=NULL,
                          title.rowvar=NULL, title.colvar=NULL, title.unitvar=NULL,
                          title.estvar=NULL, title.filter=NULL, gainloss=FALSE,
                          gainloss.vals=NULL, ...) {
  # set up list of parameters
  l <- as.list(match.call())
  l <- l[-1]
  
  # this evaluates objects in the user's global environment and saves them back
  # into the list in order to pass them correctly to other functions
  objs <- ls(envir = globalenv())
  for (i in 1:length(l)) {
    if (class(l[i]) == "name") {
      if (l[i] %in% objs) {
        l[i] <- eval(l[i][[1]], envir = globalenv())
      }
    }
  }
  
  # returns the list
  return(l)
}
