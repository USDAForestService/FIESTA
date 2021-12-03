#' Title output options.
#' 
#' Returns a list of user-supplied parameters and parameter values for outputting
#' title with custom aesthetics.
#' 
#' If no parameters, an empty list is returned.
#' 
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
#' @param title.estvarn String. TITLE: if savedata=TRUE and/or returntitle=TRUE:
#' pretty name for the estimate variable. If NULL, title.estvar = estvar.name.
#' @param title.filter String. TITLE, if savedata=TRUE and/or returntitle=TRUE:
#' pretty name for filter(s). If title.filter=NULL, a default is generated from
#' cfilter.  If title.filter="", no title.filter is used.
#' @param title.units String. 
#' @param ... For extendibility.
#' @return A list of user-supplied parameters and parameter values for outputting
#' titles with custom aesthetics.
#' @author Grayson W. White
#' @keywords options
#' @examples
#' 
#' title_options(title.main = "My fancy title", title.estvar = "Estimate title")
#' 
#' @export title_options

title_options <- function(title.main=NULL, title.ref=NULL, title.rowvar=NULL,
                          title.colvar=NULL, title.unitvar=NULL,
                          title.estvar=NULL, title.estvarn=NULL,
                          title.filter=NULL, title.units="acres",...) {
  # Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- c(names(formals(title_options)))
  if (!all(input.params %in% formallst)) {
    miss <- input.params[!input.params %in% formallst]
    stop("invalid parameter: ", toString(miss))
  }
  
  # create list from input parameters
  l <- c(as.list(environment()), list(...))
  
  # return list
  return(l)
}
