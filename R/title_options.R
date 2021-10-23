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
#' @param title.filter String. TITLE, if savedata=TRUE and/or returntitle=TRUE:
#' pretty name for filter(s). If title.filter=NULL, a default is generated from
#' cfilter.  If title.filter="", no title.filter is used.
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
                          title.estvar=NULL, title.filter=NULL, ...) {
  # set up list of parameters
  l <- as.list(match.call())
  l <- l[-1]
  
  # this evaluates objects in the user's global environment and saves them back
  # into the list in order to pass them correctly to other functions
  objs <- ls(envir = globalenv())
  for (i in 1:length(l)) {
    if (class(l[[i]]) == "name") {
      if (l[i] %in% objs) {
        l[i] <- eval(l[i][[1]], envir = globalenv())
      }
    }
  }
  
  # returns the list
  return(l)
}
