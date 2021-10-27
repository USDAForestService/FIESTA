#' Multest output options.
#' 
#' Returns a list of user-supplied parameters and parameter values for outputting
#' multest with custom aesthetics.
#' 
#' If no parameters, an empty list is returned.
#' 
#' @param multest_fmt String. Format for multest output tables ('csv',
#' 'sqlite', 'gpkg').
#' @param multest_outfolder String. Outfolder for multest. If NULL, same as
#' outfolder.
#' @param multest_dsn String. Name of database if multest_fmt = c('sqlite',
#' 'gpkg').
#' @param multest_layer String. Name of database layer if multest_fmt =
#' c('sqlite', 'gpkg').
#' @param multest.append Logical. If TRUE, appends multest dataframe to output.
#' @param multest.AOIonly Logical. If TRUE, appends multest dataframe (AOI=1)
#' to output.
#' @param ... For extendibility.
#' @return A list of user-supplied parameters and parameter values for outputting
#' multest. 
#' @author Grayson W. White
#' @keywords options
#' @examples
#' 
#' multest_options(multest.append = TRUE)
#' 
#' @export multest_options

multest_options <- function(multest_fmt="csv", multest_outfolder=NULL, 
                          multest_dsn=NULL, multest_layer=NULL,
                          multest.append=FALSE, multest.AOIonly=FALSE, ...) {
  # set up list of parameters
  l <- as.list(match.call())
  l <- l[-1]
  
  # this evaluates objects in the user's global environment and saves them back
  # into the list in order to pass them correctly to other functions
  objs <- ls(envir = globalenv())
  if (length(l) > 0) {
    for (i in 1:length(l)) {
      if (class(l[[i]]) == "name") {
        if (l[i] %in% objs) {
          l[i] <- eval(l[i][[1]], envir = globalenv())
        }
      }
    }
  }
  
  # returns the list
  return(l)
}
