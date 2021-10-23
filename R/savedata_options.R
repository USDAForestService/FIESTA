#' Data saving options.
#' 
#' Returns a list of user-supplied parameters and parameter values for saving data.
#' 
#' If no parameters, an empty list is returned.
#' 
#' @param outfolder String. The outfolder to write files to. If NULL, files are
#' written to working directory, or if gui, a window to browse.
#' @param outfn.pre String. If savedata=TRUE, prefix for output files. If
#' rawdata=TRUE, prefix for rawdata files (if raw_fmt = 'csv') or raw_dsn (if
#' raw_fmt != 'csv').
#' @param outfn.date Logical. If TRUE, add current date to out_dsn.
#' @param addtitle Logical. If TRUE and savedata=TRUE, adds title to outfile.
#' @param raw_fmt String. Format for output rawdata tables ('sqlite',
#' 'sqlite3', 'db', 'db3', 'gpkg', 'csv', 'gdb', 'shp').
#' @param raw_dsn String. Data source name for rawdata output. If extension is
#' not included, out_fmt is used. Use full path if outfolder=NULL.
#' @param overwrite_dsn Logical. If TRUE, overwrites raw_dsn, if exists.
#' @param overwrite_layer Logical. If TRUE, overwrites the output. If
#' rawdata=TRUE, overwrites out_layer in rawdata folder (if raw_fmt = 'csv') or
#' out_layers in raw_dsn (if raw_fmt != 'csv').
#' @param append_layer Logical. If TRUE, and rawdata=TRUE, appends raw data to
#' existing *.csv files (if raw_fmt = 'csv') or raw_dsn layers (if raw_fmt !=
#' 'csv".
#' @param ... For extendibility.
#' @return A list of user-supplied parameters and parameter values for saving data.
#' @author Grayson W. White
#' @keywords options
#' @examples
#' 
#' savedata_options(outfolder = "path", overwrite_dsn = FALSE)
#' 
#' @export savedata_options

savedata_options <- function(outfolder=NULL, outfn.pre=NULL, outfn.date=FALSE,
                             addtitle=TRUE, raw_fmt="csv",
                             raw_dsn=NULL, overwrite_dsn=FALSE, overwrite_layer=TRUE,
                             append_layer=FALSE, ...) {
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

