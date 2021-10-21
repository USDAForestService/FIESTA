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
#' @param rawdata Logical. If TRUE, returns a list of raw data tables that are
#' used for estimation (See Value). If savedata = TRUE, tables are written to
#' outfolder (if raw_fmt='csv') or raw_dsn (if raw_fmt != 'csv').
#' @param rawonly Logical. If TRUE, only rawdata are output. If dataset
#' includes many estimation units, and only raw data tables are desired, it is
#' more efficient to output raw data only.
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
                             addtitle=TRUE, rawdata=FALSE, rawonly=FALSE, raw_fmt="csv",
                             raw_dsn=NULL, overwrite_dsn=FALSE, overwrite_layer=TRUE,
                             append_layer=FALSE, returntitle=FALSE, title.main=NULL,
                             title.ref=NULL, title.rowvar=NULL, title.colvar=NULL,
                             title.unitvar=NULL, title.estvar=NULL, title.filter=NULL, ...) {
  # check if list -- todo
  l <- as.list(match.call())
  l <- l[-1]
  return(l)
}

# formals(savedata_options)[-length(formals(savedata_options))]

# opts <- savedata_options(other_param = "test",
#                                  hi = "boop",
#                                  this_thing = NULL)
# 
# for (i in 1:length(opts)) {
#   assign(names(opts)[[i]], opts[[i]])
# }
