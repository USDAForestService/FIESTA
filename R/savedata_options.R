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
  ## Check addtitle
  addtitle <- pcheck.logical(
    addtitle,
    varnm = "addtitle",
    title = "Add title to output?",
    first = "YES",
    gui = gui,
    stopifnull = TRUE
  )
  ## Check output info
  ########################################################
  if (savedata) {
    if (!rawonly) {
      outlst <- pcheck.output(
        out_fmt = "csv",
        outfolder = outfolder,
        outfn.pre = outfn.pre,
        outfn.date = outfn.date,
        overwrite_layer = overwrite_layer,
        append_layer = append_layer,
        gui = gui
      )
      outfolder <- outlst$outfolder
      overwrite_layer <- outlst$overwrite_layer
      outfn.pre <- outfn.pre
    }
    ## Check raw_fmt
    if (raw_fmt) {
      raw_fmtlst <-
        c('sqlite', 'sqlite3', 'db', 'db3', 'gpkg', 'csv', 'gdb', 'shp')
      raw_fmt <-
        pcheck.varchar(
          raw_fmt,
          varnm = "raw_fmt",
          checklst = raw_fmtlst,
          caption = "Out raw format",
          gui = gui
        )
    }
    if (!is.null(raw_fmt) && raw_fmt == "csv") {
      rawfolder <- paste(outfolder, "rawdata", sep = "/")
      if (!file.exists(rawfolder))
        dir.create(rawfolder)
    } else {
      if (is.null(raw_dsn)) {
        raw_dsn <- "rawdata"
      }
      outlst <- pcheck.output(
        out_dsn = raw_dsn,
        out_fmt = raw_fmt,
        outfolder = outfolder,
        outfn.pre = outfn.pre,
        outfn.date = outfn.date,
        overwrite_dsn = overwrite_dsn,
        overwrite_layer = overwrite_layer,
        append_layer = append_layer,
        gui = gui
      )
      rawfolder <- outlst$outfolder
      raw_fmt <- outlst$out_fmt
      raw_dsn <- outlst$out_dsn
      overwrite_layer <- outlst$overwrite_layer
    }
  }
  
  
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

