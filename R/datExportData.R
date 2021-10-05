#' Spatial - Exports a data frame object.
#' 
#' Exports a data frame object to a specified output.
#' 
#' Wrapper for sf::st_write function.
#' 
#' @param dfobj Data.frame class R object. Data frame object to export.
#' @param outfolder String. Optional. Name of output folder. If NULL, export to
#' working directory.
#' @param out_fmt String. File format for output ('csv', 'sqlite','gpkg',
#' 'gdb').  If out_fmt %in% c('sqlite','gpkg'), RSQLite package must be
#' installed. If out_fmt='gdb', arcgisbinding package and R-Bridge must be
#' installed.
#' @param out_dsn String. Data source name for output. If extension is not
#' included, out_fmt is used. Use full path if outfolder=NULL.
#' @param out_layer String. Name of layer in out_dsn. If NULL, basename of
#' out_dsn is used.
#' @param outfn.pre String. Prefix for out_dsn or csv file, if out_fmt=".csv".
#' @param layer.pre String. Prefix for layer in database, if out_fmt != ".csv".
#' @param outfn.date Logical. If TRUE, add current date to out_dsn.
#' @param create_dsn Logical. If TRUE, creates new database.
#' @param overwrite_dsn Logical. If TRUE, overwrites the out_dsn, if exists.
#' @param overwrite_layer Logical. If TRUE, overwrites the out_layer, if
#' exists.
#' @param add_layer Logical. If TRUE, adds to existing out_dsn (if out_fmt !=
#' c('csv','shp')).
#' @param append_layer Logical. If TRUE, appends to a layer in an existing
#' out_dsn or a csv or shapefile. Note: currently cannot append to layer in
#' out_dsn when out_fmt="gdb".
#' @param index.unique String. Name of variable(s) in dfobj to make unique
#' index.
#' @param index String. Name of variable(s) in dfobj to make (non-unique)
#' index.
#' @return An sf spatial object is written to the out_dsn.
#' @note If out_fmt='shp':\cr The ESRI shapefile driver truncates variable
#' names to 10 characters or less.  Variable names are changed before export
#' using an internal function (trunc10shp). Name changes are output to the
#' outfolder, 'outshpnm'_newnames.csv.
#' 
#' If sf object has more than 1 record, it cannot be exported to a shapefile.
#' @author Tracey S. Frescino
#' @keywords data
#' @export datExportData
datExportData <- function(dfobj, outfolder=NULL, out_fmt="csv", out_dsn=NULL,
 	out_layer=NULL, outfn.pre=NULL, layer.pre=NULL, outfn.date=FALSE, 
	create_dsn=FALSE, overwrite_dsn=FALSE, overwrite_layer=FALSE, 
	add_layer=TRUE, append_layer=FALSE, index.unique=NULL, index=NULL) {
  ###########################################################################
  ## DESCRIPTION: Exports a data.frame to file or database.
  ## out_fmt	Output format ('csv', 'sqlite', 'gpkg', 'shp')		
  ## out_dsn	Database file path (including extension or outfolder
  ## out_layer	Only include if out_dsn is a database (e.g., *.db, *.gdb)
  ##			If NULL, basename of out_dsn is used
  ## outfn.pre	Add a prefix to layer name
  ## index.unique Unique index for dfobj (if out_fmt = "sqlite")
  ## index 		Index for dfobj (if out_fmt = "sqlite")
  ###########################################################################

  ##################################################################
  ## CHECK INPUT PARAMETERS
  ##################################################################

  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- ifelse(nargs() == 0, TRUE, FALSE)


  ## Check dfobj
  ###########################################################
  if (is.null(dfobj)) {
    dfnm <- select.list(ls(pos=1, all.names=TRUE), title="data frame object?", 
		multiple=FALSE)
    dfobj <- get(dfnm)
  }
  if (!"data.frame" %in% class(dfobj)) {
    stop("the object must be of class data frame")
#  } else if ("data.table" %in% class(dfobj)) {
#    dfobj <- setDF(dfobj)
  }
 
  ## Check output data
  outlst <- pcheck.output(out_fmt=out_fmt, outfolder=outfolder, 
	out_dsn=out_dsn, overwrite_dsn=overwrite_dsn, overwrite_layer=overwrite_layer,
	outfn.date=outfn.date, add_layer=add_layer, append_layer=append_layer, 
	outfn.pre=outfn.pre)
  out_fmt <- outlst$out_fmt
  out_dsn <- outlst$out_dsn
  outfolder <- outlst$outfolder
  overwrite_layer <- outlst$overwrite_layer
  append_layer <- outlst$append_layer

  ## Check out_layer
  ####################################################
  if (is.null(out_layer)) {
    out_layer <- basename.NoExt(out_dsn)
  }
  if (!is.null(layer.pre)) {
    out_layer <- paste0(layer.pre, "_", out_layer)
  }

  ## Write data frame
  ########################################################
  if (out_fmt %in% c("sqlite", "gpkg")) {
    gpkg <- ifelse(out_fmt == "gpkg", TRUE, FALSE)
    if (create_dsn) {
      out_dsn <- DBcreateSQLite(out_dsn, gpkg=gpkg, outfolder=outfolder, 
		overwrite=overwrite_dsn, outfn.date=outfn.date,
		returnpath=TRUE)
    } else {
      out_dsn <- DBtestSQLite(out_dsn, gpkg=gpkg, outfolder=outfolder, 
		returnpath=TRUE, showlist=FALSE)
    }
    write2sqlite(dfobj, SQLitefn=out_dsn, out_name=out_layer, gpkg=gpkg, 
		overwrite=overwrite_layer, append_layer=append_layer, 
		index.unique=index.unique, index=index)    
 
  } else if (out_fmt == "gdb") {
    message(out_layer, " written to ", out_dsn)
    out_dsn <- DBtestESRIgdb(out_dsn, outfolder=outfolder, 
		overwrite=overwrite_dsn, outfn.date=outfn.date, showlist=FALSE)
    arcgisbinding::arc.write(file.path(out_dsn, out_layer), dfobj, 
			overwrite=overwrite_layer)
    
  } else if (out_fmt == "csv") {
    write2csv(dfobj, outfolder=outfolder, outfilenm=out_layer, 
		outfn.pre=outfn.pre, outfn.date=outfn.date, overwrite=overwrite_layer,
		appendfile=append_layer)

  } else if (out_fmt == "rda") {
    objfn <- getoutfn(outfn=out_layer, outfolder=outfolder, outfn.pre=outfn.pre,
		outfn.date=outfn.date, overwrite=overwrite_layer, ext="rda")
    save(dfobj, file=objfn)
  } else {
    stop(out_fmt, " currently not supported")
  }  
}
