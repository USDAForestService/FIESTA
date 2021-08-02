datExportData <- function(dfobj, outfolder=NULL, out_fmt="csv", out_dsn=NULL,
 	out_layer=NULL, outfn.pre=NULL, layer.pre=NULL, outfn.date=FALSE, 
	create_dsn=FALSE, overwrite_dsn=FALSE, overwrite_layer=FALSE, 
	add_layer=TRUE, append_layer=FALSE, index.unique=NULL, index=NULL) {
  ###########################################################################
  ## DESCRIPTION: Exports a data.frame to file or database.
  ## out_fmt	Output format ('csv', 'sqlite', 'gpkg', 'shp')		
  ## out_dsn	Database file path (including extension or outfolder
  ## out_layer	Only include if out_dsn is a database (e.g., *.sqlite, *.gdb)
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
		returnpath=TRUE)
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
