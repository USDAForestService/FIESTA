datExportData <- function(dfobj, outfolder=NULL, out_fmt="csv", out_dsn=NULL,
 	out_layer=NULL, outfn.pre=NULL, outfn.date=FALSE, create_dsn=FALSE,
	overwrite_dsn=FALSE, overwrite_layer=FALSE, append_layer=FALSE, 
	index.unique=NULL, index=NULL) {
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
  if (!"data.frame" %in% class(dfobj))
    stop("the object must be of class data frame")


  ## Check out_fmt
  ###########################################################
  out_fmtlst <- c('sqlite', 'gpkg', 'csv', 'gdb')
  out_fmt <- pcheck.varchar(out_fmt, varnm="out_fmt", checklst=out_fmtlst, 
		caption="Out format", gui=gui)

  ## Check for necessary packages
  ###########################################################
  if (out_fmt %in% c("sqlite", "gpkg")) {
    if (!"RSQLite" %in% rownames(installed.packages()))
      stop("RSQLite package is required for datExportData")
  } else if (out_fmt %in% c("gdb")) {
    if (!"arcgisbinding" %in% rownames(installed.packages()))
      stop("arcgisbinding package is required for datExportData")
    arcgisbinding::arc.check_product()
  }

  ## Check outfolder, overwrite_dsn, overwrite_layer, outfn.date 
  ########################################################
  outfolder <- pcheck.outfolder(outfolder, default=NULL)
  overwrite_dsn <- FIESTA::pcheck.logical(overwrite_dsn, varnm="overwrite_dsn", 
		title="Overwrite dsn?", first="NO", gui=gui)  
  overwrite_layer <- FIESTA::pcheck.logical(overwrite_layer, varnm="overwrite_layer", 
		title="Overwrite layer?", first="NO", gui=gui)  
  outfn.date <- FIESTA::pcheck.logical(outfn.date , varnm="outfn.date", 
		title="Add date to dsn name?", first="YES", gui=gui)  
  append_layer <- FIESTA::pcheck.logical(append_layer, varnm="append_layer", 
		title="Append to dsn?", first="YES", gui=gui) 

  if (append_layer) 
    overwrite_dsn <- FALSE
    
  ## Check out_layer
  ####################################################
  if (is.null(out_layer))
    out_layer <- basename.NoExt(out_dsn) 

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
    out_dsn <- DBtestESRIgdb(out_dsn, outfolder=outfolder, 
		overwrite=overwrite_dsn, outfn.date=outfn.date, showlist=FALSE)

    arcgisbinding::arc.write(file.path(out_dsn, out_layer), dfobj, 
			overwrite=overwrite_layer)
    
  } else if (out_fmt == "csv") {

    write2csv(dfobj, outfolder=outfolder, outfilenm=out_layer, 
		outfn.pre=outfn.pre, outfn.date=outfn.date, overwrite=overwrite_layer,
		appendfile=append_layer)

  } else {
    stop(out_fmt, " currently not supported")
  }  
}
