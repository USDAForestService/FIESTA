spExportSpatial <- function(sfobj, out_layer=NULL, out_fmt="shp", 
	outfolder=NULL, out_dsn=NULL, outfn.pre=NULL, outfn.date=FALSE, 
	overwrite_dsn=FALSE, overwrite_layer=TRUE, append_layer=FALSE) {
  ###########################################################################
  ## DESCRIPTION: Exports an S4 Spatial object to an ArcGIS shapefile (*.shp).
  ## out_fmt	Output format ('sqlite', 'gpkg', 'shp')		
  ## out_dsn	Database file path (including extension or outfolder
  ## out_layer	Only include if out_dsn is a database (e.g., *.sqlite, *.gdb)
  ##			If NULL, basename of out_dsn is used
  ## outfn.pre	Add a prefix to layer name
  ## 
  ## if out_fmt=shp   Also, variable names will be truncated to 10 characters.
  ###########################################################################

  ##################################################################
  ## CHECK INPUT PARAMETERS
  ##################################################################

  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- ifelse(nargs() == 0, TRUE, FALSE)

  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  if (!all(input.params %in% names(formals(spExportSpatial)))) {
    miss <- input.params[!input.params %in% formals(spExportSpatial)]
    stop("invalid parameter: ", toString(miss))
  }

  ## Check sfobj
  ###########################################################
  if (is.null(sfobj)) {
    sfnm <- select.list(ls(pos=1, all.names=TRUE), title="sf object?", 
		multiple=FALSE)
    sfobj <- get(sfnm)
  }
  if (!"sf" %in% class(sfobj))
    stop("the object must be of class sf")


  ## Check out_fmt
  ###########################################################
  out_fmtlst <- c('sqlite', 'gpkg', 'shp', 'gdb')
  out_fmt <- pcheck.varchar(out_fmt, varnm="out_fmt", checklst=out_fmtlst, 
		caption="Out format", gui=gui)

  ## Check for necessary packages
  ###########################################################
  if (out_fmt == "shp") {
    if (!"rgdal" %in% rownames(installed.packages()))
      stop("rgdal package is required for spExportSpatial")
  } else if (out_fmt %in% c("sqlite", "gpkg")) {
    if (!"RSQLite" %in% rownames(installed.packages()))
      stop("RSQLite package is required for spExportSpatial")
  } else if (out_fmt %in% c("gdb")) {
    if (!"arcgisbinding" %in% rownames(installed.packages()))
      stop("arcgisbinding package is required for spExportSpatial")
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

    
  ## Check out_layer
  ####################################################
  #if (is.null(out_layer))
   # out_layer <- basename.NoExt(out_dsn) 

  ## Write sf layer
  ########################################################
  if (out_fmt %in% c("sqlite", "gpkg")) {
    if (append_layer) overwrite_dsn <- FALSE
    gpkg <- ifelse(out_fmt == "gpkg", TRUE, FALSE)

    if (!is.null(out_dsn) && is.na(getext(out_dsn))) 
      out_dsn <- paste0(out_dsn, ".", out_fmt)

    ## Test and get filename of SQLite database
    out_dsn <- DBtestSQLite(out_dsn, gpkg=gpkg, outfolder=outfolder, showlist=FALSE,
		createnew=FALSE)
    if (is.null(out_dsn)) 
      out_dsn <- "data"

    ## Check out_layer
    if (is.null(out_layer))
      out_layer <- basename.NoExt(out_dsn) 

    if (!file.exists(out_dsn)) {
      ## Check if spatiaLite database
      sf::st_write(sfobj, dsn=out_dsn, layer=out_layer, driver="SQLite", append=TRUE,
		dataset_options="SPATIALITE=YES", layer_options="GEOMETRY_NAME = geometry",
		delete_dsn=overwrite_dsn, delete_layer=overwrite_layer, quiet=FALSE) 
    } else {
      sf::st_write(sfobj, dsn=out_dsn, layer=out_layer, driver="SQLite", append=FALSE,
		layer_options="GEOMETRY_NAME = geometry",
		delete_dsn=overwrite_dsn, delete_layer=TRUE, quiet=FALSE) 
    }

  } else if (out_fmt == "gdb") {
    if (append_layer) overwrite_dsn <- FALSE
    out_dsn <- DBtestESRIgdb(out_dsn, outfolder=outfolder, 
		overwrite=overwrite_dsn, outfn.date=outfn.date, showlist=FALSE)

    ## Check out_layer
    if (is.null(out_layer))
      out_layer <- basename.NoExt(out_dsn) 

    geofld <- attr(sfobj, "sf_column")
    sfobj <- sfobj[, c(names(sfobj)[!names(sfobj) %in% names(sfobj)[
				grepl(geofld, names(sfobj))]], geofld)]
    arcgisbinding::arc.write(file.path(out_dsn, out_layer), sfobj, overwrite=overwrite_layer)

  } else if (out_fmt == "shp") {

## Note: for a new shapefile, overwrite_dsn=FALSE, delete_layer=TRUE
#delete_dsn=FALSE; delete_layer=TRUE; append_layer=TRUE	## overwrites layer
#delete_dsn=FALSE; delete_layer=TRUE; append_layer=FALSE	## overwrites layer
#delete_dsn=FALSE; delete_layer=FALSE; append_layer=TRUE	## appends or creates layer
#delete_dsn=FALSE; delete_layer=FALSE; append_layer=FALSE	## cannot replace layer if delete_layer=FALSE
#delete_dsn=TRUE; delete_layer=FALSE; append_layer=TRUE	## overwrites layer
#delete_dsn=TRUE; delete_layer=TRUE; append_layer=FALSE	## overwrites layer
#delete_dsn=TRUE; delete_layer=FALSE; append_layer=FALSE	## cannot replace layer if delete_layer=FALSE
#delete_dsn=TRUE; delete_layer=TRUE; append_layer=TRUE	## appends layer (if exists)

    ## Get out_dsn
    ########################################################
    overwrite_layer <- ifelse(any(overwrite_dsn, overwrite_layer), TRUE, FALSE)
    if (is.null(out_dsn) || !file.exists(out_dsn)) {
      out_dsn <- getoutfn(outfn=out_layer, outfolder=outfolder,
		outfn.pre=outfn.pre, outfn.date=outfn.date, ext=out_fmt,
		overwrite=overwrite_layer, append=append_layer)
    }

    ## Check out_layer
    if (is.null(out_layer))
      out_layer <- basename.NoExt(out_dsn) 

    ## Truncate variable names to 10 characters or less
    sfobjdat <- FIESTA::trunc10shp(sfobj)
    sfobj <- sfobjdat$shp
    newnms <- sfobjdat$newnms

    suppressWarnings(sf::st_write(sfobj, dsn=out_dsn, layer=out_layer, 
		driver="ESRI Shapefile", append=append_layer, delete_dsn=FALSE,
 		delete_layer=overwrite_layer, quiet=FALSE))


    ## Write new names to *.csv file
    if (!is.null(newnms))
      write2csv(newnms, outfolder=dirname(out_dsn), 
		outfilenm=paste0(basename.NoExt(out_dsn), "_newnames"),
		outfn.date=outfn.date, overwrite=overwrite_layer) 

  } else {
    stop(out_fmt, " currently not supported")
  }  
}
