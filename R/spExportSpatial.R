#' Spatial - Exports an sf object.
#' 
#' Exports an sf object to a specified output.
#' 
#' Wrapper for sf::st_write function.
#' 
#' @param sfobj sf class R object. Spatial object to export.
#' @param savedata_opts List. See help(savedata_options()) for a list
#' of options for saving data. If out_layer = NULL, default = 'datsp'.
#' 
#' @return An sf spatial object is written to outfolder.
#' @note If out_fmt='shp':\cr The ESRI shapefile driver truncates variable
#' names to 10 characters or less. Variable names are changed before export
#' using an internal function (trunc10shp). Name changes are output to the
#' outfolder, 'out_layer'_newnames.csv.
#' 
#' If sf object has more than 1 record, it cannot be exported to a shapefile.
#' @author Tracey S. Frescino
#' @keywords data
#' @examples 
#' # Set up data from FIESTA
#' WYbh <- spImportSpatial(system.file("extdata",
#'                                     "sp_data/WYbighorn_adminbnd.shp",
#'                                     package = "FIESTA"))
#' 
#' # Export data with spExportSpatial
#' spExportSpatial(WYbh,
#'                 savedata_opts = list(out_dsn = "WYbh.shp", 
#'                                      outfolder = tempdir(), 
#'                                      overwrite_dsn = TRUE))
#' @export spExportSpatial
spExportSpatial <- function(sfobj, savedata_opts=NULL) {
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
 
  ## Check parameter lists
  pcheck.params(input.params, savedata_opts=savedata_opts)

  ## Set savedata defaults
  savedata_defaults_list <- formals(savedata_options)[-length(formals(savedata_options))]
  
  for (i in 1:length(savedata_defaults_list)) {
    assign(names(savedata_defaults_list)[[i]], savedata_defaults_list[[i]])
  }
  
  ## Set user-supplied savedata values
  if (length(savedata_opts) > 0) {
    for (i in 1:length(savedata_opts)) {
      if (names(savedata_opts)[[i]] %in% names(savedata_defaults_list)) {
        assign(names(savedata_opts)[[i]], savedata_opts[[i]])
      } else {
        stop(paste("Invalid parameter: ", names(savedata_opts)[[i]]))
      }
    }
  }


  ## Check sfobj
  ###########################################################
  if (is.null(sfobj)) {
    sfnm <- select.list(ls(pos=1, all.names=TRUE), title="sf object?", 
		multiple=FALSE)
    sfobj <- get(sfnm)
  }
  if (!"sf" %in% class(sfobj)) {
    stop("the object must be of class sf")
  }

  ## Check out_fmt
  ###########################################################
  outlst <- pcheck.output(out_dsn=out_dsn, out_fmt=out_fmt, 
                outfolder=outfolder, outfn.pre=outfn.pre, outfn.date=outfn.date, 
                overwrite_dsn=overwrite_dsn, overwrite_layer=overwrite_layer, 
                add_layer=add_layer, append_layer=append_layer,
 	              createSQLite=FALSE)
  out_fmt <- outlst$out_fmt
  out_dsn <- outlst$out_dsn
  outfolder <- outlst$outfolder
  overwrite_dsn <- outlst$overwrite_dsn
  overwrite_layer <- outlst$overwrite_layer
  append_layer <- outlst$append_layer
  outfn.date <- outlst$outfn.date
  outfn.pre <- outlst$outfn.pre

  out_fmt <- ifelse(out_fmt == "csv", "shp", out_fmt)

  ## Check out_layer
  if (is.null(out_layer)) {
    out_layer <- "datsp"
  } 
  
  ## Write sf layer
  ########################################################
  if (out_fmt %in% c("sqlite", "gpkg")) {
    if (append_layer) overwrite_dsn <- FALSE
    gpkg <- ifelse(out_fmt == "gpkg", TRUE, FALSE)

#    if (!is.null(out_dsn) && is.na(getext(out_dsn))) {
#      out_dsn <- paste0(out_dsn, ".", out_fmt)
#    }

    ## Test and get filename of SQLite database
    out_dsn <- DBtestSQLite(out_dsn, gpkg=gpkg, outfolder=outfolder, showlist=FALSE,
		createnew=FALSE)

    ## Write to SQLite database
    if (!file.exists(out_dsn)) {
      sf::st_write(sfobj, dsn=out_dsn, layer=out_layer, driver="SQLite", append=TRUE,
		dataset_options="SPATIALITE=YES", layer_options="GEOMETRY_NAME = geometry",
		delete_dsn=overwrite_dsn, delete_layer=overwrite_layer, quiet=FALSE) 
    } else {
   
      ## If file exists, check if spatiaLite database
      if (DBI::dbCanConnect(RSQLite::SQLite(), out_dsn)) {
        sqlconn <- DBI::dbConnect(RSQLite::SQLite(), out_dsn, loadable.extensions = TRUE)
        tablst <- DBI::dbListTables(sqlconn)
        if (length(tablst) == 0 || !"SpatialIndex" %in% tablst) {
          stop(paste(out_dsn, "is a Spatialite database... "))
        }
      } 
      if (out_layer %in% tablst) {
        if (overwrite_layer) {
          message("overwriting ", out_layer, " in ", basename(out_dsn), "...")
        } else {
          stop(out_layer, " exists in ", basename(out_dsn), " and overwrite_layer = FALSE")
        }
      }
      sf::st_write(sfobj, dsn=out_dsn, layer=out_layer, driver="SQLite", append=FALSE,
		layer_options="GEOMETRY_NAME = geometry",
		delete_dsn=overwrite_dsn, delete_layer=TRUE, quiet=FALSE) 
    }

  } else if (out_fmt == "gdb") {
    message("cannot write to geodatabases")
#    if (append_layer) {
#      stop("can't append data to ", out_layer, " with out_fmt='gdb'")
#    }
#    out_dsn <- DBtestESRIgdb(out_dsn, outfolder=outfolder, 
#		overwrite=overwrite_dsn, outfn.date=outfn.date, showlist=FALSE)
#
#    ## Check out_layer
#    if (is.null(out_layer)) {
#      if (!is.null(out_dsn)) {
#        out_layer <- "outfile"
#      }
#    } 
#    geofld <- attr(sfobj, "sf_column")
#    sfobj <- sfobj[, c(names(sfobj)[!names(sfobj) %in% names(sfobj)[
#				grepl(geofld, names(sfobj))]], geofld)]
#    arcgisbinding::arc.write(file.path(out_dsn, out_layer), sfobj, overwrite=overwrite_layer)

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

    ## Append prefix
    ########################################################
    #if (!is.null(outfn.pre)) {
    #  out_layer <- paste(outfn.pre, out_layer, sep="_")
    #}

    ## Get out_dsn
    ########################################################
    if (is.null(out_dsn) || !file.exists(out_dsn)) {
      out_dsn <- getoutfn(outfn=out_layer, outfolder=outfolder,
		outfn.pre=outfn.pre, outfn.date=outfn.date, ext=out_fmt,
		overwrite=overwrite_layer, append=append_layer)
    }
    ## Get out_layer
    out_layer <- basename.NoExt(out_dsn)
    
    ## Truncate variable names to 10 characters or less
    sfobjdat <- trunc10shp(sfobj)
    sfobj <- sfobjdat$shp
    newnms <- sfobjdat$newnms

    delete_layer <- ifelse(append_layer, FALSE, TRUE)
    suppressWarnings(sf::st_write(sfobj, dsn=out_dsn, layer=out_layer, 
		driver="ESRI Shapefile", append=append_layer, delete_dsn=FALSE,
 		delete_layer=delete_layer, quiet=FALSE))

 
    ## Write new names to *.csv file
    if (!is.null(newnms)) {    
      suppressWarnings(write2csv(newnms, outfolder=normalizePath(dirname(out_dsn)), 
		outfilenm=paste0(basename.NoExt(out_dsn), "_newnames"),
		outfn.date=outfn.date, overwrite=overwrite_layer)) 
    }

  } else {
    stop(out_fmt, " currently not supported")
  }  
}
