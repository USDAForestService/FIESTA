spExportShape <- function (spobj, outshpnm=NULL, outfolder=NULL, outfn.date=TRUE,
	overwrite=FALSE, uniqueid=NULL) {
  ###########################################################################
  ## DESCRIPTION: Exports an S4 Spatial object to an ArcGIS shapefile (*.shp).  
  ## 
  ## Note: If there are more than one unique record, an error will occur. 
  ##    Also, variable names will be truncated to 10 characters.
  ###########################################################################

  ##################################################################
  ## CHECK INPUT PARAMETERS
  ##################################################################

  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- ifelse(nargs() == 0, TRUE, FALSE)

  ## If gui.. set variables to NULL
  if (gui) spobj <- NULL

  if (is.null(spobj)) {
    shpnm <- select.list(ls(pos=1, all.names=TRUE), title="Spatial object?", 
		multiple=FALSE)
    spobj <- get(shpnm)
  }
  if (typeof(spobj) != "S4" || !grepl("Spatial", class(spobj)))
    stop("the object selected is not a Spatial object")


  ## Check uniqueid
  if (!is.null(uniqueid)) {
    spobjnmlst <- names(spobj)
    uniqueid <- FIESTA::pcheck.varchar(var2check=uniqueid, varnm="uniqueid", 
		checklst=spobjnmlst, caption="UniqueID variable - spobj", 
		warn="uniqueid not in spobj")

    if (nrow(spobj) > length(unique(spobj[[uniqueid]])))
      message("cannot export to shapefile... more than 1 record per uniqueid")
  }

  ## Check outshpnm
  if (is.null(outshpnm)) {
    outshpnm <- "outshp"
  } else if (!is.character(outshpnm)) {
    stop("outshpnm must be character")
  } else {
    if (raster::extension(outshpnm) == ".shp")
      outshpnm <- unlist(strsplit(outshpnm, "[.]"))[1]

    if (dirname(outshpnm) != ".") {
      if (file.exists(dirname(outshpnm))) {
        if (dirname(outshpnm) != "/")
          outfolder <- dirname(outshpnm)
        outshpnm <- basename(outshpnm)
      } else {
        stop("path does not exist")
      }
    }
  }
 
  ## Check outfolder
  outfolder <- FIESTA::pcheck.outfolder(outfolder, gui)


  ##################################################################
  ## DO WORK
  ##################################################################

  ## Truncate variable names to 10 characters or less
  spobjdat <- FIESTA::trunc10shp(spobj)
  spobj <- spobjdat$shp
  newnms <- spobjdat$newnms


  ## Output shapefile to outfolder
  if (outfn.date)
    outshpnm <- paste0(outshpnm, "_", format(Sys.time(), "%Y%m%d"))
  if (!overwrite)
    outshpnm <- FIESTA::fileexistsnm(outfolder, outshpnm, "shp")

  rgdal::writeOGR(spobj, outfolder, outshpnm, driver="ESRI Shapefile",
		overwrite_layer=overwrite)

  cat(
  " ###############################################################################", 
  "\n", paste0("Saved shapefile to: ", outfolder, "/", outshpnm, ".shp"), "\n", 
  "###############################################################################",
  "\n" )

  ## Write new names to *.csv file
  if (!is.null(newnms)) {
    outshpnames <- paste(outshpnm, "newnames", sep="_")
    if (outfn.date)
      outshpnames <- paste0(outshpnames, "_", format(Sys.time(), "%Y%m%d"))
    if (!overwrite)
      outshpnames <- FIESTA::fileexistsnm(outfolder, outshpnames, "csv")
  
    write.csv(newnms, paste0(outfolder, "/", outshpnames, ".csv"), 
		row.names=FALSE)
  
    cat(
    " ###############################################################################", 
    "\n", paste0("Saved new names to: ", outfolder, "/", outshpnames, ".csv"), "\n", 
    "###############################################################################",
    "\n" )
  }
}


