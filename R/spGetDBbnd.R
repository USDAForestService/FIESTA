spGetDBbnd <- function(bnd_layer, bnd_dsn=NULL, stbnd=NULL, states=NULL, 
	RMRSonly=TRUE, datsource="ORACLE", SQLitefn=NULL, actual=FALSE, istree=TRUE, 
	coordsfn=NULL, coordsfn.uniqueid=NULL, coordsfn.x=NULL, coordsfn.y=NULL, 
	coordsfn.prj=NULL, coordsfn.datum=NULL, savedata=FALSE, outfolder=NULL, 
	outfn=NULL, outfn.pre=NULL, outfn.date=FALSE, overwrite=FALSE, ...) {

  ##############################################################################
  ## DESCRIPTION
  ## Get FIA plots within the boundary population (area of interest)
  ## 1) Intersect with state boundary 
  ## 2) If RMRSonly=TRUE (i.e., actual coordinates), subset states to IW states only
  ## 3) Get FIA plots for intesected states (including tree, shp, INTENSITY=1)
  ## 4) Merge coordinate data if included separately (e.g., coordinates from SDS)
  ## 5) Clip spatial coordinates and other tables to boundary
  ##
  ## VALUE
  ## List of clipped data frames
  ##############################################################################

  ## Set global variables
  CTcoverfn=othertabnms=NULL

  ##################################################################
  ## CHECK INPUT PARAMETERS
  ##################################################################
  gui <- FALSE

  ## Import boundary
  #############################################################################
  bndx <- pcheck.spatial(layer=bnd_layer, dsn=bnd_dsn, caption="boundary")


  ## Check RMRSonly 
  ########################################################
  RMRSonly <- FIESTA::pcheck.logical(RMRSonly, varnm="RMRSonly", 
		title="RMRS only?", first="YES", gui=gui, stopifnull=TRUE)

  ## Check savedata
  #############################################################################
  savedata <- FIESTA::pcheck.logical(savedata, varnm="savedata", 
		title="Save data?", first="NO", gui=gui)  

  ## Check exportshp
  #############################################################################
#  exportshp <- FIESTA::pcheck.logical(exportshp, varnm="exportshp", 
#		title="Export to shapefile?", first="NO", gui=gui)  

#  if (savedata || exportshp) {
  if (savedata) {
    ## Check overwrite 
    overwrite <- FIESTA::pcheck.logical(overwrite, varnm="overwrite", 
		title="Overwrite files?", first="NO", gui=gui)  

    outfolder <- FIESTA::pcheck.outfolder(outfolder, gui)

    if (is.null(outfn.pre))
      outfn.pre <- "clip" 

    if (is.null(outfn)) {
      outfn <- "SAares"
    } else if (!is.character(outfn)) {
      stop("outfn must be character")
    }      
  }

  ## Check datsource
  datsourcelst <- c("ORACLE", "CSV", "SQLite")
  datsource <- FIESTA::pcheck.varchar(var2check=datsource, varnm="datsource", 
		checklst=datsourcelst, gui=gui, caption="Data source?") 


  ########################################################################
  ### DO THE WORK
  ########################################################################

  if (datsource != "SQLite") {
    ## Get intersecting states
    #############################################################################
    if (is.null(states)) {
      if (is.null(stbnd))
      ## Get state boundary and reproject polygon boundary
        stbnd <- raster::getData('GADM', country="USA", level=1)

      ## Reproject stbnd to bnd projection
      prjdat <- FIESTA::CRScompare(bndx, stbnd, nolonglat=TRUE)
      bndx <- prjdat$layer1
      stbnd <- prjdat$layer2

      ## Check intersecting states
      states <- as.vector(na.omit(stbnd[["NAME_1"]][!is.na(sp::over(stbnd, bndx))]))

      sp::plot(stbnd[stbnd$NAME_1 %in% states, ])
      sp::plot(bndx, add=TRUE, border="red")
    } else {

      states <- FIESTA::pcheck.states(states, "MEANING")
    }
  

    #############################################################################
    ## NOTE: Maybe add percent overlap here
    #############################################################################
  
    ## Subset states to RMRS states only and clip bnd to polygons that intersect states
    #############################################################################
    if (RMRSonly) {
      RMRSstates <- c("Arizona", "Colorado", "Montana", "Idaho", "Nevada", "New Mexico", 
		"Wyoming", "Utah")

      ## Subset states to RMRS
      states <- states[states %in% RMRSstates]

      ## Subset stbnd
      stbndRMRS <- stbnd[stbnd$NAME_1 %in% states,]

      ## Subset bnd to only polygons that intersect RMRS states
      inrows <- rgeos::gIntersects(bndx, stbndRMRS, byid=TRUE, returnDense=TRUE)
      inrows <- colnames(inrows)[apply(inrows, 2, sum) > 0]
      bndx <- bndx[row.names(bndx) %in% inrows, ]

      sp::plot(stbndRMRS)
      sp::plot(bndx, add=TRUE, border="red")
    }
  }

  if (datsource == "SQLite") {
    if (!file.exists(SQLitefn)) 
      stop("invalid SQLite database")
    if (!"RSQLite" %in% rownames(installed.packages()))
	 stop("DBI package is require to run SQLite queries.")
    

    con <- DBI::dbConnect(RSQLite::SQLite(), SQLitefn)

    plt <- DBI::dbGetQuery(con, "select * from plot") 
    cond <- DBI::dbGetQuery(con, "select * from cond") 
    tree <- DBI::dbGetQuery(con, "select * from tree") 
    spplt <- spImportSpatial(dsn=SQLitefn, layer="spplt")

    DBI::dbDisconnect(con)

    #############################################################################
    ## Subset coordinates to boundary
    #############################################################################
    clipdat <- spClipPoint(spplt=spplt, uniqueid="PLT_CN", 
		clippolyv_dsn=bndx, savedata=savedata, outfn.pre=outfn.pre,
 		outfolder=outfolder, outfn.date=outfn.date, overwrite=overwrite)
    clip_spplt <- clipdat$clip_spplt
    clip_xyplt <- clipdat$clip_xyplt
    clip_poly <- clipdat$clip_poly
    uniqueid <- "PLT_CN"

    othertabnms <- c("plt", "cond", "tree")
    othertabs <- lapply(othertabnms, function(x) get(x, envir=environment()))
    clip_tabs <- FIESTA::clip.othertables(clip_xyplt[[uniqueid]], othertabnms=othertabnms,
		othertabs=othertabs, savedata=savedata, outfn.pre=outfn.pre, 
		outfolder=outfolder, outfn.date=outfn.date, overwrite=overwrite)
    clipdat$clip_tabs <- clip_tabs
    clipdat$coordtype <- "PUBLIC"

    return(clipdat)
  }

  #############################################################################
  ## Get most current plots from database that intersect states
  #############################################################################

  if (actual) {
    if (!RMRSonly) stop("must have select permission to SDS_PLOT table")
    datsource <- "ORACLE"
    spcoords <- c("PUBLIC", "ACTUAL")
  } else {
    if (is.null(datsource)) datsource <- "CSV"
    spcoords <- "PUBLIC"
  }

  datPlots <- DBgetPlots(states=states, datsource=datsource, istree=istree,
		issp=TRUE, actual=actual, spcoords=spcoords, savedata=FALSE,
		intensity1=TRUE, spcond=TRUE, ...)
  plt <- datPlots$plt
  cond <- datPlots$cond
  tree <- datPlots$tree
  actualp <- datPlots$actualp
  actualc <- datPlots$actualc
  if (is.null(tree) && istree) stop("must include tree")
  spplt_PUBLIC <- datPlots$spplt_PUBLIC
  spplt_ACTUAL <- datPlots$spplt_ACTUAL
  evalid <- datPlots$evalid

  for (nm in names(datPlots)) {
    assign(nm, datPlots[[nm]])
    if (is.data.frame(get(nm)) && nm != "pltcnt") 
      othertabnms <- c(othertabnms, nm)
  }



  #############################################################################
  ## Merge coordsfn to plt and create plt shapefile
  #############################################################################
  coordtype <- NULL
  coords <- FIESTA::pcheck.table(coordsfn, gui=FALSE)
  if (!actual && !is.null(coords)) {
    coordtype <- "external"

    ## Check that matching columns are the same class
    tabs <- FIESTA::check.matchclass(tab1=plt, tab2=coords, matchcol="CN", 
		var2=coordsfn.uniqueid, tab1txt="plt", tab2txt="coordsfn")
    plt <- tabs$tab1
    coords <- tabs$tab2

    ## Check that the values of uniqueid in coords are all in plt
    FIESTA::check.matchval(plt, coords, "CN", coordsfn.uniqueid,
		tab1txt="plt", tab2txt="coords", returnvals=FALSE, stopifnotnull=TRUE)

    spcoords <- FIESTA::spMakeSpatialPoints(xyplt=coords, uniqueid=coordsfn.uniqueid,
		x=coordsfn.x, y=coordsfn.y, prj=coordsfn.prj, datum=coordsfn.datum)
    uniqueid <- coordsfn.uniqueid  
  } else if (!is.null(spplt_ACTUAL)) {
    coordtype <- "ACTUAL"
    uniqueid <- "PLT_CN"
    spcoords <- spplt_ACTUAL
  } else if (!is.null(spplt_PUBLIC)) {
    coordtype <- "PUBLIC"
    uniqueid <- "PLT_CN"
    spcoords <- spplt_PUBLIC
  } else {
    stop("no coordinates")
  }


  #############################################################################
  ## Subset coordinates to boundary
  #############################################################################
  clipdat <- spClipPoint(spplt=spcoords, uniqueid=uniqueid, clippolyv_layer=bndx,
	savedata=savedata, outfn.pre=outfn.pre, outfolder=outfolder, 
	outfn.date=outfn.date, overwrite=overwrite)
  clip_xyplt <- clipdat$clip_xyplt
#  clip_spplt <- clipdat$clip_spplt

  ## Check Intensity of plots
  ###################################
  intensityp <- unique(clip_xyplt[["INTENSITY"]])
  if (any(intensityp > 1)) {
    message("more than 1 plot intensity... removing plots with intensity > 1")
    clip_xyplt <- clip_xyplt[clip_xyplt$INTENSITY == 1, ] 
  }


#############################################################################
## Check if boundaries cross anything weird like different intensities, nonsampled areas, etc.
#############################################################################
## Still need to add


  ## Subset other tables
  ###################################
  if (!is.null(othertabnms)) {
    othertabs <- lapply(unique(othertabnms), function(x) get(x, envir=environment()))
    clip_tabs <- FIESTA::clip.othertables(clip_xyplt[[uniqueid]], othertabnms=othertabnms,
		othertabs=othertabs, savedata=savedata, outfn.pre=outfn.pre, 
		outfolder=outfolder, outfn.date=outfn.date, overwrite=overwrite)
    clipdat$clip_tabs <- clip_tabs
    clipdat$coordtype <- coordtype
  }  

  ###########################################################################
  ## Add Chris Toney canopy cover and set NA values to 0
  ## NOTE: Use data from database
  ###########################################################################
  if (!is.null(CTcoverfn)) {
    if (!file.exists(CTcoverfn)) stop("CTcoverfn does not exist")
    CTcover <- FIESTA::pcheck.table(CTcoverfn)
    plt <- merge(plt, CTcover[, c("PLT_CN", "stemmap_cancov")], 
				by.x="CN", by.y="PLT_CN", all.x=TRUE)
    plt[is.na(plt$stemmap_cancov), "stemmap_cancov"] <- 0
    head(plt)
    dim(plt)
  }

  return(clipdat)
}
