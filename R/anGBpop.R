anGBpop <- function(bnd, bnd_dsn=NULL, bnd.att=NULL, bnd.filter=NULL, 
	datsource="sqlite", SQLitefn=NULL, RS=NULL, strat_layer=NULL, 
	showsteps=FALSE, savedata=FALSE, savexy=TRUE, outfolder=NULL, outfn.pre=NULL, 
	outfn.date=FALSE, overwrite=TRUE, GBdata=NULL, ...) {


  ## Set global variables
  gui <- FALSE
  returnlst <- list()
  strata <- TRUE


  ## Check savedata 
  savedata <- FIESTA::pcheck.logical(savedata, varnm="savedata", 
		title="Save data extraction?", first="NO", gui=gui)  

  ## Check overwrite, outfn.date, outfolder, outfn 
  ########################################################
  if (savedata) {
    outfolder <- pcheck.outfolder(outfolder, gui=gui)
    outfn.date <- FIESTA::pcheck.logical(outfn.date , varnm="outfn.date", 
		title="Add date to outfiles?", first="NO", gui=gui) 
    overwrite <- FIESTA::pcheck.logical(overwrite, varnm="overwrite", 
		title="Overwrite?", first="NO", gui=gui)  

    if (!is.null(outfn.pre)) {
      outfolder <- file.path(outfolder, outfn.pre)
      if (!dir.exists(outfolder)) dir.create(outfolder)
    }
  }

  ###########################################################################
  ## Extract FIA data and auxiliary data
  ###########################################################################

  if (is.null(GBdata)) { 
    message("extracting data...")
 
    ###########################################################################
    ## Extract FIA data and model data
    ###########################################################################
    if (is.null(strat_layer)) strata <- FALSE
    GBdata <- anGBdata(bnd_layer=bnd, bnd_dsn=bnd_dsn, bnd.att=bnd.att, 
		bnd.filter=bnd.filter, RS=RS, datsource=datsource, istree=TRUE, 
		data_dsn=SQLitefn, strata=strata, strat_layer=strat_layer, 
		showsteps=showsteps, cex.plots=.75, savedata=savedata, savexy=savexy, 
		outfolder=outfolder, outfn.pre="GBdata", out_fmt="csv", out_dsn=NULL, 
		overwrite=TRUE, ...)
    returnlst$GBdata <- GBdata
  } else {
    GBdata <- pcheck.object(GBdata, objnm="GBdata", 
		list.items=c("bnd", "plt", "cond", "unitarea"))
  }
	
  ## GBdata
  bnd <- GBdata$bnd
  plt <- GBdata$plt
  cond <- GBdata$cond
  tree <- GBdata$tree
  pltassgn <- GBdata$pltassgn
  unitarea <- GBdata$unitarea
  unitvar <- GBdata$unitvar
  areavar <- GBdata$areavar
  stratalut <- GBdata$stratalut
  strvar <- GBdata$strvar
  puniqueid <- GBdata$puniqueid
  pltassgnid <- GBdata$pltassgnid
  pjoinid <- GBdata$pjoinid
  rm(GBdata)
  gc()


  ####################################################################
  ## Get population data
  ####################################################################
  GBpopdat <- modGBpop(tree=tree, cond=cond, plt=plt, pltassgn=pltassgn, 
		puniqueid=puniqueid, pltassgnid=pltassgnid, pjoinid=pjoinid, 
		unitarea=unitarea, unitvar=unitvar, areavar=areavar, 
		strata=strata, stratalut=stratalut, strvar=strvar, 
		saveobj=TRUE, outfolder=outfolder)
  names(GBpopdat)
  returnlst$GBpopdat <- GBpopdat


  return(returnlst)
}

