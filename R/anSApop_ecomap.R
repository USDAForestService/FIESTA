anSApop_ecomap <- function(smallbnd, smallbnd_dsn=NULL, smallbnd.unique, 
	smallbnd.domain=NULL, smallbnd.filter=NULL, smallbnd.stfilter=NULL, 
	smallbnd.ecofilter=NULL, maxbnd.threshold=10, largebnd.unique="PROVINCE", 
	largebnd.filter=NULL, largebnd.threshold=10, nbrdom.min=10, RS=NULL, 
	xy_datsource=NULL, xy=NULL, xy_dsn=NULL, xyjoinid="PLOT_ID", 
	datsource="sqlite", data_dsn, 
	measEndyr=NULL, measEndyr.filter=NULL, intensity1=TRUE, 
	rastlst.cont=NULL, rastlst.cont.name=NULL,
	rastlst.cat=NULL, rastlst.cat.name=NULL, showsteps=FALSE, savedata=FALSE, 
	savexy=FALSE, saveobj=TRUE, outfolder=NULL, out_fmt="sqlite", 
	out_dsn="SApopdat", outfn.date=FALSE, overwrite_dsn=FALSE, 
	overwrite_layer=TRUE, append_layer=FALSE, SAdomdat=NULL, SAdata=NULL, ...) {
		

  ## Set global variables
  gui <- FALSE
  returnlst <- list()

  ## Check savedata 
  savedata <- FIESTA::pcheck.logical(savedata, varnm="savedata", 
		title="Save data extraction?", first="NO", gui=gui)  

  ## Check overwrite, outfn.date, outfolder, outfn 
  ########################################################
  if (savedata) {
    outlst <- pcheck.output(out_dsn=out_dsn, out_fmt=out_fmt, 
		outfolder=outfolder, outfn.date=outfn.date, 
		overwrite_dsn=overwrite_dsn, overwrite_layer=overwrite_layer, 
		append_layer=append_layer, createSQLite=FALSE, gui=gui)
    out_dsn <- outlst$out_dsn
    outfolder <- outlst$outfolder
    out_fmt <- outlst$out_fmt
    overwrite_layer <- outlst$overwrite_layer
    overwrite_dsn <- outlst$overwrite_dsn
    append_layer <- outlst$append_layer

  } else if (saveobj) {
    outfolder <- pcheck.outfolder(outfolder)
  }
    

  ## Set parameters
  helperbnd=FIESTA::ecomap
  helperbnd.unique="SUBSECTION"
  largebnd=NULL
  maxbnd=NULL
  maxbnd.unique=NULL
  helper_autoselect=TRUE
  multiSAdoms=FALSE
  maxbnd.threshold=maxbnd.threshold
  largebnd.threshold=largebnd.threshold
  savesteps=savedata
  measCur=TRUE

#smallbnd.domain=NULL
#smallbnd.ecofilter <- getfilter("PROVINCE", RAVG.ecoprov)
#nbrdom.min=10

 
  #############################################################################
  ## SA domains
  #############################################################################
  if (is.null(SAdomdat) && is.null(SAdata)) {
    message("generating SAdoms...")
    SAdomdat <- spGetSAdoms(smallbnd=smallbnd, smallbnd_dsn=smallbnd_dsn, 	
		smallbnd.unique=smallbnd.unique, smallbnd.domain=smallbnd.domain,
 		smallbnd.filter=smallbnd.filter, smallbnd.stfilter=smallbnd.stfilter,
 		smallbnd.ecofilter=smallbnd.ecofilter, 
		helperbnd=helperbnd, helperbnd.unique=helperbnd.unique, 
		largebnd=largebnd, largebnd.unique=largebnd.unique, largebnd.filter=largebnd.filter, 
		maxbnd=maxbnd, maxbnd.unique=maxbnd.unique, helper_autoselect=TRUE, 
		multiSAdoms=multiSAdoms, maxbnd.threshold=maxbnd.threshold,
 		largebnd.threshold=largebnd.threshold, nbrdom.min=nbrdom.min,
		savedata=TRUE, showsteps=showsteps, savesteps=savesteps, 
		outfolder=outfolder, out_fmt="shp", overwrite_dsn=overwrite_dsn, 
		overwrite_layer=overwrite_layer)
    if (is.null(SAdomdat)) return(NULL)

    if (saveobj) {
      saveRDS(SAdomdat, file=file.path(outfolder, "SAdomdat.rds"))
    }
  }

  smallbnd <- SAdomdat$smallbndlst[[1]]
  SAdoms <- SAdomdat$SAdomlst[[1]]
  SAdomnm <- names(SAdoms)
  smallbnd.unique <- SAdomdat$smallbnd.unique

  if (is.null(SAdata)) {
    ###########################################################################
    ## Extract FIA data and model data
    ###########################################################################
    SAdata <- anSAdata(SAdoms, RS=RS, xy_datsource=xy_datsource, 
		xy=xy, xy_dsn=xy_dsn, xyjoinid=xyjoinid, 
		datsource=datsource, istree=TRUE, data_dsn=data_dsn, 
		measCur=measCur, measEndyr=measEndyr, measEndyr.filter=measEndyr.filter, 
		intensity1=intensity1, 
		rastlst.cont=rastlst.cont, rastlst.cont.name=rastlst.cont.name, 
		rastlst.cat=rastlst.cat, rastlst.cat.name=rastlst.cat.name, 
		showsteps=showsteps, savedata=savedata, savexy=savexy, 
		outfolder=outfolder, out_fmt=out_fmt, out_dsn=out_dsn, 
		overwrite_dsn=overwrite_dsn, overwrite_layer=overwrite_layer, ...)
    if (is.null(SAdata)) return(NULL)
    returnlst$SAdata <- SAdata

    if (saveobj) {
      saveRDS(SAdata, file=file.path(outfolder, "SAdata.rds"))
    }
  } 

  ## SAdata
#  SAdoms <- SAdata$SAdoms
#  plt <- SAdata$plt
#  cond <- SAdata$cond
#  tree <- SAdata$tree
#  pltassgn <- SAdata$pltassgn
#  dunitlut <- SAdata$dunitlut
#  dunitvar <- SAdata$dunitvar
#  prednames <- SAdata$prednames
#  zonalnames <- SAdata$zonalnames
#  predfac <- SAdata$predfac
#  dunitarea <- SAdata$dunitarea
#  areavar <- SAdata$areavar
#  puniqueid <- SAdata$puniqueid
#  pltassgnid <- SAdata$pltassgnid
#  pjoinid <- SAdata$pjoinid

#  rm(SAdata)
#  gc()
 
  if (is.null(SAdata$SAdoms)) {
    return(NULL)
  }
 
  ####################################################################
  ## Get population data
  ####################################################################
  SApopdat <- modSApop(SAdata=SAdata, smallbnd=smallbnd, 
		smallbnd.unique=smallbnd.unique)
  names(SApopdat)
  returnlst$SApopdat <- SApopdat


  if (saveobj) {
    saveRDS(SApopdat, file=file.path(outfolder, "SApopdat.rds"))
  }

  return(returnlst)
}
