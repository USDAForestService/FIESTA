anSApop_ecomap <- function(smallbnd, smallbnd_dsn=NULL, smallbnd.unique, 
	smallbnd.domain=NULL, smallbnd.filter=NULL, smallbnd.stfilter=NULL, 
	smallbnd.ecofilter=NULL, maxbnd.threshold=10, largebnd.threshold=10, 
	nbrdom.min=10, datsource="sqlite", SQLitefn, RS=NULL, measEndyr=NULL, 
	measEndyr.filter=NULL, rastlst.cont=NULL, rastlst.cont.name=NULL, 
	rastlst.cat=NULL, rastlst.cat.name=NULL, showsteps=FALSE, 
	savedata=FALSE, savexy=FALSE, saveobj=TRUE, outfolder=NULL, outfn.pre=NULL, 
	out_fmt="sqlite", out_dsn=NULL, outfn.date=FALSE, overwrite=TRUE, 
	SAdomdat=NULL, SAdata=NULL, ...) {
		

  ## Check for packages
  if (!"FIESTAdata" %in% rownames(installed.packages()))
    stop("FIESTAdata package is required for anSApop_ecomap()")

  ## Set global variables
  gui <- FALSE
  returnlst <- list()

  ## Check savedata 
  savedata <- FIESTA::pcheck.logical(savedata, varnm="savedata", 
		title="Save data extraction?", first="NO", gui=gui)  

  ## Check overwrite, outfn.date, outfolder, outfn 
  ########################################################
  if (savedata || saveobj) {
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

  ## Set parameters
  helperbnd=FIESTA::ecomap
  helperbnd.unique="SUBSECTION"
  largebnd=NULL
  largebnd.unique="SECTION"
  maxbnd=NULL
  maxbnd.unique="PROVINCE"
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
		largebnd=largebnd, largebnd.unique=largebnd.unique, 
		maxbnd=maxbnd, maxbnd.unique=maxbnd.unique, helper_autoselect=TRUE, 
		multiSAdoms=multiSAdoms, maxbnd.threshold=maxbnd.threshold,
 		largebnd.threshold=largebnd.threshold, nbrdom.min=nbrdom.min,
		savedata=TRUE, showsteps=showsteps, savesteps=savesteps, 
		outfolder=outfolder, out_fmt="shp")
    if (is.null(SAdomdat)) return(NULL)

    if (saveobj) 
      save(SAdomdat, file=file.path(outfolder, "SAdomdat.rda"))
  }

  smallbnd <- SAdomdat$smallbndlst[[1]]
  SAdoms <- SAdomdat$SAdomlst[[1]]
  SAdomnm <- names(SAdoms)

  ###########################################################################
  ## Extract FIA data and auxiliary data
  ###########################################################################
 
  if (is.null(SAdata)) {

    ###########################################################################
    ## Extract FIA data and model data
    ###########################################################################
    SAdata <- anSAdata(SAdoms, smallbnd=smallbnd, RS=RS, datsource=datsource, 
		xy.joinid="ZSTUNCOPLOT", istree=TRUE, data_dsn=SQLitefn, 
		measCur=measCur, measEndyr=measEndyr, measEndyr.filter=measEndyr.filter, 
		rastlst.cont=rastlst.cont, rastlst.cont.name=rastlst.cont.name, 
		rastlst.cat=rastlst.cat, rastlst.cat.name=rastlst.cat.name, 
		showsteps=showsteps, savedata=savedata, savexy=savexy, 
		outfolder=outfolder, out_fmt="sqlite", out_dsn=NULL, overwrite=TRUE, ...)
    if (is.null(SAdata)) return(NULL)
    returnlst$SAdata <- SAdata

    if (saveobj) 
      save(SAdata, file=file.path(outfolder, "SAdata.rda"))

  } 

  ## SAdata
  SAdoms <- SAdata$SAdoms
  plt <- SAdata$plt
  cond <- SAdata$cond
  tree <- SAdata$tree
  pltassgn <- SAdata$pltassgn
  dunitlut <- SAdata$dunitlut
  dunitvar <- SAdata$dunitvar
  prednames <- SAdata$prednames
  zonalnames <- SAdata$zonalnames
  predfac <- SAdata$predfac
  dunitarea <- SAdata$dunitarea
  areavar <- SAdata$areavar
  puniqueid <- SAdata$puniqueid
  pltassgnid <- SAdata$pltassgnid
  pjoinid <- SAdata$pjoinid

  rm(SAdata)
  gc()
 
  if (is.null(SAdoms)) 
    return(NULL)

  ####################################################################
  ## Get population data
  ####################################################################
  SApopdat <- modSApop(SAdoms=SAdoms, tree=tree, cond=cond, plt=plt, 
		pltassgn=pltassgn, puniqueid=puniqueid, pltassgnid=pltassgnid, 
		pjoinid=pjoinid, measEndyr=measEndyr, measEndyr.filter=measEndyr.filter, 
		dunitarea=dunitarea, dunitvar=dunitvar, areavar=areavar, 
		dunitlut=dunitlut, prednames=prednames, predfac=predfac)
  names(SApopdat)
  returnlst$SApopdat <- SApopdat


  if (saveobj)
    save(SApopdat, file=file.path(outfolder, "SApopdat.rda"))


  return(returnlst)
}
