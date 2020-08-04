anSAest_custom <- function(SApopdat, esttype="TREE", SApackage="JoSAE", 
	SAmethod="unit", landarea="FOREST", plt.filter=NULL, cond.filter=NULL, 
	estvarlst=NULL, tfilterlst="live", showsteps=FALSE, savedata=FALSE, 
	savemultest=FALSE, outfolder=NULL, outfn.pre=NULL, outfn.date=FALSE,
 	multest_outfolder=NULL, multest_dsn="SAmultest", multest.append=FALSE,
 	multest.AOIonly=TRUE, overwrite=FALSE, barplot.compare=FALSE, 
	smallbnd.att=NULL, title.ref=NULL, ...) {


  ## Set global variables
  gui=gettitle <- FALSE
  ref_titles <- FIESTA::ref_titles
  plt=measyear=measyear.filter <- NULL

  if (is.null(title.ref)) gettitle <- TRUE

  ## Check esttype 
  ########################################################
  esttypelst <- c("AREA", "TREE")
  esttype <- FIESTA::pcheck.varchar(var2check=esttype, varnm="esttype", 
		checklst=esttypelst, caption="Estimation type", stopifnull=TRUE)

  ## Check savedata 
  savedata <- FIESTA::pcheck.logical(savedata, varnm="savedata", 
		title="Save data extraction?", first="NO", gui=gui)

  ## Check savemultest 
  savemultest <- FIESTA::pcheck.logical(savemultest, varnm="savemultest", 
		title="Save multiple estimates?", first="NO", gui=gui)  
  

  ## Check overwrite, outfn.date, outfolder, outfn 
  ########################################################
  if (savedata || savemultest) {
    outfolder <- pcheck.outfolder(outfolder, gui=gui)
    multest_outfolder <- pcheck.outfolder(multest_outfolder, gui=gui)
    if (is.null(multest_outfolder)) multest_outfolder <- NULL

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
  ## Extract FIA data and model data
  ###########################################################################
  if (is.null(SApopdat))
    stop("must include population data - anSApop*()")

 
  ##################################################################################
  ## modSAtree() default parameters
  ##################################################################################

  if (is.null(SApopdat)) {
    SApop <- anSApop_ecomap(showsteps=showsteps, savedata=savedata, 
		outfolder=outfolder, outfn.pre=outfn.pre, ...)

    SApopdat <- SApop$SApopdat
    #SAdata <- SApop$SAdata
  } 


  ####################################################################
  ## Get estimates
  ####################################################################
  SAest <- list()
  SAmultest <- list()

  message("calculating estimates...")


  if (esttype == "TREE") {
    for (j in 1:length(estvarlst)) {
      estvar <- estvarlst[j]
      for (k in 1:length(tfilterlst)) {
        tfilter <- tfilterlst[k]
        message("generating estimates for ", estvar, " - ", tfilter, "...")

        estvar.filter <- ifelse(tfilter == "live", "STATUSCD == 1", 
			ifelse(tfilter == "dead", "STATUSCD == 2 & STANDING_DEAD_CD == 1", NULL)) 

        estvarnm <- ifelse(estvar == "TPA_UNADJ", "COUNT", estvar)
        outnm <- paste(SApackage, estvarnm, tfilter, sep="_")
 

#        SAestdat <- tryCatch(modSAtree(SApopdat=SApopdat, SApackage=SApackage, 
#		SAmethod=SAmethod, landarea=landarea, plt.filter=plt.filter, 
#		cond.filter=cond.filter, estvar=estvar, estvar.filter=estvar.filter,
# 		smallbnd.att=smallbnd.att, savedata=TRUE, multest=TRUE, 		
#		multest_fmt="sqlite", multest_dsn=multest_dsn, multest_layer=outnm, 
#		returntitle=TRUE, outfolder=outfolder, multest_outfolder=multest_outfolder,
# 		multest.append=multest.append, multest.AOIonly=multest.AOIonly, 
#		overwrite=TRUE, outfn.pre=outfn.pre) ,
#     	  error=function(e) {
#			message("error with ", estvarnm)
#			return(NULL) }) 

        SAestdat <- modSAtree(SApopdat=SApopdat, SApackage=SApackage, 
		SAmethod=SAmethod, landarea=landarea, plt.filter=plt.filter, 
		cond.filter=cond.filter, estvar=estvar, estvar.filter=estvar.filter,
 		smallbnd.att=smallbnd.att, savedata=TRUE, multest=TRUE, 		
		multest_fmt="sqlite", multest_dsn=multest_dsn, multest_layer=outnm, 
		returntitle=TRUE, outfolder=outfolder, multest_outfolder=multest_outfolder,
 		multest.append=multest.append, multest.AOIonly=multest.AOIonly, 
		overwrite=TRUE, outfn.pre=outfn.pre)

        if (is.null(SAestdat)) return(NULL)

        SAest[[outnm]] <- SAestdat$est
        SAmultest[[outnm]] <- SAestdat$dunit.multest
 
        if (barplot.compare) {
          ## build plots
          FIESTA_SAmod_demo_plots(estvar=estvar, prednames=SApopdat$prednames, 
			est.com=SAestdat$raw$dunit.multest, title.ref=title.ref, saveimg=TRUE, 
			outfolder=outfolder, showimg=TRUE)
        }
      }
    }
  }
  return(list(SAest, SAmultest))
}
		