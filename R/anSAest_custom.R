anSAest_custom <- function(SApopdat, SApackage="JoSAE", SAmethod="unit", 
	largebnd.att=NULL, landarea="FOREST", pcfilter=NULL, estvarlst=NULL, 
	tfilterlst="live", showsteps=FALSE, savedata=FALSE, append_layer=FALSE,
	savemultest=FALSE, outfolder=NULL, outfn.pre=NULL, outfn.date=FALSE,
 	multest_outfolder=NULL, multest_fmt="sqlite", multest_dsn="SAmultest", 
	multest.append=FALSE, multest.AOIonly=TRUE, overwrite=FALSE, 
	barplot.compare=FALSE, smallbnd.att=NULL, title.ref=NULL, 
	save4testing=FALSE, save4testing.append=FALSE, testfolder=NULL, 
	addSAdomsdf=FALSE, SAdomvars=NULL, ...) {


  ## Set global variables
  gui=gettitle <- FALSE
  plt=measyear=measyear.filter <- NULL
  #addSAdomsdf <- FALSE
  rawdata <- TRUE
  dunitvar <- "DOMAIN"

  if (is.null(title.ref)) gettitle <- TRUE


  ## Check savedata 
  savedata <- FIESTA::pcheck.logical(savedata, varnm="savedata", 
		title="Save data extraction?", first="NO", gui=gui)

  ## Check savemultest 
  savemultest <- FIESTA::pcheck.logical(savemultest, varnm="savemultest", 
		title="Save multiple estimates?", first="NO", gui=gui)  
  

  ## Check overwrite, outfn.date, outfolder, outfn 
  ########################################################
  if (savedata || savemultest || save4testing) {
    outfolder <- pcheck.outfolder(outfolder, gui=gui)
    if (is.null(multest_outfolder)) {
      multest_outfolder <- outfolder
    } else {
      multest_outfolder <- pcheck.outfolder(multest_outfolder, gui=gui)
    }
    outfn.date <- FIESTA::pcheck.logical(outfn.date , varnm="outfn.date", 
		title="Add date to outfiles?", first="NO", gui=gui) 
    overwrite <- FIESTA::pcheck.logical(overwrite, varnm="overwrite", 
		title="Overwrite?", first="NO", gui=gui)  

    #if (!is.null(outfn.pre)) {
    #  outfolder <- file.path(outfolder, outfn.pre)
    #  if (!dir.exists(outfolder)) dir.create(outfolder)
    #}
    if (save4testing) {
      testfolder <- pcheck.outfolder(testfolder, gui=gui)
      if (is.null(testfolder)) {
        testfolder <- outfolder
      }
    }
  }
  ###########################################################################
  ## Extract FIA data and model data
  ###########################################################################
  if (is.null(SApopdat)) {
    stop("must include population data - anSApop*()")
  }

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
  message("calculating estimates...") 

  SAestlst <- list()
  SAmultestlst <- list()
  SApredselectlst <- list()
  SAareadat <- modSAest(SApopdat=SApopdat, SApackage=SApackage, 
	SAmethod=SAmethod, largebnd.att=largebnd.att, smallbnd.att=smallbnd.att, 
	esttype="AREA", landarea="FOREST", 
	savedata=savedata, rawdata=rawdata, multest=TRUE, 
	savemultest=savemultest, multest_fmt=multest_fmt,
 	multest_dsn=multest_dsn, multest_layer=multest_layer, returntitle=TRUE,
 	outfolder=outfolder, multest_outfolder=multest_outfolder, 
	multest.append=multest.append, multest.AOIonly=multest.AOIonly, 
	overwrite_layer=TRUE, append_layer=append_layer, outfn.pre=outfn.pre,
 	addSAdomsdf=addSAdomsdf, SAdomvars=SAdomvars, 
	save4testing=save4testing, showsteps=showsteps)
  if (is.null(SAareadat)) return(NULL)
  response <- SAareadat$raw$estvar

  SAestlst[[response]] <- SAareadat$est
  SAmultestlst[[response]] <- SAareadat$dunit_multest
  SApredselectlst[[response]] <- SAareadat$raw$prednames.select
 
  if (multest_fmt == "csv") {
    multest_layer <- paste0("multest_", response)
  }
  if (save4testing) {
    pltdom <- SAareadat$pdomdat
    cuniqueid <- SAareadat$cuniqueid
    dunitlut <- SAareadat$dunitlut

    if (addSAdomsdf) {
      SAdomsdf <- SApopdat$SAdomsdf
      pltdom <- merge(setDT(SAdomsdf)[, 
			unique(c(dunitvar, largebnd.att, SAdomvars)), with=FALSE], 
			pltdom, by=dunitvar)
      dunitlut <- merge(setDT(SAdomsdf)[, 
			unique(c(dunitvar, "AOI", largebnd.att, SAdomvars)), with=FALSE], 
			dunitlut, by=c(dunitvar, "AOI"))
    }
  }
 
  #if (esttype == "TREE") {
    for (j in 1:length(estvarlst)) {
      estvar <- estvarlst[j]
      for (k in 1:length(tfilterlst)) {
        tfilter <- tfilterlst[k]
        message("generating estimates for ", estvar, " - ", tfilter, "...")

        estvar.filter <- ifelse(tfilter == "live", "STATUSCD == 1", 
			ifelse(tfilter == "dead", "STATUSCD == 2 & STANDING_DEAD_CD == 1", NULL)) 

        estvarnm <- ifelse(estvar == "TPA_UNADJ", "COUNT", estvar)
        outnm <- paste(SApackage, estvarnm, tfilter, sep="_")
        outnm <- paste(estvarnm, tfilter, sep="_")
        multest_layer <- outnm
        if (multest_fmt == "csv") {
          multest_layer <- paste0("multest_", multest_layer)
          chkfn <- paste0(file.path(multest_outfolder, multest_layer), ".csv")
          multest.append <- ifelse(file.exists(chkfn), TRUE, FALSE)
        } 

        SAestdat <- modSAest(SApopdat=SApopdat, SApackage=SApackage, 
			SAmethod=SAmethod, smallbnd.att=smallbnd.att, largebnd.att=largebnd.att, 
			esttype="TREE", landarea=landarea, pcfilter=pcfilter, 
			estvar=estvar, estvar.filter=estvar.filter,
 			savedata=savedata, rawdata=TRUE, multest=TRUE, savemultest=savemultest,
 			multest_fmt=multest_fmt, multest_dsn=multest_dsn, multest_layer=multest_layer,
 			returntitle=TRUE, outfolder=outfolder, multest_outfolder=multest_outfolder,
 			multest.append=multest.append, multest.AOIonly=multest.AOIonly,
 			overwrite_layer=TRUE, outfn.pre=outfn.pre, save4testing=save4testing,
 			addSAdomsdf=addSAdomsdf, SAdomvars=SAdomvars, showsteps=showsteps)
        response <- SAestdat$raw$estvar
        rowvar <- SAestdat$raw$rowvar

        if (save4testing) {
          pltdom <- merge(pltdom, 
			SAestdat$pdomdat[, c(dunitvar, cuniqueid, rowvar, response), with=FALSE], 
			by=c("DOMAIN", cuniqueid, rowvar))
          dunitlut <- merge(dunitlut, 
			SAestdat$dunitlut[, c(dunitvar, "AOI", response, paste0(response, ".var")),
 				with=FALSE], by=c(dunitvar, "AOI"))
        }

        if (is.null(SAestdat$est)) {
          message("no estimates for ", outnm)
          SAestlst[[outnm]] <- NA
          SAmultestlst[[outnm]] <- NA
          SApredselectlst[[outnm]] <- NA

        } else {
          SAestlst[[outnm]] <- SAestdat$est
          SAmultestlst[[outnm]] <- SAestdat$dunit_multest
          SApredselectlst[[outnm]] <- SAestdat$raw$prednames.select

          if (barplot.compare) {
            ## build plots
            FIESTA_SAmod_demo_plots(estvar=estvar, prednames=SApopdat$prednames, 
			est.com=SAestdat$dunit.multest, title.ref=title.ref, saveimg=TRUE, 
			outfolder=outfolder, showimg=TRUE)
          }
        }
      }
    }
  #}
  if (save4testing) {
    if (!is.null(outfn.pre)) {
      saveRDS(dunitlut, file=file.path(testfolder, paste(outfn.pre, "_dunitlut.rds")))
      saveRDS(pltdom, file=file.path(testfolder, paste(outfn.pre, "_pltdom.rds")))
    } else {
      datExportData(dunitlut, outfolder=testfolder, out_layer="dunitlut", 
		overwrite_layer=overwrite, append_layer=save4testing.append)
      datExportData(pltdom, outfolder=testfolder, out_layer="pltdom", 
		overwrite_layer=overwrite, append_layer=save4testing.append)

      #save(dunitlut, file=file.path(testfolder, "dunitlut.rda"))
      #save(pltdom, file=file.path(testfolder, "pltdom.rda"))
    }
  }

  return(list(SAest=SAestlst, SAmultest=SAmultestlst, SApredselect=SApredselectlst))
}
		