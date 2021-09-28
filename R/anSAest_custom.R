anSAest_custom <- function(SApopdatlst, SApackage="JoSAE", SAmethod="unit", 
	largebnd.unique=NULL, landarea="FOREST", pcfilter=NULL, estvarlst=NULL, 
	tfilterlst="live", showsteps=FALSE, savedata=FALSE, savesteps=FALSE, 
	append_layer=FALSE, savemultest=FALSE, outfolder=NULL, outfn.pre=NULL, 
	outfn.date=FALSE, multest_outfolder=NULL, multest_fmt="sqlite", 
	multest_dsn="SAmultest", multest.append=FALSE, multest.AOIonly=TRUE, 
	overwrite=FALSE, barplot.compare=FALSE, smallbnd.att=NULL, title.ref=NULL, 
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
  if (is.null(SApopdatlst)) {
    stop("must include population data - anSApop*()")
  } else {
    if (class(SApopdatlst) != "list") {
      SApopdatlst <- list(SApopdatlst) 
    }
  }

  ####################################################################
  ## Get estimates
  ####################################################################
  message("calculating estimates...") 

  SAestlst <- list()
  SAmultestlst <- list()
  SApredunit <- list()
  SApredarea <- list()

  SAareadat <- modSAest(SApopdatlst=SApopdatlst, SApackage=SApackage, 
	SAmethod=SAmethod, largebnd.unique=largebnd.unique, 
	esttype="AREA", landarea="FOREST", 
	savedata=savedata, rawdata=rawdata, multest=TRUE, 
	savemultest=savemultest, multest_fmt=multest_fmt,
 	multest_dsn=multest_dsn, multest_layer=multest_layer, returntitle=TRUE,
 	outfolder=outfolder, multest_outfolder=multest_outfolder, 
	multest.append=multest.append, multest.AOIonly=multest.AOIonly, 
	overwrite_layer=TRUE, append_layer=append_layer, outfn.pre=outfn.pre,
 	addSAdomsdf=addSAdomsdf, SAdomvars=SAdomvars, 
	save4testing=save4testing, showsteps=showsteps, savesteps=savesteps)
  if (is.null(SAareadat)) return(NULL)
  response <- SAareadat$raw$estvar

  SAestlst[[response]] <- SAareadat$est
  SAmultestlst[[response]] <- SAareadat$multest
  SApredunit[[response]] <- SAareadat$raw$predselect.unit
  SApredarea[[response]] <- SAareadat$raw$predselect.area

  if (multest_fmt == "csv") {
    multest_layer <- paste0("multest_", response)
  }
  if (save4testing) {
    pltdom <- SAareadat$pdomdat
    cuniqueid <- SAareadat$cuniqueid
    dunitlut <- SAareadat$dunitlut
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
        SAestdat <- modSAest(SApopdatlst=SApopdatlst, SApackage=SApackage, 
			SAmethod=SAmethod, smallbnd.att=smallbnd.att, largebnd.unique=largebnd.unique, 
			esttype="TREE", landarea=landarea, pcfilter=pcfilter, 
			estvar=estvar, estvar.filter=estvar.filter,
 			savedata=savedata, rawdata=TRUE, multest=TRUE, savemultest=savemultest,
 			multest_fmt=multest_fmt, multest_dsn=multest_dsn, multest_layer=multest_layer,
 			returntitle=TRUE, outfolder=outfolder, multest_outfolder=multest_outfolder,
 			multest.append=multest.append, multest.AOIonly=multest.AOIonly,
 			overwrite_layer=TRUE, outfn.pre=outfn.pre, save4testing=save4testing,
 			addSAdomsdf=addSAdomsdf, SAdomvars=SAdomvars, showsteps=showsteps,
			savesteps=savesteps)
        response <- SAestdat$raw$estvar
        rowvar <- SAestdat$raw$rowvar

#        if (save4testing) {
#          pltdom <- merge(pltdom, 
#			SAestdat$pdomdat[, c(dunitvar, cuniqueid, rowvar, response), with=FALSE], 
#			by=c("DOMAIN", cuniqueid, rowvar))
#          dunitlut <- merge(dunitlut, 
#			SAestdat$dunitlut[, c(dunitvar, "AOI", response, paste0(response, ".var")),
# 				with=FALSE], by=c(dunitvar, "AOI"))
#        }

        if (is.null(SAestdat$est)) {
          message("no estimates for ", outnm)
          SAestlst[[outnm]] <- NA
          SAmultestlst[[outnm]] <- NA
          SApredunit[[outnm]] <- NA
          SApredarea[[outnm]] <- NA

        } else {
          SAestlst[[outnm]] <- SAestdat$est
          SAmultestlst[[outnm]] <- SAestdat$multest
          SApredunit[[outnm]] <- SAestdat$raw$predselect.unit
          SApredarea[[outnm]] <- SAestdat$raw$predselect.area

          if (barplot.compare) {
            ## build plots
            FIESTA_SAmod_demo_plots(estvar=estvar, prednames=SApopdatlst$prednames, 
			est.com=SAestdat$dunit.multest, title.ref=title.ref, saveimg=TRUE, 
			outfolder=outfolder, showimg=TRUE)
          }
        }
        rm(SAestdat)
        gc()
      }   ## k loop
    }  ## j loop
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

  return(list(SAest=SAestlst, SAmultest=SAmultestlst, SApredunit=SApredunit, SApredarea=SApredarea))
}
		