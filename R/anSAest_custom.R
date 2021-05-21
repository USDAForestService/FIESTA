anSAest_custom <- function(SApopdat, esttype="TREE", SApackage="JoSAE", 
	SAmethod="unit", landarea="FOREST", pcfilter=NULL, 
	estvarlst=NULL, tfilterlst="live", showsteps=FALSE, savedata=FALSE, 
	savemultest=FALSE, outfolder=NULL, outfn.pre=NULL, outfn.date=FALSE,
 	multest_outfolder=NULL, multest_fmt="sqlite", multest_dsn="SAmultest", 
	multest.append=FALSE, multest.AOIonly=TRUE, overwrite=FALSE, 
	barplot.compare=FALSE, smallbnd.att=NULL, title.ref=NULL, 
	save4testing=FALSE, ...) {


  ## Set global variables
  gui=gettitle <- FALSE
  plt=measyear=measyear.filter <- NULL
  addSAdomsdf <- TRUE

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
  if (savedata || savemultest || save4testing) {
    outfolder <- pcheck.outfolder(outfolder, gui=gui)
    multest_outfolder <- pcheck.outfolder(multest_outfolder, gui=gui)
    outfn.date <- FIESTA::pcheck.logical(outfn.date , varnm="outfn.date", 
		title="Add date to outfiles?", first="NO", gui=gui) 
    overwrite <- FIESTA::pcheck.logical(overwrite, varnm="overwrite", 
		title="Overwrite?", first="NO", gui=gui)  

    #if (!is.null(outfn.pre)) {
    #  outfolder <- file.path(outfolder, outfn.pre)
    #  if (!dir.exists(outfolder)) dir.create(outfolder)
    #}
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
  SAest <- list()
  SAmultest <- list()

  message("calculating estimates...")
  outnm <- paste(SApackage, "CONDPROP_ADJ", sep="_")
  multest_layer <- outnm
  if (multest_fmt == "csv") {
    multest_layer <- paste0("multest_", multest_layer)
  }
 
  SAareadat <- modSAest(SApopdat=SApopdat, SApackage=SApackage, 
	SAmethod=SAmethod, esttype="AREA", landarea="FOREST", 
 	smallbnd.att=smallbnd.att, savedata=savedata, rawdata=TRUE, 
	multest=TRUE, multest_fmt="sqlite", multest_dsn=multest_dsn, 
	multest_layer=multest_layer, returntitle=TRUE, outfolder=outfolder, 
	multest_outfolder=multest_outfolder, multest.append=multest.append,
 	multest.AOIonly=multest.AOIonly, overwrite_layer=TRUE, 
	outfn.pre=outfn.pre, save4testing=save4testing)
  if (is.null(SAareadat)) return(NULL)
  SAest[[outnm]] <- SAareadat$est
  SAmultest[[outnm]] <- SAareadat$dunit.multest
  response <- SAareadat$raw$estvar

  if (save4testing) {
    pltdom <- SAareadat$pdomdat
    cuniqueid <- SAareadat$cuniqueid
    dunitlut <- SApopdat$dunitlut

    if (addSAdomsdf) {
      SAdomsdf <- SApopdat$SAdomsdf
      pltdom <- merge(SAdomsdf, pltdom, by="DOMAIN")
      dunitlut <- merge(SAdomsdf, dunitlut, by=c("DOMAIN", "AOI"))
    }
  }

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
        multest_layer <- outnm
        if (multest_fmt == "csv") {
          multest_layer <- paste0("multest_", multest_layer)
          chkfn <- paste0(file.path(multest_outfolder, multest_layer), ".csv")
          multest.append <- ifelse(file.exists(chkfn), TRUE, FALSE)
        }  
        SAestdat <- modSAest(SApopdat=SApopdat, SApackage=SApackage, 
			SAmethod=SAmethod, esttype="TREE", landarea=landarea, 
			pcfilter=pcfilter, estvar=estvar, estvar.filter=estvar.filter,
 			smallbnd.att=smallbnd.att, savedata=TRUE, rawdata=TRUE, 
			multest=TRUE, multest_fmt=multest_fmt, multest_dsn=multest_dsn, 
			multest_layer=multest_layer, returntitle=TRUE, outfolder=outfolder,
 			multest_outfolder=multest_outfolder, multest.append=multest.append,
 			multest.AOIonly=multest.AOIonly, overwrite_layer=TRUE, 
			outfn.pre=outfn.pre, save4testing=save4testing)
        response <- SAestdat$raw$estvar
        rowvar <- SAestdat$raw$rowvar

        if (save4testing) {
          pltdom <- merge(pltdom, 
			SAestdat$pdomdat[, c("DOMAIN", cuniqueid, rowvar, response), with=FALSE], 
			by=c("DOMAIN", cuniqueid, rowvar))
        }

        if (is.null(SAestdat$est)) {
          SAest[[outnm]] <- NA
          SAmultest[[outnm]] <- NA
        } else {
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
  }
  if (save4testing) {
    if (outfn.pre) {
      save(dunitlut, file=file.path(outfolder, paste(outfn.pre, "_dunitlut.rda")))
      save(pltdom, file=file.path(outfolder, paste(outfn.pre, "_pltdom.rda")))
    } else {
      save(dunitlut, file=file.path(outfolder, "dunitlut.rda"))
      save(pltdom, file=file.path(outfolder, "pltdom.rda"))
    }
  }

  return(list(SAest, SAmultest))
}
		