anSAest_RAVG <- function(RAVG, RAVG_dsn=NULL, RAVG.fire=NULL, 
	RAVG.year=NULL, RAVG.state=NULL, RAVG.ecoprov=NULL, RAVG.minacre=NULL, 
	datsource="sqlite", SQLitefn, RS=NULL, largebnd.unique="PROVINCE", 
	largebnd.threshold=10, nbrdom.min=10, rastlst.cont=NULL, 
	rastlst.cont.name=NULL, rastlst.cat=NULL, rastlst.cat.name=NULL, 
	SApackage="JoSAE", SAmethod="unit", pcfilter=NULL, landarea="FOREST", 
	estvarlst, savedata=FALSE, saveobj=TRUE, showsteps=FALSE, outfolder=NULL,
 	multest_outfolder=NULL, multest_fmt="sqlite", multest_dsn="RAVG_SAmultest", 
 	multest.append=FALSE, overwrite_dsn=FALSE, overwrite_layer=TRUE, 
	append_layer=FALSE, barplot.compare=FALSE, title.ref=NULL, save4testing=FALSE, 
	SAdomdat=NULL, SAdata=NULL, SApopdat=NULL, addPS=FALSE) {


  ## Set global variables
  gui=gettitle <- FALSE
  plt=RAVG.filter=RAVG.ecofilter=measyear=measyear.filter <- NULL
  returnlst <- list()

  if (is.null(title.ref)) gettitle <- TRUE


  ##################################################################################
  ## spGetSAdoms() - default parameters
  ##################################################################################
  smallbnd.unique <- "FIRE_ID"
  #smallbnd.domain <- "Fire_Name"
  smallbnd.domain <- NULL
  maxbnd.threshold <- 50
  #largebnd.threshold <- 10
  multiSAdoms <- FALSE
  showstep <- TRUE
  savexy=TRUE
  tfilterlst <- "live"

  ## set RAVG polygons from FIESTA data package as smallbnd
  RAVG <- pcheck.spatial(dsn=RAVG_dsn, layer=RAVG)

  if (is.null(multest_dsn)) {
    multest_dsn <- "RAVG_SAmultest"
  }
 
  ## Check RAVG filters
  ##################################################################################
  RAVG.filter <- NULL
  if (!is.null(RAVG.fire)) {
    ## Check RAVG.fire
    firenamelst <- sort(unique(RAVG$Fire_Name))
    if (!all(RAVG.fire %in% firenamelst)) {
      print(firenamelst)
      stop("invalid RAVG.fire... must be in list")
    }
    RAVG.filter <- getfilter("Fire_Name", RAVG.fire)

    if (gettitle && length(RAVG.fire) < 3) 
      title.ref <- toString(RAVG.fire)
      
  } else if (!is.null(RAVG.year)) {
    ## Check RAVG.year
    fireyearlst <- sort(unique(RAVG$FIRE_YEAR))
    if (!all(RAVG.year %in% fireyearlst)) 
      stop("invalid RAVG.year... must be in list: ", toString(fireyearlst))
    RAVG.yrfilter <- getfilter("FIRE_YEAR", as.character(RAVG.year))
    if (is.null(RAVG.filter)) {
      RAVG.filter <- RAVG.yrfilter 
    } else {
      RAVG.filter <- paste(RAVG.filter, "&", RAVG.yrfilter)
    }

    if (gettitle && length(RAVG.year) < 3) {
      title.ref <- paste(toString(RAVG.year), "fires")
    } else {
      title.ref <- paste0(min(RAVG.year), ":", max(RAVG.year), " fires")
    }
  }       
  ## Check RAVG.state
  RAVG.stfilter <- pcheck.states(RAVG.state)
  if (!is.null(RAVG.stfilter) && gettitle) {
    if (is.null(title.ref)) {
      title.ref <- toString(RAVG.stfilter)
    } else {
      title.ref <- paste(title.ref, ", ", toString(RAVG.stfilter))
    }
  }
  ## Check RAVG.ecoprov
  if (!is.null(RAVG.ecoprov)) {
    if (length(RAVG.ecoprov) > 1) stop("RAVG.ecoprom must only be length of 1")
    ecoprovlst <- sort(unique(FIESTA::ecomap$PROVINCE))
    if (!all(RAVG.ecoprov %in% ecoprovlst)) {
      print(ecoprovlst)
      stop("invalid RAVG.ecoprov... must be in list")
    }
    RAVG.ecofilter <- getfilter("PROVINCE", RAVG.ecoprov)

    if (is.null(title.ref)) {
      title.ref <- toString(RAVG.ecoprov)
    } else {
      title.ref <- paste0(title.ref, ", ", toString(RAVG.ecoprov))
    }
  }
  ## Check RAVG.minacre
  if (!is.null(RAVG.minacre)) {
    if (!is.numeric(RAVG.minacre)) stop("RAVG.minacre must be numeric")
    acre.filter <- paste("ACRES >=", RAVG.minacre)
    if (is.null(RAVG.filter)) {
      RAVG.filter <- acre.filter
    } else {
      RAVG.filter <- paste(RAVG.filter, acre.filter, sep=" & ")
    }
  }

  ##################################################################################
  ## anSAdata() default parameters
  ##################################################################################
#  if (datsource == "sqlite") {
#    xy_dsn <- SQLitefn
#    data_dsn <- SQLitefn

#    ## Check SQLitefn
#    #################################################################
#    dbconn <- DBtestSQLite(SQLitefn, dbconnopen=TRUE)
#    tabs <- DBI::dbListTables(dbconn)

    ## Check xy parameters
#    xy <- tabs[grepl("xy", tabs)]
#    if (length(xy) == 0)
#      stop("no xy is SQLitefn")
#    xy.uniqueid <- "PLT_CN"
#    xyjoinid <- "PLOT_ID"

#    xyx <- xy[grepl("ACTUAL", xy)]
#    if (length(xyx) == 1) {
#      xy <- xyx
#      xvar <- "LON_ACTUAL"
#      yvar <- "LAT_ACTUAL"
#    } else {
#      xyx <- xy[grepl("PUBLIC", xy)]
#      if (length(xyx) == 1) {
#        xvar <- "LON_PUBLIC"
#        yvar <- "LAT_PUBLIC"
#      }
#    }
#  }


  ##################################################################################
  ## modSApop() default parameters
  ##################################################################################
  if (!is.null(RAVG.year)) {
    measEndyr <- RAVG.year
    measEndyr.filter <- "AOI == 1"
  }

  ## Check outfolder
  outfolder <- pcheck.outfolder(outfolder)
  ecofolder <- file.path(outfolder, RAVG.ecoprov)
  if (!dir.exists(ecofolder)) {
    dir.create(ecofolder)
  }

  ##################################################################################
  ## modSAtree() default parameters
  #################################################################################
  if (is.null(SApopdat)) {
    SApop <- anSApop_ecomap(smallbnd=RAVG, smallbnd_dsn=RAVG_dsn, 	
		smallbnd.unique=smallbnd.unique, smallbnd.domain=smallbnd.domain, 
		smallbnd.filter=RAVG.filter, smallbnd.stfilter=RAVG.stfilter, 
		smallbnd.ecofilter=RAVG.ecofilter, largebnd.filter=RAVG.ecofilter,
		largebnd.threshold=largebnd.threshold, largebnd.unique=largebnd.unique,
		maxbnd.threshold=maxbnd.threshold, nbrdom.min=nbrdom.min, 
		datsource=datsource, data_dsn=SQLitefn, RS=RS,
 		measEndyr=measEndyr, measEndyr.filter=measEndyr.filter, 
		rastlst.cont=rastlst.cont, rastlst.cont.name=rastlst.cont.name,
 		rastlst.cat=rastlst.cat, rastlst.cat.name=rastlst.cat.name, 
		showsteps=showsteps, savedata=savedata, savexy=savexy, savesteps=savedata,
 		saveobj=saveobj, outfolder=ecofolder, out_fmt="sqlite", out_dsn="SApopdat", 
		overwrite_dsn=overwrite_dsn, overwrite_layer=overwrite_layer, 
		SAdomdat=SAdomdat, SAdata=SAdata)
     ## Return SA population data
     returnlst$SApopdat <- SApop$SApopdat
     SApopdat <- SApop$SApopdat
  }

  ####################################################################
  ## Get estimates
  ####################################################################
  if (is.null(SApopdat) || all(SApopdat$dunitlut$AOI == 0)) {
    unlink(ecofolder, force=TRUE, recursive=TRUE)    
    return(NULL)
  } else {
    message("getting estimates... ")

source("C:\\_tsf\\_GitHub\\FIESTA\\R\\modSAest.R")
source("C:\\_tsf\\_GitHub\\FIESTA\\R\\anSAest_custom.R")
    ## Get SA estimates
    ##############################################
    SAest <- anSAest_custom(SApopdat=SApopdat, largebnd.att=largebnd.unique,
		estvarlst=estvarlst, showsteps=showsteps, 
		savedata=savedata, outfolder=ecofolder, AOIonly=TRUE, 
		save4testing=TRUE, save4testing.append=multest.append,	
		testfolder=outfolder, addSAdomsdf=TRUE, 
		SAdomvars=unique(c("PROVINCE", largebnd.unique)))
    multest <- SAest$SAmultest
 
    if (addPS) {
      ## Convert SAdata to GBdata
      GBdata <- SApop$SAdata
      dunitlut <- GBdata$dunitlut
      GBdata$stratalut <- strat.pivot(dunitlut, strvar="tnt", unitvars=c("DOMAIN", "AOI"))
      GBdata$strvar <- "tnt"
      GBdata$strwtvar <- "Prop"
      names(GBdata)[names(GBdata) == "dunitarea"] <- "unitarea"
      names(GBdata)[names(GBdata) == "dunitvar"] <- "unitvar"

      ## Green-book - Post-strat
      GBpopdatPS <- modGBpop(GBdata=GBdata, strata=TRUE, minplotnum.unit=0, adj="plot")

      ## Area estimate - GB - PS
      GBestPS <- modGBarea(GBpopdat=GBpopdatPS, landarea="FOREST", 
			rawdata=TRUE, rawonly=TRUE)$raw$unit_totest
      GBestPS$nhat.se <- sqrt(GBestPS$nhat.var)
      GBestPS <- GBestPS[, c("DOMAIN", "nhat", "nhat.se")]
      setnames(GBestPS, c("DOMAIN", "PS", "PS.se"))

      multest[["AREA_ADJ"]] <- merge(multest[["AREA_ADJ"]], GBestPS, 
		by="DOMAIN", all.x=TRUE)

      ## Get GB post-strat estimates
      ##############################################
      for (estvar in estvarlst) {
        ## Green-book - Post-strat
        GBestPS <- modGBtree(GBpopdat=GBpopdatPS, landarea="FOREST", 
			estvar=estvar, estvar.filter="STATUSCD == 1", 
			rawdata=TRUE, rawonly=TRUE)$raw$unit_totest
        GBestPS$nhat.se <- sqrt(GBestPS$nhat.var)
        GBestPS <- GBestPS[, c("DOMAIN", "nhat", "nhat.se")]
        setnames(GBestPS, c("DOMAIN", "PS", "PS.se"))

        if (estvar == "TPA_UNADJ") {
          estvar <- "COUNT"
        }
        multestvar <- paste0(estvar, "_live")

        ## Append GB-PS estimate to multest
        multest[[multestvar]] <- merge(multest[[multestvar]], GBestPS, 
		by="DOMAIN", all.x=TRUE)
      } 
    } 
 
    for (estvar in c("AREA_ADJ", estvarlst)) {
      if (estvar %in% estvarlst) {
        if (estvar == "TPA_UNADJ") {
          estvar <- "COUNT"
        }
        estvar <- paste0(estvar, "_live")
        multestvar <- estvar
      } else if (estvar == "AREA_ADJ") {
        multestvar <- "FOREST_prop"
      }
 
      ## Add province name to multest
      multest[[estvar]]$PROVINCE <- RAVG.ecoprov

      ## Export dunit_multest
      overwrite_layer <- ifelse(multest.append, FALSE, TRUE)
      datExportData(multest[[estvar]], out_fmt="sqlite", outfolder=outfolder, 
 		out_dsn=multest_dsn, out_layer=multestvar, 
		overwrite_layer=overwrite_layer, append_layer=multest.append)
    }
  }
 

  ## Save selected predictors to ecofolder
  if (saveobj) {
    saveRDS(SAest$SApredselect, file=file.path(ecofolder, "SApredselect.rds"))
  }
  returnlst$SAest <- SAest
  returnlst$SAmultest <- multest
  returnlst$SApredselect <- SAest$SApredselect

  return(returnlst)
}
		