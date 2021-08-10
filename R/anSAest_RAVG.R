anSAest_RAVG <- function(RAVG, RAVG_dsn=NULL, RAVG.fire=NULL, RAVG.year=NULL,
 	RAVG.state=NULL, RAVG.ecoprov=NULL, RAVG.minacre=NULL, datsource="sqlite", 
	SQLitefn, RS=NULL, largebnd.threshold=10, nbrdom.min=10, rastlst.cont=NULL,
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
		maxbnd.threshold=maxbnd.threshold, largebnd.threshold=largebnd.threshold,
		nbrdom.min=nbrdom.min, datsource=datsource, data_dsn=SQLitefn, RS=RS,
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
  if (is.null(SApopdat)) {
    return(NULL)
  } else {
    message("getting estimates... ")

    ## Get SA estimates
    ##############################################
    SAest <- anSAest_custom(SApopdat=SApopdat, 
		estvarlst=estvarlst, showsteps=showsteps, 
		savedata=savedata, outfolder=ecofolder, AOIonly=TRUE, 
		save4testing=TRUE, save4testing.append=multest.append,	
		testfolder=outfolder, SAdomvars=c("STATECD",largebnd.unique))
    multest <- SAest$SAmultest

    if (addPS) {
      SAdata <- SApop$SAdata

      ## Convert SAdata to GBdata
      dunitlut <- SAdata$dunitlut
      SAdata$stratalut <- strat.pivot(dunitlut, strvar="tnt", unitvars=c("DOMAIN", "AOI"))
      SAdata$strvar <- "tnt"
      SAdata$strwtvar <- "Prop"
      names(SAdata)[names(SAdata) == "dunitarea"] <- "unitarea"
      names(SAdata)[names(SAdata) == "dunitvar"] <- "unitvar"

      ## Green-book - Post-strat
      GBpopdatPS <- modGBpop(GBdata=SAdata, strata=TRUE, minplotnum.unit=0, adj="plot")

      GBest <- modGBarea(GBpopdat=GBpopdatPS, landarea="FOREST", 
			rawdata=TRUE, rawonly=TRUE)$raw$unit_totest
      GBest$nhat.se <- sqrt(GBest$nhat.var)
      GBest <- GBest[, c("DOMAIN", "nhat", "nhat.se")]
      setnames(GBest, c("DOMAIN", "PS", "PS.se"))

      multest[["CONDPROP_ADJ"]] <- merge(multest[["CONDPROP_ADJ"]], GBest, 
		by="DOMAIN", all.x=TRUE, all.y=TRUE)


      ## Get GB post-strat estimates
      ##############################################
      for (estvar in estvarlst) {
        ## Green-book - Post-strat
        GBest <- modGBtree(GBpopdat=GBpopdatPS, landarea="FOREST", 
			estvar=estvar, estvar.filter="STATUSCD == 1", 
			rawdata=TRUE, rawonly=TRUE)$raw$unit_totest
        GBest$nhat.se <- sqrt(GBest$nhat.var)
        GBest <- GBest[, c("DOMAIN", "nhat", "nhat.se")]
        setnames(GBest, c("DOMAIN", "PS", "PS.se"))

        if (estvar == "TPA_UNADJ") {
          estvar <- "COUNT"
        }
        multestvar <- paste0(estvar, "_live")
        multest[[multestvar]] <- merge(multest[[multestvar]], GBest, 
		by="DOMAIN", all.x=TRUE, all.y=TRUE)
      } 
    } 
    
    for (estvar in c("CONDPROP_ADJ", estvarlst)) {
      if (estvar %in% estvarlst) {
        if (estvar == "TPA_UNADJ") {
          estvar <- "COUNT"
        }
        estvar <- paste0(estvar, "_live")
        multestvar <- estvar
      } else if (estvar == "CONDPROP_ADJ") {
        multestvar <- "FOREST_prop"
      }

      ## Add province name to multest
      multest[[estvar]]$PROVINCE <- ecoprov

      ## Export dunit_multest
      overwrite_layer <- ifelse(multest.append, FALSE, TRUE)
      datExportData(multest[[estvar]], out_fmt="sqlite", outfolder=outfolder, 
 		out_dsn="SAmultest_subsect", out_layer=multestvar, 
		overwrite_layer=overwrite_layer, append_layer=multest.append)
    }
  }

  returnlst$SAest <- SAest
  returnlst$SAmultest <- multest

  return(returnlst)
}
		