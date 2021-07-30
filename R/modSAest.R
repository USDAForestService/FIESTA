modSAest <- function(SApopdat=NULL, SAdomsdf=NULL, prednames=NULL, 
	SApackage="JoSAE", SAmethod="area", esttype="TREE", totals=FALSE, 
	estseed="none", largebnd.att=NULL, landarea="FOREST", pcfilter=NULL, 
	estvar=NULL, estvar.filter=NULL, smallbnd.att=NULL, allin1=FALSE, 
	metric=FALSE, estround=3, pseround=3, estnull=0, psenull="--", 
	divideby=NULL, savedata=FALSE, rawdata=FALSE, rawonly=FALSE, multest=TRUE, 
	addSAdomsdf=TRUE, SAdomvars=NULL, outfolder=NULL, outfn.pre=NULL, 
	outfn.date=FALSE, addtitle=TRUE, raw_fmt="csv", raw_dsn="rawdata", 
	savemultest=FALSE, multest_fmt="csv", multest_outfolder=NULL, 
	multest_dsn=NULL, multest_layer=NULL, multest.append=FALSE, 
	multest.AOIonly=FALSE, overwrite_dsn=FALSE, overwrite_layer=TRUE, 
	append_layer=FALSE, returntitle=FALSE, title.main=NULL, title.ref=NULL, 
	title.dunitvar=NULL, title.estvar=NULL, title.filter=NULL, 
	save4testing=FALSE, ...){


  ######################################################################################
  ## DESCRIPTION: 
  ## Generates model-assisted estimates by domain (and estimation unit)
  ## if saveraw...  and raw_fmt = 'csv', a new folder is created within the outfolder
  ##			named as raw_dsn. If raw_fmt != 'csv', a database is created
  ##			within the outfolder names as raw_dsn. 
  ######################################################################################
  gui <- FALSE
  returnlst <- list()

  ## Check input parameters
#  input.params <- names(as.list(match.call()))[-1]
#  formallst <- c(names(formals(FIESTA::modSAtree)),
#		names(formals(FIESTA::modSApop))) 
#  if (!all(input.params %in% formallst)) {
#    miss <- input.params[!input.params %in% formallst]
#    stop("invalid parameter: ", toString(miss))
#  }

  ## CHECK GUI - IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  if (nargs() == 0 && is.null(SApopdat)) {
    gui <- TRUE
  } 

  ## If gui.. set variables to NULL
  if (gui) { 
    tree=landarea <- NULL
    if (!row.FIAname) row.FIAname <- NULL
    if (!col.FIAname) col.FIAname <- NULL
  }

  ## Set global variables
  ONEUNIT=n.total=n.strata=strwt=TOTAL=AOI=rowvar.filter=colvar.filter=
	title.rowvar=title.colvar=TOTAL=JoSAE=JU.EBLUP=JFH=JoSAE.se=
	JU.EBLUP.se.1=pse=AREAUSED=JoSAE.pse=JoSAE.total <- NULL


  ##################################################################
  ## INITIALIZE SETTINGS
  ##################################################################
# divideby=NULL
# allin1=FALSE
# addtitle=FALSE
# returntitle=TRUE
# rawdata=TRUE
# estround=0
# pseround=3
# rowvar=NULL
# colvar=NULL
# row.FIAname=FALSE
# col.FIAname=FALSE
# row.orderby=NULL
# col.orderby=NULL
# row.add0=FALSE
# col.add0=FALSE
# rowlut=NULL
# collut=NULL
# rowgrp=FALSE
# rowgrpnm=NULL
# rowgrpord=NULL
# title.rowvar=NULL
# title.colvar=NULL
# title.main=NULL
# title.ref=NULL
# title.rowvar=NULL
# title.colvar=NULL
# title.dunitvar=NULL
# title.estvar=NULL
# title.filter=NULL

  ## SET OPTIONS
  options.old <- options()
  options(scipen=8) # bias against scientific notation
  on.exit(options(options.old), add=TRUE) 
  title.rowgrp <- NULL
  pvars2keep <- c("DOMAIN", "AOI")
  returnSApopdat <- TRUE
  sumunits=FALSE

  rowvar=NULL
  colvar=NULL
  row.FIAname=FALSE
  col.FIAname=FALSE
  row.orderby=NULL
  col.orderby=NULL
  row.add0=FALSE
  col.add0=FALSE
  rowlut=NULL
  collut=NULL
  rowgrp=FALSE
  rowgrpnm=NULL
  rowgrpord=NULL 


  ## Check SApackage 
  SApackagelst <- c("JoSAE", "sae")
  SApackage <- FIESTA::pcheck.varchar(var2check=SApackage, varnm="SApackage", gui=gui, 
		checklst=SApackagelst, caption="SApackage", multiple=FALSE, stopifnull=TRUE)

  ## Check for JoSAE library
  if (SApackage == "JoSAE") {
    if (!"JoSAE" %in% rownames(installed.packages())) {
	 message("SApackage JoSAE requires package JoSAE")
    }
  } else {
    if (!"sae" %in% rownames(installed.packages())) {
	 message("SApackage sae requires package sae")
    }
  }

  ## Check SAmethod 
  SAmethodlst <- c("unit", "area", "combo")
  SAmethod <- FIESTA::pcheck.varchar(var2check=SAmethod, varnm="SAmethod", gui=gui, 
		checklst=SAmethodlst, caption="SAmethod", multiple=FALSE, stopifnull=TRUE)

  ###################################################################################
  ## Check data and generate population information 
  ###################################################################################
  if (is.null(SApopdat)) {
    SApopdat <- modSApop(gui=gui, prednames=prednames, ...)
  } else {
    returnSApopdat <- FALSE
    list.items <- c("condx", "pltcondx", "treex", "cuniqueid", "condid", 
		"tuniqueid", "ACI.filter", "dunitarea", "dunitvar", "dunitlut",
		"prednames", "plotsampcnt", "condsampcnt")
    SApopdat <- FIESTA::pcheck.object(SApopdat, "SApopdat", list.items=list.items)
  }

  if (is.null(SApopdat)) return(NULL)
  SAdomsdf <- SApopdat$SAdomsdf
  condx <- SApopdat$condx
  pltcondx <- SApopdat$pltcondx
  treex <- SApopdat$treex
  seedx <- SApopdat$seedx
  if (is.null(treex) && is.null(seedx)) {
    stop("must include tree data for tree estimates")
  }
  cuniqueid <- SApopdat$cuniqueid
  condid <- SApopdat$condid
  tuniqueid <- SApopdat$tuniqueid
  ACI.filter <- SApopdat$ACI.filter
  dunitarea <- SApopdat$dunitarea
  areavar <- SApopdat$areavar
  areaunits <- SApopdat$areaunits
  dunitvar <- SApopdat$dunitvar
  dunitvar2 <- SApopdat$dunitvar2
  dunitlut <- SApopdat$dunitlut
  plotsampcnt <- SApopdat$plotsampcnt
  condsampcnt <- SApopdat$condsampcnt
  states <- SApopdat$states
  invyrs <- SApopdat$invyrs
  adj <- SApopdat$adj
  estvar.area <- SApopdat$estvar.area
  predfac <- SApopdat$predfac

  ## check SAdomsdf
  ########################################################
  SAdomsdf <- pcheck.table(SAdomsdf, tabnm="SAdomsdf", caption="SAdoms?")
  if (is.null(SAdomsdf) && addSAdomsdf) {
    message("need to add SAdomsdf when addSAdomsdf = TRUE")
    addSAdomsdf <- FALSE
  }

  ## check smallbnd.att
  ########################################################
  smallbnd.att <- FIESTA::pcheck.varchar(var2check=smallbnd.att, varnm="smallbnd.att", 
		checklst=names(SAdomsdf), caption="smallbnd attribute for output?", 
		multiple=FALSE, stopifnull=FALSE)
  if (is.null(smallbnd.att)) smallbnd.att <- "DOMAIN"


  ## Check prednames
  if (is.null(prednames)) {
    prednames <- SApopdat$prednames
  } else {
    if (!all(prednames %in% SApopdat$prednames)) {
      stop("invalid prednames... must be in: ", toString(SApopdat$prednames))
    }
  }

  ########################################
  ## Check area units
  ########################################
  unitchk <- pcheck.areaunits(unitarea=dunitarea, areavar=areavar, 
			areaunits=areaunits, metric=metric)
  dunitarea <- unitchk$unitarea
  areavar <- unitchk$areavar
  areaunits <- unitchk$outunits


  ###################################################################################
  ## Check parameters and apply plot and condition filters
  ###################################################################################
  estdat <- check.estdata(esttype=esttype, totals=totals, pltcondf=pltcondx, 
	cuniqueid=cuniqueid, condid=condid, treex=treex, seedx=seedx, estseed=estseed, 
	sumunits=sumunits, landarea=landarea, ACI.filter=ACI.filter, pcfilter=pcfilter,
	allin1=allin1, estround=estround, pseround=pseround, divideby=divideby,
 	addtitle=addtitle, returntitle=returntitle, rawdata=rawdata, rawonly=rawonly, 
	savedata=savedata, outfolder=outfolder, overwrite_dsn=overwrite_dsn, 
	overwrite_layer=overwrite_layer, outfn.pre=outfn.pre, outfn.date=outfn.date, 
	append_layer=append_layer, raw_fmt=raw_fmt, raw_dsn=raw_dsn, gui=gui)
  if (is.null(estdat)) return(NULL)
  pltcondf <- estdat$pltcondf
  cuniqueid <- estdat$cuniqueid
  treef <- estdat$treef
  seedf <- estdat$seedf
  tuniqueid <- estdat$tuniqueid
  estseed <- estdat$estseed
  sumunits <- estdat$sumunits
  landarea <- estdat$landarea
  allin1 <- estdat$allin1
  estround <- estdat$estround
  pseround <- estdat$pseround
  divideby <- estdat$divideby
  addtitle <- estdat$addtitle
  returntitle <- estdat$returntitle
  rawdata <- estdat$rawdata
  rawonly <- estdat$rawonly
  savedata <- estdat$savedata
  outfolder <- estdat$outfolder
  overwrite_layer <- estdat$overwrite_layer
  append_layer <- estdat$append_layer
  layer.pre <- estdat$layer.pre
  raw_fmt <- estdat$raw_fmt
  raw_dsn <- estdat$raw_dsn
  rawfolder <- estdat$rawfolder
  totals <- estdat$totals

 
  ## Check output for multest 
  ########################################################
  if (savedata || savemultest) {
    fmtlst <- c("sqlite", "sqlite3", "db", "db3", "gpkg", "csv", "gdb")

    if (multest) {
      multest_outfolder <- FIESTA::pcheck.outfolder(multest_outfolder, gui)
      if (is.null(multest_outfolder)) multest_outfolder <- outfolder

      multest.append <- FIESTA::pcheck.logical(multest.append, varnm="multest.append", 
		title="Append multest data?", first="NO", gui=gui) 

      multest_fmt <- FIESTA::pcheck.varchar(var2check=multest_fmt, varnm="multest_fmt", 
		checklst=fmtlst, gui=gui, caption="Output multest format?") 
      if (multest_fmt == "csv") {
        multest_dsn <- NULL
      } else {
        if (is.null(multest_dsn)) {
          multest_dsn <- paste0("SAmultest_", SApackage, ".", multest_fmt)
        }
        if (multest_fmt == "gdb") {
          multest_dsn <- DBtestESRIgdb(gdbfn=multest_dsn, outfolder=outfolder, 
			overwrite=overwrite_dsn, showlist=FALSE, returnpath=FALSE)
        }	else if (multest_fmt %in% c("sqlite", "gpkg")) {
          gpkg <- ifelse(multest_fmt == "gpkg", TRUE, FALSE)
          if (multest.append || !overwrite_dsn) {
            multest_dsn <- DBtestSQLite(SQLitefn=multest_dsn, gpkg=gpkg, outfolder=outfolder, 
			showlist=FALSE, returnpath=FALSE, createnew=TRUE)
          } else {
            multest_dsn <- DBcreateSQLite(SQLitefn=multest_dsn, gpkg=gpkg, outfolder=outfolder, 
			overwrite=overwrite_dsn, returnpath=FALSE, outfn.date=outfn.date)
          }
        }	
      }
    }
  }
 
  ###################################################################################
  ### GET ROW AND COLUMN INFO FROM condf
  ###################################################################################
  if (!sumunits) col.add0 <- TRUE
  rowcolinfo <- check.rowcol(gui=gui, esttype=esttype, treef=treef, seedf=seedf,
	condf=pltcondf, cuniqueid=cuniqueid, rowvar=rowvar, rowvar.filter=rowvar.filter, 
	colvar=colvar, colvar.filter=colvar.filter, row.FIAname=row.FIAname, 
	col.FIAname=col.FIAname, row.orderby=row.orderby, col.orderby=col.orderby,
 	row.add0=row.add0, col.add0=col.add0, title.rowvar=title.rowvar, 
	title.colvar=title.colvar, rowlut=rowlut, collut=collut, rowgrp=rowgrp, 
	rowgrpnm=rowgrpnm, rowgrpord=rowgrpord, landarea=landarea) 
  treef <- rowcolinfo$treef
  seedf <- rowcolinfo$seedf
  condf <- rowcolinfo$condf
  uniquerow <- rowcolinfo$uniquerow
  uniquecol <- rowcolinfo$uniquecol
  domainlst <- rowcolinfo$domainlst
  rowvar <- rowcolinfo$rowvar
  colvar <- rowcolinfo$colvar
  row.orderby <- rowcolinfo$row.orderby
  col.orderby <- rowcolinfo$col.orderby
  row.add0 <- rowcolinfo$row.add0
  col.add0 <- rowcolinfo$col.add0
  title.rowvar <- rowcolinfo$title.rowvar
  title.colvar <- rowcolinfo$title.colvar
  bytdom <- rowcolinfo$bytdom
  tdomvar <- rowcolinfo$tdomvar
  tdomvar2 <- rowcolinfo$tdomvar2
  grpvar <- rowcolinfo$grpvar

  #rm(rowcolinfo)  

  ## Generate a uniquecol for estimation units
  if (!sumunits && colvar == "NONE") {
    uniquecol <- data.table(dunitarea[[dunitvar]])
    setnames(uniquecol, dunitvar)
    uniquecol[[dunitvar]] <- factor(uniquecol[[dunitvar]])
  }

 
  if (esttype == "AREA") {
    estvar.name <- estvar.area
    estvarunits <- areaunits

    setkeyv(condx, c(cuniqueid, condid))
    setkeyv(condf, c(cuniqueid, condid))
    cdomdat <- merge(condx, condf, by=c(cuniqueid, condid), all.x=TRUE)
  
  } else {
    #####################################################################################
    ### Get estimation data from tree table, with plot-level adjustment for nonresponse
    #####################################################################################
    adjtree <- ifelse(adj %in% c("samp", "plot"), TRUE, FALSE)
    treedat <- check.tree(gui=gui, treef=treef, seedf=seedf, estseed=estseed, 
		bycond=TRUE, condf=condf, bytdom=bytdom, tuniqueid=tuniqueid, 
		cuniqueid=cuniqueid, esttype=esttype, estvarn=estvar, 
		estvarn.filter=estvar.filter, esttotn=TRUE, tdomvar=tdomvar, 
		adjtree=adjtree, metric=metric)
    if (is.null(treedat)) return(NULL) 
    estvar <- treedat$estvar
    estvar.name <- treedat$estvar.name
    estvar.filter <- treedat$estvar.filter
    tdomvarlst <- treedat$tdomvarlst
    estvarunits <- treedat$estunits

    tdomdat <- treedat$tdomdat
    if (rowvar != "TOTAL") {
      if (!row.add0 && any(tdomdat[[rowvar]] == 0)) {
        tdomdat <- tdomdat[tdomdat[[rowvar]] != 0,]
      }
      if (colvar != "NONE") {
        if (!col.add0 && any(tdomdat[[colvar]] == 0)) {
          tdomdat <- tdomdat[tdomdat[[colvar]] != 0,]
        }
      }
    }

    cdomdat <- merge(condx, tdomdat, by=c(cuniqueid, condid), all.x=TRUE)
    #cdomdat <- DT_NAto0(tdomdat, estvar.name, 0)
  }
 
  #####################################################################################
  ### GET TITLES FOR OUTPUT TABLES
  #####################################################################################
  if (is.null(title.dunitvar)) title.dunitvar <- smallbnd.att
  alltitlelst <- check.titles(dat=cdomdat, esttype=esttype, estseed=estseed, 
	sumunits=sumunits, title.main=title.main, title.ref=title.ref, 
	title.rowvar=title.rowvar, title.colvar=title.colvar, 
	title.unitvar=title.dunitvar, title.filter=title.filter, 
	title.unitsn=estvarunits, title.estvarn=title.estvar, unitvar=dunitvar, 
	rowvar=rowvar, colvar=colvar, estvarn=estvar, 
	estvarn.filter=estvar.filter, addtitle=addtitle, returntitle=returntitle, 
	rawdata=rawdata, states=states, invyrs=invyrs, landarea=landarea, 
	pcfilter=pcfilter, allin1=allin1, divideby=divideby, parameters=FALSE, 
	outfn.pre=outfn.pre)
  title.dunitvar <- alltitlelst$title.unitvar
  title.est <- alltitlelst$title.est
  title.pse <- alltitlelst$title.pse
  title.estpse <- alltitlelst$title.estpse
  title.ref <- alltitlelst$title.ref
  outfn.estpse <- alltitlelst$outfn.estpse
  outfn.param <- alltitlelst$outfn.param
  if (rawdata) {
    outfn.rawdat <- alltitlelst$outfn.rawdat
    outfn.rawdat <- paste0(outfn.rawdat, "_modSA_", SApackage, "_", SAmethod) 
  } 
  ## Append name of package and method to outfile name
  outfn.estpse <- paste0(outfn.estpse, "_modSA_", SApackage, "_", SAmethod) 
 
  #####################################################################################
  ## GENERATE ESTIMATES
  #####################################################################################
  dunit_totest=dunit_rowest=dunit_colest=dunit_grpest=rowunit=totunit <- NULL
  response <- estvar.name
  #setnames(cdomdat, dunitvar, "DOMAIN")

  message("getting estimates...")
  message("using the following predictors...", toString(prednames))

  ############################################################################
  ## Generate models
  ############################################################################
  # set up formula with user-defined response and predictors
  fmla <- as.formula(paste(response," ~ ", paste(prednames, collapse= "+")))

  ## Sum estvar.name by dunitvar (DOMAIN), plot, domain
  pdomdat <- cdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(dunitvar, cuniqueid, "TOTAL", prednames), .SDcols=estvar.name]

  ## Add mean response to dunitlut for Area-level estimates
  estmean <- pdomdat[, list(mean=mean(get(estvar.name), na.rm=TRUE),
			mean.var=var(get(estvar.name), na.rm=TRUE)), by="DOMAIN"]
  setkey(estmean, "DOMAIN")
  dunitlut <- merge(dunitlut, estmean)
  setnames(dunitlut, c("mean", "mean.var"), c(estvar.name, paste0(estvar.name, ".var")))
  domain <- rowvar
 
  if (is.null(largebnd.att)) {
    pdomdat$LARGEBND <- 1
    largebnd.att <- "LARGEBND"
  }   
  ## get unique largebnd values
  largebnd.vals <- sort(unique(pdomdat[[largebnd.att]]))
  largebnd.vals <- largebnd.vals[table(pdomdat[[largebnd.att]]) > 30]
 
  ## get estimate by domain, by largebnd value
  message("generating unit-level estimates for ", response, " using ", SApackage, "...")
  dunit_multest.unit <- 
	tryCatch(
		do.call(rbind, lapply(largebnd.vals, SAest.large, 
			dat=pdomdat, cuniqueid=cuniqueid, 
			largebnd.att=largebnd.att, dunitlut=dunitlut, dunitvar=dunitvar,
			SApackage=SApackage, SAmethod="unit", 
			prednames=prednames, fmla=fmla, domain=domain,
			response=response)),
     	 error=function(e) {
			message("error with unit-level estimates of ", response, "...")
			message(e, "\n")
			return(NULL) }) 
 
  ## get estimate by domain, by largebnd value
  message("generating area-level estimates for ", response, "...")
  dunit_multest.area <- 
	tryCatch(
    		do.call(rbind, lapply(largebnd.vals, SAest.large, 
			dat=pdomdat, cuniqueid=cuniqueid, 
			largebnd.att=largebnd.att, dunitlut=dunitlut, dunitvar=dunitvar,
			SApackage=SApackage, SAmethod="area", 
			prednames=prednames, fmla=fmla, domain=domain,
			response=response)),
     	 error=function(e) {
			message("error with area-level estimates of ", response, "...")
			message(e, "\n")
			return(NULL) })  

  if (!is.null(dunit_multest.unit)) {
    if (ncol(dunit_multest.unit) == 1) {
      dunit_multest.unit <- NULL
    }
    SAunit.vars <- c("DIR", "DIR.se", "JU.Synth", "JU.GREG", "JU.GREG.se", 
		"JU.EBLUP", "JU.EBLUP.se.1")
  }
  if (!is.null(dunit_multest.area)) {
    if (ncol(dunit_multest.area) == 1) {
      dunit_multest.area <- NULL
    }
    SAarea.vars <- c("JFH", "JFH.se", "JA.synth", "JA.synth.se")
  }
 
  if (!is.null(dunit_multest.unit) && !is.null(dunit_multest.area)) {
    dunit_multest <- merge(
		dunit_multest.unit[, c(domain, "AOI", dunitvar, "NBRPLT", "NBRPLT.gt0", 
			SAunit.vars), with=FALSE],
 		dunit_multest.area[, c(domain, dunitvar, SAarea.vars), with=FALSE],
		by=c(domain, dunitvar), all.x=TRUE)
    estkey <- c(domain, largebnd.att, dunitvar)
    estkey <- estkey[estkey %in% names(dunit_multest)]
    setkeyv(dunit_multest, estkey)

    dunit_multest[, JoSAE := ifelse(is.na(dunit_multest$JFH), JU.EBLUP, JFH)]  
    dunit_multest[, JoSAE.se := ifelse(is.na(dunit_multest$JFH), JU.EBLUP, JU.EBLUP.se.1)]

    SAcombo.vars <- c("JoSAE", "JoSAE.se")  

  } else {
    dunit_multest <- NULL
  }

#  if (addSAdomsdf && !is.null(dunit_multest)) {
#    dunit_multest_SAdoms <- copy(dunit_multest)
#    dunit_multest_SAdoms[, AOI := NULL]
#    dunit_multest_SAdoms <- merge(SAdomsdf[, names(SAdomsdf)[
#		names(SAdomsdf) != areavar], with=FALSE], dunit_multest_SAdoms, by=dunitvar)         
#  } else if (!is.null(smallbnd.att)) {
#    dunit_multest_SAdoms <- merge(SAdomsdf[, unique(c(dunitvar, smallbnd.att)), with=FALSE], 
#		dunit_multest_SAdoms, by=dunitvar)
#  } 

  if (SAmethod == "unit") {
    nhat <- "JU.EBLUP"
    nhat.se <- "JU.EBLUP.se.1"
    nhat.var <- "JU.EBLUP.var"
    nhat.cv <- "JU.EBLUP.cv"

  } else if (SAmethod == "area") {
    nhat <- "JFH"
    nhat.se <- "JFH.se"
    nhat.var <- "JFH.var"
    nhat.cv <- "JFH.cv"

  } else if (SAmethod == "combo") {
    nhat <- "JoSAE"
    nhat.se <- "JoSAE.se"
    nhat.var <- "JoSAE.var"
    nhat.cv <- "JoSAE.cv"

  } 
  ## Subset dunit_multest.unit to estimation output
  dunit_totest <- dunit_multest[AOI==1, 
		c(dunitvar, nhat, nhat.se, "NBRPLT.gt0"), with=FALSE]
  setkeyv(dunit_totest, dunitvar)

  ## Merge dunitarea
  tabs <- FIESTA::check.matchclass(dunitarea, dunit_totest, dunitvar)
  dunitarea <- tabs$tab1
  dunit_totest <- tabs$tab2
  dunit_totest <- merge(dunit_totest, 
		dunitarea[, c(dunitvar, "AREAUSED"), with=FALSE], by=dunitvar)

  if (!is.null(dunit_totest)) {
    if (totals) {
      dunit_totest <- getarea(dunit_totest, areavar=areavar, esttype=esttype,
				nhatcol=nhat, nhatcol.var=nhat.var)
      estnm <- "est"
    } else {
      dunit_totest[, (nhat.var) := get(nhat.se)^2]
      dunit_totest[, (nhat.cv) := get(nhat.se)/get(nhat)]
      dunit_totest[, pse := get(nhat.cv) * 100]
      estnm <- nhat
    }
  }

  ###################################################################################
  ## GENERATE OUTPUT TABLES
  ###################################################################################
  message("getting output...")
  tabs <- est.outtabs(esttype=esttype, sumunits=sumunits, areavar=areavar, 
	unitvar=smallbnd.att, unit_totest=dunit_totest, unit_rowest=dunit_rowest, 
	unit_colest=dunit_colest, unit_grpest=dunit_grpest, rowvar=rowvar, colvar=colvar, 
	uniquerow=uniquerow, uniquecol=uniquecol, rowgrp=rowgrp, rowgrpnm=rowgrpnm, 
	rowunit=rowunit, totunit=totunit, allin1=allin1, savedata=savedata, 
	addtitle=addtitle, title.ref=title.ref, title.colvar=title.colvar, 
	title.rowvar=title.rowvar, title.rowgrp=title.rowgrp, title.unitvar=title.dunitvar,
 	title.estpse=title.estpse, title.est=title.est, title.pse=title.pse, 
	rawdata=rawdata, rawonly=rawonly, outfn.estpse=outfn.estpse, outfolder=outfolder, 
	outfn.date=outfn.date, overwrite=overwrite_layer, estnm=estnm, estround=estround, 
	pseround=pseround, divideby=divideby, returntitle=returntitle,
	estnull=estnull, psenull=psenull) 
  est2return <- tabs$tabest
  pse2return <- tabs$tabpse

  est2return[is.na(est2return$Estimate), "Estimate"] <- estnull 
  if ("Percent Sampling Error" %in% names(est2return)) {
    est2return[is.na(est2return$"Percent Sampling Error"), 
		"Percent Sampling Error"] <- psenull 
  }

  if (!is.null(est2return)) {
    returnlst$est <- est2return
  } 
  if (!is.null(pse2return)) {
    returnlst$pse <- pse2return 
  }
  if (returntitle) {
    returnlst$titlelst <- alltitlelst
  }

  if (multest && !is.null(dunit_multest)) {
    ## Merge dunitarea
    #tabs <- FIESTA::check.matchclass(dunitarea, dunit_multest, dunitvar)
    #dunitarea <- tabs$tab1
    #dunit_multest <- tabs$tab2

    dunit_multest <- merge(dunit_multest, 
		dunitarea[, c(dunitvar, "AREAUSED"), with=FALSE], by=dunitvar)
    dunit_multest[, JoSAE.total := get(nhat) * AREAUSED]
    dunit_multest[, JoSAE.pse := get(nhat.se)/get(nhat) * 100]

    ## Merge SAdom attributes to dunit_multest
    if (addSAdomsdf) {
      dunit_multest[, AOI := NULL]
      dunit_multest <- merge(SAdomsdf, dunit_multest, by="DOMAIN")
      dunit_multest <- dunit_multest[order(-dunit_multest$AOI, dunit_multest$DOMAIN),]
    } else if (!is.null(SAdomvars)) {
      invars <- SAdomvars[SAdomvars %in% names(SAdomsdf)]
      if (length(invars) == 0) stop("invalid SAdomvars")
      dunit_multest <- merge(SAdomsdf[, unique(c("DOMAIN", SAdomvars)), with=FALSE], 
					dunit_multest, by="DOMAIN")
      dunit_multest <- dunit_multest[order(-dunit_multest$AOI, dunit_multest$DOMAIN),]
    } else {
      dunit_multest <- dunit_multest[order(-dunit_multest$AOI, dunit_multest$DOMAIN),]
    }

    ## Remove TOTAL column from dunit_multest
    if (domain == "TOTAL" && "TOTAL" %in% names(dunit_multest)) {
      dunit_multest[, TOTAL := NULL]
    }
    if (multest.AOIonly) {
      ## Subset dunit_multest, where AOI = 1
      dunit_multest <- dunit_multest[dunit_multest$AOI == 1, ]
    }

    ## Save multest table
    if (savemultest) {

      ## Remove TOTAL column from est
      if (domain == "TOTAL" && "TOTAL" %in% names(dunit_multest)) {
        dunit_multest[, TOTAL := NULL]
      }
      ## Remove column headings if appending to csv file
      if (multest.append && multest_fmt == "csv") {
        dunit_multest <- setDF(dunit_multest)
        colnames(dunit_multest) <- NULL
      }
      if (is.null(multest_layer)) {
        if (multest_fmt == "csv") {
          multest_layer <- paste0("SAmultest_", SApackage, "_", response, ".csv")
        } else {
          multest_layer <- paste0(SApackage, "_", response)
        }
      }
      ## Export dunit_multest
      overwrite_layer <- ifelse(multest.append, FALSE, overwrite_layer)
      datExportData(dunit_multest, out_fmt=multest_fmt, outfolder=multest_outfolder, 
 		out_dsn=multest_dsn, out_layer=multest_layer, overwrite_layer=overwrite_layer, 
		append_layer=multest.append)
    }
  } 
 
  if (rawdata) {
    rawdat <- tabs$rawdat
    names(rawdat)[names(rawdat) == "unit_totest"] <- "dunit_totest"
    rawdat$domdat <- setDF(cdomdat)

    if (savedata) {
      if (!is.null(title.estpse)) {
        title.raw <- paste(title.estpse, title.ref)
      } else {
        title.raw <- title.est
      }

      for (i in 1:length(rawdat)) {
        tabnm <- names(rawdat[i])
        if (!tabnm %in% c(prednames)) {
          rawtab <- rawdat[[i]]

          outfn.rawtab <- paste0(outfn.rawdat, "_", tabnm) 
          if (tabnm %in% c("plotsampcnt", "condsampcnt")) {
            write2csv(rawtab, outfolder=rawfolder, outfilenm=outfn.rawtab, 
			outfn.date=outfn.date, overwrite=overwrite_layer)
          } else if (is.data.frame(rawtab)) {
            if (raw_fmt != "csv") {
              out_layer <- tabnm 
            } else {
              out_layer <- outfn.rawtab
            }
            datExportData(rawtab, out_fmt=raw_fmt, outfolder=rawfolder, 
 			out_dsn=raw_dsn, out_layer=out_layer, 
			overwrite_layer=overwrite_layer, add_layer=TRUE, 
			append_layer=append_layer)
          }
        }
      }
    }
    rawdat$esttype <- esttype
    rawdat$SApackage <- SApackage
    rawdat$SAmethod <- SAmethod
    if (esttype == "TREE") {
      rawdat$estvar <- response
      rawdat$estvar.filter <- estvar.filter
    }
    if (!is.null(rowvar)) rawdat$rowvar <- rowvar
    if (!is.null(colvar)) rawdat$colvar <- colvar
    rawdat$areaunits <- areaunits
    rawdat$estunits <- estvarunits
    returnlst$raw <- rawdat  
  }

  if (multest) {
    returnlst$dunit_multest <- setDF(dunit_multest)
  }
  if (returnSApopdat) {
    returnlst$SApopdat <- SApopdat
  }

  ## Save objects for testing
  if (save4testing) {
    message("saving object for testing")
    returnlst$pdomdat <- pdomdat
    returnlst$dunitlut <- dunitlut
    returnlst$cuniqueid <- cuniqueid
  }


  return(returnlst)
}


