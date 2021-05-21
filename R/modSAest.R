modSAest <- function(SApopdat=NULL, SAdomsdf=NULL, prednames=NULL, 
	SApackage="JoSAE", SAmethod="unit", esttype="TREE", estseed="none", 
	largebnd.att=NULL, landarea="ALL", pcfilter=NULL, estvar=NULL, 
	estvar.filter=NULL, smallbnd.att=NULL, allin1=FALSE, metric=FALSE, 
	estround=0, pseround=3, estnull=0, psenull="--", divideby=NULL, 
	savedata=FALSE, rawdata=FALSE, rawonly=FALSE, multest=TRUE, 
	addSAdomsdf=TRUE, SAdomvars=NULL, outfolder=NULL, outfn.pre=NULL, 
	outfn.date=FALSE, addtitle=TRUE, raw_fmt="csv", raw_dsn="rawdata", 
	multest_fmt="csv", multest_outfolder=NULL, multest_dsn=NULL, multest_layer=NULL, 
	multest.append=FALSE, multest.AOIonly=FALSE, overwrite_dsn=FALSE,
	overwrite_layer=TRUE, append_layer=FALSE, returntitle=FALSE, title.main=NULL, 
	title.ref=NULL, title.dunitvar=NULL, title.estvar=NULL, title.filter=NULL, 
	save4testing=FALSE, ...){


  ######################################################################################
  ## DESCRIPTION: 
  ## Generates model-assisted estimates by domain (and estimation unit)
  ## if saveraw...  and raw_fmt = 'csv', a new folder is created within the outfolder
  ##			named as raw_dsn. If raw_fmt != 'csv', a database is created
  ##			within the outfolder names as raw_dsn. 
  ######################################################################################
  gui=addSAdomsdf <- FALSE

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
	title.rowvar=title.colvar=TOTAL <- NULL


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
  unitcombine <- FALSE
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
    if (!"JoSAE" %in% rownames(installed.packages()))
	 stop("SApackage JoSAE requires package JoSAE.")
  } else {
    if (!"sae" %in% rownames(installed.packages()))
	 stop("SApackage sae requires package sae.")
  }

  ## Check SAmethod 
  SAmethodlst <- c("unit", "area")
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


  ## check SAdomsdf
  ########################################################
  SAdomsdf <- pcheck.table(SAdomsdf, tabnm="SAdomsdf", caption="SAdoms?")
  if (!is.null(SAdomsdf)) {
    addSAdomsdf <- TRUE
  }

  ## check SAdomsdf
  ########################################################
  SAdomsdf <- pcheck.table(SAdomsdf, tabnm="SAdomsdf", caption="SAdoms?")

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

  ###################################################################################
  ## Check parameters and apply plot and condition filters
  ###################################################################################
  estdat <- check.estdata(esttype=esttype, pltcondf=pltcondx, cuniqueid=cuniqueid,
 	condid=condid, treex=treex, seedx=seedx, estseed=estseed, sumunits=sumunits, 
	landarea=landarea, ACI.filter=ACI.filter, pcfilter=pcfilter,
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
  layer.pre <- estdat$layer.pre
  raw_fmt <- estdat$raw_fmt
  raw_dsn <- estdat$raw_dsn
  rawfolder <- estdat$rawfolder


  ## Check output for multest 
  ########################################################
  if (savedata) {
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
  unit.totest=unit.domest=unit.domest=unit.rowest=unit.colest=unit.grpest=
	rowunit=totunit <- NULL
  response <- estvar.name
  #setnames(cdomdat, dunitvar, "DOMAIN")

  message("getting estimates...")
  message("using the following predictors...", toString(prednames))

  ############################################################################
  ## Generate models
  ############################################################################

  # set up formula with user-defined response and predictors
  fmla <- as.formula(paste(response," ~ ", paste(prednames, collapse= "+")))

  domain <- rowvar
  estkey <- "DOMAIN"

  ## Get total estimate and merge area
  domain <- rowvar

  if (esttype == "AREA") {
    cdomdat[is.na(cdomdat[[domain]]), estvar.name] <- 0
  }

  ## Sum estvar.name by dunitvar (DOMAIN), plot, domain
  pdomdat <- cdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(dunitvar, cuniqueid, "TOTAL", prednames), .SDcols=estvar.name]


  if (sum(pdomdat[[response]]) == 0) {
    return(list(est=NULL, estvar=response, pdomdat=pdomdat))
  }

  if (is.null(largebnd.att)) {
    estkey <- "DOMAIN"

    ## get unique domains
    doms <- as.character(na.omit(unique(pdomdat[[domain]])))

    ## get estimate by domain (SAmethod="unit")
    message("generating unit-level estimates for ", response, "...")
    dunit.multest.unit <- 
	tryCatch(
      	do.call(rbind, lapply(doms, SAest.dom, 
			pdomdat=pdomdat, cuniqueid=cuniqueid, 
     			dunitlut=dunitlut, dunitvar=dunitvar, 
			SApackage=SApackage, SAmethod="unit", 
			esttype=esttype, prednames=prednames, fmla=fmla,
			domain=domain, response=response)),
     	 error=function(e) {
			message("error with ", response)
			return(NULL) }) 

    ## get estimate by domain (SAmethod="area")
    message("generating area-level estimates for ", response, "...")
    dunit.multest.area <- 
	tryCatch(
		do.call(rbind, lapply(doms, SAest.dom, 
			pdomdat=pdomdat, cuniqueid=cuniqueid, 
     			dunitlut=dunitlut, dunitvar=dunitvar, 
			SApackage=SApackage, SAmethod="area", 
			esttype=esttype, prednames=prednames, fmla=fmla,
			domain=domain, response=response)), 
     	 error=function(e) {
			message("error with ", response)
			return(NULL) })  
  } else {
    estkey <- c(largebnd.att, "DOMAIN")

    ## get unique largebnd values
    largebnd.vals <- sort(unique(pdomdat[[largebnd.att]]))
    largebnd.vals <- largebnd.vals[table(pdomdat[[largebnd.att]]) > 30]

    ## get estimate by domain, by largebnd value
    message("generating unit-level estimates for ", response, "...")
    dunit.multest.unit <- 
	tryCatch(
		do.call(rbind, lapply(largebnd.vals, SAest.large, 
			pdomdat=pdomdat, cuniqueid=cuniqueid, 
			largebnd.att=largebnd.att, dunitlut=dunitlut, dunitvar=dunitvar,
			SApackage=SApackage, SAmethod="unit", esttype=esttype, 
			prednames=prednames, fmla=fmla, domain=domain,
			response=response)),
     	 error=function(e) {
			message("error with ", response)
			return(NULL) })  

    ## get estimate by domain, by largebnd value
    message("generating area-level estimates for ", response, "...")
    dunit.multest.area <- 
	tryCatch(
    		do.call(rbind, lapply(largebnd.vals, SAest.large, 
			pdomdat=pdomdat, cuniqueid=cuniqueid, 
			largebnd.att=largebnd.att, dunitlut=dunitlut, dunitvar=dunitvar,
			SApackage=SApackage, SAmethod="area", esttype=esttype, 
			prednames=prednames, fmla=fmla, domain=domain,
			response=response)),
     	 error=function(e) {
			message("error with ", response)
			return(NULL) })  

  }

  if (!is.null(dunit.multest.unit)) {
    if (ncol(dunit.multest.unit) == 1) {
      dunit.multest.unit <- NULL
    }
    SAunit.vars <- c("JU.Synth", "JU.GREG", "JU.GREG.se", "JU.EBLUP", "JU.EBLUP.se.1")
  }
  if (!is.null(dunit.multest.area)) {
    if (ncol(dunit.multest.area) == 1) {
      dunit.multest.area <- NULL
    }
    SAarea.vars <- c("JFH", "JFH.se", "JA.synth", "JA.synth.se")
  }
 
  if (!is.null(dunit.multest.unit) && !is.null(dunit.multest.area)) {
    dunit.multest <- merge(
		dunit.multest.unit[, c(domain, "AOI", dunitvar, "NBRPLT", "NBRPLT.gt0", 
			"DIR", "DIR.se", SAunit.vars), with=FALSE],
 		dunit.multest.area[, c(domain, dunitvar, SAarea.vars), with=FALSE],
		by=c(domain, dunitvar), all.x=TRUE)
    setkeyv(dunit.multest, c(domain, estkey))
  } else {
    dunit.multest <- NULL
  }
 

  if (SAmethod == "unit" && !is.null(dunit.multest.unit)) {
    nhat <- "JU.EBLUP"
    nhat.se <- "JU.EBLUP.se.1"
    nhat.var <- "JU.EBLUP.var"  

    ## Subset dunit.multest.unit to estimation output
    est <- dunit.multest.unit[AOI==1, c(dunitvar, nhat, nhat.se), with=FALSE]

  } else if (SAmethod == "area" && !is.null(dunit.multest.area)) {
    nhat <- "JFH"
    nhat.se <- "JFH.se"
    nhat.var <- "JFH.EBLUP.var"

    ## Subset dunit.multest.area to estimation output
    est <- dunit.multest.area[AOI==1, c(dunitvar, nhat, nhat.se), with=FALSE]
  } else {
    est <- NULL
  }

  if (!is.null(est)) {
    if (addSAdomsdf) {
      est <- merge(SAdomsdf, est, by=dunitvar)         
    } else if (!is.null(smallbnd.att)) {
      est <- merge(SAdomsdf[, unique(c(dunitvar, smallbnd.att)), with=FALSE], est, by=dunitvar)
    } 
    est[, (nhat.var) := get(nhat.se)^2]
    setkeyv(est, dunitvar)

    ## Merge dunitarea
    tabs <- FIESTA::check.matchclass(dunitarea, est, dunitvar)
    dunitarea <- tabs$tab1
    est <- tabs$tab2

    est <- est[dunitarea, nomatch=0]
    est <- getarea(est, areavar=areavar, esttype=esttype,
				nhatcol=nhat, nhatcol.var=nhat.var)
  } else {
    returnlst <- list(est=NULL)
    if (multest) returnlst$dunit.multest <- NULL

    rawdat <- list(est=NULL, domdat=setDF(cdomdat), estvar=response, 
		estvar.filter=estvar.filter, esttype=esttype, 
		SApackage=SApackage, SAmethod=SAmethod) 
    if (!is.null(rowvar)) rawdat$rowvar <- rowvar
    if (!is.null(colvar)) rawdat$colvar <- colvar
    rawdat$areaunits <- areaunits
    rawdat$estunits <- estvarunits
    returnlst$raw <- rawdat  

    if (returnSApopdat) returnlst$SApopdat <- SApopdat

    ## Save objects for testing
    if (save4testing) {
      message("saving object for testing")
      returnlst$pdomdat <- pdomdat
      returnlst$cuniqueid <- cuniqueid
    }
    return(returnlst)
  }

  if (multest && !is.null(dunit.multest)) {
    ## Merge SAdom attributes to dunit.multest
    if (addSAdomsdf) {
      dunit.multest[, AOI := NULL]
      dunit.multest <- merge(SAdomsdf, dunit.multest, by="DOMAIN")
      dunit.multest <- dunit.multest[order(-dunit.multest$AOI, dunit.multest$DOMAIN),]
    } else if (!is.null(SAdomvars)) {
      invars <- SAdomvars[SAdomvars %in% names(SAdomsdf)]
      if (length(invars) == 0) stop("invalid SAdomvars")
      dunit.multest <- merge(SAdomsdf[, unique(c("DOMAIN", SAdomvars)), with=FALSE], 
					dunit.multest, by="DOMAIN")
      dunit.multest <- dunit.multest[order(-dunit.multest$AOI, dunit.multest$DOMAIN),]
    } else {
      dunit.multest <- dunit.multest[order(-dunit.multest$AOI, dunit.multest$DOMAIN),]
    }  

    ## Remove TOTAL column from dunit.multest
    if (domain == "TOTAL" && "TOTAL" %in% names(dunit.multest)) {
      dunit.multest[, TOTAL := NULL]
    }
    if (multest.AOIonly) {
      ## Subset dunit.multest, where AOI = 1
      dunit.multest <- dunit.multest[dunit.multest$AOI == 1, ]
    }
    ## Save multest table
    if (savedata) {

      ## Remove TOTAL column from est
      if (domain == "TOTAL" && "TOTAL" %in% names(dunit.multest)) {
        dunit.multest[, TOTAL := NULL]
      }
      ## Remove column headings if appending to csv file
      if (multest.append && multest_fmt == "csv") {
        dunit.multest <- setDF(dunit.multest)
        colnames(dunit.multest) <- NULL
      }
      if (is.null(multest_layer)) {
        if (multest_fmt == "csv") {
          multest_layer <- paste0("SAmultest_", SApackage, "_", response, ".csv")
        } else {
          multest_layer <- paste0(SApackage, "_", response)
        }
      }

      ## Export dunit.multest
      overwrite_layer <- ifelse(multest.append, FALSE, overwrite_layer)
      datExportData(dunit.multest, out_fmt=multest_fmt, outfolder=multest_outfolder, 
 		out_dsn=multest_dsn, out_layer=multest_layer, overwrite_layer=overwrite_layer, 
		append_layer=multest.append)
    }
  } 


  ###################################################################################
  ## GENERATE OUTPUT TABLES
  ###################################################################################
  message("getting output...")
  estnm <- "est"
  unit.totest <- setDT(est)
  tabs <- est.outtabs(esttype=esttype, sumunits=sumunits, areavar=areavar, 
	unitvar=smallbnd.att, unit.totest=unit.totest, unit.rowest=unit.rowest, 
	unit.colest=unit.colest, unit.grpest=unit.grpest, rowvar=rowvar, colvar=colvar, 
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

  if (rawdata) {
    rawdat <- tabs$rawdat
    names(rawdat)[names(rawdat) == "unit.totest"] <- "dunit.totest"
    rawdat$domdat <- setDF(cdomdat)
    rawdat$estvar <- response
    rawdat$estvar.filter <- estvar.filter
  }
  if (returntitle) {
    titlelst <- tabs$titlelst
  }

  if (savedata) {
    if (rawdata) {
      rawfolder <- paste(outfolder, "rawdata", sep="/")
      if (!file.exists(rawfolder)) dir.create(rawfolder)

      if (!is.null(title.estpse)) {
        title.raw <- paste(title.estpse, title.ref)
      } else {
        title.raw <- title.est
      }
      for (i in 1:length(rawdat)) {
        tabnm <- names(rawdat[i])
        rawtab <- rawdat[[i]]
        outfn.rawtab <- paste0(outfn.rawdat, "_", tabnm, ".csv") 
        if (!tabnm %in% c("estvar", "estvar.filter")) {
          suppressWarnings(save1tab(tab=rawtab, tab.title=title.raw, 
			outfolder=rawfolder, allin1=allin1, coltitlerow=FALSE, 
			rowtotal=FALSE, outfn=outfn.rawtab, addtitle=FALSE,
			addformat=FALSE, outfn.date=outfn.date, overwrite=TRUE))
        }
      }
    }
  }


  returnlst <- list(est=est2return)
  if (multest) returnlst$dunit.multest <- dunit.multest 
  if (rawdata) {
    rawdat$esttype <- esttype
    rawdat$SApackage <- SApackage
    rawdat$SAmethod <- SAmethod
    if (!is.null(rowvar)) rawdat$rowvar <- rowvar
    if (!is.null(colvar)) rawdat$colvar <- colvar
    rawdat$areaunits <- areaunits
    rawdat$estunits <- estvarunits
    returnlst$raw <- rawdat
  }
  if (returntitle) returnlst$titlelst <- alltitlelst
  if (returnSApopdat) returnlst$SApopdat <- SApopdat

  ## Save objects for testing
  if (save4testing) {
    message("saving object for testing")
    returnlst$pdomdat <- pdomdat
    returnlst$cuniqueid <- cuniqueid
  }


  return(returnlst)
}


