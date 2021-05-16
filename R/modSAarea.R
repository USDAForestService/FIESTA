modSAarea <- function(SApopdat=NULL, SAdomsdf=NULL, prednames=NULL, SApackage="JoSAE", 
	SAmethod="unit", largebnd.att=NULL, landarea="ALL", pcfilter=NULL, 
	smallbnd.att=NULL, allin1=FALSE, metric=FALSE, estround=0, pseround=3, 
	estnull=0, psenull="--", divideby=NULL, savedata=FALSE, rawdata=FALSE, 
	rawonly=FALSE, multest=TRUE, addSAdomsdf=TRUE, SAdomvars=NULL, outfolder=NULL, 
	outfn.pre=NULL, outfn.date=FALSE, addtitle=TRUE, raw_fmt="csv", raw_dsn="rawdata", 
	multest_fmt="csv", multest_outfolder=NULL, multest_dsn=NULL, multest_layer=NULL, 
	multest.append=FALSE, multest.AOIonly=FALSE, overwrite_dsn=FALSE,
	overwrite_layer=TRUE, append_layer=FALSE, returntitle=FALSE, title.main=NULL, 
	title.ref=NULL, title.dunitvar=NULL, title.filter=NULL, ...){

  ########################################################################################
  ## DESCRIPTION: 
  ## Generates model-assisted estimates by domain (and estimation unit)
  ######################################################################################
  gui=addSAdomsdf <- FALSE

  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- c(names(formals(FIESTA::modSAarea)),
		names(formals(FIESTA::modSApop))) 
  if (!all(input.params %in% formallst)) {
    miss <- input.params[!input.params %in% formallst]
    stop("invalid parameter: ", toString(miss))
  }

  ## CHECK GUI - IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  if (nargs() == 0 && is.null(SApopdat)) {
    gui <- TRUE
  } 

  ## If gui.. set variables to NULL
  if (gui) { 
    landarea <- NULL
    if (!row.FIAname) row.FIAname <- NULL
    if (!col.FIAname) col.FIAname <- NULL
  }

  ## Set global variables
  ONEUNIT=n.total=n.strata=strwt=TOTAL=AOI=rowvar.filter=colvar.filter=
	title.rowvar=title.colvar <- NULL


  ##################################################################
  ## INITIALIZE SETTINGS
  ##################################################################
  options.old <- options()
  options(scipen=8) # bias against scientific notation
  on.exit(options(options.old), add=TRUE) 
  esttype <- "AREA"
  title.rowgrp <- NULL
  pvars2keep <- c("DOMAIN", "AOI")
  unitcombine <- FALSE
  returnSApopdat <- TRUE
  esttype <- "AREA"
  sumunits <- FALSE

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
    list.items <- c("condx", "pltcondx", "cuniqueid", "condid", 
		"ACI.filter", "dunitarea", "dunitvar", "dunitlut",
		"prednames", "plotsampcnt", "condsampcnt")
    SApopdat <- FIESTA::pcheck.object(SApopdat, "SApopdat", list.items=list.items)
  }		
  if (is.null(SApopdat)) return(NULL)
  SAdomsdf <- SApopdat$SAdomsdf
  condx <- SApopdat$condx
  pltcondx <- SApopdat$pltcondx
  cuniqueid <- SApopdat$cuniqueid
  condid <- SApopdat$condid
  ACI.filter <- SApopdat$ACI.filter
  dunitarea <- SApopdat$dunitarea
  areavar <- SApopdat$areavar
  areaunits <- SApopdat$areaunits
  dunitvar <- SApopdat$dunitvar
  dunitlut <- SApopdat$dunitlut
  plotsampcnt <- SApopdat$plotsampcnt
  condsampcnt <- SApopdat$condsampcnt
  states <- SApopdat$states
  invyrs <- SApopdat$invyrs
  estvar.name <- SApopdat$estvar.area
  adj <- SApopdat$adj

  ## check SAdomsdf
  ########################################################
  SAdomsdf <- pcheck.table(SAdomsdf, tabnm="SAdomsdf", caption="SAdoms?")
  if (!is.null(SAdomsdf)) {
    addSAdomsdf <- TRUE
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


  ###################################################################################
  ## Check parameters and apply plot and condition filters
  ###################################################################################
  estdat <- check.estdata(esttype=esttype, pltcondf=pltcondx, cuniqueid=cuniqueid,
 	condid=condid, sumunits=sumunits, landarea=landarea,
 	ACI.filter=ACI.filter, pcfilter=pcfilter, 
	allin1=allin1, estround=estround, pseround=pseround, divideby=divideby,
 	addtitle=addtitle, returntitle=returntitle, rawdata=rawdata, rawonly=rawonly, 
	savedata=savedata, outfolder=outfolder, overwrite_dsn=overwrite_dsn, 
	overwrite_layer=overwrite_layer, outfn.pre=outfn.pre, outfn.date=outfn.date, 
	append_layer=append_layer, raw_fmt=raw_fmt, raw_dsn=raw_dsn, gui=gui)
  if (is.null(estdat)) return(NULL)
  pltcondf <- estdat$pltcondf
  cuniqueid <- estdat$cuniqueid
  sumunits <- estdat$sumunits
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
        multest_layer <- paste0("SAmultest_", SApackage, ".", multest_fmt)
      } else {
        if (is.null(multest_dsn))
          multest_dsn <- paste0("SAmultest_", SApackage, ".", multest_fmt)
        if (is.null(multest_layer))
          multest_layer <- "dunit_multest"

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
  rowcolinfo <- check.rowcol(gui=gui, esttype=esttype, condf=pltcondf, 
	cuniqueid=cuniqueid, rowvar=rowvar, rowvar.filter=rowvar.filter, 
	colvar=colvar, colvar.filter=colvar.filter, row.FIAname=row.FIAname, 
	col.FIAname=col.FIAname, row.orderby=row.orderby, col.orderby=col.orderby,
 	row.add0=row.add0, col.add0=col.add0, title.rowvar=title.rowvar, 
	title.colvar=title.colvar, rowlut=rowlut, collut=collut, rowgrp=rowgrp, 
	rowgrpnm=rowgrpnm, rowgrpord=rowgrpord, landarea=landarea) 
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
  grpvar <- rowcolinfo$grpvar
  #rm(rowcolinfo)  

  ## Generate a uniquecol for estimation units
  if (!sumunits && colvar == "NONE") {
    uniquecol <- data.table(dunitarea[[dunitvar]])
    setnames(uniquecol, dunitvar)
    uniquecol[[dunitvar]] <- factor(uniquecol[[dunitvar]])
  }


  ## Merge filtered condition data (condf) to all conditions (condx)
  ###################################################################################
  setkeyv(condx, c(cuniqueid, condid))
  setkeyv(condf, c(cuniqueid, condid))

  cdomdat <- condx[condf]


  #####################################################################################
  ### GET TITLES FOR OUTPUT TABLES
  #####################################################################################
  if (is.null(title.dunitvar)) title.dunitvar <- smallbnd.att
  alltitlelst <- check.titles(dat=cdomdat, esttype=esttype, sumunits=sumunits, 
 	title.main=title.main, title.ref=title.ref, title.rowvar=title.rowvar,
 	title.colvar=title.colvar, title.unitvar=title.dunitvar,
	title.filter=title.filter, title.unitsn=areaunits, unitvar=dunitvar, rowvar=rowvar, 
	colvar=colvar, addtitle=addtitle, returntitle=returntitle, rawdata=rawdata, 
	states=states, invyrs=invyrs, landarea=landarea, pcfilter=pcfilter, allin1=allin1, 
	parameters=FALSE, divideby=divideby)
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
  unit.totest=unit.tdomest=unit.domest=unit.rowest=unit.colest=unit.grpest=
	rowunit=totunit <- NULL
  response <- estvar.name
  

  ############################################################################
  ## Generate models
  ############################################################################

  # set up formula with user-defined response and predictors
  fmla <- as.formula(paste(response," ~ ", paste(prednames, collapse= "+")))

  domain <- rowvar
  if (rowvar == "TOTAL") {
    cdomdat$TOTAL <- 1
  }
  estkey <- "DOMAIN"

#  if (addtotal) {
    ## Get total estimate and merge area
    cdomdat$TOTAL <- 1
    domain <- "TOTAL"

    ## Sum response by dunitvar (DOMAIN), plot, domain
    cdomdattot <- cdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(dunitvar, cuniqueid, "TOTAL", prednames), .SDcols=response]

  if (sum(cdomdattot[[response]]) == 0) {
    return(NULL)
  }

  if (is.null(largebnd.att)) {
    estkey <- "DOMAIN"

    ## get unique domains
    doms <- as.character(unique(cdomdattot[[domain]]))

    ## get estimate by domain (SAmethod="unit")
    dunit.multest.unit <- do.call(rbind, lapply(doms, SAest.dom, 
			plt.dom=cdomdattot, cuniqueid=cuniqueid, 
     			dunitlut=dunitlut, dunitvar=dunitvar, 
			SApackage=SApackage, SAmethod="unit", 
			esttype=esttype, prednames=prednames, fmla=fmla,
			domain=domain, response=response)) 

    ## get estimate by domain (SAmethod="area")
    dunit.multest.area <- do.call(rbind, lapply(doms, SAest.dom, 
			plt.dom=cdomdattot, cuniqueid=cuniqueid, 
     			dunitlut=dunitlut, dunitvar=dunitvar, 
			SApackage=SApackage, SAmethod="area", 
			esttype=esttype, prednames=prednames, fmla=fmla,
			domain=domain, response=response)) 

  } else {
    estkey <- c(largebnd.att, "DOMAIN")

    ## get unique largebnd values
    largebnd.vals <- sort(unique(cdomdattot[[largebnd.att]]))
    largebnd.vals <- largebnd.vals[table(cdomdattot[[largebnd.att]]) > 30]

    ## get estimate by domain, by largebnd value
    dunit.multest.unit <- do.call(rbind, lapply(largebnd.vals, SAest.large, 
			plt.dom=cdomdattot, cuniqueid=cuniqueid, 
			largebnd.att=largebnd.att, dunitlut=dunitlut, dunitvar=dunitvar,
			SApackage=SApackage, SAmethod="unit", esttype=esttype, 
			prednames=prednames, fmla=fmla, domain=domain,
			response=response))

    ## get estimate by domain, by largebnd value
    dunit.multest.area <- do.call(rbind, lapply(largebnd.vals, SAest.large, 
			plt.dom=cdomdattot, cuniqueid=cuniqueid, 
			largebnd.att=largebnd.att, dunitlut=dunitlut, dunitvar=dunitvar,
			SApackage=SApackage, SAmethod="area", esttype=esttype, 
			prednames=prednames, fmla=fmla, domain=domain,
			response=response))

  }

  SAunit.vars <- c("JU.Synth", "JU.GREG", "JU.GREG.se", "JU.EBLUP", "JU.EBLUP.se.1")
  SAarea.vars <- c("JFH", "JFH.se", "JA.synth", "JA.synth.se")

  if (SAmethod == "unit") {
    nhat <- "JU.EBLUP"
    nhat.se <- "JU.EBLUP.se.1"
    nhat.var <- "JU.EBLUP.var"
  } else {
    nhat <- "JFH"
    nhat.se <- "JFH.se"
    nhat.var <- "JFH.EBLUP.var"
  }


  dunit.multest <- merge(
		dunit.multest.unit[, c(domain, "AOI", dunitvar, "NBRPLT", "NBRPLT.gt0", 
			"DIR", "DIR.se", SAunit.vars), with=FALSE],
 		dunit.multest.area[, c(domain, dunitvar, SAarea.vars), with=FALSE],
		by=c(domain, dunitvar), all.x=TRUE)
  setkeyv(dunit.multest, c(domain, estkey))

  if (multest.AOIonly) 
    ## Subset dunit.multest to estimation output
    est <- dunit.multest[AOI==1, c(dunitvar, nhat, nhat.se), with=FALSE]

  if (!is.null(smallbnd.att) && addSAdomsdf) {
    est <- merge(SAdomsdf[, c(dunitvar, smallbnd.att), with=FALSE], est, by=dunitvar)
  }
  est[, (nhat.var) := get(nhat.se)^2]
  setkeyv(est, dunitvar)

  ## Remove TOTAL column from est
  if (domain == "TOTAL" && "TOTAL" %in% names(est))
    est$TOTAL <- NULL


  ## Merge dunitarea
  tabs <- FIESTA::check.matchclass(dunitarea, est, dunitvar)
  dunitarea <- tabs$tab1
  est <- setDT(tabs$tab2)

  est <- est[dunitarea, nomatch=0]
  est <- getarea(est, areavar=areavar, esttype=esttype,
				nhatcol=nhat, nhatcol.var=nhat.var)


  if (multest) {
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

    if (!"data.table" %in% class(dunit.multest)) 
      dunit.multest <- setDT(dunit.multest)

    ## Subset dunit.multest, where AOI = 1
    #dunit.multest <- dunit.multest[dunit.multest$AOI == 1, ]

    ## Save multest table
    if (savedata) {

      ## Remove TOTAL column from est
      if (domain == "TOTAL" && "TOTAL" %in% names(dunit.multest))
        dunit.multest$TOTAL <- NULL

      ## Remove column headings if appending to csv file
      if (multest.append && multest_fmt == "csv") {
        dunit.multest <- setDF(dunit.multest)
        colnames(dunit.multest) <- NULL
      }

      ## Export dunit.multest
      datExportData(dunit.multest, out_fmt=multest_fmt, outfolder=outfolder, 
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
	unitvar=dunitvar, unit.totest=unit.totest, unit.rowest=unit.rowest, 
	unit.colest=unit.colest, unit.grpest=unit.grpest, rowvar=rowvar, colvar=colvar, 
	uniquerow=uniquerow, uniquecol=uniquecol, rowgrp=rowgrp, rowgrpnm=rowgrpnm, 
	rowunit=rowunit, totunit=totunit, allin1=allin1, savedata=savedata, 
	addtitle=addtitle, title.ref=title.ref, title.colvar=title.colvar, 
	title.rowvar=title.rowvar, title.rowgrp=title.rowgrp, title.unitvar=title.dunitvar,
 	title.estpse=title.estpse, title.est=title.est, title.pse=title.pse, 
	rawdata=rawdata, rawonly=rawonly, outfn.estpse=outfn.estpse, outfolder=outfolder, 
	outfn.date=outfn.date, overwrite=overwrite_layer, estnm=estnm, estround=estround, 
	pseround=pseround, divideby=divideby, rawdat=rawdat, returntitle=returntitle,
	estnull=estnull, psenull=psenull) 
  est2return <- tabs$tabest
  pse2return <- tabs$tabpse

  if (rawdata) {
    rawdat <- tabs$rawdat
    names(rawdat)[names(rawdat) == "unit.totest"] <- "dunit.totest"
    rawdat$domdat <- setDF(cdomdat)
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
			addformat=FALSE, outfn.date=outfn.date, overwrite=overwrite_layer))
        }
      }
    }
  }


  returnlst <- list(est=est2return)
  if (multest) {
    returnlst$dunit.multest <- dunit.multest 
  }
  if (rawdata) {
    rawdat$esttype <- "AREA"
    rawdat$SApackage <- SApackage
    rawdat$SAmethod <- SAmethod
    if (!is.null(rowvar)) rawdat$rowvar <- rowvar
    if (!is.null(colvar)) rawdat$colvar <- colvar
    rawdat$areaunits <- areaunits
    returnlst$raw <- rawdat
  }
  if (returntitle) {
    returnlst$titlelst <- alltitlelst
  }
  if (returnSApopdat) {
    returnlst$SApopdat <- SApopdat
  }

  return(returnlst)
}


