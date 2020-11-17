modSAtree <- function(SAdomsdf=NULL, tree=NULL, cond=NULL, plt=NULL, 
	pltassgn=NULL, seed=NULL, dsn=NULL, tuniqueid="PLT_CN", cuniqueid="PLT_CN", 
	condid="CONDID", puniqueid="CN", pltassgnid="CN", measCur=FALSE, measEndyr=NULL, 
	invyrs=NULL, ACI=FALSE, adj="plot", plt.nonsamp.filter=NULL, cond.nonsamp.filter=NULL, 
	dunitvar="DOMAIN", dunitvar2=NULL, dunitarea=NULL, areavar=NULL, dunitlut=NULL, 
	prednames=NULL, predfac=NULL, SApackage="JoSAE", SAmethod="unit", 
	largebnd.att=NULL, landarea="ALL", plt.filter=NULL, cond.filter=NULL, 
	estvar=NULL, estvar.filter=NULL, smallbnd.att=NULL, allin1=FALSE, estround=0, 
	pseround=3, estnull=0, psenull="--", divideby=NULL, savedata=FALSE, rawdata=FALSE, 
	multest=TRUE, addSAdomsdf=TRUE, SAdomvars=NULL, outfolder=NULL, outfn.pre=NULL, 
	raw_fmt="csv", raw_dsn="rawdata", multest_fmt="csv", multest_outfolder=NULL, 
	multest_dsn=NULL, multest_layer=NULL, multest.append=FALSE, multest.AOIonly=FALSE, 
	outfn.date=FALSE, overwrite=FALSE, addtitle=TRUE, returntitle=FALSE, 
	title.main=NULL, title.ref=NULL, title.dunitvar=NULL, title.estvar=NULL, 
	title.filter=NULL, SApopdat=NULL, SAdata=NULL){


  ######################################################################################
  ## DESCRIPTION: 
  ## Generates model-assisted estimates by domain (and estimation unit)
  ## if saveraw...  and raw_fmt = 'csv', a new folder is created within the outfolder
  ##			named as raw_dsn. If raw_fmt != 'csv', a database is created
  ##			within the outfolder names as raw_dsn. 
  ######################################################################################

  ## CHECK GUI - IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  if (nargs() == 0 || is.null(tree) && is.null(SApopdat) && is.null(SAdata)) gui <- TRUE 

  ## Set global variables
  ONEUNIT=n.total=n.strata=strwt=TOTAL=AOI=rowvar.filter=colvar.filter=
	title.rowvar=title.colvar=TOTAL <- NULL
  gui <- FALSE

  ##################################################################
  ## INITIALIZE SETTINGS
  ##################################################################
 
  ## If gui.. set variables to NULL
  if (gui) { 
    tree=landarea <- NULL
    if (!row.FIAname) row.FIAname <- NULL
    if (!col.FIAname) col.FIAname <- NULL
  }

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
  esttype <- "TREE"
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


  ### Check savedata 
  savedata <- FIESTA::pcheck.logical(savedata, varnm="savedata", 
		title="Save data tables?", first="YES", gui=gui, stopifnull=TRUE)

  ## Check outfolder 
  ########################################################
  if (savedata) {
    fmtlst <- c("sqlite", "gpkg", "csv", "gdb")

    outfolder <- FIESTA::pcheck.outfolder(outfolder, gui)

    overwrite <- FIESTA::pcheck.logical(overwrite, varnm="overwrite", 
		title="Overwrite?", first="NO", gui=gui)  
    outfn.date <- FIESTA::pcheck.logical(outfn.date, varnm="outfn.date", 
		title="Add date to outfiles?", first="NO", gui=gui) 

    if (rawdata) {
      raw_fmt <- FIESTA::pcheck.varchar(var2check=raw_fmt, varnm="raw_fmt", 
		checklst=fmtlst, gui=gui, caption="Output rawdata format?") 
      if (is.null(raw_dsn)) raw_dsn <- "rawdata"
      if (raw_fmt == "csv") {
        rawfolder <- file.path(outfolder, raw_dsn)
        if (!file.exists(rawfolder)) dir.create(rawfolder)
      } else {
        rawfolder <- outfolder
        raw_dsn <- paste0(raw_dsn, ".", raw_fmt)

        if (raw_fmt == "gdb") {
          raw_dsn <- DBtestESRIgdb(gdbfn=raw_dsn, outfolder=outfolder, overwrite=FALSE, 
			showlist=FALSE, returnpath=FALSE)
        }	else if (raw_fmt %in% c("sqlite", "gpkg")) {
          gpkg <- ifelse(raw_fmt == "gpkg", TRUE, FALSE)
          raw_dsn <- DBcreateSQLite(SQLitefn=raw_dsn, gpkg=gpkg, outfolder=outfolder, 
			overwrite=FALSE, returnpath=FALSE, outfn.date=outfn.date)
        }	
      }
    }

    multest <- FIESTA::pcheck.logical(multest, varnm="multest", 
		title="Unit and area estimates?", first="NO", gui=gui) 
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
			overwrite=overwrite, showlist=FALSE, returnpath=FALSE)
        }	else if (multest_fmt %in% c("sqlite", "gpkg")) {
          gpkg <- ifelse(multest_fmt == "gpkg", TRUE, FALSE)
          if (multest.append || !overwrite) {
            multest_dsn <- DBtestSQLite(SQLitefn=multest_dsn, gpkg=gpkg, outfolder=outfolder, 
			showlist=FALSE, returnpath=FALSE, createnew=TRUE)
          } else {
            multest_dsn <- DBcreateSQLite(SQLitefn=multest_dsn, gpkg=gpkg, outfolder=outfolder, 
			overwrite=overwrite, returnpath=FALSE, outfn.date=outfn.date)
          }
        }	
      }
    }
  }

  ## Check SA packages and method
  SApackagelst <- c("JoSAE", "sae")
  SApackage <- FIESTA::pcheck.varchar(var2check=SApackage, varnm="SApackage", gui=gui, 
		checklst=SApackagelst, caption="SA package", multiple=FALSE, stopifnull=TRUE)

  ## Check for JoSAE library
  if (SApackage == "JoSAE") {
    if (!"JoSAE" %in% rownames(installed.packages()))
	 stop("SApackage JoSAE requires package JoSAE.")
  } else {
    if (!"sae" %in% rownames(installed.packages()))
	 stop("SApackage sae requires package sae.")
  }
  methodlst <- c("unit", "area")
  SAmethod <- FIESTA::pcheck.varchar(var2check=SAmethod, varnm="SAmethod", gui=gui, 
		checklst=methodlst, caption="SAmethod", multiple=FALSE, stopifnull=TRUE)



  ###################################################################################
  ## Check data and generate population information 
  ###################################################################################
  if (is.null(SApopdat)) {
    SApopdat <- modSApop(tree=tree, cond=cond, plt=plt, dsn=dsn, pltassgn=pltassgn,
 		tuniqueid=tuniqueid, cuniqueid=cuniqueid, condid=condid, 
		puniqueid=puniqueid, pltassgnid=pltassgnid, measCur=FALSE, measEndyr=NULL,
 		invyrs=NULL, ACI=ACI, adj=adj, plt.nonsamp.filter=plt.nonsamp.filter, 
		cond.nonsamp.filter=cond.nonsamp.filter, dunitvar=dunitvar, dunitvar2=dunitvar2,
		dunitarea=dunitarea, areavar=areavar, unitcombine=unitcombine, dunitlut=dunitlut,
 		prednames=prednames, predfac=predfac, pvars2keep=pvars2keep, SAdata=SAdata,
		gui=gui)
  } else {
    returnSApopdat <- FALSE
    if (!is.list(SApopdat))
      stop("SApopdat must be a list")
    listitems <- c("condx", "pltcondx", "treex", "cuniqueid", "condid", 
		"tuniqueid", "ACI.filter", "dunitarea", "dunitvar", "dunitlut",
		"prednames", "plotsampcnt", "condsampcnt")
    if (!all(listitems %in% names(SApopdat))) {
      items.miss <- listitems[!listitems %in% names(SApopdat)]
      stop("invalid SApopdat... missing items: ", paste(items.miss, collapse=", "))
    }   
  }		
  if (is.null(SApopdat)) return(NULL)
  SAdomsdf <- SApopdat$SAdomsdf
  condx <- SApopdat$condx
  pltcondx <- SApopdat$pltcondx
  treex <- SApopdat$treex
  cuniqueid <- SApopdat$cuniqueid
  condid <- SApopdat$condid
  tuniqueid <- SApopdat$tuniqueid
  ACI.filter <- SApopdat$ACI.filter
  dunitarea <- SApopdat$dunitarea
  areavar <- SApopdat$areavar
  dunitvar <- SApopdat$dunitvar
  dunitlut <- SApopdat$dunitlut
  plotsampcnt <- SApopdat$plotsampcnt
  condsampcnt <- SApopdat$condsampcnt
  states <- SApopdat$states
  invyrs <- SApopdat$invyrs


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
    if (!all(prednames %in% SApopdat$prednames))
      stop("invalid prednames... must be in: ", toString(SApopdat$prednames))
  }


  ###################################################################################
  ## Check parameters and apply plot and condition filters
  ###################################################################################
  estdat <- check.estdata(esttype=esttype, pltcondf=pltcondx, cuniqueid=cuniqueid,
 		condid=condid, treex=treex, sumunits=sumunits, landarea=landarea,
 		ACI.filter=ACI.filter, plt.filter=plt.filter, cond.filter=cond.filter, 
		allin1=allin1, estround=estround, pseround=pseround, divideby=divideby,
 		addtitle=addtitle, returntitle=returntitle, rawdata=rawdata, 
		savedata=savedata, outfolder=outfolder)
  if (is.null(estdat)) return(NULL)
  pltcondf <- estdat$pltcondf
  cuniqueid <- estdat$cuniqueid
  treef <- estdat$treef
  tuniqueid <- estdat$tuniqueid
  sumunits <- estdat$sumunits
  allin1 <- estdat$allin1
  estround <- estdat$estround
  pseround <- estdat$pseround
  divideby <- estdat$divideby
  addtitle <- estdat$addtitle
  returntitle <- estdat$returntitle
  rawdata <- estdat$rawdata
  savedata <- estdat$savedata
  outfolder <- estdat$outfolder
  estround <- estdat$estround
  pseround <- estdat$pseround
  landarea <- estdat$landarea
  if (sumunits && nrow(dunitarea) == 1) sumunits <- FALSE 


  ###################################################################################
  ### GET ROW AND COLUMN INFO FROM condf
  ###################################################################################
  if (!sumunits) col.add0 <- TRUE
  rowcolinfo <- check.rowcol(gui=gui, esttype=esttype, treef=treef, 
	condf=pltcondf, cuniqueid=cuniqueid, rowvar=rowvar, rowvar.filter=rowvar.filter, 
	colvar=colvar, colvar.filter=colvar.filter, row.FIAname=row.FIAname, 
	col.FIAname=col.FIAname, row.orderby=row.orderby, col.orderby=col.orderby,
 	row.add0=row.add0, col.add0=col.add0, title.rowvar=title.rowvar, 
	title.colvar=title.colvar, rowlut=rowlut, collut=collut, rowgrp=rowgrp, 
	rowgrpnm=rowgrpnm, rowgrpord=rowgrpord, landarea=landarea) 
  treef <- rowcolinfo$treef
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
  concat <- rowcolinfo$concat
  grpvar <- rowcolinfo$grpvar
  #rm(rowcolinfo)  


  ## Generate a uniquecol for estimation units
  if (!sumunits && colvar == "NONE") {
    uniquecol <- data.table(dunitarea[[dunitvar]])
    setnames(uniquecol, dunitvar)
    uniquecol[[dunitvar]] <- factor(uniquecol[[dunitvar]])
  }


  #####################################################################################
  ### Get estimation data from tree table, with plot-level adjustment for nonresponse
  #####################################################################################
  adjtree <- ifelse(adj %in% c("samp", "plot"), TRUE, FALSE)
  treedat <- check.tree(gui=gui, treef=treef, bycond=TRUE, condf=condf, 
	bytdom=bytdom, tuniqueid=tuniqueid, cuniqueid=cuniqueid, 
	puniqueid=puniqueid, esttype=esttype, estvarn=estvar, 
	estvarn.filter=estvar.filter, esttotn=TRUE, tdomvar=tdomvar, adjtree=adjtree)
  if (is.null(treedat)) return(NULL) 
  estvar <- treedat$estvar
  estvar.name <- treedat$estvar.name
  estvar.filter <- treedat$estvar.filter
  tdomvarlst <- treedat$tdomvarlst
  tdomdat <- merge(condx, treedat$tdomdat, by=c(cuniqueid, condid), all.x=TRUE)
  tdomdat <- DT_NAto0(tdomdat, estvar.name, 0)


  ## add separate columns
  if (concat) {
    if (is.null(grpvar)) {
      tdomdatkey <- key(tdomdat)
      setkeyv(tdomdat, c(rowvar, colvar))   
      grpvar <- paste(rowvar, colvar, sep="#")
      tdomdat[, (grpvar) := paste(tdomdat[[rowvar]], tdomdat[[colvar]], sep="#")]
      setkeyv(tdomdat, tdomdatkey)
    } else if (!is.null(tdomvar) && grpvar == tdomvar) { 
      tdomdat[,(rowvar) := sapply(get(grpvar), 
			function(x){strsplit(as.character(x), "#")[[1]][1]})]
      tdomdat[,(colvar) := sapply(get(grpvar), 
			function(x){strsplit(as.character(x), "#")[[1]][2]})]
    }
  } 


  #####################################################################################
  ### GET TITLES FOR OUTPUT TABLES
  #####################################################################################
  if (is.null(title.dunitvar)) title.dunitvar <- smallbnd.att
  alltitlelst <- check.titles(dat=tdomdat, esttype=esttype, sumunits=sumunits, 
 	title.main=title.main, title.ref=title.ref, title.rowvar=title.rowvar,
 	title.rowgrp=title.rowgrp, title.colvar=title.colvar, title.unitvar=title.dunitvar,
	title.filter=title.filter, title.estvarn=title.estvar, unitvar=dunitvar, 
	rowvar=rowvar, colvar=colvar, estvarn=estvar, estvarn.filter=estvar.filter, 
	addtitle=addtitle, returntitle=returntitle, rawdata=rawdata, states=states, 
	invyrs=invyrs, landarea=landarea, plt.filter=plt.filter, cond.filter=cond.filter, 
	allin1=allin1, divideby=divideby, parameters=FALSE, outfn.pre=outfn.pre)
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
  #setnames(tdomdat, dunitvar, "DOMAIN")


  ############################################################################
  ## Generate models
  ############################################################################

  # set up formula with user-defined response and predictors
  fmla <- as.formula(paste(response," ~ ", paste(prednames, collapse= "+")))

  domain <- rowvar
  if (rowvar == "TOTAL") tdomdat$TOTAL <- 1
  estkey <- "DOMAIN"

  ## Get total estimate and merge area
  tdomdat$TOTAL <- 1
  domain <- "TOTAL"

  ## Sum estvar.name by dunitvar (DOMAIN), plot, domain
  tdomdattot <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(dunitvar, cuniqueid, "TOTAL", prednames), .SDcols=estvar.name]

  if (sum(tdomdattot[[response]]) == 0) return(NULL)

  if (is.null(largebnd.att)) {
    estkey <- "DOMAIN"

    ## get unique domains
    doms <- as.character(unique(tdomdattot[[domain]]))

    ## get estimate by domain (SAmethod="unit")
    message("generating unit-level estimates for ", response, "...")
    dunit.multest.unit <- do.call(rbind, lapply(doms, SAest.dom, 
			plt.dom=tdomdattot, cuniqueid=cuniqueid, 
     			dunitlut=dunitlut, dunitvar=dunitvar, 
			SApackage=SApackage, SAmethod="unit", 
			esttype=esttype, prednames=prednames, fmla=fmla,
			domain=domain, response=response))

#save(tdomdattot, file=file.path(outfolder, "tdomdattot.rda"))
#save(dunitlut, file=file.path(outfolder, "dunitlut.rda"))
#save(fmla, file=file.path(outfolder, "fmla.rda"))
    ## get estimate by domain (SAmethod="area")
    message("generating area-level estimates for ", response, "...")
    dunit.multest.area <- do.call(rbind, lapply(doms, SAest.dom, 
			plt.dom=tdomdattot, cuniqueid=cuniqueid, 
     			dunitlut=dunitlut, dunitvar=dunitvar, 
			SApackage=SApackage, SAmethod="area", 
			esttype=esttype, prednames=prednames, fmla=fmla,
			domain=domain, response=response)) 

  } else {
    estkey <- c(largebnd.att, "DOMAIN")

    ## get unique largebnd values
    largebnd.vals <- sort(unique(tdomdattot[[largebnd.att]]))
    largebnd.vals <- largebnd.vals[table(tdomdattot[[largebnd.att]]) > 30]

    ## get estimate by domain, by largebnd value
    message("generating unit-level estimates for ", response, "...")
    dunit.multest.unit <- do.call(rbind, lapply(largebnd.vals, SAest.large, 
			plt.dom=tdomdattot, cuniqueid=cuniqueid, 
			largebnd.att=largebnd.att, dunitlut=dunitlut, dunitvar=dunitvar,
			SApackage=SApackage, SAmethod="unit", esttype=esttype, 
			prednames=prednames, fmla=fmla, domain=domain,
			response=response))

    ## get estimate by domain, by largebnd value
    message("generating area-level estimates for ", response, "...")
    dunit.multest.area <- do.call(rbind, lapply(largebnd.vals, SAest.large, 
			plt.dom=tdomdattot, cuniqueid=cuniqueid, 
			largebnd.att=largebnd.att, dunitlut=dunitlut, dunitvar=dunitvar,
			SApackage=SApackage, SAmethod="area", esttype=esttype, 
			prednames=prednames, fmla=fmla, domain=domain,
			response=response))

  }

  if (ncol(dunit.multest.area) == 1) return(NULL)
  if (ncol(dunit.multest.unit) == 1) return(NULL)

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

  ## Subset dunit.multest to estimation output
  est <- dunit.multest[AOI==1, c(dunitvar, nhat, nhat.se), with=FALSE]


  if (!is.null(smallbnd.att)) 
    est <- merge(SAdomsdf[, unique(c(dunitvar, smallbnd.att)), with=FALSE], est, by=dunitvar)
  est[, (nhat.var) := get(nhat.se)^2]
  setkeyv(est, dunitvar)

  ## Merge dunitarea
  tabs <- FIESTA::check.matchclass(dunitarea, est, dunitvar)
  dunitarea <- tabs$tab1
  est <- tabs$tab2

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

    ## Remove TOTAL column from dunit.multest
    if (domain == "TOTAL" && "TOTAL" %in% names(dunit.multest))
      dunit.multest[, TOTAL := NULL]

    if (multest.AOIonly) 
      ## Subset dunit.multest, where AOI = 1
      dunit.multest <- dunit.multest[dunit.multest$AOI == 1, ]

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

      ## Export dunit.multest
      overwrite_layer <- ifelse(multest.append, FALSE, overwrite)

      datExportData(dunit.multest, out_fmt=multest_fmt, outfolder=multest_outfolder, 
 		out_dsn=multest_dsn, out_layer=multest_layer, overwrite_layer=overwrite_layer, 
		append_layer=multest.append)
    }
  } 


  ###################################################################################
  ## GENERATE OUTPUT TABLES
  ###################################################################################
  if (rawdata) {
    rawdat <- list()
    rawdat$domdat <- setDF(tdomdat)
    rawdat$estvar <- response
    rawdat$estvar.filter <- estvar.filter
  }


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
	rawdata=rawdata, outfn.estpse=outfn.estpse, outfolder=outfolder, 
	outfn.date=outfn.date, overwrite=overwrite, estnm=estnm, estround=estround, 
	pseround=pseround, divideby=divideby, rawdat=rawdat, returntitle=returntitle,
	estnull=estnull, psenull=psenull) 

  est2return <- tabs$tabest
  pse2return <- tabs$tabpse
  rawdat <- tabs$rawdat
  titlelst <- tabs$titlelst
  if (rawdata) 
    names(rawdat)[names(rawdat) == "unit.totest"] <- "dunit.totest"

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
    rawdat$esttype <- "TREE"
    rawdat$SApackage <- SApackage
    rawdat$SAmethod <- SAmethod
    rawdat$estvar <- estvar
    rawdat$estvar.filter <- estvar.filter
    if (!is.null(rowvar)) rawdat$rowvar <- rowvar
    if (!is.null(colvar)) rawdat$colvar <- colvar
    returnlst$raw <- rawdat
  }
  if (returntitle) returnlst$titlelst <- alltitlelst
  if (returnSApopdat) returnlst$SApopdat <- SApopdat

  return(returnlst)
}


