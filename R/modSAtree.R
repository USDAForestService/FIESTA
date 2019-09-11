modSAtree <- function(tree, cond=NULL, pltmodel=NULL, tuniqueid="PLT_CN", 
	cuniqueid="PLT_CN", puniqueid="CN", sumunits=FALSE, adjplot=TRUE, 
	SApackage="JoSAE", SAmethod="unit", landarea="ALL", ACI=FALSE, 
	nonsamp.filter=NULL, plt.filter=NULL, cond.filter=NULL, dunitvar="DOMAIN",
 	dunitarea=NULL, areavar=NULL, dunitlut=NULL, prednames=NULL, predfac=NULL, 	
	largebnd.att=NULL, estvar=NULL, estvar.filter=NULL, rowvar=NULL, rowvar.filter=NULL, 
	colvar=NULL, colvar.filter=NULL, row.FIAname=FALSE, col.FIAname=FALSE, 	
	row.orderby=NULL, col.orderby=NULL, row.add0=FALSE, col.add0=FALSE, 
	rowlut=NULL, collut=NULL, rowgrp=FALSE, rowgrpnm=NULL, rowgrpord=NULL, 
	allin1=FALSE, estround=0, pseround=3, divideby=NULL, savedata=FALSE, 
	rawdata=FALSE, outfolder=NULL, outfn=NULL, outfn.pre=NULL, outfn.date=TRUE,
	overwrite=FALSE, addtitle=TRUE, returntitle=FALSE, title.main=NULL, 
	title.ref=NULL, title.rowvar=NULL, title.colvar=NULL, title.dunitvar=NULL, 
	title.estvar=NULL, title.filter=NULL, gui=FALSE){

  ########################################################################################
  ## DESCRIPTION: 
  ## Generates model-assisted estimates by domain (and estimation unit)
  ######################################################################################


  ##################################################################
  ## INITIALIZE SETTINGS
  ##################################################################
 
  ## CHECK GUI - IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  if (nargs() == 0 | is.null(cond)) gui <- TRUE 

  ## If gui.. set variables to NULL
  if (gui) { 
    tree=landarea <- NULL
    if (!row.FIAname) row.FIAname <- NULL
    if (!col.FIAname) col.FIAname <- NULL
  }

  ## Set global variables
  ONEUNIT=n.total=n.strata=strwt=TOTAL=char.width=ECOSECTION=ECOSUBSECTION=ECOPROVINCE <- NULL


  ## SET OPTIONS
  options.old <- options()
  options(scipen=8) # bias against scientific notation
  on.exit(options(options.old), add=TRUE) 
  condid <- "CONDID"
  esttype <- "TREE"
  minplotnum <- 10
  title.rowgrp=NULL
  bycond=TRUE
   
 
  ###################################################################################
  ## CHECK PARAMETERS AND DATA
  ## Generates table of sampled/nonsampled conditions
  ## Remove nonsampled conditions (e.g., COND_STATUS_CD = 5)
  ###################################################################################
  datcheck <- check.data(gui=gui, esttype=esttype, module="SA", method=SAmethod, 
	SApackage=SApackage, tree=tree, cond=cond, plt=pltmodel, tuniqueid=tuniqueid,
 	cuniqueid=cuniqueid, condid=condid, puniqueid=puniqueid, unitvar=dunitvar, 
	prednames=prednames, predfac=predfac, sumunits=sumunits, adjplot=adjplot, 
	landarea=landarea, ACI=ACI, nonsamp.filter=nonsamp.filter, plt.filter=plt.filter,
 	cond.filter=cond.filter, allin1=allin1, estround=estround, pseround=pseround,
 	divideby=divideby, savedata=savedata, addtitle=addtitle, returntitle=returntitle,
 	rawdata=rawdata, outfolder=outfolder)
  treef <- datcheck$treef
  tuniqueid <- datcheck$tuniqueid
  condx <- datcheck$condx
  condf <- datcheck$condf
  pltmodelx <- datcheck$pltx
  cuniqueid <- datcheck$cuniqueid
  condid <- datcheck$condid
  puniqueid <- datcheck$puniqueid
  dunitvar <- datcheck$unitvar
  prednames <- datcheck$prednames
  predfac <- datcheck$predfac
  adj <- datcheck$adj
  plotsampcnt <- datcheck$plotsampcnt
  condsampcnt <- datcheck$condsampcnt
  invyrs <- datcheck$invyrs
  allin1 <- datcheck$allin1
  savedata <- datcheck$savedata
  addtitle <- datcheck$addtitle
  estround <- datcheck$estround
  pseround <- datcheck$pseround
  divideby <- datcheck$divideby
  returntitle <- datcheck$returntitle
  add0 <- datcheck$add0
  rawdata <- datcheck$rawdata
  outfolder <- datcheck$outfolder
  SApackage <- datcheck$SApackage
  SAmethod <- datcheck$method
  rm(datcheck)

  ## Check for JoSAE library
  if (SApackage == "JoSAE") {
    if (!"JoSAE" %in% rownames(installed.packages()))
	 stop("SApackage JoSAE requires package JoSAE.")
  } else {
    if (!"sae" %in% rownames(installed.packages()))
	 stop("SApackage sae requires package sae.")
  }

  ###################################################################################
  ## CHECK domarea BY ESTIMATION UNIT
  ## Returns: data table with unitvar and acres by estimation unit (unitvar)
  ##	 and areavar (default="ACRES")
  ###################################################################################
  dunitdat <- FIESTA::check.unitarea(unitarea=dunitarea, pltx=pltmodelx, 
		unitvars=dunitvar, areavar=areavar, gui=gui)
  dunitarea <- dunitdat$unitarea
  areavar <- dunitdat$areavar


  ###################################################################################
  ## Check auxiliary data
  ##  (including partially sampled plots - COND_STATUS_CD=5) 
  ###################################################################################
  ## If < 2 plots, an error occurs, must collapse plots.
  ## If 2-10 plots, a warning is displayed, suggesting to collapse plots. 
  ## Returns:
  ## - unitlut including total number of plots by estimation unit (n.total)
  ###################################################################################
  domcheck <- check.auxiliary(pltmodelx, puniqueid, auxlut=dunitlut, 
		prednames=prednames, predfac=predfac, unitvars=dunitvar, checkcnt=FALSE)
  pltmodelx <- domcheck$pltmodelx
  dunitlut <- domcheck$auxlut


  ###################################################################################
  ## Calculate adjustment factors by estimation unit for area and trees to
  ##		account for nonsampled conditions.
  ## Returns:
  ##  1. Summed proportions (*PROP_UNADJ_SUM) and adjustment factors (*PROP_ADJFAC)  
  ##     by unitvar(s) (*PROP_UNADJ_SUM / n.total)
  ##  2. Adjusted condition proportion (CONDPROP_ADJ) appended to condx
  ###################################################################################
  if (adjplot) {
    adjfacdata <- getadjfactorPLOT(esttype=esttype, treex=treef, condx=condx, 
		tuniqueid=tuniqueid, cuniqueid=cuniqueid)
    condx <- adjfacdata$condadj
    treef <- adjfacdata$treeadj
  }
#condx[condx$PLT_CN == 40406947010690,]


  ###################################################################################
  ### GET ROW AND COLUMN INFO FROM condf
  ###################################################################################
  if (!sumunits) col.add0 <- TRUE
  rowcolinfo <- FIESTA::check.rowcol(gui=gui, esttype="TREE", treef=treef, condf=condf, 
	cuniqueid=cuniqueid, rowvar=rowvar, rowvar.filter=rowvar.filter, colvar=colvar,
 	colvar.filter=colvar.filter, row.FIAname=row.FIAname, col.FIAname=col.FIAname,
 	row.orderby=row.orderby, col.orderby=col.orderby, title.rowvar=title.rowvar,
 	title.colvar=title.colvar)
  treef <- rowcolinfo$treef
  condf <- rowcolinfo$condf
  uniquerow <- rowcolinfo$uniquerow
  uniquecol <- rowcolinfo$uniquecol
  domainlst <- rowcolinfo$domainlst
  rowvar <- rowcolinfo$rowvar
  colvar <- rowcolinfo$colvar
  row.orderby <- rowcolinfo$row.orderby
  col.orderby <- rowcolinfo$col.orderby
  title.rowvar <- rowcolinfo$title.rowvar
  title.colvar <- rowcolinfo$title.colvar
  bytdom <- rowcolinfo$bytdom
  domain <- rowcolinfo$domain
  dom.orderby <- rowcolinfo$dom.orderby
  tdomvar <- rowcolinfo$tdomvar
  rm(rowcolinfo)  

  ## Generate a uniquecol for estimation units
  if (!sumunits && colvar == "NONE") {
    uniquecol <- data.table(dunitarea[[dunitvar]])
    setnames(uniquecol, dunitvar)
    uniquecol[[dunitvar]] <- factor(uniquecol[[dunitvar]])
  }

  ## Merge filtered condition data (condf) to all conditions (condx)
  #####################################################################################
  condall <- condx[condf]


  #####################################################################################
  ### Get estimation data from tree table, with plot-level adjustment for nonresponse
  #####################################################################################
  treedat <- check.tree(gui=gui, treef=treef, bycond=bycond, condall=condall, 
	plt=pltmodelx, bytdom=bytdom, tuniqueid=tuniqueid, cuniqueid=cuniqueid,
	puniqueid=puniqueid, esttype=esttype, estvarn=estvar, 
	estvarn.filter=estvar.filter, esttotn=TRUE, tdomvar=tdomvar, adjtree=adjplot)
  if (is.null(treedat)) return(NULL)
 
  tdomdat <- setDT(treedat$tdomdat)
  estvar <- treedat$estvar
  estvar.name <- treedat$estvar.name
  estvar.filter <- treedat$estvar.filter
  tdomvarlst <- treedat$tdomvarlst

  if (bytdom && rowvar %in% names(treef) && colvar %in% names(treef)) {
    row.tdomvar <- strsplit(tdomvar, "#")[[1]][1]
    row.treedat <- FIESTA::check.tree(gui=gui, treef=treef, condall=condall, 
		bytdom=bytdom, tuniqueid=tuniqueid, cuniqueid=cuniqueid, esttype=esttype, 
		estvarn=estvar, estvarn.filter=estvar.filter, esttotn=FALSE, 
		tdomvar=row.tdomvar, adjsamp=adjplot)
    row.tdomdat <- setDT(row.treedat$tdomdat)
    row.tdomvarlst <- row.treedat$tdomvarlst

    col.tdomvar <- strsplit(tdomvar, "#")[[1]][2]
    col.treedat <- FIESTA::check.tree(gui=gui, treef=treef, condall=condall, 
		bytdom=bytdom, tuniqueid=tuniqueid, cuniqueid=cuniqueid, esttype=esttype, 
		estvarn=estvar, estvarn.filter=estvar.filter, esttotn=FALSE, 
		tdomvar=col.tdomvar, adjsamp=adjplot)
    col.tdomdat <- setDT(col.treedat$tdomdat)
    col.tdomvarlst <- col.treedat$tdomvarlst
  }

  #####################################################################################
  ### GET TITLES FOR OUTPUT TABLES
  #####################################################################################
  alltitlelst <- FIESTA::check.titles(dat=condall, esttype=esttype, sumunits=sumunits, 
 	title.main=title.main, title.ref=title.ref, title.rowvar=title.rowvar,
 	title.rowgrp=title.rowgrp, title.colvar=title.colvar, title.unitvar=title.dunitvar,
	title.filter=title.filter, title.estvarn=title.estvar, unitvar=dunitvar, 
	rowvar=rowvar, colvar=colvar, estvarn=estvar, estvarn.filter=estvar.filter, 
	addtitle=addtitle, returntitle=returntitle, rawdata=rawdata, invyrs=invyrs, 
	landarea=landarea, plt.filter=plt.filter, cond.filter=cond.filter, 
	allin1=allin1, divideby=divideby, outfn=outfn, outfn.pre=outfn.pre)
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
  domindi <- "AOI"
  #setnames(tdomdat, dunitvar, "DOMAIN")


  ###################################################################################
  ## GENERATE OUTPUT TABLES
  ###################################################################################
  result <- list()
  if (rawdata) {
    rawdat <- list()
    if (!is.null(plotsampcnt)) rawdat$plotsampcnt <- plotsampcnt
    if (!is.null(condsampcnt)) rawdat$condsampcnt <- setDF(condsampcnt)
    rawdat$dunitlut <- dunitlut
    rawdat$dunitarea <- dunitarea
    rawdat$pltmodel <- pltmodelx
    rawdat$dat <- tdomdat
    rawdat$estvar <- response
  }


  ###################################################################################
  ## Add ECOSECTION
  ###################################################################################
  if (!"ECOSECTION" %in% names(pltmodelx) && 
 		all(c("ECOSUBSECTION", "ECOPROVINCE") %in% names(pltmodelx))) {
    pltmodelx[, ECOSECTION := substr(ECOSUBSECTION, 1, nchar(ECOPROVINCE)+1)] 
  }
  
    
  ###################################################################################
  ## Create dummy variables
  ###################################################################################
  for (fac in predfac) {
    ## Set as factor if not already
    if (!is.factor(pltmodelx[[fac]])) {
      fac.levels <- sort(unique(pltmodelx[[fac]]))
      pltmodelx[[fac]] <- factor(pltmodelx[[fac]], levels=fac.levels)
    }

    ## Get factor levels
    fac.levels <- sort(unique(pltmodelx[[fac]]))
    fac.names <- paste(fac, fac.levels, sep=".")

    ## Create dummy variables for all factor levels
    dtfac <- pltmodelx[, as.data.table(model.matrix(~.-1, 	
		data=pltmodelx[, fac, with=FALSE]))]
    setnames(dtfac, fac.names)
    pltmodelx <- cbind(pltmodelx, dtfac)
    pltmodelx[, (fac) := NULL]
    prednames <- unique(c(prednames[prednames != fac], fac.names))
  }


  ###################################################################################
  ## Generate models
  ###################################################################################

  # set up formula with user-defined response and predictors
  fmla <- as.formula(paste(response," ~ ", paste(prednames, collapse= "+")))

  if (is.null(domain)) domain <- rowvar
  if (rowvar == "TOTAL") tdomdat$TOTAL <- 1
  estkey <- "DOMAIN"
  
  if (SAmethod == "unit") {
    if (is.null(largebnd.att)) {

      ## get unique domains
      doms <- as.character(unique(tdomdat[[domain]]))

      ## get estimate by domain
      est <- do.call(rbind, lapply(doms, SAest.dom, 
			dat=tdomdat, cuniqueid=cuniqueid, 
			pltmodelx=pltmodelx, puniqueid=puniqueid,
     			dunitlut=dunitlut,
			esttype=esttype, bytdom=bytdom,
			SApackage=SApackage, SAmethod=SAmethod, 
			prednames=prednames, fmla=fmla,
			domain=domain, 
			response=response, tdomvarlst=tdomvarlst))
 
    } else {
      estkey <- c(largebnd.att, "DOMAIN")

      ## get unique largebnd values
      largebnd.vals <- sort(unique(pltmodelx[[largebnd.att]]))
      largebnd.vals <- largebnd.vals[table(pltmodelx[[largebnd.att]]) > 30]

      ## get estimate by domain, by largebnd value
      est <- do.call(rbind, lapply(largebnd.vals, SAest.large, 
			dat=tdomdat, cuniqueid=cuniqueid, 
			pltmodelx=pltmodelx, puniqueid=puniqueid, 
			largebnd.att=largebnd.att, dunitlut=dunitlut,
			esttype=esttype, bytdom=bytdom, 
			SApackage=SApackage, SAmethod=SAmethod, 
			prednames=prednames, fmla=fmla, 
 			domain=domain, 
			response=response, tdomvarlst=tdomvarlst))
    }
  } else if (SAmethod == "area") {

    ## get unique domains
    doms <- as.character(unique(tdomdat[[domain]]))

    ## get estimate by domain
    est <- do.call(rbind, lapply(doms, SAest.dom, 
			dat=tdomdat, cuniqueid=cuniqueid, 
			pltmodelx=pltmodelx, puniqueid=puniqueid,
     			dunitlut=dunitlut,
			esttype=esttype, bytdom=bytdom,
			SApackage=SApackage, SAmethod=SAmethod, 
			prednames=prednames, fmla=fmla,
			domain=domain, 
			response=response, tdomvarlst=tdomvarlst)) 
  }
  setkeyv(est, estkey)
  result$est <- est

  if (savedata) {
     suppressWarnings(save1tab(estpse=rawtab, title.estpse=title.raw, 
		outfolder=rawfolder, allin1=allin1, coltitlerow=FALSE, 
		rowtotal=FALSE, outfn.estpse=outfn.rawtab, addtitle=FALSE,
		addformat=FALSE, outfn.date=outfn.date, overwrite=overwrite))
  }

  if (savedata && rawdata) {
    rawfolder <- paste(outfolder, "rawdata", sep="/")
    if (!file.exists(rawfolder)) dir.create(rawfolder)

    if (!is.null(title.estpse)) {
      title.raw <- paste(title.estpse, title.ref)
    } else {
      title.raw <- title.est
    }

    for (i in 1:length(rawdat)) {
      tabnm <- names(rawdat[i])
      if (tabnm != estvar && !grepl("lme", tabnm)) {
        rawtab <- rawdat[[i]]
        outfn.rawtab <- paste(outfn.rawdat, tabnm, sep="_") 
        if (tabnm %in% c("plotsampcnt", "condsampcnt", "domzonal", "domarea")) {
          write2csv(rawtab, outfolder=rawfolder, outfilenm=outfn.rawtab, 
			outfn.date=outfn.date, overwrite=overwrite)
        } else  { 
          suppressWarnings(save1tab(estpse=rawtab, title.estpse=title.raw, 
			outfolder=rawfolder, allin1=allin1, coltitlerow=FALSE, 
			rowtotal=FALSE, outfn.estpse=outfn.rawtab, addtitle=FALSE,
			addformat=FALSE, outfn.date=outfn.date, overwrite=overwrite))
        }
      }
    }
  }
  if (rawdata) result$raw <- rawdat
  return(result)
}

	

