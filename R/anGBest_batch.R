anGBest_batch <- function(GBpopdatlst=NULL, esttype="AREA", estseed="none",
	landarea="FOREST", pcfilter=NULL, estvarlst=NULL, 
	estvar.filterlst=NULL, rowvar=NULL, colvar=NULL, sumunits=TRUE, 
	savedata=FALSE, raw_dsn="estdat", raw_fmt="sqlite", outfolder=NULL,  
	outfn.pre=NULL, outfn.date=FALSE, overwrite_dsn=FALSE, 
	overwrite_layer=TRUE, append_layer=FALSE, title.filterlst=NULL, ...) {
  ## DESCRIPTION: estimates for each evalid in list
  
  ## Set global variables
  tree=seed=seed_layer=unitvar2 <- NULL
  gui <- FALSE
  istree=FALSE
  rawonly <- TRUE
  RS=NULL
  GBpop_evalEndyrlst <- list()
  if (sumunits) {
    exportlst <- c("totest", "rowest", "colest", "grpest")
  } else {
    exportlst <- c("unit_totest", "unit_rowest", "unit_colest", "unit_grpest")
  }

  ## Check esttype 
  ########################################################
  esttypelst <- c("AREA", "TREE", "RATIO")
  esttype <- FIESTA::pcheck.varchar(var2check=esttype, varnm="esttype", 
		checklst=esttypelst, caption="Estimation type", stopifnull=TRUE)
  if (esttype == "AREA") {
    estvarlst <- "AREA"
    estvar.filterlst <- NULL
  }

  ## Check savedata 
  savedata <- FIESTA::pcheck.logical(savedata, varnm="savedata", 
		title="Save data extraction?", first="NO", gui=gui) 

  ## If savedata, check output file names
  ################################################################
  if (savedata) { 
    outlst <- pcheck.output(out_dsn=raw_dsn, out_fmt=raw_fmt, 
		outfolder=outfolder, outfn.date=outfn.date, 
		overwrite_dsn=overwrite_dsn, overwrite_layer=overwrite_layer,
		append_layer=append_layer, createSQLite=FALSE, gui=gui)
    out_dsn <- outlst$out_dsn
    outfolder <- outlst$outfolder
    out_fmt <- outlst$out_fmt
    overwrite_layer <- outlst$overwrite_layer
    overwrite_dsn <- outlst$overwrite_dsn
    append_layer <- outlst$append_layer
  
    if (out_fmt == "csv") {
      rawfolder <- paste(outfolder, "rawdata", sep="/")
      if (!file.exists(rawfolder)) dir.create(rawfolder)
    } else {
      rawfolder <- outfolder
    }
  }

  ## If population data
  ################################################################
  if (is.null(GBpopdatlst)) {
    GBpopdatlst <- anGBpop_eval(...)
    names(GBpopdatlst)
  }


  ## check estvarlst
  ################################################################
  nbrestvar <- length(estvarlst)
  if (!is.null(title.filterlst) && length(title.filterlst) < nbrestvar) {
    title.filterlst <- rep(title.filterlst, nbrestvar)
  }
  #if (!is.null(estvar.filterlst) && length(estvar.filterlst) < nbrestvar) {
  #  estvar.filterlst <- rep(estvar.filterlst, nbrestvar)
  #}
  if (is.null(estvar.filterlst)) {
    estvar.filterlst <- "NONE"
  }


  ####################################################################
  ## Get estimates
  ####################################################################
  GBest <- list()

  states <- names(GBpopdatlst)
  for (st in 1:length(states)) {
    state <- states[st]
    GBpopdat <- GBpopdatlst[[state]]
    evalidst <- GBpopdat$evalid
    stabbr <- pcheck.states(state, statereturn="ABBR")
    layer.pre <- NULL
    if (st > 1) {
      append_layer <- TRUE
    }      

    for (i in 1:length(evalidst)) {
      eval <- evalidst[i]
      outfn.pre <- paste0(stabbr, "_", eval)

      for (j in 1:length(estvarlst)) {
        msg <- paste0("getting estimates for ", state, ", evaluation ", eval, "...")
        estvar <- estvarlst[j]
        estvarnm <- ifelse(estvar == "TPA_UNADJ", "COUNT", 
			ifelse(estvar == "CONDPROP_UNADJ", "AREA", estvar))
        for (k in 1:length(estvar.filterlst)) {
          msg2 <- paste(msg, estvar)
          estvar.filter <- estvar.filterlst[k]
          if (estvar.filter != "NONE") {
            title.filter <- title.filterlst[k]
            if (is.null(title.filter)) {
              title.filter <- gsub(" ", "", estvar.filter)
            }
            msg2 <- paste0(msg2, " - ", title.filter, "...")
            layer.pre <- paste(estvarnm, title.filter, landarea, sep="_")
          } else {
            layer.pre <- paste(estvarnm)
          }
          message(msg2)           

          if (esttype == "AREA") {
            GBestdat <- tryCatch(
		    modGBarea(GBpopdat=GBpopdat, 
			landarea=landarea, pcfilter=pcfilter, 
  			rowvar=rowvar, row.FIAname=TRUE,
  			colvar=colvar, col.FIAname=TRUE,
			savedata=FALSE, returntitle=TRUE, rawdata=TRUE, 
			rawonly=rawonly),
				error=function(err) {
					message(err,"\n")
					return(NULL)
				} )
          } else if (esttype == "TREE") {
            GBestdat <- tryCatch(
		   modGBtree(GBpopdat=GBpopdat, 
			landarea=landarea, pcfilter=pcfilter, 
			estvar=estvar, estvar.filter=estvar.filter,
  			rowvar=rowvar, row.FIAname=TRUE,
  			colvar=colvar, col.FIAname=TRUE,
			savedata=FALSE, returntitle=TRUE, rawdata=TRUE, 
			rawonly=rawonly, title.filter=title.filter),
				error=function(err) {
					message(err,"\n")
					return(NULL)
				} )
          } else if (esttype == "RATIO") {
            GBestdat <- tryCatch(
		   modGBratio(GBpopdat=GBpopdat, 
			landarea=landarea, pcfilter=pcfilter, 
			estvarn=estvar, estvarn.filter=estvar.filter,
  			rowvar=rowvar, row.FIAname=TRUE,
  			colvar=colvar, col.FIAname=TRUE,
			savedata=FALSE, returntitle=TRUE, rawdata=TRUE, 
			rawonly=rawonly, title.filter=title.filter),
				error=function(err) {
					message(err,"\n")
					return(NULL)
				} )
          }
          exportlst <- exportlst[exportlst %in% names(GBestdat$raw)]
          if (length(exportlst) == 0) {
            stop("no tables to export")
          }
          for (tabnm in exportlst) { 
            tab <- data.table(EVALID=eval[[1]], GBestdat$raw[[tabnm]])
            if (savedata) {
              datExportData(tab, outfolder=rawfolder, out_fmt=out_fmt, out_dsn=out_dsn, 
			overwrite_dsn=overwrite_dsn, overwrite_layer=overwrite_layer,
      		append_layer=append_layer, layer.pre=layer.pre, out_layer=tabnm)
            } else {
              GBest[[layer.pre]][[tabnm]] <- 
				rbindlist(list(GBest[[layer.pre]][[tabnm]], tab))              
            }  
          }
        }  ## k loop - estvar.filter
      }  ## j loop - estvar
    } ## i loop
  }
  return(GBest)
}
