modWWWest <- function(WWWpopdat,
                      SApackage = "JoSAE",
                      esttype,
                      landarea,
                      estseed = "none", 
                      ratiotype = "PERACRE",
                      woodland = "Y",
                      estvar = NULL, 
                      estvar.filter = NULL,
                      rowvar = NULL, 
                      colvar = NULL, 
                      savedata = FALSE, 
                      table_opts = NULL, 
                      title_opts = NULL, 
                      outfolder = NULL){
  
  ##################################################################################
  ## DESCRIPTION: 
  ## Generates per-acre or per-tree estimates by domain using ratio estimators
  ##################################################################################
  
  
  ##################################################################
  ## CHECK PARAMETER NAMES
  ##################################################################
  
  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- names(formals(modWWWest))
  if (!all(input.params %in% formallst)) {
    miss <- input.params[!input.params %in% formallst]
    stop("invalid parameter: ", toString(miss))
  }
  
  ## Check parameter lists
  pcheck.params(input.params, table_opts=table_opts, title_opts=title_opts, 
                savedata_opts=savedata_opts)
  
  
  ## Set savedata defaults
  savedata_defaults_list <- formals(savedata_options)[-length(formals(savedata_options))]
  for (i in 1:length(savedata_defaults_list)) {
    assign(names(savedata_defaults_list)[[i]], savedata_defaults_list[[i]])
  }
  
  ## Set user-supplied savedata values
  if (length(savedata_opts) > 0) {
    if (!savedata) {
      message("savedata=FALSE with savedata parameters... no data are saved")
    }
    for (i in 1:length(savedata_opts)) {
      if (names(savedata_opts)[[i]] %in% names(savedata_defaults_list)) {
        assign(names(savedata_opts)[[i]], savedata_opts[[i]])
      } else {
        stop(paste("Invalid parameter: ", names(savedata_opts)[[i]]))
      }
    }
  }
  
  ## Set table defaults
  table_defaults_list <- formals(table_options)[-length(formals(table_options))]
  for (i in 1:length(table_defaults_list)) {
    assign(names(table_defaults_list)[[i]], table_defaults_list[[i]])
  }
  
  ## Set user-supplied table values
  if (length(table_opts) > 0) {
    for (i in 1:length(table_opts)) {
      if (names(table_opts)[[i]] %in% names(table_defaults_list)) {
        assign(names(table_opts)[[i]], table_opts[[i]])
      } else {
        stop(paste("Invalid parameter: ", names(table_opts)[[i]]))
      }
    }
  }
 
  ## Set title defaults
  title_defaults_list <- formals(title_options)[-length(formals(title_options))]
  for (i in 1:length(title_defaults_list)) {
    assign(names(title_defaults_list)[[i]], title_defaults_list[[i]])
  }
  
  ## Set user-supplied title values
  if (length(title_opts) > 0) {
    for (i in 1:length(title_opts)) {
      if (names(title_opts)[[i]] %in% names(title_defaults_list)) {
        assign(names(title_opts)[[i]], title_opts[[i]])
      } else {
        stop(paste("Invalid parameter: ", names(title_opts)[[i]]))
      }
    }
  }
  
  
  ## Set global variables
  tdomvar=row.classify=domclassify=cdomdat=tdomdat=uniquerow=uniquecol=
    colvarnm=title.colvar <- NULL
  title.ref <- NULL
  
  
  ## Define default parameters
  ################################################################
  estround <- 2
  pseround <- 2
  rawdata <- TRUE  
  rawonly <- TRUE
  sumunits <- FALSE
  totals <- TRUE
  returnlst <- list()
  largebnd.unique <- "ecomap_province"
  
  
  ##################################################################
  ## Get population data from WWWpopdat
  ##################################################################
  SAE <- WWWpopdat$SAE
  pltassgnx <- WWWpopdat$pltassgnx
  unitarea <- WWWpopdat$unitarea
  unitlut <- WWWpopdat$unitlut
  areavar <- WWWpopdat$areavar
  prednames <- WWWpopdat$prednames
  predfac <- WWWpopdat$predfac
  
  if (!SAE) {
    pltassgnxGB <- WWWpopdat$pltassgnxGB
    stratalut <- WWWpopdat$stratalut
    strata <- WWWpopdat$strata
    strvar <- WWWpopdat$strvar
    strunitvars <- c(unitvar, strvar)
    npixels <- WWWpopdat$npixels
  }
  states <- WWWpopdat$states
  invyrs <- WWWpopdat$invyrs
  popconn <- WWWpopdat$popconn
  
  dbqueriesWITH <- WWWpopdat$dbqueriesWITH
  
  if (popType == "VOL") {
    pltcondxadjWITHqry <- dbqueriesWITH$pltcondxadjVOL
  } else if (popType == "CHNG") {
    pltcondxadjWITHqry <- dbqueriesWITH$pltcondxadjCHNG
  }
  
  
  rowvar <- "FORTYPCD"
  landarea <- "FOREST"
  woodland <- "Y"
  estseed <- "none" 
  esttype <- "TREE"
  ratiotype <- "PERACRE"
  estvarn <- "TPA_UNADJ"
  #estvarn <- "volcfnet"
  estvarn.filter <- "STATUSCD == 1"
  
  
  
  ## Create landarea.filter
  ################################################################
  landarea.filter <- NULL
  if (landarea != "ALL") {
    if (landarea == "FOREST") {
      landarea.filter <- "pc.COND_STATUS_CD = 1"
    } else if (landarea == "TIMBERLAND") {
      landarea.filter <- "pc.SITECLCD IN(1:6) AND pc.RESERVCD = 0"
    }
  }
  pcwhereqry <- paste0("\nWHERE ", landarea.filter)
  
  
  ## Get rowvar data
  ################################################################
  if (!is.null(rowvar)) {
    datnmlst <- tolower(sort(unique(ref_codes$VARIABLE)))
    rowvar <- tolower(rowvar)
    if (rowvar %in% c(datnmlst, "spcd")) {
      if (rowvar == "spcd") {
        ref <- setDT(FIESTAutils::ref_species)
      } else {
        ref <- setDT(FIESTAutils::ref_codes[tolower(FIESTAutils::ref_codes[["VARIABLE"]]) == rowvar,
                                            c("VALUE", "MEANING")])
      }
      
      ## Classify row values
      ############################################################
      if (!is.null(row.classify)) {
        if (is.vector(row.classify)) {
          minx <- min(uniquex, na.rm=TRUE)
          minbrk <- min(row.classify)
          if (minx < minbrk) {
            minxmiss <- sort(unique(minx[minx < minbrk]))
            message("there are values in dataset less than class breaks defined for ", rowvar)
            message("...these values are classified as NA: ", toString(minxmiss))
          }
          rowclassnm <- paste0(rowvar, "CL")
          rowclassqry <- classifyqry(classcol = rowvar,
                                     cutbreaks = row.classify,
                                     class. = class.,
                                     fill = NULL)
          
        } else if (is.data.frame(row.classify)) {
          if (ncol(row.classify) != 2) {
            message("invalid row.classify... must be a vector of class breaks or a data.frame with 2 columns")
            stop()
          }
          rowclassnm <- names(row.classify)[!names(row.classify) %in% rowvar]
          if (length(rowclassnm) != 1) {
            message("invalid classes for ", rowvar, 
                    "... the data.frame must include name of variable to classify: ", rowvar)
            stop()
          }
          fromval <- row.classify[[rowvar]]
          toval <- row.classify[[rowclassnm]]
          
          ## Check values of fromval
          if (any(!fromval %in% uniquex)) {
            missvals <- fromval[which(!fromval %in% uniquex)]
            message("missing values in row.classify: ", toString(missvals))
          }
          rowclassqry <- classqry(rowvar, fromval, toval, 
                                  classnm = rowclassnm, 
                                  class. = class.,
                                  fill = NULL)
        } else {
          message("invalid row.classify... must be a vector of class breaks or a data.frame with 2 columns")
          stop()
        }
        
        classifyrow <- list()
        classifyrow[["row.classify"]] <- row.classify
        classifyrow[["rowclassnm"]] <- rowclassnm
        classifyrow[["rowclassqry"]] <- rowclassqry
        returnlst$classifyrow <- classifyrow
        
        ## if classified columns, create domclassify list for summarizing tree data
        if (any(!is.null(classifyrow), !is.null(classifycol))) {
          domclassify <- list()
          if (!is.null(classifyrow)) {
            domclassify[[rowvar]] <- classifyrow$row.classify
          }
          if (!is.null(classifycol)) {
            domclassify[[colvar]] <- classifycol$col.classify
          }
        }
      }
    }
  } 
  
  
  ###############################################################################
  ### Get condition domain-level data
  ###############################################################################
  pltidsWITHqry <- pltcondxadjWITHqry
  pcdomainlst <- rowvar
  
  if (esttype == "TREE") {
    
    treeflds <- DBI::dbListFields(popconn, "tree")
    if (!is.null(rowvar) && rowvar %in% treeflds) {
      tdomvar <- rowvar 
      bytdom <- TRUE
    } else {
      bytdom <- FALSE
    }
    
    ###############################################################################
    ### Get estimation data from tree table
    ###############################################################################

    ## Get tree query
    treeqry <- wwwGettreeqry(estvar = estvarn,
                             tfilter = estvarn.filter,
                             pcdomainlst = pcdomainlst,
                             pcwhereqry = pcwhereqry,
                             estseed = "add")
    #message(treeqry)
    
    ## Append tree query to pltidsWITHqry to get summed condition-level tree data
    tdomdatqry <- paste0(
      pltidsWITHqry, ", ",
      treeqry)
    #message(tdomdatqry)
    
    ## Run tdomdatqry
    tdomdat <- data.table(DBI::dbGetQuery(dbconn, tdomdatqry))
    setkeyv(tdomdat, c("plt_cn"))
    #head(tdomdat)
  }
  
  if (esttype %in% c("AREA", "RATIO")) {
    
    ## Get cond query
    condqry <- wwwGetcondqry(pcdomainlst = pcdomainlst,
                             pcwhereqry = pcwhereqry)
    #message(condqry)
    
    ## Append cond query to pltidsWITHqry to get summed condprop_adj data
    cdomdatqry <- paste0(
      pltidsWITHqry, 
      condqry)
    #message(cdomdatqry)
    
    ## Run cdomdatqry
    cdomdat <- data.table(DBI::dbGetQuery(dbconn, cdomdatqry))
    setkeyv(tdomdat, c("plt_cn"))
    #head(cdomdat)
  }
  
  if (esttype == "CHNG") {
    
    
  }
  
  
  ###############################################################################
  ### Get titles for output tables
  ###############################################################################
  if (esttype %in% c("TREE", "RATIO")) {
    alltitlelst <- 
      check.titles(dat = tdomdat, 
                   esttype = esttype, 
                   estseed = estseed, 
                   woodland = woodland, 
                   sumunits = TRUE, 
                   title.ref = title.ref, 
                   unitvar = unitvar, 
                   rowvar = rowvar, 
                   colvar = "NONE", 
                   estvarn = estvarn, 
                   estvarn.filter = estvarn.filter,
                   returntitle = TRUE, 
                   rawdata = TRUE, 
                   states = states, invyrs = invyrs,
                   landarea = landarea)
  } else {
    alltitlelst <- 
      check.titles(dat = cdomdat, 
                   esttype = esttype, 
                   sumunits = TRUE, 
                   title.ref = title.ref, 
                   unitvar = unitvar, 
                   rowvar = rowvar, 
                   colvar = "NONE", 
                   returntitle = TRUE, 
                   rawdata = TRUE, 
                   states = states, invyrs = invyrs,
                   landarea = landarea)
  }
  
  title.est <- alltitlelst$title.est
  title.pse <- alltitlelst$title.pse
  title.estpse <- alltitlelst$title.estpse
  title.ref <- alltitlelst$title.ref
  title.rowvar <- alltitlelst$title.rowvar
  outfn.estpse <- alltitlelst$outfn.estpse
  if (rawdata) {
    outfn.rawdat <- alltitlelst$outfn.rawdat
  }
  
  if (!is.null(rowvar)) {
    rowvarnm <- title.rowvar
    uniquerow <- ref
    names(uniquerow) <-c(rowvar, title.rowvar)
    uniquerow[[rowvar]] <- factor(uniquerow[[rowvar]])
  } else {
    rowvar=rowvarnm <- "TOTAL"
  }
  

  ###################################################################################
  ## GENERATE ESTIMATES
  ###################################################################################
  estvarn.name <- "estimated_tvalue"
  
  if (SAE) {
    estdatSA <- 
      getSAestimates(esttype = esttype,
                     domdat = tdomdat,
                     SApackage = SApackage,
                     uniqueid = "plt_cn",
                     estvar.name = estvarn.name,
                     rowvar = rowvar, colvar = "NONE", 
                     grpvar = NULL,
                     pltassgnx = pltassgnx,
                     dunitarea = unitarea,
                     dunitvar = unitvar,
                     areavar = areavar,
                     prednames = prednames,
                     predfac = predfac,
                     totals = totals,
                     uniquerow = uniquerow,
                     uniquecol = uniquecol)
    
    
    
  } else {
    cols2keep <- c("est", "est.se", "pse")
    estdatGB <- 
      getGBestimates(esttype = esttype,
                     domdatn = tdomdat,
                     domdatd = cdomdat,
                     uniqueid = "plt_cn",
                     estvarn.name = estvarn.name,
                     estvard.name = NULL,
                     rowvar = rowvar, colvar = "NONE", 
                     #grpvar = NULL,
                     pltassgnx = pltassgnx,
                     unitarea = unitarea,
                     unitvar = unitvar,
                     areavar = areavar,
                     stratalut = stratalut,
                     strvar = strvar,
                     totals = totals,
                     sumunits = FALSE,
                     uniquerow = uniquerow,
                     uniquecol = uniquecol)
    
    if (is.null(estdatGB)) stop()
    GBcols2keep <- c("domain_unit", "NBRPLT.gt0", "total_acres", cols2keep)
    unit_totestGB <- estdatGB$unit_totest[, 
          c("domain_unit", "NBRPLT.gt0", "total_acres", cols2keep), with=FALSE]
    setnames(unit_totestGB, cols2keep, paste0("GB", cols2keep))
    unit_rowestGB <- estdatGB$unit_rowest[, 
          c("domain_unit", rowvar, title.rowvar, "NBRPLT.gt0", "total_acres", cols2keep), with=FALSE]
    setnames(unit_rowestGB, cols2keep, paste0("GB", cols2keep))
    # unit_colestGB <- estdatGB$unit_colest[, 
    #       c("domain_unit", colvar, title.colvar, "NBRPLT.gt0", "total_acres", cols2keep), with=FALSE], with=FALSE]
    # setnames(unit_colestGB, cols2keep, paste0("GB", cols2keep))
    # unit_grpestGB <- estdatGB$unit_grpest[, 
    #       c("domain_unit", rowvar, title.rowvar, colvar, title.colvar, "NBRPLT.gt0", "total_acres", cols2keep), with=FALSE], with=FALSE]
    # setnames(unit_grpestGB, cols2keep, paste0("GB", cols2keep))
    
    estdatMA <- 
      getMAestimates(MAmethod = "greg",
                     esttype = esttype,
                     domdatn = tdomdat,
                     domdatd = cdomdat,
                     uniqueid = "plt_cn",
                     estvarn.name = estvarn.name,
                     estvard.name = NULL,
                     rowvar = rowvar, colvar = "NONE", 
                     #grpvar = NULL,
                     pltassgnx = pltassgnx,
                     unitlut = unitlut,
                     unitarea = unitarea,
                     npixels = npixels,
                     unitvar = unitvar,
                     areavar = areavar,
                     prednames = prednames,
                     totals = totals,
                     uniquerow = uniquerow,
                     uniquecol = uniquecol,
                     modelselect = TRUE)
    if (is.null(estdatMA)) stop()
    unit_totestMA <- estdatMA$unit_totest[, 
            c("domain_unit", cols2keep), with=FALSE]
    setnames(unit_totestMA, cols2keep, paste0("GREG", cols2keep))
    unit_rowestMA<- estdatMA$unit_rowest[, 
            c("domain_unit", rowvar, cols2keep), with=FALSE]
    setnames(unit_rowestMA, cols2keep, paste0("GREG", cols2keep))
    # unit_colestMA <- estdatMA$unit_colest[, 
    #         c("domain_unit", colvar, cols2keep), with=FALSE]
    # setnames(unit_colestMA, cols2keep, paste0("GREG", cols2keep))
    # unit_grpestMA <- estdatMA$unit_grpest[, 
    #         c("domain_unit", rowvar, colvar, cols2keep), with=FALSE]
    # setnames(unit_grpestMA, cols2keep, paste0("GREG", cols2keep))
  

    raw_unit_totest <- unit_totestGB[unit_totestMA]
    raw_unit_rowest <- unit_rowestGB[unit_rowestMA]
    
    ## Append total number of plots in by domain_unit to raw_unit_totest
    raw_unit_totest <- merge(raw_unit_totest, unitlut[,c("domain_unit", "n.total")], by="domain_unit")
    
    
    
    ## Choose estimator based on the smallest se of the total estimate
    ###################################################################################
    if (unit_totestMA$GREGest.se < unit_totestGB$GBest) {
      unit_totest <- unit_totestMA
      unit_rowest <- unit_rowestMA
      
      WWWpopdat$reportdata$estimator <- "GREG"
        
    } else {
      unit_totest <- unit_totestGB
      unit_rowest <- unit_rowestGB
      
      WWWpopdat$reportdata$estimator <- "Post-stratified"
    }
  }
  
  
  
  ## Write files to outfolder
  ###################################################################################
  writefiles <- c("unit_totest", "unit_rowest", "raw_unit_totest", "raw_unit_rowest")
  for (i in 1:length(writefiles)) {
    write2csv(get(writefiles[i]), 
              outfolder = outfolder, 
              outfilenm = writefiles[i], 
              overwrite = FALSE)
  }
  
  returnlst <- list(unit_totest = unit_totest,
                    unit_rowest = unit_rowest,
                    titlelst = alltitlelst)
  return(returnlst)
}
