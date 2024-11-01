modWWWest <- function(WWWpopdat,
                      dbconn, 
                      esttype,
                      landarea,
                      estvar = NULL, 
                      estvar.filter = NULL,
                      rowvar = NULL, 
                      colvar = NULL, 
                      estseed = "none", 
                      ratiotype = "PERACRE",
                      woodland = "Y",
                      SApackage = "JoSAE",
                      savedata = FALSE, 
                      table_opts = NULL, 
                      title_opts = NULL, 
                      outfolder = NULL){
  
  ##################################################################################
  ## DESCRIPTION: 
  ## Generates per-acre or per-tree estimates by domain using ratio estimators
  ##################################################################################
  
  unit_totest <- unit_rowest <- raw_unit_rowest <- raw_unit_totest <- NULL
  
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
  pcheck.params(input.params, table_opts=table_opts, title_opts=title_opts)
  
  
 
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
  unitvar <- "domain_unit"
  
  
  ##################################################################
  ## Get population data from WWWpopdat
  ##################################################################
  SAE <- WWWpopdat$SAE
  popType <- WWWpopdat$popType
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
    treeqry <- wwwGettreeqry(estvar = estvar,
                             tfilter = estvar.filter,
                             pcdomainlst = pcdomainlst,
                             pcwhereqry = pcwhereqry,
                             estseed = estseed)
    #message(treeqry)
    
    ## Append tree query to pltidsWITHqry to get summed condition-level tree data
    domdatqry <- paste0(
      pltidsWITHqry, ", ",
      treeqry)
    #message(domdatqry)
    
    ## Run domdatqry
    domdat <- data.table(DBI::dbGetQuery(dbconn, domdatqry))
    setkeyv(domdat, c("plt_cn"))
    #head(tdomdat)
  }
  
  if (esttype %in% c("AREA", "RATIO")) {
    
    ## Get cond query
    condqry <- wwwGetcondqry(pcdomainlst = pcdomainlst,
                             pcwhereqry = pcwhereqry)
    #message(condqry)
    
    ## Append cond query to pltidsWITHqry to get summed condprop_adj data
    domdatqry <- paste0(
      pltidsWITHqry, 
      condqry)
    #message(domdatqry)
    
    ## Run domdatqry
    domdat <- data.table(DBI::dbGetQuery(dbconn, domdatqry))
    setkeyv(domdat, c("plt_cn"))
    #head(cdomdat)
    
    if (esttype == "RATIO") {
      cdomdat <- domdat
    }
  }
  
  if (esttype == "CHNG") {
    
    
  }
  
  
  ###############################################################################
  ### Get titles for output tables
  ###############################################################################
  if (esttype %in% c("TREE", "RATIO")) {
    alltitlelst <- 
      check.titles(dat = domdat, 
                   esttype = esttype, 
                   estseed = estseed, 
                   woodland = woodland, 
                   sumunits = TRUE, 
                   title.ref = title.ref, 
                   unitvar = unitvar, 
                   rowvar = rowvar, 
                   colvar = "NONE", 
                   estvarn = estvar, 
                   estvarn.filter = estvar.filter,
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
  estvar.name <- "estimated_tvalue"
  
  if (SAE) {
    
    SAestimates <- getSAestimatesWWW(esttype = esttype,
                                     i = 1,
                                     largebnd.unique = largebnd.unique,
                                     estvar.name = estvar.name,
                                     domdat = domdat,
                                     pltassgnx = pltassgnx,
                                     dunitlut = unitlut,
                                     dunitvar = unitvar,
                                     uniqueid = "plt_cn",
                                     pltassgnid = "plt_cn",
                                     prednames = prednames,
                                     rowvar = rowvar,
                                     SApopdatnm = "out",
                                     SAdomsDF = NULL,
                                     smallbnd.dom = "domain_unit",
                                     SApackage = SApackage,
                                     SAmethod = SAmethod,
                                     showsteps = FALSE,
                                     savesteps = FALSE,
                                     stepfolder = NULL,
                                     prior = function(x) 1/(sqrt(x)*(1+x)),
                                     modelselect = FALSE,
                                     multest = TRUE,
                                     SAobjlst = list(),
                                     estlst = list(),
                                     pdomdatlst = list(),
                                     dunitlutlst = list(),
                                     SAdomvars = NULL,
                                     SAobjlst_row = list(),
                                     estlst_row = list(),
                                     predselectlst.unit = list(),
                                     predselectlst.area = list(),
                                     predselectlst.unit_row = list(),
                                     predselectlst.area_row = list(),
                                     pdomdatlst_row = list(),
                                     dunitlutlst_row = list(),
                                     save4testing = FALSE)
    
    if(is.null(SAestimates)) stop()
    
    SAobjlst <- SAestimates$SAobjlst
    estlst <- SAestimates$estlst
    
    estdf <- do.call(rbind, estlst)
    
    aoi_doms <- unique(pltassgnx[pltassgnx$aoi == 1, "domain_unit"][[1]])
    
    estdf$aoi <- ifelse(estdf$DOMAIN %in% aoi_doms, 1, 0)
    
    vars <- c("ecomap_province", "total", "DOMAIN", "NBRPLT", "NBRPLT.gt0")
    estimator_vars <- c("JU.GREG", "JU.GREG.se", "JFH", "JFH.se")
    estdf <- estdf[estdf$aoi == 1, c(vars, estimator_vars), with = FALSE]
    raw_unit_totest <- estdf
    
    # choose based on which has fewer NA values
    if(mean(!is.na(estdf$JFH)) >= mean(!is.na(estdf$JU.GREG))) {
      chosen.est <- c("JFH", "JFH.se")
      WWWpopdat$reportdata$estimator <- "JFH"
    } else {
      chosen.est <- c("JU.GREG", "JU.GREG.se")
      WWWpopdat$reportdata$estimator <- "JU.GREG"
    }
    unit_totest <- estdf[ , c("DOMAIN", chosen.est, "NBRPLT", "NBRPLT.gt0"), with = FALSE]
    names(unit_totest) <- c("domain_unit", "est", "est.se", "n.total", "NBRPLT.gt0")
    
    SAobjlst_row <- SAestimates$SAobjlst_row
    estlst_row <- SAestimates$estlst_row
    
    if(length(estlst_row) != 0) {
      
      estdf_row <- do.call(rbind, estlst_row)
      estdf_row$aoi <- ifelse(estdf_row$DOMAIN %in% aoi_doms, 1, 0)
    
      estdf_row <- estdf_row[estdf_row$aoi == 1, c(vars, estimator_vars), with = FALSE]
      raw_unit_rowest <- estdf_row
      
      # choose based on which has fewer NA values
      if(mean(!is.na(estdf_row$JFH)) >= mean(!is.na(estdf_row$JU.GREG))) {
        chosen.est <- c("JFH", "JFH.se")
        WWWpopdat$reportdata$estimator <- "JFH"
      } else {
        chosen.est <- c("JU.GREG", "JU.GREG.se")
        WWWpopdat$reportdata$estimator <- "JU.GREG"
      }
      unit_rowest <- estdf_row[ , c("DOMAIN", chosen.est, "NBRPLT", "NBRPLT.gt0"), with = FALSE]
      setnames(unit_rowest, "NBRPLT", "n.total")
      setnames(unit_rowest, "DOMAIN", "domain_unit")
      
    }
    
  } else {
    
    cols2keep <- c("est", "est.se", "pse")
    estdatGB <- 
      getGBestimatesWWW(esttype = esttype,
                        domdatn = domdat,
                        domdatd = cdomdat,
                        uniqueid = "plt_cn",
                        estvarn.name = estvar.name,
                        estvard.name = NULL,
                        rowvar = rowvar, 
                        colvar = "NONE", 
                        grpvar = NULL,
                        pltassgnx = pltassgnx,
                        unitarea = unitarea,
                        unitvar = unitvar,
                        stratalut = stratalut,
                        strvar = strvar,
                        uniquerow = uniquerow,
                        uniquecol = uniquecol)
    
    if (is.null(estdatGB)) stop()
    GBcols2keep <- c("domain_unit", "NBRPLT.gt0", "total_acres", cols2keep)
    unit_totestGB <- estdatGB$unit_totest[, 
          c("domain_unit", "NBRPLT.gt0", "total_acres", cols2keep), with=FALSE]
    setnames(unit_totestGB, cols2keep, paste0("GB", cols2keep))
    
    if (!is.null(estdatGB$unit_rowest)) {
      unit_rowestGB <- estdatGB$unit_rowest[, 
           c("domain_unit", rowvar, title.rowvar, "NBRPLT.gt0", "total_acres", cols2keep), with=FALSE]
      setnames(unit_rowestGB, cols2keep, paste0("GB", cols2keep))
    }
    
    estdatMA <- 
      getMAestimatesWWW(MAmethod = "greg",
                        esttype = esttype,
                        domdatn = domdat,
                        domdatd = cdomdat,
                        uniqueid = "plt_cn",
                        estvarn.name = estvar.name,
                        estvard.name = NULL,
                        rowvar = rowvar, 
                        colvar = "NONE", 
                        grpvar = NULL,
                        pltassgnx = pltassgnx,
                        unitlut = unitlut,
                        unitarea = unitarea,
                        npixels = npixels,
                        unitvar = unitvar,
                        prednames = prednames,
                        uniquerow = uniquerow,
                        uniquecol = uniquecol,
                        modelselect = TRUE)
    
    if (is.null(estdatMA)) stop()
    unit_totestMA <- estdatMA$unit_totest[, 
            c("domain_unit", cols2keep), with=FALSE]
    setnames(unit_totestMA, cols2keep, paste0("GREG", cols2keep))
    raw_unit_totest <- unit_totestGB[unit_totestMA]
    
    if (!is.null(estdatMA$unit_rowest)) {
      unit_rowestMA<- estdatMA$unit_rowest[, 
              c("domain_unit", rowvar, cols2keep), with=FALSE]
      setnames(unit_rowestMA, cols2keep, paste0("GREG", cols2keep))
      raw_unit_rowest <- unit_rowestGB[unit_rowestMA]
    }
    
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
                    titlelst = alltitlelst,
                    domdatqry = domdatqry,
                    reportdata = WWWpopdat$reportdata)
  return(returnlst)
}
