getSAestimates <- function(esttype, i, largebnd.unique,
                           estvar.name, domdat,
                           pltassgnx,
                           dunitlut,
                           dunitvar,
                           uniqueid,
                           prednames,
                           rowvar,
                           SApopdatnm,
                           SAdomsDF,
                           vars2keep = NULL,
                           smallbnd.dom,
                           SApackage,
                           SAmethod,
                           showsteps,
                           savesteps,
                           stepfolder,
                           prior,
                           modelselect,
                           multest,
                           multest_estimators = 'all',
                           SAobjlst,
                           estlst,
                           pdomdatlst,
                           dunitlutlst,
                           uniquerow,
                           uniquecol,
                           SAdomvars,
                           SAobjlst_row,
                           estlst_row,
                           predselectlst.unit,
                           predselectlst.area,
                           predselectlst.unit_row,
                           predselectlst.area_row,
                           pdomdatlst_row,
                           dunitlutlst_row,
                           row.NAname,
                           col.NAname,
                           save4testing) {
  
  dunit_totest=dunit_rowest=dunit_colest=dunit_grpest=rowunit=totunit <- NULL
  response <- estvar.name
  addSAdomsdf <- FALSE
  SAdomsdf <- NULL
  save4testing <- FALSE
  
  if (i == 1) {
    message("getting estimates for ", response, "...")
    message("using the following predictors... ", toString(prednames))
  }
  
  ## Check column names
  prednames <- names(data.frame(matrix(NA,1,length(prednames), 
                                       dimnames=list(rownames=NULL, colnames=prednames))))
  pltassgnx <- data.table(pltassgnx, check.names = TRUE)
  dunitlut <- data.table(dunitlut, check.names = TRUE)
  
  
  if (!is.null(largebnd.unique)) {
    if (!largebnd.unique %in% names(pltassgnx)) {
      stop("largebnd.unique is not in pltassgn")
    }
    if (largebnd.unique %in% names(domdat) && largebnd.unique %in% names(pltassgnx)) {
      domdat[[largebnd.unique]] <- NULL
    }
  }
  ## Join domdat to pltassgnx using data.table key
  domdat <- pltassgnx[domdat]
  if (is.null(largebnd.unique)) {
    domdat$LARGEBND <- 1
    dunitlut$LARGEBND <- 1
    largebnd.unique <- "LARGEBND"
  }
  ## Append TOTAL to domdatn
  if (!"TOTAL" %in% names(domdat)) {
    domdat$TOTAL <- 1
  }
 
  if (SApackage == "spAbundance") {
    bayes <- TRUE
  } else {
    bayes <- FALSE
  }
  if (bayes) {
    vars2keep <- largebnd.unique
    tdomdat$LARGEBND <- 1
    largebnd.unique <- "LARGEBND"
    largebnd.vals <- 1
  }
  
  byvars <- unique(c(vars2keep, largebnd.unique, dunitvar, "AOI", uniqueid, "TOTAL", prednames))
  if (all(c("X", "Y") %in% names(pltassgnx))) {
    byvars <- c(byvars, "X","Y")
  }
  ## Sum estvar.name by dunitvar (DOMAIN), plot, domain
  domdattot <- domdat[, lapply(.SD, sum, na.rm=TRUE), 
                        by=byvars, 
                        .SDcols=estvar.name]
  
  
  ## get unique largebnd values
  largebnd.vals <- sort(unique(domdattot[[largebnd.unique]]))
  largebnd.vals <- largebnd.vals[table(domdattot[[largebnd.unique]]) > 30]
  
# dat = domdattot
# cuniqueid = uniqueid
# dunitvar = "DOMAIN"
# domain = "TOTAL"
# largebnd.val = largebnd.vals[1]
#   

  dunit_totestlst <- 
    tryCatch(
      {
        lapply(largebnd.vals, SAest.large, 
               dat = domdattot, 
               cuniqueid = uniqueid, largebnd.unique = largebnd.unique, 
               dunitlut = dunitlut, dunitvar = "DOMAIN", 
               prednames = prednames, domain = "TOTAL", response = response, 
               showsteps = showsteps, savesteps = savesteps, 
               stepfolder = stepfolder, prior = prior, 
               modelselect = modelselect, 
               multest = multest, multest_estimators = multest_estimators,
               SApackage = SApackage, SAmethod = SAmethod, bayes = bayes, 
               save4testing = FALSE, vars2keep = vars2keep)
      },
      error = function(cond) {
        message("error with estimates of ", response, "...")
        message(cond, "\n")
        return(NULL)
      }
    )

  if (is.null(dunit_totestlst)) {
    return(NULL)
  }
  
  if (length(largebnd.vals) > 1) {
    
    dunit_est <- do.call(rbind, do.call(rbind, dunit_totestlst)[ ,"est.large"])
    
    if (multest || SAmethod == "unit") {
      predselect.unit <- do.call(rbind, dunit_totestlst)[ ,"predselect.unit"]
    }
    if (multest || SAmethod == "area") {
      predselect.area <- do.call(rbind, dunit_totestlst)[ ,"predselect.area"]
    }
    if (save4testing) {
      pdomdat <- do.call(rbind, do.call(rbind, dunit_totestlst)[ ,"pltdat.dom"])
      dunitlut <- do.call(rbind, do.call(rbind, dunit_totestlst)[ ,"dunitlut.dom"])
    }
    
    SAobjlst[[SApopdatnm]] <- do.call(rbind, dunit_totestlst)[,"SAobjlst.dom"]
    
  } else {
    
    dunit_est <- do.call(rbind, dunit_totestlst)[ ,"est.large"]$est.large
    
    if (multest || SAmethod == "unit") {
      predselect.unit <- do.call(rbind, dunit_totestlst)[ ,"predselect.unit"]$predselect.unit
    }
    if (multest || SAmethod == "area") {
      predselect.area <- do.call(rbind, dunit_totestlst)[ ,"predselect.area"]$predselect.area
    }
    if (save4testing) {
      pdomdat <- do.call(rbind, dunit_totestlst)[ ,"pltdat.dom"]$pltdat.dom
      dunitlut <- do.call(rbind, dunit_totestlst)[ ,"dunitlut.dom"]$dunitlut.dom
    }
    
    SAobjlst[[SApopdatnm]] <- do.call(rbind, dunit_totestlst)[ ,"SAobjlst.dom"]$SAobjlst.dom
    
  }
  
  if (multest || SAmethod == "unit") {
    predselectlst.unit[[SApopdatnm]] <- predselect.unit
  }
  if (multest || SAmethod == "area") {
    predselectlst.area[[SApopdatnm]] <- predselect.area
  }
  
  if (save4testing) {
    ## Merge SAdom attributes to dunit_totest
    if (addSAdomsdf) {
      pdomdat <- merge(setDT(SAdomsdf)[ ,unique(c("DOMAIN", "AOI", SAdomvars)), with=FALSE], 
                       pdomdat,
                       by=c("DOMAIN", "AOI"))
      dunitlut <- merge(setDT(SAdomsdf)[ ,unique(c("DOMAIN", "AOI", SAdomvars)), with=FALSE], 
                        dunitlut,
                        by=c("DOMAIN", "AOI"))
    }
    
    pdomdatlst[[SApopdatnm]] <- pdomdat
    dunitlutlst[[SApopdatnm]] <- dunitlut
    
  }

  if (is.null(dunit_est)) {
    warning("no estimates generated...")
    return(0)
  }
  estlst[[SApopdatnm]] <- dunit_est

  ## row estimates
  if (rowvar != "TOTAL") {
    
    ## Check uniquerow - add NA factor value
    uniquerow <- check.unique(x = domdat, 
                              uniquex = uniquerow,
                              xvar = rowvar, 
                              NAname = row.NAname)
    
    domdat <- domdat[!is.na(domdat[[rowvar]]),] 
    domdattot <- setDT(domdat)[ ,lapply(.SD, sum, na.rm=TRUE), 
                                by=c(largebnd.unique, dunitvar, uniqueid, rowvar, prednames)
                                , .SDcols=response]
    
    if (!"DOMAIN" %in% names(domdattot)) {
      domdattot$DOMAIN <- domdattot[[dunitvar]]
      domdattot[[dunitvar]] <- NULL
    }
    if (!"AOI" %in% names(domdattot)) {
      domdattot$AOI <- 1
    }
    
    dunit_rowestlst <-
      tryCatch(
        {
          lapply(largebnd.vals, SAest.large, 
                 dat = domdattot, 
                 cuniqueid = uniqueid, largebnd.unique = largebnd.unique, 
                 dunitlut = dunitlut, dunitvar = "DOMAIN",
                 prednames = prednames, domain = rowvar,
                 response = response, 
                 showsteps = showsteps, savesteps = savesteps,
                 stepfolder = stepfolder, prior = prior, 
                 modelselect = modelselect, 
                 multest = multest, multest_estimators = multest_estimators,
                 SApackage = SApackage, SAmethod = SAmethod, bayes = bayes,
                 vars2keep = vars2keep)
        },
        error = function(cond) {
          message("error with estimates of ", response, " by ", rowvar, "...")
          message(cond, "\n")
          return(NULL)
        }
      )

    if (length(largebnd.vals) > 1) {
      
      dunit_est_row <- do.call(rbind, do.call(rbind, dunit_rowestlst)[ ,"est.large"])
      
      if (multest || SAmethod == "unit") {
        predselect.unit_row <- do.call(rbind, dunit_rowestlst)[ ,"predselect.unit"]
      }
      if (multest || SAmethod == "area") {
        predselect.area_row <- do.call(rbind, dunit_rowestlst)[ ,"predselect.area"]
      }
      if (save4testing) {
        pdomdat_row <- do.call(rbind, do.call(rbind, dunit_rowestlst)[ ,"pltdat.dom"])
        dunitlut_row <- do.call(rbind, do.call(rbind, dunit_rowestlst)[ ,"dunitlut.dom"])
      }
      
      SAobjlst_row[[SApopdatnm]] <- do.call(rbind, dunit_rowestlst)[ ,"SAobjlst.dom"]
      
    } else {
      
      dunit_est_row <- do.call(rbind, dunit_rowestlst)[ ,"est.large"]$est.large
      
      if (multest || SAmethod == "unit") {
        predselect.unit_row <- do.call(rbind, dunit_rowestlst)[ ,"predselect.unit"]$predselect.unit
      }
      if (multest || SAmethod == "area") {
        predselect.area_row <- do.call(rbind, dunit_rowestlst)[ ,"predselect.area"]$predselect.area
      }
      if (save4testing) {
        pdomdat_row <- do.call(rbind, dunit_rowestlst)[ ,"pltdat.dom"]$pltdat.dom
        dunitlut_row <- do.call(rbind, dunit_rowestlst)[ ,"dunitlut.dom"]$dunitlut.dom
      }
      
      SAobjlst_row[[SApopdatnm]] <- do.call(rbind, dunit_rowestlst)[ ,"SAobjlst.dom"]$SAobjlst.dom
      
    }
    
    if (multest || SAmethod == "unit") {
      predselectlst.unit_row[[SApopdatnm]] <- predselect.unit_row
    }
    
    if (multest || SAmethod == "area") {
      predselectlst.area_row[[SApopdatnm]] <- predselect.area_row
    }
    
    if (save4testing) {
      ## Merge SAdom attributes to dunit_totest
      if (addSAdomsdf) {
        pdomdat_row <- merge(setDT(SAdomsdf)[, unique(c("DOMAIN", "AOI", SAdomvars)), with=FALSE], 
                             pdomdat_row,
                             by=c("DOMAIN", "AOI"))
        dunitlut_row <- merge(setDT(SAdomsdf)[ ,unique(c("DOMAIN", "AOI", SAdomvars)), with=FALSE], 
                              dunitlut_row,
                              by=c("DOMAIN", "AOI"))
      }
      
      pdomdatlst_row[[SApopdatnm]] <- pdomdat_row
      dunitlutlst_row[[SApopdatnm]] <- dunitlut_row
      
    }
    
    estlst_row[[SApopdatnm]] <- dunit_est_row
    
  }
  
  out <- list(largebnd.unique = largebnd.unique,
              response = response,
              domdat = domdat,
              SAobjlst = SAobjlst,
              estlst = estlst,
              SAobjlst_row = SAobjlst_row,
              estlst_row = estlst_row,
              predselectlst.unit = predselectlst.unit,
              predselectlst.area = predselectlst.area,
              predselectlst.unit_row = predselectlst.unit_row,
              predselectlst.area_row = predselectlst.area_row)
  
  if (save4testing) {
    out$pdomdatlst <- pdomdatlst
    out$dunitlutlst <- dunitlutlst
    if (rowvar != "TOTAL") {
      out$pdomdatlst_row <- pdomdatlst_row
      out$dunitlutlst_row <- dunitlutlst_row
    }
  }

  return(out)
  
}




















