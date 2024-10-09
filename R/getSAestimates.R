getSAestimates <- function(esttype, i, largebnd.unique,
                           estvar.name, domdat,
                           pltassgnx,
                           unitlut,
                           unitvar,
                           uniqueid, pltassgnid,
                           prednames,
                           rowvar,
                           SApopdatnm,
                           SAdomsDF,
                           smallbnd.dom,
                           SApackage,
                           SAmethod,
                           showsteps,
                           savesteps,
                           stepfolder,
                           prior,
                           modelselect,
                           multest,
                           SAobjlst,
                           estlst,
                           pdomdatlst,
                           dunitlutlst,
                           SAdomvars,
                           SAobjlst_row,
                           estlst_row,
                           predselectlst.unit,
                           predselectlst.area,
                           predselectlst.unit_row,
                           predselectlst.area_row,
                           pdomdatlst_row,
                           dunitlutlst_row,
                           save4testing) {
  
  

  
  dunit_totest=dunit_rowest=dunit_colest=dunit_grpest=rowunit=totunit <- NULL
  response <- estvar.name
  
  if (i == 1) {
    message("getting estimates for ", response, "...")
    message("using the following predictors... ", toString(prednames))
  }
  
  
  vars2keep <- NULL
  if (!is.null(largebnd.unique)) {
    if (largebnd.unique %in% names(domdat) && largebnd.unique %in% names(pltassgnx)) {
      domdat <- merge(pltassgnx, domdat, 
                      by.x = c(largebnd.unique, pltassgnid, "DOMAIN"), 
                      by.y = c(largebnd.unique, uniqueid, "DOMAIN"), , all.x=TRUE)
    } else if (largebnd.unique %in% names(pltassgnx)) {
      domdat <- merge(pltassgnx, domdat, 
                      by.x = c(pltassgnid, "DOMAIN"), 
                      by.y = c(uniqueid, "DOMAIN"), all.x=TRUE)
    } else if (!is.null(SAdomsdf)) {
      domdat <- merge(domdat, 
                      unique(setDT(SAdomsdf)[, c(smallbnd.dom, largebnd.unique), with=FALSE]),
                      by=smallbnd.dom)
    } else {
      domdat$LARGEBND <- 1
      largebnd.unique <- "LARGEBND"
    }
 
  } else {
    domdat$LARGEBND <- 1
    largebnd.unique <- "LARGEBND"
    # domdat <- merge(pltassgnx, domdat, 
    #                 by.x=c(pltassgnid, "DOMAIN"), 
    #                 by.y=c(uniqueid, "DOMAIN"), all.x=TRUE)
    domdat <- merge(pltassgnx, domdat, 
                    by.x=c(pltassgnid), 
                    by.y=c(uniqueid), all.x=TRUE)
  }
  
  if (pltassgnid != uniqueid) {
    setnames(domdat, pltassgnid, uniqueid)
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
  
  byvars <- unique(c(vars2keep, largebnd.unique, unitvar, "AOI", uniqueid, "TOTAL", prednames))
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
  
  dunit_totestlst <- 
    tryCatch(
      {
        lapply(largebnd.vals, SAest.large, 
               dat = domdattot, 
               cuniqueid = uniqueid, largebnd.unique = largebnd.unique, 
               dunitlut = unitlut, dunitvar = "DOMAIN", 
               prednames = prednames, domain = "TOTAL", response = response, 
               showsteps = showsteps, savesteps = savesteps, 
               stepfolder = stepfolder, prior = prior, 
               modelselect=modelselect, multest=multest,
               SApackage = SApackage, SAmethod = SAmethod, bayes = bayes, 
               save4testing=FALSE, vars2keep = vars2keep)
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
  
  estlst[[SApopdatnm]] <- dunit_est
  
  ## row estimates
  if (rowvar != "TOTAL") {
    
    domdat <- domdat[!is.na(domdat[[rowvar]]),] 
    domdattot <- setDT(domdat)[ ,lapply(.SD, sum, na.rm=TRUE), 
                                by=c(largebnd.unique, unitvar, uniqueid, rowvar, prednames)
                                , .SDcols=response]
    
    if (!"DOMAIN" %in% names(domdattot)) {
      domdattot$DOMAIN <- domdattot[[unitvar]]
      domdattot[[unitvar]] <- NULL
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
                 dunitlut = unitlut, dunitvar = "DOMAIN",
                 prednames = prednames, domain = rowvar,
                 response = response, 
                 showsteps = showsteps, savesteps = savesteps,
                 stepfolder = stepfolder, prior = prior, 
                 modelselect = modelselect, multest = multest, 
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
              predselectlst.unit = predselectlst.unit,
              estlst = estlst)
  
  return(out)
  
}




















