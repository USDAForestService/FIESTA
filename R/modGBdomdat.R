#' Generates estimates from domain-level data.
#' 
#' @param domdat List. domain-level data for estimation
#' @param uniqueid string. name of unique identifier of plots in domdat (e.g., plt_cn) 
#' @param estvar string. name of variable in domdat to estimate.
#' @param rowvar string. name of rowvar domain in domdat to estimate by.
#' @param colvar string. name of colvar domain in domdat to estimate by.
#' @param stratdat list. returned list from spGetStrata. If NULL, no strata
#'     used for estimation (i.e., simple random sample)
#' @param savedata Logical. If TRUE, saves table(s) to outfolder.
#' @param table_opts List. See help(table_options()) for a list of
#' options.
#' @param savedata_opts List. See help(savedata_options()) for a list
#' of options. Only used when savedata = TRUE.  
#' @param ...  Parameters for modGBpop() if GBpopdat is NULL.
#'
#' @return A list with estimates with percent sampling error for rowvar (and
#' colvar).  If sumunits=TRUE or unitvar=NULL and colvar=NULL, one data frame
#' is returned.  Otherwise, a list object is returned with the following
#' information.  If savedata=TRUE, all data frames are written to outfolder.
#' 
#' @keywords data
#' @export modGBdomdat
modGBdomdat <- function(domdat,
                        uniqueid = NULL,
                        estvar, 
                        rowvar = NULL, 
                        colvar = NULL, 
                        stratdat = NULL,
                        savedata = FALSE, 
                        table_opts = NULL, 
                        title_opts = NULL, 
                        savedata_opts = NULL, 
                        ...){

  ##############################################################################
  ## DESCRIPTION:
  ## Generates estimates of trees by domain using non-ratio estimators.
  ##############################################################################

 
  ## Set parameter
  esttype <- "TREE"
  returnlst <- list()
  sumunits <- TRUE
  domdatd <- NULL
  condid <- "CONDID"
  rawdata <- TRUE

  ## Set global variables
  stratalut=strvar=rowvarnm=colvarnm <- NULL
  areavar=unitvars = NULL
  
 
  ##################################################################
  ## CHECK PARAMETER NAMES
  ##################################################################
  
  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- names(formals(modGBdomdat)) 
  if (!all(input.params %in% formallst)) {
    miss <- input.params[!input.params %in% formallst]
    stop("invalid parameter: ", toString(miss))
  }
 
  ## Check parameter lists
  pcheck.params(input.params = input.params,
                table_opts = table_opts, 
                savedata_opts = savedata_opts)
  
  ## Check parameter option lists
  optslst <- pcheck.opts(optionlst = list(
    table_opts = table_opts,
    savedata_opts = savedata_opts)) 
  table_opts <- optslst$table_opts  
  savedata_opts <- optslst$savedata_opts  
  
  for (i in 1:length(table_opts)) {
    assign(names(table_opts)[[i]], table_opts[[i]])
  }
  
  
  ##################################################################
  ## CHECK PARAMETER INPUTS
  ##################################################################
  if (!is.null(stratdat)) {
    list.items <- c("pltassgn", "stratalut", "unitarea")
  
    stratdat <- pcheck.object(stratdat, "stratdat", list.items=list.items)
    if (is.null(stratdat)) return(NULL)
    pltassgn <- data.table(stratdat$pltassgn)
    pltassgnid <- stratdat$pltassgnid
    unitvar <- stratdat$unitvar
    areavar <- stratdat$areavar
    strvar <- stratdat$strvar
    areaunits <- tolower(stratdat$areaunits)
    strwtvar <- "strwt"
    stratalut <- data.table(stratdat$stratalut)
    setnames(stratalut, "P1POINTCNT", "n.strata")
    stratalut[, n.total := sum(n.strata), by = unitvar]
    
  } else {
    
    uniqueid <- findnm(uniqueid, names(domdat), returnNULL = TRUE)
    if (is.null(uniqueid)) {
      stop("uniqueid must be in domdat")
    }
    uniqueid <- findnm(uniqueid, names(domdat), returnNULL = TRUE)
    if (!is.null(domdatd)) {
      if (is.null(uniqueid)) {
        stop("uniqueid must be in domdat")
      }
    }
    pltassgn <- data.table(domdat[, uniqueid, drop=FALSE], ONEUNIT = 1, ONESTRAT = 1)
    pltassgnid <- uniqueid 
    stratalut <- data.table(ONEUNIT = 1, ONESTRAT = 1, 
                            strwt = 1, 
                            n.strata = nrow(pltassgn),
                            n.total = nrow(pltassgn))
    unitvar <- "ONEUNIT"
    strvar <- "ONESTRAT"
  }
  
  ## check for condid in domdat 
  condidchk <- findnm(condid, names(domdat), returnNULL = TRUE)
  if (is.null(condidchk)) {
    domdat[[condid]] <- 1
  }
  ## check for condid in domdatd
  condidchk <- findnm(condid, names(domdatd), returnNULL = TRUE)
  if (is.null(condidchk)) {
    domdatd[[condid]] <- 1
  }
  
  ## check estvar
  estvar <- findnm(estvar, names(domdat), returnNULL = TRUE)
  if (is.null(estvar)) {
    stop(estvar, " not in domdat")
  }
  
  ## check rowvar
  if (is.null(rowvar)) {
    rowvarnm <- "TOTAL"
  }
  
  ########################################
  ## Check area units
  ########################################
  if (is.null(unitarea)) {
    totals <- FALSE
  }  
  
 
  ###################################################################################
  ## GENERATE ESTIMATES
  ###################################################################################
  estimates <- 
    getGBestimates(esttype = esttype,
                   domdatn = data.table(domdat),
                   uniqueid = uniqueid, 
                   condid = condid,
                   estvarn.name = estvar,
                   rowvar = rowvar, 
                   colvar = colvar, 
                   grpvar = c(rowvar, colvar),
                   pltassgnx = pltassgn,
                   pltassgnid = pltassgnid,
                   unitarea = unitarea,
                   unitvar = unitvar,
                   areavar = areavar,
                   stratalut = stratalut,
                   strvar = strvar,
                   strwtvar = strwtvar,
                   totals = totals,
                   sumunits = sumunits)
  if (is.null(estimates)) stop()
  unit_totest <- estimates$unit_totest
  unit_rowest <- estimates$unit_rowest
  unit_colest <- estimates$unit_colest
  unit_grpest <- estimates$unit_grpest
  rowunit <- estimates$rowunit
  totunit <- estimates$totunit
  unitvar <- estimates$unitvar
  

  
  ###################################################################################
  ## GENERATE OUTPUT TABLES
  ###################################################################################
  message("getting output...")
  estnm <- "est" 

  tabs <- 
    est.outtabs(esttype = esttype, 
                sumunits = sumunits, areavar = areavar, 
                unitvar = unitvar, unitvars = unitvars, 
                unitarea = unitarea,
                unit_totest = unit_totest, 
                unit_rowest = unit_rowest, unit_colest = unit_colest, 
                unit_grpest = unit_grpest,
                rowvar = rowvarnm, colvar = colvarnm, 
                rowgrp = rowgrp, rowgrpnm = rowgrpnm, 
                rowunit = rowunit, totunit = totunit, 
                allin1 = allin1, 
                savedata = savedata, addtitle = FALSE, 
                rawdata = TRUE, rawonly = rawonly, 
                outfn.estpse = outfn.estpse, 
                outfolder = outfolder, outfn.date = outfn.date, 
                overwrite = overwrite_layer, estnm = estnm, 
                estround = estround, pseround = pseround, 
                divideby = divideby, 
                estnull = estnull, psenull = psenull, 
                raw.keep0 = raw.keep0) 
  
  est2return <- tabs$tabest
  pse2return <- tabs$tabpse

  if (!row.add0 && any(est2return$Total == "--")) {
    est2return <- est2return[est2return$Total != "--",]
  }
  if (!is.null(est2return)) {
    if (!row.add0 && any(est2return$Total == "--")) {
      est2return <- est2return[est2return$Total != "--",]
    }
    returnlst$est <- setDF(est2return)
  }
  if (!is.null(pse2return)) {
    if (!row.add0 && any(pse2return$Total == "--")) {
      pse2return <- pse2return[pse2return$Total != "--",]
    }
    returnlst$pse <- setDF(pse2return) 
  }

  if (rawdata) {
    
    if ("unit_totest" %in% names(tabs$rawdat)) {
      
      ## Add total number of plots in population to unit_totest and totest (if sumunits=TRUE)
      UNITStot <- sort(unique(tabs$rawdat$unit_totest[[unitvar]]))
      NBRPLTtot <- stratalut[stratalut[[unitvar]] %in% UNITStot, list(NBRPLT = sum(n.strata, na.rm=TRUE)), 
                             by = unitvar]
      
      tabs$rawdat$unit_totest <- merge(tabs$rawdat$unit_totest, NBRPLTtot, by = unitvar)
      tabs$rawdat$unit_totest <- setorderv(tabs$rawdat$unit_totest, unitvar)
    }
    
    if (sumunits && "totest" %in% names(tabs$rawdat)) {
      tabs$rawdat$totest <- data.frame(tabs$rawdat$totest, NBRPLT = sum(NBRPLTtot$NBRPLT))
    }
    
    rawdat <- tabs$rawdat
    rawdat$estvar <- estvar
    
    if (savedata) {
      for (i in 1:length(rawdat)) {
        tabnm <- names(rawdat[i])
        rawtab <- rawdat[[i]]
        outfn.rawtab <- paste0(outfn.rawdat, "_", tabnm) 
        
        if (tabnm %in% c("plotsampcnt", "condsampcnt", "stratcombinelut")) {
          write2csv(rawtab, 
                    outfolder = rawoutlst$rawfolder, 
                    outfilenm = outfn.rawtab, 
			              outfn.date = outfn.date, 
			              appendfile = append_layer,
			              overwrite = overwrite_layer)
          
        } else if (is.data.frame(rawtab)) {
          if (rawoutlst$out_fmt != "csv") {
            rawoutlst$out_layer <- tabnm
          } else {
            rawoutlst$out_layer <- outfn.rawtab
          }
          datExportData(rawtab,
                        savedata_opts = rawoutlst)
        }
      }
    }
    if (!is.null(rowvar)) rawdat$rowvar <- rowvar
    if (!is.null(colvar)) rawdat$colvar <- colvar
    returnlst$raw <- rawdat
  }

  
  return(returnlst)
}
