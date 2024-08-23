getpopFilterqry <- function(popType, 
                            popFilter,
                            pltfromqry,
                            plotnm,
                            pltassgnnm,
                            pflds, 
                            pltassgnflds = NULL,
                            puniqueid, 
                            pltassgn., 
                            plt.,
                            dbconn,
                            datsource,
                            dbTabs = dbTables(plot_layer = tabs$plt),
                            datindb,
                            pltaindb,
                            selectpvars,
                            pltassgnx = NULL,
                            pltx = NULL,
                            projectid = NULL,
                            SCHEMA. = "",
                            chkvalues = FALSE,
                            dbconnopen = TRUE) {
  ## DESCRIPTION: Creates pwhereqry for plots
  ##
  ## datindb - if TRUE, PLOT and other tables are in a database
  ## pltaindb - if TRUE, the pltassgn table is in the database
  ## ppsaindb - if TRUE, and EVALID is in popFilters, POP_PLOT_STRATUM_ASSGN is in database
  pwhereqry=ewhereqry=nonsamp.pfilter=plotsampcnt=ppsaflds <- NULL
  syntax <- "SQL"
  subcycle99 <- FALSE
  
#  getjoinqry <- function (joinid1, joinid2, alias1 = "p.", alias2 = "plta.") {
#    joinqry <- "ON ("
#    for (i in 1:length(joinid1)) {
#      joinqry <- paste0(joinqry, alias1, joinid1[i], " = ", alias2, 
#                        joinid2[i])
#      if (i == length(joinid1)) {
#        joinqry <- paste0(joinqry, ")")
#      }
#      else {
#        joinqry <- paste(joinqry, "AND ")
#      }
#    }
#    return(joinqry)
#  }
  
  ## Check states
  if (popFilter$evalCur && is.null(popFilter$states) && !is.null(pltassgnflds)) {
    statenm <- findnm("STATECD", pltassgnflds, returnNULL=TRUE)
    states.qry <- paste0(
      "\nSELECT DISTINCT ", statenm,
      "\nFROM ", pltassgnnm)
    
    if (pltaindb) {
      states <- DBI::dbGetQuery(dbconn, states.qry)[[1]]
    } else {
      states <- sqldf::sqldf(states.qry)[[1]]
    }
    popFilter$states <- states
  }
 
  ##################################################################################
  ## 1. Get FIA Evaluation info
  ##################################################################################
  iseval=subcycle <- FALSE 
  returnPOP <- ifelse(pltaindb, FALSE, TRUE)
  evalInfo <- tryCatch( 
    DBgetEvalid(states = popFilter$states, 
                datsource = datsource,
                invtype = "ANNUAL", 
                evalid = popFilter$evalid, 
                evalCur = popFilter$evalCur, 
                evalEndyr = popFilter$evalEndyr, 
                evalType = popType, 
                dbTabs = dbTabs,
                dbconn = dbconn,
                dbconnopen = TRUE,
                returnPOP = returnPOP),
    error = function(e) {
      message(e,"\n")
      return(NULL) })
  
  if (is.null(evalInfo)) {
    iseval <- FALSE
  }

#  if (is.null(evalInfo)) {
    #message("no data to return")
    #return(NULL)
#  }
  states <- evalInfo$states
  invyrs <- evalInfo$invyrs
  invyrtab <- evalInfo$invyrtab
  popevalid <- unlist(evalInfo$evalidlist)
  if (!is.null(popevalid)) {
    iseval <- TRUE
    POP_PLOT_STRATUM_ASSGN <- evalInfo$POP_PLOT_STRATUM_ASSGN
    ppsaflds <- evalInfo$ppsaflds
    ppsanm <- evalInfo$ppsanm
    ppsa. <- "ppsa."
    ppsaindb <- evalInfo$ppsaindb
  }

  ####################################################################
  ## 2. Check custom Evaluation data
  ####################################################################
  if (!iseval) {
    evalchk <- tryCatch(
        customEvalchk(states = popFilter$states, 
                      measCur = popFilter$measCur, 
                      measEndyr = popFilter$measEndyr, 
                      invyrs = popFilter$invyrs, 
                      measyrs = popFilter$measyrs,
                      invyrtab = invyrtab),
        error = function(e) {
          message(e,"\n")
          return(NULL) })
    if (!is.null(evalchk)) {
#    if (is.null(evalchk)) {
#      stop("must specify an evaluation timeframe for data extraction... \n", 
#           "...see eval_opts parameter, (e.g., eval_opts=eval_options(Cur=TRUE))")
#    }
      measCur <- evalchk$measCur
      measEndyr <- evalchk$measEndyr
      invyrs <- evalchk$invyrs
      measyears <- evalchk$measyrs
      invyrlst <- evalchk$invyrlst
      measyrlst <- evalchk$measyrlst
    }
  }

  ###################################################################################
  ## 3. Build pwhereqry
  ###################################################################################
  
  ## 3.1. Check popevalid and add to where statement (ewhereqry)
  ############################################################################
  if (!is.null(popevalid)) {
    ## If filtering with EVALID, check if POP_PLOT_STRATUM_ASSGN is in database
    datindb <- ifelse(!ppsaindb, FALSE, datindb) 
    
    ## Check popevalid pop filter in ppsa and plt
    evalidnm <- findnm("EVALID", pflds, returnNULL = TRUE)
    if (is.null(evalidnm)) {
      evalidnm <- findnm("EVALID", ppsaflds, returnNULL = TRUE)
      if (is.null(evalidnm)) {
        stop("EVALID not in data")
      } 
      evalida. <- ifelse(evalidnm %in% ppsaflds, ppsa., plt.)
      pltfromqry <- paste0(
	        pltfromqry, 
            "\n JOIN ", ppsanm, " ppsa ON (", ppsa., "PLT_CN = ", plt., puniqueid, ")")
#      pfromqry <- paste0(
#	        pfromqry, 
#            "\n JOIN ", ppsanm, " ppsa ON (", ppsa., "PLT_CN = ", plt., puniqueid, ")")
    } else {
      evalida. <- ifelse(evalidnm %in% pltassgnflds, pltassgn., plt.)	  
    }
      
    ## Check popevalid values in database
    if (chkvalues) {
      evalidqry <- paste0(
	       "SELECT DISTINCT ", evalida., evalidnm, 
           pltfromqry,
           "\nORDER BY ", evalida., evalidnm)
      if (datindb) {      
        evalidvals <- DBI::dbGetQuery(dbconn, evalidqry)[[1]]
      } else {
        evalidvals <- sqldf::sqldf(evalidqry, connection = NULL)[[1]]
      }
      evalidmiss <- popevalid[!popevalid %in% evalidvals]
      if (any(!popevalid %in% evalidvals)) {
        message("evalids are missing: ", toString(popevalid[!popevalid %in% evalidvals]))
        return(NULL)
      }
    }
     
    ## Build where query to include popevalid popfilter 
    ewhereqry <- paste0(evalida., evalidnm, " IN(", toString(popevalid), ")")
    if (is.null(pwhereqry)) {
      pwhereqry <- paste0("\n WHERE ", ewhereqry)
    } else {
      pwhereqry <- paste0(pwhereqry, 
                          "\n  AND ", ewhereqry)
    }

  } else {
    
    ## 3.2. Check states, add to where query
    ############################################################################
    if (!is.null(popFilter$states)) {
      stcds <- pcheck.states(popFilter$states, statereturn = "VALUE")
      statenm <- findnm("STATECD", pflds, returnNULL=TRUE)
      if (!is.null(statenm)) {
        statenm <- "STATECD"
      }
      stwhereqry <- paste0(plt., statenm, " IN(", toString(stcds), ")")
      if (is.null(pwhereqry)) {
        pwhereqry <- paste0("\n WHERE ", stwhereqry)
      } else {
        pwhereqry <- paste0(pwhereqry, 
                            "\n  AND ", stwhereqry)
      }
    }
    
    ## 3.3. Check subcycle, add to where statement
    ############################################################################
    if (!is.null(subcycle99) && !subcycle99) {
      subcyclenm <- findnm("SUBCYCLE", pflds, returnNULL=TRUE)
      if (is.null(subcyclenm)) {
        message("SUBCYCLE not in data... assuming all SUBCYCLE <> 99")
      } else {
        subcycle.filter <- paste0(plt., subcyclenm, " <> 99")
        if (syntax == 'R') subcycle.filter <- gsub("<>", "!=", subcycle.filter)
        if (is.null(pwhereqry)) {
          pwhereqry <- paste0("\n WHERE ", subcycle.filter)
        } else {
          pwhereqry <- paste0(pwhereqry, 
                              "\n  AND ", subcycle.filter)
        }
      }
    }

    ## 3.4. If Change Plots, remove plots that have no remeasurement data
    ######################################################################################
    if (popType %in% c("GRM", "CHNG", "LULC")) {
      rempernm <- findnm("REMPER", pflds, returnNULL = TRUE)
      if (is.null(rempernm)) {
        message("REMPER is not in dataset... assuming all remeasured plots")
      } else {
        
        ## Build where query to include remove remper NA values
        rempera. <- ifelse(rempernm %in% ppsaflds, pltassgn., plt.)	  
        remper.filter <- paste0(rempera., rempernm, " > 0")
        if (is.null(pwhereqry)) {
          pwhereqry <- paste0("\n WHERE ", remper.filter)
        } else {
          pwhereqry <- paste0(pwhereqry, 
                              "\n  AND ", remper.filter)
        }	
      }
    }
	
	  ## 3.5. If P2VEG Plots, remove plots that have no sampled P2VEG data
    ######################################################################################
    if (popType == "P2VEG") {
      p2vegstatusnm <- findnm("P2VEG_SAMPLING_STATUS_CD", pflds, returnNULL = TRUE)
      if (is.null(p2vegstatusnm)) {
        message("P2VEG_SAMPLING_STATUS_CD is not in dataset... assuming all plots sample P2VEG")
      } else {
        
        ## Build where query to remove plots that didn't sample P2VEG
        p2vegstatusa. <- ifelse(p2vegstatusnm %in% ppsaflds, pltassgn., plt.)	  
        p2vegstatus.filter <- paste0(p2vegstatusa., p2vegstatusnm, " < 3")
        if (is.null(p2whereqry)) {
          pwhereqry <- paste0("\n WHERE ", p2vegstatus.filter)
        } else {
          pwhereqry <- paste0(pwhereqry, 
                              "\n AND ", p2vegstatus.filter)
        }	
      }
    }

    
    ## 3.6. Check designcd in ppsa and plt
    #######################################################################
    if (chkvalues) {
      designcdnm <- findnm("DESIGNCD", pflds, returnNULL = TRUE)
      if (is.null(designcdnm)) {
        message("DESIGNCD is not in dataset... assuming one plot design")
      } else {
        
        ## Check designcd values in database
        designcda. <- ifelse(designcdnm %in% ppsaflds, pltassgn., plt.)	  
        designcdqry <- paste0(
		    "SELECT DISTINCT ", designcda., designcdnm, 
            pltfromqry,
            pwhereqry,
            "\nORDER BY ", designcda., designcdnm)
        if (pltaindb) {      
          designcdvals <- DBI::dbGetQuery(dbconn, designcdqry)[[1]]
        } else {
          designcdvals <- sqldf::sqldf(designcdqry, connection = NULL)[[1]]
        }
        
        if (length(designcdvals) > 1) {
          if (any(!designcdvals %in% c(1, 501:505, 230:242, 311:323, 328))) {
            if (adj == "samp") {
              message("designcds include: ", toString(designcdvals))
              message("samp adjustment for trees is only for annual inventory designs... see FIA database manual")
            } else {
              warning("more than 1 plot design, calculate separate estimates by design")
            }
          }
        }
      }
    }
  }

  ## 3.7. Check invyrs and add to where query. 
  ############################################################################
  if (!is.null(popFilter$invyrs)) {
    
    #print(invyrs)
    if (chkvalues) {
      invyrlst.qry <- paste("SELECT DISTINCT invyr \nFROM", plotnm, "\nORDER BY invyr")
      pltyrs <- DBI::dbGetQuery(dbconn, invyrlst.qry)
      
      invyrs.miss <- invyrs[which(!invyrs %in% pltyrs)]
      message("invyrs not in dataset: ", paste(invyrs.miss, collapse=", "))
      if (length(invyrs.miss) == length(invyrs)) stop("")
      invyrs <- invyrs[!invyrs %in% invyrs.miss]
    }
    
    ## Create pltidsqry
#    pltidsqry <- paste0("SELECT DISTINCT ", selectpvars,
#                       pltfromqry)	   
    
    ## Add invyrs to where statement 
    invyrnm <- findnm("INVYR", pltflds, returnNULL=TRUE)
    if (is.null(invyrnm)) {
      message("INVYR variable not in data")
    } else {
      invyr.filter <- paste0(plt., invyrnm, " IN(", toString(invyrs), ")")
      #if (syntax == 'R') invyr.filter <- gsub("IN\\(", "%in% c\\(", invyr.filter)
      if (is.null(pwhereqry)) {
        pwhereqry <- invyr.filter
      } else {
        pwhereqry <- paste(paste(pwhereqry, invyr.filter, sep=" AND "))
      }
    }
    
  } else if (!is.null(popFilter$measyrs)) {
    
    ## 3.8. Check measyear and add to where query.
    ############################################################################
    if (chk) {
      measyrlst.qry <- paste(
	       "SELECT DISTINCT measyear",  
		   "\nFROM", plotnm, 
		   "\nORDER BY measyear")
      pltyrs <- DBI::dbGetQuery(dbconn, measyrlst.qry)
      
      measyr.miss <- measyears[which(!measyears %in% pltyrs)]
      message("measyears not in dataset: ", paste(measyr.miss, collapse=", "))
      if (length(measyr.miss) == length(measyears)) stop("")
      measyears <- measyears[!measyears %in% measyr.miss]
    }
    
    ## Build pltidsqry
    pltselectqry <- paste0("SELECT DISTINCT ", toString(selectpvars))
    pltidsqry <- paste0(
          pltselectqry,
          pltfromqry)	   
    
    ## Add measyears to where statement 
    measyrnm <- findnm("MEASYEAR", pltflds, returnNULL=TRUE)
    if (is.null(measyrnm)) {
      message("MEASYEAR variable not in data")
    } else {
      measyears.filter <- paste0(plt., measyrnm, " IN(", toString(measyears), ")")
      #if (syntax == 'R') measyears.filter <- gsub("IN\\(", "%in% c\\(", measyears.filter)
      if (!is.null(measyr.filter)) {
        measyr.filter <- RtoSQL(measyr.filter, x=pltflds)
        measyears.filter <- paste0(measyears.filter, " AND ", measyr.filter)
      }
      if (is.null(pwhereqry)) {
        pwhereqry <- measyears.filter
      } else {
        pwhereqry <- paste(paste(pwhereqry, measyears.filter, sep=" AND "))
      }
    }
    
    ## Add pwhereqry to pltidsqry
#    if (!is.null(pwhereqry) || pwhereqry != "") {
#      pltidsqry <- paste0(pltidsqry, pwhereqry)
#    }
  }   

  ## 3.9 Check INTENSITY and add to where query.
  ########################################################################
  if (!is.null(popFilter$intensity)) { 	   
    intensitynm <- findnm("INTENSITY", pflds, returnNULL = TRUE)
    intensitya. <- ifelse(intensitynm %in% ppsaflds, pltassgn., plt.)	  
    if (is.null(intensitynm)) {
      message("the INTENSITY field does not exist in data set...")
      return(NULL)
    }
    
    if (chkvalues) {
      ## Check intensity values in database
      intensity.qry <- paste0(
	       "SELECT DISTINCT ", intensitya., intensitynm, 
            pltfromqry,
            pwhereqry,
           "\nORDER BY ", intensitya., intensitynm)
      if (datindb) {      
        intensityvals <- DBI::dbGetQuery(dbconn, intensity.qry)[[1]]
      } else {
        intensityvals <- sqldf::sqldf(intensityqry, connection = NULL)[[1]]
      }
      intensitymiss <- intensity[!intensity %in% intensityvals]
      if (any(!intensity %in% intensityvals)) {
        message("intensity are missing: ", toString(intensity[!intensity %in% intensityvals]))
        return(NULL)
      }
    }
    
    ## Build where query to include invyr popfilter 
    iwhere.qry <- paste0(intensitya., intensitynm, " IN(", toString(intensity), ")")
    if (is.null(pwhereqry)) {
      pwhereqry <- paste0("\n WHERE ", iwhere.qry)
    } else {
      pwhereqry <- paste0(pwhereqry, 
                          "\n  AND ", iwhere.qry)
    }
  }

  ## 3.10. Check PLOT_STATUS_CD and generate table with number of plots
  ########################################################################
  pstatusvars <- c("PLOT_STATUS_CD", "PSTATUSCD")
  pstatuschk <- unlist(sapply(pstatusvars, findnm, pflds, returnNULL=TRUE))
  if (is.null(pstatuschk)) {
    message("PLOT_STATUS_CD not in dataset.. assuming all plots are at least ",
            "partially sampled")
  } else {
    if (length(pstatuschk) > 1) {
      pstatuscdnm <- pstatuschk[1]
    } else {
      pstatuscdnm <- pstatuschk
    }  
    ## Create nonsamp.pfilter
    if (!is.null(pstatuscdnm) && (is.null(nonsamp.pfilter) || nonsamp.pfilter == "")) {
      nonsamp.pfilter <- paste0(plt., "PLOT_STATUS_CD != 3")
      message("removing nonsampled forest plots...")
    }
  } 

  ## If ACI, check NF_PLOT_STATUS_CD and generate table with number of plots
  ##########################################################################
  if (popFilter$ACI) {
    nfpstatusvars <- c("NF_PLOT_STATUS_CD", "PSTATUSNF")
    nfpstatuschk <- unlist(sapply(nfpstatusvars, findnm, pflds, returnNULL=TRUE))
    if (is.null(nfpstatuschk)) {
      message("NF_PLOT_STATUS_CD not in dataset.. assuming all ACI nonforest plots are at least ",
              "partially sampled")
    } else {
      if (length(nfpstatuschk) > 1) {
        nfpstatuscdnm <- nfpstatuschk[1]
      } else {
        nfpstatuscdnm <- nfpstatuschk
      }  
      ## Create nonsamp.pfilter
      if (!is.null(nfpstatuscdnm) && (is.null(nonsamp.pfilter) || nonsamp.pfilter == "")) {
        nfnonsamp.pfilter <- paste(plt., "NF_PLOT_STATUS_CD != 3")
      }
      if (!is.null(nonsamp.pfilter)) {
        nonsamp.pfilter <- paste0(nonsamp.pfilter, " AND ", nfnonsamp.pfilter)
      } else {
        nonsamp.pfilter <- nfnonsamp.pfilter
      }  
      message("removing nonsampled nonforest plots...")
    }
  }

  ## Add plot_status_cd to where statement
  ##########################################################################
  if (popType != "All" && !is.null(nonsamp.pfilter) && is.null(popevalid)) {
    if (is.null(pwhereqry)) {
      pwhereqry <- paste0("\n WHERE ", nonsamp.pfilter)
    } else {
      pwhereqry <- paste0(pwhereqry, " AND ", nonsamp.pfilter)
    } 
  }   

#  ## Add projectid and popType to selectqry
#  if (!is.null(projectid)) {
#    selectidvars <- paste0("'", projectid, "' AS PROJECTID, '", popType, "' AS POP_TYP, ")
#  } else {
#    selectidvars <- paste0("'", popType, "' AS POP_TYP, ")
#  }
  
  ###################################################################################
  ## Get most current plots in database
  ###################################################################################
  if (popFilter$measCur) {
    surveyfromqry <- NULL
    varCur <- "INVYR"
    
    ## Check SURVEY table
    if (!is.null(dbconn)) {
      surveynm <- findnm(dbTabs$survey_layer, DBI::dbListTables(dbconn), returnNULL = TRUE)
      if (!is.null(surveynm) && "SRV_CN" %in% pltflds) {
        surveyfromqry <- paste0("\n JOIN ", SCHEMA., surveynm,
		      " survey ON (survey.CN = ", plt., "SRV_CN AND survey.ANN_INVENTORY = 'Y')")
      }
    }
    
    ## Add an Endyr to where statement
    if (!is.null(measEndyr)) {
      if (chkvalues) {
        yrlst.qry <- paste0(
		    "SELECT DISTINCT", varCur, 
            "\nFROM", plotnm, 
            "\nORDER BY ", varCur)
        pltyrs <- DBI::dbGetQuery(dbconn, yrlst.qry)
        
        if (measEndyr <= min(pltyrs, na.rm=TRUE)) {
          message(measEndyr, " is less than minimum year in dataset")
          return(NULL)
        }
      }
      Endyr.filter <- paste0(plt., varCur, " <= ", measEndyr)
      if (is.null(pwhereqry)) {
        pwhereqry <- Endyr.filter
      } else {
        pwhereqry <- paste(paste(pwhereqry, Endyr.filter, sep="\n   AND "))
      }
    }
    
    ## Define group variables
    groupvars <- c("STATECD", "UNITCD", "COUNTYCD", "PLOT")
    if (!is.null(pltflds)) {
      pgroupvars <- sapply(groupvars, findnm, pltflds, returnNULL = TRUE)
      if (any(is.null(pgroupvars))) {
        missvars <- pgroupvars[is.null(pgroupvars)]
        if (length(missvars) > 1 || missvars != "unitcd") {
          warning("dataset must include statecd, countycd, and plot")
        }
      } else {
        groupvars <- as.vector(pgroupvars)
      }
    }

    ## Create subquery
    #######################################################################
    subqry <- paste0("SELECT ", toString(paste0(plt., groupvars)), 
                     ", MAX(", plt., varCur, ") MAXYR  ",
                     pltfromqry, surveyfromqry)
    if (!is.null(pwhereqry) || pwhereqry != "") {
      subqry <- paste0(subqry, pwhereqry)
    }
    subqry <- paste0(subqry,
                     "\n GROUP BY ", toString(paste0(plt., groupvars)))
    
    ## Create pltidsqry
    subjoinqry <- getjoinqry(c(groupvars, varCur), c(groupvars, "MAXYR"), alias2 = "pp.")
    pltselectqry <- paste0("SELECT DISTINCT ", toString(selectpvars))
    pltidsqry <- paste0(pltselectqry,
                        pltfromqry, 
                        "\n INNER JOIN ",
                        "\n (", subqry, ") pp ", subjoinqry)
    
  } else {

    ## Create pltidsqry
    pltselectqry <- paste0("SELECT ", toString(selectpvars))
    pltidsqry <- paste0(pltselectqry,
                        pltfromqry)
  }

  ###################################################################################
  ## Get most current plots in database
  ###################################################################################
  if (!is.null(popFilter$pfilter)) {
    
    pfilter <- popFilter$pfilter

    if (!is.null(pltassgnflds)) {
      pfilter <- tryCatch(
        check.logic(pltassgnflds, pfilter, syntax="SQL", filternm="pfilter"),
        error = function(e) {
          return(NULL) })
    
    
      if (is.null(pfilter)) {
        pfilter <- tryCatch(
          check.logic(pflds, pfilter, syntax="SQL", filternm="pfilter"),
          error = function(e) {
            return(NULL) })
        if (length(pfiltervars) == 0) {
          stop("pfilter is invalid: ", pfilter)
        }
      } 
    }
    if (!is.null(pfilter)) {
      if (is.null(pwhereqry)) {
        pwhereqry <- pfilter
      } else {
        pwhereqry <- paste(paste(pwhereqry, pfilter, sep="\n   AND "))
      }
    }
  }

  ## Add pwhereqry to pltidsqry
  if (!is.null(pwhereqry) || pwhereqry != "") {
    pltidsqry <- paste0(pltidsqry, pwhereqry)
  }
  
  
  if (!is.null(dbconn) && !dbconnopen) {
    DBI::dbDisconnect(dbconn)
  }
  
  returnlst <- list(pltidsqry = pltidsqry,  
                    states = states, invyrs = invyrs, 
                    pwhereqry = pwhereqry, ewhereqry = ewhereqry, 
                    #pfromqry = pfromqry, 
                    pltselectqry = pltselectqry,
                    pltfromqry = pltfromqry,
                    nonsamp.pfilter = nonsamp.pfilter,
                    datindb = datindb)
  if (!is.null(plotsampcnt)) {
    returnlst$plotsampcnt <- plotsampcnt
  }
  if (iseval) {
    returnlst$popevalid  <- popevalid
    returnlst$POP_PLOT_STRATUM_ASSGN <- POP_PLOT_STRATUM_ASSGN
    returnlst$ppsanm <- ppsanm
    ppsa. <- "ppsa."
  }
  
  return(returnlst)
}

