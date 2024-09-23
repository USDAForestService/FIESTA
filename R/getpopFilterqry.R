getpopFilterqry <- function(popType, 
                            popFilter,
                            pfromqry,
                            plotnm,
                            pltassgnnm,
                            pltflds, 
                            pltassgnflds = NULL,
                            puniqueid, 
                            pjoinid,
                            pltassgnid,
                            pltassgn., 
                            plt.,
                            dbconn,
                            datsource,
                            dbTabs,
                            datindb,
                            pltaindb,
                            selectpvars,
                            pltassgnx = NULL,
                            projectid = NULL,
                            schema = NULL,
                            chkvalues = FALSE,
                            dbconnopen = TRUE) {
  ## DESCRIPTION: Creates pwhereqry for plots
  ##
  ## datindb - if TRUE, PLOT and other tables are in a database
  ## pltaindb - if TRUE, the pltassgn table is in the database
  ## ppsaindb - if TRUE, and EVALID is in popFilters, POP_PLOT_STRATUM_ASSGN is in database
  pwhereqry=ewhereqry=nonsamp.pfilter=ppsaflds=PLOT=stcntywhereqry <- NULL
  syntax <- "SQL"
  subcycle99 <- FALSE
  pflds <- unique(c(pltassgnflds, pltflds))
  
  SCHEMA. <- ""
  if (!is.null(schema)) {
    SCHEMA. <- paste0(schema, ".")
  }
  
  ## Create join for including pltassgnx
  if (!is.null(plotnm)) {
    pjoinqry <- getjoinqry(pltassgnid, pjoinid, pltassgn., plt.)
    pltafromqry <- paste0(pfromqry, 
        "\n JOIN ", SCHEMA., pltassgnnm, " plta ", pjoinqry)
  }
  
  
  ## Check states
  ##################################################################################
  states <- popFilter$states
  if (is.null(popFilter$evalid) && is.null(states)) {
    statenm <- findnm("STATECD", pltassgnflds, returnNULL=TRUE)
    statecda. <- ifelse(statenm %in% pltflds, plt., pltassgn.)
    
    if (is.null(statenm) && datindb) {
      message("STATECD is not in pltassgn...  getting all states in database...")
    }
    if (is.null(statenm) && datindb) {
        
      if (!is.null(plotnm)) {
        message("STATECD is not in pltassgn...  extracting plot data for all states in database...")
          
        getpltx.qry <- paste0(
          "\nSELECT p.CN, p.STATECD, p.COUNTYCD",
          pfromqry)
        pltxtmp <- DBI::dbGetQuery(dbconn, getpltx.qry)
        states <- unique(pltxtmp[[statenm]])
          
        countynm <- findnm("COUNTYCD", names(pltxtmp))
        if (!is.null(countynm)) {
          stcntywhereqry <- NULL
          for (i in 1: length(states)) {
            stcd <- states[i]
            cntycds <- unique(pltassgnx[pltassgnx[[statenm]] %in% stcd, countynm, with=FALSE])[[1]]
            if (length(cntycds) > 0) {
              if (!is.null(stcntywhereqry)) {
                stcntywhereqry <- paste0(stcntywhereqry, "\n  AND ")
              }
              stcntywhereqry <- paste0(stcntywhereqry, 
                  "(p.", statenm, " = ", stcd, " AND p.", countynm, " IN (", toString(cntycds), "))")
            }
          }
        } else {
          stcntywhereqry <- paste0("p.", statenm, " IN (", toString(states), ")") 
        }
      } else {
        message("STATECD is not in pltassgn...  extracting data for all states in database...")
        stcntywhereqry <- NULL
      }
    } else {
        
      states <- unique(pltassgnx[[statenm]])
      countynm <- findnm("COUNTYCD", pltassgnflds)
      countycda. <- ifelse(countynm %in% pltflds, plt., pltassgn.)
      if (!is.null(countynm)) {
        stcntywhereqry <- NULL
        for (i in 1: length(states)) {
          stcd <- states[i]
          cntycds <- unique(pltassgnx[pltassgnx[[statenm]] %in% stcd, countynm, with=FALSE])[[1]]
          if (length(cntycds) > 0) {
            if (!is.null(stcntywhereqry)) {
              stcntywhereqry <- paste0(stcntywhereqry, "\n  AND ")
            }
            stcntywhereqry <- paste0(stcntywhereqry, 
                 "(", statecda., statenm, " = ", stcd, " AND ", countycda., countynm, " IN (", toString(cntycds), "))")
          }
        }
      } else {
        stcntywhereqry <- paste0(statecda., statenm, " IN (", toString(states), ")") 
      }
    }
  }

  ##################################################################################
  ## 1. Get FIA Evaluation info
  ##################################################################################
  iseval=subcycle <- FALSE 
  returnPOP <- ifelse(pltaindb, FALSE, TRUE)
  evalInfo <- tryCatch( 
    DBgetEvalid(states = states, 
                datsource = datsource,
                invtype = "ANNUAL", 
                evalid = popFilter$evalid, 
                evalCur = popFilter$evalCur, 
                evalEndyr = popFilter$evalEndyr, 
                evalType = popType, 
                dbTabs = dbTabs,
                dbconn = dbconn,
                schema = schema,
                dbconnopen = TRUE,
                returnPOP = returnPOP),
    error = function(e) {
      message(e,"\n")
      return(NULL) })
  if (is.null(evalInfo)) {
    iseval <- FALSE
  }

  if (is.null(evalInfo)) {
    #message("no data to return")
    return(NULL)
  }
  states <- evalInfo$states
  invyrs <- evalInfo$invyrs
  invyrtab <- evalInfo$invyrtab
  popevalid <- unlist(evalInfo$evalidlist)
  if (!is.null(popevalid)) {
    iseval <- TRUE
    POP_PLOT_STRATUM_ASSGN <- evalInfo$POP_PLOT_STRATUM_ASSGN
    PLOT <- evalInfo$PLOT
    plotnm <- evalInfo$plotnm
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
    #datindb <- ifelse(!ppsaindb, FALSE, datindb) 

    ## Check popevalid pop filter in ppsa and plt
    evalidnm <- findnm("EVALID", pflds, returnNULL = TRUE)
    if (is.null(evalidnm)) {
      evalidnm <- findnm("EVALID", ppsaflds, returnNULL = TRUE)
      if (is.null(evalidnm)) {
        stop("EVALID not in data")
      } 
      evalida. <- ppsa.
      ejoinqry <- paste0("\n JOIN ", SCHEMA., ppsanm, " ppsa ON (", evalida., "PLT_CN = ", plt., puniqueid, ")")

      pfromqry <- paste0(pfromqry, 
                         ejoinqry)
      pltafromqry <- paste0(pltafromqry, 
                            ejoinqry)
      
    } else {
      evalida. <- pltassgn.
      ejoinqry <- paste0("\n JOIN ", SCHEMA., pltassgnnm, " plta ON (", evalida., "PLT_CN = ", plt., puniqueid, ")")
      pfromqry <- paste0(pfromqry, 
                         ejoinqry)
    }

    ## Check popevalid values in database
    if (chkvalues) {
      evalidqry <- paste0(
	       "SELECT DISTINCT ", evalida., evalidnm, 
           pfromqry,
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
    ewhereqry <- paste0(evalida., evalidnm, " IN (", toString(popevalid), ")")
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
        ## Build where statement for states
        statecda. <- ifelse(statenm %in% pltflds, plt., pltassgn.)
        stwhereqry <- paste0(statecda., statenm, " IN (", toString(stcds), ")")
        if (is.null(pwhereqry)) {
          pwhereqry <- paste0("\n WHERE ", stwhereqry)
        } else {
          pwhereqry <- paste0(pwhereqry, 
                            "\n  AND ", stwhereqry)
        }
      }
    } else if (!is.null(stcntywhereqry)) {
      if (is.null(pwhereqry)) {
        pwhereqry <- paste0("\n WHERE ", stcntywhereqry)
      } else {
        pwhereqry <- paste0(pwhereqry, 
                            "\n  AND ", stcntywhereqry)
      }
    }

    ## 3.3. Check subcycle, add to where statement
    ############################################################################
    if (!is.null(subcycle99) && !subcycle99) {
      subcyclenm <- findnm("SUBCYCLE", pflds, returnNULL=TRUE)
      if (is.null(subcyclenm)) {
        message("SUBCYCLE not in data... assuming all SUBCYCLE <> 99")
      } else {

        ## Build where query to include subcycle = 99
        subcyclea. <- ifelse(subcyclenm %in% pltflds, plt., pltassgn.)
        subcycle.filter <- paste0(subcyclea., subcyclenm, " <> 99")
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
        rempera. <- ifelse(rempernm %in% pltflds, plt., pltassgn.)
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
        p2vegstatusa. <- ifelse(p2vegstatusnm %in% pltflds, plt., pltassgn.)
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
        designcda. <- ifelse(designcdnm %in% pltflds, plt., pltassgn.)
        designcdqry <- paste0(
		    "SELECT DISTINCT ", designcda., designcdnm, 
              pfromqry,
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
      invyrlst.qry <- paste0("SELECT DISTINCT invyr", 
                             "\nFROM ", SCHEMA., plotnm, 
                             "\nORDER BY invyr")
      pltyrs <- DBI::dbGetQuery(dbconn, invyrlst.qry)
      
      invyrs.miss <- invyrs[which(!invyrs %in% pltyrs)]
      message("invyrs not in dataset: ", paste(invyrs.miss, collapse=", "))
      if (length(invyrs.miss) == length(invyrs)) stop("")
      invyrs <- invyrs[!invyrs %in% invyrs.miss]
    }
    
    ## Create pltidsqry
#    pltidsqry <- paste0("SELECT DISTINCT ", selectpvars,
#                       pfromqry)	   
    
    ## Add invyrs to where statement 
    invyrnm <- findnm("INVYR", pflds, returnNULL=TRUE)
    if (is.null(invyrnm)) {
      message("INVYR variable not in data")
    } else {

      ## Build where statement for INVYR
      invyra. <- ifelse(invyrnm %in% pltflds, plt., pltassgn.)
      invyr.filter <- paste0(invyra., invyrnm, " IN (", toString(invyrs), ")")
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
    if (chkvalues) {
      measyrlst.qry <- paste0(
	         "SELECT DISTINCT measyear",  
		       "\nFROM ", SCHEMA., plotnm, 
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
          pltafromqry)	   
    
    ## Add measyears to where statement 
    measyrnm <- findnm("MEASYEAR", pflds, returnNULL=TRUE)
    if (is.null(measyrnm)) {
      message("MEASYEAR variable not in data")
    } else {

      ## Build where statement for measyer
      measyr. <- ifelse(measyrnm %in% pltflds, plt., pltassgn.)
      measyears.filter <- paste0(measyr., measyrnm, " IN (", toString(measyears), ")")
      #if (syntax == 'R') measyears.filter <- gsub("IN\\(", "%in% c\\(", measyears.filter)
      if (!is.null(measyr.filter)) {
        measyr.filter <- RtoSQL(measyr.filter, x=pflds)
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
    intensity <- popFilter$intensity
    intensitynm <- findnm("INTENSITY", pflds, returnNULL = TRUE)
    if (is.null(intensitynm)) {
      message("the INTENSITY field does not exist in data set...")
      return(NULL)
    }

    intensitya. <- ifelse(intensitynm %in% pltflds, plt., pltassgn.)   
    if (chkvalues) {
      ## Check intensity values in database
      intensity.qry <- paste0(
	       "SELECT DISTINCT ", intensitya., intensitynm, 
            pfromqry,
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
      pstatuscda. <- ifelse(pstatuscdnm %in% pltflds, plt., pltassgn.)
      nonsamp.pfilter <- paste0(pstatuscda., "PLOT_STATUS_CD != 3")
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
        nfpstatuscda. <- ifelse(nfpstatuscdnm %in% pltflds, plt., pltassgn.)
        nfnonsamp.pfilter <- paste(nfpstatuscda., "NF_PLOT_STATUS_CD != 3")
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
  ## 4. Get most current plots in database
  ###################################################################################
  if (popFilter$measCur) {
    surveyfromqry <- NULL
    varCur <- "INVYR"
    
    ## Check SURVEY table
    if (!is.null(dbconn)) {
      surveynm <- findnm(dbTabs$survey_layer, DBI::dbListTables(dbconn), returnNULL = TRUE)
      if (!is.null(surveynm) && "SRV_CN" %in% pflds) {
        surveyfromqry <- paste0("\n JOIN ", SCHEMA., surveynm,
		      " survey ON (survey.CN = ", plt., "SRV_CN AND survey.ANN_INVENTORY = 'Y')")
      }
    }
    
    ## Add an Endyr to where statement
    if (!is.null(measEndyr)) {
      if (chkvalues) {
        yrlst.qry <- paste0(
		          "SELECT DISTINCT ", varCur, 
              "\nFROM ", SCHEMA., plotnm, 
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
    pgroupvars <- sapply(groupvars, findnm, pflds, returnNULL = TRUE)
    if (any(is.null(pgroupvars))) {
      missvars <- pgroupvars[is.null(pgroupvars)]
      if (length(missvars) > 1 || missvars != "unitcd") {
        warning("dataset must include statecd, countycd, and plot")
      }
    } else {
      groupvars <- as.vector(pgroupvars)
    }

    ## Create subquery
    #######################################################################
    subqry <- paste0("SELECT ", toString(paste0(plt., groupvars)), 
                     ", MAX(", plt., varCur, ") MAXYR  ",
                     pfromqry, surveyfromqry)
    if (!is.null(pwhereqry) || pwhereqry != "") {
      subqry <- paste0(subqry, pwhereqry)
    }
    subqry <- paste0(subqry,
                     "\n GROUP BY ", toString(paste0(plt., groupvars)))
    
    ## Create pltidsqry
    subjoinqry <- getjoinqry(c(groupvars, varCur), c(groupvars, "MAXYR"), alias2 = "pp.")
    pltselectqry <- paste0("SELECT DISTINCT ", toString(selectpvars))
    pltidsqry <- paste0(pltselectqry,
                        pltafromqry, 
                        "\n INNER JOIN ",
                        "\n (", subqry, ") pp ", subjoinqry)
    
  } else {

    ## Create pltidsqry
    pltselectqry <- paste0("SELECT ", toString(selectpvars))
    pltidsqry <- paste0(pltselectqry,
                        pltafromqry)
  }

  ###################################################################################
  ## 5. Add other filters in popFilter
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

  
  ###################################################################################
  ## 6. Create pltidsqry and getdataWITH queries for identifying plots
  ## pltidsqry - used as WITH statement for extracting data (if pltassgn in database)
  ## getdataWITHqry - used for extracting data (if pltassgn not in database)
  ###################################################################################
  
  
  ## Add pwhereqry to pltidsqry
  if (!is.null(pwhereqry) || pwhereqry != "") {
    pltidsqry <- paste0(pltidsqry, pwhereqry)
  }
  
  if (!is.null(dbconn) && !dbconnopen) {
    DBI::dbDisconnect(dbconn)
  }
  
  returnlst <- list(pltidsqry = pltidsqry,  
                    pltx = pltx, plotnm = plotnm,
                    states = states, invyrs = invyrs, 
                    pwhereqry = pwhereqry, 
                    pltselectqry = pltselectqry,
                    pfromqry = pfromqry,
                    pltafromqry = pltafromqry,
                    nonsamp.pfilter = nonsamp.pfilter,
                    datindb = datindb,
                    getdataWITHqry = getdataWITHqry,
                    iseval = iseval)
  if (iseval) {
    returnlst$popevalid  <- popevalid
    returnlst$POP_PLOT_STRATUM_ASSGN <- POP_PLOT_STRATUM_ASSGN
    returnlst$PLOT <- PLOT
    returnlst$ppsanm <- ppsanm
    returnlst$plotnm <- plotnm
    returnlst$ppsa. <- "ppsa."
  }
  
  return(returnlst)
}

