getpopFilterqry <- function(popType,
                            popFilter,
                            pltassgnnm,
                            pltassgnflds,
                            pltassgnid,
                            plotnm = NULL,
                            pltflds = NULL,
                            puniqueid = NULL,
                            pjoinid = NULL,
                            plt. = NULL,
                            dbconn,
                            schema = NULL,
                            datsource,
                            dbTabs,
                            datindb,
                            pltaindb,
                            pltassgnvars,
                            selectpvars = NULL,
                            pltassgnx = NULL,
                            pltx = NULL,
                            projectid = NULL,
                            adj = "samp",
                            chkvalues = FALSE) {
  ## DESCRIPTION: Creates pwhereqry for plots.
  ## datindb - if TRUE, PLOT and other tables are in a database
  ## pltaindb - if TRUE, the pltassgn table is in the database
  ## ppsaindb - if TRUE, and EVALID is in popFilters, POP_PLOT_STRATUM_ASSGN is in database
  pwhereqry=pltawhereqry=ewhereqry=nonsamp.pfilter=ppsaflds=PLOT=stcntywhereqry=
    POP_PLOT_STRATUM_ASSGN=pfromqry <- NULL
  popevalid=invyrs <- NULL
  syntax <- "SQL"
  subcycle99 <- FALSE
  pflds <- unique(c(pltassgnflds, pltflds))
  SCHEMA. <- ""

  if (!is.null(dbconn)) {
    if (!is.null(schema)) {
      SCHEMA. <- paste0(schema, ".")
    }
    dbtables <- DBI::dbListTables(dbconn)
  }

  ## 1. Create FROM statement or JOIN for including pltassgnx (pltafromqry).
  ##################################################################################
  noplt <- TRUE
  pltassgn. <- "plta."
  
  ## If there is a plot table, join with pltassgn
  if (!is.null(plotnm)) {
    noplt <- FALSE
    pfromqry <- paste0("\nFROM ", SCHEMA., plotnm, " p ")
    pjoinqry <- getjoinqry(pltassgnid, pjoinid, pltassgn., plt.)
    pltafromqry <- paste0(pfromqry,
                          "\n JOIN ", SCHEMA., pltassgnnm, " plta ", pjoinqry)
    
  } else {
    pltafromqry <- paste0("\nFROM ", SCHEMA., pltassgnnm, " plta ")
  }
  
  
  if (noplt) {
    countynm <- findnm("COUNTYCD", pltassgnflds, returnNULL=TRUE)
    countycda. <- pltassgn.
    
  } else {
    countynm <- findnm("COUNTYCD", pltflds, returnNULL=TRUE)
    if (is.null(countynm)) {
      countynm <- findnm("COUNTYCD", pltassgnflds, returnNULL=TRUE)
      countycda. <- ifelse(countynm %in% pltassgnflds, pltassgn., plt.)
    } else {
      countycda. <- ifelse(countynm %in% pltflds, plt., pltassgn.)
    }
  }


  ## 2. Check states for subsetting data if evalid and state are not in popFilter
  ## If evalid and state are not in popFilter, there is no way of subsetting 
  ## data if using a database with all states included. This gets the states 
  ## (and counties) that are in pltassgn to subset the data before bringing into 
  ## R memory. It is used in the WHERE statement in the queries (stcntywhereqry).
  ##################################################################################
  states <- popFilter$states
  pfilter <- popFilter$pfilter

  if (is.null(popFilter$evalid) && is.null(states)) {
    querydb <- TRUE
    
    ## if there is no plot table (noplt = TRUE), look for statecd and countycd in pltassgn
    ## if there is a plot table, look there first, otherwise look in pltassgn
    if (noplt) {
      statenm <- findnm("STATECD", pltassgnflds, returnNULL=TRUE)
      statecda. <- pltassgn.
      
      ## build FROM query for checking statecd
      if (!is.null(statenm)) {
        statecda. <- pltassgn.
        pchkfromqry <- pltafromqry
        
        countynm <- findnm("COUNTYCD", pltassgnflds, returnNULL=TRUE)
        if (!is.null(countynm)) {
          countycda. <- pltassgn.
        }
        
        if (!pltaindb) querydb <- FALSE
      }
      
    } else {
      statenm <- findnm("STATECD", pflds, returnNULL=TRUE)
      if (!is.null(statenm)) {
        statecda. <- ifelse (statenm %in% pltflds, plt., pltassgn.)
        
        countynm <- findnm("COUNTYCD", pltflds, returnNULL=TRUE)
        if (!is.null(countynm)) {
          countycda. <- plt.
        }
      }
      
      ## build FROM query for checking for variables
      if ((pltaindb && datindb) || (!pltaindb && !datindb)) {
        pchkfromqry <- pltafromqry
        if (!pltaindb)querydb <- FALSE
        
      } else if (statenm %in% pltflds) {
        pchkfromqry <- paste0("\nFROM ", SCHEMA., plotnm, " p ")
        if (!datindb) querydb <- FALSE
        
      } else {
        pchkfromqry <- paste0("\nFROM ", SCHEMA., pltassgnnm, " plta ")
        if (!pltaindb) querydb <- FALSE
      }
    }
    
    if (!is.null(statenm)) {
      stcntywhereqry <- NULL
      
      ## build query to get unique states, counties
      stselect.qry <- paste0("\nSELECT DISTINCT ", statecda., "statecd")
      if (!is.null(countynm)) {
        stselect.qry <- paste0(stselect.qry, ", ", countycda., "countycd")
      }
      stcnty.qry <- paste0(
        stselect.qry,
        pchkfromqry)
      
      if (!is.null(pfilter)) {
        stcnty.qry <- paste(stcnty.qry, 
                            "\nWHERE ", pfilter)
      }
      
      if (querydb) {
        stcnty <- DBI::dbGetQuery(dbconn, stcnty.qry)
      } else {
        stcnty <- sqldf::sqldf(stcnty.qry)
      }
      
      if (!is.null(countynm)) {
        states <- unique(stcnty[[statenm]])
        
        for (i in 1: length(states)) {
          stcd <- states[i]
          cntycds <- unique(stcnty[stcnty[[statenm]] %in% stcd, countynm])
          if (length(cntycds) > 0) {
            if (!is.null(stcntywhereqry)) {
              stcntywhereqry <- paste0(stcntywhereqry, "\n  OR ")
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
    }
  }
  
  
  ##################################################################################
  ## 3. Check FIA Evaluation information using DBgetEvalid().
  ##################################################################################
  iseval=subcycle <- FALSE
  returnPOP <- ifelse(pltaindb, FALSE, TRUE)
  
  if (!is.null(dbconn) && popFilter$evalCur &&
      !is.null(findnm("DATAMART_MOST_RECENT_INV", dbtables, returnNULL = TRUE))) {

    if (!is.numeric(states)) {
      states <- FIESTAutils::pcheck.states(states, statereturn="VALUE")
    }
    #states <- c(6,8,16,30,32,41,49,53,56)
    evalgrps.qry <-
      paste0("SELECT eval_grps FROM datamart_most_recent_inv WHERE statecd IN (",
               toString(states), ")")
    evalgrps <- DBI::dbGetQuery(dbconn, evalgrps.qry)[[1]]
    evalpre <- substr(evalgrps, 1, nchar(evalgrps) - 4)
    evalyr <- substr(evalgrps, nchar(evalgrps) - 1, nchar(evalgrps))
    evalend <- ifelse (popType == "CHNG", "03", "01")
    popevalid <- paste0(evalpre, evalyr, evalend)
    iseval <- TRUE

    ppsanm <- findnm("POP_PLOT_STRATUM_ASSGN", dbtables, returnNULL = TRUE)
    if (is.null(ppsanm)) {
      stop("need to include pop_plot_stratum_assgn in database")
    }
    ppsaflds <- dbgetflds(conn = dbconn, schema = schema, tabnm = ppsanm, upper = TRUE)
    ppsa. <- "ppsa."

    ## Get inventory years from pop_plot_stratum_assgn
    invyrnm <- findnm("INVYR", ppsaflds, returnNULL  = TRUE)
    statecdnm <- findnm("STATECD", ppsaflds, returnNULL  = TRUE)

    if (!is.null(invyrnm) && !is.null(statecdnm)) {
      invyrs.qry <- paste0("SELECT DISTINCT ", toString(c(statecdnm, invyrnm)),
                           "\nFROM pop_plot_stratum_assgn",
                           "\nWHERE evalid IN (", toString(popevalid), ")")
      invyrsdf <- DBI::dbGetQuery(dbconn, invyrs.qry)
      invyrs <- sort(unique(invyrsdf[[invyrnm]]))

      if (length(invyrs) == 0) {
        message("no data in database for ", toString(states))
        return(NULL)
      } else {

        stcds <- sort(unique(invyrsdf[[statecdnm]]))
        if (length(stcds) < length(states)) {
          statemiss <- states[!states %in% stcds]

          message("no data in database for ", toString(pcheck.states(statemiss)))
          states <- states[!states %in% statemiss]

          for (st in statemiss) {
            popevalid <- popevalid[!startsWith(popevalid, as.character(st))]
          }
        }
      }
    }

  } else if (datsource == "obj" ||
             any(!is.null(popFilter$evalid), popFilter$evalCur, !is.null(popFilter$evalEndyr))) {

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
                database_opts = database_options(schema = schema),
                returnPOP = returnPOP),
      error = function(e) {
        message(e,"\n")
        return(NULL) })
    if (is.null(evalInfo)) {
      message("error getting evaluation info...")
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
    ## 4. Check custom Evaluation data
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
        measCur <- evalchk$measCur
        measEndyr <- evalchk$measEndyr
        measyears <- evalchk$measyrs
        invyrlst <- evalchk$invyrlst
        measyrlst <- evalchk$measyrlst
        if (!is.null(evalchk$invyrs)) {
          invyrs <- evalchk$invyrs
        }
      }
    }
  } 
  
  ###################################################################################
  ## 5. Build WHERE statement (pltawhereqry and pwhereqry)
  ###################################################################################

  ## 5.1. Check popevalid and add to WHERE statement (ewhereqry)
  ############################################################################
  if (!is.null(popevalid) || !is.null(popFilter$evalid)) {
    ## If filtering with EVALID, check if POP_PLOT_STRATUM_ASSGN is in database
    #datindb <- ifelse(!ppsaindb, FALSE, datindb)
    
    
    ## Note:
    ## if you include multiple evaluations (e.g., 81901, 82101) in your population data 
    ## that have more than one subpopulation (i.e., estimation unit; e.g., counties),
    ## and you have set sumunits = TRUE, you will get a sum of all the subpopulations across all the evaluations.
    ## Therefore, if you have overlapping subpopulations in your population dataset, you will get erroneous results.
    ## Try running one at a time or using the anGBetpop_evallst from FIESTAanalysis 
    ## devtools::source_url('https://raw.githubusercontent.com/USDAForestService/FIESTAnalysis/refs/heads/main/R/anGBetpop_evallst.R')
    
    
    
    ## Check evalids for multiple states
    evalstates <- sapply(popevalid, function(x) substr(x, 1, nchar(x)-4))
    if (any(duplicated(evalstates))) {
      dupstates <- evalstates[duplicated(evalstates)]
      message()
      message("duplicate states found in evaluations... ", toString(names(dupstates)))
      message("set unit_opts = unitvar_options(unitvar2 = 'EVALID')\n")
      
      ## Check evalids for multiple years
      evalyears <- sapply(popevalid, function(x) substr(x, nchar(x)-3, nchar(x)-2))
      dupyears <- evalyears[duplicated(evalyears)]
      
      if (length(dupyears) > 1) {
      message("Note:  
       If you include multiple evaluations (e.g., 81901, 82101) in your population
       that have more than one subpopulation (i.e., estimation unit; e.g., counties) and
       sumunits = TRUE, you will get a sum of all the subpopulations across all evaluations.
       Therefore, if your subpopulations overlap, you may get erroneous results.
       Try running the evaluations one at a time or use the anGBgetpop_evallst from FIESTAanalysis
       (devtools::source_url('https://raw.githubusercontent.com/USDAForestService/FIESTAnalysis/refs/heads/main/R/anGBetpop_evallst.R')\n")
      }
    }
    

    ## Check popevalid pop filter in ppsa and plt
    evalidnm <- findnm("EVALID", pflds, returnNULL = TRUE)
    if (is.null(evalidnm)) {
      evalidnm <- findnm("EVALID", ppsaflds, returnNULL = TRUE)
      if (is.null(evalidnm)) {
        stop("EVALID not in data")
      }
      evalida. <- ppsa.
      if (!is.null(plt.)) {
        ejoinqry <- paste0("\n JOIN ", SCHEMA., ppsanm, " ppsa ON (", evalida., "PLT_CN = ", plt., puniqueid, ")")
      } else {
        ejoinqry <- paste0("\n JOIN ", SCHEMA., ppsanm, " ppsa ON (", evalida., "PLT_CN = ", pltassgn., pltassgnid, ")")
      }
      if (!is.null(pfromqry)) {
        pfromqry <- paste0(pfromqry,
                           ejoinqry)
      }
      pltafromqry <- paste0(pltafromqry,
                            ejoinqry)

    } else {
      evalida. <- pltassgn.
      
#      if (!is.null(plt.)) {
#        ejoinqry <- paste0("\n JOIN ", SCHEMA., pltassgnnm, " plta ON (", evalida., "PLT_CN = ", plt., puniqueid, ")")
#      } else {
#        ejoinqry <- paste0("\n JOIN ", SCHEMA., pltassgnnm, " plta ON (", evalida., "PLT_CN = ", pltassgn., pltassgnid, ")")
#      }
#      pltafromqry <- paste0(pltafromqry,
#                            ejoinqry)
    }
    
    if (is.null(evalidnm)) {
      stop("there is no EVALID variable in data")
    } else {
      if (is.null(popevalid)) popevalid <- popFilter$evalid
      pltassgnvars <- unique(c(pltassgnvars, evalidnm))
    }
    

    ## Check popevalid values in database
    if (chkvalues) {
      evalidqry <- paste0(
        "SELECT DISTINCT ", evalida., evalidnm,
        pltafromqry,
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

    ## 5.2. Check states, add to where query
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
    } else if (!is.null(stcntywhereqry) && !noplt) {
      if (is.null(pwhereqry)) {
        pwhereqry <- paste0("\n WHERE ", stcntywhereqry)
      } else {
        pwhereqry <- paste0(pwhereqry,
                            "\n  AND ", stcntywhereqry)
      }
    }

    ## 5.3. Check subcycle, add to where statement
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
        if (subcyclea. == pltassgn.) {
          pltassgnvars <- c(pltassgnvars, subcyclenm)
        }
      }
    }

    ## 5.4. If Change Plots, remove plots that have no remeasurement data
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
        if (rempera. == pltassgn.) {
          pltassgnvars <- c(pltassgnvars, rempernm)
        }
      }
    }

    ## 5.5. If P2VEG Plots, remove plots that have no sampled P2VEG data
    ######################################################################################
    if (popType == "P2VEG") {
      p2vegstatusnm <- findnm("P2VEG_SAMPLING_STATUS_CD", pflds, returnNULL = TRUE)
      if (is.null(p2vegstatusnm)) {
        message("P2VEG_SAMPLING_STATUS_CD is not in dataset... assuming all plots sample P2VEG")
      } else {

        ## Build where query to remove plots that didn't sample P2VEG
        p2vegstatusa. <- ifelse(p2vegstatusnm %in% pltflds, plt., pltassgn.)
        p2vegstatus.filter <- paste0(p2vegstatusa., p2vegstatusnm, " < 3")
        if (is.null(pwhereqry)) {
          pwhereqry <- paste0("\n WHERE ", p2vegstatus.filter)
        } else {
          pwhereqry <- paste0(pwhereqry,
                              "\n AND ", p2vegstatus.filter)
        }
        if (p2vegstatusa. == pltassgn.) {
          pltassgnvars <- c(pltassgnvars, p2vegstatusnm)
        }
      }
    }


    ## 5.6. Check designcd in ppsa and plt
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
          pltafromqry,
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
  

  ## 5.7. Check invyrs and add to where query.
  ############################################################################
  if (!is.null(popFilter$invyrs)) {
    invyrs <- popFilter$invyrs
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
      if (invyra. == pltassgn.) {
        pltassgnvars <- unique(c(pltassgnvars, invyrnm))
      }
    }

  } else if (!is.null(popFilter$measyrs)) {

    ## 5.8. Check measyear and add to where query.
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
      if (measyr. == pltassgn.) {
        pltassgnvars <- c(pltassgnvars, measyrnm)
      }
    }

    ## Add pwhereqry to pltidsqry
    #    if (!is.null(pwhereqry) || pwhereqry != "") {
    #      pltidsqry <- paste0(pltidsqry, pwhereqry)
    #    }
  }

  ## 5.9 Check INTENSITY and add to where query.
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
        pltafromqry,
        pwhereqry,
        "\nORDER BY ", intensitya., intensitynm)
      if (datindb) {
        intensityvals <- DBI::dbGetQuery(dbconn, intensity.qry)[[1]]
      } else {
        intensityvals <- sqldf::sqldf(intensity.qry, connection = NULL)[[1]]
      }
      intensitymiss <- intensity[!intensity %in% intensityvals]
      if (any(!intensity %in% intensityvals)) {
        message("intensity are missing: ", toString(intensity[!intensity %in% intensityvals]))
        return(NULL)
      }
    }

    ## Build where query to include invyr popfilter
    iwhere.qry <- paste0(intensitya., intensitynm, " IN (", toString(intensity), ")")
    if (is.null(pwhereqry)) {
      pwhereqry <- paste0("\n WHERE ", iwhere.qry)
    } else {
      pwhereqry <- paste0(pwhereqry,
                          "\n  AND ", iwhere.qry)
    }
    if (intensitya. == pltassgn.) {
      pltassgnvars <- c(pltassgnvars, intensitynm)
    }
  }

  ## 5.10. Check PLOT_STATUS_CD and create filter for excluding nonsampled plots
  ##       (nonsamp.pfilter)
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
    if (pstatuscda. == pltassgn.) {
      pltassgnvars <- c(pltassgnvars, pstatuscdnm)
    }
  }

  ## 5.11. If ACI, check NF_PLOT_STATUS_CD and add to nonsamp.filter
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
      if (!is.null(nfpstatuscdnm)) {
        nfpstatuscda. <- ifelse(nfpstatuscdnm %in% pltflds, plt., pltassgn.)
        nfnonsamp.pfilter <- paste0(nfpstatuscda., "NF_PLOT_STATUS_CD != 3")

        if (nfpstatuscda. == pltassgn.) {
          pltassgnvars <- c(pltassgnvars, nfpstatuscdnm)
        }
        if (!is.null(nonsamp.pfilter)) {
          nonsamp.pfilter <- paste0(nonsamp.pfilter, " OR ", nfnonsamp.pfilter)
        } else {
          nonsamp.pfilter <- nfnonsamp.pfilter
        }
        message("and removing nonsampled nonforest plots for ACI estimates...")
        message(nonsamp.pfilter)
      }
    }
  }

  ## 9.5.12. Add nonsamp.pfilter to WHERE statement
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
  ## 6. Add other filters in popFilter
  ###################################################################################
  if (!is.null(popFilter$pfilter)) {
    pfilter <- popFilter$pfilter

    if (!is.null(pltassgnflds)) {
      pfilterchk <- tryCatch(
        suppressMessages(check.logic(pltassgnflds, pfilter,
                                     syntax="SQL", filternm="pfilter", stopifinvalid = FALSE)),
        error = function(e) {
          return(NULL) })
      if (is.null(pfilterchk)) {
        pfilterchk <- tryCatch(
          check.logic(pflds, pfilter, syntax="SQL", filternm="pfilter"),
          error = function(e) {
            return(NULL) })
        if (length(pfilterchk) == 0) {
          message(pfilter)
          stop()
        }
        pflds_match <- pflds[sapply(pflds, grepl, pfilter)]
        if (length(pflds_match) == 1) {
          pfilter <- sub(pflds_match, paste0("p.", pflds_match), pfilter)
        }
      } else {
        ppsaflds_match <- pltassgnflds[sapply(pltassgnflds, grepl, pfilter)]
        if (length(ppsaflds_match) == 1) {
          pfilter <- sub(ppsaflds_match, paste0(pltassgn., ppsaflds_match), pfilter)
        }
      }
    }
    
    ## Add filter for pfilter to pwhereqry
    if (!is.null(pfilter)) {
      if (is.null(pwhereqry)) {
        pwhereqry <- pfilter
      } else {
        pwhereqry <- paste(paste(pwhereqry, pfilter, sep="\n   AND "))
      }
      returnlst$pfilter <- pfilter
    }
  }

  ###################################################################################
  ## 7. Build the final query to define population of plots (pltidsqry). 
  ###################################################################################
  
   
  ## 7.1. Create subquery to get most current plots in database.
  ##########################################################################
  if (popFilter$measCur) {
    surveyfromqry <- NULL
    varCur <- "INVYR"
    varCurnm <- findnm(varCur, pflds, returnNULL = TRUE)
    if (is.null(varCurnm)) {
      message("the ", varCur, " field does not exist in data set...")
      return(NULL)
    }

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
          "SELECT DISTINCT ", varCurnm,
          "\nFROM ", SCHEMA., plotnm,
          "\nORDER BY ", varCur)
        pltyrs <- DBI::dbGetQuery(dbconn, yrlst.qry)

        if (measEndyr <= min(pltyrs, na.rm=TRUE)) {
          message(measEndyr, " is less than minimum year in dataset")
          return(NULL)
        }
      }
      varCura. <- ifelse(varCur %in% pltflds, plt., pltassgn.)
      Endyr.filter <- paste0(varCura., varCur, " <= ", measEndyr)
      if (is.null(pwhereqry)) {
        pwhereqry <- Endyr.filter
      } else {
        pwhereqry <- paste(paste(pwhereqry, Endyr.filter, sep="\n   AND "))
      }
      if (varCura. == pltassgn.) {
        pltassgnvars <- c(pltassgnvars, varCurnm)
      }
    }

    ## Define group variables
    groupvars <- c("STATECD", "UNITCD", "COUNTYCD", "PLOT")
    pgroupvars <- sapply(groupvars, findnm, pltflds, returnNULL = TRUE)
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
                     pltafromqry,
                     surveyfromqry)
    if (!is.null(pwhereqry) || pwhereqry != "") {
      subqry <- paste0(subqry, pwhereqry)
    }
    subqry <- paste0(subqry,
                     "\n GROUP BY ", toString(paste0(plt., groupvars)))

    ## 7.2. Build final pltidsqry
    ##########################################################################
    subjoinqry <- getjoinqry(c(groupvars, varCur), c(groupvars, "MAXYR"), alias2 = "pp.")
    pltselectqry <- paste0("SELECT DISTINCT ", toString(selectpvars))
    pltidsqry <- paste0(pltselectqry,
                        pltafromqry,
                        "\n INNER JOIN ",
                        "\n (", subqry, ") pp ", subjoinqry)
  } else {

    ## 7.2. Build final pltidsqry
    ##########################################################################
    pltselectqry <- paste0("SELECT ", toString(selectpvars))
    pltidsqry <- paste0(pltselectqry,
                        pltafromqry)

    ## Add pwhereqry to pltidsqry
    if (!is.null(pwhereqry) && pwhereqry != "") {
      pltidsqry <- paste0(pltidsqry, pwhereqry)
    }
  }
  

  ###################################################################################
  ## 8. Create list of objects to return
  ###################################################################################
  returnlst <- list(pltidsqry = pltidsqry,
                    states = states, invyrs = invyrs,
                    pltselectqry = pltselectqry,
                    pfromqry = pfromqry,
                    pltafromqry = pltafromqry,
                    nonsamp.pfilter = nonsamp.pfilter,
                    iseval = iseval)
  if (iseval) {
    returnlst$popevalid  <- popevalid
    returnlst$POP_PLOT_STRATUM_ASSGN <- POP_PLOT_STRATUM_ASSGN
    returnlst$PLOT <- PLOT
    returnlst$ppsanm <- ppsanm
    returnlst$plotnm <- plotnm
  }
  if (!is.null(pfilter)) {
    returnlst$pfilter <- pfilter
  }
  returnlst$pwhereqry <- pwhereqry
  returnlst$pltassgnvars <- pltassgnvars

  return(returnlst)
}
