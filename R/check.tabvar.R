check.tabvar <- function(popType, tabvartype, tabvar, tab.orderby, 
                         tab.FIAname, tablut, 
                         tab.add0, tab.classify, title.tabvar, 
                         pltcondflds, pltcondx, cuniqueid, condid = "CONDID",
                         treex, treeflds, seedx, seedflds, estseed,
                         tuniqueid, whereqry, withqry, cvars2keep,
                         popdatindb, popconn, SCHEMA., 
                         tabgrp = FALSE, tabgrpnm = NULL, 
                         tabgrpord = NULL, title.tabgrp = NULL,
                         domlut = NULL, domvarlst = NULL, 
                         factor.addNA = FALSE, spcdname = "COMMON_SCIENTIFIC") {
  
  ## set global variables
  tabuniquex=uniquetabvar=SITECLCD=GSSTKCD=tuniquex=suniquex=coluniquex=
    rowvarnm=colvarnm=tabclassnm=tabclassqry <- NULL
  tabvarnm=tabvarnew <- tabvar
  keepNA <- TRUE
  isdbc=isdbt=bytdom=bypcdom <- FALSE
  seedclnm <- "<1"
  gui <- FALSE
  
  
  ## define function to make factors
  makefactor <- function(x) {
    if (!is.factor(x)) {
      uniquevals <- unique(x)
      x <- factor(x, levels = ifelse(is.na(uniquevals), "NA", uniquevals))
    }
    return(x)
  }
    
  ## lookup table for understory vegetation
  ref_growth_habit <- 
    data.frame(GROWTH_HABIT_CD = c("SD", "ST", "GR", "FB", "SH", "TT", "LT", "TR", "NT", "DS"),
                 GROWTH_HABIT_NM = c("Seedlings/Saplings", "Seedlings", "Graminoids", 
                                     "Forbs", "Shrubs", "Trees", "Large trees", "Trees", "Non-tally", "Dead pinyon"))
    
  
  ## Check popconn
  ###############################################
  if (popdatindb) {
    if (!is.null(popconn)) {
      if (!DBI::dbIsValid(popconn)) {
        message("invalid database connection") 
        return(NULL)
      }
      tablst <- DBI::dbListTables(popconn)
      if (length(tablst) == 0) {
        message("invalid database connection") 
        return(NULL)
      }	  
    }
    isdbt <- TRUE
    if (is.character(pltcondx)) {
      isdbc <- TRUE
      pltcondxnm <- pltcondx
    }
  } else {
    if (!is.null(pltcondx) && is.data.frame(pltcondx)) {
      pltcondxnm <- "pltcondx"
    }
  }
  
  if (!is.null(tab.FIAname) && tab.FIAname) {
    ## Get FIA reference table for xvar
    xvar.ref <- getRefobject(toupper(tabvar))
    if (is.null(xvar.ref) && !toupper(tabvar) %in% c("SPCD", "GROWTH_HABIT_CD")) {
      message(paste("no reference name for", tabvar))
      tab.FIAname <- FALSE
    }
  }

  ## GET column titles defined in FIESTA
  ###################################################
  if (is.null(title.tabvar)) {
    title.tabvar <- 
      ifelse (tabvar %in% ref_titles[["DOMVARNM"]],
              ref_titles[ref_titles[["DOMVARNM"]] == tabvar, "DOMTITLE"],
              ifelse (sub("PREV_", "", tabvar) %in% ref_titles[["DOMVARNM"]],
                      paste0("Previous ", tolower(ref_titles[ref_titles[["DOMVARNM"]] ==
                            sub("PREV_", "", tabvar), "DOMTITLE"])), tabvar))
  }
  
  ## Check tablut
  if (!is.null(tablut)) {
    if (is.vector(tablut) && length(tablut) > 1) {
      tablut <- data.table(tablut)
      names(tablut) <- tabvar
      setkeyv(tablut, tabvar)
    } else {
      tablut <- pcheck.table(tablut, tabnm = tablut, gui = gui,
                             caption = paste0(tabvartype, " look up?"))
    }
    tablut <- tablut[!duplicated(tablut)]
  }
  
  ##################################################################################
  ## Check for lookup tables
  ##################################################################################
  
  ## domlut defines columns in cond to use for codes, code names, and table titles
  ##################################################################################
  if (!is.null(domlut)) {
    
    if (!tabvar %in% domvarlst) stop(paste(tabvar, "is not in domlut"))
    if (tabvar %in% domlut[["DOMCODE"]]) {
      tab.orderby <- tabvar
      title.tabvar <- as.character(domlut[match(tabvar, domlut[["DOMCODE"]]), "DOMTITLE"])
      tabvar <- as.character(domlut[match(tabvar, domlut[["DOMCODE"]]), "DOMNAME"])
      if (!tabvar %in% pltcondflds) {
        warning(paste(tabvar, "not in cond table... using code"))
        tabvarnm <- tab.orderby
        tab.orderby <- NULL
      }
    } else if (tabvar %in% domlut[["DOMNAME"]]) {
      tab.orderby <- as.character(domlut[match(tabvar, domlut[["DOMNAME"]]), "DOMCODE"])
      title.tabvar <- as.character(domlut[match(tabvar, domlut[["DOMNAME"]]), "DOMTITLE"])
      if (!tab.orderby %in% pltcondflds) {
        warning(paste(tab.orderby, "not in cond table... ordering by name"))
        tab.orderby <- NULL
      }
    }
  } else {  ## domlut is null
    
    ## Build fromqry for tabvar 
    ###############################################
    if (tabvar %in% pltcondflds) {
      bypcdom <- TRUE
      tabisdb <- isdbc
      tabflds <- pltcondflds
      if (!tabisdb) {
        tabnm <- "pltcondx"
      } else {
        tabnm <- pltcondx
      }
      joinid <- cuniqueid
      
      ## add tabvar to cvars2keep
      cvars2keep <- c(cvars2keep, tabvar)
      
      ## build tab fromqry
      tabfromqry <- paste0(
        "\nFROM ", tabnm, " pc")
      
    } else {
      if (estseed == "only" && tabvar %in% seedflds) {
        bytdom <- TRUE
        tabisdb <- isdbt
        tabflds <- seedflds
        if (!tabisdb) {
          tabnm <- "seedx"
        } else {
          tabnm <- seedx
        }
      } else if (tabvar %in% treeflds) {
        bytdom <- TRUE
        tabisdb <- isdbt
        tabflds <- treeflds
        if (!tabisdb) {
          tabnm <- "treex"
        } else {
          tabnm <- treex
        }
        if (estseed == "add") {
          if (!tabisdb) {
            seedtabnm <- "seedx"
          } else {
            seedtabnm <- seedx
          }
        }
      }
      
      ## build tab fromqry
      tabfromqry <- paste0(
        "\nFROM ", SCHEMA., tabnm, " t")
      
      if (!is.null(pltcondx)) {
        tabtjoinqry <- getjoinqry(c(tuniqueid, condid), c(cuniqueid, condid), "t.", "pc.")
        tabfromqry <- paste0(
          tabfromqry,
          "\nJOIN ", pltcondxnm, " pc ", tabtjoinqry)
      }
      
      if (estseed == "add") {
        seedfromqry <- paste0(
          "\nFROM ", SCHEMA., seedtabnm, " s")
        
        if (!is.null(pltcondx)) {
          tabsjoinqry <- getjoinqry(c(tuniqueid, condid), c(cuniqueid, condid), "s.", "pc.")
          seedfromqry <- paste0(
            seedfromqry,
            "\nJOIN ", pltcondxnm, " pc ", tabsjoinqry)
        }
      }
    }  
    
    ## define where statement (if tab.add0, get all values in population)
    whereqry.tab <- whereqry
    if (tab.add0 && !is.null(whereqry)) {
      whereqry.tab <- NULL
    }
    
    ## Check tab.orderby
    ###############################################
    if (!is.null(tab.orderby) && tab.orderby != "NONE") {
      if (tab.orderby == tabvar) {
        message(tabvartype, ".orderby must be different than ", tabvartype, "var")
        tab.orderby <- "NONE"
      }	  
      if (tab.orderby != "NONE") {
        if (!tab.orderby %in% pltcondflds) {
          message(tabvartype, ".orderby must be in plot/cond")
          return(NULL)
        }
        
        ## add tabvar to cvars2keep
        cvars2keep <- c(cvars2keep, tab.orderby)	
        
        ## Build query for getting unique values within population
        uniquetabvar.qry <- 
          paste0("SELECT DISTINCT ", toString(c(tab.orderby, tabvar)), 
                 tabfromqry,
                 whereqry.tab,
                 "\nORDER BY ", toString(c(tab.orderby, tabvar)))
        if (!is.null(withqry)) {
          uniquetabvar.qry <- paste0(withqry,
                                  "\n", uniquetabvar.qry)
        }
        tabvartmp <- tab.orderby
        tab.orderby <- tabvar
        tabvar <- tabvartmp
        
        #message("getting unique values for ", tabvar, ":\n", uniquetabvar.qry, "\n")
        if (popdatindb) {
          uniquetabvar <- tryCatch(
            DBI::dbGetQuery(popconn, uniquetabvar.qry),
            error=function(e) {
              message(e,"\n")
              return(NULL)})
          if (is.null(uniquetabvar)) {
            message("invalid unique ", tabvartype, "var query...")
            message(uniquetabvar.qry)
            stop()
          }
        } else {
          uniquetabvar <- tryCatch( 
            sqldf::sqldf(uniquetabvar.qry, connection = NULL),
            error=function(e) {
              message("invalid unique ", tabvartype, "var query...")
              message(e,"\n")
              return(NULL)})
          if (is.null(uniquetabvar)) {
            message("invalid unique ", tabvartype, "var query...")
            message(uniquetabvar.qry)
            stop()
          }
        }	
      }  ## end tab.orderby != "NONE"
      
      if (tabvar %in% treeflds && estseed %in% c("add", "only")) {
        if (!is.null(seedx)) {
          
          if (!tab.orderby %in% seedflds) {
            message(tab.orderby, " not in seed")
            return(NULL)
          }	 
          
          ## Build query for getting unique values within population
          uniquetabvar.qry <- 
            paste0("SELECT DISTINCT ", toString(c(tab.orderby, tabvar)), 
                   seedfromqry,
                   whereqry.tab,
                   "\nORDER BY ", toString(c(tab.orderby, tabvar)))
          if (!is.null(withqry)) {
            uniquetabvar.qry <- paste0(withqry,
                                    "\n", uniquetabvar.qry)
          }
          
          #message("getting unique values for ", tabvar, ":\n", uniquetab.qry, "\n")
          if (tabisdb) {
            uniquetabvar <- tryCatch(
              DBI::dbGetQuery(popconn, uniquetabvar.qry)[[1]],
              error=function(e) {
                message(e,"\n")
                return(NULL)})
            if (is.null(uniquetabvar)) {
              message("invalid unique ", tabvartype, "var query...")
              message(uniquetabvar.qry)
              stop()
            }
          } else {
            uniquetabvar <- tryCatch(
              sqldf::sqldf(uniquetabvar.qry)[[1]],
              error=function(e) {
                message(e,"\n")
                return(NULL)})
            if (is.null(uniquetabvar)) {
              message("invalid unique ", tabvartype, "var query...")
              message(uniquetabvar.qry)
              stop()
            }
          }
          
          if (estseed == "add" && tabvar == "DIACL" && is.data.frame(treex)) {
            seedclord <- min(treex[[tab.orderby]]) - 0.5
            seedx[[tab.orderby]] <- seedclord
          } else {
            if (estseed == "add" && is.data.frame(seedx) && tabvar=="DIACL" && !"DIACL" %in% seedflds) {
              seedx$DIACL <- seedclnm
            }
          }			
        } else {
          uniquetabvar <- NULL
        }
      }  ## end tabvar %in% treeflds & estseed in c("add", "only")
      
    } else {   ## !is.null(tab.orderby) && tab.orderby != "NONE"
      
      ## Build query for getting unique tabvar values within population
      uniquex.qry <- 
        paste0("SELECT DISTINCT ", tabvar, 
               tabfromqry,
               whereqry.tab,
               "\nORDER BY ", tabvar)
      
      #message("getting unique values for ", tabvar, ":\n", cuniquex.qry, "\n")
      if (tabisdb) {
        if (!is.null(withqry)) {
          uniquex.qry <- paste0(withqry, 
                                "\n", uniquex.qry)
        }
        uniquex <- tryCatch(
          DBI::dbGetQuery(popconn, uniquex.qry)[[1]],
          error=function(e) {
            message(e,"\n")
            return(NULL)})
        if (is.null(uniquex)) {
          message("invalid unique tabvar query...")
          message(uniquex.qry)
          stop()
        }
      } else {
        uniquex <- tryCatch(
          sqldf::sqldf(uniquex.qry, connection = NULL)[[1]],
          error=function(e) {
            message(e,"\n")
            return(NULL)})
        if (is.null(uniquex)) {
          message("invalid unique tabvar query...")
          message(uniquex.qry)
          stop()
        }
      }
      
      ## Build query for getting unique tabvarclass values within population
      if (!is.null(tab.classify)) {
        class. <- ifelse (tabvar %in% pltcondflds, "pc.", "t.")
        
        if (is.vector(tab.classify)) {
          minx <- min(uniquex, na.rm=TRUE)
          #maxx <- max(uniquex, na.rm=TRUE)
          minbrk <- min(tab.classify)
          #maxbrk <- max(tab.classify)
          if (minx < minbrk) {
            minxmiss <- sort(unique(minx[minx < minbrk]))
            message("there are values in dataset less than class breaks defined for ", tabvar)
            message("...these values are classified as NA: ", toString(minxmiss))
          }
          #if (maxx > maxbrk) {
          #  maxxmiss <- sort(unique(maxx[maxx > maxbrk]))
          #  message("there are values in dataset greater than class breaks defined for ", tabvar)
          #  message("...these values are classified as NA: ", toString(maxxmiss))
          #}
          
          tabclassnm <- paste0(tabvar, "CL")
          tabclassqry <- classifyqry(classcol = tabvar,
                                     cutbreaks = tab.classify,
                                     class. = class.,
                                     fill = NULL)
          
        } else if (is.data.frame(tab.classify)) {
          
          if (ncol(tab.classify) < 2) {
            message("invalid tab.classify... must be a data.frame with TO and FROM columns")
            stop()
          }
          tabclassnm <- checknm(paste0(tabvar, "CL"), pltcondflds)
          names(tab.classify) <- toupper(names(tab.classify))
          if (!all(c("TO", "FROM") %in% names(tab.classify))) {
            message("invalid classes for ", tabvar, 
                    "... the data.frame must include columns: FROM and TO")
            stop()
          }
          fromval <- tab.classify$FROM
          toval <- tab.classify$TO
          
          ## Check values of fromval
          if (!any(!is.na(uniquex) %in% fromval)) {
            missvals <- uniquex[which(!uniquex %in% fromval)]
            message("missing values in tab.classify: ", toString(missvals))
          }
          tabclassqry <- classqry(classcol = tabvar, 
                                  fromval, toval, 
                                  classnm = tabclassnm, 
                                  class. = class.,
                                  fill = NULL)
        } else {
          message("invalid tab.classify... must be a vector of class breaks or a data.frame with TO and FROM columns")
          stop()
        }
        
        ## Build query for getting unique tabclass values within population
        tabvarnm=tabvarnew <- tabclassnm
        uniquex.qry <- 
          paste0("SELECT DISTINCT \n", 
                 tabclassqry,
                 tabfromqry,
                 whereqry.tab,
                 "\nORDER BY ", tabclassnm)
        
        
        ## get unique values for classified tabvar
        if (tabisdb) {
          if (!is.null(withqry)) {
            uniquex.qry <- paste0(withqry, 
                                  "\n", uniquex.qry)
          }
          uniquex <- tryCatch(
            DBI::dbGetQuery(popconn, uniquex.qry)[[1]],
            error=function(e) {
              message(e,"\n")
              return(NULL)})
          if (is.null(uniquex)) {
            message("invalid unique ", tabvartype, "var query...")
            message(uniquex.qry)
            stop()
          }
        } else {
          uniquex <- tryCatch(
            sqldf::sqldf(uniquex.qry, connection = NULL)[[1]],
            error=function(e) {
              message(e,"\n")
              return(NULL)})
          if (is.null(uniquex)) {
            message("invalid unique ", tabvartype, "var query...")
            message(uniquex.qry)
            stop()
          }
        }
      }   ## end tab.classify
      
      if (any(is.na(uniquex)) && !keepNA) {
        uniquex <- uniquex[!is.na(uniquex)]
      }
      tabuniquex <- uniquex
      
      ## Check seedling table
      if (tabvar %in% seedflds && estseed == "add") {
        
        ## Build seedling from query
        if (!is.null(seedx)) {
          
          if (estseed == "add" && (tabvar == "DIACL" ||
                                   (tabvar == "DIA" && !is.null(tab.classify) && "DIACL" %in% names(tab.classify))))  {
            suniquex <- "<1"
            seedflds <- c(seedflds, "DIACL")
          } else {  
            #if (!tabvar %in% seedflds) {
            #  message(tabvar, " not in seed")
            #  return(NULL)
            #}	  
            
            ## Build query for getting unique seedling tabvar values within population
            suniquex.qry <- 
              paste0("SELECT DISTINCT ", tabvar, 
                     seedfromqry,
                     "\nORDER BY ", tabvar)
            if (estseed == "only") {
              #message("getting unique values for ", tabvar, ":\n", suniquex.qry, "\n")
            }
            if (tabisdb) {
              if (!is.null(withqry)) {
                suniquex.qry <- paste0(withqry, 
                                       "\n", suniquex.qry)
              }
              suniquex <- tryCatch(
                DBI::dbGetQuery(popconn, suniquex.qry)[[1]],
                error=function(e) {
                  message(e,"\n")
                  return(NULL)})
              if (is.null(suniquex)) {
                message("invalid unique ", tabvartype, "var query...")
                message(suniquex.qry)
                stop()
              }
              
            } else {
              suniquex <- tryCatch(
                sqldf::sqldf(suniquex.qry, connection = NULL)[[1]],
                error=function(e) {
                  message("invalid unique ", tabvartype, "var query...")
                  message(e,"\n")
                  return(NULL)})
              if (is.null(suniquex)) {
                message("invalid unique ", tabvartype, "var query...")
                message(suniquex.qry)
                stop()
              }
            }  
            if (any(is.na(suniquex)) && !keepNA) {
              suniquex <- suniquex[!is.na(suniquex)]		
            }
          }			
        } else {
          suniquex <- NULL
        }
        tabuniquex <- sort(unique(c(uniquex, suniquex)))
      }   ## end check seedling table
      
      ## Check values in tablut
      if (!is.null(tablut)) {
        tablutvals <- tablut[[tabvar]]
        if (!all(tablutvals %in% tabuniquex)) {
          missvals <- tablutvals[!tablutvals %in% tabuniquex]
          message("values in tablut are not in population: ", toString(missvals))
        }
        
        if (!is.null(tab.orderby) && tab.orderby == "NONE") {
          if (tab.orderby == tabvar) {
            tab.name <- names(tablut)[names(tablut) != tabvar]
            if (length(tab.name) > 1) {
              message("invalid tablut... only 2 columns allowed")
            }
            tabvarnm <- tab.name
          }
        }
        
      } else if (tab.FIAname) {
        
        tabLUTgrp <- FALSE
        
        if (tabgrp) {
          if (!is.null(tabgrpnm)) {
            if (!tabgrpnm %in% tabflds) {
              message(tabgrpnm, "not in ", tabnm)
              return(NULL)
            }  
            if (is.null(title.tabgrp)) {
              title.tabgrp <- tabgrpnm
            }
            if (!is.null(tabgrpord)) {
              if (!tabgrpord %in% tabflds) {
                message(tabgrpord, "not in ", tabnm)
              }
            }
          } else {
            tabLUTgrp <- TRUE
          }
        }
        
        if (tabvar %in% treeflds) {
          
          if (tabvar == "GROWTH_HABIT_CD") {
            tablut <- ref_growth_habit
            tabLUTnm <- "GROWTH_HABIT_NM"
            if (is.data.table(treex)) {
              treex <- merge(treex, ref_growth_habit, by=tabvar, all.x=TRUE)
              tablut <- data.table(tablut[tablut[[tabvar]] %in% treex[[tabvar]], ])
            }
            tablut <- tablut[, lapply(.SD, makefactor)]
          } else {
            if (estseed != "only") {
              if (!is.data.frame(treex)) { 
                x <- treeflds 
              } else { 
                x <- treex 
              } 
              
              if (tabvar == "SPCD") {
                tabLUT <- datLUTspp(x = x, 
                                    add0 = tab.add0, 
                                    xtxt = "tree", 
                                    uniquex = uniquex,
                                    spcdname = spcdname)
              } else {
                tabLUT <- datLUTnm(x = x, 
                                   xvar = tabvar, 
                                   LUT = tablut, 
                                   FIAname = tab.FIAname,
                                   group = tabLUTgrp, 
                                   add0 = tab.add0, 
                                   xtxt = "tree", 
                                   uniquex = uniquex)
              }
              if (!tabisdb) {
                treex <- setDT(tabLUT$xLUT)
              }
              tablut <- setDT(tabLUT$LUT)
              tabLUTnm <- tabLUT$xLUTnm
              tabvarnm <- tabLUTnm
              
            } ## end estseed != only
            
            if (estseed %in% c("add", "only") && !is.null(seedx)) {
              if (!is.data.frame(seedx)) { 
                x <- seedflds 
              } else { 
                x <- seedx 
              } 
              if (tabvar %in% seedflds) {
                if (tabvar == "SPCD") {
                  tabLUT <- datLUTspp(x = x, 
                                      add0 = tab.add0, 
                                      xtxt = "seed", 
                                      uniquex = suniquex)
                } else {            
                  tabLUT <- datLUTnm(x = x, 
                                     xvar = tabvar, 
                                     LUT = NULL, 
                                     FIAname = tab.FIAname,
                                     group = tabLUTgrp, 
                                     add0 = tab.add0, 
                                     xtxt = "seed", 
                                     uniquex = suniquex)
                }  
                tabluts <- setDT(tabLUT$LUT)
                tabluts <- tabluts[!tabluts[[tabvar]] %in% tablut[[tabvar]],]
                tabLUTnm <- tabLUT$xLUTnm
                tabvarnm <- tabLUTnm
                
                if (nrow(tabluts) > 0) {
                  tablut <- rbind(tablut, tabluts)
                }
                if (!tabisdb) {
                  seedx <- tabLUT$xLUT
                }       
              } else if (tabvar == "DIACL") {
                if (tabisdb) {
                  seedx$DIACL <- seedclnm
                }
              }  ## end estseed %in% c("add", "only")
            }
          }
        } else { ## tabvar in pltcondflds
            
          ## check for prefix (PREV_) xvar
          xvar <- ifelse(popType %in% c("CHNG"), sub("PREV_", "", tabvar), tabvar)
          #xvar <- tabvar
            
          tabLUT <- datLUTnm(x = tabflds, 
                             xvar = xvar, 
                             uniquex = uniquex,
                             LUT = tablut, 
                             FIAname = tab.FIAname,
                             group = tabLUTgrp,
                             add0 = tab.add0)
          tablut <- setDT(tabLUT$LUT)
          tabLUTnm <- tabLUT$xLUTnm
        } 
        
          
        # ## append prefix
        # if (popType %in% c("CHNG")) {
        #   names(tablut) <- paste0("PREV_", names(tablut))
        #   tabLUTnm <- paste0("PREV_", tabLUTnm)
        #   tabvarnew=tabvar <- paste0("PREV_", tabvar)
        # }

        if (tabgrp) {
          tabgrpord <- tabLUT$grpcode
          tabgrpnm <- tabLUT$grpname
          if (all(sapply(tablut[[tabgrpnm]], function(x) x == "")) || 								
              all(is.na(tablut[[tabgrpnm]]))) {
              stop("no groups for ", tabvar)
          }
          title.tabgrp <- ifelse (tabgrpord %in% ref_titles[["DOMVARNM"]], 
                                    ref_titles[ref_titles[["DOMVARNM"]] == tabgrpord, "DOMTITLE"], tabgrpnm)
        }
          
        if (is.null(tab.orderby) || tab.orderby == "NONE") {
          if (!is.null(tabLUTnm)) {
            tab.orderby <- tabvar
            tabvarnm <- tabLUTnm
          }
          if (!is.null(tab.orderby) && tab.orderby == tabvar) {
            tab.name <- names(tablut)[names(tablut) != tabvar]
            if (length(tab.name) > 1) {
              message("invalid tablut... only 2 columns allowed")
              return(NULL)
            }
            tabvarnm <- tab.name
          }
        } else {
          if (!tab.orderby %in% names(tablut)) {
            message("tab.orderby not in tablut")
            return(NULL)
          }
        }
      }  ## end if (tab.FIAname & !is.null(tablut))
    }  ## end !is.null(tab.orderby) && tab.orderby != "NONE"
  }  ## end domlut is null
  
  
  ############################################################################
  ## create uniquetabvar add create factors
  ############################################################################
  
  ## uniquetabvar
  #########################################################
  if (!is.null(tablut)) {
    #    if (sum(unlist(lapply(tablut, duplicated))) > 0) {
    #      print(tablut)
    #      stop("invalid tablut... no duplicates allowed")
    #    }
    uniquetabvar <- tablut
    if (all(!is.factor(uniquetabvar[[tabvar]]), tab.orderby != "NONE", 
            tab.orderby %in% names(uniquetabvar))) {
      setorderv(uniquetabvar, tab.orderby, na.last=TRUE)
    }
  } else if (!is.null(uniquetabvar)) {
    uniquetabvar <- setDT(uniquetabvar)
    if (!is.null(tab.orderby) && tab.orderby != "NONE" && 
        tab.orderby %in% names(uniquetabvar)) {
      setkeyv(uniquetabvar, c(tabgrpnm, tab.orderby))
    }
    
  } else if (!is.null(tabuniquex)) {
    uniquetabvar <- as.data.table(tabuniquex)
    names(uniquetabvar) <- tabvarnew
    
    if (tabvar == "GROWTH_HABIT_CD") {
      ghcodes <- ref_growth_habit[[tabvar]]
      ghord <- ghcodes[ghcodes %in% tabuniquex]
      if (length(ghord) < length(tabuniquex)) {
        missgh <- tabuniquex[!tabuniquex %in% ghord]
        message("growth_habit_cd not in ref: ", toString(missgh)) 
      } else {		  
        tabuniquex <- tabuniquex[match(ghord, tabuniquex)]
      }
    }
    uniquetabvar[[tabvarnew]] <- factor(uniquetabvar[[tabvarnew]], levels=tabuniquex)
    if (factor.addNA && any(!is.na(uniquetabvar[[tabvarnew]]))) {
      uniquetabvar[[tabvarnew]] <- addNA(uniquetabvar[[tabvarnew]])
    }
    setkeyv(uniquetabvar, tabvarnew)
    
  } else if (tabvar %in% pltcondflds && is.data.frame(pltcondx)) {
    if (!is.null(tab.orderby) && tab.orderby != "NONE") {
      uniquetabvar <- unique(pltcondx[,c(tabgrpord, tabgrpnm, tab.orderby, tabvar), with=FALSE])
      setkeyv(uniquetabvar, c(tabgrpord, tabgrpnm, tab.orderby))
    } else if (is.data.frame(pltcondx)) {	
      if (is.factor(pltcondx[[tabvar]])) {
        uniquetabvar <- as.data.table(levels(pltcondx[[tabvar]]))
        names(uniquetabvar) <- tabvar
        uniquetabvar[[tabvar]] <- factor(uniquetabvar[[tabvar]], levels=levels(pltcondx[[tabvar]]))
      } else {
        #tabvals <- na.omit(unique(pltcondx[, tabvar, with=FALSE]))
        tabvals <- unique(pltcondx[, tabvar, with=FALSE])
        setorderv(tabvals, tabvar, na.last=TRUE)
        uniquetabvar <- as.data.table(tabvals)
        names(uniquetabvar) <- tabvar
        setkeyv(uniquetabvar, tabvar)
      }
    }
  } else if (tabvar %in% treeflds) {
    if (!is.null(tab.orderby) && tab.orderby != "NONE") {
      if (estseed == "only" && is.data.frame(seedx)) {
        uniquetabvar <- unique(seedx[,c(tabgrpord, tabgrpnm, tab.orderby, tabvar), with=FALSE])
        setkeyv(uniquetabvar, c(tabgrpord, tabgrpnm, tab.orderby))
      } else {
        uniquetabvar <- unique(treex[,c(tabgrpord, tabgrpnm, tab.orderby, tabvar), with=FALSE])
        setkeyv(uniquetabvar, c(tabgrpord, tabgrpnm, tab.orderby))
        
        if (estseed == "add" && !is.null(seedx) && is.data.frame(seedx)) {
          if (all(c(tabvar, tab.orderby) %in% names(seedx)) && tabvar == "DIACL") {
            if (is.factor(uniquetabvar[[tabvar]])) {
              levels(uniquetabvar[[tabvar]]) <- c(seedclnm, levels(uniquetabvar[[tabvar]]))
            }
            if (is.factor(uniquetabvar[[tab.orderby]])) {
              levels(uniquetabvar[[tab.orderby]]) <- c(seedclord, levels(uniquetabvar[[tab.orderby]]))
            }
            uniqueseed <- data.table(seedclord, seedclnm)
            setkeyv(uniqueseed, c(col.orderby, tabvar))
            uniquetabvar <- rbindlist(list(uniqueseed, uniquetabvar))
          }
        }
      }
    } else if (!is.null(uniquetabvar) && is.data.frame(treex)) {
      
      if (is.factor(treex[[tabvar]])) {
        if (estseed == "add" && tabvar == "DIACL") {
          tablevels <- c(seedclnm, levels(treex[[tabvar]]))
        } else {
          tablevels <- levels(treex[[tabvar]])
        }
        #uniquetabvar <- as.data.table(tablevels)
        #names(uniquetabvar) <- tabvar
        uniquetabvar[[tabvar]] <- factor(uniquetabvar[[tabvar]], levels=tablevels)
        uniquetabvar[[tabvar]] <- sort(uniquetabvar[[tabvar]])
      } else {
        if (estseed == "add" && tabvar == "DIACL") {
          tabvals <- c(seedclnm, sort(na.omit(unique(treex[, tabvar, with=FALSE][[1]]))))
        } else {
          tabvals <- sort(na.omit(unique(treex[, tabvar, with=FALSE][[1]])))
        }
        uniquetabvar <- as.data.table(tabvals)
        names(uniquetabvar) <- tabvar
        uniquetabvar[[tabvar]] <- factor(uniquetabvar[[tabvar]], levels=tabvals)
        uniquetabvar[[tabvar]] <- sort(uniquetabvar[[tabvar]])
        setkeyv(uniquetabvar, tabvar)
      }
    } 
    
  } else if (is.data.frame(treex)) {
    if (is.factor(treex[[tabvar]])) {
      if (estseed == "add" && tabvar == "DIACL") {
        tablevels <- c(seedclnm, levels(treex[[tabvar]]))
      } else {
        tablevels <- levels(treex[[tabvar]])
      }
      uniquetabvar <- as.data.table(tablevels)
      names(uniquetabvar) <- tabvar
      uniquetabvar[[tabvar]] <- factor(uniquetabvar[[tabvar]], levels=tablevels)
      uniquetabvar[[tabvar]] <- sort(uniquetabvar[[tabvar]])
    } else {
      if (estseed == "add" && tabvar == "DIACL") {
        tabvals <- c(seedclnm, sort(na.omit(unique(treex[, tabvar, with=FALSE][[1]]))))
      } else {
        tabvals <- sort(na.omit(unique(treex[, tabvar, with=FALSE][[1]])))
      }
      uniquetabvar <- as.data.table(tabvals)
      names(uniquetabvar) <- tabvar
      uniquetabvar[[tabvar]] <- factor(uniquetabvar[[tabvar]], levels=tabvals)
      uniquetabvar[[tabvar]] <- sort(uniquetabvar[[tabvar]])
      setkeyv(uniquetabvar, tabvar)
    }		
  }
  
  ## Check for duplicate values
  if (!popdatindb && any(duplicated(uniquetabvar[[tabvar]]))) {
    pcfields <- names(condx)
    tfields <- names(treex)
    dupvals <- uniquetabvar[[tabvar]][duplicated(uniquetabvar[[tabvar]])]
    for (dup in dupvals) {       
      vals <- uniquetabvar[uniquetabvar[[tabvar]] == dup, tab.orderby, with=FALSE][[1]]
      val <- vals[length(vals)]
      vals2chg <- vals[-length(vals)]
      
      if (any(c(tabvar, tab.orderby) %in% pcfields)) {
        if (tab.orderby %in% pcfields) {
          if (class(condx[[tab.orderby]]) != class(val)) {
            class(val) <- class(condx[[tab.orderby]])
          } 
          condx[condx[[tab.orderby]] %in% vals2chg, tab.orderby] <- val
        } else {
          if (class(condx[[tabvar]]) != class(val)) {
            class(val) <- class(condx[[tabvar]])
          } 
          condx[condx[[tabvar]] %in% vals2chg, tabvar] <- val
        }
      }
      if (any(c(tabvar, tab.orderby) %in% tfields)) {
        if (tab.orderby %in% tfields) {
          if (class(treex[[tab.orderby]]) != class(val)) {
            class(val) <- class(treex[[tab.orderby]])
          } 
          treex[treex[[tab.orderby]] %in% vals2chg, tab.orderby] <- val
        } else {
          if (class(treex[[tabvar]]) != class(val)) {
            class(val) <- class(treex[[tabvar]])
          } 
          treex[condx[[tabvar]] %in% vals2chg, tabvar] <- val
        }
      }
      uniquetabvar <- uniquetabvar[!uniquetabvar[[tab.orderby]] %in% vals2chg, ] 
    }
  }
  
  
  ## Create factors for ordering tables
  ##############################################################################
  if (!is.null(uniquetabvar)) {
    
    ## Change SITECLCD to descending order
    if (tab.FIAname && "SITECLCD" %in% names(uniquetabvar))
      uniquetabvar <- setorder(uniquetabvar, -SITECLCD)
    if (tab.FIAname && "GSSTKCD" %in% names(uniquetabvar))
      uniquetabvar <- setorder(uniquetabvar, -GSSTKCD)
    
    ## Create factors for ordering
    uniquetabvar <- uniquetabvar[, lapply(.SD, makefactor)]
    setkeyv(uniquetabvar, tabvarnew)
    setorderv(uniquetabvar, tabvarnew, na.last=TRUE)
  }
  
  
  returnlst <- list(uniquetabvar = uniquetabvar,  
                    tabvar = tabvar, tabvarnm = tabvarnm, 
                    tab.orderby = tab.orderby, 
                    title.tabvar = title.tabvar, 
                    tabgrpnm = tabgrpnm, title.tabgrp = title.tabgrp,
                    tabclassnm = tabclassnm, tabclassqry = tabclassqry,
                    cvars2keep = cvars2keep,
                    bytdom = bytdom, bypcdom = bypcdom)
  
  return(returnlst)
}      
