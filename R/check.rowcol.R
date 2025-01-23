check.rowcol <- 
  function(esttype, 
           popType, 
           popdatindb, 
           popconn = NULL, SCHEMA. = "",
           pltcondx = NULL, pltcondflds = NULL, 
           withqry = NULL, 
           estseed = "none",
           treex = NULL, treeflds = NULL,
           seedx = NULL, seedflds = NULL,
	         cuniqueid = "PLT_CN", condid = "CONDID", 
           tuniqueid = "PLT_CN",  
	         rowvar = NULL, colvar = NULL, 
           row.FIAname = FALSE, col.FIAname = FALSE,
	         row.orderby = NULL, col.orderby = NULL, 
           row.classify = NULL, col.classify = NULL,
           row.add0 = FALSE, col.add0 = FALSE, 
	         domvarlst = NULL, domlut = NULL, 
           title.rowvar = NULL, title.colvar = NULL, 
 	         rowlut = NULL, collut = NULL, 
           rowgrp = FALSE, rowgrpnm = NULL, 
           rowgrpord = NULL, title.rowgrp = NULL, 
           cvars2keep = NULL, whereqry = NULL,
           factor.addNA = TRUE,
           gui = FALSE){

  ####################################################################################
  ## CHECKS ROW AND COLUMN INFO
  ## 1. Checks for domlut. domlut is an optional lookup table to define potential
  ##    domain variables for table row/columns. This table must have 3 columns:
  ## 		DOMCODE - variable codes
  ##		DOMNAME - variable code names
  ##		DOMTITLE - pretty name for variable to use for output table titles
  ##    The DOMCODE and DOMNAME variables must be dataset. The info is used to populate
  ##    rowvar/colvar, row.orderby/col.orderby, and title.rowvar/title.colvar.
  ## 2. Define variables to exclude as potential domains
  ## 3. Check rowvar. If rowvar = NULL or "NONE", exit and only estimate totals.
  ## 4. Check for lookup tables, row.orderby, and uniquerow.
  ## 3. Check colvar.
  ## 4. Check for lookup tables, col.orderby, and uniquecol.
  ## 5. Check row and column filters.
  ## 6. Create uniquerow/uniquecol if NULL and set keys
  ## 7. Get title.rowvar/title.colvar from ref_titles
  ## 8. Concatenate variables:
  ##	   If rowvar and colvar in cond table, concatenate columns and add to cond table.
  ## 	   For tree/ratio esttypes:
  ##	   If colvar in tree table, concatenate columns and add to tree table.
  ## 8. Define domain.
  ## 9. Define cvars2keep
  ####################################################################################

  ## Set global variables
  SITECLCD=GSSTKCD=domainlst=tdomvar=tdomvar2=grpvar=rowvarnm=colvarnm <- NULL
  tuniquex=suniquex=coluniquex <- NULL
  isdbc=isdbt=colgrp <- FALSE
  keepNA <- TRUE

  ## define function to make factors
  makefactor <- function(x) {
    if (!is.factor(x)) {
	    uniquevals <- unique(x)
      x <- factor(x, levels = ifelse(is.na(uniquevals), "NA", uniquevals))
	  }
    return(x)
  }
  
  ref_growth_habit <- 
  data.frame(GROWTH_HABIT_CD = c("SD", "ST", "GR", "FB", "SH", "TT", "LT", "TR", "NT"),
             GROWTH_HABIT_NM = c("Seedlings/Saplings", "Seedlings", "Graminoids", 
			             "Forbs", "Shrubs", "Trees", "Large trees", "Trees", "Non-tally"))
  
  
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

  ## Check for condid
  if (!is.null(condid) && !condid %in% c(treeflds, pltcondflds)) condid <- NULL
  if (!is.null(pltcondflds)) {
    if (!is.null(cuniqueid) && !cuniqueid %in% pltcondflds) stop("invalid cuniqueid")
  }
  if (!is.null(treex) && !is.null(tuniqueid) && !tuniqueid %in% treeflds) {
    stop("invalid tuniqueid")
  }
  #ref_titles <- FIESTAutils::ref_titles
  bytdom=bypcdom <- FALSE
  seedclnm <- "<1"

  ##################################################################
  ## SET UP VARIABLE LISTS
  ##################################################################
  ## DEFINE DOMAIN VARIABLES LISTS (VARIABLES TO KEEP AND EXCLUDE)

  ## CHECK domlut
  domlut <- pcheck.table(domlut, tabnm="domlut", nullcheck=TRUE, gui=gui)

  if (!is.null(domlut)) {
    domlutvars <- c("DOMCODE", "DOMNAME")
    if (!all(domlutvars %in% names(domlut))){
      missvars <- domlutvars[which(!domlutvars %in% names(domlut))]
      warning("missing columns in domlut: ", addcommas(missvars))
    }
    if (is.null(domvarlst))
      domvarlst <- c(domlut[["DOMCODE"]], domlut[["DOMNAME"]])
  } else {
    domvarlst <- pltcondflds[!pltcondflds %in% c(cuniqueid, condid, "LON", "LAT", "PLOT")]
  }

  ## DEFINE other variables
  varlst <- sort(domvarlst)
 
  if (esttype %in% c("TREE", "RATIO")){
    ## DEFINE TREE VARIABLE LISTS
    tpavars <- c("TPA_UNADJ", "TPAMORT_UNADJ", "TPAGROW_UNADJ", "TPAREMV_UNADJ")
    volvars <- c("VOLCFNET", "VOLCSNET", "VOLBFNET", "VOLCFGRS", "VOLBFGRS", "VOLCFSND")
    mortvars <- c("TPAMORT_UNADJ", "MORTCFGS", "MORTBFSL", "MORTCFAL", "FMORTCFAL",
		"FMORTCFGS")
    growvars <- c("TPAGROW_UNADJ", "GROWCFGS", "GROWBFSL", "GROWCFAL", "FGROWCFAL",
 		"FGROWCFGS")
    remvvars <- c("TPAREMV_UNADJ", "REMVCFGS", "REMVBFSL", "REMVCFAL", "FREMVCFAL",
 		"FREMVCFGS")
    biovars <- c("DRYBIO_AG", "DRYBIO_BG", "DRYBIO_WDLD_SPP", "DRYBIO_SAPLING",
 		"DRYBIO_STUMP", "DRYBIO_TOP", "DRYBIO_BOLE", "DRYBIOT", "DRYBIOM",
		"DRYBIOTB", "JBIOTOT")
    carbvars <- c("CARBON_BG", "CARBON_AG")

    ## DEFINE TREE DOMAIN VARIABLE LISTS (VARIABLES TO EXCLUDE)
    tdomvarlst.not <- c("TREE", condid, "PREV_TRE_CN", "SUBP", "GROWBA",
		"RADGRW_RMRS", "BA", "TREEAGE", tpavars, volvars, mortvars, growvars,
		remvvars, biovars, carbvars,
		paste(volvars, "TPA", sep="_"), paste(mortvars, "TPA", sep="_"),
		paste(growvars, "TPA", sep="_"), paste(remvvars, "TPA", sep="_"),
		paste(biovars, "TPA", sep="_"), paste(carbvars, "TPA", sep="_"))

    ## DEFINE TREE DOMAIN VARIABLE LISTS (VARIABLES TO KEEP)
    tdomvarlst <- treeflds[!treeflds %in% tdomvarlst.not] 	## Tree domain variables
	
	  if (!is.null(seedflds)) {
      tdomvarlst <- unique(c(tdomvarlst, seedflds[!seedflds %in% tdomvarlst.not])) 	## Seed domain variables
    }
    varlst <- c(varlst, sort(tdomvarlst))
  } else {
    varlst <- c(varlst, treeflds)
  }

  ## Check row.add0 and col.add0
  ########################################################
  row.add0 <- pcheck.logical(row.add0, varnm="row.add0",
		title="Add 0 for row?", first="NO", gui=gui)
  col.add0 <- pcheck.logical(col.add0, varnm="col.add0",
		title="Add 0 for column?", first="NO", gui=gui)
  rowgrp <- pcheck.logical(rowgrp, varnm="rowgrp", title="Row groups?",
		first="NO", gui=gui)
  row.FIAname <- pcheck.logical(row.FIAname, varnm="row.FIAname",
		title="Row names?", first="NO", gui=gui)
  if (rowgrp && is.null(rowgrpnm) && !row.FIAname) {
    stop("either row.FIAname must be TRUE or rowgrpnm != NULL to add row groups")
  }

  ##############################################################
  ### ROW VARIABLE
  ##############################################################
  uniquerow <- NULL
  rowvar <- pcheck.varchar(var2check=rowvar, varnm="rowvar", gui=gui,
		   checklst=c("NONE", varlst), caption="Row variable",
		   warn=paste(rowvar, "not found"))
  if (is.null(rowvar)) rowvar <- "NONE"


  ## If rowvar == "NONE", set rowvar = "TOTAL" and exit, returning short list
  if (rowvar == "NONE") {
    rowvar=domainlst <- "TOTAL"
    colvar <- "NONE"
    row.add0 <- FALSE
    col.add0 <- FALSE
    row.FIAname <- FALSE
    
    if (!is.null(cvars2keep) && length(cvars2keep) > 0) {
      if (!all(cvars2keep %in% pltcondflds)) {
        cvars2keep <- cvars2keep[cvars2keep %in% pltcondflds]
        if (length(cvars2keep) == 0) {
          cvars2keep <- NULL
        }
      }
    }
    ## Define pltcondvars
    #pltcondvars = unique(c(cuniqueid, condid, cvars2keep, domainlst))
    
    returnlst <- list(bytdom = bytdom, bypcdom = bypcdom, 
                      domainlst = domainlst, 
                      uniquerow = NULL, uniquecol = NULL, 
                      rowvar = rowvar, rowvarnm = rowvar, colvar = colvar, 
                      row.orderby = row.orderby, col.orderby = col.orderby,
                      row.add0 = row.add0, col.add0 = col.add0,
                      title.rowvar = title.rowvar, title.colvar = title.colvar,
                      tdomvar = tdomvar)
    return(returnlst)
  }

  if (rowvar != "NONE") {   
    rowuniquex <- NULL
    rowvarnm=rowvarnew <- rowvar
    
    if (!is.null(row.FIAname) && row.FIAname) {
      ## Get FIA reference table for xvar
      xvar.ref <- getRefobject(toupper(rowvar))
      if (is.null(xvar.ref) && !toupper(rowvar) %in% c("SPCD", "GROWTH_HABIT_CD")) {
        message(paste("no reference name for", rowvar))
        row.FIAname <- FALSE
      }
    }

    ## GET row titles defined in FIESTA
    ###################################################
    if (is.null(title.rowvar)) {
      title.rowvar <- 
        ifelse (rowvar %in% ref_titles[["DOMVARNM"]],
		         ref_titles[ref_titles[["DOMVARNM"]] == rowvar, "DOMTITLE"],
		      ifelse (sub("PREV_", "", rowvar) %in% ref_titles[["DOMVARNM"]],
		         paste0("Previous ", tolower(ref_titles[ref_titles[["DOMVARNM"]] ==
			            sub("PREV_", "", rowvar), "DOMTITLE"])), rowvar))
    }

    ## Check row groups
    if (rowgrp && is.null(rowgrpnm)) {
      vargrp <- unique(FIESTAutils::ref_codes[!is.na(FIESTAutils::ref_codes[["GROUPNM"]]) &
		                   FIESTAutils::ref_codes[["GROUPNM"]] != "", "VARIABLE"])
      if (!rowvar %in% vargrp) {
        message("row group not available for rowvar")
        rowgrp <- FALSE
      }
    }

    ## Check rowlut
    if (!is.null(rowlut)) {
      if (is.vector(rowlut) && length(rowlut) > 1) {
        rowlut <- data.table(rowlut)
        names(rowlut) <- rowvar
        setkeyv(rowlut, rowvar)
      } else {
        rowlut <- pcheck.table(rowlut, gui=gui, tabnm=rowlut, caption="Row look up?")
      }
    }

    ##################################################################################
    ## Check for lookup tables
    ##################################################################################

    ## domlut defines columns in cond to use for codes, code names, and table titles
    ##################################################################################
    if (!is.null(domlut)) {

      if (!rowvar %in% domvarlst) stop(paste(rowvar, "is not in domlut"))
      if (rowvar %in% domlut[["DOMCODE"]]) {
        row.orderby <- rowvar
        title.rowvar <- as.character(domlut[match(rowvar, domlut[["DOMCODE"]]), "DOMTITLE"])
        rowvar <- as.character(domlut[match(rowvar, domlut[["DOMCODE"]]), "DOMNAME"])
        if (!rowvar %in% pltcondflds) {
          warning(paste(rowvar, "not in cond table... using code"))
          rowvarnm <- row.orderby
          row.orderby <- NULL
        }
      } else if (rowvar %in% domlut[["DOMNAME"]]) {
        row.orderby <- as.character(domlut[match(rowvar, domlut[["DOMNAME"]]), "DOMCODE"])
        title.rowvar <- as.character(domlut[match(rowvar, domlut[["DOMNAME"]]), "DOMTITLE"])
        if (!row.orderby %in% pltcondflds) {
          warning(paste(row.orderby, "not in cond table... ordering by name"))
          row.orderby <- NULL
        }
      }
    } else {  ## domlut is null
      
      if (popType == "CHNG" && (is.null(colvar) || colvar == "NONE")) {
        colvar <- rowvar
        col.orderby <- row.orderby
        title.colvar <- title.rowvar
        col.FIAname <- row.FIAname
        col.add0 = row.add0
        collut <- rowlut
        col.classify <- row.classify
        
        ## Rename rowvar variables with prefix 'PREV_'
        rowvar <- paste0("PREV_", rowvar)
        if (!is.null(row.orderby)) {
          row.orderby <- paste0("PREV_", row.orderby)
        }
        if (!is.null(title.rowvar)) {
          title.rowvar <- paste0("Previous ", title.rowvar)
        }
        rowvarnm=rowvarnew <- rowvar
      }

      ## Build fromqry for rowvar 
      ###############################################
      if (rowvar %in% pltcondflds) {
        bypcdom <- TRUE
        rowisdb <- isdbc
        rowflds <- pltcondflds
        if (!rowisdb) {
          rowtabnm <- "pltcondx"
        } else {
          rowtabnm <- pltcondx
        }
        joinid <- cuniqueid
        
        ## add rowvar to cvars2keep
        cvars2keep <- c(cvars2keep, rowvar)
        
        ## build row fromqry
        rowfromqry <- paste0(
          "\nFROM ", rowtabnm, " pc")
        
      } else {
        if (estseed == "only" && rowvar %in% seedflds) {
          bytdom <- TRUE
          rowisdb <- isdbt
          rowflds <- seedflds
          if (!rowisdb) {
            rowtabnm <- "seedx"
          } else {
            rowtabnm <- seedx
          }
        } else if (rowvar %in% treeflds) {
          bytdom <- TRUE
          rowisdb <- isdbt
          rowflds <- treeflds
          if (!rowisdb) {
            rowtabnm <- "treex"
          } else {
            rowtabnm <- treex
          }
          if (estseed == "add") {
            if (!rowisdb) {
              seedtabnm <- "seedx"
            } else {
              seedtabnm <- seedx
            }
          }
        }

        ## build row fromqry
        rowfromqry <- paste0(
          "\nFROM ", SCHEMA., rowtabnm, " t")
        
        if (!is.null(pltcondx)) {
          rowtjoinqry <- getjoinqry(c(tuniqueid, condid), c(cuniqueid, condid), "t.", "pc.")
          rowfromqry <- paste0(
            rowfromqry,
            "\nJOIN ", pltcondxnm, " pc ", rowtjoinqry)
        }
        
        if (estseed == "add") {
          seedfromqry <- paste0(
            "\nFROM ", SCHEMA., seedtabnm, " s")
          
          if (!is.null(pltcondx)) {
            rowsjoinqry <- getjoinqry(c(tuniqueid, condid), c(cuniqueid, condid), "s.", "pc.")
            seedfromqry <- paste0(
              seedfromqry,
              "\nJOIN ", pltcondxnm, " pc ", rowsjoinqry)
          }
        }
      }  

	    ## Check row.orderby
      ###############################################
      if (!is.null(row.orderby) && row.orderby != "NONE") {
        if (row.orderby == rowvar) {
		      message("row.orderby must be different than rowvar")
		      row.orderby <- "NONE"
		    }	  
        if (row.orderby != "NONE") {
          if (!row.orderby %in% pltcondflds) {
		        message("row.orderby must be in plot/cond")
		        return(NULL)
		      }
		  
          ## add rowvar to cvars2keep
          cvars2keep <- c(cvars2keep, row.orderby)	

          ## Build query for getting unique values within population
          uniquerow.qry <- 
		          paste0("SELECT DISTINCT ", toString(c(row.orderby, rowvar)), 
		                 rowfromqry,
					           whereqry,
					           "\nORDER BY ", toString(c(row.orderby, rowvar)))
          if (!is.null(withqry)) {
            uniquerow.qry <- paste0(withqry,
                                    "\n", uniquerow.qry)
          }
		      rowvartmp <- row.orderby
		      row.orderby <- rowvar
		      rowvar <- rowvartmp

		      #message("getting unique values for ", rowvar, ":\n", uniquerow.qry, "\n")
 	        if (popdatindb) {
            uniquerow <- tryCatch(
               DBI::dbGetQuery(popconn, uniquerow.qry),
                          error=function(e) {
                            message(e,"\n")
                          return(NULL)})
            if (is.null(uniquerow)) {
              message("invalid unique rowvar query...")
              message(uniquerow.qry)
              stop()
            }
 	        } else {
            uniquerow <- tryCatch( 
                 sqldf::sqldf(uniquerow.qry, connection = NULL),
                          error=function(e) {
                            message("invalid uniquerow query...")
                            message(e,"\n")
                          return(NULL)})
            if (is.null(uniquerow)) {
              message("invalid unique rowvar query...")
              message(uniquerow.qry)
              stop()
            }
          }	
        }  ## end row.orderby != "NONE"
        
        if (rowvar %in% treeflds && estseed %in% c("add", "only")) {
          if (!is.null(seedx)) {

            if (!row.orderby %in% seedflds) {
              message(row.orderby, " not in seed")
              return(NULL)
            }	 
            
            ## Build query for getting unique values within population
            uniquerow.qry <- 
              paste0("SELECT DISTINCT ", toString(c(row.orderby, rowvar)), 
                     seedfromqry,
                     whereqry,
                     "\nORDER BY ", toString(c(row.orderby, rowvar)))
            if (!is.null(withqry)) {
              uniquerow.qry <- paste0(withqry,
                                      "\n", uniquerow.qry)
            }

            #message("getting unique values for ", rowvar, ":\n", uniquerow.qry, "\n")
            if (rowisdb) {
              uniquerow <- tryCatch(
                  DBI::dbGetQuery(popconn, uniquerow.qry)[[1]],
                           error=function(e) {
                             message(e,"\n")
                           return(NULL)})
              if (is.null(uniquerow)) {
                message("invalid unique rowvar query...")
                message(uniquerow.qry)
                stop()
              }
            } else {
              uniquerow <- tryCatch(
                  sqldf::sqldf(uniquerow.qry)[[1]],
                           error=function(e) {
                             message(e,"\n")
                           return(NULL)})
              if (is.null(uniquerow)) {
                message("invalid unique rowvar query...")
                message(uniquerow.qry)
                stop()
              }
            }
            
            if (estseed == "add" && rowvar == "DIACL" && is.data.frame(treex)) {
              seedclord <- min(treex[[row.orderby]]) - 0.5
              seedx[[row.orderby]] <- seedclord
            } else {
              if (estseed == "add" && is.data.frame(seedx) && rowvar=="DIACL" && !"DIACL" %in% seedflds) {
                seedx$DIACL <- seedclnm
              }
            }			
          } else {
            uniquerow <- NULL
          }
        }  ## end rowvar %in% treeflds & estseed in c("add", "only")
        
      } else {   ## !is.null(row.orderby) && row.orderby != "NONE"
        
        ## Build query for getting unique rowvar values within population
        uniquex.qry <- 
		      paste0("SELECT DISTINCT ", rowvar, 
		               rowfromqry,
			             whereqry,
				           "\nORDER BY ", rowvar)

		    #message("getting unique values for ", rowvar, ":\n", cuniquex.qry, "\n")
	      if (rowisdb) {
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
            message("invalid unique rowvar query...")
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
            message("invalid unique rowvar query...")
            message(uniquex.qry)
            stop()
          }
		    }

        ## Build query for getting unique rowclass values within population
        if (!is.null(row.classify)) {
          class. <- ifelse (rowvar %in% pltcondflds, "pc.", "t.")
            
          if (is.vector(row.classify)) {
            minx <- min(uniquex, na.rm=TRUE)
            #maxx <- max(uniquex, na.rm=TRUE)
            minbrk <- min(row.classify)
            #maxbrk <- max(row.classify)
            if (minx < minbrk) {
              minxmiss <- sort(unique(minx[minx < minbrk]))
              message("there are values in dataset less than class breaks defined for ", rowvar)
              message("...these values are classified as NA: ", toString(minxmiss))
            }
            #if (maxx > maxbrk) {
            #  maxxmiss <- sort(unique(maxx[maxx > maxbrk]))
            #  message("there are values in dataset greater than class breaks defined for ", rowvar)
            #  message("...these values are classified as NA: ", toString(maxxmiss))
            #}
            
            rowclassnm <- paste0(rowvar, "CL")
            rowclassqry <- classifyqry(classcol = rowvar,
                                       cutbreaks = row.classify,
                                       class. = class.,
                                       fill = NULL)
            
          } else if (is.data.frame(row.classify)) {

            if (ncol(row.classify) < 2) {
              message("invalid row.classify... must be a data.frame with TO and FROM columns")
              stop()
            }
            rowclassnm <- checknm(paste0(rowvar, "CL"), pltcondflds)
            names(row.classify) <- toupper(names(row.classify))
            if (!all(c("TO", "FROM") %in% names(row.classify))) {
              message("invalid classes for ", rowvar, 
                      "... the data.frame must include columns: FROM and TO")
              stop()
            }
            fromval <- row.classify$FROM
            toval <- row.classify$TO
            
            ## Check values of fromval
            if (!any(!is.na(uniquex) %in% fromval)) {
              missvals <- uniquex[which(!uniquex %in% fromval)]
              message("missing values in row.classify: ", toString(missvals))
            }
            rowclassqry <- classqry(classcol = rowvar, 
                                    fromval, toval, 
                                    classnm = rowclassnm, 
                                    class. = class.,
                                    fill = NULL)
          } else {
            message("invalid row.classify... must be a vector of class breaks or a data.frame with TO and FROM columns")
            stop()
          }
          
          ## Build query for getting unique rowclass values within population
          rowvarnm=rowvarnew <- rowclassnm
          uniquex.qry <- 
            paste0("SELECT DISTINCT \n", 
                   rowclassqry,
                   rowfromqry,
                   whereqry,
                   "\nORDER BY ", rowclassnm)
          
          
          ## get unique values for classified rowvar
          if (rowisdb) {
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
              message("invalid unique rowvar query...")
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
              message("invalid unique rowvar query...")
              message(uniquex.qry)
              stop()
            }
          }
        }

        if (any(is.na(uniquex)) && !keepNA) {
          uniquex <- uniquex[!is.na(uniquex)]
        }
        rowuniquex <- uniquex

        ## Check seedling table
        if (rowvar %in% seedflds && estseed == "add") {
          
          ## Build seedling from query
          if (!is.null(seedx)) {

            if (estseed == "add" && (rowvar == "DIACL" ||
                  (rowvar == "DIA" && !is.null(row.classify) && "DIACL" %in% names(row.classify))))  {
              suniquex <- "<1"
              seedflds <- c(seedflds, "DIACL")
            } else {  
              #if (!rowvar %in% seedflds) {
              #  message(rowvar, " not in seed")
              #  return(NULL)
              #}	  
              
              ## Build query for getting unique seedling rowvar values within population
              suniquex.qry <- 
                paste0("SELECT DISTINCT ", rowvar, 
                       seedfromqry,
                       "\nORDER BY ", rowvar)
              if (estseed == "only") {
                #message("getting unique values for ", rowvar, ":\n", suniquex.qry, "\n")
              }
              if (rowisdb) {
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
                  message("invalid unique rowvar query...")
                  message(suniquex.qry)
                  stop()
                }
                
              } else {
                suniquex <- tryCatch(
                    sqldf::sqldf(suniquex.qry, connection = NULL)[[1]],
                            error=function(e) {
                              message("invalid unique row query...")
                              message(e,"\n")
                            return(NULL)})
                if (is.null(suniquex)) {
                  message("invalid unique rowvar query...")
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
          rowuniquex <- sort(unique(c(uniquex, suniquex)))
        }
        
	      if (row.FIAname || !is.null(rowlut)) {
        
          if (!is.null(rowlut) && ncol(rowlut) > 1 && all(names(rowlut) %in% rowflds)) {
            if (is.null(row.orderby) || row.orderby == "NONE") {
              message("row.orderby is not defined... ordering by rowvar")
			        return(NULL)
            } else {

              if (row.orderby == rowvar) {
                row.name <- names(rowlut)[names(rowlut) != rowvar]
                if (length(row.name) > 1) {
				          message("invalid rowlut... only 2 columns allowed")
			         	}
                rowvarnm <- row.name
              }
            }
          } else {
            rowLUTgrp <- FALSE

            if (rowgrp) {
              if (!is.null(rowgrpnm)) {
                if (!rowgrpnm %in% rowflds) {
				          message(rowgrpnm, "not in ", rowtabnm)
				          return(NULL)
				        }  
                if (is.null(title.rowgrp)) {
				          title.rowgrp <- rowgrpnm
                }
                if (!is.null(rowgrpord)) {
                  if (!rowgrpord %in% rowflds) {
				            message(rowgrpord, "not in ", rowtabnm)
				          }
				        }
              } else {
                rowLUTgrp <- TRUE
              }
            }

            if (!is.null(rowlut)) row.add0 <- TRUE
            
            if (rowvar %in% treeflds) {
              
              if (rowvar == "GROWTH_HABIT_CD") {
                rowlut <- ref_growth_habit
                rowLUTnm <- "GROWTH_HABIT_NM"
                if (is.data.table(treex)) {
                  treex <- merge(treex, ref_growth_habit, by=rowvar, all.x=TRUE)
                  rowlut <- data.table(rowlut[rowlut[[rowvar]] %in% treex[[rowvar]], ])
                }
                rowlut <- rowlut[, lapply(.SD, makefactor)]
              } else {
                if (estseed != "only") {
                  if (!is.data.frame(treex)) { 
                    x <- treeflds 
                  } else { 
                    x <- treex 
                  } 
                  
                  if (rowvar == "SPCD") {
                    rowLUT <- datLUTspp(x = x, 
                                        add0 = row.add0, 
                                        xtxt = "tree", 
                                        uniquex = uniquex)
                  } else {
                    rowLUT <- datLUTnm(x = x, 
                                       xvar = rowvar, 
                                       LUT = rowlut, 
                                       FIAname = row.FIAname,
                                       group = rowLUTgrp, 
                                       add0 = row.add0, 
                                       xtxt = "tree", 
                                       uniquex = uniquex)
                  }
                  if (!rowisdb) {
                    treex <- setDT(rowLUT$xLUT)
                  }
                  rowlut <- setDT(rowLUT$LUT)
                  rowLUTnm <- rowLUT$xLUTnm
                } ## end estseed != only
                
                if (estseed %in% c("add", "only") && !is.null(seedx)) {
                  if (!is.data.frame(seedx)) { 
                    x <- seedflds 
                  } else { 
                    x <- seedx 
                  } 
                  if (rowvar %in% seedflds) {
                    if (rowvar == "SPCD") {
                      rowLUT <- datLUTspp(x = x, 
                                          add0 = row.add0, 
                                          xtxt = "seed", 
                                          uniquex = suniquex)
                    } else {            
                      rowLUT <- datLUTnm(x = x, 
                                         xvar = rowvar, 
                                         LUT = NULL, 
                                         FIAname = row.FIAname,
                                         group = rowLUTgrp, 
                                         add0 = row.add0, 
                                         xtxt = "seed", 
                                         uniquex = suniquex)
                    }  
                    rowluts <- setDT(rowLUT$LUT)
                    rowluts <- rowluts[!rowluts[[rowvar]] %in% rowlut[[rowvar]],]
                    rowLUTnm <- rowLUT$xLUTnm
                    if (nrow(rowluts) > 0) {
                      rowlut <- rbind(rowlut, rowluts)
                    }
                    if (!rowisdb) {
                      seedx <- rowLUT$xLUT
                    }       
                  } else if (rowvar == "DIACL") {
                    if (rowisdb) {
                      seedx$DIACL <- seedclnm
                    }
                  }
                }  ## end estseed %in% c("add", "only")
              }
            } else { ## rowvar in pltcondflds
              
              ## check for prefix (PREV_) xvar
              xvar <- ifelse(popType %in% c("CHNG"), sub("PREV_", "", rowvar), rowvar)

              rowLUT <- datLUTnm(x = rowflds, 
                                 xvar = xvar, 
                                 uniquex = uniquex,
                                 LUT = rowlut, 
                                 FIAname = row.FIAname,
                                 group = rowLUTgrp,
                                 add0 = row.add0)
              rowlut <- setDT(rowLUT$LUT)
              rowLUTnm <- rowLUT$xLUTnm
              
              ## append prefix
              if (popType %in% c("CHNG")) {
                names(rowlut) <- paste0("PREV_", names(rowlut))
                rowLUTnm <- paste0("PREV_", rowLUTnm)
              }
            }
            
            if (rowgrp) {
              rowgrpord <- rowLUT$grpcode
              rowgrpnm <- rowLUT$grpname
              if (all(sapply(rowlut[[rowgrpnm]], function(x) x == "")) || 								
                  all(is.na(rowlut[[rowgrpnm]]))) {
                stop("no groups for ", rowvar)
              }
              title.rowgrp <- ifelse (rowgrpord %in% ref_titles[["DOMVARNM"]], 
                                      ref_titles[ref_titles[["DOMVARNM"]] == rowgrpord, "DOMTITLE"], rowgrpnm)
            }
            
            if (is.null(row.orderby) || row.orderby == "NONE") {
              if (!is.null(rowLUTnm)) {
                row.orderby <- rowvar
                rowvarnm <- rowLUTnm
              }
              if (row.orderby == rowvar) {
                row.name <- names(rowlut)[names(rowlut) != rowvar]
                if (length(row.name) > 1) {
                  message("invalid rowlut... only 2 columns allowed")
                  return(NULL)
                }
                rowvarnm <- row.name
              }
            } else {
              if (!row.orderby %in% names(rowlut)) {
                message("row.orderby not in rowlut")
                return(NULL)
              }
            }
          }
	        # if (!is.null(NA.FIAname)) {
	        #   if (any(is.na(rowlut[[rowvar]]))) {
	        #     if (is.character(rowlut[[rowvarnm]]) && 
	        #         (is.na(rowlut[is.na(rowlut[[rowvar]])][[rowvarnm]]) || rowlut[is.na(rowlut[[rowvar]])][[rowvarnm]] == "NA")) {
	        #       rowlut[is.na(rowlut[[rowvar]])][[rowvarnm]] <- NA.FIAname
	        #     }
	        #   } else {
	        #     rowlut <- rbind(rowlut, list(NA, NA.FIAname))
	        #   }
	        # }
	      }  ## end if (row.FIAname & !is.null(rowlut))
      }  ## end !is.null(row.orderby) && row.orderby != "NONE"
    }  ## end domlut is null
  } ## end rowvar != "NONE"      

 
  ##############################################################
  ## COLUMN VARIABLE
  ##############################################################
  uniquecol <- NULL
  if (!popType %in% c("CHNG", "GRM")) {
    if (!is.null(colvar) && colvar == rowvar) {
      stop("colvar must be different than rowvar")
    }
    varlst <- varlst[which(!varlst %in% rowvar)]
  }
  colvar <- pcheck.varchar(var2check=colvar, varnm="colvar", gui=gui,
		checklst=c("NONE", varlst), caption="Column variable",
		warn=paste(colvar, "not found"))
  if (is.null(colvar)) colvar <- "NONE"

  if (colvar != "NONE") {
    coluniquex <- NULL
    colvarnm=colvarnew <- colvar
	
    if (!is.null(col.FIAname) && col.FIAname) {
      ## Get FIA reference table for xvar

      xvar.ref <- getRefobject(toupper(colvar))
      if (is.null(xvar.ref) && !toupper(colvar) %in% c("SPCD", "GROWTH_HABIT_CD")) {
        message(paste("no reference name for", colvar))
        col.FIAname <- FALSE
      }
    }

    ## Check to make sure there is a rowvar when there is a colvar
    if (rowvar == "TOTAL") stop("no rowvar, use colvar as rowvar")
    if (is.null(col.orderby)) col.orderby <- "NONE"

    ## GET column titles defined in FIESTA
    ###################################################
    if (is.null(title.colvar)) {
      title.colvar <- ifelse (colvar %in% ref_titles[["DOMVARNM"]],
		                      ref_titles[ref_titles[["DOMVARNM"]] == colvar, "DOMTITLE"],
		                  ifelse (sub("PREV_", "", colvar) %in% ref_titles[["DOMVARNM"]],
		                         paste0("Previous ", tolower(ref_titles[ref_titles[["DOMVARNM"]] ==
			                           sub("PREV_", "", colvar), "DOMTITLE"])), colvar))
    }

    ## Check collut
    if (!is.null(collut)) {
      if (is.vector(collut) && length(collut) > 1) {
        collut <- data.table(collut)
        names(collut) <- colvar
        setkeyv(collut, colvar)
      } else {
        collut <- pcheck.table(collut, gui=gui, tabnm=collut, caption="Column look up?")
      }
    }

    ## domlut defines columns in cond to use for codes, code names, and table titles
    ##################################################################################
    if (!is.null(domlut)) {
      
      if (!colvar %in% domvarlst) stop(paste(colvar, "is not in domlut"))
      if (colvar %in% domlut[["DOMCODE"]]) {
        col.orderby <- colvar
        title.colvar <- as.character(domlut[match(colvar, domlut[["DOMCODE"]]), "DOMTITLE"])
        colvar <- as.character(domlut[match(colvar, domlut[["DOMCODE"]]), "DOMNAME"])
        if (!colvar %in% pltcondflds) {
          warning(paste(colvar, "not in cond table... using code"))
          colvarnm <- col.orderby
          col.orderby <- NULL
        }
      } else if (colvar %in% domlut[["DOMNAME"]]) {
        col.orderby <- as.character(domlut[match(colvar, domlut[["DOMNAME"]]), "DOMCODE"])
        title.colvar <- as.character(domlut[match(colvar, domlut[["DOMNAME"]]), "DOMTITLE"])
        if (!col.orderby %in% pltcondflds) {
          warning(paste(col.orderby, "not in cond table... ordering by name"))
          col.orderby <- NULL
        }
      }
    } else {  ## domlut is null

      ## Build fromqry for colvar 
      if (colvar %in% pltcondflds) {
        bypcdom <- TRUE
        colisdb <- isdbc
        colflds <- pltcondflds
        if (!colisdb) {
          coltabnm <- "pltcondx"
        } else {
          coltabnm <- pltcondx
        }
        joinid <- cuniqueid
        
        ## add rowvar to cvars2keep
        cvars2keep <- c(cvars2keep, colvar)
        
        ## build column fromqry
        colfromqry <- paste0(
          "\nFROM ", coltabnm, " pc")

      } else {
        if (estseed == "only" && colvar %in% seedflds) {
          bytdom <- TRUE
          colisdb <- isdbt
          colflds <- seedflds
          if (!colisdb) {
            coltabnm <- "seedx"
          } else {
            coltabnm <- seedx
          }
        } else if (colvar %in% treeflds) {
          bytdom <- TRUE
          colisdb <- isdbt
          colflds <- treeflds
          if (!colisdb) {
            coltabnm <- "treex"
          } else {
            coltabnm <- treex
          }
          if (estseed == "add") {
            if (!colisdb) {
              seedtabnm <- "seedx"
            } else {
              seedtabnm <- seedx
            }
          }
        }
        
        ## build col fromqry
        colfromqry <- paste0(
          "\nFROM ", SCHEMA., coltabnm, " t")
        
        if (!is.null(pltcondx)) {
          coltjoinqry <- getjoinqry(c(tuniqueid, condid), c(cuniqueid, condid), "t.", "pc.")
          colfromqry <- paste0(
            colfromqry,
            "\nJOIN ", pltcondxnm, " pc ", coltjoinqry)
        }
        
        if (estseed == "add") {
          seedfromqry <- paste0(
            "\nFROM ", SCHEMA., seedtabnm, " s")
          
          if (!is.null(pltcondx)) {
            colsjoinqry <- getjoinqry(c(tuniqueid, condid), c(cuniqueid, condid), "s.", "pc.")
            seedfromqry <- paste0(
              seedfromqry,
              "\nJOIN ", pltcondxnm, " pc ", colsjoinqry)
          }
        }
      }  
      
      ## Check col.orderby
      if (!is.null(col.orderby) && col.orderby != "NONE") {
        if (col.orderby == colvar) {
          message("col.orderby must be different than colvar")
          col.orderby <- "NONE"
        }	  
        if (col.orderby != "NONE") {
          if (!col.orderby %in% pltcondflds) {
            message("col.orderby must be in cond")
            return(NULL)
          }
          
          ## add colvar to cvars2keep
          cvars2keep <- c(cvars2keep, col.orderby)	
          
          ## Build query for getting unique values within population
          uniquecol.qry <- 
            paste0("SELECT DISTINCT ", toString(c(col.orderby, colvar)), 
                   colfromqry,
                   whereqry,
                   "\nORDER BY ", toString(c(col.orderby, colvar)))
          colvartmp <- col.orderby
          col.orderby <- colvar
          colvar <- colvartmp
          
          #message("getting unique values for ", colvar, ":\n", uniquecol.qry, "\n")
          if (colisdb) {
            if (!is.null(withqry)) {
              uniquecol.qry <- paste0(withqry, 
                                      "\n", uniquecol.qry)
            }
            uniquecol <- tryCatch(
              DBI::dbGetQuery(popconn, uniquecol.qry),
              error=function(e) {
                message(e,"\n")
                return(NULL)})
            if (is.null(uniquecol)) {
              message("invalid uniquecol query...")
              message(uniquecol.qry)
              stop()
            }
          } else {
            uniquecol <- tryCatch( 
              sqldf::sqldf(uniquecol.qry, connection = NULL),
              error=function(e) {
                message(e,"\n")
                return(NULL)})
            if (is.null(uniquecol)) {
              message("invalid uniquecol query...")
              message(uniquecol.qry)
              stop()
            }
          }	
        }  ## end col.orderby != "NONE"
       
        if (colvar %in% treeflds && estseed %in% c("add", "only")) {
          if (!is.null(seedx)) {
            if (!col.orderby %in% seedflds) {
              message(col.orderby, " not in seed")
              return(NULL)
            }	 

            ## Build query for getting unique values within population
            uniquecol.qry <- 
              paste0("SELECT DISTINCT ", toString(c(col.orderby, colvar)), 
                     seedfromqry,
                     whereqry,
                     "\nORDER BY ", toString(c(col.orderby, colvar)))
            if (!is.null(withqry)) {
              uniquecol.qry <- paste0(withqry,
                                      "\n", uniquecol.qry)
            }

            #message("getting unique values for ", colvar, ":\n", uniquecol.qry, "\n")
            if (colisdb) {
              uniquecol <- tryCatch(
                DBI::dbGetQuery(popconn, uniquecol.qry)[[1]],
                error=function(e) {
                  message(e,"\n")
                  return(NULL)})
              if (is.null(uniquecol)) {
                message("invalid uniquecol query...")
                message(uniquecol.qry)
                stop()
              }
            } else {
              uniquecol <- tryCatch(
                sqldf::sqldf(uniquecol.qry)[[1]],
                error=function(e) {
                  message("invalid uniquecol query...")
                  message(e,"\n")
                  return(NULL)})
              if (is.null(uniquecol)) {
                message("invalid uniquecol query...")
                message(uniquecol.qry)
                stop()
              }
            }
            
            if (estseed == "add" && colvar == "DIACL" && is.data.frame(treex)) {
              seedclord <- min(treex[[col.orderby]]) - 0.5
              seedx[[col.orderby]] <- seedclord
            } else {
              if (estseed == "add" && is.data.frame(seedx) && colvar=="DIACL" && !"DIACL" %in% seedflds) {
                seedx$DIACL <- seedclnm
              }
            }			
          } else {
            uniquecol <- NULL
          }
        }  ## end colvar %in% treeflds & estseed in c("add", "only")
        
      } else {   ## !is.null(col.orderby) && col.orderby != "NONE"

        ## Build query for getting unique colvar values within population
        uniquex.qry <- 
          paste0("SELECT DISTINCT ", colvar, 
                 colfromqry,
                 whereqry,
                 "\nORDER BY ", colvar)

        #message("getting unique values for ", colvar, ":\n", cuniquex.qry, "\n")
        if (colisdb) {
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
            message("invalid unique col query...")
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
            message("invalid unique col query...")
            message(uniquex.qry)
            stop()
          }
        }

        if (!is.null(col.classify)) {
          class. <- ifelse (colvar %in% pltcondflds, "pc.", "t.")
            
          if (is.vector(col.classify)) {
            minx <- min(uniquex, na.rm=TRUE)
            #maxx <- max(uniquex, na.rm=TRUE)
            minbrk <- min(col.classify)
            #maxbrk <- max(col.classify)
            if (minx < minbrk) {
              minxmiss <- sort(unique(minx[minx < minbrk]))
              message("there are values in dataset less than class breaks defined for ", colvar)
              message("...these values are classified as NA: ", toString(minxmiss))
            }
            #if (maxx > maxbrk) {
            #  maxxmiss <- sort(unique(maxx[maxx > maxbrk]))
            #  message("there are values in dataset greater than class breaks defined for ", colvar)
            #  message("...these values are classified as NA: ", toString(maxxmiss))
            #}
            
            colclassnm <- paste0(colvar, "CL")
            colclassqry <- classifyqry(classcol = colvar,
                                       cutbreaks = col.classify,
                                       class. = class.,
                                       fill = NULL)
            
          } else if (is.data.frame(col.classify)) {
            if (ncol(col.classify) < 2) {
              message("invalid col.classify... must be a data.frame with TO and FROM columns")
              stop()
            }
            colclassnm <- checknm(paste0(colvar, "CL"), pltcondflds)
            names(col.classify) <- toupper(names(col.classify))
            if (!all(c("TO", "FROM") %in% names(col.classify))) {
              message("invalid classes for ", colvar, 
                      "... the data.frame must include columns: FROM and TO")
              stop()
            }
            fromval <- col.classify$FROM
            toval <- col.classify$TO
            
            ## Check values of fromval
            if (!any(!is.na(uniquex) %in% fromval)) {
              missvals <- uniquex[which(!uniquex %in% fromval)]
              message("missing values in row.classify: ", toString(missvals))
            }
            colclassqry <- classqry(colvar, fromval, toval, 
                                    classnm = colclassnm, 
                                    class. = class.,
                                    fill = NULL)
          } else {
            message("invalid col.classify... must be a vector of class breaks or a data.frame with TO and FROM columns")
            stop()
          }
          
          ## Build query for getting unique colclass values within population
          colvarnm=colvarnew <- colclassnm
          uniquex.qry <- 
            paste0("SELECT DISTINCT \n", 
                   colclassqry,
                   colfromqry,
                   whereqry,
                   "\nORDER BY ", colclassnm)
          
          ## get unique values for classified colvar
          if (colisdb) {
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
              message("invalid unique colvar query...")
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
              message("invalid unique colvar query...")
              message(uniquex.qry)
              stop()
            }
          }
        }
       
        if (any(is.na(uniquex)) && !keepNA) {
          uniquex <- uniquex[!is.na(uniquex)]		
        }
        coluniquex <- uniquex
        
        ## Check seedling table
        if (colvar %in% treeflds && estseed == "add") {
          
          ## Build seedling from query
          if (!is.null(seedx)) {

            if (estseed == "add" && (colvar == "DIACL" ||
                (colvar == "DIA" && !is.null(col.classify) && "DIACL" %in% names(col.classify)))) {
              suniquex <- "<1"
              seedflds <- c(seedflds, "DIACL")
            } else {  
              #if (!colvar %in% seedflds) {
              #  message(colvar, " not in seed")
              #  return(NULL)
              #}	  
              
              ## Build query for getting unique seedling colvar values within population
              suniquex.qry <- 
                paste0("SELECT DISTINCT ", colvar, 
                       seedfromqry,
                       "\nORDER BY ", colvar)
              if (estseed == "only") {
                #message("getting unique values for ", colvar, ":\n", suniquex.qry, "\n")
              }
              if (colisdb) {
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
                  message("invalid unique colvar query...")
                  message(suniquex.qry)
                  stop()
                }
                
              } else {
                suniquex <- tryCatch(
                  sqldf::sqldf(suniquex.qry, connection = NULL)[[1]],
                  error=function(e) {
                    message(e,"\n")
                    return(NULL)})
                if (is.null(suniquex)) {
                  message("invalid unique colvar query...")
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
          coluniquex <- sort(unique(c(uniquex, suniquex)))
        }
        if (col.FIAname || !is.null(collut)) {
          
          if (!is.null(collut) && ncol(collut) > 1 && all(names(collut) %in% colflds)) {
            if (is.null(col.orderby) || col.orderby == "NONE") {
              message("col.orderby is not defined... ordering by colvar")
              return(NULL)
            } else {
              
              if (col.orderby == colvar) {
                col.name <- names(collut)[names(collut) != colvar]
                if (length(col.name) > 1) {
                  message("invalid collut... only 2 columns allowed")
                }
                colvarnm <- col.name
              }
            }
          } else {
            colLUTgrp <- FALSE
            
            if (colgrp) {
              if (!is.null(colgrpnm)) {
                if (!colgrpnm %in% colflds) {
                  message(colgrpnm, "not in ", coltabnm)
                  return(NULL)
                }  
                if (is.null(title.colgrp)) {
                  title.colgrp <- colgrpnm
                }
                if (!is.null(colgrpord)) {
                  if (!colgrpord %in% colflds) {
                    message(colgrpord, "not in ", coltabnm)
                  }
                }
              } else {
                colLUTgrp <- TRUE
              }
            }
            
            if (!is.null(collut)) col.add0 <- TRUE
            
            if (colvar %in% treeflds) {
              
              if (colvar == "GROWTH_HABIT_CD") {
                collut <- ref_growth_habit
                colLUTnm <- "GROWTH_HABIT_NM"
                if (is.data.table(treex)) {
                  treex <- merge(treex, ref_growth_habit, by=colvar, all.x=TRUE)
                  collut <- data.table(collut[collut[[colvar]] %in% treex[[colvar]], ])
                }
                collut <- collut[, lapply(.SD, makefactor)]
              } else {
                if (estseed != "only") {
                  if (!is.data.frame(treex)) { 
                    x <- treeflds 
                  } else { 
                    x <- treex 
                  } 
                  
                  if (colvar == "SPCD") {
                    colLUT <- datLUTspp(x = x, 
                                        add0 = col.add0, 
                                        xtxt = "tree", 
                                        uniquex = coluniquex)
                  } else {
                    colLUT <- datLUTnm(x = x, 
                                       xvar = sub("PREV_", "", colvar), 
                                       LUT = collut, 
                                       FIAname = col.FIAname,
                                       group = colLUTgrp, 
                                       add0 = col.add0, 
                                       xtxt = "tree", 
                                       uniquex = coluniquex)
                  }
                  if (!colisdb) {
                    treex <- setDT(colLUT$xLUT)
                  }
                  collut <- setDT(colLUT$LUT)
                  colLUTnm <- colLUT$xLUTnm
                } ## end estseed != only
                
                if (estseed %in% c("add", "only") && !is.null(seedx)) {
                  if (!is.data.frame(seedx)) { 
                    x <- seedflds 
                  } else { 
                    x <- seedx 
                  } 
                  if (colvar %in% seedflds) {
                    if (colvar == "SPCD") {
                      colLUT <- datLUTspp(x = x, 
                                          add0 = col.add0, 
                                          xtxt = "seed", 
                                          uniquex = suniquex)
                    } else {            
                      colLUT <- datLUTnm(x = x, 
                                         xvar = colvar, 
                                         LUT = NULL, 
                                         FIAname = col.FIAname,
                                         group = colLUTgrp, 
                                         add0 = col.add0, 
                                         xtxt = "seed", 
                                         uniquex = suniquex)
                    }  
                    colluts <- setDT(colLUT$LUT)
                    colluts <- colluts[!colluts[[colvar]] %in% collut[[colvar]],]
                    colLUTnm <- colLUT$xLUTnm
                    if (ncol(colluts) > 0) {
                      collut <- rbind(collut, colluts)
                    }
                    if (!colisdb) {
                      seedx <- colLUT$xLUT
                    }       
                  } else if (colvar == "DIACL") {
                    if (colisdb) {
                      seedx$DIACL <- seedclnm
                    }
                  }
                }  ## end estseed %in% c("add", "only")
              }
            } else { ## colvar in pltcondflds
              
              colLUT <- datLUTnm(x = colflds, 
                                 xvar = colvar, 
                                 uniquex = uniquex,
                                 LUT = collut, 
                                 FIAname = col.FIAname,
                                 group = colLUTgrp,
                                 add0 = col.add0)
              collut <- setDT(colLUT$LUT)
              colLUTnm <- colLUT$xLUTnm
            }
            
            if (colgrp) {
              colgrpord <- colLUT$grpcode
              colgrpnm <- colLUT$grpname
              if (all(sapply(collut[[colgrpnm]], function(x) x == "")) || 								
                  all(is.na(collut[[colgrpnm]]))) {
                stop("no groups for ", colvar)
              }
              title.colgrp <- ifelse (colgrpord %in% ref_titles[["DOMVARNM"]], 
                                      ref_titles[ref_titles[["DOMVARNM"]] == colgrpord, "DOMTITLE"], colgrpnm)
            }
            
            if (is.null(col.orderby) || col.orderby == "NONE") {
              if (!is.null(colLUTnm)) {
                col.orderby <- colvar
                colvarnm <- colLUTnm
              }
              if (col.orderby == colvar) {
                col.name <- names(collut)[names(collut) != colvar]
                if (length(col.name) > 1) {
                  message("invalid collut... only 2 columns allowed")
                  return(NULL)
                }
                colvarnm <- col.name
              }
            } else {
              if (!col.orderby %in% names(collut)) {
                message("col.orderby not in collut")
                return(NULL)
              }
            }
          }
        }
      }
    }  ## end domlut is null
  } ## end colvar != "NONE"      
 

  ###################################################################################
  ## GET DOMAIN. CONCATENATE ROWVAR & COLVAR VARIABLES IF THEY ARE IN THE SAME TABLE.
  ###################################################################################
  if (colvar == "NONE") {
    if (rowvar %in% c(treeflds, seedflds))
      tdomvar <- rowvar
  } else {
    grpvar <- c(rowvar, colvar)

    ## If rowvar and colvar both in cond table, concatenate columns for calculation.
    if (all(c(rowvar, colvar) %in% pltcondflds))
      cvars2keep <- c(cvars2keep, grpvar)

    if (esttype %in% c("TREE", "RATIO")) {
      ## If rowvar and colvar both in tree table, concatenate columns for calculation.
      if (all(c(rowvar, colvar) %in% c(treeflds, seedflds))) {
        #setkeyv(treex, c(rowvar, colvar))
        tdomvar <- rowvar
        tdomvar2 <- colvar
      } else if (any(c(rowvar, colvar) %in% treeflds)) {
        if (rowvar %in% treeflds) {
          tdomvar <- rowvar
        } else {
          tdomvar <- colvar
        }
      }
    }
  }
  
  ## Define domainlst
  domainlst <- c(domainlst, rowvar, colvar)
  domainlst <- domainlst[domainlst != "NONE"]


  ############################################################################
  ## Get uniquerow and uniquecol
  ############################################################################

  ## uniquerow
  #########################################################
  if (!is.null(rowlut)) {
#    if (sum(unlist(lapply(rowlut, duplicated))) > 0) {
#      print(rowlut)
#      stop("invalid rowlut... no duplicates allowed")
#    }
    uniquerow <- rowlut
    if (all(!is.factor(uniquerow[[rowvar]]), row.orderby != "NONE", 
	         row.orderby %in% names(uniquerow))) {
	    setorderv(uniquerow, row.orderby, na.last=TRUE)
    }
  } else if (!is.null(uniquerow)) {
    uniquerow <- setDT(uniquerow)
    if (!is.null(row.orderby) && row.orderby != "NONE" && 
	             row.orderby %in% names(uniquerow)) {
      setkeyv(uniquerow, c(rowgrpnm, row.orderby))
    }
    
  } else if (!is.null(rowuniquex)) {
    uniquerow <- as.data.table(rowuniquex)
    names(uniquerow) <- rowvarnew

    if (rowvar == "GROWTH_HABIT_CD") {
      ghcodes <- ref_growth_habit[[rowvar]]
      ghord <- ghcodes[ghcodes %in% rowuniquex]
      if (length(ghord) < length(rowuniquex)) {
        missgh <- rowuniquex[!rowuniquex %in% ghord]
        message("growth_habit_cd not in ref: ", toString(missgh)) 
      } else {		  
        rowuniquex <- rowuniquex[match(ghord, rowuniquex)]
      }
    }
    uniquerow[[rowvarnew]] <- factor(uniquerow[[rowvarnew]], levels=rowuniquex)
    if (factor.addNA && any(!is.na(uniquerow[[rowvarnew]]))) {
      uniquerow[[rowvarnew]] <- addNA(uniquerow[[rowvarnew]])
    }
    setkeyv(uniquerow, rowvarnew)
    
  } else if (rowvar %in% pltcondflds && is.data.frame(pltcondx)) {
    if (!is.null(row.orderby) && row.orderby != "NONE") {
      uniquerow <- unique(pltcondx[,c(rowgrpord, rowgrpnm, row.orderby, rowvar), with=FALSE])
      setkeyv(uniquerow, c(rowgrpord, rowgrpnm, row.orderby))
    } else if (is.data.frame(pltcondx)) {	
      if (is.factor(pltcondx[[rowvar]])) {
        uniquerow <- as.data.table(levels(pltcondx[[rowvar]]))
        names(uniquerow) <- rowvar
        uniquerow[[rowvar]] <- factor(uniquerow[[rowvar]], levels=levels(pltcondx[[rowvar]]))
      } else {
        #rowvals <- na.omit(unique(pltcondx[, rowvar, with=FALSE]))
        rowvals <- unique(pltcondx[, rowvar, with=FALSE])
		    setorderv(rowvals, rowvar, na.last=TRUE)
        uniquerow <- as.data.table(rowvals)
        names(uniquerow) <- rowvar
        setkeyv(uniquerow, rowvar)
      }
    }
  } else if (rowvar %in% treeflds) {
    if (!is.null(row.orderby) && row.orderby != "NONE") {
	    if (estseed == "only" && is.data.frame(seedx)) {
        uniquerow <- unique(seedx[,c(rowgrpord, rowgrpnm, row.orderby, rowvar), with=FALSE])
        setkeyv(uniquerow, c(rowgrpord, rowgrpnm, row.orderby))
	    } else {
        uniquerow <- unique(treex[,c(rowgrpord, rowgrpnm, row.orderby, rowvar), with=FALSE])
        setkeyv(uniquerow, c(rowgrpord, rowgrpnm, row.orderby))
		
        if (estseed == "add" && !is.null(seedx) && is.data.frame(seedx)) {
          if (all(c(rowvar, row.orderby) %in% names(seedx)) && rowvar == "DIACL") {
            if (is.factor(uniquerow[[rowvar]])) {
              levels(uniquerow[[rowvar]]) <- c(seedclnm, levels(uniquerow[[rowvar]]))
            }
            if (is.factor(uniquerow[[row.orderby]])) {
              levels(uniquerow[[row.orderby]]) <- c(seedclord, levels(uniquerow[[row.orderby]]))
            }
            uniqueseed <- data.table(seedclord, seedclnm)
            setkeyv(uniqueseed, c(col.orderby, colvar))
            uniquerow <- rbindlist(list(uniqueseed, uniquerow))
          }
		    }
      }
    } else if (!is.null(uniquerow) && is.data.frame(treex)) {
	  
	    if (is.factor(treex[[rowvar]])) {
        if (estseed == "add" && rowvar == "DIACL") {
          rowlevels <- c(seedclnm, levels(treex[[rowvar]]))
        } else {
          rowlevels <- levels(treex[[rowvar]])
        }
        #uniquerow <- as.data.table(rowlevels)
        #names(uniquerow) <- rowvar
        uniquerow[[rowvar]] <- factor(uniquerow[[rowvar]], levels=rowlevels)
        uniquerow[[rowvar]] <- sort(uniquerow[[rowvar]])
      } else {
        if (estseed == "add" && rowvar == "DIACL") {
          rowvals <- c(seedclnm, sort(na.omit(unique(treex[, rowvar, with=FALSE][[1]]))))
        } else {
          rowvals <- sort(na.omit(unique(treex[, rowvar, with=FALSE][[1]])))
        }
        uniquerow <- as.data.table(rowvals)
        names(uniquerow) <- rowvar
        uniquerow[[rowvar]] <- factor(uniquerow[[rowvar]], levels=rowvals)
        uniquerow[[rowvar]] <- sort(uniquerow[[rowvar]])
        setkeyv(uniquerow, rowvar)
      }
    } 
	
	} else if (is.data.frame(treex)) {
	  if (is.factor(treex[[rowvar]])) {
      if (estseed == "add" && rowvar == "DIACL") {
        rowlevels <- c(seedclnm, levels(treex[[rowvar]]))
      } else {
        rowlevels <- levels(treex[[rowvar]])
      }
      uniquerow <- as.data.table(rowlevels)
      names(uniquerow) <- rowvar
      uniquerow[[rowvar]] <- factor(uniquerow[[rowvar]], levels=rowlevels)
      uniquerow[[rowvar]] <- sort(uniquerow[[rowvar]])
    } else {
      if (estseed == "add" && rowvar == "DIACL") {
        rowvals <- c(seedclnm, sort(na.omit(unique(treex[, rowvar, with=FALSE][[1]]))))
      } else {
        rowvals <- sort(na.omit(unique(treex[, rowvar, with=FALSE][[1]])))
      }
      uniquerow <- as.data.table(rowvals)
      names(uniquerow) <- rowvar
      uniquerow[[rowvar]] <- factor(uniquerow[[rowvar]], levels=rowvals)
      uniquerow[[rowvar]] <- sort(uniquerow[[rowvar]])
      setkeyv(uniquerow, rowvar)
    }		
  }

  ## Check for duplicate values
  if (!popdatindb && any(duplicated(uniquerow[[rowvar]]))) {
    pcfields <- names(condx)
    tfields <- names(treex)
    dupvals <- uniquerow[[rowvar]][duplicated(uniquerow[[rowvar]])]
    for (dup in dupvals) {       
      vals <- uniquerow[uniquerow[[rowvar]] == dup, row.orderby, with=FALSE][[1]]
      val <- vals[length(vals)]
      vals2chg <- vals[-length(vals)]

      if (any(c(rowvar, row.orderby) %in% pcfields)) {
        if (row.orderby %in% pcfields) {
          if (class(condx[[row.orderby]]) != class(val)) {
            class(val) <- class(condx[[row.orderby]])
          } 
          condx[condx[[row.orderby]] %in% vals2chg, row.orderby] <- val
        } else {
          if (class(condx[[rowvar]]) != class(val)) {
            class(val) <- class(condx[[rowvar]])
          } 
          condx[condx[[rowvar]] %in% vals2chg, rowvar] <- val
        }
      }
      if (any(c(rowvar, row.orderby) %in% tfields)) {
        if (row.orderby %in% tfields) {
          if (class(treex[[row.orderby]]) != class(val)) {
            class(val) <- class(treex[[row.orderby]])
          } 
          treex[treex[[row.orderby]] %in% vals2chg, row.orderby] <- val
        } else {
          if (class(treex[[rowvar]]) != class(val)) {
            class(val) <- class(treex[[rowvar]])
          } 
          treex[condx[[rowvar]] %in% vals2chg, rowvar] <- val
        }
      }
      uniquerow <- uniquerow[!uniquerow[[row.orderby]] %in% vals2chg, ] 
    }
  }

  ## uniquecol
  #########################################################
  if (!is.null(collut)) {
#    if (sum(unlist(lapply(collut, duplicated))) > 0) {
#      print(collut)
#      stop("invalid collut... no duplicates allowed")
#    }
    uniquecol <- collut
    if (all(!is.factor(uniquecol[[colvar]]), col.orderby != "NONE", 
	         col.orderby %in% names(uniquecol))) {
	    setorderv(uniquecol, col.orderby, na.last=TRUE)
	  }
  } else if (!is.null(uniquecol)) {
    uniquecol <- setDT(uniquecol)
    if (!is.null(col.orderby)  && col.orderby != "NONE" && 
        col.orderby %in% names(uniquecol))
      setkeyv(uniquecol, col.orderby)
    
  } else if (!is.null(coluniquex)) {
    uniquecol <- as.data.table(coluniquex)
    names(uniquecol) <- colvarnew
    
    if (colvar == "GROWTH_HABIT_CD") {
      ghcodes <- ref_growth_habit[[colvar]]
      ghord <- ghcodes[ghcodes %in% coluniquex]
      if (length(ghord) < length(coluniquex)) {
        missgh <- coluniquex[!coluniquex %in% ghord]
        message("growth_habit_cd not in ref: ", toString(missgh)) 
      } else {		  
        coluniquex <- rowuniquex[match(ghord, coluniquex)]
      }
    }
    uniquecol[[colvarnew]] <- factor(uniquecol[[colvarnew]], levels=coluniquex)
    if (factor.addNA && any(!is.na(uniquecol[[colvarnew]]))) {
      uniquecol[[colvarnew]] <- addNA(uniquecol[[colvarnew]])
    }
    setkeyv(uniquecol, colvarnew)
    
  } else if (colvar %in% pltcondflds && is.data.frame(pltcondx)) {

    if (!is.null(col.orderby) && col.orderby != "NONE") {
      uniquecol <- unique(pltcondx[, c(colvar, col.orderby), with=FALSE])
      setkeyv(uniquecol, col.orderby)
    } else {
      if (is.factor(pltcondx[[colvar]])) {
        uniquecol <- as.data.table(levels(pltcondx[[colvar]]))
        names(uniquecol) <- colvar
        uniquecol[[colvar]] <- factor(uniquecol[[colvar]], levels=levels(pltcondx[[colvar]]))
      } else if (is.data.frame(pltcondx)) {
        #colvals <- na.omit(unique(pltcondx[, colvar, with=FALSE]))
        colvals <- unique(pltcondx[, colvar, with=FALSE])
		    setorderv(colvals, colvar, na.last=TRUE)
        uniquecol <- as.data.table(colvals)
        names(uniquecol) <- colvar
        setkeyv(uniquecol, colvar)
      }
    }
  } else if (colvar %in% treeflds) {
    if (!is.null(col.orderby) && col.orderby != "NONE") {
      if (estseed == "only" && is.data.frame(seedx)) {
        uniquecol <- unique(seedx[,c(colgrpord, colgrpnm, col.orderby, colvar), with=FALSE])
        setkeyv(uniquecol, c(colgrpord, colgrpnm, col.orderby))
      } else {
        uniquecol <- unique(treex[,c(colgrpord, colgrpnm, col.orderby, colvar), with=FALSE])
        setkeyv(uniquecol, c(colgrpord, colgrpnm, col.orderby))
        
        if (estseed == "add" && !is.null(seedx)) {
          if (all(c(colvar, col.orderby) %in% names(seedx)) && colvar == "DIACL") {
            if (is.factor(uniquecol[[colvar]])) {
              levels(uniquecol[[colvar]]) <- c(seedclnm, levels(uniquecol[[colvar]]))
            }
            if (is.factor(uniquecol[[col.orderby]])) {
              levels(uniquecol[[col.orderby]]) <- c(seedclord, levels(uniquecol[[col.orderby]]))
            }
            uniqueseed <- data.table(seedclord, seedclnm)
            setkeyv(uniqueseed, c(col.orderby, colvar))
            uniquecol <- rbindlist(list(uniqueseed, uniquecol))
          }
        }
      }
    } else if (!is.null(uniquecol) && is.data.frame(treex)) {
	  
	    if (is.factor(treex[[colvar]])) {
        if (estseed == "add" && colvar == "DIACL") {
          collevels <- c(seedclnm, levels(treex[[colvar]]))
        } else {
          collevels <- levels(treex[[colvar]])
        }
        #uniquecol <- as.data.table(collevels)
        #names(uniquecol) <- colvar
        uniquecol[[colvar]] <- factor(uniquecol[[colvar]], levels=collevels)
        uniquecol[[colvar]] <- sort(uniquecol[[colvar]])
      } else {
        if (estseed == "add" && colvar == "DIACL") {
          colvals <- c(seedclnm, sort(na.omit(unique(treex[, colvar, with=FALSE][[1]]))))
        } else {
          colvals <- sort(na.omit(unique(treex[, colvar, with=FALSE][[1]])))
        }
        uniquecol <- as.data.table(colvals)
        names(uniquecol) <- colvar
        uniquecol[[colvar]] <- factor(uniquecol[[colvar]], levels=colvals)
        uniquecol[[colvar]] <- sort(uniquecol[[colvar]])
        setkeyv(uniquecol, colvar)
      }

	  } else if (is.data.frame(treex)) {
	    if (is.factor(treex[[colvar]])) {
        if (estseed == "add" && colvar == "DIACL") {
          collevels <- c(seedclnm, levels(treex[[colvar]]))
        } else {
          collevels <- levels(treex[[colvar]])
        }
        uniquecol <- as.data.table(collevels)
        names(uniquecol) <- colvar
        uniquecol[[colvar]] <- factor(uniquecol[[colvar]], levels=collevels)
        uniquecol[[colvar]] <- sort(uniquecol[[colvar]])
      } else {
        if (estseed == "add" && colvar == "DIACL") {
          colvals <- c(seedclnm, sort(na.omit(unique(treex[, colvar, with=FALSE][[1]]))))
        } else {
          colvals <- sort(na.omit(unique(treex[, colvar, with=FALSE][[1]])))
        }
        uniquecol <- as.data.table(colvals)
        names(uniquecol) <- colvar
        uniquecol[[colvar]] <- factor(uniquecol[[colvar]], levels=colvals)
        uniquecol[[colvar]] <- sort(uniquecol[[colvar]])
        setkeyv(uniquecol, colvar)
      }
    }		
  }

  if (!popdatindb && any(duplicated(uniquecol[[colvar]]))) {
    dupvals <- uniquecol[[colvar]][duplicated(uniquecol[[colvar]])]
    for (dup in dupvals) {       
      vals <- uniquecol[uniquecol[[colvar]] == dup, col.orderby, with=FALSE][[1]]
      val <- vals[length(vals)]
      vals2chg <- vals[-length(vals)]

      if (any(c(colvar, col.orderby) %in% names(condx))) {
        if (col.orderby %in% names(condx)) {
          if (class(condx[[col.orderby]]) != class(val)) {
            class(val) <- class(condx[[col.orderby]])
          } 
          condx[condx[[col.orderby]] %in% vals2chg, col.orderby] <- val
        } else {
          if (class(condx[[colvar]]) != class(val)) {
            class(val) <- class(condx[[colvar]])
          } 
          condx[condx[[colvar]] %in% vals2chg, colvar] <- val
        }
      }
      if (any(c(colvar, col.orderby) %in% names(treex))) {
        if (col.orderby %in% names(treex)) {
          if (class(treex[[col.orderby]]) != class(val)) {
            class(val) <- class(treex[[col.orderby]])
          } 
          treex[treex[[col.orderby]] %in% vals2chg, col.orderby] <- val
        } else {
          if (class(treex[[colvar]]) != class(val)) {
            class(val) <- class(treex[[colvar]])
          } 
          treex[treex[[colvar]] %in% vals2chg, colvar] <- val
        }
      }
      uniquecol <- uniquecol[!uniquecol[[col.orderby]] %in% vals2chg, ] 
    }
  }


  ## Create factors for ordering tables
  ##############################################################################
  if (!is.null(uniquerow)) {

    ## Change SITECLCD to descending order
    if (row.FIAname && "SITECLCD" %in% names(uniquerow))
      uniquerow <- setorder(uniquerow, -SITECLCD)
    if (row.FIAname && "GSSTKCD" %in% names(uniquerow))
      uniquerow <- setorder(uniquerow, -GSSTKCD)

    ## Create factors for ordering
	  uniquerow <- uniquerow[, lapply(.SD, makefactor)]
	  setkeyv(uniquerow, rowvarnew)
	  setorderv(uniquerow, rowvarnew, na.last=TRUE)
  }

  if (!is.null(uniquecol)) {

    ## Change SITECLCD to descending order
    if (col.FIAname && "SITECLCD" %in% names(uniquecol))
      uniquecol <- setorder(uniquecol, -SITECLCD)
    if (col.FIAname && "GSSTKCD" %in% names(uniquecol))
      uniquecol <- setorder(uniquecol, -GSSTKCD)

    ## Create factors for ordering
	  uniquecol <- uniquecol[, lapply(.SD, makefactor)]
	  setkeyv(uniquecol, colvarnew)
	  setorderv(uniquecol, colvarnew, na.last=TRUE)
  }

  ## Define pltcondvars
  pltcondvars = unique(c(cuniqueid, condid, cvars2keep, domainlst))
  

  returnlst <- list(pltcondvars = pltcondvars, 
                    bytdom = bytdom, bypcdom = bypcdom, 
                    domainlst = domainlst, 
                    uniquerow = uniquerow, uniquecol = uniquecol, 
                    rowvar = rowvar, rowvarnm = rowvarnm, 
                    colvar = colvar, colvarnm = colvarnm,
                    row.orderby = row.orderby, col.orderby = col.orderby,
                    row.add0 = row.add0, col.add0 = col.add0,
                    title.rowvar = title.rowvar, title.colvar = title.colvar,
                    rowgrpnm = rowgrpnm, title.rowgrp = title.rowgrp, 
                    tdomvar = tdomvar, tdomvar2 = tdomvar2, grpvar = grpvar)
  
  if (any(!is.null(row.classify), !is.null(col.classify))) {
    if (!is.null(row.classify)) {
      classifyrow <- list()
      classifyrow[["row.classify"]] <- row.classify
      classifyrow[["rowclassnm"]] <- rowclassnm
      classifyrow[["rowclassqry"]] <- rowclassqry
      returnlst$classifyrow <- classifyrow
    }
    if (!is.null(col.classify)) {
      classifycol <- list()
      classifycol[["col.classify"]] <- col.classify
      classifycol[["colclassnm"]] <- colclassnm
      classifycol[["colclassqry"]] <- colclassqry
      returnlst$classifycol <- classifycol
    }
  }

  return(returnlst)
}

