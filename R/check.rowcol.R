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
           factor.addNA = FALSE, spcdname = "COMMON_SCIENTIFIC"){

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
  domainlst=tdomvar=tdomvar2=grpvar=rowvarnm=colvarnm <- NULL
  gui <- FALSE

  ## Check for condid
  if (!is.null(condid) && !condid %in% c(treeflds, pltcondflds)) condid <- NULL
  if (!is.null(pltcondflds)) {
    if (!is.null(cuniqueid) && !cuniqueid %in% pltcondflds) stop("invalid cuniqueid")
  }
  if (!is.null(treex) && !is.null(tuniqueid) && !tuniqueid %in% treeflds) {
    stop("invalid tuniqueid")
  }
  #ref_titles <- FIESTAutils::ref_titles

  
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
    
    returnlst <- list(bytdom = FALSE, bypcdom = FALSE, 
                      domainlst = domainlst, 
                      uniquerow = NULL, uniquecol = NULL, 
                      rowvar = rowvar, rowvarnm = rowvar, colvar = colvar, 
                      row.orderby = row.orderby, col.orderby = col.orderby,
                      row.add0 = row.add0, col.add0 = col.add0,
                      title.rowvar = title.rowvar, title.colvar = title.colvar,
                      tdomvar = tdomvar)
    return(returnlst)
  }

  ##############################################################
  ## ROW VARIABLE
  ##############################################################
  if (rowvar != "NONE") { 
    
    ## if popType == "CHNG" and no colvar is defined...
    ## assume rowvar is previous rowvar and colvar is current rowvar 
    if (popType == "CHNG" && (is.null(colvar) || colvar == "NONE")) {
      colvar <- rowvar
      col.orderby <- row.orderby
      title.colvar <- title.rowvar
      col.FIAname <- row.FIAname
      col.add0 = row.add0
      collut <- rowlut
      col.classify <- row.classify
    }
    

    rowvardat <- 
      check.tabvar(popType = popType, tabvartype = "row", 
                   tabvar = rowvar, tab.orderby = row.orderby, 
                   tab.FIAname = row.FIAname, tablut = rowlut, 
                   tabgrp = rowgrp, tabgrpnm = rowgrpnm, tabgrpord = rowgrpord,
                   tab.add0 = row.add0, tab.classify = row.classify, 
                   title.tabvar = title.rowvar, title.tabgrp = title.rowgrp,
                   pltcondflds = pltcondflds, pltcondx = pltcondx,
                   cuniqueid = cuniqueid, cvars2keep = cvars2keep,
                   treex = treex, treeflds = treeflds, seedx = seedx,
                   seedflds = seedflds, estseed = estseed, 
                   tuniqueid = tuniqueid, whereqry = whereqry, withqry = withqry,
                   popdatindb = popdatindb, popconn = popconn, SCHEMA. = SCHEMA.,
                   domlut = domlut, domvarlst = domvarlst, spcdname = spcdname)
                              
    uniquerow <- rowvardat$uniquetabvar
    rowvar <- rowvardat$tabvar
    rowvarnm <- rowvardat$tabvarnm
    row.orderby <- rowvardat$tab.orderby
    title.rowvar <- rowvardat$title.tabvar
    rowgrpnm <- rowvardat$tabgrpnm
    title.rowgrp <- rowvardat$title.tabgrp
    cvars2keep <- rowvardat$cvars2keep
    rowclassnm <- rowvardat$tabclassnm
    rowclassqry <- rowvardat$tabclassqry
    bytdom <- rowvardat$bytdom
    bypcdom <- rowvardat$bypcdom
  }
  
  
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
    colvardat <- 
      check.tabvar(popType = popType, tabvartype = "col", 
                   tabvar = colvar, tab.orderby = col.orderby, 
                   tab.FIAname = col.FIAname, tablut = collut, 
                   tab.add0 = col.add0, tab.classify = col.classify, 
                   title.tabvar = title.colvar, 
                   pltcondflds = pltcondflds, pltcondx = pltcondx,
                   cuniqueid = cuniqueid, cvars2keep = cvars2keep,
                   treex = treex, treeflds = treeflds, seedx = seedx,
                   seedflds = seedflds, estseed = estseed, 
                   tuniqueid = tuniqueid, whereqry = whereqry, withqry = withqry,
                   popdatindb = popdatindb, 
                   popconn = popconn, SCHEMA. = SCHEMA.,
                   domlut = domlut, domvarlst = domvarlst)
  
    uniquecol <- colvardat$uniquetabvar
    colvar <- colvardat$tabvar
    colvarnm <- colvardat$tabvarnm
    col.orderby <- colvardat$tab.orderby
    title.colvar <- colvardat$title.tabvar
    colgrpnm <- colvardat$tabgrpnm
    title.colgrp <- colvardat$title.tabgrp
    cvars2keep <- colvardat$cvars2keep
    colclassnm <- colvardat$tabclassnm
    colclassqry <- colvardat$tabclassqry
    bytdom <- colvardat$bytdom
    bypcdom <- colvardat$bypcdom
  }
  
  ## Rename rowvar variables with prefix 'PREV_'
  if (popType %in% c("CHNG", "GRM")) {

    names(uniquerow) <- paste0("PREV_", names(uniquerow))
    rowvar <- paste0("PREV_", rowvar)
    rowvarnm <- paste0("PREV_", rowvarnm)
    if (!is.null(row.orderby)) {
      row.orderby <- paste0("PREV_", row.orderby)
    }
    if (!is.null(title.rowvar)) {
      title.rowvar <- paste0("Previous ", title.rowvar)
    }
  }


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
      bypcdom <- TRUE

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

