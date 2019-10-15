check.rowcol <- function(gui, esttype, treef=NULL, condf, cuniqueid="PLT_CN", 
	tuniqueid="PLT_CN", condid="CONDID", rowvar=NULL, rowvar.filter=NULL, colvar=NULL, 
	colvar.filter=NULL, row.FIAname=FALSE, col.FIAname=FALSE, row.orderby=NULL, 
	col.orderby=NULL, row.add0=FALSE, col.add0=FALSE, domvarlst=NULL, 
	domlut=NULL, title.rowvar=NULL, title.colvar=NULL, rowlut=NULL, collut=NULL, 
	rowgrp=FALSE, rowgrpnm=NULL, rowgrpord=NULL, title.rowgrp=NULL, landarea=NULL,
 	cvars2keep=NULL){

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
  SITECLCD=GSSTKCD=domainlst <- NULL

  ## Check for condid
  if (!is.null(condid) && !condid %in% c(names(treef), names(condf))) condid <- NULL
  if (!is.null(cuniqueid) && !cuniqueid %in% names(condf)) stop("invalid cuniqueid")
  if (!is.null(treef) && !is.null(tuniqueid) && !tuniqueid %in% names(treef)) 
    stop("invalid tuniqueid")
  ref_titles <- FIESTA::ref_titles

  ##################################################################
  ## SET UP VARIABLE LISTS
  ##################################################################
  ## DEFINE DOMAIN VARIABLES LISTS (VARIABLES TO KEEP AND EXCLUDE)

  ## CHECK domlut
  domlut <- FIESTA::pcheck.table(domlut, tabnm="domlut", nullcheck=TRUE, gui=gui)

  if (!is.null(domlut)) {
    domlutvars <- c("DOMCODE", "DOMNAME")
    if (!all(domlutvars %in% names(domlut))){
      missvars <- domlutvars[which(!domlutvars %in% names(domlut))]
      warning("missing columns in domlut: ", addcommas(missvars))
    }
    if (is.null(domvarlst)) 
      domvarlst <- c(domlut[["DOMCODE"]], domlut[["DOMNAME"]])
  } else {
    domvarlst <- names(condf)[!names(condf) %in% 
		c(cuniqueid, condid, "LON", "LAT", "PLOT")]
  }
  if ("DSTRBCD1" %in% names(condf)) 
    domvarlst <- c(domvarlst, "DSTRBGRP", "DSTRBGRPNM")
  
  domvarlst.not <- names(condf)[!names(condf) %in% domvarlst]

  ## DEFINE other variables
  varlst <- sort(domvarlst)
  grpvar <- NULL


  if(esttype %in% c("TREE", "RATIO")){
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
    tdomvarlst <- names(treef)[!names(treef) %in% tdomvarlst.not] 	## Tree domain variables

    varlst <- c(varlst, sort(tdomvarlst))
  }
 
  ## Check row.add0 and col.add0
  ########################################################
  row.add0 <- FIESTA::pcheck.logical(row.add0, varnm="row.add0", 
		title="Add 0 for row?", first="NO", gui=gui)
  col.add0 <- FIESTA::pcheck.logical(col.add0, varnm="col.add0", 
		title="Add 0 for column?", first="NO", gui=gui)
  rowgrp <- FIESTA::pcheck.logical(rowgrp, varnm="rowgrp", title="Row groups?", 
		first="NO", gui=gui)
  row.FIAname <- FIESTA::pcheck.logical(row.FIAname, varnm="row.FIAname", 
		title="Row names?", first="NO", gui=gui)
  if (rowgrp && is.null(rowgrpnm) && !row.FIAname) 
     stop("either row.FIAname must be TRUE or rowgrpnm != NULL to add row groups")

  if (row.FIAname) {
    ## Get FIA reference table for xvar
    xvar.ref <- FIESTA::getRefobject(toupper(rowvar))
    if (is.null(xvar.ref)) {
      message(paste("no reference name for", rowvar))
      row.FIAname <- FALSE
    }
  }

  ##############################################################
  ### ROW VARIABLE
  ##############################################################
  uniquerow <- NULL
  rowvar <- FIESTA::pcheck.varchar(var2check=rowvar, varnm="rowvar", gui=gui, 
		checklst=c("NONE", varlst), caption="Row variable", 
		warn=paste(rowvar, "not found"))
  if (is.null(rowvar)) rowvar <- "TOTAL"
  

  ## If rowvar == "NONE", set rowvar = "TOTAL" and exit, returning short list
  if (rowvar == "NONE") {
    rowvar <- "TOTAL"
    rowvar.filter <- "NONE"
    colvar <- "NONE"
    colvar.filter <- "NONE"
    domainlst <- rowvar
    row.add0 <- FALSE
    col.add0 <- FALSE

    returnlst <- list(treef=treef, condf=condf[,c(cuniqueid, condid, domainlst), with=FALSE], 
		uniquerow=NULL, uniquecol=NULL, domainlst=domainlst, rowvar=rowvar, colvar=colvar,
		row.orderby=row.orderby, col.orderby=col.orderby, row.add0=row.add0, 
		col.add0=col.add0,title.rowvar=title.rowvar, title.colvar=title.colvar, 
		bytdom=FALSE, domain=rowvar, dom.orderby=NULL, tdomvar=NULL)
    return(returnlst)
  }

  if (rowvar != "NONE") {

    ## GET row titles defined in FIESTA
    ###################################################
    if (is.null(title.rowvar)) 
     title.rowvar <- ifelse (rowvar %in% ref_titles[["DOMVARNM"]], 
		ref_titles[ref_titles[["DOMVARNM"]] == rowvar, "DOMTITLE"], rowvar)	

    ## Check row groups
    if (rowgrp && is.null(rowgrpnm)) {
      vargrp <- unique(FIESTA::ref_codes[!is.na(FIESTA::ref_codes[["GROUPNM"]]) & 
		FIESTA::ref_codes[["GROUPNM"]] != "", "VARIABLE"])
      if (!rowvar %in% vargrp) {
        message("row group not available for rowvar")
        rowgrp <- FALSE
      }
    }

    ## Check rowlut
    if (!is.null(rowlut)) {
      if (is.vector(rowlut) && length(rowlut) > 1) {
        rowlut <- data.table(rowlut)
        setnames(rowlut, rowvar)
      } else {
        rowlut <- FIESTA::pcheck.table(rowlut, gui=gui, tabnm=rowlut, caption="Row look up?")
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
        if (!rowvar %in% names(condf)) {
          warning(paste(rowvar, "not in cond table... using code"))
          rowvar <- row.orderby
          row.orderby <- NULL       
        }
      } else if (rowvar %in% domlut[["DOMNAME"]]) {
        row.orderby <- as.character(domlut[match(rowvar, domlut[["DOMNAME"]]), "DOMCODE"])
        title.rowvar <- as.character(domlut[match(rowvar, domlut[["DOMNAME"]]), "DOMTITLE"])
        if (!row.orderby %in% names(condf)) {
          warning(paste(row.orderby, "not in cond table... ordering by name"))
          row.orderby <- NULL
        } 
      }
    } else if (rowvar %in% names(condf)) {
      if (row.FIAname || !is.null(rowlut)) {
        if (!is.null(rowlut) && ncol(rowlut) > 1 &&  all(names(rowlut) %in% names(condf))) {
          if (is.null(row.orderby) || row.orderby == "NONE") {
            message("row.orderby is not defined... ordering by rowvar")
          } else {

            if (row.orderby == rowvar) {
              row.name <- names(rowlut)[names(rowlut) != rowvar]
              if (length(row.name) > 1) stop("invalid rowlut... only 2 columns allowed")
              rowvar <- row.name
            } 
          }
        } else {

          rowLUTgrp <- FALSE
          if (rowgrp) {
            if (!is.null(rowgrpnm)) {
              if (!rowgrpnm %in% names(condf)) stop(paste(rowgrpnm, "not in cond"))
              if (is.null(title.rowgrp)) title.rowgrp <- rowgrpnm

              if (!is.null(rowgrpord)) 
                if (!rowgrpord %in% names(condf)) stop(paste(rowgrpord, "not in cond"))
            } else {
              rowLUTgrp <- TRUE
            }
          }
          if (!is.null(rowlut)) row.add0 <- TRUE
          rowLUT <- datLUTnm(x=condf, xvar=rowvar, LUT=rowlut, FIAname=row.FIAname,
		  		group=rowLUTgrp, add0=row.add0)
          condf <- rowLUT$xLUT
          rowlut <- rowLUT$LUT
          rowLUTnm <- rowLUT$xLUTnm
          if (rowgrp) {
            rowgrpord <- rowLUT$grpcode
            rowgrpnm <- rowLUT$grpname
            if (all(sapply(rowlut[[rowgrpnm]], function(x) x == "")) || 
			all(is.na(rowlut[[rowgrpnm]]))) 
              stop("no groups for ", rowvar)

            title.rowgrp <- ifelse (rowgrpord %in% ref_titles[["DOMVARNM"]], 
		  	ref_titles[ref_titles[["DOMVARNM"]] == rowgrpord, "DOMTITLE"], rowgrpnm)	
          }

          if (is.null(row.orderby) || row.orderby == "NONE") {
            if (!is.null(rowLUTnm)) {
              row.orderby <- rowvar
              rowvar <- rowLUTnm
            }
            if (row.orderby == rowvar) {
              row.name <- names(rowlut)[names(rowlut) != rowvar]
              if (length(row.name) > 1) stop("invalid rowlut... only 2 columns allowed")
              rowvar <- row.name
            } 

          } else if (row.orderby == rowvar) {
             rowvar <- rowLUTnm
          } else {
            if (!row.orderby %in% names(rowlut))
              stop("row.orderby not in rowlut")
          } 
        }
      } else if (!is.null(row.orderby) && row.orderby != "NONE") {

        if (!row.orderby %in% names(condf)) stop("row.orderby must be in cond")
        if (row.orderby == rowvar) stop("row.orderby must be different than rowvar")

        ## If NULL or empty values, substitute with 0 values
        if (length(condf[[row.orderby]][is.na(condf[[row.orderby]])]) > 0) {
          if (is.numeric(condf[[row.orderby]])) {
            condf[is.na(get(row.orderby)), (row.orderby) := 0] 
          } else {
            condf[is.na(get(row.orderby)), (row.orderby) := "Undefined"]
          }
        }
        if (length(condf[[row.orderby]][condf[[row.orderby]] == ""]) > 0) {
          if (is.numeric(condf[[row.orderby]])) {
            condf[get(row.orderby) == "", (row.orderby) := 0] 
          } else {
            condf[get(row.orderby) == "", (row.orderby) := "Undefined"]
          }
        }

        condf <- FIESTA::DT_NAto0(DT=condf, cols=rowvar)
        condf <- FIESTA::DT_NAto0(DT=condf, cols=row.orderby)
      }

      ## rowvar.filter
      ########################################################
      cnrows <- nrow(condf)
      condf <- FIESTA::datFilter(x=condf, xfilter=rowvar.filter, vardelete=domvarlst.not, 
		  title.filter=rowvar)$xf
      if (nrow(condf) < cnrows) isfilter <- TRUE

      if (sum(is.na(condf[[rowvar]])) > 0) {
        rowvar.na.filter <- paste0("!is.na(", rowvar, ")")
        condf <- subset(condf, eval(parse(text = rowvar.na.filter)))
      }

    } else if (rowvar %in% names(treef)) {

      if (row.FIAname || !is.null(rowlut)) {
        if (!is.null(rowlut) && ncol(rowlut) > 1 && all(names(rowlut) %in% names(treef))) {
          if (is.null(row.orderby) || row.orderby == "NONE") {
            message("row.orderby is not defined... ordering by rowvar")
          } else {
            if (row.orderby == rowvar) {
              row.name <- names(rowlut)[names(rowlut) != rowvar]
              if (length(row.name) > 1) stop("invalid rowlut... only 2 columns allowed")
              rowvar <- row.name
            } 
          }
        } else {
          rowLUTgrp <- FALSE
          if (rowgrp) {
            if (!is.null(rowgrpnm)) {
              if (!rowgrpnm %in% names(treef)) stop(paste(rowgrpnm, "not in tree"))
              if (is.null(title.rowgrp)) title.rowgrp <- rowgrpnm

              if (!is.null(rowgrpord)) 
                if (!rowgrpord %in% names(treef)) stop(paste(rowgrpord, "not in tree"))
            } else {
              rowLUTgrp <- TRUE
            }
          }

          if (!is.null(rowlut)) row.add0 <- TRUE
          rowLUT <- FIESTA::datLUTnm(x=treef, xvar=rowvar, LUT=rowlut, FIAname=row.FIAname,
		  		group=rowLUTgrp, add0=row.add0, xtxt="tree")

          treef <- rowLUT$xLUT
          rowlut <- rowLUT$LUT
          rowLUTnm <- rowLUT$xLUTnm
          if (rowgrp) {
            rowgrpord <- rowLUT$grpcode
            rowgrpnm <- rowLUT$grpname
            if (all(sapply(rowlut[[rowgrpnm]], function(x) x == "")) || 
			all(is.na(rowlut[[rowgrpnm]]))) 
              stop("no groups for ", rowvar)

            title.rowgrp <- ifelse (rowgrpord %in% ref_titles[["DOMVARNM"]], 
		  	ref_titles[ref_titles[["DOMVARNM"]] == rowgrpord, "DOMTITLE"], rowgrpnm)	
          }

          if (is.null(row.orderby) || row.orderby == "NONE") {
            if (!is.null(rowLUTnm)) {
              row.orderby <- rowvar
              rowvar <- rowLUTnm
            }
            if (row.orderby == rowvar) {
              row.name <- names(rowlut)[names(rowlut) != rowvar]
              if (length(row.name) > 1) stop("invalid rowlut... only 2 columns allowed")
              if (length(row.name) == 0) {
                row.orderby <- "NONE"
              } else {
                rowvar <- row.name
              }
            } 
          } else if (row.orderby == rowvar) {
             rowvar <- rowLUTnm
          } else {
            if (!row.orderby %in% names(rowlut))
              stop("row.orderby not in rowlut")
          } 
        }
      } else if (!is.null(row.orderby) && row.orderby != "NONE") {

        if (!row.orderby %in% names(treef)) stop("row.orderby must be in tree")
        if (row.orderby == rowvar) stop("row.orderby must be different than rowvar")

        ## If NULL or empty values, substitute with 0 values
        if (length(treef[[row.orderby]][is.na(treef[[row.orderby]])]) > 0) {
          if (is.numeric(treef[[row.orderby]])) {
            treef[is.na(get(row.orderby)), (row.orderby) := 0] 
          } else {
            treef[is.na(get(row.orderby)), (row.orderby) := "Undefined"]
          }
        }
        if (length(treef[[row.orderby]][treef[[row.orderby]] == ""]) > 0) {
          if (is.numeric(treef[[row.orderby]])) {
            treef[get(row.orderby) == "", (row.orderby) := 0] 
          } else {
            treef[get(row.orderby) == "", (row.orderby) := "Undefined"]
          }
        }

        treef <- FIESTA::DT_NAto0(DT=treef, cols=rowvar)
        treef <- FIESTA::DT_NAto0(DT=treef, cols=row.orderby)
      }

      ## rowvar.filter
      ########################################################
      tnrows <- nrow(treef)
      treef <- FIESTA::datFilter(x=treef, xfilter=rowvar.filter, vardelete=domvarlst.not, 
		  title.filter=rowvar)$xf
      if (nrow(treef) < tnrows) isfilter <- TRUE

      ## Remove NA values in rowvar
      if (sum(is.na(treef[[rowvar]])) > 0) {
        rowvar.na.filter <- paste0("!is.na(", rowvar, ")")
        treef <- subset(treef, eval(parse(text = rowvar.na.filter)))
      }
    }
  }

  ##############################################################
  ## COLUMN VARIABLE
  ##############################################################
  uniquecol <- NULL
  varlst <- varlst[which(!varlst %in% rowvar)]
  colvar <- FIESTA::pcheck.varchar(var2check=colvar, varnm="colvar", gui=gui, 
		checklst=c("NONE", varlst), caption="Column variable", 
		warn=paste(colvar, "not found"))
  if (is.null(colvar)) colvar <- "NONE"

  if (colvar != "NONE") {
    ## Check to make sure there is a rowvar when there is a colvar
    if (rowvar == "TOTAL") stop("no rowvar, use colvar as rowvar") 
    if (is.null(col.orderby)) col.orderby <- "NONE"

    ## GET column titles defined in FIESTA
    ###################################################
    if (is.null(title.colvar))
     title.colvar <- ifelse (colvar %in% ref_titles[["DOMVARNM"]], 
		ref_titles[ref_titles[["DOMVARNM"]] == colvar, "DOMTITLE"], 
     ifelse (colvar %in% ref_titles[["DOMVARNM"]], 
		ref_titles[ref_titles[["DOMVARNM"]] == colvar, "DOMTITLE"], colvar))	

    ## Check collut
    if (!is.null(collut)) {
      if (is.vector(collut) && length(collut) > 1) {
        collut <- data.table(collut)
        setnames(collut, colvar)
      } else {
        collut <- FIESTA::pcheck.table(collut, gui=gui, tabnm=collut, caption="Column look up?")
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
        if (!colvar %in% names(condf)) {
          warning(paste(colvar, "not in cond table... using code"))
          colvar <- col.orderby
          col.orderby <- NULL
        } 
      } else if (colvar %in% domlut[["DOMNAME"]]) {
        col.orderby <- as.character(domlut[match(colvar, domlut[["DOMNAME"]]), "DOMCODE"])
        title.colvar <- as.character(domlut[match(colvar, domlut[["DOMNAME"]]), "DOMTITLE"])
        if (!col.orderby %in% names(condf)) {
          warning(paste(col.orderby, "not in cond table... ordering by name"))
          col.orderby <- NULL
        }
      }

    } else if (colvar %in% names(condf)) {

      if (col.FIAname || !is.null(collut)) {
        if (!is.null(collut) && ncol(collut) > 1 && all(names(collut) %in% names(condf))) {
         if (is.null(col.orderby) || col.orderby == "NONE") {
            message("col.orderby is not defined... ordering by colvar")
          } else {
            if (col.orderby == colvar) {
              col.name <- names(collut)[names(collut) != colvar]
              if (length(col.name) > 1) stop("invalid collut... only 2 columns allowed")
              colvar <- col.name
            } 
          }
        } else {

          #if (!is.null(collut)) col.add0 <- TRUE
          colLUT <- datLUTnm(x=condf, xvar=colvar, LUT=collut, FIAname=col.FIAname, 
			add0=col.add0)
          condf <- colLUT$xLUT
          collut <- colLUT$LUT
          colLUTnm <- colLUT$xLUTnm

          #if (!is.null(collut)) col.add0 <- TRUE
          if (is.null(col.orderby) || col.orderby == "NONE") {
            if (!is.null(colLUTnm)) {
              col.orderby <- colvar
              colvar <- colLUTnm
            }
          } else if (col.orderby == colvar) {
             colvar <- colLUTnm
          }
        }

      } else if (col.orderby != "NONE") {

        if (!col.orderby %in% names(condf)) stop("col.orderby must be in cond")
        if (col.orderby == colvar) stop("col.orderby must be different than colvar")

        ## If NULL or empty values, substitute with 0 values
        if (length(condf[[col.orderby]][is.na(condf[[col.orderby]])]) > 0) {
          if (is.numeric(condf[[col.orderby]])) {
            condf[is.na(get(col.orderby)), (col.orderby) := 0] 
          } else {
            condf[is.na(get(col.orderby)), (col.orderby) := "Undefined"]
          }
        }
        if (length(condf[[col.orderby]][condf[[col.orderby]] == ""]) > 0) {
          if (is.numeric(condf[[col.orderby]])) {
            condf[get(col.orderby) == "", (col.orderby) := 0] 
          } else {
            condf[get(col.orderby) == "", (col.orderby) := "Undefined"]
          }
        }

        condf <- FIESTA::DT_NAto0(DT=condf, cols=colvar)
        condf <- FIESTA::DT_NAto0(DT=condf, cols=col.orderby)
        collut <- unique(condf[,c(colvar, col.orderby), with=FALSE])
        setkeyv(collut, col.orderby)

      } else {

        colvals <- na.omit(unique(condf[, colvar, with=FALSE]))
        collut <- as.data.table(colvals)
        names(collut) <- colvar
        setkeyv(collut, colvar)
      }

      ## colvar.filter
      ########################################################
      cnrows <- nrow(condf)
      condf <- FIESTA::datFilter(x=condf, xfilter=colvar.filter, vardelete=domvarlst.not, 
		  title.filter=colvar)$xf
      if (nrow(condf) < cnrows) isfilter <- TRUE

#      if (sum(is.na(condf[[colvar]])) > 0) {
#        colvar.na.filter <- paste0("!is.na(", colvar, ")")
#        condf <- subset(condf, eval(parse(text = colvar.na.filter)))
#      }
       
    } else if (colvar %in% names(treef)) {

      if (col.FIAname || !is.null(collut)) {
        if (!is.null(collut) && ncol(collut) > 1) {
          if (is.null(col.orderby) || col.orderby == "NONE") {
            message("col.orderby is not defined... ordering by colvar")
          } else {
            if (col.orderby == colvar) {
              col.name <- names(collut)[names(collut) != colvar]
              if (length(col.name) > 1) stop("invalid collut... only 2 columns allowed")
              colvar <- col.name
            } 
          }
        } else {
          if (!is.null(collut)) col.add0 <- TRUE
          colLUT <- datLUTnm(x=treef, xvar=colvar, LUT=collut, FIAname=col.FIAname,
				add0=col.add0, xtxt="treef")
          treef <- colLUT$xLUT
          collut <- colLUT$LUT
          colLUTnm <- colLUT$xLUTnm

          if (col.orderby == "NONE") {
            if (!is.null(colLUTnm)) {
              col.orderby <- colvar
              colvar <- colLUTnm
            }
          } else if (col.orderby == colvar) {
             colvar <- colLUTnm
          }
          setkeyv(collut, col.orderby)
        }

      } else if (col.orderby != "NONE") {

        if (!col.orderby %in% names(treef)) stop("col.orderby must be in tree")
        if (col.orderby == colvar) stop("col.orderby must be different than colvar")

        ## If NULL or empty values, substitute with 0 values
        if (length(treef[[col.orderby]][is.na(treef[[col.orderby]])]) > 0) {
          if (is.numeric(treef[[col.orderby]])) {
            treef[is.na(get(col.orderby)), (col.orderby) := 0] 
          } else {
            treef[is.na(get(col.orderby)), (col.orderby) := "Undefined"]
          }
        }
        if (length(treef[[col.orderby]][treef[[col.orderby]] == ""]) > 0) {
          if (is.numeric(treef[[col.orderby]])) {
            treef[get(col.orderby) == "", (col.orderby) := 0] 
          } else {
            treef[get(col.orderby) == "", (col.orderby) := "Undefined"]
          }
        }

        treef <- FIESTA::DT_NAto0(DT=treef, cols=colvar)
        treef <- FIESTA::DT_NAto0(DT=treef, cols=col.orderby)
        uniquecol <- unique(treef[,c(colvar, col.orderby), with=FALSE])
        setkeyv(uniquecol, col.orderby)

      } else {

        colvals <- na.omit(unique(treef[, colvar, with=FALSE]))
        uniquecol <- as.data.table(colvals)
        names(uniquecol) <- colvar
        setkeyv(uniquecol, colvar)
      }

      ## colvar.filter
      ########################################################
      tnrows <- nrow(treef)
      treef <- FIESTA::datFilter(x=treef, xfilter=colvar.filter, vardelete=domvarlst.not, 
		  title.filter=colvar)$xf
      if (nrow(treef) < tnrows) isfilter <- TRUE

      if (sum(is.na(treef[[colvar]])) > 0) {
        colvar.na.filter <- paste0("!is.na(", colvar, ")")
        treef <- subset(treef, eval(parse(text = colvar.na.filter)))
      }
    }
  }

  ###################################################################################
  ## GET DOMAIN. CONCATENATE ROWVAR & COLVAR VARIABLES IF THEY ARE IN THE SAME TABLE.
  ###################################################################################
  ## If rowvar and colvar both in cond table, concatenate columns for calculation.
  if (all(c(rowvar, colvar) %in% names(condf))) {
    setkeyv(condf, c(rowvar, colvar))   
 
    ## CONCATENATE THE 2 INPUT COLUMNS (tcol, trow)
    grpvar <- paste(rowvar, colvar, sep="#")
    condf[, (grpvar) := paste(condf[[rowvar]], condf[[colvar]], sep="#")]
  }

  domainlst <- "TOTAL"
  if (esttype %in% c("TREE", "RATIO")) {
    bytdom <- FALSE
    tnames <- names(treef)

    if (colvar != "NONE") {
      if (rowvar %in% tnames) {
        bytdom <- TRUE
        if (colvar %in% tnames) {
          ## CONCATENATE THE 2 INPUT COLUMNS (rowvar, colvar)
          grpvar <- paste(rowvar, colvar, sep="#")
          treef[, (grpvar) := paste(treef[[rowvar]], treef[[colvar]], sep="#")]
          tdomvar <- grpvar
          domain <- NULL
          dom.orderby <- NULL
        } else {
          domain <- colvar
          dom.orderby <- col.orderby
          tdomvar <- rowvar
          domainlst <- c(domainlst, colvar)
        }
      } else if (colvar %in% tnames) {
        if (length(unique(treef[[colvar]])) > 1) {
          bytdom <- TRUE
          domain <- rowvar
          dom.orderby <- row.orderby
          tdomvar <- colvar
        } else {
          colvar <- "NONE"
          domain <- rowvar
          dom.orderby <- row.orderby
          tdomvar <- colvar
        }
        domainlst <- c(domainlst, rowvar)
      } else {
        ## Domain is concatenated variable (grpvar)
        domain <- grpvar
        dom.orderby <- NULL
        tdomvar <- NULL
        domainlst <- c(domainlst, rowvar, colvar, grpvar)
      }
    } else {  ## colvar == "NONE"
      if (rowvar == "TOTAL") {
        domain <- NULL
        condf$DOMAIN <- 1
        dom.orderby <- row.orderby
        tdomvar <- colvar
      } else if (rowvar %in% tnames) {
        bytdom <- TRUE
        domain <- NULL
        dom.orderby <- NULL
        tdomvar <- rowvar
      } else {
        domain <- rowvar
        dom.orderby <- row.orderby
        tdomvar <- colvar
        domainlst <- c(domainlst, rowvar)
      }
    }
  } else {
    domainlst <- unique(c(domainlst, rowvar, colvar, grpvar))
  }
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
  } else if (!is.null(uniquerow)) {
    uniquerow <- setDT(uniquerow)
    if (!is.null(row.orderby) && row.orderby != "NONE" && row.orderby %in% names(uniquerow)) 
      setkeyv(uniquerow, c(rowgrpnm, row.orderby))
  } else if (rowvar %in% names(condf)) {
    if (!is.null(row.orderby) && row.orderby != "NONE") {
      uniquerow <- unique(condf[,c(rowgrpord, rowgrpnm, row.orderby, rowvar), with=FALSE])
      setkeyv(uniquerow, c(rowgrpord, rowgrpnm, row.orderby))
    } else {
      rowvals <- na.omit(unique(condf[, rowvar, with=FALSE]))
      uniquerow <- as.data.table(rowvals)
      names(uniquerow) <- rowvar
      setkeyv(uniquerow, rowvar)
    }
  } else if (rowvar %in% names(treef)) {
    if (!is.null(row.orderby) && row.orderby != "NONE") {
      uniquerow <- unique(treef[,c(rowgrpord, rowgrpnm, row.orderby, rowvar), with=FALSE])
      setkeyv(uniquerow, c(rowgrpord, rowgrpnm, row.orderby))
    } else {
      rowvals <- na.omit(unique(treef[, rowvar, with=FALSE]))
      uniquerow <- as.data.table(rowvals)
      names(uniquerow) <- rowvar
      setkeyv(uniquerow, rowvar)
    }
  }
  if (!is.null(landarea) && landarea == c("FOREST", "TIMBERLAND"))
    uniquerow2 <- uniquerow[!uniquerow[[rowvar]] %in% c(0, "Nonforest"),]

  ## uniquecol
  #########################################################
  if (!is.null(collut)) {
#    if (sum(unlist(lapply(collut, duplicated))) > 0) {
#      print(collut)
#      stop("invalid collut... no duplicates allowed") 
#    }
    uniquecol <- collut
  } else if (!is.null(uniquecol)) {
    uniquecol <- setDT(uniquecol)
    if (col.orderby != "NONE" && col.orderby %in% names(uniquecol)) 
      setkeyv(uniquecol, col.orderby)
  } else if (colvar %in% names(condf)) {
    if (!is.null(col.orderby) && col.orderby != "NONE") {
      uniquecol <- unique(condf[,c(colvar, col.orderby), with=FALSE])
      setkeyv(uniquecol, col.orderby)
    } else {
      colvals <- na.omit(unique(condf[, colvar, with=FALSE]))
      uniquecol <- as.data.table(colvals)
      names(uniquecol) <- colvar
      setkeyv(uniquecol, colvar)
    }
  } else if (colvar %in% names(treef)) {
    if (col.orderby != "NONE") {
      uniquecol <- unique(treef[,c(colvar, col.orderby), with=FALSE])
      setkeyv(uniquecol, col.orderby)
    } else {
      colvals <- na.omit(unique(treef[, colvar, with=FALSE]))
      uniquecol <- as.data.table(colvals)
      names(uniquecol) <- colvar
      setkeyv(uniquecol, colvar)
    }
  }

  if (!is.null(landarea) && landarea %in% c("FOREST", "TIMBERLAND")) {
    if (any(uniquecol[[colvar]] %in% c(0, "Nonforest"))) {
      message("0 values are assumed to represent nonforest land and are removed from analysis")
      uniquecol <- uniquecol[!uniquecol[[colvar]] %in% c(0, "Nonforest"),]
    }
  } 
 
  ## Define cvars2keep
  cvars2keep <- unique(c(cuniqueid, condid, domainlst, cvars2keep))
  condf <- condf[,cvars2keep, with=FALSE]
  setkeyv(condf, c(cuniqueid, condid))

  ## Create factors for ordering tables
  ##############################################################################
  if (!is.null(uniquerow)) {
    ## Change SITECLCD to descending order
    if (row.FIAname && "SITECLCD" %in% names(uniquerow)) 
      uniquerow <- setorder(uniquerow, -SITECLCD)
    if (row.FIAname && "GSSTKCD" %in% names(uniquerow)) 
      uniquerow <- setorder(uniquerow, -GSSTKCD)
    if (!is.null(rowgrpnm))
      uniquerow[[rowgrpnm]] <- factor(uniquerow[[rowgrpnm]], levels=unique(uniquerow[[rowgrpnm]]))
    uniquerow[[rowvar]] <- factor(uniquerow[[rowvar]], levels=unique(uniquerow[[rowvar]]))
  }

  if (!is.null(uniquecol)) {
    ## Change SITECLCD to descending order
    if (col.FIAname && "SITECLCD" %in% names(uniquecol)) 
      uniquecol <- setorder(uniquecol, -SITECLCD)
    if (col.FIAname && "GSSTKCD" %in% names(uniquecol)) 
      uniquecol <- setorder(uniquecol, -GSSTKCD)
    uniquecol[[colvar]] <- factor(uniquecol[[colvar]], levels=unique(uniquecol[[colvar]]))
  }  
 
  returnlst <- list(condf=condf, uniquerow=uniquerow, uniquecol=uniquecol, 
	domainlst=domainlst, rowvar=rowvar, colvar=colvar, row.orderby=row.orderby, 
	col.orderby=col.orderby, row.add0=row.add0, col.add0=col.add0, 
	title.rowvar=title.rowvar, title.colvar=title.colvar, rowgrpnm=rowgrpnm,
	title.rowgrp=title.rowgrp)

  if (esttype %in% c("TREE", "RATIO")) {
    ## Filter tree data for any cond filters
    treef <- treef[paste(get(tuniqueid), get(condid), sep="_") %in% 
		condf[,paste(get(cuniqueid), get(condid), sep="_")]]
 
    returnlst <- append(list(treef=treef), returnlst)
    returnlst <- append(returnlst, list(bytdom=bytdom, domain=domain, 
		dom.orderby=dom.orderby, tdomvar=tdomvar))
  } else {
    returnlst <- append(returnlst, list(grpvar=grpvar))
  }

  return(returnlst)
}

