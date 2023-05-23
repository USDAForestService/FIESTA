check.rowcolPB <- function(gui, esttype, ratio=FALSE, PBx, plotid="PLT_CN",
	condid="CONDID", pntid=NULL, rowvar=NULL, rowvar.filter=NULL, colvar=NULL,
	colvar.filter=NULL, row.FIAname=FALSE, col.FIAname=FALSE, row.orderby=NULL,
	col.orderby=NULL, row.add0=FALSE, col.add0=FALSE, domvarlst=NULL,
	domlut=NULL, title.rowvar=NULL, title.colvar=NULL, rowlut=NULL, collut=NULL,
	rowgrp=FALSE, rowgrpnm=NULL, rowgrpord=NULL, title.rowgrp=NULL, landarea=NULL,
 	PBvars2keep=NULL, filterids=NULL){

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
  ## 7. Concatenate variables:
  ##	   If rowvar and colvar in cond table, concatenate columns and add to cond table.
  ## 8. Define domain.
  ## 9. Define PBvars2keep
  ####################################################################################

  ## Set global variables
  domainlst=PBx.d <- NULL

  ## Check for condid
  if (!is.null(condid) && !condid %in% names(PBx)) condid <- NULL
  if (!is.null(plotid) && !plotid %in% names(PBx)) stop("invalid plotid")

  ## Get PBx key
  PBxkey <- key(PBx)

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
    domvarlst <- names(PBx)[!names(PBx) %in%
		c(plotid, condid, "LON", "LAT", "PLOT")]
  }
  if ("DSTRBCD1" %in% names(PBx))
    domvarlst <- c(domvarlst, "DSTRBGRP", "DSTRBGRPNM")

  domvarlst.not <- names(PBx)[!names(PBx) %in% domvarlst]

  ## DEFINE other variables
  varlst <- sort(domvarlst)


  ## Check row.add0 and col.add0
  ########################################################
  row.add0 <- pcheck.logical(row.add0, varnm="row.add0",
		title="Add 0 for row?", first="NO", gui=gui)
  col.add0 <- pcheck.logical(col.add0, varnm="col.add0",
		title="Add 0 for column?", first="NO", gui=gui)

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
    rowvar <- "TOTAL"
    rowvar.filter <- "NONE"
    colvar <- "NONE"
    colvar.filter <- "NONE"
    domainlst <- rowvar
    row.add0 <- FALSE
    col.add0 <- FALSE

    returnlst <- list(PBx=PBx[,c(plotid, pntid, condid, domainlst), with=FALSE],
		uniquerow=NULL, uniquecol=NULL, domainlst=domainlst, rowvar=rowvar, colvar=colvar,
		row.orderby=row.orderby, col.orderby=col.orderby, row.add0=row.add0,
		col.add0=col.add0, title.rowvar=title.rowvar, title.colvar=title.colvar,
		bytdom=FALSE, domain=rowvar, dom.orderby=NULL, tdomvar=NULL)
    return(returnlst)
  }

  if (rowvar != "NONE") {

    ##################################################################################
    ## Check for lookup tables
    ##################################################################################
    ## Check rowlut
    if (!is.null(rowlut)) {

      if (is.vector(rowlut) && length(rowlut) > 1) {
        rowlut <- data.table(rowlut)
        setnames(rowlut, rowvar)
      } else {
        rowlut <- pcheck.table(rowlut, gui=gui, tabnm=rowlut, caption="Row look up?")
      }
    }

    ## domlut defines columns in cond to use for codes, code names, and table titles
    ##################################################################################
    if (!is.null(domlut)) {
      if (!rowvar %in% domvarlst) stop(paste(rowvar, "not in domlut"))
      if (rowvar %in% domlut[["DOMCODE"]]) {
        row.orderby <- rowvar
        title.rowvar <- as.character(domlut[match(rowvar, domlut[["DOMCODE"]]), "DOMTITLE"])
        rowvar <- as.character(domlut[match(rowvar, domlut[["DOMCODE"]]), "DOMNAME"])
        if (!rowvar %in% names(PBx)) {
          warning(paste(rowvar, "not in cond table... using code"))
          rowvar <- row.orderby
          row.orderby <- NULL
        }
      } else if (rowvar %in% domlut[["DOMNAME"]]) {
        row.orderby <- as.character(domlut[match(rowvar, domlut[["DOMNAME"]]), "DOMCODE"])
        title.rowvar <- as.character(domlut[match(rowvar, domlut[["DOMNAME"]]), "DOMTITLE"])
        if (!row.orderby %in% names(PBx)) {
          warning(paste(row.orderby, "not in cond table... ordering by name"))
          row.orderby <- NULL
        }
      }
    } else if (rowvar %in% names(PBx)) {
      if (!is.null(rowlut) &&  all(names(rowlut) %in% names(PBx))) {
        if (is.null(row.orderby) || row.orderby == "NONE") {
          message("row.orderby is not defined... ordering by rowvar")
        } else {
          if (row.orderby == rowvar) {
            row.name <- names(rowlut)[names(rowlut) != rowvar]
            if (length(row.name) > 1) stop("invalid rowlut... only 2 columns allowed")
            rowvar <- row.name
          }
        }
      } else if (!is.null(rowlut)) {
        rowLUT <- datLUTnm(x=PBx, xvar=rowvar, LUT=rowlut, add0=row.add0)
        PBx <- rowLUT$xLUT
        rowlut <- rowLUT$LUT
        rowLUTnm <- rowLUT$xLUTnm

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
      if (row.orderby == rowvar) stop("row.orderby must be different than rowvar")
    }

    if (is.null(title.rowvar)) title.rowvar <- rowvar
  }

  ##############################################################
  ## COLUMN VARIABLE
  ##############################################################
  uniquecol <- NULL
  varlst <- varlst[which(!varlst %in% rowvar)]
  colvar <- pcheck.varchar(var2check=colvar, varnm="colvar", gui=gui,
		checklst=c("NONE", varlst), caption="Column variable",
		warn=paste(colvar, "not found"))
  if (is.null(colvar)) colvar <- "NONE"

  if (colvar != "NONE") {
    ## Check to make sure there is a rowvar when there is a colvar
    if (rowvar == "TOTAL") stop("no rowvar, use colvar as rowvar")
    if (is.null(col.orderby)) col.orderby <- "NONE"

    ## Check collut
    if (!is.null(collut)) {
      if (is.vector(collut) && length(collut) > 1) {
        collut <- data.table(collut)
        setnames(collut, colvar)
      } else {
        collut <- pcheck.table(collut, gui=gui, tabnm=collut, caption="Column look up?")
      }
    }

    ## domlut defines columns in cond to use for codes, code names, and table titles
    ##################################################################################
    if (!is.null(domlut)) {
      if (!colvar %in% domvarlst) stop(paste(colvar, "not in domlut"))
      if (colvar %in% domlut[["DOMCODE"]]) {
        col.orderby <- colvar
        title.colvar <- as.character(domlut[match(colvar, domlut[["DOMCODE"]]), "DOMTITLE"])
        colvar <- as.character(domlut[match(colvar, domlut[["DOMCODE"]]), "DOMNAME"])
        if (!colvar %in% names(PBx)) {
          warning(paste(colvar, "not in cond table... using code"))
          colvar <- col.orderby
          col.orderby <- NULL
        }
      } else if (colvar %in% domlut[["DOMNAME"]]) {
        col.orderby <- as.character(domlut[match(colvar, domlut[["DOMNAME"]]), "DOMCODE"])
        title.colvar <- as.character(domlut[match(colvar, domlut[["DOMNAME"]]), "DOMTITLE"])
        if (!col.orderby %in% names(PBx)) {
          warning(paste(col.orderby, "not in cond table... ordering by name"))
          col.orderby <- NULL
        }
      }
    } else if (colvar %in% names(PBx)) {

      if (!is.null(collut) &&  all(names(collut) %in% names(PBx))) {
        if (is.null(col.orderby) || col.orderby == "NONE") {
          message("col.orderby is not defined... ordering by colvar")
        } else {
          if (col.orderby == colvar) {
            col.name <- names(collut)[names(collut) != colvar]
            if (length(col.name) > 1) stop("invalid collut... only 2 columns allowed")
            colvar <- col.name
          }
        }
      } else if (!is.null(collut)) {
        colLUT <- datLUTnm(x=PBx, xvar=colvar, LUT=collut, add0=col.add0)
        PBx <- colLUT$xLUT
        collut <- colLUT$LUT
        colLUTnm <- colLUT$xLUTnm

        if (is.null(col.orderby) || col.orderby == "NONE") {
          if (!is.null(colLUTnm)) {
            col.orderby <- colvar
            colvar <- colLUTnm
          }
          if (col.orderby == colvar) {
            col.name <- names(collut)[names(collut) != colvar]
            if (length(col.name) > 1) stop("invalid collut... only 2 columns allowed")
            colvar <- col.name
          }
        } else if (col.orderby == colvar) {
           colvar <- colLUTnm
        } else {
          if (!col.orderby %in% names(collut))
            stop("col.orderby not in collut")
        }
      }
    } else if (!is.null(col.orderby) && col.orderby != "NONE") {
      if (col.orderby == colvar) stop("col.orderby must be different than colvar")
    }

    if (is.null(title.colvar)) title.colvar <- colvar
  }

  ###################################################################################
  ## GET DOMAIN
  ###################################################################################
  domainlst <- "TOTAL"
  domainlst <- unique(c(domainlst, rowvar, colvar))
  domainlst <- domainlst[domainlst != "NONE"]

  ############################################################################
  ## Get uniquerow and uniquecol
  ############################################################################
  setkeyv(PBx, PBxkey)
  if (ratio) PBx.d <- data.table::copy(PBx)

  if (rowvar != "NONE") {

    ## Filter photo data - rows
    if (is.factor(PBx[[rowvar]]))
      PBx[[rowvar]] <- as.character(PBx[[rowvar]])
    rowvar.val <- ifelse (is.numeric(PBx[[rowvar]]), 9999, "NOTinDOMAIN")

    if (!is.null(filterids))
      PBx[!paste(PBx[[plotid]], PBx[[pntid]]) %in% filterids,
		rowvar] <- rowvar.val

    if (!is.null(row.orderby) && row.orderby != "NONE") {
      if (is.factor(PBx[[row.orderby]]))
        PBx[[row.orderby]] <- is.character(PBx[[row.orderby]])
        orderby.val <- ifelse (is.numeric(PBx[[row.orderby]]), 9999, "NOTinDOMAIN")

      if (!is.null(filterids))
        PBx[!paste(PBx[[plotid]], PBx[[pntid]]) %in% filterids,
			row.orderby] <- orderby.val
    }
    if (is.null(rowlut)) {
      if (!is.null(row.orderby) && row.orderby != "NONE") {
        uniquerow <- unique(PBx[PBx[[row.orderby]] != orderby.val,
				c(row.orderby, rowvar), with=FALSE])
        setkeyv(uniquerow, row.orderby)
      } else {
        uniquerow <- unique(PBx[PBx[[rowvar]] != rowvar.val, rowvar, with=FALSE])
        setkeyv(uniquerow, rowvar)
      }
    } else {
      if (sum(unlist(lapply(rowlut, duplicated))) > 0) {
        print(rowlut)
        stop("invalid rowlut... no duplicates allowed")
      }
      uniquerow <- rowlut
    }
  }

  if (colvar != "NONE") {
    ## Filter photo data - columns
    if (is.factor(PBx[[colvar]]))
      PBx[[colvar]] <- as.character(PBx[[colvar]])
    colvar.val <- ifelse (is.numeric(PBx[[colvar]]), 9999, "NOTinDOMAIN")

    if (!is.null(filterids))
      PBx[!paste(PBx[[plotid]], PBx[[pntid]]) %in% filterids,
		colvar] <- colvar.val

    if (col.orderby != "NONE") {
      if (is.factor(PBx[[col.orderby]]))
          PBx[[col.orderby]] <- is.character(PBx[[col.orderby]])
      orderby.val <- ifelse (is.numeric(PBx[[col.orderby]]), 9999, "NOTinDOMAIN")

      if (!is.null(filterids))
        PBx[!paste(PBx[[plotid]], PBx[[pntid]]) %in% filterids,
			col.orderby] <- orderby.val
    }

    if (is.null(collut)) {
      if (!is.null(col.orderby) && col.orderby != "NONE") {
        uniquecol <- unique(PBx[PBx[[col.orderby]] != orderby.val,
				c(col.orderby, colvar), with=FALSE])
        setkeyv(uniquecol, col.orderby)
      } else {
        uniquecol <- unique(PBx[PBx[[colvar]] != colvar.val, colvar, with=FALSE])
        setkeyv(uniquecol, colvar)
      }
    } else {
      if (sum(unlist(lapply(collut, duplicated))) > 0) {
        print(collut)
        stop("invalid collut... no duplicates allowed")
      }
      uniquecol <- collut
    }
  }

  if (!is.null(filterids))
    PBx[!paste(PBx[[plotid]], PBx[[pntid]]) %in% filterids, "TOTAL"] <- 9999

  ## Define PBvars2keep
  PBvars2keep <- unique(c(plotid, condid, domainlst, PBvars2keep, pntid))
  PBx <- PBx[,PBvars2keep, with=FALSE]
  if (ratio)
    PBx.d <- PBx.d[,PBvars2keep, with=FALSE]

  ## Create factors for ordering tables
  ##############################################################################
  if (!is.null(uniquerow))
    uniquerow[[rowvar]] <- factor(uniquerow[[rowvar]], levels=unique(uniquerow[[rowvar]]))
  if (!is.null(uniquecol))
    uniquecol[[colvar]] <- factor(uniquecol[[colvar]], levels=unique(uniquecol[[colvar]]))

  returnlst <- list(PBx=PBx, uniquerow=uniquerow, uniquecol=uniquecol,
	domainlst=domainlst, rowvar=rowvar, colvar=colvar, row.orderby=row.orderby,
	col.orderby=col.orderby, row.add0=row.add0, col.add0=col.add0,
	title.rowvar=title.rowvar, title.colvar=title.colvar, title.rowgrp=title.rowgrp)

  if (ratio) returnlst$PBx.d <- PBx.d

  return(returnlst)
}

