#wrapSE
#tabgrp
#setCells		Populates Excel spreadsheets with table data.

#tabgrp <- function(esttype, cond=NULL, tree=NULL, pltstrat=NULL, rowvar, 
#	colvar=NULL, cond.filter=NULL, title.rowvar=NULL, title.colvar=NULL, 
#	title.rowgrp=NULL, row.FIAname=FALSE, col.FIAname=FALSE, estvar=NULL, 
#	estvar.filter=NULL, sumunits=TRUE, landarea=NULL, unitacres=NULL, stratalut=NULL,
#	allin1=FALSE, row.add0=FALSE, col.add0=FALSE) { 

wrapSE <- function(x) {
  xsplit <- strsplit(as.character(x), " \\(")[[1]]
  paste(xsplit, collapse="\r\n (")
}


tabgrp <- function(esttype, cond=NULL, tree=NULL, pltstrat=NULL, rowvar, 
	colvar=NULL, cond.filter=NULL, rowgrp=FALSE, colgrp=FALSE, collut=NULL,
	colgrpcd=NULL, rowgrpnm=NULL, rowgrpord=NULL, estvar.filter=NULL, 
	title.rowvar=NULL, title.rowgrp=NULL, row.FIAname=FALSE, allin1=FALSE, 
	row.add0=FALSE, col.add0=FALSE, rowgrptot=TRUE, colgrptot=TRUE, sumunits=TRUE,
 	unitvar="ESTN_UNIT", rowgrp.subtot=TRUE, rowgrp2.subtot=FALSE, row.orderby=NULL, 
	title.colvar=NULL, title.colgrp=NULL, landarea="ALL", col.orderby=NULL, 
	estnull=0, psenull="--", ...) {

  ## Arguments
  # esttype - String. Type of estimate ("AREA", "TREE", "RATIO").  
  # cond - Data frame or data table with FIA condition data.
  # tree - Data frame or data table with FIA tree data.
  # pltstrast - Data frame or data table with FIA plt/strata data.
  # rowvar - String. The row domain in table.
  # colvar - String. The column domain in table.
  # cond.filter - String. A condition filter.
  # rowgrp - Logical. If TRUE, add row groups to table.
  # rowgrpcd - String. Row group domain variable with codes.
  # rowgrpnm - String. Row group domain variable with code names.
  # colgrp - Logical. If TRUE, add column groups to table.
  # colgrpcd - String. Column group domain variable with codes.
  # colgrpnm - String. Column group domain variable with code names.

  ## Set global variables
  rowlutvars=collut2 <- NULL

  ## COND TABLE
  condx <- pcheck.table(cond, caption="Condition table?")
  if(is.null(condx)){ stop("you must include a cond table")}

  ## TREE TABLE
  treex <- pcheck.table(tree, caption="Tree table?", gui=FALSE)

  rowgrp <- FIESTA::pcheck.logical(rowgrp, varnm="rowgrp", title="Row groups?", 
		first="NO")
  colgrp <- FIESTA::pcheck.logical(colgrp, varnm="rowgrp", title="Column groups?", 
		first="NO")
  if (!colgrp) colgrptot <- FALSE
  subtotal <- rowgrp.subtot
  if (!rowgrp) subtotal=rowgrptot <- FALSE

  title.landarea <- ifelse(landarea == "FOREST", "forest land", 
				ifelse(landarea == "TIMBERLAND", "timberland", "all land"))

  ## Get reference tables
  ###########################################################################
  ref_codes <- FIESTA::ref_codes
  ref_titles <- FIESTA::ref_titles

  ## Get title.rowvar
  ###########################################################################
  if (is.null(title.rowvar)) {
    if (rowvar %in% ref_titles$DOMVARNM) {
      title.rowvar <- ref_titles[ref_titles$DOMVARNM == rowvar, "DOMTITLE"]
    } else if (row.orderby %in% ref_titles$DOMVARNM) {
      title.rowvar <- ref_titles[ref_titles$DOMVARNM == row.orderby, "DOMTITLE"]
    } else {
      title.rowvar <- rowvar
    }
  }

  ## Get column domain look-up table with rowvar and rowgrpcd
  ##############################################################################
  if (rowvar %in% names(condx)) {
    rowLUTgrp <- FALSE
    if (rowgrp) {
      if (!is.null(rowgrpord)) {
        if (!rowgrpord %in% names(condx)) stop(paste(rowgrpord, "not in cond"))
        if (!is.null(rowgrpnm))
          if (!rowgrpnm %in% names(condx)) stop(paste(rowgrpnm, "not in cond"))
      } else {
        rowLUTgrp <- TRUE
      }
    } 
 
    if (row.FIAname) {
      dat <- datLUTnm(x=condx, xvar=rowvar, FIAname=row.FIAname, group=rowLUTgrp, 
		add0=row.add0)
      if (is.null(dat)) stop("")
      row.orderby <- rowvar
      condx <- dat$xLUT
      rowvar <- dat$xLUTnm
      ref_row <- setDF(dat$LUT)
      if (rowgrp) {
        rowgrpord <- dat$grpcode
        if (is.null(rowgrpord)) stop("")
        rowgrpnm <- dat$grpname
        ref_rowgrp <- unique(condx[!is.na(condx[[rowvar]]), c(rowgrpord, rowgrpnm),
 			with=FALSE])
        setkeyv(ref_rowgrp, rowgrpord)
        rgrpcds <- ref_rowgrp[[rowgrpord]]
        if (any(duplicated(rgrpcds)))
          stop("duplicated row groups in cond: ", rgrpcds[duplicated(rgrpcds)])
      } else {
        rgrpcds <- 9999
      }
      rnames <- c(rowgrpnm, rowvar)
      rowlutvars <- c(rowgrpord, rowgrpnm, row.orderby, rowvar)
    } else { 
      rnames <- c(rowgrpnm, rowvar) 
      rowlutvars <- c(rowgrpord, rowgrpnm, row.orderby, rowvar)

      if (rowgrp) {
        ref_rowgrp <- unique(condx[!is.na(condx[[rowvar]]), c(rowgrpord, rowgrpnm),
 			with=FALSE])
        setkeyv(ref_rowgrp, rowgrpord)
        rgrpcds <- ref_rowgrp[[rowgrpord]]
        if (any(duplicated(rgrpcds)))
          stop("duplicated row groups in cond: ", rgrpcds[duplicated(rgrpcds)])
      }
    }    
    rowlut <- unique(condx[!is.na(get(rowvar)), rowlutvars, with=FALSE])
    setkeyv(rowlut, c(rowlutvars))

  } else if (rowvar %in% names(treex)) {
    rowLUTgrp <- FALSE
    if (rowgrp) {
      if (!is.null(rowgrpord)) {
        if (!rowgrpord %in% names(treex)) stop(paste(rowgrpord, "not in tree"))
        if (!is.null(rowgrpnm))
          if (!rowgrpnm %in% names(treex)) stop(paste(rowgrpnm, "not in tree"))
      } else {
        rowLUTgrp <- TRUE
      }
    }
    if (row.FIAname) {
      dat <- datLUTnm(x=treex, xvar=rowvar, FIAname=row.FIAname, group=TRUE, add0=row.add0)
      if (is.null(dat)) stop("")
      row.orderby <- rowvar
      treex <- dat$xLUT
      rowvar <- dat$xLUTnm
      ref_row <- setDF(dat$LUT)

      if (rowgrp) {
        rowgrpord <- dat$grpcode
        rowgrpnm <- dat$grpname
        ref_rowgrp <- unique(treex[!is.na(treex[[row.orderby]]), c(rowgrpord, rowgrpnm), 
			with=FALSE])
        setkeyv(ref_rowgrp, rowgrpord)
        rgrpcds <- ref_rowgrp[[rowgrpord]]
        if (any(duplicated(rgrpcds))) 
          stop("duplicated row groups in cond: ", rgrpcds[duplicated(rgrpcds)])
      } else {
        rgrpcds <- 9999
      }
      row.FIAname <- FALSE
    } else { 

      if (rowgrp) {
        ref_rowgrp <- unique(treex[!is.na(treex[[rowvar]]), c(rowgrpord, rowgrpnm),
 			with=FALSE])
        setkeyv(ref_rowgrp, rowgrpord)
        rgrpcds <- ref_rowgrp[[rowgrpord]]
        if (any(duplicated(rgrpcds)))
          stop("duplicated row groups in tree: ", rgrpcds[duplicated(rgrpcds)])
      }
    }
    rnames <- c(rowgrpnm, rowvar)
    rowlutvars <- c(rowgrpord, rowgrpnm, row.orderby, rowvar)
    rowlut <- unique(treex[!is.na(get(rowvar)), rowlutvars, with=FALSE])
    setkeyv(rowlut, c(rowlutvars))

  } else {
    stop("rowvar not in tables")
  }

  if (!is.null(rowgrpnm)) {
    if (is.null(title.rowgrp)) {
      if (rowgrpord %in% ref_titles$DOMVARNM) {
        title.rowgrp <- ref_titles[ref_titles$DOMVARNM == rowgrpord, "DOMTITLE"]
      } else if (sub("_1", "", rowgrpord) %in% ref_titles$DOMVARNM) {
        title.rowgrp <- ref_titles[ref_titles$DOMVARNM == 
			sub("_1", "", rowgrpord), "DOMTITLE"]               
      } else {
        title.rowgrp <- rowgrpnm
      }
    }
    rowlut[[rowgrpnm]] <- factor(rowlut[[rowgrpnm]], levels=unique(rowlut[[rowgrpnm]]))
  } else {
    rowlut[[rowvar]] <- factor(rowlut[[rowvar]], levels=unique(rowlut[[rowvar]]))
  }

  ## Get rowlut names
  rnbr <- length(rnames)
  totals <- rep("Total", rnbr)

  ## Get column domain look-up table with colvar and colgrpcd (if colgrp=TRUE)
  ##############################################################################
  cgrpcds <- 9999 
  if (!is.null(colvar)) {
    if (is.null(title.colvar)) {
      if (colvar %in% ref_titles$DOMVARNM) {
        title.colvar <- ref_titles[ref_titles$DOMVARNM == colvar, "DOMTITLE"]
      } else {
        title.colvar <- colvar
      }
    }
    if (is.null(collut)) {
      if (is.null(col.orderby)) {
        if (colvar %in% unique(ref_codes[["VARIABLE"]])) {
          ref_col <- ref_codes[ref_codes[["VARIABLE"]] == colvar, c("VALUE", "MEANING")]
          collut <- ref_col[["VALUE"]]
        } else {
          stop(paste(colvar, "not in ref_codes"))
        } 
      } else {
        if (colvar %in% names(condx)) {
          ref_col <- unique(condx[, c(col.orderby, colvar), with=FALSE])
        } else if (colvar %in% names(treex)) {
          ref_col <- unique(treex[, c(col.orderby, colvar), with=FALSE])
        }
        collut <- ref_col
        setcolorder(collut, col.orderby)        
      }
      if (colgrp) {
        if (colgrpcd %in% unique(ref_codes[["VARIABLE"]])) { 
          ref_colgrp <- ref_codes[ref_codes[["VARIABLE"]] == colgrpcd, 
			c("VALUE", "MEANING")]
          ref_colgrp <- ref_colgrp[ref_colgrp[["VALUE"]] != -1, ]
          cgrpcds <- ref_colgrp[["VALUE"]]
        } else {
          stop(paste(colgrpcd, "not in ref_codes"))
        }
      }
    } else {
      if (colvar %in% names(collut)) {
        ref_col <- collut
      } else {
        stop(paste(colvar, "not in collut"))
      }
      if (is.null(col.orderby))
        collut[[colvar]] <- factor(collut[[colvar]], levels=unique(collut[[colvar]]))

      if (colgrp) {
        if (colgrpcd %in% names(collut)) {
          ref_colgrp <- collut
        } else {
          stop(paste(colgrpcd, "not in collut"))
        }
      } 
    } 

#   if (!col.add0) {
      if (colvar %in% names(condx)) {
        ref_col <- ref_col[ref_col[["VALUE"]] %in% unique(condx[[colvar]]),]
        if (colgrp) 
          ref_colgrp <- ref_colgrp[ref_colgrp[["VALUE"]] %in% unique(condx[[colgrpcd]]),]
      } else if (colvar %in% names(treex)) {
        ref_col <- ref_col[ref_col[["VALUE"]] %in% unique(treex[[colvar]]),]
        if (colgrp) 
          ref_colgrp <- ref_colgrp[ref_colgrp[["VALUE"]] %in% unique(treex[[colgrpcd]]),]
      } else {
        stop("colvar not in tables")
      }
#   }

    if (colgrp) {
      if (is.null(title.colgrp)) {
        if (colgrpcd %in% ref_titles$DOMVARNM) {
          title.colgrp <- ref_titles[ref_titles$DOMVARNM == colgrpcd, "DOMTITLE"]
        } else {
          title.colgrp <- colgrpcd
        }
      }
    }
  }

  esttabcol <- {}
  psetabcol <- {}
  pse <- NULL
  coltot <- {}
  #############################################################################
  ## Loop thru column group codes
  #############################################################################
  for (i in 1:length(cgrpcds)) {
    cgrp <- cgrpcds[i]

    if (cgrp != 9999) 
      colgrpnm <- ref_colgrp[ref_colgrp$VALUE == cgrp, "MEANING"]

#      if (colvar == "TIMBERCD.PROD") {
#        col.FIAname <- FALSE

#        if (colgrpnm == "Reserved") {
#          collut <- data.frame(c(1,2), c("Productive", "Nonproductive"))
#        } else {
#          collut <- data.frame(c(1,2), c("Timberland", "Nonproductive"))
#        }
#        names(collut) <- c(colvar, "MEANING")
#        col.orderby <- colvar   
#        colvar <- "MEANING"
#      }            
#    }
    if (cgrp == 9999) {
      cond2.filter <- cond.filter
    } else if (!is.null(cond.filter)) {
      cond2.filter <- paste(cond.filter, "&", colgrpcd, "==", cgrp)
    } else {
      cond2.filter <- paste(colgrpcd, "==", cgrp)
    }
    esttabrow <- {}
    psetabrow <- {}
    cond3.filter <- cond2.filter
    estvar2.filter <- estvar.filter

    for (j in 1:length(rgrpcds)) {
      rgrp <- rgrpcds[j]

      if (rgrp == 9999) {
        cond3.filter <- cond2.filter
        estvar2.filter <- estvar.filter
        rowlut2 <- rowlut
      } else {
        if (rowgrpord %in% names(condx)) {
          if (!is.null(cond2.filter)) {
            cond3.filter <- paste(cond2.filter, "&", rowgrpord, "==", rgrp)
          } else {
            cond3.filter <- paste(rowgrpord, "==", rgrp)
          }
        } else if (rowgrpord %in% names(treex)) {
          if (!is.null(estvar.filter)) {
            estvar2.filter <- paste(estvar.filter, "&", rowgrpord, "==", rgrp)
          } else {
            estvar2.filter <- paste(rowgrpord, "==", rgrp)
          }
        } else {
          stop(paste(rowgrpord, "not in cond or tree table"))
        }
        if (!rowgrp) stop("rowgrp is FALSE")
        grpnm <- ref_rowgrp[ref_rowgrp[[rowgrpord]] == rgrp, rowgrpnm, with=FALSE]
        rowlut2 <- rowlut[rowlut[[rowgrpord]] == rgrp, ]
      } 
      if (colgrptot && i == 1) {
        if (rgrp != 9999) {
          if (rowgrpord %in% names(condx)) {
            if (!is.null(cond.filter)) {
              condtot.filter <- paste(cond.filter, "&", rowgrpord, "==", rgrp)
            } else {
              condtot.filter <- paste(rowgrpord, "==", rgrp)
            }
          } else {
            condtot.filter <- cond.filter
          }
        } else {
          condtot.filter <- cond.filter
        }
        if (esttype == "AREA") { 
          coltottab <- modGBarea(cond=condx, pltstrat=pltstrat, sumunits=sumunits,
			cond.filter=condtot.filter, rowvar=rowvar, row.orderby=row.orderby, 
			title.rowvar=title.rowvar, allin1=allin1, row.add0=TRUE, unitvar=unitvar, 
			rowlut=rowlut2, landarea=landarea, col.orderby=col.orderby, 
			estnull=estnull, psenull=psenull, ...)$est

        } else if (esttype == "TREE") {
          coltottab <- modGBtree(tree=treex, cond=condx, pltstrat=pltstrat, sumunits=sumunits, 
			cond.filter=condtot.filter, rowvar=rowvar, row.orderby=row.orderby,
 			title.rowvar=title.rowvar, allin1=allin1, row.add0=TRUE, unitvar=unitvar, 
			rowlut=rowlut2, estvar.filter=estvar.filter, landarea=landarea, 
			col.orderby=col.orderby, estnull=estnull, psenull=psenull, ...)$est
        }
        if (!subtotal && rgrp != 9999 && nrow(coltottab) > 1) {
          coltottab <- coltottab[-nrow(coltottab),]
        } else if (subtotal) {
          coltottab[coltottab[[title.rowvar]] == "Total", title.rowvar] <- "Subtotal"
        } 
        coltot <- rbind(coltot, setDF(coltottab))
      }
 
      if (esttype == "AREA") {
        estdat <- modGBarea(cond=condx, pltstrat=pltstrat, cond.filter=cond3.filter, 
		rowvar=rowvar, colvar=colvar, row.orderby=row.orderby, returntitle=TRUE, 
		col.add0=TRUE, collut=collut, row.add0=TRUE, rowlut=rowlut2, allin1=allin1, 
		title.rowvar=title.rowvar, sumunits=sumunits, unitvar=unitvar, 
		title.filter="", landarea=landarea, title.colvar=title.colvar, 
		col.orderby=col.orderby, estnull=estnull, psenull=psenull, ...)
 
      } else if (esttype == "TREE") { 
        estdat <- modGBtree(tree=treex, cond=condx, pltstrat=pltstrat, sumunits=sumunits, 
		unitvar=unitvar, cond.filter=cond3.filter, rowvar=rowvar, colvar=colvar,
 		row.orderby=row.orderby, returntitle=TRUE, col.add0=TRUE, collut=collut, 
		row.add0=TRUE, rowlut=rowlut2, title.rowvar=title.rowvar, allin1=allin1, 
		title.filter="", estvar.filter=estvar2.filter, landarea=landarea, 
		title.colvar=title.colvar, col.orderby=col.orderby, estnull=estnull, 
		psenull=psenull, ... )
      }
 
      if (!is.null(estdat)) {
        est <- estdat$est
        est.titlelst <- estdat$titlelst
        title.est <- est.titlelst$title.est
        title.colvar <- est.titlelst$title.colvar
        title.rowvar <- est.titlelst$title.rowvar
        if (!subtotal && rgrp != 9999) est <- est[-nrow(est),]

        if (!allin1) {
          pse <- estdat$pse
          title.pse <- est.titlelst$title.pse
          if (!subtotal && rgrp != 9999) pse <- pse[-nrow(pse),]
        }
      } else {
        est <- setDF(rowlut2[, rowvar, with=FALSE])
        names(est)[names(est) == rowvar] <- title.rowvar
        if (subtotal) est <- rbind(est, "Subtotal")

        estncols <- length(cgrpcds) + 1
        #if (rowgrptot) estncols <- estncols + 1

        if (!allin1) {
          est <- cbind(est, as.data.frame(matrix(0, nrow(est), estncols)))
          pse <- est
          pse[pse == 0] <- "--"
        } else {
          char.width <- max(sapply(esttabrow, function(x) { 
			begin <- gregexpr(pattern='\\(', x)[[1]][1]
			end <- gregexpr(pattern='\\)', x)[[1]][1]
			end - begin - 1}))
          est <- cbind(est, 
			as.data.frame(matrix(allin1f(estnull, psenull, char.width=char.width), 
			nrow(est), estncols)))
        }
      }
      if (rgrp != 9999) {
        est[est[[title.rowvar]] == "Total", title.rowvar] <- "Subtotal"
        est <- cbind(grpnm, est)
        names(est)[names(est) == rowgrpnm] <- title.rowgrp
 
        #esttabrow <- rbind(esttabrow, setDF(est))
        esttabrow <- setDF(rbindlist(list(esttabrow, est), use.names=FALSE, 
			fill=FALSE, idcol=FALSE))
      } else {
        esttabrow <- setDF(est)
      }

      if (!allin1 && !is.null(pse)) {
        if (rgrp != 9999) {
          pse[pse[[title.rowvar]] == "Total", title.rowvar] <- "Subtotal"
          pse <- cbind(grpnm, pse)
          names(pse)[names(pse) == rowgrpnm] <- title.rowgrp
          #psetabrow <- rbind(psetabrow, setDF(pse))
          psetabrow <- setDF(rbindlist(list(psetabrow, pse), use.names=FALSE, 
			fill=FALSE, idcol=FALSE))
        } else {
          psetabrow <- setDF(pse)
        } 
      }
    }    ## end for loop
 
#    if (rowgrp && rowgrptot) {
    if (rowgrp) {
      rowvar2 <- rowgrpnm
      row2.orderby <- rowgrpord

      if (esttype == "AREA") {
        estdat2 <- modGBarea(cond=condx, pltstrat=pltstrat, cond.filter=cond2.filter, 
		rowvar=rowvar2, row.orderby=row2.orderby, colvar=colvar, row.FIAname=FALSE,
		col.add0=TRUE, collut=collut, row.add0=TRUE, allin1=allin1, 
		title.rowvar=title.rowvar, title.colvar=title.colvar, sumunits=sumunits, 
		unitvar=unitvar, landarea=landarea, col.orderby=col.orderby, estnull=estnull,
 		psenull=psenull, ...)

      } else if (esttype == "TREE") {
        estdat2 <- modGBtree(tree=treex, cond=condx, pltstrat=pltstrat, sumunits=sumunits, 
		unitvar=unitvar, cond.filter=cond2.filter, rowvar=rowvar2, row.orderby=row2.orderby, 
		colvar=colvar, row.FIAname=FALSE, title.rowvar=title.rowvar, col.add0=TRUE, 
		collut=collut, row.add0=TRUE, allin1=allin1, estvar.filter=estvar.filter, 
		landarea=landarea, title.colvar=title.colvar, col.orderby=col.orderby, 
		estnull=estnull, psenull=psenull, ...)

      }

      est2 <- estdat2$est
      if (!allin1) pse2 <- estdat2$pse
      
      if (is.null(est2)) {
        est2 <- setDF(rowlut2[, rowvar, with=FALSE])
        names(est2)[names(est2) == rowvar] <- title.rowvar
        est2 <- cbind(est2, as.data.frame(matrix(0, nrow(est2), length(cgrpcds)+1)))
        est.totalrow <- cbind(rgrp="Total", est2)
        names(est.totalrow)[names(est.totalrow) == "rgrp"] <- title.rowgrp

        if (!allin1) {
          pse.totalrow <- est.totalrow
          pse.totalrow[pse.totalrow == 0] <- "--"
        }
      } else {
        est.totalrow <- cbind(rgrp="Total", est2[est2[[title.rowvar]] == "Total",])
        names(est.totalrow)[names(est.totalrow) == "rgrp"] <- title.rowgrp
        if (!allin1) {
          pse.totalrow <- cbind(rgrp="Total", pse2[pse2[[title.rowvar]] == "Total",])
          names(pse.totalrow)[names(pse.totalrow) == "rgrp"] <- title.rowgrp
        }
      }

      ## Add total row
      ###########################################################
 #     if (rowgrptot) {
        esttabrow <- rbind(esttabrow, est.totalrow)
        if (!allin1) 
          psetabrow <- rbind(psetabrow, pse.totalrow)
#      }
    } 
      if (colvar == "TIMBERCD.PROD") {
        if (colgrpnm == "Reserved") {
          names(esttabrow)[names(esttabrow) %in% c(1,2)] <- c("Productive", "Nonproductive")
          if (!allin1) 
            names(psetabrow)[names(psetabrow) %in% c(1,2)] <- c("Productive", "Nonproductive")
        } else {
          names(esttabrow)[names(esttabrow) %in% c(1,2)] <- c("Timberland", "Nonproductive")
          if (!allin1) 
            names(psetabrow)[names(psetabrow) %in% c(1,2)] <- c("Timberland", "Nonproductive")
        }
     }

 
    if (cgrp != 9999) {
      ## Change names to concatenate column group name 
      varnms <- paste(colgrpnm, names(esttabrow)[-(1:rnbr)], sep="#")
      names(esttabrow)[-(1:rnbr)] <- varnms
      if (colgrp && !allin1) {
        varnms <- paste(colgrpnm, names(psetabrow)[-(1:rnbr)], sep="#")
        names(psetabrow)[-(1:rnbr)] <- varnms
      }
    }

    ## Initialize esttab and psetab
    if (i == 1) {
      if (!is.null(title.rowvar)) 
        rnames[rnames == rowvar] <- title.rowvar
      if (!is.null(title.rowgrp)) 
        rnames[rnames == rowgrpnm] <- title.rowgrp

      esttabcol <- esttabrow 
      if (!is.null(pse))
        psetabcol <- psetabrow
    } else {

      ## Merge column groups
      ##################################################################
      esttabcol <- cbind(esttabcol, esttabrow[, which(!names(esttabrow) %in% rnames)])
      if (!is.null(pse))     
        psetabcol <- cbind(psetabcol, psetabrow[, which(!names(psetabrow) %in% rnames)])      
    } 
  }
 
  if (colgrptot) {
    if (rowgrptot) {
      if (esttype == "AREA") {
        coltottab2 <- modGBarea(cond=condx, pltstrat=pltstrat, sumunits=sumunits,
		unitvar=unitvar, landarea=landarea, cond.filter=cond.filter, allin1=allin1, 
		...)$est

      } else if (esttype == "TREE") {
        coltottab2 <- modGBtree(cond=condx, pltstrat=pltstrat, tree=treex, 
			sumunits=sumunits, unitvar=unitvar, landarea=landarea, 
			cond.filter=cond.filter, estvar.filter=estvar.filter, allin1=allin1, 
			...)$est

      }
      names(coltottab2)[names(coltottab2) == "TOTAL"] <- title.rowvar
      coltot <- rbind(coltot, coltottab2)
    } 
    if (allin1) {
      esttabcol <- cbind(esttabcol, Total=coltot[, "Estimate (% Sampling Error)"], 
		stringsAsFactors=FALSE)
    } else {
      esttabcol <- cbind(esttabcol, Total=coltot[, "Estimate"], stringsAsFactors=FALSE)
    }

    if (!is.null(pse))     
      psetabcol <- cbind(psetabcol, Total=coltot[, "Percent Sampling Error"],
		stringsAsFactors=FALSE)
  }

  ## esttab.title
  ###########################################################
  if (rowgrp) {
    if (colgrp) {
      esttab.title <- sub(tolower(title.rowvar), 
		tolower(paste0(title.rowgrp, ", ", title.rowvar, ", ", title.colgrp, ", ")), 
		title.est)
    } else {
      esttab.title <- sub(tolower(title.rowvar), 
		tolower(paste(title.rowgrp, "and", title.rowvar)), title.est)
    }
    if (!allin1) {
      if (colgrp) {
        psetab.title <- sub(tolower(title.rowvar), 
		tolower(paste0(title.rowgrp, ", ", title.rowvar, ", ", title.colgrp, ", ")), 
		title.pse)
      } else {
        psetab.title <- sub(tolower(title.rowvar), 
		tolower(paste(title.rowgrp, "and", title.rowvar)), title.pse)
      }
    }
  } else {
    if (colgrp) {
      esttab.title <- sub(tolower(title.rowvar), 
		tolower(paste0(title.rowvar, ", ", title.colgrp, ",")), title.est)
      if (!allin1)
        psetab.title <- sub(tolower(title.rowvar), 
		tolower(paste0(title.rowvar, ", ", title.colgrp, ",")), title.pse)
    } else {
      esttab.title <- title.est
      if (!allin1) psetab.title <- title.pse
    }
  }

  cnames <- names(esttabcol)[-(1:rnbr)]
  cnameslst <- sapply(cnames, strsplit, "#")
  cnamesdf <- do.call(rbind.data.frame, cnameslst)
  cnamesdf[] <- lapply(cnamesdf, as.character)

  if (colgrp) {
    names(cnamesdf) <- c("COLGRP", "COLVAR")
  } else {
    names(cnamesdf) <- "COLVAR"
  }


  returnlst <- list(esttab=esttabcol, esttab.title=esttab.title, 
		title.rowvar=title.rowvar, title.colvar=title.colvar, cnames=cnamesdf)
  if (rowgrp) returnlst$title.rowgrp <- title.rowgrp

  if (!allin1) {
    returnlst$psetab <- psetabcol
    returnlst$psetab.title <- psetab.title
  }

  return(returnlst)
}


setCells <- function(datsheet, estgrp, psegrp, nbrgrps=1, startrow=1, endrow=NULL,
		subtotal=TRUE, totcols, estcols, psecols, esttab.style, psetab.style, 
		rnames=rnames, norname=FALSE, allin1=FALSE, addSEcol=FALSE) {

  ###############################################################################
  ## DESCRIPTION: Populates Excel spreadsheets with table data.
  ## ARGUMENTS:
  ## datsheet		jobjRef. Excel .xlsx sheet
  ## estgrp			DF. Table with estimates
  ## psegrp			DF. Table with percent standard errors
  ## nbrgrps		Number. Number of row groups
  ## startrow		Number. The starting row number for populating cells
  ## endrow			Number. The ending row number for populating cells
  ## subtotal		Logical. If TRUE, subtotal is in table.
  ## totcols		Number. Number of total columns in output table.
  ## estcols		Number. Columns identifying estimates.
  ## psecols 		Number. Columns identifying percent standard errors.
  ## esttab.style	CellStyle. Style for output estimates.
  ## psetab.style	CellStyle. Style for output percent standard errors.
  ## rnames			String vector. Row name(s).
  ## norname		Logical. If there no rname is used for output table.
  ## allin1			Logical. If TRUE, estimate and percent standard error in same cell.
  ## addSEcol		Logical. If TRUE, %SE is included in table. 

  ## Create new rows for table data
  ################################################################
  if (is.null(endrow)) endrow <- nrow(estgrp)
  endrow.tab <- startrow + endrow-1
  tab.rows <- xlsx::createRow(datsheet, startrow:endrow.tab)
  tab.cells <- xlsx::createCell(tab.rows, colIndex=1:totcols)
 
  if (is.null(norname) || is.na(norname)) norname <- FALSE
  colIndex.rname <- 1:(ifelse(norname, nbrgrps, nbrgrps + 1))

  rnamecol <- ifelse(norname, nbrgrps, nbrgrps + 1)
  if (rnamecol == 0) rnamecol <- 1
  tabcols <- (nbrgrps+2):ncol(estgrp)

  ## Add row names 
  ################################################################
  rnamevals <- as.matrix(estgrp[, rnames[rnamecol]])
  value.cells <- mapply(xlsx::setCellValue, tab.cells[,rnamecol], rnamevals)

  ## Add table data values
  ################################################################
  estgrp.cells <- xlsx::getCells(tab.rows, colIndex=estcols)
  #tabvals <- t(as.matrix(estgrp[, -(colIndex.rname)]))
  tabvals <- t(as.matrix(estgrp[, tabcols]))

  if (allin1) tabvals <- mapply(wrapSE, tabvals)
  value.cells <- mapply(xlsx::setCellValue, estgrp.cells, tabvals)

  ## Set style for cell data values
  ################################################################
  style.cells <- lapply(estgrp.cells, 
			function(x) xlsx::setCellStyle(x, esttab.style))

  ## Edit NULL values in Percent Sampling Error Columns
  ################################################################    
  if (!allin1) {
    ## Change null values, 0 values, and NA values of est cells to --
    lapply(estgrp.cells, function(x) {
      cellval <- xlsx::getCellValue(x)
      if (is.null(cellval) | cellval == 0 | is.na(cellval))  
			xlsx::setCellValue(x, "--")
    })

    if (addSEcol) {
      psegrp.cells <- xlsx::getCells(tab.rows, colIndex=psecols)
      psegrp <- data.frame(lapply(psegrp, function(x) gsub("--", 0, x)), stringsAsFactors=FALSE)
      psegrp <- suppressMessages(FIESTA::check.numeric(psegrp, tabcols))

      value.cells <- mapply(xlsx::setCellValue, psegrp.cells, 
			t(as.matrix(psegrp[, tabcols])))
      style.cells <- lapply(psegrp.cells, 
			function(x) xlsx::setCellStyle(x, psetab.style))

      ## Change null values, 0 values, and NA values of pse cells to --
      lapply(psegrp.cells, function(x) {
        cellval <- xlsx::getCellValue(x)
        if (is.null(cellval) | cellval == 0 | is.na(cellval))  xlsx::setCellValue(x, "--")
      })
    }
  }
}

