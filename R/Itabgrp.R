#wrapSE
#tabgrp

wrapSE <- function(x) {
  xsplit <- strsplit(as.character(x), " \\(")[[1]]
  paste(xsplit, collapse="\r\n (")
}


tabgrp <- function(GBpopdat=NULL, esttype, 
                   rowvar, colvar=NULL, pcfilter=NULL, 
                   rowgrp=FALSE, colgrp=FALSE, collut=NULL, 
                   colgrpcd=NULL, rowgrpnm=NULL, rowgrpord=NULL, 
                   estvar=NULL, estvar.filter=NULL, 
                   title.rowvar=NULL, title.rowgrp=NULL, 
                   row.FIAname=FALSE, col.FIAname=FALSE, 
                   allin1=FALSE, row.add0=FALSE, col.add0=FALSE, 
                   rowgrptot=TRUE, colgrptot=TRUE, 
                   sumunits=TRUE, unitvar="ESTN_UNIT", 
                   rowgrp.subtot=TRUE, rowgrp2.subtot=FALSE, 
                   row.orderby=NULL, title.colvar=NULL, title.colgrp=NULL, 
                   landarea="ALL", col.orderby=NULL, 
                   estnull="--", psenull="--", divideby=NULL) {

  ## Arguments
  # esttype - String. Type of estimate ("AREA", "TREE", "RATIO").  
  # cond - Data frame or data table with FIA condition data.
  # tree - Data frame or data table with FIA tree data.
  # pltstrast - Data frame or data table with FIA plt/strata data.
  # rowvar - String. The row domain in table.
  # colvar - String. The column domain in table.
  # pcfilter - String. A plot/condition filter.
  # rowgrp - Logical. If TRUE, add row groups to table.
  # rowgrpcd - String. Row group domain variable with codes.
  # rowgrpnm - String. Row group domain variable with code names.
  # colgrp - Logical. If TRUE, add column groups to table.
  # colgrpcd - String. Column group domain variable with codes.
  # colgrpnm - String. Column group domain variable with code names.

  ## Set global variables
  rowlutvars=collut2 <- NULL

  if (!is.null(GBpopdat)) {
    ## check cond table
    pltcondx <- pcheck.table(GBpopdat$pltcondx, caption="Condition table?", stopifnull=TRUE)

    ## check tree table
    treex <- pcheck.table(GBpopdat$treex, caption="Tree table?", gui=FALSE, stopifnull=TRUE)

  } else {

    ## check cond table
    pltcondx <- pcheck.table(cond, caption="Condition table?", stopifnull=TRUE)

    ## check tree table
    treex <- pcheck.table(tree, caption="Tree table?", gui=FALSE, stopifnull=TRUE)
  }

  rowgrp <- pcheck.logical(rowgrp, varnm="rowgrp", title="Row groups?", 
		first="NO")
  colgrp <- pcheck.logical(colgrp, varnm="rowgrp", title="Column groups?", 
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
  if (rowvar %in% names(pltcondx)) {
    rowLUTgrp <- FALSE
    if (rowgrp) {
      if (!is.null(rowgrpord)) {
        if (!rowgrpord %in% names(pltcondx)) stop(paste(rowgrpord, "not in cond"))
        if (!is.null(rowgrpnm))
          if (!rowgrpnm %in% names(pltcondx)) stop(paste(rowgrpnm, "not in cond"))
      } else {
        rowLUTgrp <- TRUE
      }
    } 

    if (row.FIAname) {
      if (rowvar == "OWNCD" && rowLUTgrp && "OWNGRPCD" %in% names(pltcondx)) {
        dat <- datLUTnm(x=pltcondx, xvar=rowvar, FIAname=row.FIAname, add0=row.add0)
      } else {       
        dat <- datLUTnm(x=pltcondx, xvar=rowvar, FIAname=row.FIAname, group=rowLUTgrp, 
		add0=row.add0)
      }
      if (is.null(dat)) stop("")
      row.orderby <- rowvar
      pltcondx <- dat$xLUT
      rowvar <- dat$xLUTnm
      ref_row <- setDF(dat$LUT)
      if (rowgrp) {
        if (any(c(rowvar, row.orderby) == "OWNCD")) {
          rowgrpnm <- "OWNGRPCD"
          dat <- datLUTnm(x=pltcondx, xvar=rowgrpnm, FIAname=row.FIAname, add0=row.add0)
          rowgrpord <- rowgrpnm
          pltcondx <- dat$xLUT
          rowgrpnm <- dat$xLUTnm
        } else {
          rowgrpord <- dat$grpcode
          if (is.null(rowgrpord)) stop("")
          rowgrpnm <- dat$grpname
        }
        ref_rowgrp <- unique(pltcondx[!is.na(pltcondx[[rowvar]]), c(rowgrpord, rowgrpnm),
 			with=FALSE])
        setkeyv(ref_rowgrp, rowgrpord)
        rgrpcds <- na.omit(ref_rowgrp[[rowgrpord]])
        if (any(duplicated(rgrpcds))) {
          stop("duplicated row groups in cond: ", rgrpcds[duplicated(rgrpcds)])
        }
      } else {
        rgrpcds <- 9999
      }
      rnames <- c(rowgrpnm, rowvar)
      rowlutvars <- c(rowgrpord, rowgrpnm, row.orderby, rowvar)
    } else { 
      rnames <- c(rowgrpnm, rowvar) 
      rowlutvars <- c(rowgrpord, rowgrpnm, row.orderby, rowvar)

      if (rowgrp) {
        ref_rowgrp <- unique(pltcondx[!is.na(pltcondx[[rowvar]]), c(rowgrpord, rowgrpnm),
 			with=FALSE])
        setkeyv(ref_rowgrp, rowgrpord)
        rgrpcds <- ref_rowgrp[[rowgrpord]]
        if (any(duplicated(rgrpcds)))
          stop("duplicated row groups in cond: ", rgrpcds[duplicated(rgrpcds)])
      }
    }    
    rowlut <- unique(pltcondx[!is.na(get(rowvar)), rowlutvars, with=FALSE])
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
        if (colvar %in% names(pltcondx)) {
          ref_col <- unique(pltcondx[, c(col.orderby, colvar), with=FALSE])
        } else if (colvar %in% names(treex)) {
          ref_col <- unique(treex[, c(col.orderby, colvar), with=FALSE])
        } else {
          stop(colvar, " not in cond")
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
      if (colvar %in% names(pltcondx)) {
        ref_col <- ref_col[ref_col[["VALUE"]] %in% unique(pltcondx[[colvar]]),]
        if (colgrp) 
          ref_colgrp <- ref_colgrp[ref_colgrp[["VALUE"]] %in% unique(pltcondx[[colgrpcd]]),]
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

  ###############################################################
  ## Replace filtered tables
  ###############################################################
  if (!is.null(GBpopdat)) {
    GBpopdat$pltcondx <- pltcondx
    if (esttype != "AREA")
      GBpopdat$treex <- treex
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
   
    if (cgrp == 9999) {
      cond2.filter <- pcfilter
    } else if (!is.null(pcfilter)) {
      cond2.filter <- paste(pcfilter, "&", colgrpcd, "==", cgrp)
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
        if (rowgrpord %in% names(pltcondx)) {
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
          if (rowgrpord %in% names(pltcondx)) {
            if (!is.null(pcfilter)) {
              condtot.filter <- paste(pcfilter, "&", rowgrpord, "==", rgrp)
            } else {
              condtot.filter <- paste(rowgrpord, "==", rgrp)
            }
          } else {
            condtot.filter <- pcfilter
          }
        } else {
          condtot.filter <- pcfilter
        }
        if (esttype == "AREA") { 
          coltottab <- modGBarea(GBpopdat=GBpopdat, 
                                 landarea=landarea, sumunits=sumunits,
                                 pcfilter=condtot.filter, 
                                 rowvar=rowvar, 
                                 table_opts=list(row.orderby=row.orderby, 
                                                 col.orderby=col.orderby, 
                                                 row.add0=TRUE, 
                                                 rowlut=rowlut2, 
                                                 allin1=allin1, 
                                                 estnull=estnull, psenull=psenull,
                                                 divideby=divideby
                                                 ), 
                                 title_opts=list(title.rowvar=title.rowvar))$est
        } else if (esttype == "TREE") {
          coltottab <- modGBtree(GBpopdat=GBpopdat, 
                                 landarea=landarea, sumunits=sumunits, 
                                 pcfilter=condtot.filter, 
                                 estvar=estvar, 
                                 estvar.filter=estvar.filter,
                                 rowvar=rowvar, 
                                 table_opts=list(row.orderby=row.orderby, 
                                                 row.add0=TRUE, 
                                                 rowlut=rowlut2, 
                                                 allin1=allin1, 
                                                 estnull=estnull, psenull=psenull,
                                                 divideby=divideby), 
                                 title_opts=list(title.rowvar=title.rowvar))$est
        }
        if (!subtotal && rgrp != 9999 && nrow(coltottab) > 1) {
          coltottab <- coltottab[-nrow(coltottab),]
        } else if (subtotal) {
          coltottab[coltottab[[title.rowvar]] == "Total", title.rowvar] <- "Subtotal"
        } 
        coltot <- rbind(coltot, setDF(coltottab))
      }
      if (esttype == "AREA") {
        estdat <- 	tryCatch(
        	modGBarea(GBpopdat=GBpopdat, 
        	          landarea=landarea, sumunits=sumunits, 
        	          pcfilter=cond3.filter, 
        	          rowvar=rowvar, colvar=colvar, 
        	          returntitle=TRUE, 
        	          table_opts=list(row.orderby=row.orderby, 
        	                          col.orderby=col.orderby, 
        	                          col.add0=TRUE, row.add0=TRUE, 
        	                          rowlut=rowlut2, collut=collut, 
        	                          allin1=allin1, 
        	                          estnull=estnull, psenull=psenull,
        	                          divideby=divideby), 
        	          title_opts=list(title.rowvar=title.rowvar, 
        	                          title.colvar=title.colvar, 
        	                          title.filter="")), 
        	error=function(err) {
					    message(err)
					    return(NULL)
				  } )
      } else if (esttype == "TREE") { 
 
        estdat <- 	tryCatch(
          modGBtree(GBpopdat=GBpopdat, 
                    landarea=landarea, sumunits=sumunits, 
                    pcfilter=cond3.filter, 
                    estvar=estvar,
                    estvar.filter=estvar2.filter,
                    rowvar=rowvar, colvar=colvar, 
                    returntitle=TRUE, 
                    table_opts=list(row.orderby=row.orderby, 
                                    col.orderby=col.orderby, 
                                    col.add0=TRUE, row.add0=TRUE, 
                                    rowlut=rowlut2, collut=collut, 
                                    allin1=allin1, 
                                    estnull=estnull, psenull=psenull,
                                    divideby=divideby),
                    title_opts=list(title.rowvar=title.rowvar, 
                                    title.colvar=title.colvar, 
                                    title.filter="")), 
          error=function(err) {
            message(err) 
            return(NULL)
				} )
      }

      if (!is.null(estdat)) {
        est <- estdat$est
        est.titlelst <- estdat$titlelst
        title.est <- ifelse(allin1, est.titlelst$title.estpse, est.titlelst$title.est)
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

        #estncols <- length(cgrpcds) + 1
        if (is.data.frame(collut)) {
          estncols <- nrow(collut) + 1
        } else {
          estncols <- length(collut) + 1
        }
   
        #if (rowgrptot) estncols <- estncols + 1

        if (!allin1) {
          est <- cbind(est, as.data.frame(matrix(0, nrow(est), estncols)))
          pse <- est
          pse[pse == 0] <- "--"
        } else {
          est <- cbind(est, 
                       as.data.frame(matrix(allin1f(estnull, psenull), 
                                            nrow(est), estncols)))
        }
        if (j == 1) {
          refnames <- ref_col[match(collut, ref_col$VALUE), "MEANING"]
          names(est) <- c(title.rowvar, refnames, "Total")
          if (!allin1) 
            names(pse) <- c(title.rowvar, refnames, "Total")
        }
      }
      if (rgrp != 9999) {
        est[est[[title.rowvar]] == "Total", title.rowvar] <- "Subtotal"
        est <- cbind(grpnm, est)
        names(est)[names(est) == rowgrpnm] <- title.rowgrp
 
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
          psetabrow <- setDF(rbindlist(list(psetabrow, pse), 
                                       use.names=FALSE, fill=FALSE, idcol=FALSE))
        } else {
          psetabrow <- setDF(pse)
        } 
      }
    }    ## end for j loop
#    if (rowgrp && rowgrptot) {
 
    if (rowgrp) {
      rowvar2 <- rowgrpnm
      row2.orderby <- rowgrpord

      if (esttype == "AREA") {
        estdat2 <- modGBarea(GBpopdat=GBpopdat, 
		                        landarea=landarea, sumunits=sumunits, 
		                        pcfilter=cond2.filter, 
		                        rowvar=rowvar2, colvar=colvar, 
		                        table_opts=list(row.orderby=row2.orderby, 
		                                        col.orderby=col.orderby, 
		                                        row.FIAname=FALSE, 
		                                        row.add0=TRUE, col.add0=TRUE, 
		                                        collut=collut, 
		                                        allin1=allin1, 
		                                        estnull=estnull, psenull=psenull,
		                                        divideby=divideby), 
		                        title_opts=list(title.rowvar=title.rowvar, 
		                                        title.colvar=title.colvar))
      } else if (esttype == "TREE") {
        estdat2 <- modGBtree(GBpopdat=GBpopdat, 
                             landarea=landarea, sumunits=sumunits, 
                             pcfilter=cond2.filter,
                             estvar=estvar,
                             estvar.filter=estvar.filter,
                             rowvar=rowvar2, colvar=colvar, 
                             table_opts=list(row.orderby=row2.orderby, 
                                             col.orderby=col.orderby, 
                                             row.FIAname=FALSE, 
                                             row.add0=TRUE, col.add0=TRUE, 
                                             collut=collut, 
                                             allin1=allin1, 
                                             estnull=estnull, psenull=psenull,
                                             divideby=divideby), 
                             title_opts=list(title.rowvar=title.rowvar, 
                                             title.colvar=title.colvar))
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
        esttabrow <- setDF(rbindlist(list(esttabrow, est.totalrow), use.names=FALSE,
			fill=FALSE, idcol=FALSE))
        if (!allin1) 
          psetabrow <- setDF(rbindlist(list(psetabrow, pse.totalrow), use.names=FALSE,
			fill=FALSE, idcol=FALSE))
#      }
    } 
      if (colvar == "TIMBERCD.PROD") {
        if (colgrpnm == "Reserved") {
          names(esttabrow)[names(esttabrow) %in% c(1,2)] <- c("Productive", "Unproductive")
          if (!allin1)
            names(psetabrow)[names(psetabrow) %in% c(1,2)] <- c("Productive", "Unproductive")
        } else {
          names(esttabrow)[names(esttabrow) %in% c(1,2)] <- c("Timberland", "Unproductive")
          names(esttabrow)[names(esttabrow) %in% c("Productive", "Unproductive")] <- 
			c("Timberland", "Unproductive")
          if (!allin1) {
            names(psetabrow)[names(psetabrow) %in% c(1,2)] <- c("Timberland", "Unproductive")
            names(psetabrow)[names(psetabrow) %in% c("Productive", "Unproductive")] <- 
			c("Timberland", "Unproductive")
          }
        }
     }

    if (cgrp != 9999) {
      ## Change names to concatenate column group name 
      varnms <- paste(colgrpnm, names(esttabrow)[-(1:rnbr)], sep="#")
      names(esttabrow)[-(1:rnbr)] <- varnms
      if (!allin1) {
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
        coltottab2 <- modGBarea(GBpopdat=GBpopdat, 
                                landarea=landarea, sumunits=sumunits,
                                pcfilter=pcfilter, 
                                table_opts=list(allin1=allin1,
                                                divideby=divideby))$est
      } else if (esttype == "TREE") {
        coltottab2 <- modGBtree(GBpopdat=GBpopdat, 
                                landarea=landarea, sumunits=sumunits, 
                                pcfilter=pcfilter, 
                                estvar=estvar,
                                estvar.filter=estvar.filter, 
                                table_opts=list(allin1=allin1, 
                                                divideby=divideby))$est
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
                          tolower(paste0(title.rowgrp, ", ", 
                                         title.rowvar, ", ", title.colgrp, ",")), title.est)
    } else {
      esttab.title <- sub(tolower(title.rowvar), 
                          tolower(paste(title.rowgrp, "and", title.rowvar)), title.est)
    }
    if (!allin1) {
      if (colgrp) {
        psetab.title <- sub(tolower(title.rowvar), 
                            tolower(paste0(title.rowgrp, ", ", 
                                           title.rowvar, ", ", title.colgrp, ",")), 
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

  if (!row.add0)
    esttabcol <- esttabcol[!apply(esttabcol[,cnames], 1, function(x) sum(sapply(x, 
		function(xx) length(strsplit(xx, "--")[[1]]) > 2)) == length(x)),]
  

  returnlst <- list(esttab=esttabcol, esttab.title=esttab.title, 
		title.rowvar=title.rowvar, title.colvar=title.colvar, cnames=cnamesdf)
  if (rowgrp) returnlst$title.rowgrp <- title.rowgrp

  if (!allin1) {
    returnlst$psetab <- psetabcol
    returnlst$psetab.title <- psetab.title
  }

  return(returnlst)
}

