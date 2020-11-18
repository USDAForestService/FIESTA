## allin1f
## crosstabx
## add0unit
## addrowgrp
## crossxtab
## getdomain
## getestvar


allin1f <- function(x, y, char.width=NULL, estnull="--", psenull="--",
	estround=NULL, pseround=NULL) {
  ## DESCRIPTION: Gets estimate (% standard error)

  if (all(is.na(x))) x <- estnull
  if (all(x == 0) && (all(y == 0) || all(y == psenull))) x <- estnull
  if (all(y == 0)) y <- psenull

  if (is.numeric(estnull) || !all(x == estnull)) {
    x <- as.numeric(x)  
    if (!is.null(estround))
      x <- round(x, estround)
  }
 
  if (is.numeric(psenull) || !all(y == psenull)) {
    y <- as.numeric(y)
    if (!is.null(pseround))
      y <- round(y, pseround)
  }
  if (is.null(char.width)) char.width <- max(nchar(y))  

  x.nsmall <- ifelse(!is.null(estround), estround, nbrdecimals(x))
  y.nsmall <- ifelse(!is.null(pseround), pseround, nbrdecimals(x))

  paste0(format(x, big.mark=",", digits=nbrdigits(x), nsmall=x.nsmall), " (", 
	format(y, width=char.width, justify="right", digits=nbrdigits(y), 
	nsmall=y.nsmall), ")")

}



crosstabx <- function(x, xvar, estnm, psenm, allin1=FALSE, char.width=NULL,
		estround=0, pseround=0, estnull="--", psenull="--") {
  
  ## Set global variable
  NBRPLT.gt0 <- NULL

  if (!is.null(estround) && is.numeric(x[[estnm]]))
    x[[estnm]] <- round(x[[estnm]], estround)
  if (!is.null(pseround) && is.numeric(x[[psenm]]))
    x[[psenm]] <- round(x[[psenm]], pseround)

  if (is.character(estnull))
    x[[estnm]] <- as.character(x[[estnm]])
  if (is.character(psenull))
    x[[psenm]] <- as.character(x[[psenm]])

  x[NBRPLT.gt0 == 0, (estnm) := estnull]
  x[NBRPLT.gt0 == 0, (psenm) := psenull]

  if (allin1) {
    if (is.null(char.width)) 
      char.width <- max(nchar(na.omit(x[[psenm]])))

    estpse <- mapply(allin1f, x=x[[estnm]], y=x[[psenm]], char.width=char.width,
		estnull=estnull, psenull=psenull, estround=estround, pseround=pseround)
    names(estpse) <- x[[xvar]]
    return (estpse)
  
  } else {
    est <- x[[estnm]]
    names(est) <- x[[xvar]]
    pse <- x[[psenm]]
    return(list(est=est, pse=pse))
  }
}


add0unit <- function(x, xvar, uniquex, unitvar=NULL, add0, xvar2=NULL, 
	uniquex2=NULL) {
  ## DESCRIPTION: Merges a table with all classes to another table and
  ## 			adds 0s if does not match
  ## x - table to join to
  ## xvar - joining variable
  ## uniquex - lut table with unique values
  ## unitvar - estimation unit variable
 
  byvars <- xvar

  if (!is.null(xvar2)) {
    uniquex.exp <- data.table(expand.grid(uniquex[[xvar]], uniquex2[[xvar2]]))
    setnames(uniquex.exp, c(xvar, xvar2))

    uniquex.exp <- merge(uniquex.exp, uniquex2, by=xvar2)
    uniquex <- merge(uniquex.exp, uniquex, by=xvar)
    byvars <- c(byvars, xvar2)
  }

  addfactors <- function(x, lutx, unitvar=NULL, add0) {
	## DESCRIPTION: to merge lookup table of factor names
	## If add0=TRUE, replaces classes with NA with 0 values
	byvars <- names(lutx)[names(lutx) %in% names(x)]
 	xchk <- FIESTA::check.matchclass(lutx, x, byvars)
	lutx <- xchk$tab1
	x <- xchk$tab2

	if (!is.null(unitvar)) {
        byvars <- c(unitvar, byvars)
        lutx <- data.table(unit=unique(x[[unitvar]]), lutx)
        setnames(lutx, "unit", unitvar)
      } 
 
	setkeyv(lutx, byvars)
	setkeyv(x, byvars)
 
	x <- merge(lutx, x, by=byvars, all.x=add0)
	if (add0) x[is.na(x)] <- 0
	setorderv(x, byvars)
     return(x)
   }
 
   if (!is.null(unitvar)) {
     tab2return <- do.call(rbind, lapply(split(x, by=unitvar), 
		addfactors, lutx=uniquex, unitvar, add0))
   } else {
     tab2return <- addfactors(x, lutx=uniquex, add0=add0)
   }
   return(tab2return)   
}


addrowgrp <- function(x, uniquerow, rowvar, rowgrpnm, title.rnames=NULL) {

   x[[rowgrpnm]] <- uniquerow[match(x[,1], uniquerow[[rowvar]]), get(eval(rowgrpnm))]
   x <- x[, c(ncol(x), 1:(ncol(x)-1))]
   return(x)
}   


crossxtab <- function (group.est, rowvar.est=NULL, colvar.est=NULL, total.est=NULL, 
	rowvar, colvar, estnm, psenm, estround=0, pseround=2, gtotal=TRUE, 
	allin1=FALSE, rowgrp=FALSE, rowgrpnm=NULL, title.rnames=NULL, estnull=0,
	psenull="--", char.width=NULL) {

  ## DESCRIPTION: Internal function to generate 1 table with estimates with percent 
  ##		standard errors

  ## Set global variables
  Total=NBRPLT.gt0 <- NULL

  ## COLUMN TOTALS
  if (is.null(title.rnames)) title.rnames <- rowvar
  rnbr <- length(title.rnames)
  totals <- rep("Total", rnbr)

  ##############################################################################
  ## Round values and get character width for table
  ## Note: If NBRPLT.gt0 = 0, it is replaced by null value (i.e., estnull, psenull)
  ##############################################################################

  if (is.numeric(group.est[[estnm]]))
    group.est[[estnm]] <- round(group.est[[estnm]], estround)
  if (is.numeric(group.est[[psenm]]))
    group.est[[psenm]] <- round(group.est[[psenm]], pseround)
  if (is.null(char.width))
    char.width <- max(nchar(na.omit(group.est[[psenm]])))
  if (is.character(estnull)) 
    group.est[[estnm]] <- as.character(group.est[[estnm]])
  if (is.character(psenull))
    group.est[[psenm]] <- as.character(group.est[[psenm]])
  group.est[NBRPLT.gt0 == 0, (estnm) := estnull]
  group.est[NBRPLT.gt0 == 0, (psenm) := psenull]

  if (!is.null(rowvar.est)) {
    if (is.numeric(rowvar.est[[estnm]]))
      rowvar.est[[estnm]] <- round(rowvar.est[[estnm]], estround)
    if (is.numeric(rowvar.est[[psenm]]))
      rowvar.est[[psenm]] <- round(rowvar.est[[psenm]], pseround)
    row.char.width <- max(nchar(na.omit(rowvar.est[[psenm]])))
    char.width <- max(char.width, row.char.width)

    if (!is.null(rowvar.est)) rowtotal <- sum(rowvar.est[[estnm]])

    if (is.character(estnull))
      rowvar.est[[estnm]] <- as.character(rowvar.est[[estnm]])
    if (is.character(psenull))
      rowvar.est[[psenm]] <- as.character(rowvar.est[[psenm]])

    rowvar.est[NBRPLT.gt0 == 0, (estnm) := estnull]
    rowvar.est[NBRPLT.gt0 == 0, (psenm) := psenull]
  }

  if (!is.null(colvar.est)) {
    if (is.numeric(colvar.est[[estnm]]))
      colvar.est[[estnm]] <- round(colvar.est[[estnm]], estround)
    if (is.numeric(colvar.est[[psenm]]))
      colvar.est[[psenm]] <- round(colvar.est[[psenm]], pseround)
    col.char.width <- max(nchar(na.omit(colvar.est[[psenm]])))
    char.width <- max(char.width, col.char.width)

    if (is.character(estnull))
      colvar.est[[estnm]] <- as.character(colvar.est[[estnm]])
    if (is.character(psenull))
      colvar.est[[psenm]] <- as.character(colvar.est[[psenm]])

    colvar.est[NBRPLT.gt0 == 0, (estnm) := estnull]
    colvar.est[NBRPLT.gt0 == 0, (psenm) := psenull]
  }

  if (!is.null(total.est)) {
    if (is.numeric(total.est[[estnm]]))
      total.est[[estnm]] <- round(total.est[[estnm]], estround)
    if (is.numeric(total.est[[psenm]]))
      total.est[[psenm]] <- round(total.est[[psenm]], pseround)
    tot.char.width <- max(nchar(na.omit(total.est[[psenm]])))
    char.width <- max(char.width, tot.char.width)

    if (is.character(estnull))
      total.est[[estnm]] <- as.character(total.est[[estnm]])
    if (is.character(psenull))
      total.est[[psenm]] <- as.character(total.est[[psenm]])

    total.est[NBRPLT.gt0 == 0, (estnm) := estnull]
    total.est[NBRPLT.gt0 == 0, (psenm) := psenull]
  }

  ## Generate tables
  #################################################################################
  #est <- xtabs(get(estnm) ~ get(rowvar) + get(colvar), group.est)
  #pse <- xtabs(get(psenm) ~ get(rowvar) + get(colvar), group.est)

  if (rowgrp) {
    est <- dcast(group.est, get(rowgrpnm) + get(rowvar) ~ get(colvar), value.var=estnm,
		fill=estnull)
    pse <- dcast(group.est, get(rowgrpnm) + get(rowvar) ~ get(colvar), value.var=psenm,
		fill=psenull)
    crnames <- c("rowgrpnm", "rowvar")
  } else {
    est <- dcast(group.est, get(rowvar) ~ get(colvar), value.var=estnm, fill=estnull)
    pse <- dcast(group.est, get(rowvar) ~ get(colvar), value.var=psenm, fill=psenull)
    crnames <- "rowvar"
  }

  ## Set factor order
  est <- est[order(rowvar), ]
  pse <- pse[order(rowvar), ]

  setnames(est, crnames, title.rnames) 
  setnames(pse, crnames, title.rnames) 
  cnames <- names(est)[!names(est) %in% crnames]

  ## Convert factors to characters
  est[, (title.rnames) := lapply(.SD, as.character), .SDcols=title.rnames]
  pse[, (title.rnames) := lapply(.SD, as.character), .SDcols=title.rnames]
 
  if (allin1) {
    estmat <- as.matrix(est[, -(1:rnbr)])
    psemat <- as.matrix(pse[, -(1:rnbr)])
    
    estall1 <- mapply(allin1f, estmat, psemat, char.width=char.width, estnull=estnull,
		psenull=psenull, estround=estround, pseround=pseround)

    estpse <- data.table(cbind(est[, 1:rnbr], matrix(estall1, nrow(est), ncol(est)-rnbr)))
    names(estpse) <- cnames
  }    
  
  if (is.null(colvar.est) || is.null(rowvar.est)) {
    if (!is.null(colvar.est)) {
      estpse.col <- crosstabx(colvar.est, colvar, estnm, psenm, allin1=allin1,
		char.width=char.width, estnull=estnull, psenull=psenull, 
		estround=estround, pseround=pseround)

      if (allin1) {
        estpse <- rbind(setDF(estpse), c(totals, estpse.col))
      } else {
        est <- rbind(setDF(est), c(totals, estpse.col$est))
        pse <- rbind(setDF(pse), c(totals, estpse.col$pse))
      }
    } else if (!is.null(rowvar.est)) { 
      estpse.row <- crosstabx(rowvar.est, rowvar, estnm, psenm, allin1=allin1,
		char.width=char.width, estnull=estnull, psenull=psenull,
		estround=estround, pseround=pseround)

      if (allin1) {
        estpse$Total <- estpse.row
      } else {
        est$Total <- estpse.row$est
        pse$Total <- estpse.row$pse
      }
    }
  } else if (!is.null(colvar.est) || !is.null(rowvar.est)) {
 
    ## colvar.est
    ##############################################################
    estpse.col <- crosstabx(colvar.est, colvar, estnm, psenm, allin1=allin1,
		char.width=char.width, estnull=estnull, psenull=psenull,
		estround=estround, pseround=pseround)
 
    if (allin1) {
      estpse <- rbind(setDF(estpse), c(totals, estpse.col))
    } else {
      est <- rbind(setDF(est), c(totals, estpse.col$est))
      pse <- rbind(setDF(pse), c(totals, estpse.col$pse))
    }
    ## rowvar.est
    ##############################################################
    estpse.row <- crosstabx(rowvar.est, rowvar, estnm, psenm, allin1=allin1,
		char.width=char.width, estnull=estnull, psenull=psenull,
		estround=estround, pseround=pseround)
    if (!allin1) {
      est.row <- estpse.row$est
      pse.row <- estpse.row$pse
    }

    ## total.est
    ##############################################################
    if (gtotal) {
      if (is.null(total.est)) {
        if (!allin1) {
          est.tot <- sum(as.numeric(est.row), na.rm=TRUE)
          pse.tot <- psenull
        } else {
          estpse.tot <- paste0(format(psenull, big.mark=","), " (", 
			format(psenull, justify="right", width=char.width), ")")
        }
      } else {
        est.tot <- total.est[[estnm]]
        pse.tot <- total.est[[psenm]]
        if (allin1) 
          estpse.tot <- mapply(allin1f, est.tot, pse.tot, char.width=char.width,
			estnull=estnull, psenull=psenull, estround=estround,
			pseround=pseround)
      }
    } else {
      if (allin1) {
        estpse.tot <- paste0(format(psenull, big.mark=","), " (", format(psenull, 
			justify="right", width=char.width), ")")
      } else {
        est.row <- as.character(est.row)
        est.tot <- 0
        pse.tot <- psenull
      }
    }

    ## Merge row
    ##############################################################
    if (allin1) {

      estpse.row <- c(estpse.row, estpse.tot)
      estpse$Total <- estpse.row
    } else {
      est.row <- c(est.row, est.tot)
      pse.row <- c(pse.row, pse.tot)
      est$Total <- est.row
      pse$Total <- pse.row
    }
  }       
  if (allin1) {
    return(estpse)
  } else {
    return(list(est=est, pse=pse))
  }
}


getdomain <- function() {
  ref_domain <- FIESTA::ref_domain
  titlelst <- ref_domain$DOMTITLE

  title <- select.list(c("NONE", titlelst), title="Domain?", multiple=FALSE)
  domain <- ref_domain[ref_domain$DOMTITLE == title, "DOMVARNM"]

  return(list(domain=domain, title.domain=title))
}
	

getestvar <- function() {
  ref_estvar <- FIESTA::ref_estvar
  catlst <- ref_estvar$CATEGORY

  category <- select.list(catlst, title="Category?", multiple=FALSE)
  ref <- ref_estvar[ref_estvar$CATEGORY == category, ]
  
  titlelst <- ref$ESTTITLE
  title <- select.list(titlelst, title="Estimate variable?", multiple=FALSE)
  ref <- ref[ref$ESTTITLE == title, ]

  if (nrow(ref) > 1) stop("more than 1 row selected")

  return(list(estvar=ref$ESTVAR, esttitle=ref$ESTTITLE, 
		est.filter=ref$ESTFILTER, units=ref$ESTUNITS))
}


