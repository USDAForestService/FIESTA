est.outtabs <- function(esttype, phototype="PCT", photoratio=FALSE, sumunits=FALSE, 
	areavar, unitvar, unitvars=NULL, unit.totest, unit.rowest=NULL, unit.colest=NULL, 
	unit.grpest=NULL, rowvar=NULL, colvar=NULL, uniquerow=NULL, uniquecol=NULL, 
	rowgrp=FALSE, rowgrpnm=NULL, rowunit=NULL, totunit=NULL, allin1=FALSE, 
	savedata=FALSE, addtitle=FALSE, returntitle=FALSE, title.ref=NULL, 
	title.colvar=NULL, title.rowvar=NULL, title.rowgrp=NULL, title.unitvar=NULL, 
	title.estpse=NULL, title.est=NULL, title.pse=NULL, outfn.estpse=NULL, 
	outfolder=NULL, outfn.date=TRUE, overwrite=FALSE, estnm, psenm="pse", 
	estround=0, pseround=2, estnull="--", psenull="--", divideby=NULL, 
	coltitlerow=TRUE, rowtotal=TRUE, rawdata=FALSE, CI=TRUE, rawdat=NULL, 
	char.width=NULL, rawonly=FALSE){ 

  ## Set global variables
  estn=pse=keepvars=TOTAL=totest=rowest=colest=grpest <- NULL

  #####  TITLE INFO FOR TABLE COLUMNS
  ########################################################
  if (allin1) {
    title.yhatpse <- "Estimate (% Sampling Error)"
  } else {
    title.yhat <- "Estimate"
    title.yhat.pse <- "Percent Sampling Error"
  }

  ## Check if a total table exists
  if (!is.null(unit.totest)) gtotal <- TRUE

  if (!is.null(title.ref) && title.ref != "")
    title.ref <- paste(";", title.ref)

  
  ## Define est.var name
  estnm.var <- paste0(estnm, ".var")
  senm <- paste0(estnm, ".se")
  estnm2 <- ifelse(esttype == "RATIO", "rhat", estnm)
  senm2 <- paste0(estnm2, ".se")


  ## Get conversion number
  dividebynum <- NULL
 
  if (!is.null(divideby)) {
    dividebynum <- ifelse(divideby == "hundred", 100, 
				ifelse(divideby == "thousand", 1000, 
					ifelse(divideby == "million", 1000000, 1)))
    estnmd <- paste(estnm, divideby, sep=".")
    senmd <- paste(senm, divideby, sep=".")
  } else {
    estnmd <- estnm
    senmd <- senm
  }
  estnmd <- ifelse(esttype == "RATIO", "rhat", estnmd)
  senmd <- ifelse(esttype == "RATIO", "rhat.se", senmd)

  rnames <- rowvar
  title.rnames <- title.rowvar
  totals <- "Total"
  if (!is.null(rowgrpnm)) {
    rnames <- c(rowgrpnm, rowvar)
    title.rnames <- c(title.rowgrp, title.rnames)
    totals <- c(totals, "Total")
  }
  rnbr <- length(rnames)

  ## Set up lists
  returnlst <- list()
  titlelst <- list()

  ## sumunits = FALSE
  if (!is.null(unit.totest)) {
    if (esttype == "RATIO") {
      unit.totest <- suppressWarnings(FIESTA::getrhat(unit.totest))
    } else {
      if (!is.null(dividebynum)) {
        unit.totest[[estnmd]] <- unit.totest[[estnm2]] / dividebynum
        unit.totest[[senmd]] <- unit.totest[[senm2]] / dividebynum
      } else {
        unit.totest[[estnmd]] <- unit.totest[[estnm2]]
      }  
      unit.totest[[psenm]] <- unit.totest[[psenm]]
    }
    if (allin1) {
      char.width <- max(nchar(na.omit(round(unit.totest[[psenm]], pseround)))) 
      if (!is.null(char.width) && char.width == -Inf) char.width <- 0
    }
  }

  if (!is.null(unit.rowest)) {
    if (esttype == "RATIO") {
      unit.rowest <- suppressWarnings(FIESTA::getrhat(unit.rowest))
    } else {
      if (!is.null(dividebynum)) {
        unit.rowest[[estnmd]] <- unit.rowest[[estnm2]] / dividebynum
        unit.rowest[[senmd]] <- unit.rowest[[senm2]] / dividebynum
      } else {
        unit.rowest[[estnmd]] <- unit.rowest[[estnm2]]
      }
      unit.rowest[[psenm]] <- unit.rowest[[psenm]]
    }
    if (allin1) {
      char.width <- max(char.width, 
		max(nchar(na.omit(round(unit.rowest[[psenm]], pseround)))))
    }
  }

  if (!is.null(unit.colest)) {
    if (esttype == "RATIO") {
      unit.colest <- suppressWarnings(FIESTA::getrhat(unit.colest))
    } else {
      if (!is.null(dividebynum)) {
        unit.colest[[estnmd]] <- unit.colest[[estnm2]] / dividebynum
        unit.colest[[senmd]] <- unit.colest[[senm2]] / dividebynum
      } else {
        unit.colest[[estnmd]] <- unit.colest[[estnm2]]
      }
      unit.colest[[psenm]] <- unit.colest[[psenm]]
    }
    if (allin1) {
      char.width <- max(char.width, 
		max(nchar(na.omit(round(unit.colest[[psenm]], pseround)))))
    }
  }

  if (!is.null(unit.grpest)) {
    if (esttype == "RATIO") {
      unit.grpest <- suppressWarnings(FIESTA::getrhat(unit.grpest))
    } else {
      if (!is.null(dividebynum)) {
        unit.grpest[[estnmd]] <- unit.grpest[[estnm2]] / dividebynum
        unit.grpest[[senmd]] <- unit.grpest[[senm2]] / dividebynum
      } else {
        unit.grpest[[estnmd]] <- unit.grpest[[estnm2]]
      }
      unit.grpest[[psenm]] <- unit.grpest[[psenm]]
    }
    if (allin1) {
      char.width <- max(char.width, 
		max(nchar(na.omit(round(unit.grpest[[psenm]], pseround)))))
    }
  }

  if (sumunits) {
    ## Group estimates
    if (!is.null(uniquerow)) 
      keepvars.row <- names(uniquerow)[names(uniquerow) != rowvar]
    if (!is.null(uniquecol)) 
      keepvars.col <- names(uniquecol)[names(uniquecol) != colvar]

    ## GROUP TOTAL TABLE
    if (!is.null(unit.totest)) {
      totest <- groupUnits(unit.totest, estncol=estnm, estncol.var=estnm.var,
		domain="TOTAL", esttype=esttype, rowgrpnm=rowgrpnm, unitvar=unitvar, 
		areavar=areavar, phototype=phototype, photoratio=photoratio)
      if (esttype != "RATIO" && !is.null(dividebynum)) {
        totest[[estnmd]] <- totest[[estnm2]] / dividebynum
        totest[[senmd]] <- totest[[senm2]] / dividebynum
      } else {
        totest[[estnmd]] <- totest[[estnm2]]
      }  
      totest[[psenm]] <- totest[[psenm]]     
      if (allin1) {
        char.width <- max(nchar(na.omit(round(totest[[psenm]], pseround)))) 
        if (!is.null(char.width) && char.width == -Inf) char.width <- 0
      }
    }
    if (!is.null(unit.rowest)) {
      rowest <- groupUnits(tabest=unit.rowest, domain=rowvar, estncol=estnm,
			estncol.var=estnm.var, esttype=esttype, rowgrpnm=rowgrpnm,
			unitvar=unitvar, areavar=areavar, phototype=phototype, 
			photoratio=photoratio, keepvars=keepvars.row)
      if (esttype != "RATIO" && !is.null(dividebynum)) {
        rowest[[estnmd]] <- rowest[[estnm2]] / dividebynum
        rowest[[senmd]] <- rowest[[senm2]] / dividebynum
        unit.rowest[[estnmd]] <- unit.rowest[[estnm2]] / dividebynum
        unit.rowest[[senmd]] <- unit.rowest[[senm2]] / dividebynum
      } else {
        rowest[[estnmd]] <- rowest[[estnm2]]
      }
      rowest[[psenm]] <- rowest[[psenm]]
      if (allin1) {
        char.width <- max(char.width, 
		max(nchar(na.omit(round(rowest[[psenm]], pseround)))))
      }
    }
    if (!is.null(unit.colest)) {
      colest <- groupUnits(tabest=unit.colest, domain=colvar, estncol=estnm,
			estncol.var=estnm.var, esttype=esttype, unitvar=unitvar, 
			areavar=areavar, phototype=phototype, photoratio=photoratio,
			keepvars=keepvars.col)
      if (esttype != "RATIO" && !is.null(dividebynum)) {
        colest[[estnmd]] <- colest[[estnm2]] / dividebynum
        colest[[senmd]] <- colest[[senm2]] / dividebynum
        unit.colest[[estnmd]] <- unit.colest[[estnm2]] / dividebynum
        unit.colest[[senmd]] <- unit.colest[[senm2]] / dividebynum
      } else {
        colest[[estnmd]] <- colest[[estnm2]]
      }
      colest[[psenm]] <- colest[[psenm]]
      if (allin1) {
        char.width <- max(char.width, 
		max(nchar(na.omit(round(colest[[psenm]], pseround)))))
      }
    }
    if (!is.null(unit.grpest)) {
      grpest <- groupUnits(tabest=unit.grpest, domain=rowvar, estncol=estnm, 
			estncol.var=estnm.var, domvar2=colvar, esttype=esttype,
			rowgrpnm=rowgrpnm, unitvar=unitvar, areavar=areavar, 
			phototype=phototype, photoratio=photoratio, 
			keepvars=c(keepvars.row, keepvars.col))
      if (esttype != "RATIO" && !is.null(dividebynum)) {
        grpest[[estnmd]] <- grpest[[estnm2]] / dividebynum
        grpest[[senmd]] <- grpest[[senm2]] / dividebynum
        unit.grpest[[estnmd]] <- unit.grpest[[estnm2]] / dividebynum
        unit.grpest[[senmd]] <- unit.grpest[[senm2]] / dividebynum
      } else {
        grpest[[estnmd]] <- grpest[[estnm2]]
      }
      grpest[[psenm]] <- grpest[[psenm]]
      if (allin1) {
        char.width <- max(char.width, 
		max(nchar(na.omit(round(grpest[[psenm]], pseround)))))
      }
    } 
  }

  if (!rawonly) {
  ###################################################################
  ## GENERATE OUTPUT TABLES
  ###################################################################
  if (colvar == "NONE") {
    if (sumunits) {
      if (rowvar == "TOTAL") {
        if (allin1) {
          estpse <- data.frame(TOTAL="Total", allin1f(round(totest[[estnmd]], estround), 
			round(totest[[psenm]], pseround), estnull=estnull, psenull=psenull, 
			char.width=char.width), stringsAsFactors=FALSE)
          setnames(estpse, c("TOTAL", title.yhatpse))

        } else {
          estpse <- data.table(TOTAL="Total", 
			totest[, c(estnmd, psenm), with=FALSE],
			stringsAsFactors=FALSE)
          estpse[[estnmd]] <- round(estpse[[estnmd]], estround)
          estpse[[psenm]] <- round(estpse[[psenm]], pseround)
          names(estpse) <- c("TOTAL", title.yhat, title.yhat.pse)
        }
        rowtotal <- FALSE

      } else {  ##  rowvar != "TOTAL"
        ## Get estimates
        estcross <- crosstabx(rowest, rowvar, estnmd, psenm, allin1=allin1,
		estnull=estnull, psenull=psenull, char.width=char.width,
		estround=estround, pseround=pseround)

        if (allin1) {
          tottab <- data.frame(TOTAL="Total", allin1f(totest[[estnmd]], 
			totest[[psenm]], char.width=char.width, estnull=estnull, 
			psenull=psenull, estround=estround, pseround=pseround), stringsAsFactors=FALSE)
          setnames(tottab, c("TOTAL", title.yhatpse))

          estpse <- estcross
          estpse <- data.frame(names(estcross), estcross, 
					stringsAsFactors=FALSE, row.names=NULL)
          if (rowgrp)
            estpse <- FIESTA::addrowgrp(estpse, uniquerow, rowvar, rowgrpnm)
          names(estpse) <- c(title.rnames, title.yhatpse)

          if (!is.null(totest)) {
            for (rname in title.rnames) 
              estpse[[rname]] <- as.character(estpse[[rname]])
            estpse <- rbind(estpse, c(totals, tottab[, title.yhatpse]))
          }

        } else {
          est <- estcross$est
          pse <- estcross$pse

          estpse <- data.frame(names(est), est, pse, stringsAsFactors=FALSE, row.names=NULL)
          names(estpse) <- c(rowvar, title.yhat, title.yhat.pse)

          if (rowgrp)
            estpse <- FIESTA::addrowgrp(estpse, uniquerow, rowvar, rowgrpnm)
          names(estpse) <- c(title.rnames, title.yhat, title.yhat.pse)

          if (!is.null(totest)) {
            for (rname in title.rnames) 
              estpse[[rname]] <- as.character(estpse[[rname]])

            tottab <- c(totals, round(totest[[estnmd]], estround), round(totest[[psenm]], pseround))
            names(tottab) <- c(totals, title.yhat, title.yhat.pse) 
            estpse <- rbind(estpse, tottab)
          }
        }
      }

      ## TABLES TO RETURN
      est2return <- estpse
      pse2return <- NULL

      ## SAVE TO FILE
      if ((savedata & addtitle) | returntitle) 
        title.estpse <- paste0(title.estpse, title.ref)
 
      if (savedata) {
        suppressWarnings(
        save1tab(tab=setDF(estpse), tab.title=title.estpse, outfn=outfn.estpse, 
			outfolder=outfolder, allin1=allin1, coltitlerow=FALSE, 
			addtitle=addtitle, rowtotal=rowtotal, outfn.date=outfn.date, 
			overwrite=overwrite, charvars=title.rnames, cols2format=title.yhat) )
      }
            
    } else {  ## colvar == NONE, sumunits = FALSE
      ## GET INITIAL TABLE AND TOTALS
      estnmd <- ifelse(esttype == "RATIO", "rhat", estnmd)
      senmd <- ifelse(esttype == "RATIO", "rhat.se", senmd)

      ## GET TOTAL TABLE
      if (!is.null(unit.totest)) {
        if (allin1) { 
            ## Get table with estimates by unitvar
             estpsetot <- data.table(unit.totest[[unitvar]], 
			crosstabx(x=unit.totest, unitvar, estnm=estnmd, 
				psenm=psenm, allin1=TRUE, estnull=estnull, 
				psenull=psenull, char.width=char.width,
				estround=estround, pseround=pseround))
            setnames(estpsetot, c(title.unitvar, title.yhatpse))
        } else {
          ## GET TABLE OF ESTIMATES WITH TITLES
          estpsetot <- unit.totest[,c(unitvar, estnmd, psenm), with=FALSE]
          estpsetot[[estnmd]] <- round(estpsetot[[estnmd]], estround)
          estpsetot[[psenm]] <- round(estpsetot[[psenm]], pseround)
          setnames(estpsetot, c(title.unitvar, title.yhat, title.yhat.pse))
        }
      }

      if (!is.null(totunit)) {
        if (esttype == "RATIO") {
          totunit <- suppressWarnings(getrhat(totunit))
        } else {
          if (!is.null(divideby)) {
            totunit[, (estnmd) := round(get(estnm2) / dividebynum, estround)]
            totunit[, (senmd) := get(senm2) / dividebynum]
          }
          totunit[, (estnmd) := round(get(estnmd), estround)]
          totunit[, (psenm) := round(get(psenm), pseround)]
        }
      }

      if (rowvar == "TOTAL") {
        if (!is.null(totunit)) {
          totunit <- setDF(totunit)
          if (allin1) {
            totunit <- data.frame(TOTAL="Total", allin1f(totunit[[estnmd]], totunit[[psenm]],
			estnull=estnull, psenull=psenull, char.width=char.width, estround=estround,
			pseround=pseround), stringsAsFactors=FALSE)
            setnames(totunit, c(unitvar, title.yhatpse))
          } else {         
            totunit <- c("TOTAL", totunit[, c(estnmd, psenm)])
            names(totunit) <- c(unitvar, title.yhat, title.yhat.pse)
          }
          if (!is.null(estpsetot))
            estpsetot <- rbind(setDF(estpsetot), totunit)
        } 

        ## TABLES TO RETURN
        est2return <- setDF(estpsetot)
        pse2return <- NULL

        if ((savedata & addtitle) | returntitle)
          title.estpse <- paste0(title.estpse, title.ref)

        if (savedata) 
          suppressWarnings(
          FIESTA::save1tab(tab=est2return, tab.title=title.estpse, 
			outfn=outfn.estpse, outfolder=outfolder, allin1=allin1, 
			coltitlerow=FALSE, rowtotal=FALSE, addtitle=addtitle,
			outfn.date=outfn.date, overwrite=overwrite, cols2format=title.yhat))

      } else {  ## rowvar != "TOTAL"

        if (!is.null(rowunit)) {
          if (esttype == "RATIO") {
            rowunit <- suppressWarnings(FIESTA::getrhat(rowunit))
          } else {
            if (!is.null(divideby)) {
              rowunit[, (estnmd) := get(estnm2) / dividebynum]
              rowunit[, (senmd) := get(senm2) / dividebynum]
            }
            rowunit[, (estnmd) := round(get(estnmd), estround)]
            rowunit[, (psenm) := round(get(psenm), pseround)]
          }
        }
        if (!is.null(unit.totest) && !is.null(uniquecol)) {
          unit.totest <- add0unit(x=unit.totest, xvar=unitvar, uniquex=uniquecol, 
			xvar.add0=TRUE)
        }
        ## Get table of estimates
        estpsecross <- crossxtab(group.est=unit.rowest, rowvar.est=rowunit, 
			colvar.est=unit.totest, total.est=totunit, rowvar=rowvar, 
			colvar=unitvar, estnm=estnmd, psenm=psenm, allin1=allin1, 
			rowgrp=rowgrp, rowgrpnm=rowgrpnm, title.rnames=title.rnames,
			estround=estround, pseround=pseround, estnull=estnull, 
			psenull=psenull, char.width=char.width)

        if (esttype != "RATIO") {
          if (!is.null(unit.totest)) {
            ## GET TOTAL ESTIMATE TO COMPARE TO SUM OF ATTRIBUTE ESTIMATES
            totest <- sum(unit.totest[[estnm2]], na.rm=TRUE)

            ## GET SUM OF ESTIMATES TO COMPARE TO TOTAL
            sumest <- sum(unit.rowest[[estnm]])
            if (!(sumest < (totest + 100) & sumest > (totest - 100)))
              warning("the total estimate does not match sum of attributes.. difference of ", 
			abs(sumest-totest))  
          }
        }

        if (unitvar == "ONEUNIT" | length(unique(unit.rowest[[unitvar]])) == 1) {
 
          if (allin1) {
            estpsetab <- estpsecross
            names(estpsetab) <- c(title.rnames, title.yhatpse)
          } else {
            esttab <- estpsecross$est
            names(esttab) <- c(title.rnames, title.yhat)
            psetab <- estpsecross$pse
            names(psetab) <- c(title.rnames, title.yhat.pse)

            estpsetab <- data.table(esttab, psetab[[title.yhat.pse]])
            if (is.numeric(estnull)) estpsetab[[title.yhat]] <- 
				as.numeric(estpsetab[[title.yhat]])
            if (is.numeric(psenull)) estpsetab[[title.yhat.pse]] <- 
				as.numeric(estpsetab[[title.yhat.pse]])
            names(estpsetab) <- c(title.rnames, title.yhat, title.yhat.pse)
          }

          est2return <- setDF(estpsetab)
          pse2return <- NULL

          if ((savedata & addtitle) | returntitle)
            title.estpse <- paste0(title.estpse, title.ref)

            if (savedata)
              ## SAVE TO FILE
              suppressWarnings(
              save1tab(tab=estpsetab, tab.title=title.estpse, 
			outfn=outfn.estpse, outfolder=outfolder, allin1=allin1, 
			coltitlerow=FALSE, rowtotal=TRUE, addtitle=addtitle,
			outfn.date=outfn.date, overwrite=overwrite,  			
			charvars=title.rnames, cols2format=title.yhat))
        } else {  # > 1 unit

          if (allin1) {
            est2return <- estpsecross
            #names(est2return) <- c(title.rnames, title.yhatpse)
            pse2return <- NULL

            if (savedata)
              ## SAVE TO FILE
              suppressWarnings(
              FIESTA::save1tab(tab=estpsecross, tab.title=title.estpse,
 				outfn=outfn.estpse, outfolder=outfolder, allin1=TRUE, 
				coltitlerow=FALSE, coltitle=title.unitvar, rowtotal=TRUE, 
				addtitle=addtitle, outfn.date=outfn.date, overwrite=overwrite,
				charvars=title.rnames))
          } else {

            est2return <- estpsecross$est
            pse2return <- estpsecross$pse

            if ((savedata & addtitle) | returntitle) {
              title.est <- paste0(title.est, title.ref)
              title.pse <- paste0(title.pse, title.ref)
            }

            if (savedata)
            ## SAVE TO FILE
             suppressWarnings(
              FIESTA::save2tabs(tab1=est2return, tab2=pse2return, 
				tab1.title=title.est, tab2.title=title.pse, outfn.estpse=outfn.estpse,
 				outfolder=outfolder, coltitlerow=TRUE, coltitle=title.unitvar, 
				addtitle=addtitle, rowtotal=rowtotal, rnames=rnames, 
				outfn.date=outfn.date, overwrite=overwrite))
          }
        }
      }
    } 

  ### BY ROW AND COLUMN
  #######################################  
  } else {    ## colvar != "NONE"
    ## Get cross tables 
    ###############################################
    if (sumunits) {
      numunits <- length(units)
      tabs <- crossxbyunit(unit=NULL, grpest, rowest, colest, 
		totest, unitvar=unitvar, rowvar=rowvar, colvar=colvar, 
		estnm=estnmd, psenm=psenm, allin1, char.width, estnull, psenull,
		estround=estround, pseround=pseround,  
		rowgrp, rowgrpnm, title.rnames, numunits, savedata, addtitle, 
		returntitle, outfn.estpse, title.estpse, title.est, title.pse, 
		title.ref, outfolder, outfn.date, overwrite, esttype, phototype,
		rnames, title.colvar, title.unitvar)
      est2return <- tabs[[1]]
      if (length(tabs) == 2)
        pse2return <- tabs[[2]]

    } else {  ## colvar != "NONE" & sumunits == FALSE

      units <- unique(unit.grpest[[unitvar]])
      numunits <- length(units)
      tabs <- lapply(units, crossxbyunit, unit.grpest, unit.rowest, unit.colest, 
		unit.totest, unitvar=unitvar, rowvar=rowvar, colvar=colvar, 
		estnm=estnmd, psenm=psenm, allin1, char.width, estnull, psenull, 
		estround=estround, pseround=pseround,  
		rowgrp, rowgrpnm, title.rnames, numunits, savedata, addtitle, returntitle, 
		outfn.estpse, title.estpse, title.est, title.pse, title.ref,
		outfolder, outfn.date, overwrite, esttype, phototype,
		rnames, title.colvar, title.unitvar)
      names(tabs) <- units
      est2return <- rbindlist(lapply(tabs, `[[`, 1), use.names=TRUE, fill=TRUE)
      if (!allin1) {
        est2return[is.na(est2return)] <- estnull
      }
      if (unique(lapply(tabs, length)) == 2) {
        pse2return <- rbindlist(lapply(tabs, `[[`, 2), use.names=TRUE, fill=TRUE)
        if (!allin1) {
          pse2return[is.na(est2return)] <- psenull
        }
      }
    }
  }

  returnlst$tabest <- est2return
  titlelst$title.estpse <- title.estpse
  if (!is.null(pse2return)) {
    returnlst$tabpse <- pse2return
    titlelst$title.est <- title.est
    titlelst$title.pse <- title.pse
  }
  }

  ## Make rawtable
  if (rawdata) {
    rawdat.tabs <- {}
    if (is.null(rawdat)) rawdat <- list()

    ## Append totest to rawdat
    if (!is.null(unit.totest)) {
      if (esttype == "RATIO") {
        unit.totest <- FIESTA::getrhat(unit.totest)
      }
      ## Remove total column
      if ("TOTAL" %in% names(unit.totest)) {
        unit.totest[, TOTAL := NULL]
      }
      ## Split columns if unitvars exists
      if (!is.null(unitvars) && length(unitvars) > 1) {
        unit.totest[, (unitvars) := tstrsplit(get(unitvar), "-", fixed=TRUE)]
        unit.totest[, (unitvar) := NULL]
        setcolorder(unit.totest, c(unitvars, 
		names(unit.totest)[!names(unit.totest) %in% unitvars])) 
      } 
      rawdat$unit.totest <- setDF(unit.totest)
      rawdat.tabs <- c(rawdat.tabs, "unit.totest")

      if (sumunits) {
        rawdat$totest <- setDF(totest)
        rawdat.tabs <- c(rawdat.tabs, "totest")
      }
    }       
    if (!rowvar %in% c("NONE", "TOTAL")) {
      if (!is.null(unit.rowest)) {
        setorderv(unit.rowest, c(unitvar, rowvar))

        ## Split columns if unitvars exists
        if (!is.null(unitvars) && length(unitvars) > 1) {
          unit.rowest[, (unitvars) := tstrsplit(get(unitvar), "-", fixed=TRUE)]
          unit.rowest[, (unitvar) := NULL]
          setcolorder(unit.rowest, c(unitvars, 
			names(unit.rowest)[!names(unit.rowest) %in% unitvars])) 
        } 
        setnames(unit.rowest, rowvar, title.rowvar)
        rawdat$unit.rowest <- setDF(unit.rowest)
        rawdat.tabs <- c(rawdat.tabs, "unit.rowest")

        if (sumunits) {
          setnames(rowest, rowvar, title.rowvar)
          rawdat$rowest <- setDF(rowest)
          rawdat.tabs <- c(rawdat.tabs, "rowest")
        }  
      }     
    }
 
    if (colvar != "NONE") {
      if (!is.null(unit.colest)) {
        setorderv(unit.colest, c(unitvar, colvar))

        ## Split columns if unitvars exists
        if (!is.null(unitvars) && length(unitvars) > 1) {
          unit.colest[, (unitvars) := tstrsplit(get(unitvar), "-", fixed=TRUE)]
          unit.colest[, (unitvar) := NULL]
          setcolorder(unit.colest, c(unitvars, 
			names(unit.colest)[!names(unit.colest) %in% unitvars])) 
        } 
        setnames(unit.colest, colvar, title.colvar)
        rawdat$unit.colest <- setDF(unit.colest)
        rawdat.tabs <- c(rawdat.tabs, "unit.colest")

        if (sumunits) {
          setnames(colest, colvar, title.colvar)
          rawdat$colest <- setDF(colest)
          rawdat.tabs <- c(rawdat.tabs, "colest")
        }  
      }  
      if (!is.null(unit.grpest)) { 
        if ("NBRPLT.gt0" %in% names(unit.grpest)) { 
          unit.grpest <- unit.grpest[unit.grpest[["NBRPLT.gt0"]] > 0,] 
        }
        setorderv(unit.grpest, c(unitvar, rowvar, colvar))  

        ## Split columns if unitvars exists
        if (!is.null(unitvars) && length(unitvars) > 1) {
          unit.grpest[, (unitvars) := tstrsplit(get(unitvar), "-", fixed=TRUE)]
          unit.grpest[, (unitvar) := NULL]
          setcolorder(unit.grpest, c(unitvars, 
			names(unit.grpest)[!names(unit.grpest) %in% unitvars])) 
        } 
        setnames(unit.grpest, rowvar, title.rowvar)
        setnames(unit.grpest, colvar, title.colvar)
        rawdat$unit.grpest <- setDF(unit.grpest)
        rawdat.tabs <- c(rawdat.tabs, "unit.grpest")
      }
      if (sumunits) {
        setnames(grpest, rowvar, title.rowvar)
        setnames(grpest, colvar, title.colvar)
        rawdat$grpest <- setDF(grpest)
        rawdat.tabs <- c(rawdat.tabs, "grpest")
      }
    }

    if (CI) {
      rawdat[rawdat.tabs] <- lapply(rawdat[rawdat.tabs], FIESTA::addCI, estnm=estnm2)
    }
  }

  if (!is.null(title.rowvar)) titlelst$title.rowvar <- title.rowvar
  if (!is.null(title.colvar)) titlelst$title.colvar <- title.colvar
  if (!is.null(title.rowgrp)) titlelst$title.rowgrp <- title.rowgrp
  returnlst$titlelst <- titlelst
  
  if (rawdata) returnlst$rawdat <- rawdat

  return(returnlst)
}
