est.outtabs <- function(esttype, phototype="PCT", photoratio=FALSE, sumunits=FALSE, 
	areavar, unitvar, unitvar2=NULL, unit.totest, unit.rowest=NULL, unit.colest=NULL, 
	unit.grpest=NULL, rowvar=NULL, colvar=NULL, uniquerow=NULL, uniquecol=NULL, 
	rowgrp=FALSE, rowgrpnm=NULL, rowunit=NULL, totunit=NULL, allin1=FALSE, 
	savedata=FALSE, addtitle=FALSE, returntitle=FALSE, title.ref=NULL, 
	title.colvar=NULL, title.rowvar=NULL, title.rowgrp=NULL, title.unitvar=NULL, 
	title.estpse=NULL, title.est=NULL, title.pse=NULL, outfn.estpse=NULL, 
	outfolder=NULL, outfn.date=TRUE, overwrite=FALSE, estnm, psenm, 
	estround=0, pseround=2, estnull="--", psenull="--", divideby=NULL, 
	coltitlerow=TRUE, rowtotal=TRUE, rawdata=FALSE, CI=TRUE, rawdat=NULL,
	char.width=NULL){ 

  ## Set global variables
  estn=pse=keepvars=TOTAL <- NULL

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
  psenm <- "pse"

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

  ###################################################################
  ## GENERATE OUTPUT TABLES
  ###################################################################
  if (colvar == "NONE") {
    if (sumunits) {
      ## GROUP TOTAL TABLE
      if (!is.null(unit.totest)) {
        totest <- groupUnits(unit.totest, estncol=estnm, estncol.var=estnm.var,
		domain="TOTAL", esttype=esttype, rowgrpnm=rowgrpnm, unitvar=unitvar, 
		areavar=areavar, phototype=phototype, photoratio=photoratio)

        if (esttype != "RATIO" && !is.null(dividebynum)) {
          unit.totest[[estnmd]] <- unit.totest[[estnm]] / dividebynum
          totest[[estnmd]] <- totest[[estnm]] / dividebynum
          unit.totest[[senmd]] <- unit.totest[[senm]] / dividebynum
          totest[[senmd]] <- totest[[senm]] / dividebynum
        }
      } else {
        totest <- NULL
      }
      if (rowvar == "TOTAL") {

        if (allin1) {
          estpse <- data.table(TOTAL="Total", allin1f(totest[[estnmd]], totest[[psenm]],
			estnull=estnull, psenull=psenull, estround=estround, pseround=pseround,
			char.width=char.width), stringsAsFactors=FALSE)
          setnames(estpse, c("TOTAL", title.yhatpse))
        } else {
          estpse <- data.table(TOTAL="Total", totest[, c(estnmd, psenm), with=FALSE],
			stringsAsFactors=FALSE)
          estpse[, (estnmd) := round(get(estnmd), estround)]
          estpse[, (psenm) := round(get(psenm), pseround)]
          names(estpse) <- c("TOTAL", title.yhat, title.yhat.pse)
        }
        rowtotal <- FALSE

      } else {  ##  rowvar != "TOTAL"

        ## GROUP ROW TABLE
        if (!is.null(uniquerow)) 
          keepvars <- names(uniquerow)[names(uniquerow) != rowvar]
        rowest <- groupUnits(unit.rowest, estncol=estnm, estncol.var=estnm.var, 
				domain=rowvar, esttype=esttype, rowgrpnm=rowgrpnm,
				unitvar=unitvar, areavar=areavar, phototype=phototype, 
				photoratio=photoratio, keepvars=keepvars)

        if (esttype != "RATIO" && !is.null(dividebynum)) {
          unit.rowest[[estnmd]] <- unit.rowest[[estnm]] / dividebynum
          rowest[[estnmd]] <- rowest[[estnm]] / dividebynum
          unit.rowest[[senmd]] <- unit.rowest[[senm]] / dividebynum
          rowest[[senmd]] <- rowest[[senm]] / dividebynum
        }

        ## Get estimates
        if (is.null(char.width)) 
          char.width <- max(nchar(round(na.omit(rowest[["pse"]]), pseround)))
        estcross <- crosstabx(rowest, rowvar, estnmd, psenm, allin1=allin1,
		estnull=estnull, psenull=psenull, char.width=char.width,
		estround=estround, pseround=pseround)

        if (allin1) {
          tottab <- data.frame(TOTAL="Total", allin1f(totest[[estnmd]], totest[[psenm]],
			char.width=char.width, estnull=estnull, psenull=psenull, estround=estround,
 			pseround=pseround), stringsAsFactors=FALSE)
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

            tottab <- c(totals, round(totest[[estnmd]], estround), 
					round(totest[[psenm]], pseround))
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
            
    } else {  ## sumunits = FALSE

      ## GET INITIAL TABLE AND TOTALS
      estnmd <- ifelse(esttype == "RATIO", "rhat", estnmd)
      senmd <- ifelse(esttype == "RATIO", "rhat.se", senmd)
 
      if (esttype == "RATIO") {
        if (!is.null(unit.totest)) {
          unit.totest <- suppressWarnings(getrhat(unit.totest))
          if (allin1) 
            char.width <- max(nchar(round(na.omit(unit.totest[["pse"]]), pseround)))
        }    
        if (!is.null(unit.rowest)) {
          unit.rowest <- suppressWarnings(getrhat(unit.rowest))
          if (allin1) 
            char.width <- max(char.width, 
			max(nchar(round(na.omit(unit.rowest[["pse"]]), pseround))))
        }
      } else {
        ## ROUND VALUES FOR TOTAL TABLE
        if (!is.null(unit.totest) && !is.null(divideby)) {
          unit.totest[, (estnmd) := get(estnm) / dividebynum]          
          unit.totest[, (senmd) := get(senm) / dividebynum]          
        }
        if (!is.null(unit.rowest) && !is.null(divideby)) {
          unit.rowest[, (estnmd) := get(estnm) / dividebynum]
          unit.rowest[, (senmd) := get(senm) / dividebynum]
        }
      }
 
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
          setnames(estpsetot, c(title.unitvar, title.yhat, title.yhat.pse))
        }
      }

      if (!is.null(totunit)) {
        if (esttype == "RATIO") {
          totunit <- getrhat(totunit)
        } else {
          if (!is.null(divideby))
            totunit[, (estnmd) := get(estnm) / dividebynum]
            totunit[, (senmd) := get(senm) / dividebynum]

          totunit[, (estnmd) := round(get(estnmd), estround)]
          totunit[, (psenm) := round(get(psenm), pseround)]
        }
      }

      if (rowvar == "TOTAL") {

        if (!is.null(totunit)) {
          totunit <- setDF(totunit)
          if (allin1) {
            totunit <- data.frame(TOTAL="Total", allin1f(totunit[[estnmd]], totunit[[psenm]],
			estnull=estnull, psenull=psenull, estround=estround, pseround=pseround),
 			stringsAsFactors=FALSE)
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
            rowunit <- getrhat(rowunit)
          } else {
            if (!is.null(divideby))
              rowunit[, (estnmd) := get(estnm) / dividebynum]
              rowunit[, (senmd) := get(senm) / dividebynum]

            rowunit[, (estnmd) := round(get(estnmd), estround)]
            rowunit[, (psenm) := round(get(psenm), pseround)]
          }
        }

        if (!is.null(unit.totest) && !is.null(uniquecol))
          unit.totest <- add0unit(unit.totest, unitvar, uniquecol, "TOTAL", TRUE)
   

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
            totest <- sum(unit.totest[[estnm]])

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
			outfn.date=outfn.date, overwrite=overwrite, charvars=title.rnames,
			cols2format=title.yhat))
        } else {  # > 1 unit

          if (allin1) {
            est2return <- estpsecross
            #names(est2return) <- c(title.rnames, title.yhatpse)
            pse2return <- NULL

            if (savedata)
              ## SAVE TO FILE
              suppressWarnings(
              FIESTA::save1tab(tab=estpsecross, title.tab=title.estpse,
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
    if (sumunits) {
      colest=rowest=totest <- NULL

      ## Group estimates
      if (!is.null(uniquerow)) 
        keepvars.row <- names(uniquerow)[names(uniquerow) != rowvar]
      if (!is.null(uniquecol)) 
        keepvars.col <- names(uniquecol)[names(uniquecol) != colvar]
 
      if (!is.null(unit.totest)) {
        totest <- groupUnits(tabest=unit.totest, estncol=estnm, estncol.var=estnm.var, 
				domain="TOTAL", esttype=esttype, unitvar=unitvar, 
				areavar=areavar, phototype=phototype, photoratio=photoratio)
        if (!is.null(dividebynum)) {
          totest[[estnmd]] <- totest[[estnm]] / dividebynum
          unit.totest[[estnmd]] <- unit.totest[[estnm]] / dividebynum
          totest[[senmd]] <- totest[[senm]] / dividebynum
          unit.totest[[senmd]] <- unit.totest[[senm]] / dividebynum
        }
        if (allin1) 
          char.width <- max(nchar(round(na.omit(totest[["pse"]]), pseround)))
      }

      if (!is.null(unit.rowest)) {
        rowest <- groupUnits(tabest=unit.rowest, domain=rowvar, estncol=estnm,
				estncol.var=estnm.var, esttype=esttype, rowgrpnm=rowgrpnm,
				unitvar=unitvar, areavar=areavar, phototype=phototype, 
				photoratio=photoratio, keepvars=keepvars.row)
        if (!is.null(dividebynum)) {
          rowest[[estnmd]] <- rowest[[estnm]] / dividebynum
          unit.rowest[[estnmd]] <- unit.rowest[[estnm]] / dividebynum
          rowest[[senmd]] <- rowest[[senm]] / dividebynum
          unit.rowest[[senmd]] <- unit.rowest[[senm]] / dividebynum
        }
        if (allin1) 
          char.width <- max(char.width, 
			max(nchar(round(na.omit(rowest[["pse"]]), pseround))))
      }

      if (!is.null(unit.colest)) {
        colest <- groupUnits(tabest=unit.colest, domain=colvar, estncol=estnm,
				estncol.var=estnm.var, esttype=esttype, unitvar=unitvar, 
				areavar=areavar, phototype=phototype, photoratio=photoratio,
				keepvars=keepvars.col)
        if (!is.null(dividebynum)) {
          colest[[estnmd]] <- colest[[estnm]] / dividebynum
          unit.colest[[estnmd]] <- unit.colest[[senm]] / dividebynum
          colest[[senmd]] <- colest[[estnm]] / dividebynum
          unit.colest[[senmd]] <- unit.colest[[senm]] / dividebynum
        }
        if (allin1) 
          char.width <- max(char.width, 
			max(nchar(round(na.omit(colest[["pse"]]), pseround))))
      }

      if (!is.null(unit.grpest)) {
        grpest <- groupUnits(tabest=unit.grpest, domain=rowvar, estncol=estnm, 
			estncol.var=estnm.var, domvar2=colvar, esttype=esttype,
			rowgrpnm=rowgrpnm, unitvar=unitvar, areavar=areavar, 
			phototype=phototype, photoratio=photoratio, 
			keepvars=c(keepvars.row, keepvars.col))

        if (!is.null(dividebynum)) {
          grpest[[estnmd]] <- grpest[[estnm]] / dividebynum
          unit.grpest[[estnmd]] <- unit.grpest[[estnm]] / dividebynum
          grpest[[senmd]] <- grpest[[senm]] / dividebynum
          unit.grpest[[senmd]] <- unit.grpest[[senm]] / dividebynum
        }
        if (allin1) 
          char.width <- max(char.width, 
			max(nchar(round(na.omit(grpest[["pse"]]), pseround))))
      }

      ## Get estimate tables
      #########################################################
      tabs <- crossxtab(group.est=grpest, rowvar.est=rowest, colvar.est=colest, 
			total.est=totest, rowvar=rowvar, colvar=colvar, estnm=estnmd, 
			psenm=psenm, estround=estround, pseround=pseround, allin1=allin1, 
			rowgrp=rowgrp, rowgrpnm=rowgrpnm, title.rnames=title.rnames,
			estnull=estnull, psenull=psenull, char.width=char.width)
 
      if (allin1) {
        estpsetab <- tabs

        ## TABLES TO RETURN
        est2return <- estpsetab
        pse2return <- NULL

        if ((savedata & addtitle) | returntitle)
          title.estpse <- paste0(title.estpse, title.ref)

        ## SAVE TABLE
        if (savedata) 
          suppressWarnings(
          FIESTA::save1tab(tab=estpsetab, tab.title=title.estpse, 
			outfolder=outfolder, allin1=TRUE, outfn=outfn.estpse, 
			coltitlerow=TRUE, coltitle=title.colvar, addtitle=addtitle, 
			rnames=rnames, outfn.date=outfn.date, overwrite=overwrite) )
      } else {
        esttab <- tabs$est
        psetab <- tabs$pse
        
        est2return <- esttab
        pse2return <- psetab

        if ((savedata & addtitle) | returntitle) {
          title.est <- paste0(title.est, title.ref)
          title.pse <- paste0(title.pse, title.ref)
        }
 
        ## SAVE TABLES
        if (savedata) 
          suppressWarnings(
          FIESTA::save2tabs(tab1=esttab, tab2=psetab, tab1.title=title.est, 
			tab2.title=title.pse, coltitle=title.colvar, outfolder=outfolder,
 			outfn.estpse=outfn.estpse, addtitle=addtitle, rnames=rnames,
			outfn.date=outfn.date, overwrite=overwrite) )
      } 
     
    } else {  ## colvar != "NONE" & sumunits == FALSE
      units <- unique(unit.grpest[[unitvar]])

      ## Get rhat values
      ####################################################
      if (esttype == "RATIO") {
        if (!is.null(unit.colest))
          unit.colest <- suppressWarnings(FIESTA::getrhat(unit.colest))
        if (!is.null(unit.rowest))
          unit.rowest <- suppressWarnings(FIESTA::getrhat(unit.rowest))
        if (!is.null(unit.totest))
          unit.totest <- suppressWarnings(FIESTA::getrhat(unit.totest))
        if (!is.null(unit.grpest))
          unit.grpest <- suppressWarnings(FIESTA::getrhat(unit.grpest))
      } else {
        if (!is.null(dividebynum)) {
          if (!is.null(unit.colest))
            unit.colest[[estnmd]] <- unit.colest[[estnm]] / dividebynum
            unit.colest[[senmd]] <- unit.colest[[senm]] / dividebynum
          if (!is.null(unit.rowest))
            unit.rowest[[estnmd]] <- unit.rowest[[estnm]] / dividebynum
            unit.rowest[[senmd]] <- unit.rowest[[senm]] / dividebynum
          if (!is.null(unit.totest))
            unit.totest[[estnmd]] <- unit.totest[[estnm]] / dividebynum
            unit.totest[[senmd]] <- unit.totest[[senm]] / dividebynum
          if (!is.null(unit.grpest))
            unit.grpest[[estnmd]] <- unit.grpest[[estnm]] / dividebynum
            unit.grpest[[senmd]] <- unit.grpest[[senm]] / dividebynum
        }
      }

      est2return <- {}
      pse2return <- {}
      numunits <- length(units)

      for (unit in units) {
        ## GENERATE TABLE OF ESTIMATES
        if (!is.null(unit.totest)) {
          total.est <- unit.totest[get(unitvar) == unit,]
          if (allin1) 
            char.width <- max(nchar(round(na.omit(total.est[["pse"]]), pseround)))
        } else {
          total.est <- NULL
        }
        if (!is.null(unit.rowest)) {
          rowvar.est <- unit.rowest[get(unitvar) == unit,]
          if (allin1) 
            char.width <- max(char.width, 
			max(nchar(round(na.omit(rowvar.est[["pse"]]), pseround))))
        } else {
          rowvar.est <- NULL
          rowtotal <- FALSE
        }
        if (!is.null(unit.colest)) {
          colvar.est <- unit.colest[get(unitvar) == unit,]
          if (allin1) 
            char.width <- max(char.width, 
			max(nchar(round(na.omit(colvar.est[["pse"]]), pseround))))
        } else {
          colvar.est <- NULL
          coltitlerow <- FALSE
        }
        if (!is.null(unit.grpest)) 
          group.est <- unit.grpest[get(unitvar) == unit,]

        ## Get estimate tables
        #########################################################
        tabs <- crossxtab(group.est=group.est, rowvar.est=rowvar.est, 
			colvar.est=colvar.est, total.est=total.est, rowvar=rowvar, 
			colvar=colvar, estnm=estnmd, psenm=psenm, estround=estround, 
			pseround=pseround, allin1=allin1, rowgrp=rowgrp, rowgrpnm=rowgrpnm,
 			title.rnames=title.rnames, estnull=estnull, psenull=psenull,
			char.width=char.width)
 
        if (allin1) {
          estpsetab <- tabs
        
          if (!is.null(estpsetab)) {

            ## TABLES TO RETURN
            if (numunits > 1) {
              est2return <- rbind(est2return, cbind(unit=unit, estpsetab))
            }   else {
              est2return <- rbind(est2return, estpsetab)
            }
            pse2return <- NULL

            if ((savedata & addtitle) | returntitle) {
         
              ## TABLE TITLES
              if (numunits > 1) {
                returntitle <- FALSE
                outfn.estpse.unit <- paste(outfn.estpse, unit, sep="_")
                title.estpse.unit <- paste0(title.estpse, ": ", unit, title.ref)
              } else {
                title.estpse <- paste0(title.estpse, title.ref)
                title.estpse.unit <- title.estpse
                outfn.estpse.unit <- outfn.estpse
              }

              if (savedata) { 
                ## SAVE TABLE
                suppressWarnings(
                FIESTA::save1tab(tab=estpsetab, tab.title=title.estpse.unit,
 				outfolder=outfolder, allin1=TRUE, coltitlerow=FALSE,
 				rnames=rnames, outfn=outfn.estpse.unit, addtitle=addtitle,
				outfn.date=outfn.date, overwrite=overwrite))
              }
            }
          }
        } else { # not allin1
          esttab <- tabs$est
          psetab <- tabs$pse

          ## RETURN TABLES AND WRITE TO FILE
          if (is.null(esttab)) {
            warning(paste("There are no estimates for unit", unit))
          } else {
            cols <- levels(group.est[[colvar]])

            ## TABLES TO RETURN
            if (length(cols) == 1) {
              estpsetab <- data.frame(cbind(esttab[,title.rnames], esttab[,cols], 
        		psetab[,cols]), stringsAsFactors=FALSE)
              names(estpsetab) <- c(rnames, title.yhat, title.yhat.pse)
                
              if (numunits > 1)
                estpsetab <- cbind(unit=unit, estpsetab)
              
              estpsetab[,title.yhat] <- as.numeric(estpsetab[,title.yhat])
              estpsetab[,title.yhat.pse] <- as.numeric(estpsetab[,title.yhat.pse])
              est2return <- rbind(est2return, estpsetab)
              pse2return <- NULL

              if((savedata & addtitle) | returntitle){
                ## TABLE TITLES
                if (numunits > 1) {
                  returntitle <- FALSE
                  outfn.estpse.unit <- paste(outfn.estpse, unit, sep="_")
                  title.estpse.unit <- paste0(title.estpse, ": ", unit, title.ref)
                } else {
                  title.estpse <- paste0(title.estpse, title.ref)
                  title.estpse.unit <- title.estpse
                  outfn.estpse.unit <- outfn.estpse
                }

                if (savedata) {
                  ## SAVE TABLE
                  suppressWarnings(
                  FIESTA::save1tab(tab=estpsetab, tab.title=title.estpse.unit,
 				outfolder=outfolder, allin1=FALSE, outfn=outfn.estpse.unit,
 				coltitlerow=TRUE, coltitle=title.colvar, addtitle=addtitle, 
				rnames=rnames, outfn.date=outfn.date, overwrite=overwrite,
				cols2format=title.yhat)
                  )
                }
              }    
            } else {

              if (numunits > 1) {
                est2return <- rbind(est2return, cbind(unit=unit, esttab))
                pse2return <- rbind(pse2return, cbind(unit=unit, psetab))
              } else {
                est2return <- rbind(est2return, esttab)  
                pse2return <- rbind(pse2return, psetab)
              }

              if ((savedata & addtitle) | returntitle) {

                ## TABLE TITLES
                if (numunits > 1) {
                  returntitle <- FALSE
                  outfn.estpse.unit <- paste(outfn.estpse, unit, sep="_")
                  title.est.unit <- paste0(title.est, ": ", unit, title.ref)
                  title.pse.unit <- paste0(title.pse, ": ", unit, title.ref)
                } else {
                  outfn.estpse.unit <- outfn.estpse
                  title.est <- paste0(title.est, title.ref)
                  title.pse <- paste0(title.pse, title.ref)
                  title.est.unit <- title.est
                  title.pse.unit <- title.pse
                }

                if(savedata){
                  if (esttype == "PHOTO" && phototype == "PCT") {
                    suppressWarnings(
                    FIESTA::save2tabs(tab1=esttab, tab2=psetab, tab1.title=title.est.unit, 
					tab2.title=title.pse.unit, outfolder=outfolder, coltitlerow=TRUE,
 					coltitle=title.colvar, rnames=rnames, outfn.estpse=outfn.estpse.unit, 
					addtitle=addtitle, rowtotal=FALSE, outfn.date=outfn.date,
 					overwrite=overwrite))
                  } else {
                    suppressWarnings(
                    FIESTA::save2tabs(tab1=esttab, tab2=psetab, tab1.title=title.est.unit, 
					tab2.title=title.pse.unit, outfolder=outfolder, coltitlerow=TRUE,
 					coltitle=title.colvar, rnames=rnames, outfn.estpse=outfn.estpse.unit, 
					addtitle=addtitle, rowtotal=FALSE, outfn.date=outfn.date,
 					overwrite=overwrite)
                    )
                  }
                }
              }
            }
          }
        }
      }
      names(est2return)[names(est2return) == "unit"] <- title.unitvar
      if (!is.null(pse2return))
        names(pse2return)[names(pse2return) == "unit"] <- title.unitvar
    }
  }
 
  ## Make rawtable
  if (rawdata) {
    rawdat.tabs <- {}
    if (is.null(rawdat)) rawdat <- list()

    ## Append totest to rawdat
    if (!is.null(unit.totest)) {
      if (esttype == "RATIO") 
        unit.totest <- FIESTA::getrhat(unit.totest)

      ## Split columns if unitvar2 exists
      unit.totest[, TOTAL := NULL]
      if (!is.null(unitvar2)) {
        unit.totest[, c(unitvar2, unitvar) := tstrsplit(get(unitvar), "-", fixed=TRUE)]
        setcolorder(unit.totest, c(unitvar2, unitvar, 
		names(unit.totest)[!names(unit.totest) %in% c(unitvar2, unitvar)])) 
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
        if (esttype == "RATIO")
          unit.rowest <- FIESTA::getrhat(unit.rowest)
        setorderv(unit.rowest, c(unitvar, rowvar))

        ## Split columns if unitvar2 exists
        if (!is.null(unitvar2)) {
          unit.rowest[, c(unitvar2, unitvar) := tstrsplit(get(unitvar), "-", fixed=TRUE)]
          setcolorder(unit.rowest, c(unitvar2, unitvar, 
			names(unit.rowest)[!names(unit.rowest) %in% c(unitvar2, unitvar)])) 
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
        if (esttype == "RATIO")
          unit.colest <- FIESTA::getrhat(unit.colest)
        setorderv(unit.colest, c(unitvar, colvar))

        ## Split columns if unitvar2 exists
        if (!is.null(unitvar2)) {
          unit.colest[, c(unitvar2, unitvar) := tstrsplit(get(unitvar), "-", fixed=TRUE)]
          setcolorder(unit.colest, c(unitvar2, unitvar, 
			names(unit.colest)[!names(unit.colest) %in% c(unitvar2, unitvar)])) 
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
 
      if (esttype == "RATIO")
        unit.grpest <- FIESTA::getrhat(unit.grpest)
      setorderv(unit.grpest, c(unitvar, rowvar, colvar))
     
      setnames(unit.grpest, rowvar, title.rowvar)
      setnames(unit.grpest, colvar, title.colvar)
      rawdat$unit.grpest <- setDF(unit.grpest)
      rawdat.tabs <- c(rawdat.tabs, "unit.grpest")

      if (sumunits) {
        setnames(grpest, rowvar, title.rowvar)
        setnames(grpest, colvar, title.colvar)
        rawdat$grpest <- setDF(grpest)
        rawdat.tabs <- c(rawdat.tabs, "grpest")
      }
    }
    if (CI)
      rawdat[rawdat.tabs] <- lapply(rawdat[rawdat.tabs], FIESTA::addCI, estnm=estnm)
  }

  returnlst <- list(tabest=est2return, tabpse=pse2return)
  if (!is.null(pse2return)) {
    titlelst <- list(title.est=title.est, title.pse=title.pse)
  } else {
    titlelst <- list(title.est=title.estpse)
  }
  if (!is.null(title.rowvar)) titlelst$title.rowvar <- title.rowvar
  if (!is.null(title.colvar)) titlelst$title.colvar <- title.colvar
  if (!is.null(title.rowgrp)) titlelst$title.rowgrp <- title.rowgrp
  returnlst$titlelst <- titlelst
  
  if (rawdata) returnlst$rawdat <- rawdat

  return(returnlst)
}
