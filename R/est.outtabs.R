est.outtabs <- function(esttype, phototype="PCT", photoratio=FALSE, sumunits=FALSE,
	areavar, unitvar, unitvars=NULL, unit_totest, unit_rowest=NULL, unit_colest=NULL,
	unit_grpest=NULL, rowvar=NULL, colvar=NULL, uniquerow=NULL, uniquecol=NULL,
	rowgrp=FALSE, rowgrpnm=NULL, rowunit=NULL, totunit=NULL, allin1=FALSE,
	savedata=FALSE, addtitle=FALSE, returntitle=FALSE, title.ref=NULL,
	title.colvar=NULL, title.rowvar=NULL, title.rowgrp=NULL, title.unitvar=NULL,
	title.estpse=NULL, title.est=NULL, title.pse=NULL, outfn.estpse=NULL,
	outfolder=NULL, outfn.date=TRUE, overwrite=FALSE, estnm, psenm="pse",
	estround=0, pseround=2, estnull="--", psenull="--", divideby=NULL,
	coltitlerow=TRUE, rowtotal=TRUE, rawdata=FALSE, CI=TRUE, rawdat=NULL,
	char.width=NULL, rawonly=FALSE, raw.keep0=FALSE){

  ## Set global variables
  estn=pse=keepvars=TOTAL=totest=rowest=colest=grpest <- NULL
  rowvar <- ifelse(is.null(rowvar), "NONE", rowvar)
  colvar <- ifelse(is.null(colvar), "NONE", colvar)
  
  
  #####  TITLE INFO FOR TABLE COLUMNS
  ########################################################
  if (allin1) {
    title.yhatpse <- "Estimate (% Sampling Error)"
  } else {
    title.yhat <- "Estimate"
    title.yhat.pse <- "Percent Sampling Error"
  }
  
  ## Check if a total table exists
  if (!is.null(unit_totest)) gtotal <- TRUE
  
  if (!is.null(title.ref) && title.ref != "") {
    title.ref <- paste(";", title.ref)
  }
  
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
  if (!is.null(unit_totest)) {
    if (esttype == "RATIO") {
      unit_totest <- suppressWarnings(getrhat(unit_totest))
    } else {
      if (!is.null(dividebynum)) {
        unit_totest[[estnmd]] <- unit_totest[[estnm2]] / dividebynum
        unit_totest[[senmd]] <- unit_totest[[senm2]] / dividebynum
      } else {
        unit_totest[[estnmd]] <- unit_totest[[estnm2]]
      }
      unit_totest[[psenm]] <- unit_totest[[psenm]]
    }
    if (allin1) {
      char.width <- suppressWarnings(max(nchar(na.omit(round(unit_totest[[psenm]], pseround)))))
      if (!is.null(char.width) && char.width == -Inf) char.width <- 0
    }
  }
  
  if (!is.null(unit_rowest)) {
    if (esttype == "RATIO") {
      unit_rowest <- suppressWarnings(getrhat(unit_rowest))
    } else {
      if (!is.null(dividebynum)) {
        unit_rowest[[estnmd]] <- unit_rowest[[estnm2]] / dividebynum
        unit_rowest[[senmd]] <- unit_rowest[[senm2]] / dividebynum
      } else {
        unit_rowest[[estnmd]] <- unit_rowest[[estnm2]]
      }
      unit_rowest[[psenm]] <- unit_rowest[[psenm]]
    }
    if (allin1) {
      char.width <- max(char.width,
                        max(nchar(na.omit(round(unit_rowest[[psenm]], pseround)))))
    }
  }
  
  if (!is.null(unit_colest)) {
    if (esttype == "RATIO") {
      unit_colest <- suppressWarnings(getrhat(unit_colest))
    } else {
      if (!is.null(dividebynum)) {
        unit_colest[[estnmd]] <- unit_colest[[estnm2]] / dividebynum
        unit_colest[[senmd]] <- unit_colest[[senm2]] / dividebynum
      } else {
        unit_colest[[estnmd]] <- unit_colest[[estnm2]]
      }
      unit_colest[[psenm]] <- unit_colest[[psenm]]
    }
    if (allin1) {
      char.width <- max(char.width,
                        max(nchar(na.omit(round(unit_colest[[psenm]], pseround)))))
    }
  }
  
  if (!is.null(unit_grpest)) {
    if (esttype == "RATIO") {
      unit_grpest <- suppressWarnings(getrhat(unit_grpest))
    } else {
      if (!is.null(dividebynum)) {
        unit_grpest[[estnmd]] <- unit_grpest[[estnm2]] / dividebynum
        unit_grpest[[senmd]] <- unit_grpest[[senm2]] / dividebynum
      } else {
        unit_grpest[[estnmd]] <- unit_grpest[[estnm2]]
      }
      unit_grpest[[psenm]] <- unit_grpest[[psenm]]
    }
    if (allin1) {
      char.width <- max(char.width,
                        max(nchar(na.omit(round(unit_grpest[[psenm]], pseround)))))
    }
  }
  
  if (sumunits) {
    ## Group estimates
    #if (!is.null(uniquerow))
    #  keepvars.row <- names(uniquerow)[names(uniquerow) != rowvar]
    #if (!is.null(uniquecol))
    #  keepvars.col <- names(uniquecol)[names(uniquecol) != colvar]
	  keepvars.row=keepvars.col <- NULL

    ## GROUP TOTAL TABLE
    if (!is.null(unit_totest)) {
      totest <- 
        groupUnits(unit_totest, 
                   estncol = estnm, estncol.var = estnm.var,
		               domain = "TOTAL", esttype = esttype, 
		               rowgrpnm = rowgrpnm, 
		               unitvar = unitvar, areavar = areavar,
		               phototype = phototype, photoratio = photoratio)
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
    if (!is.null(unit_rowest)) {
      rowest <- 
        groupUnits(tabest = unit_rowest, domain = rowvar, 
                   estncol = estnm, estncol.var = estnm.var, 
                   esttype = esttype, rowgrpnm = rowgrpnm,
			             unitvar = unitvar,  
			             phototype = phototype, photoratio = photoratio, 
			             keepvars = keepvars.row)
      if (esttype != "RATIO" && !is.null(dividebynum)) {
        rowest[[estnmd]] <- rowest[[estnm2]] / dividebynum
        rowest[[senmd]] <- rowest[[senm2]] / dividebynum
        unit_rowest[[estnmd]] <- unit_rowest[[estnm2]] / dividebynum
        unit_rowest[[senmd]] <- unit_rowest[[senm2]] / dividebynum
      } else {
        rowest[[estnmd]] <- rowest[[estnm2]]
      }
      rowest[[psenm]] <- rowest[[psenm]]
      if (allin1) {
        char.width <- max(char.width,
		        max(nchar(na.omit(round(rowest[[psenm]], pseround)))))
      }
    }
	  if (!is.null(unit_colest)) {
      colest <- 
        groupUnits(tabest = unit_colest, domain = colvar, 
                   estncol = estnm, estncol.var = estnm.var, 
                   esttype = esttype, 
                   unitvar = unitvar,  
                   phototype = phototype, photoratio = photoratio,
			             keepvars = keepvars.col)
      if (esttype != "RATIO" && !is.null(dividebynum)) {
        colest[[estnmd]] <- colest[[estnm2]] / dividebynum
        colest[[senmd]] <- colest[[senm2]] / dividebynum
        unit_colest[[estnmd]] <- unit_colest[[estnm2]] / dividebynum
        unit_colest[[senmd]] <- unit_colest[[senm2]] / dividebynum
      } else {
        colest[[estnmd]] <- colest[[estnm2]]
      }
      colest[[psenm]] <- colest[[psenm]]
      if (allin1) {
        char.width <- max(char.width,
		       max(nchar(na.omit(round(colest[[psenm]], pseround)))))
      }
	  	
      if (!is.null(unit_grpest)) {
        grpest <- 
          groupUnits(tabest = unit_grpest, domain = rowvar, 
                   estncol = estnm, estncol.var = estnm.var, 
                   domvar2 = colvar, esttype = esttype,
			             rowgrpnm = rowgrpnm, 
			             unitvar = unitvar, 
			             phototype = phototype, photoratio = photoratio,
			             keepvars = c(keepvars.row, keepvars.col))
        if (esttype != "RATIO" && !is.null(dividebynum)) {
          grpest[[estnmd]] <- grpest[[estnm2]] / dividebynum
           grpest[[senmd]] <- grpest[[senm2]] / dividebynum
          unit_grpest[[estnmd]] <- unit_grpest[[estnm2]] / dividebynum
          unit_grpest[[senmd]] <- unit_grpest[[senm2]] / dividebynum
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
  }

  if (!rawonly) {
  ###################################################################
  ## GENERATE OUTPUT TABLES
  ###################################################################
  if (is.null(colvar) || colvar == "NONE") {
    if (sumunits) {
      if (rowvar == "TOTAL") {
        if (allin1) {
          estpse <- 
            data.frame(TOTAL = "Total", 
                       allin1f(round(totest[[estnmd]], estround),
			                 round(totest[[psenm]], pseround), 
			                 estnull = estnull, psenull = psenull,
			                 char.width = char.width), 
			                 stringsAsFactors = FALSE)
          setnames(estpse, c("TOTAL", title.yhatpse))
        } else {
          estpse <- 
            data.table(TOTAL = "Total",
			                 totest[, c(estnmd, psenm), with=FALSE], stringsAsFactors=FALSE)
                       estpse[[estnmd]] <- round(estpse[[estnmd]], estround)
                       estpse[[psenm]] <- round(estpse[[psenm]], pseround)
          names(estpse) <- c("TOTAL", title.yhat, title.yhat.pse)
        }
        rowtotal <- FALSE

      } else {  ##  rowvar != "TOTAL"
        
        ## Get estimates
        estcross <- 
          crosstabx(rowest, rowvar, estnmd, psenm, allin1=allin1,
		                estnull = estnull, psenull = psenull, 
		                char.width = char.width,
		                estround = estround, pseround = pseround)
        if (allin1) {
          tottab <- 
            data.frame(TOTAL = "Total", allin1f(totest[[estnmd]],
			                 totest[[psenm]], char.width=char.width, estnull=estnull,
			                 psenull=psenull, estround=estround, pseround=pseround), 
			                 stringsAsFactors=FALSE)
          setnames(tottab, c("TOTAL", title.yhatpse))

          estpse <- estcross
          estpse <- data.frame(names(estcross), estcross,
					stringsAsFactors=FALSE, row.names=NULL)
          if (rowgrp)
            estpse <- addrowgrp(estpse, uniquerow, rowvar, rowgrpnm)
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
            estpse <- addrowgrp(estpse, uniquerow, rowvar, rowgrpnm)
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
      if (!is.null(unit_totest)) {
        if (allin1) {
          ## Get table with estimates by unitvar
          estpsetot <- data.table(unit_totest[[unitvar]],
			            crosstabx(x = unit_totest, 
			                      xvar = "TOTAL",
			                      estnm = estnmd, psenm = psenm, allin1 = TRUE, 
			                      estnull = estnull, psenull = psenull, 
				                    char.width = char.width,
				                    estround = estround, pseround = pseround))
          setnames(estpsetot, c(title.unitvar, title.yhatpse))
        } else {
          ## GET TABLE OF ESTIMATES WITH TITLES
          estpsetot <- data.table(unit_totest[,c(unitvar, estnmd, psenm), with=FALSE])
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
            totunit <- data.frame(TOTAL = "Total", 
                                  allin1f(totunit[[estnmd]], 
                                          totunit[[psenm]],
			                            estnull = estnull, psenull = psenull, 
			                            char.width = char.width, 
			                            estround = estround,
			                            pseround = pseround), stringsAsFactors = FALSE)
            setnames(totunit, c(unitvar, title.yhatpse))
          } else {
            totunit <- c("TOTAL", totunit[, c(estnmd, psenm)])
            names(totunit) <- c(unitvar, title.yhat, title.yhat.pse)
          }
          if (!is.null(estpsetot))
            estpsetot <- data.frame(rbind(setDF(estpsetot), totunit))
        }

        ## TABLES TO RETURN
        est2return <- estpsetot
        pse2return <- NULL

        if ((savedata & addtitle) | returntitle)
          title.estpse <- paste0(title.estpse, title.ref)

        if (savedata)
          suppressWarnings(
          save1tab(tab=est2return, tab.title=title.estpse,
			             outfn=outfn.estpse, outfolder=outfolder, allin1=allin1,
			             coltitlerow=FALSE, rowtotal=FALSE, addtitle=addtitle,
			             outfn.date=outfn.date, overwrite=overwrite, cols2format=title.yhat))

      } else {  ## rowvar != "TOTAL"

        if (!is.null(rowunit)) {
          if (esttype == "RATIO") {
            rowunit <- suppressWarnings(getrhat(rowunit))
          } else {
            if (!is.null(divideby)) {
              rowunit[, (estnmd) := get(estnm2) / dividebynum]
              rowunit[, (senmd) := get(senm2) / dividebynum]
            }
            rowunit[, (estnmd) := round(get(estnmd), estround)]
            rowunit[, (psenm) := round(get(psenm), pseround)]
          }
        }
        if (!is.null(unit_totest) && !is.null(uniquecol)) {
          unit_totest <- add0unit(x=unit_totest, xvar=unitvar, uniquex=uniquecol,
			                            xvar.add0=TRUE)
        }

        ## Get table of estimates
        estpsecross <- crossxtab(group.est = unit_rowest, 
                                 rowvar.est = rowunit, colvar.est = unit_totest, 
                                 total.est = totunit, 
			                           rowvar = rowvar, colvar = unitvar, 
			                           estnm = estnmd, psenm = psenm, allin1 = allin1,
			                           rowgrp = rowgrp, rowgrpnm = rowgrpnm, 
			                           title.rnames = title.rnames,
			                           estround = estround, pseround = pseround, 
			                           estnull = estnull, psenull = psenull, 
			                           char.width = char.width)

        if (esttype != "RATIO") {
          if (!is.null(unit_totest)) {
            ## GET TOTAL ESTIMATE TO COMPARE TO SUM OF ATTRIBUTE ESTIMATES
            totest <- sum(unit_totest[[estnm2]], na.rm=TRUE)

            ## GET SUM OF ESTIMATES TO COMPARE TO TOTAL
            sumest <- sum(unit_rowest[[estnm]], na.rm=TRUE)
            if (!(sumest < (totest + 100) & sumest > (totest - 100)))
              warning("the total estimate does not match sum of attributes.. difference of ",
			                 abs(sumest-totest))
          }
        }

        if (unitvar == "ONEUNIT" || length(unique(unit_rowest[[unitvar]])) == 1) {

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
              save1tab(tab=estpsecross, tab.title=title.estpse,
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
              save2tabs(tab1=est2return, tab2=pse2return,
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
#      estunits <- unique(unit_grpest[[unitvar]])
#      numunits <- length(estunits)
      numunits <- 1
      tabs <- 
        crossxbyunit(unit = NULL, unit_grpest = grpest,
		                 unit_rowest = rowest, unit_colest = colest, 
		                 unit_totest = totest,
		                 unitvar = unitvar, 
		                 rowvar = rowvar, colvar = colvar,
		                 estnm = estnmd, psenm = psenm, 
		                 allin1 = allin1, char.width = char.width, 
		                 estnull = estnull, psenull = psenull,
		                 estround = estround, pseround = pseround,
		                 rowgrp = rowgrp, rowgrpnm = rowgrpnm, 
		                 title.rnames = title.rnames, 
		                 numunits = numunits, 
		                 savedata = savedata, addtitle = addtitle,
		                 returntitle = returntitle, 
		                 outfn.estpse = outfn.estpse, 
		                 title.estpse = title.estpse, 
		                 title.est = title.est, title.pse = title.pse,
		                 title.ref = title.ref, 
		                 outfolder = outfolder, outfn.date = outfn.date, 
		                 overwrite = overwrite, 
		                 esttype = esttype, phototype = phototype,
		                 rnames = rnames, 
		                 title.colvar = title.colvar, title.unitvar = title.unitvar)
      est2return <- tabs[[1]]
      if (length(tabs) == 2)
        pse2return <- tabs[[2]]

    } else {  ## colvar != "NONE" & sumunits == FALSE
      estunits <- unique(unit_grpest[[unitvar]])
      numunits <- length(estunits)
      tabs <- lapply(estunits, crossxbyunit, 
                     unit_grpest, unit_rowest, unit_colest,
		                 unit_totest, 
		                 unitvar = unitvar, 
		                 rowvar = rowvar, colvar = colvar,
		                 estnm = estnmd, psenm = psenm, 
		                 allin1 = allin1, char.width = char.width,
		                 estnull = estnull, psenull = psenull, 
		                 estround = estround, pseround = pseround,
		                 rowgrp = rowgrp, rowgrpnm = rowgrpnm, 
		                 title.rnames = title.rnames,
		                 numunits = numunits, 
		                 savedata = savedata, addtitle = addtitle,
		                 returntitle = returntitle, 
		                 outfn.estpse = outfn.estpse,
		                 title.estpse = title.estpse, 
		                 title.est = title.est, title.pse = title.pse,
		                 title.ref = title.ref, 
		                 outfolder = outfolder, outfn.date = outfn.date,
		                 overwrite = overwrite, 
		                 esttype = esttype, phototype = phototype,
		                 rnames = rnames, 
		                 title.colvar = title.colvar, title.unitvar = title.unitvar)
      names(tabs) <- estunits
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
    if (!is.null(unit_totest)) {
      if (esttype == "RATIO") {
        unit_totest <- getrhat(unit_totest)
      }
      ## Remove total column
      if ("TOTAL" %in% names(unit_totest)) {
        unit_totest[, TOTAL := NULL]
      }
      ## Split columns if unitvars exists
      if (!is.null(unitvars) && length(unitvars) > 1) {
        suppressWarnings(unit_totest[, (unitvars) := tstrsplit(get(unitvar), "-", fixed=TRUE)])
        unit_totest[, (unitvar) := NULL]
        setcolorder(unit_totest, c(unitvars,
		    names(unit_totest)[!names(unit_totest) %in% unitvars]))
      }
      rawdat$unit_totest <- setDF(unit_totest)
      rawdat.tabs <- c(rawdat.tabs, "unit_totest")

      if (sumunits) {
        rawdat$totest <- setDF(totest)
        rawdat.tabs <- c(rawdat.tabs, "totest")
      }
    }
    if (!rowvar %in% c("NONE", "TOTAL")) {
      if (!is.null(unit_rowest)) {

        ## Split columns if unitvars exists
        if (!is.null(unitvars) && length(unitvars) > 1) {
          suppressWarnings(unit_rowest[, (unitvars) := tstrsplit(get(unitvar), "-", fixed=TRUE)])
          unit_rowest[, (unitvar) := NULL]
          setcolorder(unit_rowest, c(unitvars,
			    names(unit_rowest)[!names(unit_rowest) %in% unitvars]))
        }

        ## Set order of table
        setorderv(unit_rowest, c(unitvars, rowvar))		
        setnames(unit_rowest, rowvar, title.rowvar)
        rawdat$unit_rowest <- setDF(unit_rowest)
        rawdat.tabs <- c(rawdat.tabs, "unit_rowest")

        if (sumunits) {
          setnames(rowest, rowvar, title.rowvar)
          rawdat$rowest <- setDF(rowest)
          rawdat.tabs <- c(rawdat.tabs, "rowest")
        }
      }
    }
    if (colvar != "NONE") {
      if (!is.null(unit_colest)) {

        ## Split columns if unitvars exists
        if (!is.null(unitvars) && length(unitvars) > 1) {
          suppressWarnings(unit_colest[, (unitvars) := tstrsplit(get(unitvar), "-", fixed=TRUE)])
          unit_colest[, (unitvar) := NULL]
          setcolorder(unit_colest, c(unitvars,
			    names(unit_colest)[!names(unit_colest) %in% unitvars]))
        }
        ## Set order of table
        setorderv(unit_colest, c(unitvars, colvar))

        setnames(unit_colest, colvar, title.colvar)
        rawdat$unit_colest <- setDF(unit_colest)
        rawdat.tabs <- c(rawdat.tabs, "unit_colest")

        if (sumunits) {
          setnames(colest, colvar, title.colvar)
          rawdat$colest <- setDF(colest)
          rawdat.tabs <- c(rawdat.tabs, "colest")
        }
      }

      if (!is.null(unit_grpest)) {
        if (!raw.keep0 && "NBRPLT.gt0" %in% names(unit_grpest)) {
          unit_grpest <- unit_grpest[unit_grpest[["NBRPLT.gt0"]] > 0,]
        }

        ## Split columns if unitvars exists
        if (!is.null(unitvars) && length(unitvars) > 1) {
          suppressWarnings(unit_grpest[, (unitvars) := tstrsplit(get(unitvar), "-", fixed=TRUE)])
          unit_grpest[, (unitvar) := NULL]
          setcolorder(unit_grpest, c(unitvars,
			    names(unit_grpest)[!names(unit_grpest) %in% unitvars]))
        }
        ## Set order of table
        setorderv(unit_grpest, c(unitvars, rowvar, colvar))

        setnames(unit_grpest, rowvar, title.rowvar)
        setnames(unit_grpest, colvar, title.colvar)
        rawdat$unit_grpest <- setDF(unit_grpest)
        rawdat.tabs <- c(rawdat.tabs, "unit_grpest")
      }
      if (sumunits) {
        setnames(grpest, rowvar, title.rowvar)
        setnames(grpest, colvar, title.colvar)
        rawdat$grpest <- setDF(grpest)
        rawdat.tabs <- c(rawdat.tabs, "grpest")
      }
    }
    if (CI) {
      rawdat[rawdat.tabs] <- lapply(rawdat[rawdat.tabs], addCI, estnm=estnm2)
    }
  }

  if (!is.null(title.rowvar)) titlelst$title.rowvar <- title.rowvar
  if (!is.null(title.colvar)) titlelst$title.colvar <- title.colvar
  if (!is.null(title.rowgrp)) titlelst$title.rowgrp <- title.rowgrp
  returnlst$titlelst <- titlelst

  if (rawdata) returnlst$rawdat <- rawdat

  return(returnlst)
}
