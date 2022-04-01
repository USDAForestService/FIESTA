check.titles <- function(dat=NULL, esttype, estseed="none", phototype=NULL, Npts=NULL,
	ratiotype="PERACRE", tabtype="PCT", sumunits=FALSE, title.main=NULL,
	title.pre=NULL, title.ref=NULL, title.rowvar=NULL, title.rowgrp=NULL,
	title.colvar=NULL, title.unitvar=NULL, title.filter=NULL, title.unitsn=NULL,
 	title.unitsd=NULL, title.estvarn=NULL, title.estvard=NULL, unitvar,
	unitvar2=NULL, rowvar=NULL, colvar=NULL, estvarn=NULL, estvarn.filter=NULL,
	estvard=NULL, estvard.filter=NULL, addtitle=FALSE, returntitle=TRUE,
	rawdata=FALSE, parameters=TRUE, states=NULL, invyrs=NULL, landarea=NULL,
	pcfilter=NULL, allin1=FALSE, divideby=NULL, outfn=NULL, outfn.pre=NULL){

  ## TITLE INFO FOR OUTPUT TABLES
  ########################################################
  if (!is.null(unitvar2) && unitvar2 == "NONE") unitvar2 <- NULL

  ## title.unitvar
  #if (is.null(title.unitvar) && unitvar != "ONEUNIT") title.unitvar <- unitvar
  if (is.null(title.unitvar)) title.unitvar <- unitvar

  ## Initialize variables
  title.est=title.pse=title.estpse=title.row=title.col=outfn.estpse=
	 title.part2.row=title.part2.col=title.estvar=title.yvar=title.yvard <- NULL

  ## Title for landarea
  title.landarea <- ifelse (landarea == "FOREST", "forest land",
	ifelse (landarea == "ALL", "all lands",
		ifelse (landarea == "TIMBERLAND", "timberland",
  			ifelse (landarea == "CHANGE" & esttype == "LULC", "land changed",
				ifelse (landarea == "CHANGE", "land with observed change", "")))))
  landarea2 <- ifelse (landarea == "FOREST", "forestland",
	ifelse (landarea == "ALL", "allland",
		ifelse (landarea == "TIMBERLAND", "timberland",
			ifelse (landarea == "CHANGE", "change", ""))))
  if (addtitle || returntitle) {
    if (!is.null(dat)) {
      title.unitvar.out <- NULL
      if (!sumunits && length(unique(dat[[unitvar]])) > 1) {
        title.unitvar.out <- paste("by", title.unitvar)
      }
      if (!is.null(title.unitvar.out) && !is.null(unitvar2)) {
        title.unitvar.out <- paste(title.unitvar.out, "and", unitvar2)
      }
    } else {
      title.unitvar.out <- title.unitvar
    }

    #####  TITLE INFO FOR OUTPUT TABLES
    ########################################################
    ## Reference title
    if (is.null(title.ref)) {
      if (!is.null(states))
        title.state <- paste(as.character(states), collapse=" and ")
      if (!is.null(invyrs)) {
        if (is.null(states) && !is.null(names(invyrs)))
          title.state <- paste(as.character(states), collapse=" and ")
        invyrs <- as.numeric(as.character(unlist(invyrs)))
        if (length(unique(invyrs)) > 1) {
          title.ref <- paste0(title.state, ", ", min(invyrs), "-", max(invyrs))
        } else {
          title.ref <- paste0(title.state, ", ", invyrs)
        }
      }
      if (is.null(title.ref)) title.ref <- ""
    }

    ## GENERATE TABLE TITLES AND outfn
    ###########################################################################
    if (is.null(title.main) || title.main == "") {
      ## Title for filters
      if (is.null(title.filter)) {
        if (!is.null(pcfilter)) {
          title.filter <- paste0("(", pcfilter, ")")
        }
      } else if (title.filter == "") {
        title.filter <- NULL
      } else {
        title.filter <- paste0("(", title.filter, ")")
      }

      ## title.part1
      ####################################################################
      title.divideby <- " "
      if (esttype == "PHOTO") {
        title.part1 <- ifelse(tabtype == "PCT", "Estimated percent",
		paste0("Estimated area, in ", title.unitsn, ", of"))
        title.units <- NULL

      } else if (esttype %in% c("AREA", "LULC", "P2VEG")) {
        if (is.null(title.estvarn)) {
          title.estvarn <- "Area"
        }
        title.part1 <- title.estvarn
        title.landarea <- paste("on", title.landarea)
        if (is.null(title.unitsn)) {
          title.unitsn <- "acres"
        }
      } else if (esttype %in% c("TREE", "RATIO")) {
        #ref_estvar <- FIESTAutils::ref_estvar
        if (is.null(title.unitsn)) {
          title.unitsn <- unique(ref_estvar[ref_estvar$ESTVAR == estvarn, "ESTUNITS"])
        }
        if (length(title.unitsn) > 1) {
          title.units <- title.unitsn[1]
        }
        if (esttype == "RATIO") {
          if (is.null(title.unitsd)) {
            title.unitsd <- unique(ref_estvar[ref_estvar$ESTVAR == estvard, "ESTUNITS"])
          }
          if (length(title.unitsd) > 1) {
            title.unitsd <- title.unitsd[1]
          }
        }

        if (is.null(title.estvarn)) {
          ref_estvarn <- ref_estvar[ref_estvar$ESTVAR == estvarn, ]
          if (nrow(ref_estvarn) == 0) {
            title.estvarn <- estvarn
          } else if (estseed %in% c("add", "only")) {
            ref_estvarn <- ref_estvarn[grep("seedlings", ref_estvarn$ESTTITLE),]
            if (estseed == "only") {
              gfind.max <- grep("seedlings", ref_estvarn$ESTUNITS)
              title.unitsn <- "seedlings"
            } else {
              gfind.max <- grep("trees", ref_estvarn$ESTUNITS)
            }
            title.estvarn <- ref_estvarn[as.numeric(gfind.max), "ESTTITLE"]
          } else {
            if (!is.null(estvarn.filter)) {
              gfind.max <- grep(estvarn.filter, ref_estvarn$ESTFILTER)
              ## If the filter is not found in ref_estvar, try splitting
              if (length(gfind.max) == 0) {
                estfilters <- strsplit(estvarn.filter, "&")[[1]]
                ## Find matching filter in ref_estvar. If more than 1, uses first.
                gfind <- sapply(estfilters, function(x, ref_estvarn)
        			grep(gsub("\\s", "", x), gsub("\\s", "", ref_estvarn$ESTFILTER)),
				ref_estvarn)
                if (length(gfind) > 1 && any(lapply(gfind, length) > 0)) {
                  gfind <- gfind[1]
                }
                gfind <- table(gfind)
                if (length(gfind) > 0) {
                  gfind.max <- names(gfind)[max(gfind)]
                  if (length(gfind.max) > 1) {
                    gfind.max <- gfind.max[1]
                  }
                } else {
                  gfind.max <- 1
                }
              } else {
                 gfind.max <- gfind.max[1]
              }
            } else {
              gfind.max <- 1
            }
            title.estvarn <- ref_estvarn[as.numeric(gfind.max), "ESTTITLE"]
          }
          ## Get title.yvar
          title.yvar <- ref_estvar[ref_estvar$ESTTITLE == title.estvarn, "ESTTITLE1"]
          filternm <- ref_estvar[ref_estvar$ESTTITLE == title.estvarn, "FILTERNM"][1]
          if (is.null(title.filter)) {
            if (!is.null(filternm) && !is.na(filternm) && filternm != "") {
              title.filter <- filternm
            }
          }
          if (!is.null(title.unitsn)) {
            title.yvar <- paste0(title.yvar, ", in ", title.unitsn[1])
          }
          if (esttype == "RATIO" && ratiotype == "PERACRE") {
            title.estvarn <- paste(title.estvarn, "per acre")
          }
        }
        title.part1 <- title.estvarn

        if (esttype == "RATIO" && ratiotype == "PERTREE") {
          if (is.null(title.estvard)) {
            ref_estvard <- ref_estvar[ref_estvar$ESTVAR == estvarn, ]

            if (nrow(ref_estvard) == 0) {
              title.estvard <- estvard
            } else {
              if (!is.null(estvard.filter)) {
                gfind.max <- grep(estvarn.filter, ref_estvarn$ESTFILTER)
                if (length(gfind.max) == 0) {

                  estfilters <- strsplit(estvard.filter, "&")[[1]]

                  ## Find matching filter in ref_estvar. If more than 1, uses first.
                  gfind <- sapply(estfilters, function(x, ref_estvard)
        			grep(gsub("\\s", "", x), gsub("\\s", "", ref_estvard$ESTFILTER)),
				ref_estvard)
                  gfind <- table(gfind)
                  gfind.max <- names(gfind)[max(gfind)]
                  if (length(gfind.max) > 1) gfind.max <- gfind.max[1]
                } else {
                  gfind.max <- 1
                }
              } else {
                gfind.max <- 1
              }
              title.estvard <- ref_estvard[gfind.max, "ESTTITLE"]
            }
          }
          title.part1 <- paste(title.part1, "by", tolower(title.estvard))
          title.yvard <- ref_estvar[ref_estvar$ESTTITLE == title.estvard, "ESTTITLE1"]
          if (!is.null(title.unitsd)) {
            title.yvard <- paste0(title.yvard, ", in ", title.unitsd)
          }
        } else {
          title.yvard <- "Acres"
        }
        title.landarea <- paste("on", title.landarea)
      }

      if (!is.null(title.unitsn) && length(title.unitsn) > 0) {
        title.unitsn2 <- ifelse (is.null(divideby), paste0(", in ", title.unitsn),
		paste0(", in ", divideby, " ", title.unitsn))
        title.part1 <- paste0(title.part1, title.unitsn2)
        title.unitsn <- title.unitsn[1]
      }

      ## title.part2
      ####################################################################
      title.rowgrp2 <- if(!is.null(title.rowgrp)) paste(" and", title.rowgrp)
      if (!is.null(rowvar) && rowvar == "TOTAL") {
        title.part2 <- ""
      } else {
        title.part2 <- tolower(paste0("by ", title.rowvar, title.rowgrp2))
        title.part2.row <- tolower(paste0("by ", title.rowvar, title.rowgrp2))
      }
      if (colvar != "NONE") {
        if (esttype == "PHOTO" && length(grep("nratio", phototype)) == 0) {
          title.part2.col <- tolower(paste0("by ", title.colvar))
          title.part2 <- paste("of", title.colvar, "within", title.rowvar)
        } else {
          title.part2.col <- tolower(paste0("by ", title.colvar, title.rowgrp2))
          title.part2 <- tolower(paste(title.part2, "and", title.colvar))
        }
      }

      ## Add percent sampling error
      if (allin1) {
        title.error <- ifelse(is.null(title.unitsn), "(percent sampling error)",
		"(percent sampling error),")
        if (rowvar == "TOTAL") {
          title.estpse <- paste(title.part1, title.error, title.landarea)
          title.tot <- paste(title.part1, title.error, title.landarea)
        } else {
          title.estpse <- paste(title.part1, title.error, title.part2,
			title.landarea)
          title.row <- paste(title.part1, title.error, title.part2, title.landarea)
        }
        if (colvar != "NONE") {
          title.row <- paste(title.part1, title.error, title.part2.row, title.landarea)
          title.col <- paste(title.part1, title.error, title.part2.col, title.landarea)
        }
        if (!is.null(title.filter))
          title.estpse <- paste(title.estpse, title.filter)
        if (!is.null(title.unitvar.out))
          title.estpse <- paste(title.estpse, title.unitvar.out)

      } else {
        if (!is.null(title.unitsn)) {
           title.part1 <- paste0(title.part1, ",")
        }

        if (rowvar == "TOTAL" ||
		(colvar == "NONE" && sumunits) ||
 		(colvar == "NONE" && is.null(dat)) ||
		(colvar == "NONE" && length(unique(dat[[unitvar]])) == 1)) {

          if (rowvar == "TOTAL") {
            title.estpse <- paste(title.part1, "and percent sampling error",
			 title.landarea)
          title.tot <- paste(title.part1, title.landarea)
          } else {
            title.estpse <- paste(title.part1, "and percent sampling error",
			title.landarea, title.part2)
            title.row <- paste(title.part1, title.landarea, title.part2.row)
          }
          if (!is.null(title.filter)) {
            title.estpse <- paste(title.estpse, title.filter)
          }
          if (!is.null(title.unitvar.out)) {
            title.estpse <- paste(title.estpse, title.unitvar.out)
          }
        } else {

          title.est <- paste(title.part1, title.landarea, title.part2)
          title.pse <- paste("Percent sampling error of", tolower(title.part1),
			title.landarea, title.part2)
          title.row <- paste(title.part1, title.landarea, title.part2.row)
          title.col <- paste(title.part1, title.landarea, title.part2.col)

          if (!is.null(title.filter)) {
            title.est <- paste(title.est, title.filter)
            title.pse <- paste(title.pse, title.filter)
          }
          if (!is.null(title.unitvar.out)) {
            title.pse <- paste(title.est, title.unitvar.out)
            title.est <- paste(title.est, title.unitvar.out)
          }
        }
      }

      if (!is.null(title.filter)) {
        if (rowvar == "TOTAL") {
          title.tot <- paste(title.tot, title.filter)
        } else {
          title.row <- paste(title.row, title.filter)
        }
        if (colvar != "NONE") {
          title.col <- paste(title.col, title.filter)
        }
      }
      if (!is.null(title.unitvar.out)) {
        if (rowvar != "TOTAL")
          title.row <- paste(title.row, title.unitvar.out)
        if (colvar != "NONE") {
          title.row <- paste(title.row, title.unitvar.out)
          title.col <- paste(title.col, title.unitvar.out)
        }
      }
      if (!is.null(title.ref) && title.ref != "") {
        if (!is.null(title.estpse)) {
          #title.estpse <- paste0(title.estpse, "; ", title.ref)
          title.estpse <- paste0(title.estpse)
        } else {
          #title.est <- paste0(title.est, "; ", title.ref)
          #title.pse <- paste0(title.pse, "; ", title.ref)
          title.est <- paste0(title.est)
          title.pse <- paste0(title.pse)
        }
        if (rowvar == "TOTAL") {
          title.tot <- paste0(title.tot, "; ", title.ref)
        } else {
          title.row <- paste0(title.row, "; ", title.ref)
        }
        if (colvar != "NONE") {
          title.col <- paste0(title.col, "; ", title.ref)
        }
      }
    } else {  ## title.main != NULL
      title.estpse <- title.main
      title.est <- title.main
      title.pse <- title.main
    }
  }

  ## GENERATE outfn name
  ###########################################################################
  if (is.null(outfn) || gsub(" ", "", outfn) == "") {
    pretitle <- tolower(esttype)
    if (esttype == "PHOTO") {
      if (!is.null(phototype)) {
        pretitle <- paste(pretitle, phototype, sep="_")
        if (!is.null(Npts))
          pretitle <- paste0(pretitle, Npts)
      } else {
        warning("no phototype parameter")
      }
    }

    if (!is.null(outfn.pre)) {
      pretitle <- paste(outfn.pre, pretitle, sep="_")
    }

    if (esttype %in% c("TREE", "RATIO")) {
      pretitle <- paste0(pretitle, "_", estvarn)

      if (esttype == "RATIO") {
        if (is.null(estvard)) estvard <- "ACRE"
        pretitle <- paste0(pretitle, "_", estvard)
      }
      if (!is.null(title.filter)) {
        pretitle <- paste0(pretitle, "_", sub(" ", "", title.filter))
      }
    }

    if (rowvar == "TOTAL") {
      outfn.estpse <- paste0(pretitle, "_", landarea2, unitvar2)
    } else if (colvar == "NONE") {
      outfn.estpse <- paste0(pretitle, "_", rowvar, "_", landarea2, unitvar2)
    } else {
      outfn.estpse <- paste0(pretitle, "_", rowvar, "_", colvar, "_", landarea2,
		unitvar2)
    }
  } else {
    outfn.estpse <- outfn
  }

  if (is.null(title.estpse)) {
    titlelst <- list(title.est=trimws(title.est), title.pse=trimws(title.pse))
  } else {
    titlelst <- list(title.estpse=trimws(title.estpse))
  }

  if (esttype %in% c("TREE", "RATIO")) {
    titlelst$title.yvar <- title.yvar
    titlelst$title.estvar <- title.estvarn

    if (esttype == "RATIO") {
      titlelst$title.yvard <- title.yvard
      titlelst$title.estvard <- title.estvard
    }
  }

  if (!is.null(title.unitvar)) {
    titlelst$title.unitvar <- title.unitvar
  }
  titlelst$title.ref <- title.ref
  titlelst$outfn.estpse <- outfn.estpse
  if (rawdata) {
    outfn.rawdat <- paste(outfn.estpse, "rawdata", sep="_")
    titlelst$outfn.rawdat <- outfn.rawdat
  }
  if (parameters) {
    outfn.param <- paste(outfn.estpse, "parameters", sep="_")
    titlelst$outfn.param <- outfn.param
  }
 
  if (rowvar == "TOTAL") {
    titlelst$title.tot <- title.tot
  } else {
    titlelst$title.rowvar <- title.rowvar
    titlelst$title.row <- title.row
  }
  if (colvar != "NONE") {
    titlelst$title.colvar <- title.colvar
    titlelst$title.col <- title.col
  }
  if (!is.null(title.unitsn)) {
    titlelst$title.unitsn <- title.unitsn
  }
  if (!is.null(title.unitsd)) {
    titlelst$title.unitsd <- title.unitsd
  }

  return(titlelst)
}


