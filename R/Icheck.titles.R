check.titles <- function(dat, esttype, phototype=NULL, Npts=NULL, ratiotype="PERACRE", 
	tabtype="PCT", sumunits=FALSE, title.main=NULL, title.pre=NULL, title.ref=NULL,
 	title.rowvar=NULL, title.rowgrp=NULL, title.colvar=NULL, title.unitvar=NULL, 
	title.filter=NULL, title.units=NULL, title.estvarn=NULL, title.estvard=NULL, 
	unitvar, unitvar2=NULL, rowvar=NULL, colvar=NULL, estvarn=NULL, estvarn.filter=NULL,
 	estvard=NULL, estvard.filter=NULL, addtitle=FALSE, returntitle=TRUE, rawdata=FALSE,
	states=NULL, invyrs=NULL, landarea=NULL, plt.filter=NULL, cond.filter=NULL, 
	allin1=FALSE, divideby=NULL, outfn=NULL, outfn.pre=NULL){ 


  ## TITLE INFO FOR OUTPUT TABLES
  ########################################################
  ref_titles <- FIESTA::ref_titles
  if (!is.null(unitvar2) && unitvar2 == "NONE") unitvar2 <- NULL  
  if (is.null(title.units)) {
    title.units <- "acres"
  } else {
    title.units <- tolower(title.units)
  }

  ## title.unitvar
  if (is.null(title.unitvar)) title.unitvar <- unitvar   
  
  title.est <- NULL
  title.pse <- NULL
  title.estpse <- NULL
  outfn.estpse <- NULL

  ## Title for landarea
  title.landarea <- ifelse (landarea == "FOREST", "forest land", 
	ifelse (landarea == "ALL", "all lands", 
		ifelse (landarea == "TIMBERLAND", "timberland",
			ifelse (landarea == "CHANGE", "land with observed change", ""))))
  landarea2 <- ifelse (landarea == "FOREST", "forestland", 
	ifelse (landarea == "ALL", "allland", 
		ifelse (landarea == "TIMBERLAND", "timberland",
			ifelse (landarea == "CHANGE", "change", ""))))

  if (addtitle || returntitle) {
    title.unitvar.out <- NULL
    if (!sumunits && unitvar != "ONEUNIT") 
      title.unitvar.out <- paste("by", title.unitvar)
    if (!is.null(title.unitvar.out) && !is.null(unitvar2)) 
      title.unitvar.out <- paste(title.unitvar.out, "and", unitvar2)

    #####  TITLE INFO FOR OUTPUT TABLES
    ########################################################
    ## Reference title
    if (is.null(title.ref)) {
      if (!is.null(invyrs)) {
        if (class(invyrs) == "list") {
          refyr <- function(x) 
     			ifelse (length(x) > 1, paste0(min(x), "-", max(x)), x)
          title.ref <- paste(paste(names(invyrs), lapply(invyrs, refyr), sep=", "), 
			collapse="; ")
        } else {
          title.ref <- paste0(states, ", ", min(invyrs), "-", max(invyrs))
        }
      } else if (!is.null(states)) {
        title.ref <- paste(states, collapse="; ")      
      } else {
        title.ref <- ""
      }
    }

    ## GENERATE TABLE TITLES AND outfn
    ###########################################################################
    if (is.null(title.main) || title.main == "") {
      ## Title for filters
      if (is.null(title.filter)) {
        if (!is.null(plt.filter)) {
          title.filter <- paste0(" (", plt.filter)
          if (!is.null(cond.filter))
            title.filter <- paste0(title.filter, " & ", cond.filter)
          title.filter <- paste0(title.filter, ")")
        } else if (!is.null(cond.filter)) {
          title.filter <- paste0(" (", cond.filter, ")")
        }
      } else if (title.filter == "") {
        title.filter <- NULL
      } else {
        title.filter <- paste0(" (", title.filter, ")")
      }

      ## title.part1
      ####################################################################
      title.divideby <- " "
      if (esttype == "PHOTO") {
        title.part1 <- ifelse(tabtype == "PCT", "Estimated percent of", 
		paste0("Estimated area, in ", title.units, ", of"))
        title.units <- NULL

      } else if (esttype == "AREA") {
        if (is.null(title.estvarn)) 
          title.estvarn <- "Area"
        title.part1 <- title.estvarn
        title.landarea <- paste("of", title.landarea)
      
      } else if (esttype %in% c("TREE", "RATIO")) {
        ref_estvar <- FIESTA::ref_estvar
        title.units <- unique(ref_estvar[ref_estvar$ESTVAR == estvarn, "ESTUNITS"])

        if (is.null(title.estvarn)) {
          ref_estvarn <- ref_estvar[ref_estvar$ESTVAR == estvarn, ]
      
          if (!is.null(estvarn.filter)) {
            estfilters <- strsplit(estvarn.filter, "&")[[1]]

            ## Find matching filter in ref_estvar. If more than 1, uses first.
            gfind <- sapply(estfilters, function(x, ref_estvarn) 
        			grep(gsub("\\s", "", x), gsub("\\s", "", ref_estvarn$ESTFILTER)), 
				ref_estvarn)
            if (length(gfind) > 1) gfind <- gfind[1]
            gfind <- table(gfind)
            if (length(gfind) > 0) {
              gfind.max <- names(gfind)[max(gfind)]
              if (length(gfind.max) > 1) gfind.max <- gfind.max[1]
            } else {
              gfind.max <- 1
            }
          } else {
            gfind.max <- 1
          }
          title.estvarn <- ref_estvarn[as.numeric(gfind.max), "ESTTITLE"]

          if (esttype == "RATIO" && ratiotype == "PERACRE")
            title.estvarn <- paste(title.estvarn, "per acre")
        }
        title.part1 <- title.estvarn

        if (esttype == "RATIO" && ratiotype == "PERTREE") {

          if (is.null(title.estvard)) {
            ref_estvard <- ref_estvar[ref_estvar$ESTVAR == estvarn, ]

            if (!is.null(estvard.filter)) {

              estfilters <- strsplit(estvard.filter, "&")[[1]]

              ## Find matching filter in ref_estvar. If more than 1, uses first.
              gfind <- sapply(estfilters, function(x, ref_estvard) 
        			grep(gsub("\\s", "", x), gsub("\\s", "", ref_estvard$ESTFILTER)), 
				ref_estvard)
              gfind <- table(gfind)
              gfind.max <- names(gfind)[max(gfind)]
              if (length(gfind.max) > 1) gfind.max <- gfind.max[1]
            } else {
              gfind.max <- TRUE
            }
            title.estvard <- ref_estvard[gfind.max, "ESTTITLE"]
          }
          title.part1 <- paste(title.part1, "by", tolower(title.estvard))
        }
        title.units <- unique(ref_estvar[ref_estvar$ESTVAR == estvarn, "ESTUNITS"])
        title.landarea <- paste("on", title.landarea)
      }
      if (!is.null(title.units)) {
        title.units <- ifelse (is.null(divideby), paste0(", in ", title.units, ","), 
		paste0(", in ", divideby, " ", title.units, ","))
      } else {
        title.units <- NULL
      }

      title.part1 <- paste0(title.part1, title.units, " ", title.landarea)

      ## title.part2
      ####################################################################
      title.rowgrp2 <- if(!is.null(title.rowgrp)) paste(" and", title.rowgrp) 
      if (rowvar == "TOTAL") {
        title.part2 <- NULL
      } else if (colvar == "NONE") {
        title.part2 <- tolower(paste0("by ", title.rowvar, title.rowgrp2))
      } else {
        if (esttype == "PHOTO" && length(grep("nratio", phototype)) == 0) {
          title.part2 <- paste("of", title.colvar, "within", title.rowvar)         
        } else {
          title.part2 <- tolower(paste0("by ", title.rowvar, title.rowgrp2, 
			" and ", title.colvar))
        }
      }

      ## Add percent sampling error
      if (allin1) {
        title.estpse <- paste(title.part1, "(percent sampling error)", 
			title.part2)
        if (!is.null(title.filter)) title.estpse <- paste(title.estpse, title.filter)
        if (!is.null(title.unitvar.out)) title.estpse <- paste(title.estpse, title.unitvar.out)

      } else {

        if (rowvar == "TOTAL" || 
		(colvar == "NONE" && sumunits) || 
		(colvar == "NONE" && length(unique(dat[[unitvar]])) == 1)) {

          title.estpse <- paste(title.part1, "and percent sampling error", title.part2)

          if (!is.null(title.filter)) title.estpse <- paste(title.estpse, title.filter)
          if (!is.null(title.unitvar.out)) title.estpse <- paste(title.estpse, title.unitvar.out)

        } else {
          title.est <- paste(title.part1, title.part2)
          title.pse <- paste("Percent sampling error of", tolower(title.part1),
			title.part2)
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
    if (!is.null(outfn.pre))
      pretitle <- paste(outfn.pre, pretitle, sep="_")

    if (esttype %in% c("TREE", "RATIO")) {
      pretitle <- paste0(pretitle, "_", estvarn)
      if (esttype == "RATIO") {
        if (is.null(estvard)) estvard <- "ACRE"
        pretitle <- paste0(pretitle, "_", estvard)
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
    if (sumunits)
      outfn.estpse <- paste(outfn.estpse, "sumunits", sep="_") 
  } else {
    outfn.estpse <- outfn
  }

  ## Parameter outfile names
  outfn.param <- paste(outfn.estpse, "parameters", sep="_")

  if (rawdata)
    outfn.rawdat <- paste(outfn.estpse, "rawdata", sep="_")

  titlelst <- list(title.unitvar=title.unitvar, title.est=title.est, title.pse=title.pse,
 	title.estpse=title.estpse, outfn.estpse=outfn.estpse, title.ref=title.ref, 
	outfn.param=outfn.param)
  
  if (rawdata) titlelst$outfn.rawdat <- outfn.rawdat

  return(titlelst)
}  


