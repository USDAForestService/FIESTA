check.pltcnt <- function(pltx, puniqueid=NULL, unitlut, unitvars=NULL,
	strvars=NULL, savedata=FALSE, outfolder=NULL, outfn=NULL, overwrite=FALSE,
	outfn.date=TRUE, outfn.pre=NULL, minplotnum.unit=10, minplotnum.strat=2,
	gui=FALSE, stopiferror=FALSE, showwarnings=TRUE) {

  ####################################################################################
  ## CHECKS NUMBER OF PLOTS BY ESTIMATION UNIT/STRATA
  ## If there are < 2 plots in combined strunitvars, an error occurs.. must collapse
  ## If there are 2-10 plots, a warning is displayed..  suggesting to collapse.
  ## If there are plots with no data in acrelut, an error occurs.
  ## If savedata=TRUE and any errors occurred, saves strata counts to outfolder
  ##	(stratacnt.csv).
  ## Appends to unitlut:
  ##   n.strata	- if strvars != NULL, number of plots by strata
  ##   n.total - total number of plots by strata
  ## Returns: unitlut
  ####################################################################################

  ## CHECK GUI - IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  if (nargs() == 0) gui <- TRUE

  ## If gui.. set variables to NULL
  if (gui) getwt=savedata <- NULL

  ## Set global variables
  NBRPLOTS=NBRSTRATA=n.strata=n.total=TOTACRES=strwt=errtab=joinvars=nostrata <- NULL

  ###############################################################
  ## Parameter checks
  ###############################################################

  ## Check pltx
  pltx <- pcheck.table(pltx, gui=gui, tabnm="pltx")

  ## Check unitlut
  ############################################################################
  unitlut <- pcheck.table(unitlut, gui=gui, tabnm="unitlut",
 	caption="Strata table?", nullcheck=TRUE, stopifnull=TRUE)

  ## GET savedata, outfolder, outfn
  savedata <- pcheck.logical(savedata, varnm="savedata", title="Save data?",
		first="YES", gui=gui)

  if (savedata) {
    outfolder <- pcheck.outfolder(outfolder, gui)
    if (is.null(outfn)) outfn <- "stratacnt"
  }

  ###############################################################
  ## DO work
  ###############################################################
  strunitvars <- c(unitvars, strvars)
  pvars <- c("STATECD", "UNITCD")
  pvars2keep <- pvars[which(pvars %in% names(pltx))]
  pvars2keep <- pvars2keep[which(pvars2keep %in% names(unitlut))]

  if (!is.null(strvars)) {
    joinvars <- unique(c(pvars2keep, strunitvars))

    ## Add number of plots by unit
    pltcnt <- pltx[, list(n.total=.N), by=unitvars]
    setkeyv(pltcnt, unitvars)

    pltstrcnt <- pltx[, list(n.strata=.N), by=strunitvars]
    setkeyv(pltstrcnt, strunitvars)

    ## combine total counts and strata counts
    pltcnt <- pltcnt[pltstrcnt]
    setkeyv(pltcnt, strunitvars)

    ## combine total counts and strata counts
    setkeyv(unitlut, strunitvars)
    unitlut <- merge(unitlut, pltcnt, by=strunitvars, all.x=TRUE)
    cols <- c("n.total", "n.strata")
    cols <- cols[cols %in% names(unitlut)]
    if (length(cols) > 0) {
      unitlut <- DT_NAto0(unitlut, cols)
    }

    ## Add number of plots by unit
    #pltstrcnt <- pltx[, n.strata := sum(NBRSTRATA, na.rm=TRUE), by=strunitvars]

    ## Get number of plots by strata variables from pltx - n.strata
    #pltcnt <- pltx[, list(NBRPLOTS=.N), joinvars]
    #setkeyv(pltcnt, strunitvars)

    ## Get number of potential combinations of strata from unitlut
    unitlutcnt <- pltcnt[, list(NBRSTRATA=.N), strunitvars]
    setkeyv(unitlutcnt, strunitvars)

    ## Merge number of plots by strata from pltx and number of potential combos
    pltcnt <- merge(pltcnt, unitlutcnt, all.x=TRUE, all.y=TRUE)
    pltcnt[is.na(pltcnt)] <- 0
    nostrata <- subset(pltcnt, NBRPLOTS > 0 & NBRSTRATA == 0)

    pltcnt$errtyp <- "none"
    #pltcnt[pltcnt$n.strata < minplotnum.strat & pltcnt$n.total < minplotnum.unit
	#	& pltcnt$NBRSTRATA > 0, "errtyp"] <- "warn"
    pltcnt[((pltcnt$n.strata < minplotnum.strat & pltcnt$n.total > minplotnum.unit) |
		pltcnt$n.total < minplotnum.unit), "errtyp"] <- "warn"
    pltcnt[pltcnt$n.total < minplotnum.strat & pltcnt$NBRSTRATA > 0, "errtyp"] <- "warn"

    ## ## Remove NBRSTRATA and merge to unitlut
    pltcnt[, NBRSTRATA:=NULL]
    pvars <- pvars[pvars %in% names(unitlut)]
    othervars <- names(unitlut)[!names(unitlut) %in% unique(c(pvars, strunitvars))]
    setcolorder(unitlut, c(unique(c(pvars, strunitvars)), othervars))
    setorderv(unitlut, unique(c(pvars, strunitvars)))

  } else {
    joinvars <- unique(c(pvars2keep, unitvars))

    ## Add number of plots by unit
    pltcnt <- pltx[, list(n.total=.N), by=unitvars]
    setkeyv(pltcnt, unitvars)

    pltcnt$errtyp <- "none"
    pltcnt[pltcnt$n.total < minplotnum.unit, "errtyp"] <- "warn"

    ## ## Remove NBRSTRATA and merge to unitlut
    unitlut <- merge(unitlut, pltcnt[, c(joinvars, "n.total"), with=FALSE],
		by=joinvars, all.x=TRUE)
    unitlut <- DT_NAto0(unitlut, "n.total")
    pvars <- pvars[pvars %in% names(unitlut)]
    othervars <- names(unitlut)[!names(unitlut) %in% unique(c(pvars, unitvars))]
    setcolorder(unitlut, c(unique(c(pvars, unitvars)), othervars))
    setorderv(unitlut, unique(c(pvars, unitvars)))
  }

  if (showwarnings && any(pltcnt$errtyp == "warn")) {

    msg <- "## warnings/errors"
    message("\n################################### \n",
            msg, "\n###################################")
    message(paste0(capture.output(data.frame(pltcnt)), collapse = "\n"))

    if (stopiferror && any(errtab[["errtyp"]] == "warn")) {
      stop("not enough plots in strata")
    }

    ## If savedata, write to file
    ###############################################################
    if (savedata) {
      write2csv(pltcnt, outfolder=outfolder, outfilenm=outfn, outfn.date=outfn.date,
		outfn.pre=outfn.pre, overwrite=overwrite)
    }
  }

  returnlst <- list(unitlut=unitlut, errtab=pltcnt)

  if (!is.null(nostrata) && nrow(nostrata) > 0) {
    returnlst$nostrat <- nostrata
  }
  return(returnlst)
}
