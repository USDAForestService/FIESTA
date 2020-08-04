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
  if (gui) getwt=savedata=nostrata <- NULL

  ## Set global variables
  NBRPLOTS=NBRSTRATA=n.strata=n.total=TOTACRES=strwt=errtab=joinvars <- NULL

  ###############################################################
  ## Parameter checks
  ###############################################################

  ## Check pltx
  pltx <- FIESTA::pcheck.table(pltx, gui=gui, tabnm="pltx")

  ## Check unitlut
  ############################################################################
  unitlut <- FIESTA::pcheck.table(unitlut, gui=gui, tabnm="unitlut",
 	caption="Strata table?", nullcheck=TRUE, stopifnull=TRUE)

  ## GET savedata, outfolder, outfn
  savedata <- FIESTA::pcheck.logical(savedata, varnm="savedata", title="Save data?", 
		first="YES", gui=gui)

  if (savedata) {
    outfolder <- FIESTA::pcheck.outfolder(outfolder, gui)
    if (is.null(outfn)) outfn <- "stratacnt"
  }

  ###############################################################
  ## DO work
  ###############################################################
  strunitvars <- c(unitvars, strvars)
  pvars <- c("STATECD", "UNITCD")
  pvars2keep <- pvars[which(pvars %in% names(pltx))]
  pvars2keep <- pvars2keep[which(pvars2keep %in% names(unitlut))]
  joinvars <- unique(c(pvars2keep, strunitvars))
   

  ## Get number of plots by strata variables from pltx - n.strata
  pltcnt <- pltx[, list(NBRPLOTS=.N), joinvars]
  setkeyv(pltcnt, strunitvars)

  ## Get number of potential combinations of strata from unitlut
  unitlutcnt <- unitlut[, list(NBRSTRATA=.N), strunitvars]
  setkeyv(unitlutcnt, strunitvars)

  ## Merge number of plots by strata from pltx and number of potential combos
  pltcnt <- merge(pltcnt, unitlutcnt, all.x=TRUE, all.y=TRUE)
  pltcnt[is.na(pltcnt)] <- 0

  ## Add number of plots by unit
  pltcnt[, n.total := sum(NBRPLOTS, na.rm=TRUE), by=unitvars]

  nostrata <- subset(pltcnt, NBRPLOTS > 0 & NBRSTRATA == 0)

  pltcnt$errtyp <- "none"
  pltcnt[pltcnt$n.total >= minplotnum.strat & pltcnt$n.total < minplotnum.unit 
		& pltcnt$NBRSTRATA > 0, "errtyp"] <- "warn"
  pltcnt[pltcnt$NBRPLOTS < minplotnum.strat & pltcnt$NBRSTRATA > 0, "errtyp"] <- "warn"
  pltcnt[, NBRSTRATA := NULL]


  if (showwarnings && any(pltcnt$errtyp == "warn")) { 

    cat(
    "\n \n", 
    "##############################################################################", 
    "\n", paste("## warnings/errors.."), "\n",
    "##############################################################################",
    "\n" )

    print(pltcnt)
    cat("\n")

    if (stopiferror && any(errtab[["errtyp"]] == "warn"))
      stop("not enough plots in strata")

    ###############################################################
    ## If savedata, write to file
    ###############################################################
    if (savedata)
      write2csv(pltcnt, outfolder=outfolder, outfilenm=outfn, outfn.date=outfn.date,
		outfn.pre=outfn.pre, overwrite=overwrite)
  }
   
  if (!is.null(strunitvars)) {
    ## Change name of NBRPLOTS
    setnames(pltcnt, "NBRPLOTS", "n.strata")

    ## ## Remove NBRSTRATA and merge to unitlut
    unitlut <- merge(unitlut, pltcnt[, c(joinvars, "n.strata", "n.total"), with=FALSE], 
		by=joinvars)
    pvars <- pvars[pvars %in% names(unitlut)]
    othervars <- names(unitlut)[!names(unitlut) %in% unique(c(pvars, strunitvars))]
    setcolorder(unitlut, c(unique(c(pvars, strunitvars)), othervars))
    setorderv(unitlut, unique(c(pvars, strunitvars)))
  }


  if (is.null(strvars) && "n.strata" %in% unitlut) 
    unitlut[, n.strata := NULL]

  returnlst <- list(unitlut=unitlut, errtab=pltcnt)

  if (!is.null(nostrata) && nrow(nostrata) > 0)       
    returnlst$nostrat <- nostrata

  return(returnlst)
}
