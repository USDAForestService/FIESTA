check.pltcnt <- function(pltx, puniqueid=NULL, unitlut, unitvars=NULL, 
	strvars=NULL, savedata=FALSE, outfolder=NULL, outfn=NULL, gui=FALSE,
	stopiferror=FALSE, showwarnings=TRUE) {

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
  NBRPLOTS=NBRSTRATA=n.strata=n.total=TOTACRES=strwt <- NULL

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

  nostrata <- subset(pltcnt, NBRPLOTS > 0 & NBRSTRATA == 0)
  lt2plots <- subset(pltcnt, NBRPLOTS < 2 & NBRSTRATA > 0)
  lt10plots <- subset(pltcnt, NBRPLOTS >= 2 & NBRPLOTS < 10 & NBRSTRATA > 0)

  errtab <- {}
  errmsg <- {}
  errtyp <- {}
  if (nrow(lt10plots) > 0) { 
    errtyp <- "warn"
    errtab <- rbind(errtab, cbind(lt10plots, errtyp))
    errmsg <- c(errmsg, "there is acerage present for a class with < 10 plots") } 
  if (nrow(nostrata) > 0) {
    errtyp <- "stop" 
    errtab <- rbind(errtab, cbind(nostrata, errtyp))
    errmsg <- c(errmsg, "there is plot(s) in a class with zero acerage") } 
  if (nrow(lt2plots) > 0) { 
    errtyp <- "stop"
    errtab <- rbind(errtab, cbind(lt2plots, errtyp))
    errmsg <- c(errmsg, "there is acerage present for a class with < 2 plots.. must collapse")
  }

  
  if (!is.null(errtab) && showwarnings) { 
    errtab[, NBRSTRATA := NULL]

    cat(
    "\n \n", 
    "##############################################################################", 
    "\n", paste("## warnings/errors.."), "\n",
    "##############################################################################",
    "\n" )

    print(pltcnt)
    cat("\n")

    cat(errmsg, sep="\n")
    print(errtab)

    if (stopiferror && any(errtab[["errtyp"]] == "stop"))
      stop("not enough plots in estimation unit (domain)")

    ###############################################################
    ## If savedata, write to file
    ###############################################################
    if (savedata) {
      acrelutfnbase <- paste(outfn, format(Sys.time(), "%Y%m%d"), sep="_")
      acrelutfn <- fileexistsnm(outfolder, acrelutfnbase, "csv")
      acrelutoutfn <- paste0(outfolder, "/", acrelutfn, ".csv")
      write.csv(pltcnt, acrelutoutfn, row.names=FALSE)

      if (any(errtab$errtyp == "stop")) {
        cat(
      "\n", 
      "########################################################################################", 
      "\n", paste("Table written to:", acrelutoutfn), "\n", "\n", 
	   "Collapse classes and join back to table with estimation unit/strata values.", "\n", 
        "Use datLUTnm FIESTA function.", "\n",
      "########################################################################################",
      "\n" )
      }else{
        cat(
      "\n", 
      "########################################################################################", 
      "\n", paste("Table written to:", acrelutoutfn), "\n",
      "########################################################################################",
      "\n" )
      }
    }
  }

   
  if (!is.null(strunitvars)) {
    ## Change name of NBRPLOTS
    setnames(pltcnt, "NBRPLOTS", "n.strata")

    ## ## Remove NBRSTRATA and merge to unitlut
    pltcnt[, NBRSTRATA := NULL]
    unitlut <- merge(unitlut, pltcnt, by=joinvars)
    pvars <- pvars[pvars %in% names(unitlut)]
    othervars <- names(unitlut)[!names(unitlut) %in% unique(c(pvars, strunitvars))]
    setcolorder(unitlut, c(unique(c(pvars, strunitvars)), othervars))
    setorderv(unitlut, unique(c(pvars, strunitvars)))
  }

  ## Add total number of plots by estimation unit to acrelut (n.total)
  unitlut[, n.total := sum(n.strata, na.rm=TRUE), by=unitvars]


  if (is.null(strvars)) 
    unitlut[, n.strata := NULL]

  return(list(unitlut=unitlut, errtab=errtab))
}
