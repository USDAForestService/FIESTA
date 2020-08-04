anGBest_core_data <- function(state, evalCur=TRUE, evalEndyr=NULL, 
	evalType=c("ALL", "AREAVOL", "GRM"), savedata=TRUE, outfolder=NULL, 
	outfn.date=TRUE) {

  ## Check state
  #######################################################
  state <- pcheck.states(state)
  if (length(state) == 0) stop("must include state")

  stcd <- FIESTA::ref_statecd[FIESTA::ref_statecd$MEANING == state, "VALUE"]
  st <- FIESTA::ref_statecd[FIESTA::ref_statecd$MEANING == state, "ABBR"]

  ## Notes:
  ## 1. If want to add row groups with no subtotals, use GBest*() with rowgrp=TRUE.
  ## 2. If want to add row groups with subtotals, use rowgrptab().
  ## 3. If want to add SE columns side-by-side, use write2xlsx() with addSEcol=TRUE.
  ## 4. If want to add SE values in same cell, use GBest*() with allin1=TRUE or 
  ##	rowgrptab() with allin1=TRUE if subtotals and write2xlsx() with allin1=TRUE.
  ## 5. If want all columns in ref table, use GBest*() or rowgrptab() with col.add0=TRUE.

  ## Check outfolder 
  ########################################################
  outfolder <- FIESTA::pcheck.outfolder(outfolder)

  ## Get plot data from FIA dataMart
  ############################################
  datPlots <- DBgetPlots(states=state, evalEndyr=evalEndyr, evalCur=evalCur, 
		istree=TRUE, evalType=evalType, savePOP=TRUE)
  plt <- datPlots$plt
  POP_PLOT_STRATUM_ASSGN <- datPlots$POP_PLOT_STRATUM_ASSGN
  evalid <- datPlots$evalid

  if (is.null(evalEndyr)) {
    EVALID <- evalid[[1]][1]
    evalEndyr <- paste0("20", substr(EVALID, nchar(EVALID) - 3, nchar(EVALID)-2))
  }

  ## Get strata information from FIA dataMart
  ############################################
  datStrata <- DBgetStrata(evalid=evalid, 
			POP_PLOT_STRATUM_ASSGN=POP_PLOT_STRATUM_ASSGN)

  ## Get county and survey unit reference tables
  #############################################################
  whereqry <- paste("where statecd =", stcd)

  coqry <- paste("select countycd, countynm from COUNTY", whereqry)
  unitqry <- paste("select value, meaning from REF_UNIT", whereqry)
  ref_countycd <- FIESTA::DBqryCSV(coqry, states=state, sqltables="COUNTY")
  ref_unitcd <- FIESTA::DBqryCSV(unitqry, sqltables="REF_UNIT")

############ End CSV only

  if (savedata) {
    datCorefn <- "datCore"
    if (outfn.date) 
      datCorefn <- paste(datCorefn, format(Sys.time(), "%Y%m%d"), sep="_")

    datCore <- list()
    datCore$datPlots <- datPlots
    datCore$datStrata <- datStrata
    datCore$ref_countycd <- ref_countycd
    datCore$ref_unitcd <- ref_unitcd

    if (!is.null(evalEndyr)) {
      fn <- paste0(outfolder, "/", st, "_eval", evalEndyr, "_", datCorefn, ".rda")
    } else {
      paste0(outfolder, "/", st, "_", datCorefn, ".rda")
    }     

    save(datCore, file=fn)
  }

  if (savedata) {
    cat(
    " #################################################################################", 
    "\n", paste("Data written to: ", outfolder), "\n", 
    "#################################################################################",
    "\n" )
  }

  return(datCore)
}

