anGBest_core_data <- function(state, datsource="ORACLE", FS_FIADB=TRUE, evalCur=TRUE, 
	evalEndyr=NULL, evalType=c("ALL", "AREAVOL", "GRM"), outfolder=NULL) {


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


  ## Get data from ORACLE
  ####################################
  datPlots <- DBgetPlots(states=state, datsource=datsource, FS_FIADB=FS_FIADB, 
		evalCur=TRUE, istree=TRUE, evalType=evalType, returnPOP=TRUE)
  save(datPlots, file=paste0(outfolder, "/", st, "_datPlots_", 
			format(Sys.time(), "%Y%m%d"), ".rda"))
  plt <- datPlots$plt
  POP_PLOT_STRATUM_ASSGN <- datPlots$POP_PLOT_STRATUM_ASSGN
  evalid <- datPlots$evalid

  datStrata <- DBgetStrata(datsource=datsource, dat=plt, evalid=evalid,
		POP_PLOT_STRATUM_ASSGN=POP_PLOT_STRATUM_ASSGN)
  save(datStrata, file=paste0(outfolder, "/", st, "_datStrata_", 
		format(Sys.time(), "%Y%m%d"), ".rda"))


  ## Get county and survey unit names
  #############################################################
  whereqry <- paste("where statecd ==", stcd)
  if (datsource == "CSV") {
    coqry <- paste("select countycd, countynm from COUNTY", whereqry)
    unitqry <- paste("select value, meaning from REF_UNIT", whereqry)
    ref_countycd <- FIESTA::DBqryCSV(coqry, states=state, sqltables="COUNTY")
    ref_unitcd <- FIESTA::DBqryCSV(unitqry, sqltables="REF_UNIT")
  } else {
    whereqry <- paste("where statecd =", stcd)
    schema. <- ifelse (FS_FIADB, "FS_FIADB.", "FS_NIMS_FIADB_RMRS.")
    #schema. <- "FS_FIADB."
    coqry <- paste0("select countycd, countynm from ", schema., "COUNTY ", whereqry)
    unitqry <- paste0("select value, meaning from ", schema., "REF_UNIT ", whereqry)
    ref_countycd <- FIESTA::DBqryORACLE(coqry)
    ref_unitcd <- FIESTA::DBqryORACLE(unitqry)
  }  
  save(ref_countycd, file=paste0(outfolder, "/ref_countycd_", format(Sys.time(), "%Y%m%d"), ".rda"))
  save(ref_unitcd, file=paste0(outfolder, "/ref_unitcd_", format(Sys.time(), "%Y%m%d"), ".rda"))

  cat(
  " #################################################################################", 
  "\n", paste("Data written to: ", outfolder), "\n", 
  "#################################################################################",
  "\n" )

  return(list(datPlots=datPlots, datStrata=datStrata, ref_countycd=ref_countycd, ref_unitcd=ref_unitcd))
}

