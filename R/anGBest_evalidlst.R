anGBest_evalidlst <- function(evalidlst, data_dsn=NULL,
		esttype="AREA", landarea="FOREST", rowvar=NULL, colvar=NULL,
		estvar=NULL, estvar.filter=NULL, 
		savedata=FALSE, outfolder=NULL) {
  ## DESCRIPTION: estimates for each evalid in list
  
  ## Set global variables
  tree <- NULL


  ## Check esttype 
  ########################################################
  esttypelst <- c("AREA", "TREE", "RATIO")
  esttype <- FIESTA::pcheck.varchar(var2check=esttype, varnm="esttype", 
		checklst=esttypelst, caption="Estimation type", stopifnull=TRUE)

  FIAnamelst <- sort(unique(FIESTA::ref_codes[["VARIABLE"]]))
  rowvar <- FIESTA::pcheck.varchar(var2check=rowvar, varnm="rowvar", 
		checklst=FIAnamelst, caption="Estimation type")
  colvar <- FIESTA::pcheck.varchar(var2check=colvar, varnm="colvar", 
		checklst=FIAnamelst, caption="Estimation type")


  ## Get data from FIAdatamart and store in temporary directory
  ########################################################
  if (is.null(data_dsn)) {
    if (!savedata) { 
      data_dsn <- file.path(tempdir(), "pltdat.sqlite")
    } else {
      outfolder <- pcheck.outfolder(outfolder)
      data_dsn <- file.path(outfolder, "pltdat.sqlite")
    }

    istree <- ifelse(esttype %in% c("TREE", "RATIO"), TRUE, FALSE) 
    pltdat <- DBgetPlots(evalid=evalidlst, istree=istree, savedata=TRUE, 
	out_fmt="sqlite", outfolder=outfolder, out_dsn="pltdat")
    DBI::dbListTables(DBI::dbConnect(RSQLite::SQLite(), data_dsn))
  }

  ## Get all unitarea and strata pixel counts for all evaluation in state
  stratdat <- DBgetStrata(evalid=evalidlst, getassgn=FALSE)
  names(stratdat)

  unitarea <- stratdat$unitarea
  unitvar <- stratdat$unitvar
  areavar <- stratdat$areavar
  strlut <- stratdat$strlut
  strvar <- stratdat$strvar
  strwtvar <- stratdat$strwtvar
  getwt <- stratdat$getwt
  getwtvar <- stratdat$getwtvar


  estlst <- list()
  estrawlst <- list()

  for (evalid in evalidlst) {
    message("getting estimates from ", evalid)
    evalnm <- paste0("eval_",evalid)

    unitarea <- stratdat$unitarea[stratdat$unitarea$EVALID == evalid,] 
    stratalut <- stratdat$strlut[stratdat$strlut$EVALID == evalid,]
    unitvar <- stratdat$unitvar
    areavar <- stratdat$areavar
    strvar <- stratdat$strvar
    getwtvar <- stratdat$getwtvar
    pltassgnid <- "PLT_CN"

    if (esttype %in% c("TREE", "RATIO")) tree <- "tree"
    GBpopdat <- modGBpop(cond="cond", plt="plot", tree=tree,
	pltassgn="ppsa", pltassgnid=pltassgnid, evalid=evalid,
	dsn=data_dsn, pjoinid="CN", strata=TRUE, unitvar=unitvar, 
	unitarea=unitarea, areavar=areavar, stratalut=stratalut, strvar=strvar, 
	getwt=TRUE, getwtvar=getwtvar, stratcombine=TRUE, saveobj=FALSE, savedata=savedata, 
	outfolder=outfolder, outfn.pre=evalnm, outfn.date=FALSE, overwrite=TRUE)

    if (esttype == "AREA") {
      est <- modGBarea(GBpopdat=GBpopdat, landarea=landarea, sumunits=TRUE,
		rowvar=rowvar, row.FIAname=TRUE, 
		rawdata=TRUE, returntitle=TRUE, savedata=FALSE)
    } else if (esttype == "TREE") {
      est <- modGBtree(GBpopdat=GBpopdat, landarea=landarea, sumunits=TRUE,
		estvar=estvar, estvar.filter=estvar.filter,
		rowvar=rowvar, row.FIAname=TRUE, 
		rawdata=TRUE, returntitle=TRUE, savedata=FALSE)
    } else if (esttype == "RATIO") {
      est <- modGBratio(GBpopdat=GBpopdat, landarea=landarea, sumunits=TRUE,
		estvarn=estvar, estvarn.filter=estvar.filter,
		rowvar=rowvar, row.FIAname=TRUE, 
		rawdata=TRUE, returntitle=TRUE, savedata=FALSE)
    }  
 
    estlst[[evalnm]] <- est$est
    estrawlst[[evalnm]] <- est$raw$unit.rowest
  }
}
