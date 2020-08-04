anGBest_evalid <- function(SQLitefn, state, evalidlst=NULL, ppsa="pop_plot_stratum_assgn") {


  ## Check SQLite database
  dbconn <- DBtestSQLite(SQLitefn, dbconnopen=TRUE)

  tablst <- DBI::dbListTables(dbconn)
  if (!ppsa %in% tablst) stop(ppsa, " not in database")
  

  ## Get list of all evalids in dbconn
  if (is.null(evalidlst)) {
    evalidlst <- getEvalid(dbconn, states=state, evalAll=TRUE)
  } else {
    evalidchk <- getEvalid(dbconn, states=state, evalAll=TRUE)
    if (!all(evalidlst %in% evalidchk)) {
      evalid.invalid <- evalidlst[which(!evalidlst %in% evalidchk)]
      stop("invalid evalid: ", toString(evalid.invalid)) 
    }
  }

  ## Get all unitarea and strata pixel counts for all evaluation in state
  stratdat <- DBgetStrata(evalid=evalidlst)
  names(stratdat)

  unitarea <- stratdat$unitarea
  unitvar <- stratdat$unitvar
  areavar <- stratdat$areavar
  strlut <- stratdat$strlut
  strvar <- stratdat$strvar
  strwtvar <- stratdat$strwtvar
  getwt <- stratdat$getwt
  getwtvar <- stratdat$getwtvar


  estlst <- {}
  estrawlst <- {}
  for (evalid in evalidlst) {
    message("getting estimates from ", evalid)

    unitarea.eval <- unitarea[unitarea$EVALID == evalid,]
    strlut.eval <- strlut[strlut$EVALID == evalid,]

    GBpopdat <- modGBpop(cond="cond", plt="plot", tree="tree", pltassgn=ppsa, 
		dsn=SQLitefn, evalid=evalid, strata=TRUE, unitvar=unitvar, unitarea=unitarea.eval, 
		areavar=areavar, stratalut=strlut.eval, strvar=strvar, strwtvar=strwtvar, 
		getwt=TRUE, getwtvar=getwtvar, stratcombine=TRUE, saveobj=FALSE, savedata=FALSE, 
		outfolder=NULL, outfn=NULL, outfn.pre=NULL, outfn.date=TRUE, overwrite=TRUE)
    names(GBpopdat)

    areaest <- modGBarea(GBpopdat=GBpopdat, landarea="FOREST",
                     rowvar="FORTYPCD", row.FIAname=TRUE, 
                     rawdata=TRUE, returntitle=TRUE, savedata=FALSE)
    names(areaest)
    estlst <- rbindlist(list(estlst, data.table(evalid, areaest$est)))
    estrawlst <- rbindlist(list(estrawlst, data.table(evalid, areaest$raw$rowest)))
  }

  return(list(estlst=estlst, estrawlst=estrawlst))
}

