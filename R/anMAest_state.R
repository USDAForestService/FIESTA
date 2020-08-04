anMAest_state <- function(state=NULL, bycounty=FALSE, SQLitefn, 
	plt.filter=NULL, landarea="FOREST", cond.filter=NULL, 
	esttype="TREE", estvar=NULL, estvar.filter=NULL, rowvar=NULL, 
	rastlst.cont=NULL, rastlst.cont.name=NULL, 
	rastlst.cat=NULL, rastlst.cat.name=NULL, rastlst.cat.NODATA=0,
	divideby=NULL, savedata=FALSE, savexy=FALSE, outfolder=NULL, 
	outfn.pre=NULL, outfn.date=FALSE, overwrite=TRUE, MAdata=NULL) {


  MAmethod="GREG"
  gui <- FALSE 
  PSstrvar <- NULL
  colvar <- NULL
  overwrite <- TRUE
  outfn.date <- TRUE
  getbarplot <- TRUE


  ## Check bycounty 
  bycounty <- FIESTA::pcheck.logical(bycounty, varnm = "bycounty", 
        title = "By county?", first = "YES", gui = gui)

  ## Check savedata 
  savedata <- FIESTA::pcheck.logical(savedata, varnm="savedata", 
		title="Save data extraction?", first="NO", gui=gui)  

  ## Check overwrite, outfn.date, outfolder, outfn 
  ########################################################
  if (savedata) {
    outfolder <- FIESTA::pcheck.outfolder(outfolder, gui)
    overwrite <- FIESTA::pcheck.logical(overwrite, varnm="overwrite", 
		title="Overwrite?", first="NO", gui=gui)  
    outfn.date <- FIESTA::pcheck.logical(outfn.date , varnm="outfn.date", 
		title="Add date to outfiles?", first="NO", gui=gui) 

    if (!is.null(outfn.pre)) {
      outfolder <- file.path(outfolder, outfn.pre)
      if (!dir.exists(outfolder)) dir.create(outfolder)
    }
  }

  ## Set parameters
  ##################################################################
  state <- pcheck.states(state, stopifnull=TRUE)
  bnd <- FIESTA::stunitco
  bnd_dsn <- NULL
  bnd.filter <- paste0("STATENM == '", state, "'")

  ## Check unitvar 
  ########################################################
#  unitvarlst <- names(bnd)[!names(bnd) %in% c("STATECD", "STATENM")]
#  unitvar <- FIESTA::pcheck.varchar(var2check=unitvar, varnm="unitvar", gui=gui, 
#		checklst=unitvarlst, caption="Estimation unit", stopifnull=TRUE)
  if (bycounty) {
    unitvar <- "COUNTYNM"
    sumunits <- TRUE
  }


  ####################################################################
  ## Get FIA plot and model data 
  ####################################################################
  if (is.null(MAdata)) {
    MAdata <- anMAest_data(bnd=bnd, bnd.att=unitvar, bnd.filter=bnd.filter,
		SQLitefn=SQLitefn, states=state, rastlst.cont=rastlst.cont,
 		rastlst.cont.name=rastlst.cont.name, rastlst.cat=rastlst.cat,
 		rastlst.cat.name=rastlst.cat.name, rastlst.cat.NODATA=rastlst.cat.NODATA, 
		savedata=FALSE)
  }

  plt <- MAdata$plt
  cond <- MAdata$cond
  tree <- MAdata$tree
  pltassgn <- MAdata$pltassgn
  unitarea <- MAdata$domarea
  unitvar <- MAdata$domvar
  areavar <- MAdata$areavar
  unitlut <- MAdata$domzonal
  prednames <- MAdata$prednames
  zonalnames <- MAdata$zonalnames
  predfac <- MAdata$predfac
  puniqueid <- MAdata$puniqueid
  pltassgnid <- MAdata$pltassgnid


  ####################################################################
  ## Get estimates
  ####################################################################
  title.ref <- paste0(state, ",", min(pltassgn$INVYR), "-", max(pltassgn$INVYR))

  if (esttype == "AREA") {
    MAest <- modMAarea(cond=cond, plt=plt, pltassgn=pltassgn, puniqueid="PLT_CN", 
		pltassgnid=pltassgnid, sumunits=sumunits, MAmethod=MAmethod,
 		landarea=landarea, cond.filter=NULL, unitarea=unitarea, 
		unitvar=unitvar, unitlut=unitlut, areavar=areavar, prednames=prednames, 
		PSstrvar=PSstrvar, stratcombine=TRUE, predfac=predfac, rowvar=rowvar, 
		row.FIAname=TRUE, colvar=colvar, col.FIAname=TRUE, rawdata=TRUE, 
		divideby=divideby, returntitle=TRUE, title.ref=title.ref, savedata=savedata, 
		outfolder=outfolder, outfn.pre=outfn.pre, outfn.date=outfn.date, 
		overwrite=overwrite)
    est <- MAest$est
    raw <- MAest$raw

  } else if (esttype == "TREE") {
    MAest <- modMAtree(tree=tree, plt=plt, cond=cond, pltassgn=pltassgn, 
		puniqueid="PLT_CN", pltassgnid=pltassgnid, sumunits=sumunits, 
		MAmethod=MAmethod, landarea=landarea, cond.filter=NULL, unitarea=unitarea, 
		unitvar=unitvar, unitlut=unitlut, areavar=areavar, prednames=prednames, 
		PSstrvar=PSstrvar, stratcombine=TRUE, predfac=predfac, rowvar=rowvar, 
		row.FIAname=TRUE, colvar=colvar, col.FIAname=TRUE, estvar=estvar,
 		estvar.filter=estvar.filter, rawdata=TRUE, divideby=divideby, 
		returntitle=TRUE, title.ref=title.ref, savedata=savedata, 
		outfolder=outfolder, outfn.pre=outfn.pre, outfn.date=outfn.date, 
		overwrite=overwrite)
    est <- MAest$est
    raw <- MAest$raw
  }
  if (savedata) {
    MAestfn <- ifelse(!is.null(outfn.pre), paste0(outfn.pre, "_MAest"),
		"MAest") 
    save(MAest, file=paste0(outfolder, "/", MAestfn, "_", esttype, ".rda"))
  }


  ####################################################################
  ## Get barplot
  ####################################################################
  ref_titles <- FIESTA::ref_titles
#  if (getbarplot && is.null(rowvar)) {
#    message("must include rowvar for barplot")
#    getbarplot <- FALSE
#  }
  if (getbarplot) {
    xvar <- names(est)[1]
    yvar <- names(est)[2]
    psevar <- names(est)[3]

    if (esttype == "AREA") {
      ylabel <- "Acres"
    } else if (estvar %in% ref_titles$DOMVARNM) {
      ylabel <- ref_titles[ref_titles$DOMVARNM == estvar, "DOMTITLE"]
    } else {
      ylabel <- estvar
    }
    
    if (!is.null(divideby))
      ylabel <- paste(ylabel, divideby, sep=" - ")
      
    bplotfn <- paste0(rowvar, "_barplot")
    datBarplot(est[est[[names(est)[1]]] != "Total", ], xvar=xvar, yvar=yvar, 
		errbars=TRUE, psevar=psevar, savedata=savedata, outfolder=outfolder, 
		outfn=bplotfn, outfn.pre=outfn.pre, outfn.date=outfn.date, 
		overwrite=overwrite, device.height=7, las.xnames=2, ylabel=ylabel)
  }

  return(list(est=est, raw=raw, MAdata=MAdata))
}
