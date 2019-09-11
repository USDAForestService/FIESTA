anGBestSppByYear <- function(tree=NULL, cond=NULL, plt=NULL, tuniqueid="PLT_CN",
	puniqueid="CN", getStrata=FALSE, unitarea=NULL, stratalut=NULL, 	
	strvar="STRATUMCD", areavar="ACRES", state=NULL, invyrs=NULL, sppvar="SPCD",
 	sppcd=NULL, pltcond.filter=NULL, estvar=NULL, estvar.TPA=TRUE, estvar.filter=NULL, 
	bplot=TRUE, savedata=FALSE, outfolder=NULL, outfn=NULL, overwrite=FALSE,
	title.main=NULL, title.estvar=NULL, title.spp=NULL){

  #####################################################################################
  ## DESCRIPTION:   
  ## If state-level data tables are already available for the general area, 
  ##   include them as parameters. Otherwise, they will be automatically downloaded 
  ##   from database. 
  #####################################################################################

  ##################################################################
  ## CHECK INPUT PARAMETERS
  ##################################################################

  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- ifelse(nargs() == 0, TRUE, FALSE)

  if(gui){puniqueid=cuniqueid=tuniqueid=estvar.filter=sppvar=savedata <- NULL}
  estvarlst.not <- c("PLT_CN", "CONDID")

  ## Global variables
  INVYR <- NULL


  ## Check input parameters
  ########################################################
  pltx <- FIESTA::pcheck.table(plt, gui=gui, caption="Plot table?")
  condx <- FIESTA::pcheck.table(cond, gui=gui, caption="Cond table?")
  treex <- FIESTA::pcheck.table(tree, gui=gui, caption="Tree table?")
  states <- FIESTA::pcheck.states(state)

  if (is.null(treex)) {
    getPlots <- TRUE
  } else {
    if (!is.null(pltx)) stop("must include plt")
  }

  if (getPlots) {
    if (is.null(states)) stop("must include states(s)")
    getStrata <- TRUE

    writeLines(paste("Getting data from FIA DataMart"))
    pltcond.filter <- NULL
    DBplots <- DBgetPlots(datsource="CSV", states=state, allyrs=TRUE, istree=TRUE, 
			allFilter=pltcond.filter)

#    DBplots <- DBgetPlots(states="Colorado", allyrs=TRUE, istree=TRUE, isshp=TRUE,
#		regionVars=TRUE, actual=TRUE, shpcoords="ACTUAL", savedata=TRUE, 
#		outfolder=datfolder)

    pltx <- setDT(DBplots$plt)
    condx <- setDT(DBplots$cond)
    treex <- setDT(DBplots$tree)

    puniqueid <- "CN"
    tuniqueid <- "PLT_CN"
  }
 
  if (getStrata) {

    stratadat <- DBgetStrata(dat=pltx)
      names(stratadat)
      pltstrat <- stratadat$datstrat
      unitarea <- stratadat$unitarea
      stratalut <- stratadat$strlut 
      areavar <- stratadat$areavar 

  } else {
    if (is.null(stratalut)) {
      writeLines("no strata applied")
    } else {
      stratalut <- pcheck.table(stratalut, gui=gui, caption="Strata table?")
      if (!is.data.frame(stratalut)) {
        stop("stratalut is invalid")
      } else {
        if (is.null(strvar)){ strvar <- "STRATA" }
        if (!strvar %in% names(stratalut)) {
          stop("strvar not in stratalut") }
        if (!strvar %in% c(names(pltx), names(condx))) {
          stop("strvar not in plot data") }
        if (!areavar %in% names(stratalut)) {
          stop("areavar not in stratalut") }
      }
    }
  }  

  ## GETS sppvar 
  ########################################################
  sppvarlst <- c("SPCD", "SPGRPCD")

  sppvar <- pcheck.varchar(sppvar, varnm="sppvar", sppvarlst, 
		caption="Species Variable?", stopifnull=TRUE)

  if (sppvar == "SPCD") {
    colvar <- "SPCD"
    if (!"SPCD" %in% names(treex)) 
      stop("SPCD variable is not in tree table")
    spcdlst <- sort(unique(treex$SPCD))
    if (is.null(sppcd)) {
      sppcd <- select.list(as.character(spcdlst), title="Species Code?", multiple=TRUE)
      if (length(sppcd) == 0) stop("")
    } else {
      if (!all(sppcd %in% spcdlst)) {
        notin <- sppcd[which(!sppcd %in% spcdlst)]
        stop(paste("invalid sppcd: ", paste(notin, collapse=",")))
      }
    }
    rowvar.filter <- paste("SPCD %in%", sppcd)

  } else if (sppvar == "SPGRPCD") {
    colvar <- "SPGRPCD"
    if (!"SPGRPCD" %in% names(treex))
      stop("SPGRPCD variable is not in tree table")
    spgrpcdlst <- sort(unique(treex$SPGRPCD))

    if (is.null(sppcd)) {
      sppcd <- select.list(as.character(spgrpcdlst), title="Species Group Code?", 
		multiple=TRUE)
      if (length(sppcd) == 0) stop("")
    } else {
      if (!all(sppcd %in% spgrpcdlst)) {
        notin <- sppcd[which(!sppcd %in% spgrpcdlst)]
        stop(paste("invalid sppcd: ", paste(notin, collapse=",")))
      }
    }
    rowvar.filter <- paste("SPGRPCD %in%", sppcd)
  }
 

  ## GET TREE ESTIMATION VARIABLE AND CHECK IF IN TREE DATA SET
  estvarlst <- names(treex)[!names(treex) %in% estvarlst.not]
  estvar <- pcheck.varchar(estvar, varnm="estvar", estvarlst, 
		caption="Estimation Variable?", stopifnull=TRUE)

  ## GETS TPA 
  estvar.TPA <- FIESTA::pcheck.logical(estvar.TPA, varnm="estvar.TPA", "Calculate TPA?", 
		first="YES", gui=gui)

  ## GETS bplot 
  bplot <- FIESTA::pcheck.logical(bplot, varnm="bplot", "Bar plot?", first="YES", gui=gui)


  ## TITLE INFO FOR OUTPUT TABLES
  ########################################################
  if (is.null(title.spp)) {
    title.spp <- paste0("SP", sppcd) }
  if (is.null(title.estvar)) {
    title.estvar <- estvar }
  if( is.null(title.main)) {
    title.main <- capfirst(paste(title.estvar, "by year -", title.spp)) }


  ### GET savedata 
  savedata <- pcheck.logical(savedata, varnm="savedata", "Save data tables?", 
	first="YES", gui=gui)

  ## GET outfolder 
  ########################################################
  if (savedata) {
    outfolder <- FIESTA::pcheck.outfolder(outfolder, gui=gui)

    ## GET outfn
    if (is.null(outfn)) {
      outfn <- paste0("sppByYear_", estvar, "_", sppvar, "_", sppcd)
    } else {
      outfn <- paste0(outfn, "_")
    }
    outcsvfn <- paste0(outfolder, "/", outfn, ".csv")
  }



  ########################################################################################
  ### DO THE WORK 
  ######################################################################################## 
  if (is.null(invyrs)) invyrs <- sort(unique(pltx$INVYR))
  
  rowvar <- "SPCD"
  colvar <- NULL

  ref_spcd <- FIESTA::ref_codes[FIESTA::ref_codes$VARIABLE == "SPCD",]
  spplut <- data.frame(SPCD=sort(unique(treex$SPCD)))
  sppest <- merge(spplut, ref_spcd[, c("VALUE", "MEANING")], by.x="SPCD", by.y="VALUE")
  spppse <- merge(spplut, ref_spcd[, c("VALUE", "MEANING")], by.x="SPCD", by.y="VALUE")

  invyr <- invyrs[1]
  for (invyr in invyrs) {
    print(invyr)
    invyr.filter <- paste("INVYR == ", invyr)

    sppdat <- modGBtree(tree=treex, cond=condx, pltstrat=pltstrat, 
		plt.filter=invyr.filter, landarea="FOREST", unitarea=unitarea, 
		stratalut=stratalut, strvar="STRATUMCD", estvar=estvar, estvar.filter=estvar.filter, 
		rowvar=rowvar, rowvar.filter=rowvar.filter, colvar=colvar, 
		savedata=FALSE)$est

#    sppdat <- modGBtree(tree=treex, cond=condx, pltstrat=pltstrat, 
#		plt.filter=invyr.filter, landarea="FOREST", unitarea=sum(unitarea[[areavar]]), 
#		stratalut=stratalut, strvar="STRATUMCD", estvar=estvar, estvar.filter=estvar.filter, 
#		rowvar=rowvar, rowvar.filter=rowvar.filter, colvar=colvar, rowlut=spplut[,"SPCD", drop=FALSE]
#		savedata=FALSE)$est

    sppest <- merge(sppest, sppdat[, c("Species", "Estimate")], by.x="SPCD", by.y="Species")
    spppse <- merge(spppse, sppdat[, c("Species", "Percent Sampling Error")], 
		by.x="SPCD", by.y="Species")

    names(sppest)[names(sppest) == "Estimate"] <- paste0("YR", invyr)
    names(spppse)[names(spppse) == "Percent Sampling Error"] <- paste0("YR", invyr)
  }

#  if (!is.null(title.estvar)) {
#    names(sppdat)[names(sppdat) == "Estimates"] <- title.estvar
#  } else {
#    title.estvar <- "Estimates"
#  }

  if (savedata)
    write2csv(tab=sppdat, outfilenm=outcsvfn, tabtitle=title.main, overwrite=overwrite)
  

  if (bplot) {
    if(estvar %in% c("VOLCFNET", "VOLCFGRS", "FMORTCFAL", "FGROWCFAL")){
      divideby <- 1000000
      ylab <- "Cubic-foot volume (millions)"
    }else if(estvar %in% c("DRYBIO_AG", "DRYBIO_BG")){
      ylab <- "Tons dry biomass (millions)"
      divideby <- 1000000
    }
    errornm <- "Sample Error"

    title.rowvar <- rowvar
    tab <- sppdat$est
    tab <- tab[order(tab[-nrow(tab), title.rowvar]),]
    tab$est <- as.numeric(tab[,title.estvar]) / divideby
    
    datBarplot(x=tab, xvar=title.rowvar, yvar="est", errbars=TRUE, cvvar="Sample Error", 
		device.height=10, device.width=length(invyrs), ylabel=ylab, toplabels=NULL, 
		main=title.main, savedata=savedata, outfolder=outfolder) 
  }

  #if(returndat){
    return(list(sppdat=sppdat, plt=pltx, cond=condx, tree=treex))
  #}else{
  #  return(sppdat)
  #}
}

