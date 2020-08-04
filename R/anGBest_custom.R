anGBest_custom <- function(GBpopdat, esttype="TREE", landarea="FOREST", 
	plt.filter=NULL, cond.filter=NULL, estvar=NULL, estvar.filter=NULL, 
	rowvar=NULL, colvar=NULL, treedia.brks=c(0,5,10,20,50,100), 
	divideby="thousand", title.ref=NULL, title.main=NULL, getbarplot=TRUE, 
	barplot.row=TRUE, barplot.ord="DESC", barplot.col=NULL, barplot.ylim=NULL, 
	savedata=FALSE, outfolder=NULL, outfn.pre=NULL, outfn.date=FALSE, 
	overwrite=TRUE) {


  ## Set global variables
  gui <- FALSE
  ref_titles <- FIESTA::ref_titles
  sumunits <- FALSE
  returnlst <- list()
  row.FIAname=col.FIAname <- TRUE


  ## Check esttype 
  ########################################################
  esttypelst <- c("AREA", "TREE", "RATIO")
  esttype <- FIESTA::pcheck.varchar(var2check=esttype, varnm="esttype", 
		checklst=esttypelst, caption="Estimation type", stopifnull=TRUE)

  ## Check savedata 
  savedata <- FIESTA::pcheck.logical(savedata, varnm="savedata", 
		title="Save data extraction?", first="NO", gui=gui)  

  ## Check overwrite, outfn.date, outfolder, outfn 
  ########################################################
  if (savedata) {
    outfolder <- pcheck.outfolder(outfolder, gui=gui)
    outfn.date <- FIESTA::pcheck.logical(outfn.date , varnm="outfn.date", 
		title="Add date to outfiles?", first="NO", gui=gui) 
    overwrite <- FIESTA::pcheck.logical(overwrite, varnm="overwrite", 
		title="Overwrite?", first="NO", gui=gui)  

    if (!is.null(outfn.pre)) {
      outfolder <- file.path(outfolder, outfn.pre)
      if (!dir.exists(outfolder)) dir.create(outfolder)
    }
  }


  ###########################################################################
  ## Extract FIA data and model data
  ###########################################################################
  if (is.null(GBpopdat))
    stop("must include population data - anGBpop()")


  ## Add variable for tree diameter class
  if (esttype != "AREA") {
    if ((!is.null(rowvar) && rowvar == "DIACL") || (!is.null(colvar) && colvar == "DIACL")) {

      ## Check treedia.brks
      if (!is.null(treedia.brks)) {
        datlut <- datLUTclass(x=GBpopdat$treex, xvar="DIA", cutbreaks=treedia.brks) 
        GBpopdat$treex <- datlut$xLUT

        print(table(GBpopdat$treex$DIACL))
        if (rowvar == "DIACL") row.FIAname <- FALSE
        if (colvar == "DIACL") col.FIAname <- FALSE
      }
    }
  }
    
  if (esttype == "AREA") {
    ####################################################################
    ## Get estimates
    ####################################################################
    FIAest <- modGBarea(GBpopdat=GBpopdat, landarea=landarea, 
		plt.filter=plt.filter, cond.filter=cond.filter, 
		rowvar=rowvar, row.FIAname=row.FIAname, colvar=colvar, col.FIAname=col.FIAname, 
		sumunits=sumunits, rawdata=TRUE, divideby=divideby, 
		returntitle=TRUE, title.ref=title.ref, savedata=savedata, 
		outfolder=outfolder, outfn.pre=outfn.pre, outfn.date=outfn.date, 
		overwrite=overwrite)

  } else if (esttype == "TREE") {

    ####################################################################
    ## Get estimates
    ####################################################################

    FIAest <- modGBtree(GBpopdat=GBpopdat, landarea=landarea, 
		plt.filter=plt.filter, cond.filter=cond.filter, 
		estvar=estvar, estvar.filter=estvar.filter,
		rowvar=rowvar, row.FIAname=row.FIAname, colvar=colvar, col.FIAname=col.FIAname, 
		sumunits=sumunits, rawdata=TRUE, divideby=divideby, 
		returntitle=TRUE, title.ref=title.ref, savedata=savedata, 
		outfolder=outfolder, outfn.pre=outfn.pre, outfn.date=outfn.date, 
		overwrite=overwrite)

  } else if (esttype == "RATIO") {
    FIAest <- modGBratio(GBpopdat=GBpopdat, landarea=landarea, 
		plt.filter=plt.filter, cond.filter=cond.filter, 
		estvarn=estvar, estvarn.filter=estvar.filter,
		rowvar=rowvar, row.FIAname=row.FIAname, colvar=colvar, col.FIAname=col.FIAname, 
		sumunits=sumunits, rawdata=TRUE, divideby=divideby, 
		returntitle=TRUE, title.ref=title.ref, savedata=savedata, 
		outfolder=outfolder, outfn.pre=outfn.pre, outfn.date=outfn.date, 
		overwrite=overwrite)
  }
  est <- FIAest$est

  if (!is.null(colvar)) 
    pse <- FIAest$pse
  raw <- FIAest$raw
  titlelst <- FIAest$titlelst

  ####################################################################
  ## Get barplot
  ####################################################################
  if (getbarplot) {
    estcol <- ifelse(esttype == "RATIO", "rhat", "est")
    secol <- ifelse(esttype == "RATIO", "rhat.se", "est.se")
    if (esttype != "RATIO" && !is.null(divideby)) {
      estcol <- paste0("est.", divideby)
      secol <- paste0("est.se.", divideby)
    }

    ## Check barplot.ord 
    ########################################################
    orderlst <- c('DESC', 'ASC')
    barplot.ord <- FIESTA::pcheck.varchar(var2check=barplot.ord, varnm="barplot.ord", 
		checklst=orderlst, caption="Barplot order")

    ## Check barplot.col 
    ########################################################
    colorlst <- c('rainbow', 'heat', 'terrain', 'topo', 'cm', 'hcl1', 'hcl2')
    barplot.col <- FIESTA::pcheck.varchar(var2check=barplot.col, varnm="barplot.col", 
		checklst=colorlst, caption="Barplot color")

    if (!is.null(colvar)) {
      ## Check barplot.row
      barplot.row <- FIESTA::pcheck.logical(barplot.row, varnm = "barplot.row", 
        	title = "Row values?", first = "YES", stopifnull = TRUE)

      if (barplot.row) {
        title.rowvar <- titlelst$title.rowvar
        bpest <- raw$unit.rowest[, c(title.rowvar, estcol, secol, "NBRPLT.gt0")]
      } else {
        title.colvar <- titlelst$title.colvar
        bpest <- raw$unit.colest[, c(title.colvar, estcol, secol, "NBRPLT.gt0")]
      } 
    } else {
      title.rowvar <- titlelst$title.rowvar
      bpest <- raw$unit.rowest[, c(title.rowvar, estcol, secol, "NBRPLT.gt0")]
    }

    xvar <- names(bpest)[1]
    nbrx <- nrow(bpest) 
    if (esttype %in% "AREA") {
      ylabel <- "Area - Acres"
      if (!is.null(divideby)) 
        ylabel <- paste0(ylabel, ", in ", divideby, "s")
      bplotfn <- paste0(esttype, "_", rowvar, "_barplot")

    } else {
      if (estvar %in% ref_titles$DOMVARNM) {
        ylabel <- ref_titles[ref_titles$DOMVARNM == estvar, "DOMTITLE"]
      } else {
        ylabel <- estvar
      }
 
      if (esttype == "RATIO") {
        ylabel <- paste(ylabel, "per acre") 
      } else if (!is.null(divideby)) {
        ylabel <- paste0(ylabel, ", in ", divideby, "s")
      }
      bplotfn <- paste0(esttype, "_", estvar, "_", rowvar, "_barplot")
    }

    if (is.null(barplot.col)) {
      bplot.col <- NULL
    } else if (barplot.col == "rainbow") {
      bplot.col <- rainbow(nbrx)
    } else if (barplot.col == "heat") {
      bplot.col <- heat.colors(nbrx)
    } else if (barplot.col == "terrain") {
      bplot.col <- terrain.colors(nbrx)
    } else if (barplot.col == "topo") {
      bplot.col <- topo.colors(nbrx)
    } else if (barplot.col == "cm") {
      bplot.col <- cm.colors(nbrx)
    } else if (barplot.col == "hcl1") {
      bplot.col <- hcl.colors(nbrx)
    } else if (barplot.col == "hcl2") {
      bplot.col <- hcl.colors(nbrx, "Set 2")
    } else {
      bplot.col <- NULL
    }

    if (is.null(title.main)) {
      main <- wraptitle(titlelst$title.row, 60)
    } else {
      main <- wraptitle(title.main, 60)
    }
 
   datBarplot(x=bpest, xvar=xvar, yvar=estcol, errbars=TRUE, sevar=secol, 
		savedata=savedata, outfolder=outfolder, x.order=barplot.ord,
		outfn=bplotfn, outfn.date=outfn.date, overwrite=overwrite, 
		device.height=7, las.xnames=2, ylabel=ylabel, col=bplot.col, 
		main=main, ylim=barplot.ylim, toplabelvar="NBRPLT.gt0")
  }

  returnlst$est <- est
  if (!is.null(colvar)) 
    returnlst$pse <- pse
  returnlst$raw <- raw
  returnlst$titlelst <- titlelst

  return(returnlst)
}

