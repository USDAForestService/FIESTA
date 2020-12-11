anGBest_custom <- function(GBpopdat, esttype="TREE", landarea="FOREST", 
	plt.filter=NULL, cond.filter=NULL, estvar=NULL, estvar.filter=NULL, 
	rowvar=NULL, colvar=NULL, treedia.brks=c(0,5,10,20,50,100), 
	divideby = NULL, title.ref=NULL, title.main=NULL, getbarplot=FALSE, 
	barplot.row=TRUE, barplot.ord="DESC", barplot.color=NULL, barplot.ylim=NULL, 
	barplot.nplt=FALSE, savedata=FALSE, outfolder=NULL, outfn.pre=NULL, 
	outfn.date=FALSE, overwrite=TRUE, ...) {


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


  ## Check getbarplot 
  getbarplot <- FIESTA::pcheck.logical(getbarplot, varnm="getbarplot", 
		title="Create barplot?", first="YES", gui=gui)  

  if (getbarplot) {
    toplabelvar <- NULL

    ## Check barplot.row
    barplot.row <- FIESTA::pcheck.logical(barplot.row, varnm="barplot.row", 
		title="Rows for barplot?", first="NO", gui=gui)  
 
    ## Check barplot.nplots
    barplot.nplt <- FIESTA::pcheck.logical(barplot.nplt, varnm="barplot.nplt", 
		title="Add number of plots?", first="NO", gui=gui)  
  }

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
    MODest <- modGBarea(GBpopdat=GBpopdat, landarea=landarea, 
		plt.filter=plt.filter, cond.filter=cond.filter, 
		rowvar=rowvar, row.FIAname=row.FIAname, colvar=colvar, 
		col.FIAname=col.FIAname, sumunits=sumunits, rawdata=TRUE, 
		returntitle=TRUE, title.ref=title.ref, savedata=savedata, 
		outfolder=outfolder, outfn.pre=outfn.pre, outfn.date=outfn.date, 
		overwrite=overwrite, divideby=divideby, ...)

  } else if (esttype == "TREE") {

    ####################################################################
    ## Get estimates
    ####################################################################
    MODest <- modGBtree(GBpopdat=GBpopdat, landarea=landarea, 
		plt.filter=plt.filter, cond.filter=cond.filter, 
		estvar=estvar, estvar.filter=estvar.filter,
		rowvar=rowvar, row.FIAname=row.FIAname, colvar=colvar, 
		col.FIAname=col.FIAname, sumunits=sumunits, rawdata=TRUE, 
		returntitle=TRUE, title.ref=title.ref, savedata=savedata, 
		outfolder=outfolder, outfn.pre=outfn.pre, outfn.date=outfn.date, 
		overwrite=overwrite, divideby=divideby, ...)

  } else if (esttype == "RATIO") {
    MODest <- modGBratio(GBpopdat=GBpopdat, landarea=landarea, 
		plt.filter=plt.filter, cond.filter=cond.filter, 
		estvarn=estvar, estvarn.filter=estvar.filter,
		rowvar=rowvar, row.FIAname=row.FIAname, colvar=colvar, 
		col.FIAname=col.FIAname, sumunits=sumunits, rawdata=TRUE, 
		returntitle=TRUE, title.ref=title.ref, savedata=savedata, 
		outfolder=outfolder, outfn.pre=outfn.pre, outfn.date=outfn.date, 
		overwrite=overwrite, divideby=divideby, ...)
  }
  est <- MODest$est

  if (!is.null(colvar)) 
    pse <- MODest$pse
  raw <- MODest$raw
  titlelst <- MODest$titlelst

  ####################################################################
  ## Get barplot
  ####################################################################
  if (getbarplot) {
    anMOD_barplot(MODest=MODest, barplot.row=barplot.row,
		barplot.ord=barplot.ord, barplot.color=barplot.color, 
		barplot.ylim=barplot.ylim, barplot.nplt=barplot.nplt,
		savedata=savedata, outfolder=outfolder, outfn.pre=outfn.pre,
		outfn.date=outfn.date, overwrite=overwrite, title.ref=title.ref,
		title.main=title.main, divideby=divideby)
  }


  returnlst$est <- est
  if (!is.null(colvar)) 
    returnlst$pse <- pse
  returnlst$raw <- raw
  returnlst$titlelst <- titlelst

  return(returnlst)
}

