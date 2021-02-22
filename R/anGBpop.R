anGBpop <- function(bnd, bnd_dsn=NULL, bnd.att=NULL, bnd.filter=NULL, 
	popType="VOL", isseed=FALSE, datsource="sqlite", SQLitefn=NULL, RS=NULL, 
	strat_layer=NULL, showsteps=FALSE, savedata=FALSE, savexy=TRUE, 
	outfolder=NULL, out_fmt="csv", out_dsn=NULL, outfn.pre=NULL, 
	outfn.date=FALSE, overwrite_dsn=FALSE, overwrite_layer=TRUE, 
	GBdata=NULL, ...) {


  ## Set global variables
  gui <- FALSE
  returnlst <- list()
  strata <- TRUE
  istree <- FALSE


  ## Check popType
  popTypelst <- c("VOL")
  popType <- FIESTA::pcheck.varchar(var2check=popType, varnm="popType", gui=gui, 
		checklst=popTypelst, caption="population type", stopifnull=TRUE,
		warn="only VOL is currently available")
  if (popType %in% c("VOL", "CHNG")) {
    istree <- TRUE
  }


  ## Check savedata 
  savedata <- FIESTA::pcheck.logical(savedata, varnm="savedata", 
		title="Save data extraction?", first="NO", gui=gui)  

  ## Check overwrite, outfn.date, outfolder, outfn 
  ########################################################
  if (savedata) {
    outlst <- pcheck.output(out_dsn=out_dsn, out_fmt=out_fmt, 
		outfolder=outfolder, outfn.pre=outfn.pre, outfn.date=outfn.date, 
		overwrite=overwrite_dsn, gui=gui)
    out_dsn <- outlst$out_dsn
    outfolder <- outlst$outfolder
    out_fmt <- outlst$out_fmt
  }

  ###########################################################################
  ## Extract FIA data and auxiliary data
  ###########################################################################

  if (is.null(GBdata)) { 
    message("extracting data...")
 
    ###########################################################################
    ## Extract FIA data and model data
    ###########################################################################
    if (is.null(strat_layer)) strata <- FALSE
    GBdata <- anGBdata(bnd_layer=bnd, bnd_dsn=bnd_dsn, bnd.att=bnd.att, 
		bnd.filter=bnd.filter, RS=RS, datsource=datsource, istree=istree, 
		isseed=isseed, data_dsn=SQLitefn, strata=strata, strat_layer=strat_layer, 
		showsteps=showsteps, cex.plots=.75, savedata=savedata, savexy=savexy, 
		outfolder=outfolder, outfn.pre="GBdata", out_fmt=out_fmt, out_dsn=out_dsn, 
		overwrite_dsn=overwrite_dsn, overwrite_layer=overwrite_layer,, ...)
    returnlst$GBdata <- GBdata
  } else {
    GBdata <- pcheck.object(GBdata, objnm="GBdata", 
		list.items=c("bnd", "plt", "cond", "unitarea"))
  }
	

  ####################################################################
  ## Get population data
  ####################################################################
  GBpopdat <- modGBpop(popType=popType, GBdata=GBdata, saveobj=TRUE, 
	outfolder=outfolder)
  names(GBpopdat)
  returnlst$GBpopdat <- GBpopdat


  return(returnlst)
}

