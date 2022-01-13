check.estfilters <- function(esttype, pltcondf=NULL, cuniqueid="PLT_CN",
	treex=NULL, seedx=NULL, vcondx=NULL, tuniqueid="PLT_CN", estseed="none",
	vuniqueid="PLT_CN", landarea=NULL, ACI.filter=NULL, pcfilter=NULL,
	TPA=TRUE, tpavar="TPA_UNADJ", gui=FALSE){

  ###################################################################################
  ## DESCRIPTION: Checks data inputs
  ## Apply plot filter
  ## - pcfilter (e.g., COUNTY == 3, FORTYPCD == 122)
  ## Check landarea ("FOREST", "ALL", "TIMBERLAND") and create landarea.filter
  ## - if landarea = FOREST, "COND_STATUS_CD == 1"
  ## - if landarea = TIMBERLAND, "SITECLCD %in% c(1:6) & RESERVCD == 0"
  ## Apply condition filters
  ## - landarea.filter
  ## - ACI.filter (e.g., if, ACI=FALSE, COND_STATUS_CD == 1)
  ###################################################################################


  ###########################################################################
  ## Apply pcfilter (plot and cond filters) to pltcondf table
  ###########################################################################
  pltcondnmlst <- names(pltcondf)
  pltcondf <- datFilter(x=pltcondf, xfilter=pcfilter, title.filter="plt filter?",
		gui=gui, filternm="pcfilter", xnm="pltcondf")$xf
  if (is.null(pltcondf)) {
    message(paste(pcfilter, "removed all records"))
    return(NULL)
  }

  #############################################################################
  ## Check landarea
  #############################################################################
#  if (esttype == "RATIO") {
#    landarealst <- c("FOREST", "TIMBERLAND")
#  } else {
    landarealst <- c("FOREST", "ALL", "TIMBERLAND")
#  }
  if (esttype == "LULC") {
    landarealst <- c(landarealst, "CHANGE")
  }
  landarea <- pcheck.varchar(var2check=landarea, varnm="landarea", gui=gui,
	checklst=landarealst, caption="Sample land area?")


  ## Create landarea.filter
  #############################################################################
  landarea.filter <- NULL
  landcols <- {}
  if (landarea != "ALL") {
    if (landarea == "FOREST") {
      if ("COND_STATUS_CD" %in% names(pltcondf)) {
        landarea.filter <- "COND_STATUS_CD == 1"
        landcols <- "COND_STATUS_CD"
      }
    } else if (landarea == "TIMBERLAND") {
      landcols <- c("SITECLCD", "RESERVCD")
      if (any(!landcols %in% pltcondnmlst)) {
        landcols.miss <- landcols[which(!landcols %in% pltcondnmlst)]
        stop(paste("missing variables for TIMBERLAND landarea filter:",
		paste(landcols.miss, collapse=", ")))
      }
      landarea.filter <- "SITECLCD %in% c(1:6) & RESERVCD == 0"
    }
    ## Check for missing landcols
    if (length(landcols) > 0) {
      landcolsmiss <- landcols[which(!landcols %in% pltcondnmlst)]
      if (length(landcolsmiss) > 0) {
        stop("missing variables: ", paste(landcolsmiss, collapse=", "))
      }
    }
  }

  ###################################################################################
  ## Apply landarea filters
  ###################################################################################

  ## Apply landarea.filter to pltcondf
  pltcondf <- FIESTA::datFilter(x=pltcondf, xfilter=landarea.filter,
		title.filter="landarea filter", gui=gui, stopifnull=FALSE)$xf
  if (is.null(pltcondf)) {
    message(paste(landarea.filter, "removed all records"))
    return(NULL)
  }

  ## Apply ACI.filter to condf
  if (landarea != "ALL") {
    pltcondf <- FIESTA::datFilter(x=pltcondf, xfilter=ACI.filter,
			title.filter="ACI.filter", gui=gui, stopifnull=FALSE)$xf
    if (is.null(pltcondf)) {
      message(paste(ACI.filter, "removed all records"))
      return(NULL)
    }
  }


  ## Set up list of variables to return
  ######################################################################################
  returnlst <- list(pltcondf=setDT(pltcondf), landarea=landarea)

  if (esttype %in% c("TREE", "RATIO", "SEED")) {

    ## Check that the values of tuniqueid in treex are all in cuniqueid in condf
    treef <- check.matchval(treex, pltcondf, tuniqueid, cuniqueid, tab1txt="tree",
		tab2txt="cond", subsetrows=TRUE)
    returnlst$treef <- setDT(treef)

    if (!is.null(seedx)) {
      seedf <- check.matchval(seedx, pltcondf, tuniqueid, cuniqueid, tab1txt="seed",
		tab2txt="seed", subsetrows=TRUE)
      returnlst$seedf <- setDT(seedf)
    }

    ## Check estseed
    ########################################################
    estseedlst <- c("none", "only", "add")
    estseed <- pcheck.varchar(var2check=estseed, varnm="estseed",
		checklst=estseedlst, caption="Seedlings", stopifnull=TRUE)
    if (estseed == "none") {
      seedx <- NULL
    } else {
      if (is.null(seedx)) {
        stop("no seedling data in population data")
      }
    }
    returnlst$estseed <- estseed

    ## Check TPA
    ########################################################
    TPA <- pcheck.logical(TPA, varnm="TPA",
		title="TPA?", first="YES", gui=gui, stopifnull=TRUE)
    returnlst$TPA <- TPA

    ## Check tpavar
    ########################################################
    if (TPA) {
      if (!is.null(treex) && !tpavar %in% names(treex)) {
        stop("must include ", tpavar, " in tree table")
      }
      if (!is.null(seedx) && !tpavar %in% names(seedx)) {
        stop("must include ", tpavar, " in seed table")
      }
    } else {
      tpavar <- NULL
    }
    returnlst$tpavar <- tpavar
  }

  if (esttype == "P2VEG") {
    if (!is.null(vcondx)) {
      ## Check that the values of vuniqueid in vcondsppx are all in cuniqueid in condf
      vcondf <- check.matchval(vcondx, pltcondf, vuniqueid, cuniqueid,
		tab1txt="vcondx", tab2txt="cond", subsetrows=TRUE)
     returnlst$vcondf <- vcondf
    }
  }

  return(returnlst)
}

