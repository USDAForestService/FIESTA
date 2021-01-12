anGBpop_ADFOR <- function(ADFOR.name=NULL, RS=NULL, strat_layer=NULL, 
	outfolder=NULL, ...) {


  ## Check for packages
  if (!"FIESTAdata" %in% rownames(installed.packages()))
    stop("FIESTAdata package is required for anGBest_ADFOR()")

  ## Set global variables
  gui <- TRUE
  ref_titles <- FIESTA::ref_titles
  plt <- NULL
  datsource <- "sqlite"

  ## Check Research Station (RS)
  rslst <- c("RMRS","SRS","NCRS","NERS","PNWRS")
  RS <- FIESTA::pcheck.varchar(var2check=RS, varnm="RS", 
		checklst=rslst, caption="Research Unit?", gui=FALSE, multiple=TRUE)
  if (is.null(RS)) RS <- rslst  

  fsregion <- {}
  if ("RMRS" %in% RS)
    fsregion <- c(fsregion, c("01", "02", "03", "04"))
  if ("SRS" %in% RS)
    fsregion <- c(fsregion, "08")
  if (any(c("NCRS","NERS") %in% RS))
    fsregion <- c(fsregion, "09")
  if ("PNWRS" %in% RS)
    fsregion <- c(fsregion, c("05", "06", "10"))
         				
  ## Get fnamelst from bnd.att, based on region
  fnamelst <- FIESTAdata::get_ALP_names(REGION=fsregion)

  ## Check strat_layer
  if (is.null(strat_layer)) 
    strat_layer <- FIESTAdata::get_tnt()


  ## Check ADFOR.name
  if (!is.null(ADFOR.name)) {
    if (!ADFOR.name %in% fnamelst) {
      message("invalid ADFOR.name... must be in folowing list: ")
      print(fnamelst)
      stop()
    } 
  } else {
    if (gui && .Platform$OS.type=="windows")
      ADFOR.name <- select.list(fnamelst, title="ADFOR.name?", multiple=FALSE)
  }

  
  ## Import forest boundary from FIESTAdata
  bnd.att <- "FORESTNAME"
  sql <- paste("select * from BasicOwnershipFS where", 
			FIESTA::getfilter(bnd.att, ADFOR.name, syntax="sql"))
  bnd <- spImportSpatial(dsn=FIESTAdata::get_ALP(), layer="BasicOwnershipFS", sql=sql)


  ## Get outfn.pre by subsetting ADFOR.name
#  if (is.null(outfn.pre)) 
#    outfn.pre <- strsplit(ADFOR.name, " National Forest")[[1]]

  ## Create outfn.pre for output
  outfn.pre <- strsplit(ADFOR.name, " National Forest")[[1]]

  ####################################################################
  ## Get estimates
  ####################################################################
  message("calculating estimates for ", ADFOR.name, "...")

  
  GBpoplst <- anGBpop(bnd=bnd, bnd.att=bnd.att, strat_layer=strat_layer, 
	outfn.pre=outfn.pre, ...)
 

  return(GBpoplst)
}
