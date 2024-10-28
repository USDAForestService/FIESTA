check.estdataVOL <- 
  function(esttype, 
           popdatindb, popconn = NULL, 
           cuniqueid = "PLT_CN", condid = "CONDID", 
           treex = NULL, seedx = NULL, 
           tuniqueid = "PLT_CN", 
           estseed = "none", 
           woodland = "Y", 
           TPA = TRUE, 
           tfilter = NULL,
           gui = FALSE){

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
  ## Check output parameters
  ## - divideby, divides final estimates by (hundred, thousand, million)
  ## - if sumunits=TRUE, aggregates all estimation units
  ## - If allin1=TRUE, puts estimate (% sample error) in each cell
  ## - If savedata=TRUE, saves tables to outfolder
  ## - If addtitle=TRUE, adds title(s) to saved tables
  ## - If returntitle, saves title(s) of tables
  ## - If rawdata=TRUE, generates and saves rawdata variables for estimates
  ## - If savedata, checks output folder
  ## - If rawdata, adds a folder named 'rawdata' to outfolder to add raw data
  ## - estround - round estimate values
  ## - pseround - round percent standard error values
  ###################################################################################

  ## Define returnlst
  returnlst <- list()
  
  ## Check database connection
  if (popdatindb) {
    if (!DBI::dbIsValid(popconn)) {
      stop("database connection is invalid")
    }
    tablst <- DBI::dbListTables(popconn)
  }

  ## Check estseed
  ########################################################
  estseedlst <- c("none", "only", "add")
  estseed <- pcheck.varchar(var2check=estseed, varnm="estseed",
		             checklst=estseedlst, caption="Seedlings", stopifnull=FALSE)
  if (is.null(estseed)) {
    message("estseed must be one of the following: ", toString(estseedlst))
    stop()
  }
  if (estseed == "none") {
    seedx <- NULL
  } else {
    if (is.null(seedx)) {
      message("no seedling data in population data")
		  return(NULL)
    }
  }

 	## Check treex and seedx
  ###########################################################################
  if (estseed != "only") {
    if (popdatindb) {
      if (!is.character(treex)) {
        stop("treex must be name of table in database")
      } 
      treenm <- findnm(treex, tablst, returnNULL = TRUE)
      if (is.null(treenm)) {
        stop("treex table is not in database")
      }
      treeflds <- DBI::dbListFields(popconn, treenm)
    } else {
      if (!is.data.frame(treex)) {
        stop("treex must be a data.frame object")
      }
      treex <- pcheck.table(treex, stopifnull = TRUE, 
                       stopifinvalid = TRUE)				
		  treeflds <- names(treex)
    } 
	  returnlst$treex <- treex
	  returnlst$treeflds <- treeflds
	  
    
    ## check tuniqueid in tree table
    tuniqueid <- pcheck.varchar(var2check=tuniqueid, varnm="tuniqueid", gui=gui,
	                   checklst=treeflds, caption="tuniqueid")
  }
 
  if (estseed %in% c("add", "only")) {
    if (popdatindb) {
      if (!is.character(seedx)) {
        stop("seedx must be name of table in database")
      } 
      seednm <- findnm(seedx, tablst, returnNULL = TRUE)
      if (is.null(seednm)) {
        stop("seedx table is not in database")
      }
      seedflds <- DBI::dbListFields(popconn, seednm)
    } else {
      if (!is.data.frame(seedx)) {
        stop("seedx must be a data.frame object")
      }
      seedx <- pcheck.table(seedx, stopifnull = TRUE, 
                            stopifinvalid = TRUE)				
      seedflds <- names(seedx)
    } 
    returnlst$seedx <- seedx
    returnlst$seedflds <- seedflds
    
	  ## check tuniqueid in seed table	
    if (!tuniqueid %in% seedflds) {
	    message(tuniqueid, " not in seed table")
	    return(NULL)
	  }
  }
  
  returnlst$tuniqueid <- tuniqueid
  returnlst$estseed <- estseed
	
  ## Check woodland
  woodlandlst <- c("Y", "N", "only")
  woodland <- pcheck.varchar(var2check=woodland, varnm="woodland", 
		                checklst=woodlandlst, gui=gui, caption="Woodland?") 
	returnlst$woodland <- woodland
  

  return(returnlst)
}
