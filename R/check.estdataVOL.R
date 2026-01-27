check.estdataVOL <- 
  function(datsource,
           popdatindb, 
           poptablst, 
           treex = NULL, 
           seedx = NULL, 
           treeflds = NULL,
           seedflds = NULL,
           estseed = "none", 
           woodland = "Y",
           gui = FALSE){

    
  ###################################################################################
  ### Check parameters for VOL estimation
  ###################################################################################
    
  ## Check estseed
  estseedlst <- c("none", "only", "add")
  estseed <- pcheck.varchar(var2check = estseed, varnm = "estseed",
		             checklst = estseedlst, gui = gui, 
		             caption="Seedlings", stopifnull=FALSE)
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

  ## Check woodland
  woodlandlst <- c("Y", "N", "only")
  woodland <- pcheck.varchar(var2check = woodland, varnm = "woodland", 
                             checklst = woodlandlst, gui = gui, 
                             caption = "Woodland?") 
  

  ###################################################################################
  ## DESCRIPTION: Check data tables for VOL estimates
  ###################################################################################
  treenm = seednm <- NULL

 	## Check treex and seedx
  ###########################################################################
  if (estseed != "only") {
    if (popdatindb) {
      if (!is.character(treex)) {
        stop("treex must be name of table in database")
      } 
      treenm <- findnm(treex, poptablst, returnNULL = TRUE)
      if (is.null(treenm)) {
        stop("treex table is not in database")
      }
    } else {
      if (!is.data.frame(treex)) {
        stop("treex must be a data.frame object")
      }
      treex <- pcheck.table(treex, stopifnull = TRUE, 
                       stopifinvalid = TRUE)		
      treenm <- "treex"
      names(treex) <- toupper(names(treex))
      treeflds <- names(treex)
    } 
  }

  if (estseed %in% c("add", "only")) {
    if (popdatindb) {
      if (!is.character(seedx)) {
        stop("seedx must be name of table in database")
      } 
      seednm <- findnm(seedx, poptablst, returnNULL = TRUE)
      if (is.null(seednm)) {
        stop("seedx table is not in database")
      }
    } else {
      if (!is.data.frame(seedx)) {
        stop("seedx must be a data.frame object")
      }
      seedx <- pcheck.table(seedx, stopifnull = TRUE, 
                            stopifinvalid = TRUE)
      seednm <- "seedx"
      names(seedx) <- toupper(names(seedx))
      seedflds <- names(seedx)
    } 
  }
 
  return(list(treenm = treenm,
              seednm = seednm,
              treeflds = treeflds,
              seedflds = seedflds,
              estseed = estseed,
              woodland = woodland
              ))
}
