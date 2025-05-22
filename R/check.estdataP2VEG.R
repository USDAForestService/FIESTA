check.estdataP2VEG <- 
  function(esttype, 
           popdatindb, popconn = NULL, 
           p2veg_subp_structurex = NULL, p2veg_subplot_sppx = NULL, 
           vuniqueid = "PLT_CN", 
           vfilter = NULL,
           gui = FALSE){

  ###################################################################################
  ## DESCRIPTION: Check data tables for P2VEG estimates
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

  
 	## Check p2veg_subp_structurex
  ###########################################################################
  if (popdatindb) {
    
    if (!is.character(p2veg_subp_structurex)) {
      stop("p2veg_subp_structurex must be name of table in database")
    }
    p2veg_subp_structurenm <- findnm(p2veg_subp_structurex, tablst, returnNULL = TRUE)
    if (is.null(p2veg_subp_structurenm)) {
      stop("p2veg_subp_structurex table is not in database")
    }
    p2veg_subp_structureflds <- DBI::dbListFields(popconn, p2veg_subp_structurenm)

  } else {
    if (!is.data.frame(p2veg_subp_structurex)) {
      stop("p2veg_subp_structurex must be a data.frame object")
    }
    p2veg_subp_structurex <- pcheck.table(p2veg_subp_structurex, stopifnull = TRUE, 
                                          stopifinvalid = TRUE)				
    p2veg_subp_structureflds <- names(p2veg_subp_structurex)
    
  }  
  returnlst$p2veg_subp_structurex <- p2veg_subp_structurex
  returnlst$p2veg_subp_structureflds <- p2veg_subp_structureflds
  
  ## check tuniqueid in tree table
  vuniqueid <- pcheck.varchar(var2check=vuniqueid, varnm="vuniqueid", gui=gui,
                              checklst=p2veg_subp_structureflds, caption="vuniqueid")
  returnlst$vuniqueid <- vuniqueid
  
  
  ## Check p2veg_subplot_sppx
  ###########################################################################
  if (!is.null(p2veg_subplot_sppx)) {
    
    if (popdatindb) {
      
      if (!is.character(p2veg_subplot_sppx)) {
        stop("p2veg_subplot_sppx must be name of table in database")
      }
      p2veg_subplot_sppnm <- findnm(p2veg_subplot_sppx, tablst, returnNULL = TRUE)
      if (is.null(p2veg_subplot_sppnm)) {
        stop("p2veg_subplot_sppx table is not in database")
      }
      p2veg_subplot_sppflds <- DBI::dbListFields(popconn, p2veg_subplot_sppnm)
    
    } else {
      p2veg_subplot_sppx <- pcheck.table(p2veg_subplot_sppx, stopifnull = TRUE, 
                                            stopifinvalid = TRUE)				
      p2veg_subplot_sppflds <- names(p2veg_subplot_sppx)
    }  
    
    returnlst$p2veg_subplot_sppx <- p2veg_subplot_sppx
    returnlst$p2veg_subplot_sppflds <- p2veg_subplot_sppflds
  }

  return(returnlst)
}
