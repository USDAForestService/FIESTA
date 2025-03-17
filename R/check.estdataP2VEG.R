check.estdataP2VEG <- 
  function(esttype, 
           popdatindb, popconn = NULL, 
           cuniqueid = "PLT_CN", condid = "CONDID", 
           vcondsppx = NULL, vcondstrx = NULL, 
           p2veg_subp_structure = NULL,
           p2veg_subplot_spp = NULL,
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

  
 	## Check vcondsppx and vcondstrx
  ###########################################################################
  if (popdatindb) {
    ## vcondstrx
    # if (!is.character(vcondstrx)) {
    #   stop("vcondstrx must be name of table in database")
    # } 
    # vcondstrnm <- findnm(vcondstrx, tablst, returnNULL = TRUE)
    # if (is.null(vcondstrnm)) {
    #   stop("vcondstrx table is not in database")
    # }
    # vcondstrflds <- DBI::dbListFields(popconn, vcondstrnm)
    # 
    # ## vcondsppx
    # if (!is.character(vcondsppx)) {
    #   stop("vcondsppx must be name of table in database")
    # } 
    # vcondsppnm <- findnm(vcondsppx, tablst, returnNULL = TRUE)
    # if (is.null(vcondsppnm)) {
    #   stop("vcondsppx table is not in database")
    # }
    # vcondsppflds <- DBI::dbListFields(popconn, vcondsppnm)
    
    if (!is.character(p2veg_subp_structure)) {
      stop("p2veg_subp_structure must be name of table in database")
    } 
    p2veg_subp_structurex <- findnm("p2veg_subp_structure", tablst, returnNULL = TRUE)
    if (is.null(p2veg_subp_structurex)) {
      stop("p2veg_subp_structure table is not in database")
    }
    vp2veg_subp_structureflds <- DBI::dbListFields(popconn, p2veg_subp_structurex)
    
    ## p2veg_subplot_sppx
    if (!is.character(p2veg_subplot_spp)) {
      stop("p2veg_subplot_spp must be name of table in database")
    } 
    p2veg_subplot_sppx <- findnm(p2veg_subplot_spp, tablst, returnNULL = TRUE)
    if (is.null(p2veg_subplot_sppx)) {
      stop("p2veg_subplot_spp table is not in database")
    }
    p2veg_subplot_sppflds <- DBI::dbListFields(popconn, p2veg_subplot_sppx)
    
    
  } else {
    ## vcondstrx
    if (!is.data.frame(vcondstrx)) {
      stop("vcondsppx must be a data.frame object")
    }
    vcondstrx <- pcheck.table(vcondstrx, stopifnull = TRUE, 
                              stopifinvalid = TRUE)				
    vcondstrflds <- names(vcondstrx)

    ## vcondsppx
    if (!is.data.frame(vcondsppx)) {
      stop("vcondsppx must be a data.frame object")
    }
    vcondsppx <- pcheck.table(vcondsppx, stopifnull = TRUE, 
                       stopifinvalid = TRUE)				
    vcondsppflds <- names(vcondsppx)
  } 
  returnlst$vcondstrx <- vcondstrx
  returnlst$vcondstrflds <- vcondstrflds
  
  returnlst$vcondsppx <- vcondsppx
	returnlst$vcondsppflds <- vcondsppflds
	
	
	returnlst$p2veg_subp_structurex <- p2veg_subp_structurex
	returnlst$vp2veg_subp_structureflds <- vp2veg_subp_structureflds
	
	returnlst$p2veg_subplot_sppx <- p2veg_subplot_sppx
	returnlst$p2veg_subplot_sppflds <- p2veg_subplot_sppflds
	
	  
    
  ## check tuniqueid in tree table
  vuniqueid <- pcheck.varchar(var2check=vuniqueid, varnm="vuniqueid", gui=gui,
	                   checklst=vcondstrflds, caption="vuniqueid")
  returnlst$vuniqueid <- vuniqueid

  return(returnlst)
}
