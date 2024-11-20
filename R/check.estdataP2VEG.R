check.estdataP2VEG <- 
  function(esttype, 
           popdatindb, popconn = NULL, 
           cuniqueid = "PLT_CN", condid = "CONDID", 
           vcondsppx = NULL, vcondstrx = NULL, 
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
    if (!is.character(vcondstrx)) {
      stop("treex must be name of table in database")
    } 
    vcondstrnm <- findnm(vcondstrx, tablst, returnNULL = TRUE)
    if (is.null(vcondstrnm)) {
      stop("vcondstrx table is not in database")
    }
    vcondstrflds <- DBI::dbListFields(popconn, vcondstrnm)

    ## vcondsppx
    if (!is.character(vcondsppx)) {
      stop("treex must be name of table in database")
    } 
    vcondsppnm <- findnm(vcondsppx, tablst, returnNULL = TRUE)
    if (is.null(vcondsppnm)) {
      stop("vcondsppx table is not in database")
    }
    vcondsppflds <- DBI::dbListFields(popconn, vcondsppnm)
    
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
	  
    
  ## check tuniqueid in tree table
  vuniqueid <- pcheck.varchar(var2check=vuniqueid, varnm="vuniqueid", gui=gui,
	                   checklst=vcondstrflds, caption="vuniqueid")
  returnlst$vuniqueid <- vuniqueid

  return(returnlst)
}
