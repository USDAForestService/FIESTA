check.estdataP2VEG <- 
  function(popdatindb, 
           poptablst,
           p2veg_subp_structurex = NULL, 
           p2veg_subplot_sppx = NULL, 
           p2vegtype,
           peracre,
           gui = FALSE) {

    
  
  ###################################################################################
  ### Check parameters for p2VEG estimation
  ###################################################################################
    
  ## Check peracre
  peracre <- pcheck.logical(peracre, varnm = "peracre",
                            title = "Per-acre estimates?", first = "YES", 
                            gui=gui, stopifnull = TRUE)
    

  ## Check p2vegtype 
  p2vegtypelst <- c("str", "spp")
  p2vegtype <- pcheck.varchar(var2check = p2vegtype, varnm = "p2vegtype", 
                              checklst = p2vegtypelst, caption = "P2VEG type", 
                              stopifnull = TRUE)
    
    
  
  ###################################################################################
  ## DESCRIPTION: Check data tables for P2VEG estimates
  ###################################################################################
  p2veg_subp_structurenm = p2veg_subplot_sppnm <- NULL
    
  ## Check p2veg_subp_structurex
  ###########################################################################
  if (popdatindb) {
      
    if (!is.character(p2veg_subp_structurex)) {
      stop("p2veg_subp_structurex must be name of table in database")
    }
    p2veg_subp_structurenm <- findnm(p2veg_subp_structurex, poptablst, returnNULL = TRUE)
    if (is.null(p2veg_subp_structurenm)) {
      stop("p2veg_subp_structurex table is not in database")
    }
      
  } else {
    if (!is.data.frame(p2veg_subp_structurex)) {
      stop("p2veg_subp_structurex must be a data.frame object")
    }
    p2veg_subp_structurex <- pcheck.table(p2veg_subp_structurex, stopifnull = TRUE, 
                                          stopifinvalid = TRUE)				
    p2veg_subp_structurenm <- "p2veg_subp_structurex"
      
  }  
    
  ## Check p2veg_subplot_sppx
  ###########################################################################
  if (!is.null(p2veg_subplot_sppx)) {
      
    if (popdatindb) {
        
      if (!is.character(p2veg_subplot_sppx)) {
        stop("p2veg_subplot_sppx must be name of table in database")
      }
      p2veg_subplot_sppnm <- findnm(p2veg_subplot_sppx, poptablst, returnNULL = TRUE)
      if (is.null(p2veg_subplot_sppnm)) {
        stop("p2veg_subplot_sppx table is not in database")
      }
        
    } else {
      p2veg_subplot_sppx <- pcheck.table(p2veg_subplot_sppx, stopifnull = TRUE, 
                                         stopifinvalid = TRUE)		
      p2veg_subplot_sppnm <- "p2veg_subplot_sppx"
    }  
  }
    
    

  return(list(p2veg_subp_structurenm = p2veg_subp_structurenm,
              p2veg_subplot_sppnm = p2veg_subplot_sppnm,
              peracre = peracre,
              p2vegtype = p2vegtype
              ))
  
}
