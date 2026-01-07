check.estdataDWM <- 
  function(popdatindb, 
           poptablst, 
           dwmx, 
           dwmtype,
           estdwm, 
           adj,
           gui = FALSE) {

  ###################################################################################
  ### Check parameters for DWM estimation
  ###################################################################################
  dwmadjvar <- NULL
  
  ## Check estdwm
  estdwmlst <- c("drybio", "carbon", "volcf")
  estdwm <- pcheck.varchar(var2check = estdwm, varnm = "estdwm",
                           checklst = estdwmlst, gui=gui, caption="dwm estvar?")
    
  ## Check dwmtype
  dwmtypelst <- c("cwd", "fwd_sm", "fwd_md", "fwd_lg")
  dwmtype <- pcheck.varchar(var2check = dwmtype, varnm = "dwmtype",
                           checklst = dwmtypelst, gui=gui, caption="dwm type?")
  
  
  
  ###################################################################################
  ## DESCRIPTION: Check data tables for DWM estimates
  ###################################################################################
  if (popdatindb) {
    if (!is.character(dwmx)) {
      stop("dwmx must be name of table in database")
    } 
    dwmnm <- findnm(dwmx, poptablst, returnNULL = TRUE)

    ## if the table is not in the database, check pltidsWITHqry (e.g., dwm_calc)
    # if (is.null(dwmnm)) {
    # 
    #   chk <- check.logic.vars(dwmx, pltidsWITHqry)
    #   #if (!check.logic.vars("pltids", pltidsWITHqry, returnVars=TRUE))
    #   if (!chk) {
    #     stop("must include dwm table...")
    #   }
    #   dwmnm <- dwmx
    # }
  } else {
    if (!is.data.frame(dwmx)) {
      stop("dwmx must be a data.frame object")
    }
    dwmx <- pcheck.table(dwmx, stopifnull = TRUE, 
                          stopifinvalid = TRUE)	
    dwmnm <- "dmx"
  } 
  
  
  ## get unadj variable
  dwmsumvar <- toupper(paste0(dwmtype, "_", estdwm, "_unadj"))
  
  
  ## adj variable
  if (adj %in% c("samp", "plot")) {
    dwmadjvar <- toupper(paste0("adj_factor_", dwmtype))
  }
  
  
  return(list(dwmnm = dwmnm, 
              dwmtype = dwmtype, 
              estdwm = estdwm,
              dwmsumvar = dwmsumvar,
              dwmadjvar = dwmadjvar
              ))
  
}

